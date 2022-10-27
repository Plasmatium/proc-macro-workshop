use proc_macro::TokenStream;

use quote::{format_ident, quote};
use syn::{
    parse_macro_input, punctuated::Punctuated, token::Comma, Data, DeriveInput, Field, Ident, Type,
};

use syn::{GenericArgument, Path, PathArguments, PathSegment};

fn extract_type_path(ty: &syn::Type) -> Option<&Path> {
    match *ty {
        syn::Type::Path(ref typepath) if typepath.qself.is_none() => Some(&typepath.path),
        _ => None,
    }
}

fn extract_option_segment(path: &Path) -> Option<&PathSegment> {
    let idents_of_path = path
        .segments
        .iter()
        .into_iter()
        .fold(String::new(), |mut acc, v| {
            acc.push_str(&v.ident.to_string());
            acc.push('|');
            acc
        });
    vec!["Option|", "std|option|Option|", "core|option|Option|"]
        .into_iter()
        .find(|s| &idents_of_path == *s)
        .and_then(|_| path.segments.last())
}

fn extract_type_from_option(ty: &Type) -> Option<&Type> {
    extract_type_path(ty)
        .and_then(|path| extract_option_segment(path))
        .and_then(|path_seg| {
            let type_params = &path_seg.arguments;
            // It should have only on angle-bracketed param ("<String>"):
            match *type_params {
                PathArguments::AngleBracketed(ref params) => params.args.first(),
                _ => None,
            }
        })
        .and_then(|generic_arg| match *generic_arg {
            GenericArgument::Type(ref ty) => Some(ty),
            _ => None,
        })
}

struct Helper {
    origin_name: Ident,
    builder_name: Ident,
    named: Punctuated<Field, Comma>,
}

impl Helper {
    fn new(input: DeriveInput) -> Self {
        let origin_name = input.ident.clone();
        let builder_name = format_ident!("{origin_name}Builder");
        let struct_data = match input.data.clone() {
            Data::Struct(s) => s,
            _ => panic!("Builder must be a named fields struct"),
        };
        let named_fields = match struct_data.fields {
            syn::Fields::Named(n) => n,
            _ => panic!("Builder must be a named fields struct"),
        };
        let named = named_fields.named;
        Self {
            origin_name,
            builder_name,
            named,
        }
    }

    fn gen_builder_struct(&self) -> proc_macro2::TokenStream {
        let opt_field_iter = self.named.iter().map(|f| {
            let field_name = f.ident.as_ref().expect("named field");
            let ty = &f.ty;
            let refined_ty = match extract_type_from_option(ty) {
                Some(_) => quote!(#ty),
                None => quote!(Option<#ty>),
            };
            quote! {
                #field_name: #refined_ty,
            }
        });
        let builder_name = &self.builder_name;

        quote! {
            #[derive(Debug)]
            pub struct #builder_name {
                #(#opt_field_iter)*
            }
        }
    }

    fn gen_struct_impl(&self) -> proc_macro2::TokenStream {
        let field_asign_none_value = self.named.iter().map(|f| {
            let name = f.ident.as_ref().expect("named field");
            quote! {
                #name: None,
            }
        });
        let Self {
            origin_name,
            builder_name,
            ..
        } = self;
        quote! {
            impl #origin_name {
                pub fn builder() -> #builder_name {
                    #builder_name {
                        #(#field_asign_none_value)*
                    }
                }
            }
        }
    }

    fn gen_builder_setter(&self) -> proc_macro2::TokenStream {
        let setter_iter = self.named.iter().map(|f| {
            let name = f.ident.as_ref().expect("named field");
            let ty = &f.ty;
            let inner_opt_ty = extract_type_from_option(ty);
            let setter_ty = match inner_opt_ty {
                Some(inner) => inner,
                None => ty,
            };
            quote! {
                fn #name(&mut self, #name: #setter_ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        });
        let builder_name = &self.builder_name;
        quote! {
            impl #builder_name {
                #(#setter_iter)*
            }
        }
    }

    fn gen_builder_impl(&self) -> proc_macro2::TokenStream {
        let Self {
            builder_name,
            origin_name,
            ..
        } = self;
        // fn build
        // fields checked not none and same name variable assigned
        // eg. let executable = self.executable;
        let checker = self.gen_field_opt_checker();
        let names = self.named.iter().map(
            |f| f.ident.clone().expect("need named field"));
        let fn_build = quote! {
            pub fn build(&mut self) -> Result<Command, Box<dyn std::error::Error>> {
                #(#checker)*
                let cmd = #origin_name {
                    #(#names,)
                    *
                };
                Ok(cmd)
            }
        };
        quote! {
            impl #builder_name {
                #fn_build
            }
        }
    }

    fn gen_field_opt_checker(&self) -> impl Iterator<Item = proc_macro2::TokenStream> + '_ {
        self.named.iter().map(|f| {
            let ty = &f.ty;
            let name = f.ident.as_ref().expect("should be named field");
            let err_msg = format!("fields {name} required");
            let if_inner_opt_ty = extract_type_from_option(ty);
            match if_inner_opt_ty {
                None => quote! {
                    let #name = self.#name.clone().expect(#err_msg);
                },
                Some(_) => quote! {
                    let #name = self.#name.clone();
                },
            }
        })
    }

    fn gen_expanded(&self) -> proc_macro2::TokenStream {
        let builder_struct = self.gen_builder_struct();
        let implementation = self.gen_struct_impl();
        let builder_setter = self.gen_builder_setter();
        let builder_impl = self.gen_builder_impl();
        quote! {
            #builder_struct
            #implementation
            #builder_setter
            #builder_impl
        }
    }
}

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree.
    let input = parse_macro_input!(input as DeriveInput);
    let helper = Helper::new(input);
    let expanded = helper.gen_expanded();

    proc_macro::TokenStream::from(expanded)
}
