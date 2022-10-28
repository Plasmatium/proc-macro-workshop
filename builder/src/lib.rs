use proc_macro::TokenStream;

use quote::{format_ident, quote};

use syn::{
    parse_macro_input, punctuated::Punctuated, token::Comma, Data, DeriveInput, Field, Ident, Type,
};

use syn::{Attribute, GenericArgument, MetaNameValue, Path, PathArguments, PathSegment};

fn extract_type_path(ty: &syn::Type) -> Option<&Path> {
    match *ty {
        syn::Type::Path(ref typepath) if typepath.qself.is_none() => Some(&typepath.path),
        _ => None,
    }
}

fn extract_container_segment<'a>(path: &'a Path, check_list: &[&str]) -> Option<&'a PathSegment> {
    let idents_list: Vec<String> = path
        .segments
        .iter()
        .map(|ps| ps.ident.to_string())
        .collect();
    let idents_of_path = idents_list.join("::");
    check_list
        .iter()
        .find(|&&s| &idents_of_path == s)
        .and_then(|_| path.segments.last())
}

fn extract_first_generic_param<'a>(ty: &'a Type, check_list: &[&str]) -> Option<&'a Type> {
    extract_type_path(ty)
        .and_then(|path| extract_container_segment(path, check_list))
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

fn extract_type_from_option(ty: &Type) -> Option<&Type> {
    let check_list = vec!["Option", "std::option::Option", "core::option::Option"];
    extract_first_generic_param(ty, &check_list)
}

fn extract_type_from_vector(ty: &Type) -> Option<&Type> {
    let check_list = vec!["Vec", "std::vec::Vec"];
    extract_first_generic_param(ty, &check_list)
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

    /// There are 3 kind of types
    /// 1. ty == Option<T>, extract type T and setter accepts type T and set the value as Option<T>
    /// 2. ty == Vec<T>, extract type T and setter accepts type T and push T into this vec, if `builder` attribute exist
    /// 3. ty == T, setter accepts T and set the value as T
    fn gen_builder_setter(&self) -> proc_macro2::TokenStream {
        let setter_iter = self.named.iter().map(|f| {
            let attrs = &f.attrs;
            let name = f.ident.as_ref().expect("named field");
            let ty = &f.ty;
            let (inner_opt_ty, inner_vec_ty) =
                (extract_type_from_option(ty), extract_type_from_vector(ty));
            let builder_attr_idents = get_builder_attr(attrs);
            let one_field_setter_group = match (inner_opt_ty, inner_vec_ty, builder_attr_idents.len() > 0) {
                // ty == Option<T>
                (Some(inner_ty), _, _) => {
                    // match Option<T> type
                    let fn_name = name;
                    let arg_name = name;
                    let arg_ty = inner_ty;
                    quote! {
                        fn #fn_name(&mut self, #arg_name: #arg_ty) -> &mut Self {
                            self.#name = Some(#name);
                            self
                        } 
                    }
                }

                // ty == Vec<T> and builder attrs found(len() > 0)
                (_, Some(inner_ty), true) => {
                    let single_mode_setters = builder_attr_idents.iter().map(|each_ident| {
                        // generate one single mode setter on one field
                        quote! {
                            fn #each_ident(&mut self, #each_ident: #inner_ty) -> &mut Self {
                                match self.#name {
                                    None => self.#name = Some(vec![#each_ident]),
                                    Some(ref mut v) => v.push(#each_ident),
                                }
                                self
                            } 
                        }
                    });
                    let single_mode_setters = quote! {
                        #(#single_mode_setters)*
                    };

                    // checking name conflict
                    // if conflict exists, single mode setters only
                    // no name conflict, collect single mode along with vec mode
                    // here builder_attr_idents supposed to be not empty
                    let attr_str_list: Vec<String> = builder_attr_idents.iter().map(ToString::to_string).collect();
                    if attr_str_list.contains(&quote!(#name).to_string()) {
                        // name conflict does exist
                        // we just need to generate single mode setters
                        single_mode_setters
                    } else {
                        let (fn_name, arg_name, arg_ty) = (name, name, ty);
                        quote! {
                            fn #fn_name(&mut self, #arg_name: #arg_ty) -> &mut Self {
                                self.#name = Some(#name);
                                self
                            }
                            #single_mode_setters
                        }
                    }
                },
                _ => {
                    let (fn_name, arg_name, arg_ty) = (name, name, ty);
                    quote! {
                        fn #fn_name(&mut self, #arg_name: #arg_ty) -> &mut Self {
                            self.#name = Some(#name);
                            self
                        } 
                    }
                },
            };
            
            // one_field_setter_group may contains more than one setter on Vec<T> fields, if `builder` attr is set
            // collect these generated setters into one token stream
            // **NOTE** here these are setters which just on one field
            // println!("one_field -----> {}", one_field_setter_group);
            one_field_setter_group
        });

        // collect generated setters on all fields
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
        let names = self
            .named
            .iter()
            .map(|f| f.ident.clone().expect("need named field"));
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
            let (if_inner_opt_ty, if_inner_vec_ty) = (extract_type_from_option(ty), extract_type_from_vector(ty));
            match (if_inner_opt_ty, if_inner_vec_ty) {
                (Some(_), _) => quote! {
                    // matching Option<T>
                    let #name = self.#name.clone();
                },
                (_, Some(_)) => quote! {
                    // matching Vec<T>
                    let #name = match self.#name.clone() {
                        Some(v) => v,
                        None => vec![],
                    };
                },
                _ => quote! {
                    let #name = self.#name.clone().expect(#err_msg);
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

/// find the attrs named builder: #[builder(each = "arg")]
/// and extract it's named_value pair: `each` and `"arg"`
/// 
/// 
/// #[builder(each = "arg", each = "arg2")]
/// args: Vec<String>
/// 
/// this will construct three setters: 
/// fn arg(_ String) -> Vec<String>
/// fn arg2(_ String) -> Vec<String>
/// fn args(_ Vec<String>) -> Vec<String>
fn get_builder_attr(attrs: &[Attribute]) -> Vec<Ident> {
    let err_msg = r#"error: builder attribute expect format #[builder(each = "ident""#;
    attrs
        .iter()
        .filter_map(|attr| {
            let meta = attr
                .parse_meta();
            if let Err(e) = meta {
                println!("{err_msg}");
                println!("origin_err: {e}");
                return None
            }
            // this is a safe unwrap operation
            let meta = meta.unwrap();
            match &meta {
                syn::Meta::List(li) => {
                    // inner is `each = "ident"`
                    let inner = &li.nested;
                    let pair: MetaNameValue = syn::parse_quote!(#inner);
                    let token_each = &pair.path;
                    let token_ident = &pair.lit;
                    if quote!(#token_each).to_string() != "each" {
                        // wrong format, first word should be `each`
                        println!("{}", err_msg) 
                    }
                    // trim puncuation: '"'
                    let ident = quote!(#token_ident).to_string();
                    let ident = ident.trim_matches('"');
                    // Some(quote!(#ident))
                    syn::parse_str(ident).expect("expect should be parse from string into ident")
                },
                _ => {
                    println!("{}", err_msg);
                    None
                },
            }
        })
        .collect()
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree.
    let input = parse_macro_input!(input as DeriveInput);
    let helper = Helper::new(input);
    let expanded = helper.gen_expanded();

    proc_macro::TokenStream::from(expanded)
}
