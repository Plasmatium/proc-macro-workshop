use proc_macro::TokenStream;

use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DeriveInput, Field};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree.
    let input = parse_macro_input!(input as DeriveInput);

    // gen blocks
    let builder_struct = gen_builder_struct(&input);
    let implementation = gen_impl(&input);
    let builder_setter = gen_builder_setter(&input);
    let builder_impl = gen_builder_impl(&input);

    let expanded = quote! {
        #builder_struct
        #implementation
        #builder_setter
        #builder_impl
    };

    proc_macro::TokenStream::from(expanded)
}

fn gen_name(input: &DeriveInput) -> (proc_macro2::Ident, proc_macro2::Ident) {
    let origin_name = input.ident.clone();
    let builder_name = format_ident!("{origin_name}Builder");
    (origin_name, builder_name)
}

fn gen_impl(input: &DeriveInput) -> proc_macro2::TokenStream {
    let (origin_name, builder_name) = gen_name(input);
    let field_iter = get_field_iter(input);
    let field_asign_none_value = field_iter.map(|f| {
        let field = f.clone();
        let name = field.ident.unwrap();
        quote! {
            #name: None,
        }
    });
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

fn get_field_iter(input: &DeriveInput) -> impl Iterator<Item = &Field> {
    let struct_data = match &input.data {
        Data::Struct(s) => s,
        _ => panic!("Builder must be a named fields struct"),
    };
    let named_fields = match &struct_data.fields {
        syn::Fields::Named(n) => n,
        _ => panic!("Builder must be a named fields struct"),
    };
    named_fields.named.iter()
}

fn gen_builder_struct(input: &DeriveInput) -> proc_macro2::TokenStream {
    let (origin_name, _) = gen_name(input);
    let field_iter = get_field_iter(input);
    let opt_field_iter = field_iter.map(|f| {
        let f = f.clone();
        let field_name = f.ident.expect("named field");
        let typ = f.ty;
        quote! {
            #field_name: Option<#typ>,
        }
    });

    // make builder struct name
    let builder_name = format_ident!("{origin_name}Builder");

    quote! {
        #[derive(Debug)]
        pub struct #builder_name {
            #(#opt_field_iter)*
        }
    }
}

fn gen_builder_setter(input: &DeriveInput) -> proc_macro2::TokenStream {
    let (_, builder_name) = gen_name(input);
    let field_iter = get_field_iter(input);
    let setter_iter = field_iter.map(|f| {
        let field = f.clone();
        let name = field.ident.unwrap();
        let ty = field.ty;
        quote! {
            fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    });
    quote! {
        impl #builder_name {
            #(#setter_iter)*
        }
    }
}

fn gen_builder_impl(input: &DeriveInput) -> proc_macro2::TokenStream {
    let (origin_name, builder_name) = gen_name(&input);
    let field_iter = get_field_iter(input);
    // fn build
    // fields checked not none and same name variable assigned
    // eg. let executable = self.executable;
    let check_field_not_none = field_iter.map(|f| {
        let name = f.ident.clone().unwrap();
        let name_string = name.to_string();
        quote! {
            if self.#name == None {
                let err_msg = format!("field {} not set", #name_string);
                return Err(err_msg.into());
            }
            let #name = self.#name.clone().unwrap();
        }
    });
    let names = get_field_iter(input).map(|f| f.ident.clone().unwrap());
    let fn_build = quote! {
        pub fn build(&mut self) -> Result<Command, Box<dyn std::error::Error>> {
            #(#check_field_not_none)*
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
