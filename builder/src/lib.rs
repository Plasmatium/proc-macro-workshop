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

    let expanded = quote! {
        #builder_struct
        #implementation
        #builder_setter
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
        let name = field.ident;
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
        let name = field.ident;
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