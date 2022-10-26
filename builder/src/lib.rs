use proc_macro::TokenStream;

use syn::{parse_macro_input, DeriveInput, Data};
use quote::{quote, format_ident};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree.
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;

    // gen Builder struct
    let builder_struct = gen_builder_struct(&name.to_string(), &input.data);

    let expanded = quote! {
        #builder_struct

        impl #name {
            pub fn builder() {}
        }
    };
    
    proc_macro::TokenStream::from(expanded)
}

fn gen_builder_struct(origin_name: &str, data: &Data) -> proc_macro2::TokenStream {
    // make fields
    let struct_data = match data {
        Data::Struct(s) => s,
        _ => panic!("Builder must be a named fields struct")
    };
    let named_fields = match &struct_data.fields {
        syn::Fields::Named(n) => n,
        _ => panic!("Builder must be a named fields struct")
    };
    let fields_iter = named_fields.named.iter().map(|f| {
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
        pub struct #builder_name {
            #(#fields_iter)*
        }
    }
}

fn gen_impl(input: &DeriveInput) -> proc_macro2::TokenStream {

    todo!();
}

fn get_fields_iter(input: &DeriveInput) -> impl Iterator<Item = proc_macro2::TokenStream> {
    let struct_data = match &input.data {
        Data::Struct(s) => s,
        _ => panic!("Builder must be a named fields struct")
    };
    let named_fields = match &struct_data.fields {
        syn::Fields::Named(n) => n,
        _ => panic!("Builder must be a named fields struct")
    };
    let fields_iter = named_fields.named.iter().map(|f| {
        let f = f.clone();
        let field_name = f.ident.expect("named field");
        let typ = f.ty;
        quote! {
            #field_name: Option<#typ>,
        }
    });
    fields_iter
}