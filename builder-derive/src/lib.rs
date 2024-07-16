use proc_macro::TokenStream;
use regex::Regex;
use syn::{parse_macro_input, DeriveInput};
use quote::quote;
use proc_macro2::TokenTree;

fn ty_is_option(ty: &syn::Type) -> Option<&syn::Type>  {
    if let syn::Type::Path(syn::TypePath {path: syn::Path{segments,..},..},..) = ty {
        if segments[0].ident == "Option" {
            if let syn::PathArguments::AngleBracketed(ref inner_ty) =  segments[0].arguments {
                if let syn::GenericArgument::Type(t) = &inner_ty.args[0] {
                    return Some(t);
                }
            }
        } 
    } 
    None
}

fn compile_error(f: &syn::Field) -> Option<syn::Error> {
    if f.attrs.is_empty() { return None; }
    if let syn::Meta::List(list) = &f.attrs[0].meta {
        for token in list.tokens.clone().into_iter() {
            if let TokenTree::Ident(ref i) = token {
                if i != "each" {
                    return Some(syn::Error::new_spanned(&f.attrs[0].meta, "expected `builder(each = \"...\")`"));
                }
            }
        }
    }
    None
}

fn field_is_mangled(f: &syn::Field) -> Option<String> {
    if f.attrs.is_empty() { return None; }
    if let syn::Meta::List(ref list) = f.attrs[0].meta {
        for token in list.tokens.clone().into_iter() {
            if let TokenTree::Literal(ref l) = token {
                let reg = Regex::new(r"[a-z]+").unwrap();
                let mut result = String::new();
                for cap in reg.captures_iter(&l.to_string()) {
                    result.push_str(&cap[0]);
                }
                return Some(result);
            }
        }
    }
    None
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let ident = &ast.ident;
    let bident = syn::Ident::new(&format!("{}Builder", ident.to_string()), ident.span());
    
    let fields = match &ast.data { syn::Data::Struct(syn::DataStruct 
        { fields: syn::Fields::Named(named),..})  => {
            &named.named
        },
        _ => unimplemented!()
    };

    let optionized = fields.into_iter().map(|f|{
        let ident = &f.ident;
        let ty = &f.ty;
        if ty_is_option(&ty).is_some() {
            quote! { #ident: #ty }
        } else {
            quote! { #ident: std::option::Option<#ty> }
        }
    });

    let method = fields.into_iter().map(|f| {
        let ident = &f.ident;
        let ty = &f.ty;
        if let Some(inner_ty) = ty_is_option(ty) {
            quote! {
                pub fn #ident(&mut self, #ident: #inner_ty) -> &mut #bident {
                    self.#ident = Some(#ident);
                    self
                }
            }
        } else {
            quote! {
                pub fn #ident(&mut self, #ident: #ty) -> &mut #bident {
                    self.#ident = Some(#ident);
                    self
                }
            }
        }
    });

    let mangled_methods = fields.into_iter().map(|f| {
        let pident = &f.ident;
        if let Some(literal) = field_is_mangled(&f) {
            if let Some(stream) = compile_error(&f) { return stream.to_compile_error(); }
            let ident = syn::Ident::new(&literal, ident.span());
            if pident.clone().unwrap() == &ident.to_string() { return quote! {/*..*/}; }
            quote! {
                pub fn #ident(&mut self, #ident: String) -> &mut #bident {
                    if let Some(ref mut v) = self.#pident {
                        v.push(#ident);
                    } else {
                        self.#pident = Some(std::vec::Vec::<String>::new());
                        self.#pident.clone().unwrap().push(#ident);
                    }
                    self
                }
            }
        } else {
            quote! { /*..*/ }
        }
    });
    
    let builder_fields = fields.into_iter().map(|f| {
        let ident = &f.ident;
        let ty = &f.ty;
        if ty_is_option(&ty).is_some() {
            quote! {#ident: self.#ident.clone() }
        } else {
            quote! {#ident: self.#ident.clone().ok_or("field not set")? }
        }
    });
    
    let command_fields = fields.into_iter().map(|f|{
        let ident = &f.ident;
        if field_is_mangled(&f).is_none() {
            quote! { #ident: None }
        } else {
            quote! { #ident: Some(std::vec::Vec::<String>::new()) }
        }
    });
    
    quote! {
        pub struct #bident {
            #(#optionized,)*
        }
        impl #ident {
            pub fn builder() -> #bident {
                #bident {
                    #(#command_fields,)*
                }
            }
        }   
        impl #bident {
            #(#method)*
            #(#mangled_methods)*
            pub fn build(&mut self) -> std::result::Result<#ident, std::boxed::Box<dyn std::error::Error>> { 
                Ok(#ident {
                    #(#builder_fields,)*
                }) 
            }
        }
    }.into()
}
