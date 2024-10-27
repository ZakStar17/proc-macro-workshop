use itertools::{Either, Itertools};
use proc_macro::TokenStream;
use proc_macro2::Span;
use syn::{spanned::Spanned, Field, GenericArgument, Path, PathArguments, Type, TypePath};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();

    // Build the trait implementation
    impl_macro(&ast)
}

// returns None if type is not Option
fn get_type_from_inside_option(ty: &Type) -> Option<&Type> {
    if let Type::Path(TypePath {
        qself: None,
        path: Path {
            leading_colon: _,
            segments,
        },
    }) = ty
    {
        if segments.len() != 1 {
            return None;
        } else {
            let seg = segments.first().unwrap();
            if seg.ident.to_string() != "Option" {
                return None;
            }
            if let PathArguments::AngleBracketed(bracketed) = &seg.arguments {
                if bracketed.args.len() != 1 {
                    return None;
                }
                if let GenericArgument::Type(inside_ty) = bracketed.args.first().unwrap() {
                    return Some(inside_ty);
                }
            }
        }
    }
    None
}

fn impl_macro(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;

    let (option_fields, normal_fields): (Vec<(&Field, &Type)>, Vec<&Field>) =
        if let syn::Data::Struct(data) = &ast.data {
            if let syn::Fields::Named(fields) = &data.fields {
                fields.named.iter().partition_map(|f| {
                    if let Some(inside_ty) = get_type_from_inside_option(&f.ty) {
                        Either::Left((f, inside_ty))
                    } else {
                        Either::Right(f)
                    }
                })
            } else {
                panic!("fields must be named");
            }
        } else {
            panic!("must be struct");
        };
    let field_indents = option_fields
        .iter()
        .map(|f| &f.0.ident)
        .chain(normal_fields.iter().map(|f| &f.ident));

    let builder_struct_name = syn::Ident::new(&format!("{}Builder", name), Span::call_site());
    let builder_fields = normal_fields
        .iter()
        .map(|f| {
            let name = &f.ident;
            let ty = &f.ty;
            quote::quote_spanned! {f.span()=>
                pub #name: Option<#ty>,
            }
        })
        .chain(option_fields.iter().map(|(f, _inside_ty)| {
            let name = &f.ident;
            let ty = &f.ty;
            quote::quote_spanned! {f.span()=>
                pub #name: #ty,
            }
        }));
    let builder_setter_fns = normal_fields
        .iter()
        .map(|f| {
            let name = &f.ident;
            let ty = &f.ty;
            quote::quote_spanned! {f.span()=>
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        })
        .chain(option_fields.iter().map(|(f, inside_ty)| {
            let name = &f.ident;
            quote::quote_spanned! {f.span()=>
                pub fn #name(&mut self, #name: #inside_ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        }));
    let normal_fields_build_fn_unwraps = normal_fields.iter().map(|f| {
        let name = &f.ident;
        let error_msg = format!("Missing {}", name.as_ref().unwrap());
        quote::quote_spanned! {f.span()=>
            let #name = std::mem::take(&mut self.#name)
            .ok_or({
                let err: Box<dyn std::error::Error> = #error_msg.into();
                err
            })?;
        }
    });
    let option_fields_build_fn = option_fields.iter().map(|(f, _inside_ty)| {
        let name = &f.ident;
        quote::quote_spanned! {f.span()=>
            let #name = std::mem::take(&mut self.#name);
        }
    });
    let builder_struct_def = quote::quote! {
        pub struct #builder_struct_name {
            #(#builder_fields)*
        }

        impl #builder_struct_name {
            #(#builder_setter_fns)*

            pub fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                #(#normal_fields_build_fn_unwraps)*
                #(#option_fields_build_fn)*

                Ok(#name {
                    #(#field_indents,)*
                })
            }
        }
    };

    let builder_fn_init_fields = normal_fields
        .iter()
        .chain(option_fields.iter().map(|(f, _inside_ty)| f))
        .map(|f| {
            let name = &f.ident;
            quote::quote_spanned! {f.span()=>
                #name: None,
            }
        });
    let gen = quote::quote! {
        #builder_struct_def

        impl #name {
            pub fn builder() -> #builder_struct_name {
                #builder_struct_name {
                    #(#builder_fn_init_fields)*
                }
            }
        }
    };

    gen.into()
}
