use proc_macro2::{Span, TokenStream};
use syn::{
    spanned::Spanned, Field, FieldsNamed, GenericArgument, Ident, LitStr, Path, PathArguments,
    Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = match syn::parse(input) {
        Ok(data) => data,
        Err(err) => return err.to_compile_error().into(),
    };

    // Build the trait implementation
    impl_macro(&ast).unwrap_or_else(|err| err.to_compile_error().into())
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
            if seg.ident != "Option" {
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

fn get_type_from_inside(ty: &Type) -> Option<&Type> {
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

struct ParsedFields<'a> {
    pub normal: Vec<ParsedNormalField<'a>>,
    pub option: Vec<ParsedOptionField<'a>>,
    pub repeated: Vec<ParsedRepeatedField<'a>>,
}

trait ParsedField {
    fn ident(&self) -> &Ident;
    fn init_definition(&self) -> TokenStream;
    fn struct_field_definition(&self) -> TokenStream;
    fn builder_functions(&self) -> TokenStream;
    fn build_fn_var_definition(&self) -> TokenStream;
}

struct ParsedNormalField<'a> {
    pub f: &'a Field,
}

struct ParsedOptionField<'a> {
    pub f: &'a Field,
    pub inside_ty: &'a Type,
}

struct ParsedRepeatedField<'a> {
    pub f: &'a Field,
    pub inside_ty: &'a Type,
    pub one_at_a_time_arg_name: LitStr,
}

impl<'a> ParsedField for ParsedNormalField<'a> {
    fn ident(&self) -> &Ident {
        self.f.ident.as_ref().unwrap()
    }

    fn init_definition(&self) -> TokenStream {
        let name = self.ident();
        quote::quote_spanned! {self.f.span()=>
            #name: std::option::Option::None,
        }
    }

    fn struct_field_definition(&self) -> TokenStream {
        let name = self.ident();
        let ty = &self.f.ty;
        quote::quote_spanned! {self.f.span()=>
            pub #name: std::option::Option<#ty>,
        }
    }

    fn builder_functions(&self) -> TokenStream {
        let name = self.ident();
        let ty = &self.f.ty;
        quote::quote_spanned! {self.f.span()=>
            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = std::option::Option::Some(#name);
                self
            }
        }
    }

    fn build_fn_var_definition(&self) -> TokenStream {
        let name = self.ident();
        let error_msg = format!("Missing {}", name);
        quote::quote_spanned! {self.f.span()=>
            let #name = std::mem::take(&mut self.#name)
            .ok_or({
                let err: std::boxed::Box<dyn std::error::Error> = #error_msg.into();
                err
            })?;
        }
    }
}

impl<'a> ParsedField for ParsedOptionField<'a> {
    fn ident(&self) -> &Ident {
        self.f.ident.as_ref().unwrap()
    }

    fn init_definition(&self) -> TokenStream {
        let name = self.ident();
        quote::quote_spanned! {self.f.span()=>
            #name: std::option::Option::None,
        }
    }

    fn struct_field_definition(&self) -> TokenStream {
        let name = self.ident();
        let ty = &self.f.ty;
        quote::quote_spanned! {self.f.span()=>
            pub #name: #ty,
        }
    }

    fn builder_functions(&self) -> TokenStream {
        let name = self.ident();
        let inside_ty = self.inside_ty;
        quote::quote_spanned! {self.f.span()=>
            pub fn #name(&mut self, #name: #inside_ty) -> &mut Self {
                self.#name = std::option::Option::Some(#name);
                self
            }
        }
    }

    fn build_fn_var_definition(&self) -> TokenStream {
        let name = self.ident();
        quote::quote_spanned! {self.f.span()=>
            let #name = std::mem::take(&mut self.#name);
        }
    }
}

impl<'a> ParsedField for ParsedRepeatedField<'a> {
    fn ident(&self) -> &Ident {
        self.f.ident.as_ref().unwrap()
    }

    fn init_definition(&self) -> TokenStream {
        let name = self.ident();
        let ty = &self.f.ty;
        quote::quote_spanned! {self.f.span()=>
            #name: <#ty>::new(),
        }
    }

    fn struct_field_definition(&self) -> TokenStream {
        let name = self.ident();
        let ty = &self.f.ty;
        quote::quote_spanned! {self.f.span()=>
            pub #name: #ty,
        }
    }

    fn builder_functions(&self) -> TokenStream {
        let name = self.ident();
        let inside_ty = self.inside_ty;
        let full_ty = &self.f.ty;
        let one_at_a_time_name = self.one_at_a_time_arg_name.value();
        let all_at_once_builder = if *name != one_at_a_time_name {
            quote::quote_spanned! {self.f.span()=>
                pub fn #name(&mut self, #name: #full_ty) -> &mut Self {
                    self.#name = #name;
                    self
                }
            }
        } else {
            TokenStream::new()
        };

        let one_at_a_time_fn_name_ident = Ident::new(&one_at_a_time_name, Span::call_site());
        quote::quote_spanned! {self.f.span()=>
            #all_at_once_builder

            pub fn #one_at_a_time_fn_name_ident(&mut self, #one_at_a_time_fn_name_ident: #inside_ty) -> &mut Self {
                self.#name.push(#one_at_a_time_fn_name_ident);
                self
            }
        }
    }

    fn build_fn_var_definition(&self) -> TokenStream {
        let name = self.ident();
        quote::quote_spanned! {self.f.span()=>
            let #name = std::mem::take(&mut self.#name);
        }
    }
}

impl<'a> ParsedFields<'a> {
    pub fn new(fields: &'a FieldsNamed) -> Result<Self, syn::Error> {
        let mut normal = Vec::new();
        let mut option = Vec::new();
        let mut repeated = Vec::new();

        for f in fields.named.iter() {
            let mut repeated_attr = false;
            for attr in f.attrs.iter() {
                if attr.path().is_ident("builder") {
                    attr.parse_nested_meta(|meta| {
                        if meta.path.is_ident("each") {
                            if repeated_attr {
                                return Err(meta.error("duplicated \"each\" attribute value"));
                            }
                            let value = meta.value()?;
                            let s: LitStr = value.parse()?;

                            let inside_ty = get_type_from_inside(&f.ty).ok_or(
                                syn::Error::new(f.span(),
                                 format!("field \"{}\" was assigned the `each = \"arg\"` attribute, however its type is not of the form `Vec<...>`",
                                f.ident.as_ref().unwrap())))?;

                            repeated_attr = true;
                            repeated.push(
                                ParsedRepeatedField {
                                    f,
                                    inside_ty,
                                    one_at_a_time_arg_name: s
                                });

                            Ok(())
                        } else {
                            Err(meta.error(format!("unknown attribute \"{}\"\nExpected `builder(each = \"...\")`", meta.path.get_ident().unwrap())))
                        }
                    })?
                }
            }
            if repeated_attr {
                continue;
            }

            if let Some(inside_ty) = get_type_from_inside_option(&f.ty) {
                option.push(ParsedOptionField { f, inside_ty });
            } else {
                normal.push(ParsedNormalField { f });
            }
        }

        Ok(Self {
            normal,
            option,
            repeated,
        })
    }

    pub fn iterate_fields(&self) -> impl Iterator<Item = &dyn ParsedField> {
        self.normal
            .iter()
            .map(|f| f as &dyn ParsedField)
            .chain(self.option.iter().map(|f| f as &dyn ParsedField))
            .chain(self.repeated.iter().map(|f| f as &dyn ParsedField))
    }
}

fn impl_macro(ast: &syn::DeriveInput) -> Result<proc_macro::TokenStream, syn::Error> {
    let name = &ast.ident;

    let parsed = if let syn::Data::Struct(data) = &ast.data {
        if let syn::Fields::Named(fields) = &data.fields {
            ParsedFields::new(fields)?
        } else {
            return Err(syn::Error::new(
                ast.span(),
                "macro \"Builder\" does not support structs with empty or with unnamed fields",
            ));
        }
    } else {
        return Err(syn::Error::new(
            ast.span(),
            "macro \"Builder\" only supports structs",
        ));
    };

    let builder_struct_name = syn::Ident::new(&format!("{}Builder", name), Span::call_site());

    let struct_field_definitions = parsed.iterate_fields().map(|f| f.struct_field_definition());
    let builder_from_field_functions = parsed.iterate_fields().map(|f| f.builder_functions());
    let builder_fn_var_definitions = parsed.iterate_fields().map(|f| f.build_fn_var_definition());
    let idents = parsed.iterate_fields().map(|f| f.ident());
    let builder_struct_def = quote::quote! {
        pub struct #builder_struct_name {
            #(#struct_field_definitions)*
        }

        impl #builder_struct_name {
            #(#builder_from_field_functions)*

            pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                #(#builder_fn_var_definitions)*

                Ok(#name {
                    #(#idents,)*
                })
            }
        }
    };

    let builder_fn_init_fields = parsed.iterate_fields().map(|f| f.init_definition());
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

    Ok(gen.into())
}
