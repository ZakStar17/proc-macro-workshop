use proc_macro::TokenStream;
use proc_macro2::Span;
use syn::spanned::Spanned;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();

    // Build the trait implementation
    impl_macro(&ast)
}

fn impl_macro(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let field_list = if let syn::Data::Struct(data) = &ast.data {
        if let syn::Fields::Named(fields) = &data.fields {
            fields
        } else {
            panic!("fields must be named");
        }
    } else {
        panic!("must be struct");
    };
    let field_indents = field_list.named.iter().map(|f| &f.ident);

    let builder_struct_name = syn::Ident::new(&format!("{}Builder", name), Span::call_site());
    let builder_fields = field_list.named.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        quote::quote_spanned! {f.span()=>
            pub #name: Option<#ty>,
        }
    });
    let builder_setter_fns = field_list.named.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        quote::quote_spanned! {f.span()=>
            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    });
    let build_fn_unwraps = field_list.named.iter().map(|f| {
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
    let builder_struct_def = quote::quote! {
        pub struct #builder_struct_name {
            #(#builder_fields)*
        }

        impl #builder_struct_name {
            #(#builder_setter_fns)*

            pub fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                #(#build_fn_unwraps)*

                Ok(#name {
                    #(#field_indents,)*
                })
            }
        }
    };

    let builder_fn_init_fields = field_list.named.iter().map(|f| {
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

    eprintln!("CODE\n{}", gen);
    gen.into()
}
