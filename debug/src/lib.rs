use proc_macro::TokenStream;
use syn::spanned::Spanned;

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = match syn::parse(input) {
        Ok(data) => data,
        Err(err) => return err.to_compile_error().into(),
    };

    impl_macro(&ast).unwrap_or_else(|err| err.to_compile_error().into())
}

fn impl_macro(ast: &syn::DeriveInput) -> Result<proc_macro::TokenStream, syn::Error> {
    let name = &ast.ident;
    let name_str = name.to_string();

    let fields = if let syn::Data::Struct(data) = &ast.data {
        if let syn::Fields::Named(fields) = &data.fields {
            fields
        } else {
            return Err(syn::Error::new(
                ast.span(),
                "macro \"CustomDebug\" does not support structs with empty or with unnamed fields",
            ));
        }
    } else {
        return Err(syn::Error::new(
            ast.span(),
            "macro \"CustomDebug\" only supports structs",
        ));
    };

    let fmt_fn_fields = fields.named.iter().map(|f| {
        let name = f.ident.as_ref().unwrap();
        let name_str = name.to_string();
        quote::quote_spanned! {f.span()=>
            .field(#name_str, &self.#name)
        }
    });

    let gen = quote::quote! {
        impl std::fmt::Debug for #name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#name_str)
                    #(#fmt_fn_fields)*
                    .finish()
            }
        }
    };

    Ok(gen.into())
}
