use proc_macro::TokenStream;
use syn::spanned::Spanned;

#[proc_macro_derive(CustomDebug, attributes(debug))]
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

    let mut field_format_exprs = Vec::with_capacity(fields.named.len());
    for f in fields.named.iter() {
        let mut build_attr = None;
        for attr in f.attrs.iter() {
            if attr.path().is_ident("debug") {
                if build_attr.is_some() {
                    return Err(syn::Error::new(
                        attr.span(),
                        "duplicated \"debug\" attribute",
                    ));
                }
                build_attr = Some(attr);
            }
        }

        field_format_exprs.push(if let Some(attr) = build_attr {
            let meta = attr.meta.require_name_value()?;
            Some(&meta.value)
        } else {
            None
        });
    }

    let fmt_fn_fields = fields.named.iter().enumerate().map(|(i, f)| {
        let name = f.ident.as_ref().unwrap();
        let name_str = name.to_string();
        if let Some(format_expr) = field_format_exprs[i].as_ref() {
            quote::quote_spanned! {f.span()=>
                .field(#name_str, &format_args!(#format_expr, &self.#name))
            }
        } else {
            quote::quote_spanned! {f.span()=>
                .field(#name_str, &self.#name)
            }
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
