use proc_macro::TokenStream;
use syn::punctuated::Punctuated;
use syn::visit::{self, Visit};
use syn::{spanned::Spanned, GenericParam};
use syn::{Path, TypePath};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = match syn::parse(input) {
        Ok(data) => data,
        Err(err) => return err.to_compile_error().into(),
    };

    impl_macro(ast).unwrap_or_else(|err| err.to_compile_error().into())
}

fn is_path_phantom_data(ty: &TypePath) -> bool {
    if let TypePath {
        qself: None,
        path: Path {
            leading_colon: _,
            segments,
        },
    } = ty
    {
        // todo: test as well for std::marker::PhantomData
        if segments.len() != 1 {
            return false;
        } else {
            let seg = segments.first().unwrap();
            return seg.ident == "PhantomData";
        }
    }
    false
}

struct FindGenericParamVisitor<'ast> {
    all_params: &'ast Punctuated<GenericParam, syn::token::Comma>,
    pub paths_requiring_debug_bounds: Vec<&'ast syn::Path>,
}

impl<'ast> FindGenericParamVisitor<'ast> {
    pub fn new(params: &'ast Punctuated<GenericParam, syn::token::Comma>) -> Self {
        Self {
            all_params: params,
            paths_requiring_debug_bounds: Vec::new(),
        }
    }
}

impl<'ast> Visit<'ast> for FindGenericParamVisitor<'ast> {
    fn visit_type_ptr(&mut self, _i: &'ast syn::TypePtr) {
        // do nothing
    }

    fn visit_type_path(&mut self, i: &'ast syn::TypePath) {
        // ignore any path types using PhantomData
        if is_path_phantom_data(i) {
            return;
        }

        // test if path starts with a generic parameter
        let start_indent = &i.path.segments.first().as_ref().unwrap().ident;
        for param in self.all_params.iter() {
            if let GenericParam::Type(ty_param) = param {
                if *start_indent == ty_param.ident
                    && self
                        .paths_requiring_debug_bounds
                        .iter()
                        .any(|&existing_path| i.path == *existing_path)
                {
                    self.paths_requiring_debug_bounds.push(&i.path);
                }
            }
        }

        visit::visit_type_path(self, i);
    }
}

fn impl_macro(ast: syn::DeriveInput) -> Result<proc_macro::TokenStream, syn::Error> {
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

    let mut generic_params_visitor = FindGenericParamVisitor::new(&ast.generics.params);
    generic_params_visitor.visit_derive_input(&ast);
    let paths_requiring_bounds = generic_params_visitor.paths_requiring_debug_bounds;

    let where_path_predicates = paths_requiring_bounds.iter().map(|p| {
        quote::quote_spanned! {p.span()=>
            #p: std::fmt::Debug,
        }
    });

    let (impl_generics, ty_generics, original_where_clause) = ast.generics.split_for_impl();
    let where_clause = if let Some(original) = original_where_clause {
        quote::quote_spanned! {original.span()=>
            #original
        }
    } else {
        quote::quote! {where}
    };

    // the where clause probably has some error I am not aware, or at least it doesn't seem the
    // best approach to do it this way
    let gen = quote::quote! {
        impl #impl_generics std::fmt::Debug for #name #ty_generics
            #where_clause
                #(#where_path_predicates)*
            {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#name_str)
                    #(#fmt_fn_fields)*
                    .finish()
            }
        }
    };

    Ok(gen.into())
}
