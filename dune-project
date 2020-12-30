(lang dune 2.7)
(name ocaml_portaudio)
(generate_opam_files true)

(license MIT)
(authors "Walter Litwinczyk")
(source (github wlitwin/ocaml_portaudio))
(maintainers "walter@litwinczyk.com")

(package
    (name ocaml_portaudio)
    (version 0.19.6)
    (synopsis "Bindings to the C portaudio library")
    (description "\| Bindings to the C portaudio library."
                 "\| Exposes low-level C bindings and a higher level OCaml interface."
                 "\| Version corresponds to the portaudio version this was built to."
                 )
    (depends
        conf-portaudio
    )
)
