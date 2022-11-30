(lang dune 3.6)

(name todo)

(generate_opam_files true)

(source
 (github sundywy/todo))

(authors "Sundy Yaputra")

(maintainers "Sundy Yaputra")

(package
 (name todo)
 (synopsis "A todo app")
 (description "A todo app for learning")
 (depends
  (ocaml (= 4.14.0))
   dune
  (base (= v0.15.1))
 )
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
