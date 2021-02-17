(setq org-publish-project-alist
      '(("notes-example"
         :base-extension "org"
         :base-directory "/path/notes_example/"
         :publishing-directory "/path/notes_example/dist/"
         :publishing-function org-tailwind-publish-to-html)))
