;; http://nicolas-petton.fr/blog/blogging-with-org-mode.html
;; http://www.chaosape.com/blog/org_made_this_site.html
;; http://jacmoe.dk/blog/2011/december/using-emacs-and-org-mode-for-blogging
;; http://bastibe.de/2013-11-13-blogging-with-emacs.html

(require 'ox-publish)
(require 'ox-html)

(setq org-publish-project-alist
      '(

				("publish-analog"
				 :base-directory "e:/checkedoutRepos/analogPixelorg/org/"
				 :base-extension "org"
				 :publishing-directory "e:/checkedoutRepos/analogPixelorg/html/"
				 :recursive t
				 :publishing-function org-html-publish-to-html
				 :headline-levels 6
				 :with-toc nil
				 :htmlized-source t
				 :html-head-include-default-style nil
         :html-head-include-scripts nil
				 :html-postamble ""
				 :with-author nil
         :with-creator nil
         :export-with-tags t
         :export-creator-info nil
         :export-author-info nil
         :auto-postamble nil
         :html-validation-link nil
				 :auto-sitemap nil
				 :auto-index nil
				 :makeindex nil
				 :body-only t  ;; only export the body, don't add a header or footer
				 :html-extension "htm"
				 )
				))
