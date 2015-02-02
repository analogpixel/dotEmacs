;; http://nicolas-petton.fr/blog/blogging-with-org-mode.html
;; http://www.chaosape.com/blog/org_made_this_site.html
;; http://jacmoe.dk/blog/2011/december/using-emacs-and-org-mode-for-blogging
;; http://bastibe.de/2013-11-13-blogging-with-emacs.html

(require 'ox-publish)
(require 'ox-html)

(setq org-publish-project-alist
      '(

				("org-ianbarton"
				 :base-directory "c:/users/analog/dropbox/orgmode/posts"
				 :base-extension "org"
				 :publishing-directory "c:/users/analog/dropbox/orgmode/site"
				 :recursive t
				 :publishing-function org-html-publish-to-html
				 :headline-levels 6
				 :with-toc nil
				 :html-head "
         <link href='style.css' rel='stylesheet' type='text/css'>
         <link href='http://fonts.googleapis.com/css?family=Vollkorn' rel='stylesheet' type='text/css'>
         "
				 :htmlized-source t
				 :html-head-include-default-style nil
         :html-head-include-scripts nil
				 :with-author nil
         :with-creator nil
         :export-with-tags t
         :export-creator-info nil
         :export-author-info nil
         :auto-postamble nil

				 :auto-sitemap t
				 :sitemap-filename "archive.org"
				 :sitemap-title "Archive"
				 :sitemap-sort-files anti-chronologically
				 :sitemap-style list
				 :makeindex t
				 )


				("org-static-ian"
				 :base-directory "c:/users/analog/dropbox/orgmode/posts/"
				 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
				 :publishing-directory "c:/users/analog/dropbox/orgmode/site"
				 :recursive t
				 :publishing-function org-publish-attachment)

				("ian" :components ("org-ianbarton" "org-static-ian"))

				))
