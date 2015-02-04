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
         <script src='./js/jq.js'></script>
         <script src='./js/snap.js'></script>
         <script src='./js/icon.js'></script>
         <script src='./js/iconLoad.js'></script>
         "
				 :htmlized-source t
				 :html-head-include-default-style nil
         :html-head-include-scripts nil
				 :html-preamble "
				 <div class='site-header' id=headerBar>
					<div class=svgContainer><a href='/'><svg viewbox='0 0 40 40' id=homeIcon></svg></a></div>
					<div class=svgContainer><a href='mailto:analog@analogpxixel.org'><svg viewbox='0 0 40 40' id=mailIcon ></svg></a></div>
					<div class=svgContainer><a href='https://github.com/analogpixel'><svg viewbox='0 0 40 40' id=githubIcon></svg></a></div>
					<div class=svgContainer><a href='https://twitter.com/analogp1xel'><svg viewbox='0 0 40 40' id=twitterIcon></svg></a></div>
					<div class=svgContainer><a href='http://analog.smugmug.com'><svg viewbox='0 0 40 40' id=smugIcon></svg></a></div>
				 </div>
         "
				 :html-postamble ""
				 :with-author nil
         :with-creator nil
         :export-with-tags t
         :export-creator-info nil
         :export-author-info nil
         :auto-postamble nil
         :html-validation-link nil
				 :auto-sitemap t
				 :sitemap-filename "archive.org"
				 :sitemap-title "Analogpixel.org"
				 :sitemap-sort-files anti-chronologically
				 :sitemap-style list
				 :makeindex nil
				 )


				("org-static-ian"
				 :base-directory "c:/users/analog/dropbox/orgmode/posts/"
				 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
				 :publishing-directory "c:/users/analog/dropbox/orgmode/site"
				 :recursive t
				 :makeindex nil
				 :publishing-function org-publish-attachment)

				("ian" :components ("org-ianbarton" "org-static-ian"))

				))


(setq org-publish-cache nil)
