![Version](https://img.shields.io/static/v1?label=latex-emacs&message=0.2.4&color=brightcolor)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)


# A GNU Emacs profile for using LaTeX and Org in academic research

## Make writing tasks fun by disguising them as coding projects!

This repo contains an `init.el` file (configuration file) for using LaTeX in GNU Emacs version 29.
You can adapt it to other configurations of Emacs (e.g., Spacemacs, Doom, etc).
The SciMax configuration probably has similar support built-in.
See latex-emacs28 and latex-emacs30 for configurations that work with Emacs version 28 and Emacs version 30, respectively. 

Features include the following:

* heavily customized auto-complete for LaTeX
* AUCTeX
* atomic-chrome (GhostText)
* awesome-tabs (install from Github)
* dashboard and dashboard-hackernews (keep abreast with the latest distractions)
* org-mode
* org-agenda
* org-roam (capture templates include timestamps)
* org-roam-ui (view your zettelkasten like a mind-maps in your browser, very cool, similar to the one in Obsidian)
* pdf-tools
* pdf-noter

I built the latex-emacs profile on top of a configuration by [Alvaro Ortiz Troncoso](https://github.com/aot29/Emacs_Python_R_LaTeX).
His configuration included the `auto-complete-auctex.el` package of Chris Monsanto from 2012.
Although the autocompletion is out of style for Emacs fans at the cutting edge of package development, it works!

I designed this configuration to support the efficient editing of academic documents in LaTeX.
See the [Features](#Features) section below.
See the [related repos](#Related) section below for LaTeX templates for the first manuscript submission, a writing log to accompany every writing project, a poster, a slideshow, a diary for 2022, and a diary for 2023.

This configuration is compatible with [org-mode](https://de.wikipedia.org/wiki/Org-mode). 
Org-mode is an enhanced markdown that reads LaTeX code intermingled with org markup.
Org-mode is very popular with Emacs users because the sections and subsections are easy to fold, and org documents are easy to export to many file formats. 
It evolved out of an outliner package.
It greatly eases the creation of lists, so it is useful for planning and brainstorming.
Org-mode also has a powerful org-agenda component for scheduling events.
Many have tried to use org-mode to implement the Getting Things Done method of Jim Allen.
Org-mode also powers the [org-roam](https://www.orgroam.com/) package, which implements electronically the [zettelkasten system](https://wiki2.org/en/Zettelkasten) or slip-box system of taking smart notes.


In addition to Emacs, you can edit org-mode markdown in several other leading text editors like [Visual Studio Code](https://marketplace.visualstudio.com/items?itemName=BobbyBoyd.vs-org), so you can still use org-mode if you fear Emacs.
Org-mode is responsible for the recent resurgence in the popularity of Emacs. 
(The median age of an Emacs user is about 33 years).
The recurring annual [emacsconfs](https://emacsconf.org/) reflects this surge in popularity.
The two-day conference is virtual and free.

Org-mode supports literate programming with almost any programming language, although the latex-emacs init.el file is not configured for literate programming.
Instead, see my complete configuration for [Emacs](https://github.com/MooersLab/configorg).
Unlike the above configuration written in Emacs lisp (elisp), the complete configuration is wrapped in org-mode.
It is a 6000-line literature programming document. 
I wrote the comments in org-mode; they flank blocks of elisp code. 

You can read an org-mode document into Emacs with the *latex-emacs* configuration, even though the latex-emacs init.el file does not call org-mode because the base distribution of Emacs now contains org-mode.
See this [repo](https://github.com/MooersLab/manuscriptInOrg) for a manuscript template that can be edited in `org-mode`.
For a writing log to be used in parallel to writing a manuscript in org-mode, see this [repo] (https://github.com/MooersLab/writingLogTemplateInOrg). 

I am keeping the *latex-emacs* configuration isolated from my main configuration to ease the development of *latex-emacs*.
The *latex-emacs*  profile also serves as a backup configuration when I break my main configuration, which happens less often these days.

I use the following bash alias to start Emacs29 with the configuration stored in the folder *~/latex-emacs29*.
Note that I am not hiding this folder in my top directory.
The hindrance of hiding the Emacs configuration folder is greater than the benefit of not accidentally deleting it once in a lifetime. 
With my init.el file on GitHub, I can easily restore my Emacs configuration if disaster strikes.

```bash
alias e29l='/Applications/Emacs29.0.5.app/Contents/MacOS/Emacs --init-directory ~/latex-emacs29'.
```

The **--init-directory** flag has been available as of January 2022 with Emacs version 29.0.5.
This flag negates the need for the package chemacs2 and its slightly more complex setup.

I made a second variant of the above alias for debugging:

```bash
alias e29ld='/Applications/Emacs29.0.5.app/Contents/MacOS/Emacs --init-directory ~/latex-emacs29 --debug-init'.
```


## Multiple versions of Emacs on the same computer

You can have multiple versions of Emacs on your system; however, you must match the profiles to the Emacs version because the binaries in the `elpa` subfolder of the latex-emacs29 folder may not run with different versions of Emacs. 

No warranty is implied. 
Use at your own risk.

I used this configuration to prepare the [presentation](https://github.com/MooersLab/BerlinEmacsAugust2022) given to the Berlin Emacs Meetup on 31 August 2022.

## Why LaTeX?

LaTeX is the gold standard markup system for technical writing.

- Combine a passion for coding with writing (i.e., make writing fun by disguising it as coding).
- Code reuse: recycle old documents.
- Precise control over the appearance of the output.
- Equation typesetting par none.
- Automated scaling and placement of figures.
- Syntax highlighting of code fragments.
- Reduced use of the mouse: More ergonomic.
- Reduced context switching: Be more productive.
- Slideshows as PDFs are immune to embarrassing font issues when presenting slides on another operating system.
- Support for multi-part documents (books).
- Precise control over typesetting of the bibliography.
- Automated table of contents and list assembly.
- Automated index generation. 
- Source files are compact and easy to share by e-mail.
- You can put the source files under version control.
- Overleaf supports collaborative writing.

Most markdown variants do not support the automated generation of indices for large documents.
Most literate programming projects built on top of markdown have this serious limitation, but org-mode does not. See the section `Why Org`.


## Why Emacs?

Over the past several decades, many Emacs and package developers have developed a suite of packages to aid the writing of LaTeX documents.
These features support efficient editing of LaTeX documents.

In addition, you can extend Emacs with the Emacs Lisp programming language.
Elisp is a variant of LISP that was customized to ease Emacs configuration.
Emacs is a platform for building a customized text editor.
You can customize your LaTeX environment and add functions to aid your workflow.

Of course, other text editors have these features too, but Emacs is the most extensible.
It is the ultimate editor.
You might still use other editors, but Emacs will meet most of your needs.

Emacs has been under development for 45 years.
According to the [Lindy effect](https://wiki2.org/en/Lindy_Effect), it should be available for another 45 years.


## Why Overleaf?

Overleaf is a web-based LaTeX editor. 
Overleaf is excellent for maintaining multiple writing projects and strongly supports collaborative editing.
I find the Overleaf GUI intuitive and has good support for debugging.
The GUI provides helpful tools for generating the code for tables of various sizes and for selecting images to be inserted into figure environments.
It provides an outside view of the document.
I use this feature to generate outline views of the slide shows I am developing in Beamer.
I use this outline view to navigate to specific slides and to rearrange slides while editing the beamer file.
Overleaf also has an extensive collection of attractive documentation about LaTeX, a nice supplement to TeX StackExchange.

I have over 780 writing projects on Overleaf.
These projects include grant applications, manuscripts, slideshows, and posters.
I often use an old project as a template to make a new project.
This saves a lot of time.

I use it multiple times daily because I store my daily writing in a book document as my digital laboratory notebook.
I speed up navigation to active projects by storing their URLs on a private homepage (index.html).

I have subscribed to the Pro license for several years and have used Overleaf for six years.
I recommend the free account to anyone trying to master LaTeX.
I recommend the Pro license for anyone doing collaborative writing or managing dozens to 100s of projects.
The Pro license enables you to use Git to make copies of your writing projects on your computer using Git.

The reasons not to use Overleaf include the following: 

- pricey annual subscription
- poor support for Emacs key bindings
- lack of a system for storing snippets of custom LaTeX boilerplate
- time spent in Overleaf is not time spent in Emacs (but see workaround solution below via GhostText)


#### GhostText and Overleaf
Overleaf does support a small subset of Emacs keybindings.
Even better, if you select the legacy mode on Overleaf, you can send the text area to Emacs for editing via the GhostText plugin for your browser and the atomic-chrome package for Emacs.
You can use snippets on the Emacs side of the websocket; the inserted code will appear immediately on the Overleaf side.
My configuration for atomic-chrome is below:

```emacs-lisp
;; atomic-chrome, used to interact with GhostText extension for Google Chrome.
(use-package atomic-chrome)
(atomic-chrome-start-server)
(setq atomic-chrome-default-major-mode 'python-mode)
(setq atomic-chrome-extension-type-list '(ghost-text))
;;(atomic-chrome-start-httpd)
(setq atomic-chrome-server-ghost-text-port 4001)
(setq atomic-chrome-url-major-mode-alist
      '(("github\\.com" . gfm-mode)
        ("overleaf.com" . latex-mode)
        ("750words.com" . latex-mode)))
; Select the style of opening the editing buffer by atomic-chrome-buffer-open-style.
; full: Open in the selected window.
; split: Open in the new window by splitting the selected window (default).
; frame: Create a new frame and window in it. You must be using some windowing package.
(setq atomic-chrome-buffer-open-style 'split)
```

If you use the text area of Overleaf, 750words, or WriteHoney and you have Voice In plus installed in Google Chrom, you can carry out dictation in a text area that will be mirrored in Emacs via GhostText.
The advantage of doing this over using the `whisper.el` package is that you can dictate in longer spurts, and you can use your custom voice triggers to activate the insertion of text replacements.

For example, I abhor English contractions because I do not believe they will weather well over time. 
They also violate the principles of scientific writing, which are be clear, precise, and concise.
Above all, scientific writing must be clear.
Contractions reduce clarity, especially for non-native English speakers.
I have mapped almost 200 contractions to their expansions.
When I say an English contraction, it serves as a voice trigger that leads to the insertion of its expansion.
This spares me the trouble of correcting these manually.
I have also mapped expansions of acronyms to voice triggers so that I can accurately expand the acronym on the fly without looking it up for the 10th time.
I have also mapped various kinds of LaTeX codes to voice triggers.
See the bottom section of the Mooerslab GitHub landing page for more information about [how I use voice computing in my daily work](https://github.com/MooersLab#voice-computing).



### Advantages of using LaTeX in Emacs over Overleaf

- The availability of the *yasnippets* package. You can build document templates and save them as yasnippet snippets. You then insert the document template into a new file upon entering the snippet name and hitting tab.
- Powerful macros in AUCTeX support creating and editing new documents.
- Syntax checking before compiling is available. See the keybinding lists below.
- Terse and clear bug reporting during PDF compiling.
- Preview a rendered version of a select region in the buffer. You remove the preview with a key binding. The preview allows you to check an equation's typesetting without going through the longer step of compiling the file into a PDF.


## Why Org-mode

Org-mode includes support for document preparation, time management, and literate programming.
Many people in the past decade were drawn to Emacs by org-mode.
Org documents recognize the most commonly used features of LaTeX markup.
You can use a LaTeX preamble in the front matter of your org file to use your favorite LaTeX packages and settings.
You can make the PDF coming out of org look precisely like the PDF that you would generate from LaTeX.
You can use LaTeX to build indices, glossaries, list of acronyms, lists the tables, list of figures, lists of code listings, list of equations, and so on.

I now think of org-mode as a thin but very powerful wrapper around LaTeX.
By utilizing org-mode, you gain the very easy-to-assemble tables that org-mode provides.
You also gain some very convenient ways of utilizing Gnuplot to quickly generate plots.
You gain integration with org-roam for knowledge management because the notes in the system are org files.
You can use this knowledge management in lieu of or in addition to an annotated bibliography.
You gain interactive computing in source blocks.


The main drawbacks to org-mode for me are as follows:

- Most publishers do not accept org files, but they do accept tex files. (You can export the org file to a LaTeX file that might still need heavy editing of the preamble to get the publisher's acceptance.)
- Overleaf does not support the use of org files. 
- Org markup adds another layer of abstraction on top of LaTeX. This layer is thin and not that big of a deal.



<a id="Features"><h2>Features of this configuration:</h2></a>

- Install packages from MELPA with use-package.
- Uses Protesilaos Stavrou's (Prot's) [ef-themes](https://github.com/protesilaos/ef-themes) package. Set to the ef-cyprus theme. The ef-light theme is good too.
- latex-mode and auctex-mode with an old autocompletion package that works,
- Keybindings remapped for macOS so Meta is `cmd`, Super is `cap locks`, and Hyper is `Fn`.
- Numerous keybindings from *outline-mode* to fold and unfold sections in a LaTeX file.
- Configured to cope with the minted package under compiling. It gets around the `compile with -shell-escape` command.
- Configured to use snippets from the *snippets* subfolder.
- Has hippieexpand enabled.
- Uses the *awesome-tabs* package to enable navigation between buffers via tabs. (The keybindings s-0, s-1, ..., s9 enable switching tabs from the keyboard.)
- Has the word count mode (*wc-mode*) enabled.
- Includes pomodoro-mode.
- Has minibuffer history.
- Includes *ivy* enabled for enhanced auto-completion selections.
- *atomic-chrome* configured to interact with *GhostText* extension for Google Chrome so you can edit in Emacs LaTeX documents on Chrome.
- Includes the *texcount* script with `C-c w` to get a more accurate word count when in latex-mode.
- Move N-selected lines up and down with the meta and the arrow keys.
- Scratch buffer retained.
- Add the itemize, enumerate, and description functions to convert selected markdown lists into LaTeX itemize, description, or enumerated lists.
- Includes configuration for changing the size of the font in the modeline. This is useful for preparing slides about using Emacs.


## Installation

1. `git clone https://MooersLab/github.com/latex-emacs  latex-emacs` in your home directory.
2. Create link to your snippets folder:  `ln -s snippets ~/.emacs.d/snippets`. This link negates the need to maintain multiple copies of the snippet libraries.
3. Edit file paths in init.el as needed. Git clone the packages stored in manual-packages.
4. Add `alias e29l='/Applications/Emacs29.0.5.app/Contents/MacOS/Emacs --init-directory ~/latex-emacs29` to your `.bashrc` or `.zshrc` file so you can open Emacs with alias `e29l`. 
5. Add the GhostText extension to Chrome or Firefox or Safari or Edge.
6. Install LaTeX and include in your PATH.
7. Edit in `init.el` the file path to texcount. This Perl script comes with LaTeX. You might be able to invoke texcount without the file path if you have no previously defined aliases to do texcount.


## Operation

This setup can be run in versions > 29.0.5 with `emacs --init-directory ~/latex-emacs29` or the alias `e29l`.
This command ignores your default Emacs configuration in `~/.emacs.d`
You can run it in isolation from your default emacs configuration.
It makes a nice backup system when you break the main configuration.

Create an empty text file with `touch start.tex`.
Start Emacs and load the file.
Enter `C-o C-e` to create a new document using the corresponding [AUCTeX](https://www.gnu.org/software/auctex/) macro.


## Key bindings

### Useful key bindings: LaTeX specific
These keybindings are active in the latex-mode or useful in this mode.

|Key binding | Description|
|:-----------|:-----------|
|C-c w       | Run the texcount script on the buffer. It does not count the words in the LaTeX markup. |
|C-c C-j     | Enter at the end of a line for an item in the list. Then a new item is inserted into the next line. |
|M-RET       | To insert an item in the middle of the list. |
|C-c C-e     | Enter to get a prompt for the kind of environment.|
|C-c C-s     | Enter to insert a section or subsection.|
| C-c C-a    | Compile and open PDF in the default viewer.|
|C-c C-c     | Compile document.|
|C-c `       | Go to compile error.|
|C-c C-p C-s | Preview regions.|
| C-c =      | Open the navigable table of contents view of the document. |
|C-c C-k     | Kill the text file processing.   |
|C-c ;       | Comment out a region.|
|C-c %       | Comment out a paragraph.|
|C-c C-a     | Compile a LaTeX text file and display the PDF in the default PDF viewer.|
|C-j         | Add a new line and indent the new line.|
|M-q         | To fill in a paragraph (text-wrap) and indent the current paragraph.   |
|C-c C-q C-e | Fill and indent the current environment.|
|C-c C-q C-s | Fill and indent the current section.|
|C-c C-q C-r | Fill and indent the current region.|
|C-c {       | To insert open and close braces and position the cursor between the braces.  |
|C-c }       | To move the cursor past the right brace.|
|M-x tex-validate-buffer | Enter to check the entire buffer for syntax errors.|
|M-x tex-validate-region | Enter to check the entire region for syntax errors.|
|C-c C-f     | To process a file. |
|C-c C-v     | To view a DVI file. |
|C-c C-b     | To process the buffer.  |
|C-c TAB     | To run M-x tex-bibtex-file.  |


### Useful key bindings: General editing

If you are rusty or just starting to use Emacs, you need only these 23 key bindings to function in Emacs. 

|Key binding | Description|
|:-----------|:-----------|
|C-x C-c | Quit Emacs.|
|C-g     | Abort current command.|
|C-x C-f | Open a file.|
|M-!     | Open a shell, perhaps to create a file to open: touch new.tex.|
|C-SPC   | Mark the beginning of a selected region.|
|C-x SPC | Start marking rectangular box selection.|
|C-x u |  Undo last command.|
|C-M-_ | Redo undone changes.|
|C-x C-s | Save file.|
|C-x C-w | Save the current buffer to the specified filename.|
|C-y     | Paste clipboard. |
|C-w     | Cut and store in kill-ring (and clipboard).|
|M-w     | Copy to kill-ring (and clipboard). |
|s-c     | Copy (option-c). The init.el file above maps the Mac option key to `s`, the Super key.|
|C-k     | Delete line and store in kill ring (i.e., clipboard). Repeat to delete the new-line character.|
|C-a     | Move the cursor to the beginning of the line.|
|C-e     | Move the cursor to the end of the line.|
|C-x d   | Open dired for directory navigation.|
|C-h     | Invoke the help menu. |
|M-x package-list-packages | Refresh list of packages. Emacs may prompt you to install updates: enter u x y to upgrade packages.|
|M-x package-install | Must run `package-list-packages` first.|
|M-\     | Will join the two words by removing the whitespaces between two words. |
|M-d     | Delete word. |
|C-d     | Delete character. |
|M-t     | Transpose two words when the cursor is in the space between the two words. Useful for editing full names in BibTeX entries.|



Vim users can turn on evil mode to emulate Vim key bindings.

Uncommenting the corresponding code in the init. el file invokes CUA keybindings (C-c, C-v, and C-x for copy, paste, and cut).
However, there are conflicts with the LaTeX keybindings.
I recommend either using evil-mode or learning the essential Emacs key bindings; they are much easier to master than they look :).


## Quizzes

The recall of key bindings fades quickly after the first encounter but improves with spaced repetitions.
I have provided two sets of quizzes to improve recall:  One set for each set of the above key bindings.
The quizzes in the set have the same questions, but I randomized their order to break up your memory of the answers based on the context of the question in the quiz.

## Related projects by Others

### Peter Prevos's Emacs Writing Studio (EWS)

Peter Prevos developed a similar Emacs profile (i.e., init.el file) for academic writing focused on utilizing org-mode for document assembly, citar for bibliography management, and denote for knowledge management. He gave a talk about EWS in December 2025 at the emacsconf. He gave one of the best-organized talks at this conference. Furthermore, he has a playlist of five videos on YouTube about various aspects of deploying EWS.

He has also written a book about how to utilize EWS. The book is available for purchase as an e-book, but you can compile it via org-mode. Open the top-level org file ./Documents/book/00-emacs-writing-studio.org that imports the book's chapters and other parts. To make a PDF of the book, use the key binding `C-c C-e l o` to export it PDF and open it.

This book is groundbreaking in several regards. This is the first book about Emacs to discuss modern writing workflows utilizing packages like citar and denote. It is also the first Emacs book I know that is built around an `init.el` file. Hopefully, this book will inspire other authors to write books about their `init.el` files.

The first 50 pages of this book provide an excellent introduction to Emacs for both coders and writers. The following 100 pages are more focused on the writing process in EWS. The last 50 pages is an Appendix that walks through the init.el file and explains the roles of each of the settings and packages. This is one of the most valuable appendices I have ever read and one of the few I have read in one sitting from beginning to end.

The book lacks an index, which is surprising because utilizing the LaTeX indexing system inside org-mode is relatively trivial. He might have faced limitations with exporting indices to other publication formats beyond LaTeX. His intent was to make this book available in multiple formats.

I recommend taking a learning spiral approach to getting started with EWS. I first watched his emacsconf talk. I did not discover his YouTube videos until after I had installed EWS and read his book. If I were to do things over again, I would watch the YouTube videos before reading his book. His book is highly accessible, but I am a poor judge of what a beginner would think because I have been a daily user of Emacs for four years.

He developed several packages that bring functionality similar to that of org-roam. His citar-denote package integrates the use of citar to assist the generation of literature notes for use in denote. His denote-explore package provides tools for analyzing the distribution of notes across topics and generating directed graphs of links between notes. The first functionality is absent from org-roam. The second functionality mimics the org-roam-ui. These two packages are integrated into his init.el file. These are valuable contributions because Peter has made the denote package attractive to org-roam users and more accessible to academics.

The denote system of personal knowledge management relies on filenames for information retrieval. It does not utilize a database, like org-roam. The filename has five components. Retrieval of notes and their linking is based on elements of the filename. I find this approach to be very robust to future changes in technology.

However, assembling a personal Knowledge Management System can be quite a time-sink. Creating forward and backlinks is as time-consuming and tedious as in org-roam. I would not start such a knowledge base if I faced a deadline.

His profile is meant to be easy to install and requires little customization. You do have to define a few file paths, such as where you will install your denote notes. You will have to wait several minutes while the new packages are installed the first time that you launch Emacs with his init.el file. Emacs will start in 1-2 seconds the second time that you start Emacs with the EWS init.el file.

I have installed his profile for use with Emacs 30.1. An error message was reported, but it can be safely ignored. The profile works as expected. It uses a beautiful theme that makes working in Emacs more pleasant. I have started using his key bindings for denote and denote-explore. They are pleasant to work with.





<a id="Releted"><h2>My other related projects of possible interest</h2></a>

- [LaTeX manuscript template](https://github.com/MooersLab/manuscriptInLaTeX/edit/main/README.md)
- [Writing log template in LaTeX](https://github.com/MooersLab/writingLogTemplate)
- [Slideshow template in LaTeX](https://github.com/MooersLab/slideshowTemplateLaTeX)
- [Annotated bibliography Template in LaTeX](https://github.com/MooersLab/annotatedBibliography)
- [My custom elisp functions to make specific tasks easier in Emacs](https://github.com/MooersLab/mooerslab-functions.el)
- [Diary for 2024 in LaTeX](https://github.com/MooersLab/diary2024inLaTeX)
- [Snippets for latex-mode in Emacs](https://github.com/MooersLab/snippet-latex-mode)
- [Quizzes about Emacs to improve recall of keybindings](https://github.com/MooersLab/qemacs)
- [Slides from talk about GhostText, Data Science Workshop, July 2022](https://github.com/MooersLab/DSW22ghosttext)
- [Video link to talk about GhostText, Data Science Workshop, July 2022](https://mediasite.ouhsc.edu/Mediasite/Channel/python/watch/4da0872f028c4255ae12935655e911321d)
- [The writer's law (for stoics)](https://github.com/MooersLab/thewriterslaw)
- [Writing by dictation, suitable for those with repetitive stress injuries](https://github.com/MooersLab#voice-computing)

## Funding
- NIH: R01 CA242845, R01 AI088011
- NIH: P30 CA225520 (PI: R. Mannel); P20GM103640 and P30GM145423 (PI: A. West)

## Updates
|Version      | Changes                                                                                                                                    | Date                 |
|:-----------:|:-----------------------------------------------------------------------------:|:--------------------:|
| Version 0.2 |  Added funding and update table. Minor edits to the README.md                 | 2024 May 3           |
| Version 0.2.1 | Minor edits in the README.md.                                                                                                            | 2024 May 10          |
| Version 0.2.2| More minor edits in the README.md                                                                                                         | 2024 June 27         |
| Version 0.2.3| More minor edits in the README.md                                                                                                         | 2025 March 4         |
| Version 0.2.4| Added review of EWS to README.md. Updated related links section.                                                                                                         | 2025 March 5         |
