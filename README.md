# latex-emacs: a GNU Emacs profile for LaTeX
## Make writing tasks fun be disguising them as coding projects!

This repo contains is a init.el file (configuration file) for using LaTeX in GNU Emacs.
It can probably be adapted to other configurations of Emacs like Spacemacs, Doom, and so on.
SciMax probably has similar support really built-in.

I built the latex-emacs profile on top of a configuration by [Alvaro Ortiz Troncoso](https://github.com/aot29/Emacs_Python_R_LaTeX).
His configuration included the auto-complete-auctex.el package of Chris Monsanto from 2012.
Although the autocompletion is out of style for Emacs aficinados, it works!

My configuration is designed to support efficient editing of academic documents in LaTeX.
See the [Features](#Features) section below.
See the [related repos](#Related) section below for LaTeX templates for the first manuscript submission, a writing log to accompany every writing project, a poster, a slideshow, a diary for 2022, and a diary for 2023.

This configuration is compatible with the use of org-mode, an enhanced form of markdown that can read LaTeX code intermingled with org markup and that is very popular with users of Emacs because the sections and subsections are easy to fold and org documents are easy to export to a number of different file formats.
Org-mode is largely responsible for the recent resurgence in the popularity Emacs, which is reflected in the recent annual [emacsconfs](https://emacsconf.org/).

Org-mode also supports literate programming with almost any programming language, although the latex-emacs init.el file is not configured for literate programming.
Instead, see my full configuration for [Emacs](https://github.com/MooersLab/configorg), which is actually written in org!

You can read a org document into Emacs with the *latex-emacs* configuration even though the latex-emacs init.el file does not call org because org is now built into the base distribution of Emacs.
See the this [repo](https://github.com/MooersLab/manuscriptInOrg) for a manuscript template written to be edited in org-mode.
See this [repo](https://github.com/MooersLab/writingLogTemplateInOrg) for a writing log to be used in parallel to the manuscript in org that is also written for use in org-mode.

I am keeping the *latex-eamcs* configuration isolated from my main configuration to ease its development.
It is also meant to serve as a backup configuration file when the main one is broken.

I have used this init file with the [chemacs2](https://github.com/plexus/chemacs2) package and the `--with-profile` flag.
If you have Emacs version 28.1 or earlier, you have to use this option.
Create a file called .emacs-profiles.el in you home directory.

```elisp
(("default" . ((user-emacs-directory . "~/.emacs.default")))
 ("latex" . ((user-emacs-directory . "~/latex-emacs")))
 )
```
Launch Emacs with `emacs --with-profile latex`.

I now use `--init-directory` flag pointed to the folder `latex-emacs`.
Well, actually use this following bash alias: 

```bash
alias e29l='/Applications/Emacs29.0.5.app/Contents/MacOS/Emacs --init-directory ~/latex-emacs'.
```

The use of this second flag does not require any configuration.
This flag is available as of January 2022 with Emacs version 29.0.5.
This bleeding edge build of GNU Emacs can be found under the nightlies builds [here](https://emacsformacos.com/builds) for the Mac, even though the version number is not explicit in the name of these builds.
These builds may have bugs, but I have not run into one in two years of using them.
Unfortunately, this flag is not available for Emacs 28.1. 

No warranty is implied. Use at your own risk.

I used this configuration to prepare the [presentation](https://github.com/MooersLab/BerlinEmacsAugust2022) given to the Berlin Emacs Meetup 31 August 2022.

## Why LaTeX?

LaTeX is the gold standard markup system for technical writing.

- Combine passion for coding with writing (e.i., make writing fun be disguising it as coding).
- Code reuse: recycle old documents.
- Precise control over appearance of output.
- Equation typesetting par none.
- Automated scaling and placement of figures.
- Syntax highlighting of code fragments.
- Reduced use of the mouse: More ergonomic.
- Reduced context switching: Be more productive.
- Slideshows as PDFs are immune to embarrassing font issues when presenting slides on some other computer.
- Support for multi-part documents (books).
- Precise control over typesetting of the bibliography.
- Automated table of contents and list assembly.
- Automated index generation. 
- Source files are compact and easy to share by e-mail.
- Source files can be put under version control.
- Overleaf supports collaborative writing.

Most flavors of markdown do not support for the automated generation of indices for large documents.
Most literate programming projects that are built on markdown have this serious limitation.


## Why Emacs?

Many Emacs and package developers have developed a suite of packages over the past several decades to aid the writing of LaTeX documents.
These features support efficient editing of LaTeX documents.

In addition, Emacs is an extensible text editor that is highly customizable with the Emacs Lisp programming language.
This means that you can customize your LaTeX environment and eventually add your own functions to aid your workflow.

Of course, other text editors have these features too.
However, Emacs seems to the most extensible.

It has been under development for 45 years.
According to the Lindy effect, it should be available for another 45 years.


## Why not Overleaf

Overleaf is web-based LaTeX editor.
It is great for maintaining multiple writing projects and collaborative editing.
Its interface is intuitive, and it is painless to compile documents into PDFs.
The compile times are several times faster on the Overleaf servers than on my computer.
Overleaf also has an extensive collection of attractive on-line documentation about LaTeX.

I have over 600s writing projects on it and have subscribed to the Pro license for several years.
I recommend the free account to anyone trying to master LaTeX.
I recommend the Pro license for anyone doing collaborative writing.
The Pro license also enables the use of git to make copy copies of the writing projects.

### Advantages of using LaTeX in Emacs over Overleaf

- The availability of the yasnippets package. You can build your own document templates and save them as yasnippet snippets. You then insert the document template into a new file the entering the snippet name and hitting tab.
- Powerful macros in AUCTeX support creating and editing new documents.
- Syntax checking before compiling is available. See keybindng lists below.
- Terse and clear bug reporting during PDF compiling.
- Preview a rendered version of a select region in the buffer. The preview can be removed with a key binding. This is great from checking the typesetting of an equation.


## Why not Org-mode

Org-mode includes support for document preparation, time management, and literate programming.
Many Emacs users in the past decade were drawn to Emacs by org-mode.
Org documentations can recognize LaTeX markup.
Exporting an org document through LaTeX to PDF is seemless.
I like using org-mode.

The main drawbacks for me are as follows:

- Most publishers do not accept org files while they do accept tex files.
- Overleaf does not support the use of org files. 
- Org markup adds another layer of abstraction on top of LaTeX.

I am already comfortable with LaTeX.
I do not need the crutch of org to produce tex files.


<a id="Features"><h2>Features:</h2></a>

- Install packages from MELPA with use-package.
- Uses Protesilaos Stavrou's (Prot's) [ef-themes](https://github.com/protesilaos/ef-themes) package, which I installed manually. 
- latex-mode and auctex-mode with an old autocompletion package that works,
- Keybindings remapped so Meta is `cmd`, Super is `cap locks`, and Hyper is `Fn` on the Mac.
- Numerous keybindings from outline-mode to fold and unfold sections in a LaTeX file.
- Configured to cope with the minted package under compiling. Gets around the `compile with -shell-escape` command.
- Configured to use snippets from yasnippets.
- Has hippieexpand enabled.
- Uses the awesome-tabs package to enable navigation between buffers via tabs. (I know, not good ergonomics.)
- Has the word count mode enabled.
- Has minibuffer history.
- Has ivy enabled for enhanced autocompletion selections.
- Atomic-chrome configured to interact with GhostText extension for Google Chrome so you can edit in Emacs LaTeX documents on Chrome.
- Use the texcount script with `C-c w` to get a more accurate word count in the latex-mode.
- Move lines up and down with the meta and the arrow keys.
- Scratch buffer retained.
- Add the itemize, enumerate, and description functions to convert selected markdown lists into LaTeX itemize, description, or enumerated lists.
- Included configuration for changing the size of the font in the modeline. This is useful for preparing slides about using Emacs.


## Installation

1. `git clone https://MooersLab/github.com/latex-emacs  latex-emacs` in your home directory.
2. Install Prot's ef-theme or comment out the corresponding code in init.el.
3. Create link to your snippets folder:  `ln -s snippets ~/.emacs.d/snippets`
4. Edit file paths in init.el as needed.
5. Add `alias e28l='emacs --with-profile latex-emacs` to your `.bashrc` or `.zshrc` file so you can open Emacs with alias `e28l`. Or with Emacs version 29.1, add `alias e29l='emacs --init-directory ~/latex-emacs'`.
6. Add the GhostText extension to Chrome.
7. Install LaTeX and include in your PATH.
8. Edit in init.el the file path to texcount, which is generally installed with LaTeX. You might be able invoke texcount without the path if there are no previously defined aliases to do it.


## Operation

This setup can be run in versions > 29.0.5 with `emacs --init-directory ~/latex-emacs`.
This command ignores your default Emacs configuration in `~/.emacs.d`
You can run it in isolation of your default emacs configuration.
Makes for a nice backup system when your main configuration is broken.

Create an empty tex file with `touch start.tex`.
Start Emacs and load the file.
Enter `C-o C-e` to create a new document using the correpsonding [AUCTeX](https://www.gnu.org/software/auctex/) macro.


## Key bindings

### Useful key bindings: LaTeX specific
These keybindings are active in the latex-mode or useful in this mode.

|Key binding | Description|
|:-----------|:-----------|
|C-c w       | Run the texcount script on the buffer. Does not count the words in the LaTeX markup. |
|C-c C-j     | Enter at the end the line and insert a new item on the next line. |
|M-RET       | To insert an item in the middle of the list. |
|C-c C-e     | Enter to get prompt for the kind of environment.|
|C-c C-s     | Enter to insert a section or subsection.|
| C-c C-a    | Compile and open PDF in default viewer|
|C-c C-c     | Compile document.|
|C-c `       | Got to compile error.|
|C-c C-p C-s | Preview regions.|
| C-c =      | Open navigable table of contents view of the document. |
|C-c C-k     | Kill the tex file processing.   |
|C-c ;       | Comment out a region.|
|C-c %       | Comment out a paragraph|
|C-c C-a     | Compile a LaTeX tex file and display the PDF in default PDF viewer.|
|C-j         | Add newline and indent the new line.|
|M-q         | To fill in a paragraph (text-wrap) and indent the current paragraph.   |
|C-c C-q C-e | Fill and indent the current environment.|
|C-c C-q C-s | Fill and indent the current section.|
|C-c C-q C-r | Fill and indent the current region.|
|C-c {       | To insert open and close braces and position the cursor between the braces.  |
|C-c }       | To move the cursor past the right brace.|
|M-x tex-validate-buffer | Enter to check the entire buffer for syntax errors.|
|M-x tex-validate-region | Enter to check the entire region for syntax errors.|
|C-c C-f     | To process a file. |
|C-c C-v     | To view a dvi file. |
|C-c C-b     | To process the buffer.  |
|C-c TAB     | To run M-x tex-bibtex-file.  |


### Useful key bindings: General editing

If you are rusty or starting out with Emacs, these 23 keys are all that you should master to function in Emacs. 

|Key binding | Description|
|:-----------|:-----------|
|C-x C-x | Quit Emacs|
|C-g     | Abort current command|
|C-x C-f | Open a file|
|M-!     | Open a shell, perhaps to create a file to open: touch new.tex|
|C-SPC   | Mark the beginning of a selected region.|
|C-x SPC | Start marking rectanglar box selection.|
|C-x u |  Undo|
|C-M-_ | Redo undone changes.|
|C-x C-s | Save file.|
|C-x C-w | Save file to specified filename.|
|C-y     | Paste clipboard. |
|C-w     | Cut |
|s-c     | Copy (option-c). The init.el file above maps the Mac option key to `s`, the Super key.|
|C-k     | Delete line and store in kill ring (i.e., clipboard). Repeat to delete new-line character.|
|C-a     | Move cursor to beginning of the line.|
|C-e     | Move cursor to the end of the line.|
|C-x d   | Open dired for directory navigation.|
|C-h     | Invoke the help menu. |
|M-x package-list-packages | Refresh list of packages. May be prompted to install updpates: enter u x y to upgrade packages.|
|M-x package-install | Must run `package-list-packages` first.|
|M-\     | Will join the two words by removing the whitespaces between two words. |
|M-d     | Delete word. |
|C-d     | Delete character. |



Vim users can turn on evil-mode to emulate vim key-bindings.

CUA keybindings (C-c, C-v, and C-x for copy, paste, and cut can be invoked by uncommenting the code in the init.el file).
However, there are conflicts with the latex keybindings.
I recommend either using evil-mode or just learn the essential Emacs key-bindings.


## Quizzes

The recall of key bindings fades quickly after the first encounter but improves with spaced repetitions.
I have provide two sets of quizzes to improve recall:  One set each set of the above key bindings.
The quizzes in a set have the same questions, but their order has been randomized.


<a id="Releted"><h2>Related projects of possible interest</h2></a>


- [LaTeX manuscript template](https://github.com/MooersLab/manuscriptInLaTeX/edit/main/README.md)
- [Writing log template in LaTeX](https://github.com/MooersLab/writingLogTemplate)
- [Slideshow template in LaTeX](https://github.com/MooersLab/slideshowTemplateLaTeX)
- [Annotated bibliography Template in LaTeX](https://github.com/MooersLab/annotatedBibliography)
- [Diary for 2022 in LaTeX](https://github.com/MooersLab/diary2022inLaTeX)
- [Diary for 2023 in LaTeX](https://github.com/MooersLab/diary2023inLaTeX)
- [latex-emacs profile](https://github.com/MooersLab/latex-emacs)
- [snippets for latex-mode in Emacs](https://github.com/MooersLab/snippet-latex-mode)
- [Quizzes about Emacs to improve recall of keybindings](https://github.com/MooersLab/qemacs)
- [Slides from talk about GhostText, Data Science Workshop, July 2022](https://github.com/MooersLab/DSW22ghosttext)
- [Video link to talk about GhostText, Data Science Workshop, July 2022](https://mediasite.ouhsc.edu/Mediasite/Channel/python/watch/4da0872f028c4255ae12935655e911321d)

