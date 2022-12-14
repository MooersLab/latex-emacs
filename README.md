# latex-emacs: a GNU Emacs profile for using LaTeX and Org in academic research
## Make writing tasks fun by disguising them as coding projects!

This repo contains a init.el file (configuration file) for using LaTeX in GNU Emacs.
You can adapt it to other configurations of Emacs (e.g., Spacemacs, Doom, etc).
The SciMax configuration probably has similar support built-in.

Short list of features include the following:

* heavily customized auto-complete for LaTeX
* AUCTeX
* atomic-chrome (GhostText)
* awesome-tabs (install from Github)
* dashboard
* org-mode
* org-agenda
* org-roam (capture templates include timestamps)
* org-roam-ui
* pdf-tools
* pdf-noter

I built the latex-emacs profile on top of a configuration by [Alvaro Ortiz Troncoso](https://github.com/aot29/Emacs_Python_R_LaTeX).
His configuration included the auto-complete-auctex.el package of Chris Monsanto from 2012.
Although the autocompletion is out of style for Emacs fans at the cutting edge of package development, it works!

I designed this configuration to support the efficient editing of academic documents in LaTeX.
See the [Features](#Features) section below.
See the [related repos](#Related) section below for LaTeX templates for the first manuscript submission, a writing log to accompany every writing project, a poster, a slideshow, a diary for 2022, and a diary for 2023.

This configuration is compatible with [org-mode](https://de.wikipedia.org/wiki/Org-mode). 
Org-mode is an enhanced form of markdown that can read LaTeX code intermingled with org markup.
Org-mode is very popular with users of Emacs because the sections and subsections are easy to fold, and org documents are easy to export to many file formats. 
It evolved out of an outliner package.
It greatly eases the creation of list, so it is useful for planning and brainstorming.
Org-mode also has a powerful org-agenda component.
Many have tried to use org-mode to implement the Getting Things Done method of Jim Allen.
Org-mode also helps power the [org-roam](https://www.orgroam.com/) package, which implements the [zettelkasten system](https://wiki2.org/en/Zettelkasten) or slip-box system of taking smart notes.


In addition to Emacs, you can edit org-mode markdown in several other leading text editors like [Visual Studio Code](https://marketplace.visualstudio.com/items?itemName=BobbyBoyd.vs-org), so you can still use org-mode if you fear Emacs.
Org-mode is responsible for the recent resurgence in the popularity of Emacs. The recurring annual [emacsconfs](https://emacsconf.org/) reflects this surge in popularity.
The two-day conference is virtual and free.

Org-mode supports literate programming with almost any programming language, although the latex-emacs init.el file is not configured for literate programming.
Instead, see my complete configuration for [Emacs](https://github.com/MooersLab/configorg).
Unlike the above configuration written in Emacs lisp (elisp), the complete configuration is wrapped in org-mode.
It is a 6000-line literature programming document. 
I wrote the comments in org-mode; they flank blocks of elisp code. 

You can read an org-mode document into Emacs with the *latex-emacs* configuration, even though the latex-emacs init.el file does not call org-mode because the base distribution of Emacs now contains org-mode.
See the this [repo](https://github.com/MooersLab/manuscriptInOrg) for a manuscript template that can be edited in org-mode.
See this [repo](https://github.com/MooersLab/writingLogTemplateInOrg) for a writing log to be used in parallel to writing a manuscript in org-mode. 

I am keeping the *latex-emacs* configuration isolated from my main configuration to ease the development of  *latex-emacs* .
The *latex-emacs*  profile also serves as a backup configuration when I break my main configuration, which happens less often these days.

I have used this init file with the [chemacs2](https://github.com/plexus/chemacs2) package and the `--with-profile` flag.
If you have Emacs version 28.1 or earlier, you must use this option.
Create a file called .emacs-profiles.el in your home directory.

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
The bleeding edge builds of GNU Emacs can be found under the nightlies builds [here](https://emacsformacos.com/builds) for the Mac, even though the version number is not explicit in the name of these builds.
These builds may have bugs, but I have not run into one in two years of using them.
Unfortunately, this flag is not available for Emacs 28.2. 

## Multiple versions of Emacs on same computer

You can have multiple versions of Emacs on your system; however, you will have to match the profiles to the Emacs version because the binaries in the elpa subfolder may not run with different versions of Emacs. 
To run emacs 28.2, stable version of Emacs throughout most of 2022, I had to use my chemax setup.
I copied my latex-emacs folder to latex-emacs28.
I defined latex28 in the emacs.profiles.el file.
I made the following bash alias to Emacs version 28.2 that uses the latex28 profile with the debug on:

```bash
alias e28ld='/Applications/Emacs28.2.app/Contents/MacOS/Emacs --with-profile latex28 --debug-init'
```

I then deleted the everything inside of the elpa subfolder.
I just enter `e28ld` to launch.


(Note: as of December 2022, I am using the bleeding edge Emacs 30.5. There are no issues so far.)

No warranty is implied. Use at your own risk.

I used this configuration to prepare the [presentation](https://github.com/MooersLab/BerlinEmacsAugust2022) given to the Berlin Emacs Meetup on 31 August 2022.

## Why LaTeX?

LaTeX is the gold standard markup system for technical writing.

- Combine a passion for coding with writing (e.i., make writing fun by disguising it as coding).
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
Most literate programming projects built on top of markdown have this serious limitation.


## Why Emacs?

Many Emacs and package developers have developed a suite of packages over the past several decades to aid the writing of LaTeX documents.
These features support efficient editing of LaTeX documents.

In addition, you can extend Emacs with the Emacs Lisp programming language.
Elisp is a variant of LISP that was customized to ease the configuring of Emacs.
Emacs is essentially a platform for building a customized text editor.
You can customize your LaTeX environment and eventually add functions to aid your workflow.

Of course, other text editors have these features too.
However, Emacs seems to me to be the most extensible.
In my opinion, it is the ultimate editor: Once you become competent at it, you will not make another editor your primary editor.

Emacs has been under development for 45 years.
According to the [Lindy effect](https://wiki2.org/en/Lindy_Effect), it should be available for another 45 years.


## Why not Overleaf

Overleaf is a web-based LaTeX editor. Overleaf is excellent for maintaining multiple writing projects and strongly supports collaborative editing.
I find the Overleaf GUI to be intuitive.
The compile times are several times faster on the Overleaf servers than on my computer.
Overleaf also has an extensive collection of attractive documentation about LaTeX.

I have over 600s writing projects on Overleaf and have subscribed to the Pro license for several years and have used it for six years.
I recommend the free account to anyone trying to master LaTeX.
I recommend the Pro license for anyone doing collaborative writing.
The Pro license also enables the use of git to make copies of your writing projects on your computer easily.

### Advantages of using LaTeX in Emacs over Overleaf

- The availability of the snippets package. You can build document templates and save them as yasnippet snippets. You then insert the document template into a new file upon entering the snippet name and hitting tab.
- Powerful macros in AUCTeX support creating and editing new documents.
- Syntax checking before compiling is available. See the keybinding lists below.
- Terse and clear bug reporting during PDF compiling.
- Preview a rendered version of a select region in the buffer. You remove the preview with a key binding. The preview allows you to check an equation's typesetting without going through the longer step of compiling the file into a PDF.


## Why not Org-mode

Org-mode includes support for document preparation, time management, and literate programming.
Many people in the past decade were drawn to Emacs by org-mode.
Org documentats can recognize LaTeX markup.
Exporting an org document through LaTeX to a PDF and having that PDF open in your default PDF-viewer is seamless.
However, to be honest, Overleaf is even faster.

The main drawbacks for me are as follows:

- Most publishers do not accept org files, while they do accept text files. (You can export the org file to a LaTeX file that might need heavy editing to get it accepted by the publisher.)
- Overleaf does not support the use of org files. 
- Org markup adds another layer of abstraction on top of LaTeX.

I am already comfortable with LaTeX.
I do not need the crutch of org to produce tex files.


<a id="Features"><h2>Features:</h2></a>

- Install packages from MELPA with use-package.
- Uses Protesilaos Stavrou's (Prot's) [ef-themes](https://github.com/protesilaos/ef-themes) package. Set to the ef-spring theme. The ef-light theme is good too.
- latex-mode and auctex-mode with an old autocompletion package that works,
- Keybindings remapped so Meta is `cmd`, Super is `cap locks`, and Hyper is `Fn` on the Mac.
- Numerous keybindings from *outline-mode* to fold and unfold sections in a LaTeX file.
- Configured to cope with the minted package under compiling. It gets around the `compile with -shell-escape` command.
- Configured to use snippets from the *snippets* subfolder.
- Has hippieexpand enabled.
- Uses the *awesome-tabs* package to enable navigation between buffers via tabs. (I know, not good ergonomics.)
- Has the word count mode (*wc-mode*) enabled.
- Has pomodoro-mode.
- Has minibuffer history.
- Has *ivy* enabled for enhanced auto-completion selections.
- *atomic-chrome* configured to interact with *GhostText* extension for Google Chrome so you can edit in Emacs LaTeX documents on Chrome.
- Use the *texcount* script with `C-c w` to get a more accurate word count in the latex-mode.
- Move N-selected lines up and down with the meta and the arrow keys.
- Scratch buffer retained.
- Add the itemize, enumerate, and description functions to convert selected markdown lists into LaTeX itemize, description, or enumerated lists.
- Includes configuration for changing the size of the font in the modeline. This is useful for preparing slides about using Emacs.


## Installation

1. `git clone https://MooersLab/github.com/latex-emacs  latex-emacs` in your home directory.
2. Install Prot's ef-theme or comment out the corresponding code in init.el.
3. Create link to your snippets folder:  `ln -s snippets ~/.emacs.d/snippets`
4. Edit file paths in init.el as needed.
5. Add `alias e28l='emacs --with-profile latex-emacs` to your `.bashrc` or `.zshrc` file so you can open Emacs with alias `e28l`. Or with Emacs version 29.0.5, add `alias e29l='emacs --init-directory ~/latex-emacs'`.
6. Add the GhostText extension to Chrome.
7. Install LaTeX and include in your PATH.
8. Edit in init.el the file path to texcount. This Perl script comes with LaTeX. You might be able to invoke texcount without the file path if you have no previously defined aliases to do texcount.


## Operation

This setup can be run in versions > 29.0.5 with `emacs --init-directory ~/latex-emacs`.
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
| C-c C-a    | Compile and open PDF in default viewer|
|C-c C-c     | Compile document.|
|C-c `       | Go to compile error.|
|C-c C-p C-s | Preview regions.|
| C-c =      | Open navigable table of contents view of the document. |
|C-c C-k     | Kill the text file processing.   |
|C-c ;       | Comment out a region.|
|C-c %       | Comment out a paragraph|
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
|C-x C-x | Quit Emacs|
|C-g     | Abort current command|
|C-x C-f | Open a file|
|M-!     | Open a shell, perhaps to create a file to open: touch new.tex|
|C-SPC   | Mark the beginning of a selected region.|
|C-x SPC | Start marking rectangular box selection.|
|C-x u |  Undo|
|C-M-_ | Redo undone changes.|
|C-x C-s | Save file.|
|C-x C-w | Save file to the specified filename.|
|C-y     | Paste clipboard. |
|C-w     | Cut |
|s-c     | Copy (option-c). The init.el file above maps the Mac option key to `s`, the Super key.|
|C-k     | Delete line and store in kill ring (i.e., clipboard). Repeat to delete the new-line character.|
|C-a     | Move cursor to the beginning of the line.|
|C-e     | Move the cursor to the end of the line.|
|C-x d   | Open dired for directory navigation.|
|C-h     | Invoke the help menu. |
|M-x package-list-packages | Refresh list of packages. Emacs may prompt you to install updates: enter u x y to upgrade packages.|
|M-x package-install | Must run `package-list-packages` first.|
|M-\     | Will join the two words by removing the whitespaces between two words. |
|M-d     | Delete word. |
|C-d     | Delete character. |



Vim users can turn on evil-mode to emulate vim key-bindings.

CUA keybindings (C-c, C-v, and C-x for copy, paste, and cut) can be invoked by uncommenting the code in the init.el file.
However, there are conflicts with the latex keybindings.
I recommend either using evil-mode or learn the essential Emacs key bindings; they are much easier to master than they look :).


## Quizzes

The recall of key bindings fades quickly after the first encounter but improves with spaced repetitions.
I have provided two sets of quizzes to improve recall:  One set each set of the above key bindings.
The quizzes in a set have the same questions, but I randomized their order.


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
- [The writer's law](https://github.com/MooersLab/thewriterslaw)
- [Ghost]()
