
" Startup {{{
" }}}
" Core Mappings {{{
let mapleader = "\<Space>"
map <Leader><CR> :nohlsearch<CR> "Clear search results
noremap , za "Open/close fold
map <Leader>ev :e ~/.config/nvim/init.vim<CR> "Edit vim config
map <Leader>rv :so ~/.config/nvim/init.vim<CR> "Reload vim config
map <Leader>pi :PlugInstall<CR> "Install plugins
" }}}

" Folding {{{
set foldenable "Enable folding
set foldlevelstart=10 "Start new buffers with folds of level 10 or greater as folded
set foldnestmax=10 "Don't allow folding greater than level 10
set foldmethod=indent "Fold based on an indent level
" }}}

" Misc Options {{{
filetype plugin indent on
set modelines=1
set hidden "Enable hidden buffers
set number "Enable line numbers
set showmatch "Show matching characters (parentheses, brackets, etc.)
set incsearch "Search as characters are entered
set hlsearch "Highlight search matches
let g:python_host_prog = '/usr/bin/python'
" }}}

" NERDTree {{{
map <Leader>f :NERDTreeToggle<CR>
map <Leader>o :NERDTree %<CR>
" }}}

" CtrlP {{{
map <Leader><Space> :CtrlP<CR>
let g:ctrlp_show_hidden = 1 "Show hidden files in control p
let g:ctrlp_user_command = 'ag %s -l -g "" --nocolor --hidden'
let g:ctrlp_use_caching = 0
" }}}

" Syntastic {{{
let g:syntastic_aggregate_errors = 1
" }}}

" Fugitive {{{
map <Leader>gs :Gstatus<CR>
map <Leader>gc :Gcommit -m
map <Leader>gm :Gmerge
map <Leader>gpl :Gpull
map <Leader>gps :Gpush
map <Leader>gf :Gfetch
map <Leader>gg :Ggrep
map <Leader>gl :Glog
map <Leader>gd :Gdiff
map <Leader>gb :Gblame
" }}}

" Gundo {{{
map <Leader>u :
" }}}

" Plugins {{{
call plug#begin('~/.local/share/nvim/plugged')

" Core {{{
Plug 'ctrlpvim/ctrlp.vim' 
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-dispatch'
Plug 'sjl/gundo.vim'
Plug 'dylanaraps/wal'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-syntastic/syntastic'
Plug 'ervandew/supertab'
" }}}

" Haskell {{{
Plug 'itchyny/vim-haskell-indent'
" }}}

" C# {{{
Plug 'OmniSharp/omnisharp-vim'
" }}}

call plug#end()
" }}}

" Visual {{{
colorscheme wal
let g:airline_theme='term'
let g:airline_powerline_fonts=1
" }}}

" vim:foldmethod=marker:foldlevel=0
