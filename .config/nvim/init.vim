
" Startup {{{
" }}}
" Core Mappings {{{
map <Space> <Leader>

"Clear search results
map <silent> <Leader>c :nohlsearch<CR>

"Open/close fold
noremap , za

" Use backtick for command line access
map ` :
"Reload vim config
map <Leader>rv :so ~/.config/nvim/init.vim<CR>

"Install plugins
map <Leader>pi :PlugInstall<CR>
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
map <Leader><CR> :CtrlP<CR>
let g:ctrlp_show_hidden = 1 "Show hidden files in control p
let g:ctrlp_user_command = 'ag %s -l -g "" --nocolor --hidden'
let g:ctrlp_use_caching = 0
" }}}

" Syntastic {{{
let g:syntastic_aggregate_errors = 1
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
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
map <Leader>u :GundoToggle<CR>
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
Plug 'tpope/vim-unimpaired'
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

" UI {{{
colorscheme wal
let g:airline_theme='term'
let g:airline_powerline_fonts=1
" }}}

" vim:foldmethod=marker:foldlevel=0
