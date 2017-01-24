
" Startup {{{
" }}}

" Core Mappings {{{

" Basic mappings {{{
map <Space> <Leader>
map <CR> :
"}}}

" Folding mappings {{{
noremap , za
"}}}

" Buffer mappings {{{
map <Leader>d <Leader>v:count:bd<CR> 
map <Leader>1 <Plug>AirlineSelectTab1
map <Leader>2 <Plug>AirlineSelectTab2
map <Leader>3 <Plug>AirlineSelectTab3
map <Leader>4 <Plug>AirlineSelectTab4
map <Leader>5 <Plug>AirlineSelectTab5
map <Leader>6 <Plug>AirlineSelectTab6
map <Leader>7 <Plug>AirlineSelectTab7
map <Leader>8 <Plug>AirlineSelectTab8
map <Leader>9 <Plug>AirlineSelectTab9
"}}}

" Search mappings {{{
map <silent> <Leader>c :nohlsearch<CR>
map <Leader>rv :so ~/.config/nvim/init.vim<CR>
map <Leader>pi :PlugInstall<CR>
" }}}
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
set completeopt-="preview"
" }}}

" NERDTree {{{
map <Leader>f :NERDTreeToggle<CR>
map <Leader>F :NERDTree %<CR>
" }}}

" CtrlP {{{
map <Leader><CR> :CtrlPBuffer<CR>
map <Leader>o :CtrlP<CR>
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
let g:syntastic_cs_checkers=['syntax', 'semantic', 'issues']
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
call plug#begin()

" Core {{{
Plug 'ctrlpvim/ctrlp.vim' 
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-repeat'
Plug 'sjl/gundo.vim'
Plug 'dylanaraps/wal'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-syntastic/syntastic'
Plug 'ervandew/supertab'
Plug 'airblade/vim-gitgutter'
" }}}

" Haskell {{{
Plug 'itchyny/vim-haskell-indent'
" }}}

" C# {{{
Plug 'OmniSharp/omnisharp-vim'
" }}}

" AADL {{{
Plug 'OpenAADL/AADLib', {'rtp': 'share/vim/'}
" }}}

call plug#end()
" }}}

" Filetype options{{{

" C#{{{
let g:OmniSharp_selector_ui='ctrlp'
let g:OmniSharp_want_snippet=1
" }}}
" }}}
" UI {{{
colorscheme wal
let g:airline_theme='term'
let g:airline_powerline_fonts=1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_idx_mode = 1
let g:airline#extensions#tabline#buffer_nr_show = 1
let g:airline#extensions#tabline#tab_nr_type = 1 " tab number
let g:airline#extensions#tabline#show_tab_nr = 1
let g:airline#extensions#tabline#fnamemod = ':t:.'
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'
let g:airline#extensions#whitespace#symbol = '|'
" }}}

" vim:foldmethod=marker:foldlevel=0
