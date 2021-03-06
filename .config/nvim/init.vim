
" Startup {{{
" augroup configgroup
"     autocmd FileType hs setlocal expandtab
"     autocmd FileType hs setlocal tabstop = 8
"     autocmd FileType hs setlocal softtabstop = 4
"     autocmd FileType hs setlocal shiftwidth = 4
" }}}

" Core Mappings {{{

" Basic mappings {{{
map <Space> <Leader>
let maplocalleader = ","
map <CR> :
"}}}

" Buffer mappings {{{
map <Leader>d :bd<CR> 
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

" Split mappings {{{
noremap <Leader>j <C-w>j
noremap <Leader>k <C-w>k
noremap <Leader>h <C-w>h
noremap <Leader>l <C-w>l
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
let loaded_netrwPlugin=1
" }}}

" Ranger {{{
let g:ranger_map_keys = 0
map <Leader>t :Ranger<CR>
" }}}

" CtrlP {{{
map <Leader>fo :CtrlP<CR>
map <Leader>fl :CtrlPBuffer<CR>
map <Leader>fu :CtrlPMRU<CR>
let g:ctrlp_show_hidden = 1 "Show hidden files in control p
let g:ctrlp_user_command = 'ag %s -l -g "" -f --nocolor --hidden'
let g:ctrlp_use_caching = 0
" }}}

" Syntastic {{{
let g:syntastic_aggregate_errors = 1
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_wq = 0
let g:syntastic_check_on_wq = 1
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
Plug 'ctrlpvim/ctrlp.vim', { 'on' : ['CtrlP', 'CtrlPBuffer', 'CtrlPMRU'] }
Plug 'tpope/vim-fugitive'
Plug 'xolox/vim-easytags'
Plug 'xolox/vim-misc'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-sleuth'
Plug 'francoiscabrol/ranger.vim' , {'on' : 'Ranger'}
Plug 'rbgrouleff/bclose.vim'
Plug 'sjl/gundo.vim', { 'on' : 'GundoToggle' }
Plug 'dylanaraps/wal'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-syntastic/syntastic'
Plug 'ervandew/supertab'
Plug 'SirVer/ultisnips'
Plug 'Valloric/YouCompleteMe'
Plug 'honza/vim-snippets'
Plug 'airblade/vim-gitgutter'
" }}}

" Haskell {{{
Plug 'itchyny/vim-haskell-indent'
" }}}

" C# {{{
Plug 'OmniSharp/omnisharp-vim', { 'for' : 'cs' }
" }}}

" AADL {{{
Plug 'OpenAADL/AADLib', { 'rtp': 'share/vim/', 'for' : 'aadl' }
" }}}

" Prolog {{{
Plug 'adimit/prolog.vim', { 'for' : 'pl'}
" }}}

" Latex {{{
Plug 'lervag/vimtex', { 'for' : ['tex', 'plaintex', 'latex'] }
" }}}

call plug#end()
" }}}

" Filetype options{{{

" C#{{{

" Options{{{
let g:OmniSharp_selector_ui='ctrlp'
let g:OmniSharp_want_snippet=1
" }}}

" Keybindings{{{
map <LocalLeader>b :OmniSharpBuildAsync<CR>
map <LocalLeader>g :OmniSharpGotoDefinition<CR>
map <LocalLeader>i :OmniSharpFindImplementations<CR>
map <LocalLeader>ft :OmniSharpFindType<CR>
map <LocalLeader>fs :OmniSharpFindSymbol<CR>
map <LocalLeader>fu :OmniSharpFindUsages<CR>
map <LocalLeader>fm :OmniSharpFindMembers<CR>
map <LocalLeader>x :OmniSharpFixIssue<CR>
map <LocalLeader>l :OmniSharpTypeLookup<CR>
map <LocalLeader>d :OmniSharpDocumentation<CR>
map <LocalLeader>a :OmniSharpGetCodeActions<CR>
map <LocalLeader>r :OmniSharpRename<CR>
map <LocalLeader>m :OmniSharpCodeFormat<CR>
map <LocalLeader>s :OmniSharpReloadSolution<CR>
" }}}

" }}}

" Latex{{{
let g:vimtex_complete_close_braces = 1 
" }}}
" UI {{{
colorscheme wal
let g:airline_theme='term'
let g:airline_powerline_fonts=1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_idx_mode = 1
let g:airline#extensions#tabline#tab_nr_type = 1 " tab number
let g:airline#extensions#tabline#show_tab_nr = 1
let g:airline#extensions#tabline#fnamemod = ':t:.'
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'
let g:airline#extensions#whitespace#symbol = '|'
" }}}

" vim:foldmethod=marker:foldlevel=0
