" Mappings

" Leader mappings
let mapleader = ","
map <Leader>f :NERDTree<CR>
map <Leader>b :CtrlP<CR>
"map <Leader>gs :GStatus

" Plugins
call plug#begin('~/.local/share/nvim/plugged')

Plug 'ctrlpvim/ctrlp.vim' 
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'

call plug#end()

