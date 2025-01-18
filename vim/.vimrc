syntax on
set incsearch
set tabstop=4
set softtabstop=4
set shiftwidth=4
set smartindent
set autoindent
set encoding=utf-8
colorscheme handmade-hero " Make sure to download handmade-hero theme!
set clipboard=unnamedplus
set rnu nu
set ignorecase
set smartcase
set showmatch
set backspace=indent,eol,start
set splitbelow
set splitright
set hlsearch
set nobackup
set noswapfile
set listchars=tab:>.,trail:~,extends:>,precedes:<
set list
set cursorline
set autoread
set autowrite
set hidden
set foldenable
set foldmethod=marker
set cmdheight=2
set updatetime=300
set shortmess+=c

let g:mapleader=' '

nmap <leader>w :w!<cr>

" Remove trailing whitespace from Python and Fortran files
autocmd BufWritePre *.py :%s/\s\+$//e

" Disable Highlighting
map <silent> <leader><cr> :noh<cr>

" File explorer
map <silent> <leader>op :Lexplore<cr>

" Shifting selected line(s)
vmap <Tab> >
vmap <S-Tab> <

" Color column
nnoremap <leader>cc :set colorcolumn=80<cr>
nnoremap <leader>ncc :set colorcolumn-=80<cr>

" Working with pane
nnoremap <leader>wv :vsplit<CR>
nnoremap <leader>ws :split<CR>
nnoremap <leader>wh :wincmd h<CR>
nnoremap <leader>wl :wincmd l<CR>
nnoremap <leader>wk :wincmd k<CR>
nnoremap <leader>wj :wincmd j<CR>
nnoremap <leader>w= :wincmd =<CR>

" Clipboard stuff
nnoremap p "+p
vnoremap p "+p
nnoremap P "+P
vnoremap P "+P
nnoremap y "+y
vnoremap y "+y

" buffer and airline config
nnoremap <leader>b[ :bprevious<CR>
nnoremap <leader>b] :bnext<CR>
nmap <leader>b1 :bfirst<CR>
nmap <leader>b2 :bfirst<CR>:bn<CR>
nmap <leader>b3 :bfirst<CR>:2bn<CR>
nmap <leader>b4 :bfirst<CR>:3bn<CR>
nmap <leader>b5 :bfirst<CR>:4bn<CR>
nmap <leader>b6 :bfirst<CR>:5bn<CR>
nmap <leader>b7 :bfirst<CR>:6bn<CR>
nmap <leader>b8 :bfirst<CR>:7bn<CR>
nmap <leader>b9 :bfirst<CR>:8bn<CR>
nmap <leader>b0 :bfirst<CR>:9bn<CR>
nmap <leader>bw :bwipe<CR>
nnoremap <leader>b :buffers<CR>:buffer<Space>

" Custom commands
command VimConfig :e ~/.vimrc " You might need to change the dir to .vimrc

