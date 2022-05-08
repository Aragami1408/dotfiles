call plug#begin('~/.local/share/nvim/plugged')
	" productivity
	Plug 'preservim/nerdcommenter'
	Plug 'easymotion/vim-easymotion'
	Plug 'Xuyuanp/nerdtree-git-plugin'
	Plug 'terryma/vim-multiple-cursors'
	Plug 'junegunn/vim-easy-align'
	Plug 'scrooloose/nerdtree'

	" colorscheme
	Plug 'morhetz/gruvbox'
	Plug 'phanviet/vim-monokai-pro'

	" airline bar
	Plug 'vim-airline/vim-airline'
	Plug 'vim-airline/vim-airline-themes'

	" fzf
	Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
	Plug 'junegunn/fzf.vim'

	" lsp
	Plug 'neoclide/coc.nvim', {'branch': 'release'}
	Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

call plug#end()

" Basic config section
colorscheme gruvbox
set background=dark
set clipboard=unnamedplus
set nu rnu
augroup numbertoggle
	autocmd!
	autocmd BufEnter,FocusGained,InsertLeave * set norelativenumber
	autocmd BufLeave,FocusLost,InsertEnter * set relativenumber
augroup end
set autoread
set autowrite
set autoindent
set si
set cindent
set ft=nasm
syntax on
set nobackup
set nowb
set noswapfile
set backupdir=~/tmp,/tmp
set backupskip=/tmp/*,$TMPDIR/*,$TMP/*,$TEMP/*
set directory=/tmp
set encoding=utf-8
set hidden
set foldenable
set foldmethod=marker
set cursorline
set cmdheight=2
set updatetime=300
set shortmess+=c
set list
set listchars=tab:>·,trail:~,extends:>,precedes:<
set backspace=eol,start,indent
set whichwrap+=<,>,h,l
set undofile
set undodir=$HOME/.config/nvim/undo
set undolevels=1000
set undoreload=10000
set updatetime=300
set tabstop=4 softtabstop=0 noexpandtab shiftwidth=4
set termguicolors

command! -nargs=0 VimConfig :e ~/.config/nvim/init.vim
command! -nargs=0 TermConfig :e ~/.config/alacritty/alacritty.yml

set splitbelow
set splitright

" Floating Term
let s:float_term_border_win = 0
let s:float_term_win = 0
function! FloatTerm(...)
	" Configuration
	let height = float2nr((&lines - 2) * 0.8)
	let row = float2nr((&lines - height) / 2)
	let width = float2nr(&columns * 0.8)
	let col = float2nr((&columns - width) / 2)
	" Border Window
	let border_opts = {
				\ 'relative': 'editor',
				\ 'row': row - 1,
				\ 'col': col - 2,
				\ 'width': width + 4,
				\ 'height': height + 2,
				\ 'style': 'minimal'
				\ }
	" Terminal Window
	let opts = {
				\ 'relative': 'editor',
				\ 'row': row,
				\ 'col': col,
				\ 'width': width,
				\ 'height': height,
				\ 'style': 'minimal'
				\ }
	let top = "+" . repeat("~", width + 2) . "+"
	let mid = "#" . repeat(" ", width + 2) . "#"
	let bot = "+" . repeat("*", width + 2) . "+"
	let lines = [top] + repeat([mid], height) + [bot]
	let bbuf = nvim_create_buf(v:false, v:true)
	call nvim_buf_set_lines(bbuf, 0, -1, v:true, lines)
	let s:float_term_border_win = nvim_open_win(bbuf, v:true, border_opts)
	let buf = nvim_create_buf(v:false, v:true)
	let s:float_term_win = nvim_open_win(buf, v:true, opts)
	" Styling
	hi FloatWinBorder guifg=#87bb7c
	call setwinvar(s:float_term_border_win, '&winhl', 'Normal:FloatWinBorder')
	call setwinvar(s:float_term_win, '&winhl', 'Normal:Normal')
	if a:0 == 0
		terminal
	else
		call termopen(a:1)
	endif
	startinsert
	" Close border window when terminal window close
	autocmd TermClose * ++once :bd! | call nvim_win_close(s:float_term_border_win, v:true)
endfunction

let g:mapleader = " "

nmap <leader>w :w!<cr>

map <silent> <leader><cr> :noh<cr>




" Shifting selected line(s)
vmap <Tab> >
vmap <S-Tab> <

" Working with pane
nnoremap <leader>wv :vsplit<CR>
nnoremap <leader>ws :split<CR>
nnoremap <leader>wh :wincmd h<CR>
nnoremap <leader>wl :wincmd l<CR>
nnoremap <leader>wk :wincmd k<CR>
nnoremap <leader>wj :wincmd j<CR>
nnoremap <leader>w= :wincmd =<CR>


" Floating terminal (useful for compiling stuffs on-site)
nnoremap <leader>tt :call FloatTerm()<CR>
nnoremap <leader>tg :call FloatTerm('"lazygit"')<CR>
nnoremap <leader>tn :call FloatTerm('"node"')<CR>
nnoremap <leader>tp :call FloatTerm('"python"')<CR>
nnoremap <leader>th :call FloatTerm('"ghci"')<CR>
nnoremap <leader>tl :call FloatTerm('"sbcl"')<CR>

nnoremap <leader>p "+p
vnoremap <leader>p "+p
nnoremap <leader>P "+P
vnoremap <leader>P "+P
nnoremap <leader>y "+y
vnoremap <leader>y "+y

" NERDTree config section
let NERDTreeMinimalUI=1

nnoremap <leader>op :NERDTreeToggle<cr>

autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists('s:std_in') | NERDTree | endif

autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists('s:std_in') | execute 'NERDTree' argv()[0] | wincmd p | enew | execute 'cd '.argv()[0] | endif

autocmd BufEnter * if bufname('#') =~ 'NERD_tree_\d\+' && bufname('%') !~ 'NERD_tree_\d\+' && winnr('$') > 1 | let buf=bufnr() | buffer# | execute "normal! \<C-W>w" | execute 'buffer'.buf | endif

let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'
let g:NERDTreeShowHidden=1

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
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'
let g:airline_theme='owo'

" fzf
nnoremap <leader>ff :Files<cr>
nnoremap <leader>fg :GFiles<cr>
nnoremap <leader>fb :Buffers<cr>
nnoremap <leader>fh :Tags<cr>

" easy align
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" lsp
if has("nvim-0.5.0") || has("patch-8.1.1564")
  set signcolumn=number
else
  set signcolumn=yes
endif

inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

nmap <silent> <leader>[g <Plug>(coc-diagnostic-prev)
nmap <silent> <leader>]g <Plug>(coc-diagnostic-next)

nmap <silent> <leader>gd <Plug>(coc-definition)
nmap <silent> <leader>gy <Plug>(coc-type-definition)
nmap <silent> <leader>gi <Plug>(coc-implementation)
nmap <silent> <leader>gr <Plug>(coc-references)

nnoremap <silent> <leader>K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

autocmd CursorHold * silent call CocActionAsync('highlight')

nmap <leader>rn <Plug>(coc-rename)

xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

if has('nvim-0.4.0') || has('patch-8.2.0750')
  nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
  inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
  inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
  vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
endif

nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

command! -nargs=0 Format :call CocActionAsync('format')
command! -nargs=? Fold :call     CocAction('fold', <f-args>)
command! -nargs=0 OR   :call     CocActionAsync('runCommand', 'editor.action.organizeImport')

set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}
