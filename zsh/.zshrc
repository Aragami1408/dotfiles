export ZSH="$HOME/.oh-my-zsh" # TODO: Install oh-my-zsh
ZSH_THEME="bira"

HYPHEN_INSENSITIVE="true"

DISABLE_MAGIC_FUNCTIONS="true"

DISABLE_AUTO_TITLE="true"

ENABLE_CORRECTION="true"

HIST_STAMPS="dd/mm/yyyy"

plugins=(git timer vi-mode z zsh-syntax-highlighting bgnotify per-directory-history git history command-not-found zsh-interactive-cd)

source $ZSH/oh-my-zsh.sh

if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='nano'
else
  export EDITOR='nvim' # TODO: Make sure to install nvim
fi

export WINEPREFIX="$HOME/.wine"

export WINEARCH=win64
export WINEDEBUG=-all

export XDG_CONFIG_HOME="$HOME/.config"
export EDITOR="vim"
export VISUAL="vim"
export PAGER="less"

export GTK_IM_MODULE=ibus
export QT_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT4_IM_MODULE=xim
export CLUTTER_IM_MODULE=ibus
export GLFW_IM_MODULE=ibus
export XMODIFERS=@im=ibus

export MPD_HOST="localhost"
export MPD_PORT="6601"

export PREFIX="$HOME/opt/cross"
export TARGET=i686-elf


if [ -d "$HOME/.bin" ] ;
    then PATH="$HOME/.bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ;
    then PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "/Applications/XAMPP/xamppfiles" ] ;
    then PATH="/Applications/XAMPP/xamppfiles:$PATH"
fi

if [ -d "$PREFIX/bin" ] ;
    then PATH="$PREFIX/bin:$PATH"
fi

if [ -d "/usr/local/opt/ruby/bin" ] ;
    then PATH="/usr/local/opt/ruby/bin:$PATH"
fi

if [ -d "~/.bun/bin" ] ;
    then PATH="~/.bun/bin:$PATH"
fi

if [ -d "/usr/local/Cellar/openvpn/2.6.6/sbin" ] ;
    then PATH="/usr/local/Cellar/openvpn/2.6.6/sbin:$PATH"
fi

if [ -d "$(gem path ruby-lsp)" ] ;
    then PATH="$(gem path ruby-lsp)/exe:$PATH"
fi



case ${TERM} in
  xterm*|rxvt*|Eterm*|aterm|kterm|gnome*|alacritty|st|konsole*)
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\007"'
        ;;
  screen*)
    PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\033\\"'
    ;;
esac

function extract {
 if [ -z "$1" ]; then
    # display usage if no parameters given
    echo "Usage: extract <path/file_name>.<zip|rar|bz2|gz|tar|tbz2|tgz|Z|7z|xz|ex|tar.bz2|tar.gz|tar.xz>"
    echo "       extract <path/file_name_1.ext> [path/file_name_2.ext] [path/file_name_3.ext]"
 else
    for n in "$@"
    do
      if [ -f "$n" ] ; then
          case "${n%,}" in
            *.cbt|*.tar.bz2|*.tar.gz|*.tar.xz|*.tbz2|*.tgz|*.txz|*.tar)
                         tar xvf "$n"       ;;
            *.lzma)      unlzma ./"$n"      ;;
            *.bz2)       bunzip2 ./"$n"     ;;
            *.cbr|*.rar)       unrar x -ad ./"$n" ;;
            *.gz)        gunzip ./"$n"      ;;
            *.cbz|*.epub|*.zip)       unzip ./"$n"       ;;
            *.z)         uncompress ./"$n"  ;;
            *.7z|*.arj|*.cab|*.cb7|*.chm|*.deb|*.dmg|*.iso|*.lzh|*.msi|*.pkg|*.rpm|*.udf|*.wim|*.xar)
                         7z x ./"$n"        ;;
            *.xz)        unxz ./"$n"        ;;
            *.exe)       cabextract ./"$n"  ;;
            *.cpio)      cpio -id < ./"$n"  ;;
            *.cba|*.ace)      unace x ./"$n"      ;;
            *)
                         echo "extract: '$n' - unknown archive method"
                         return 1
                         ;;
          esac
      else
          echo "'$n' - file does not exist"
          return 1
      fi
    done
fi
}

# Reload Shell
alias reload="source ~/.zshrc"

# 
alias zshconfig="nvim ~/.zshrc"
alias vimconfig="nvim ~/.config/nvim/init.lua"
alias termconfig="nvim ~/.config/alacritty/alacritty.yml"
alias mpvconfig="nvim ~/.config/mpv/mpv.conf"
alias wmconfig="nvim ~/.config/bspwm/bspwmrc"
alias hkeyconfig="nvim ~/.config/sxhkd/sxhkdrc"
alias compconfig="nvim ~/.config/picom/picom.conf"
alias cocconfig="nvim ~/.config/vim/coc-settings.json"
alias roficonfig="nvim ~/.config/rofi/config.rasi"
alias tmuxconfig="nvim ~/.config/tmux/tmux.conf"
alias ncmpconfig="nvim ~/.config/ncmpcpp/config"
alias barconfig="nvim ~/.config/polybar/config"
# MacOS only
alias yabaiconfig="nvim ~/.config/yabai/yabairc"
alias skhdconfig="nvim ~/.config/skhd/skhdrc"
alias displayconfig="nvim ~/.screenlayout/dual_monitor_1600_900.sh"

alias ls='exa -al --color=always --group-directories-first' # my preferred listing
alias la='exa -a --color=always --group-directories-first'  # all files and dirs
alias ll='exa -l --color=always --group-directories-first'  # long format
alias lt='exa -aT --color=always --group-directories-first' # tree listing
alias l.='exa -a | egrep "^\."'

# only for arch linux
alias mirror="sudo reflector -f 30 -l 30 --number 10 --verbose --save /etc/pacman.d/mirrorlist"
alias mirrord="sudo reflector --latest 50 --number 20 --sort delay --save /etc/pacman.d/mirrorlist"
alias mirrors="sudo reflector --latest 50 --number 20 --sort score --save /etc/pacman.d/mirrorlist"
alias mirrora="sudo reflector --latest 50 --number 20 --sort age --save /etc/pacman.d/mirrorlist"

alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

alias cp="cp -i"
alias mv='mv -i'
alias rm='rm -i'

alias psmem='ps auxf | sort -nr -k 4'
alias psmem10='ps auxf | sort -nr -k 4 | head -10'

alias pscpu='ps auxf | sort -nr -k 3'
alias pscpu10='ps auxf | sort -nr -k 3 | head -10'

alias gpg-check="gpg2 --keyserver-options auto-key-retrieve --verify"
alias gpg-retrieve="gpg2 --keyserver-options auto-key-retrieve --receive-keys"

alias src_vis="gource -s 1 --font-size 11 --key --highlight-users"

alias glog="git log --graph --decorate --oneline --all"
alias gsync="git checkout master && git fetch upstream && git rebase upstream/master && git push"

alias clangd-compile-commands="cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1"

alias yta-aac="yt-dlp --extract-audio --audio-format aac "
alias yta-best="yt-dlp --extract-audio --audio-format best "
alias yta-flac="yt-dlp --extract-audio --audio-format flac "
alias yta-m4a="yt-dlp --extract-audio --audio-format m4a "
alias yta-mp3="yt-dlp --extract-audio --audio-format mp3 "
alias yta-opus="yt-dlp --extract-audio --audio-format opus "
alias yta-vorbis="yt-dlp --extract-audio --audio-format vorbis "
alias yta-wav="yt-dlp --extract-audio --audio-format wav "
alias ytv-best="yt-dlp -f bestvideo+bestaudio "
alias yta-aupl='yt-dlp -f "bestaudio" --continue --no-overwrites --ignore-errors --extract-audio --audio-format opus -o "%(title)s.%(ext)s"'

alias docker="sudo docker"


alias neofetch="neofetch --ascii ~/.nfdp"
alias doom="~/.emacs.d/bin/doom"

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"

# bun completions
[ -s "/Users/higanbana/.bun/_bun" ] && source "/Users/higanbana/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

