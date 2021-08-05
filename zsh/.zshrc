# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/home/$USER/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="bira"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# Caution: this setting can cause issues with multiline prompts (zsh 5.7.1 and newer seem to work)
# See https://github.com/ohmyzsh/ohmyzsh/issues/5765
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
HIST_STAMPS="dd/mm/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git rust timer vi-mode systemd z bgnotify per-directory-history emacs git history command-not-found zsh-interactive-cd)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='nvim'
fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
alias zshconfig="nvim ~/.zshrc"
alias ohmyzsh="nvim ~/.oh-my-zsh"
alias doom="~/.emacs.d/bin/doom"
alias reload="source ~/.zshrc"
alias vimconfig="nvim ~/.config/nvim/init.vim"
alias termconfig="nvim ~/.config/alacritty/alacritty.yml"
alias mpvconfig="nvim ~/.config/mpv/mpv.conf"
alias wmconfig="nvim ~/.config/bspwm/bspwmrc"
alias hkeyconfig="nvim ~/.config/sxhkd/sxhkdrc"
alias compconfig="nvim ~/.config/picom/picom.conf"
alias cocconfig="nvim ~/.config/nvim/coc-settings.json"
alias ls='exa -al --color=always --group-directories-first' # my preferred listing
alias lt='exa -aT --color=always --group-directories-first' # tree listing
alias l.='exa -a | egrep "^\."'
alias src_vis="gource -s 1 --font-size 11 --key --highlight-users"
alias osu="WINEDEBUG=-all wine ~/Games/osu!/osu!.exe &"
alias glog="git log --graph --decorate --oneline --all"
alias gsync="git checkout master && git fetch upstream && git rebase upstream/master && git push"
alias gaddup='git add -u'
alias gaddall='git add .'
alias gbranch='git branch'
alias gcheckout='git checkout'
alias gclone='git clone'
alias gcommit='git commit -m'
alias gfetch='git fetch'
alias gpull='git pull origin'
alias gpush='git push origin'
alias gstat='git status'  # 'status' is protected name so using 'stat' instead
alias gtag='git tag'
alias gnewtag='git tag -a'
alias clangd-compile-commands="cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1"
alias yta-aac="youtube-dl --extract-audio --audio-format aac "
alias yta-best="youtube-dl --extract-audio --audio-format best "
alias yta-flac="youtube-dl --extract-audio --audio-format flac "
alias yta-m4a="youtube-dl --extract-audio --audio-format m4a "
alias yta-mp3="youtube-dl --extract-audio --audio-format mp3 "
alias yta-opus="youtube-dl --extract-audio --audio-format opus "
alias yta-vorbis="youtube-dl --extract-audio --audio-format vorbis "
alias yta-wav="youtube-dl --extract-audio --audio-format wav "
alias ytv-best="youtube-dl -f bestvideo+bestaudio "
alias yta-aupl='youtube-dl -f "bestaudio" --continue --no-overwrites --ignore-errors --extract-audio --audio-format opus -o "%(title)s.%(ext)s"'
alias docker="sudo docker"

export WINEPREFIX="$HOME/prefix32"
export WINEARCH=win32
export XDG_CONFIG_HOME="$HOME/.config"
export EDITOR="emacsclient -t -a ''"
export VISUAL="emacsclient -c -a emacs"
export PAGER="most"

if [ "$TMUX" = "" ]; then tmux; fi
