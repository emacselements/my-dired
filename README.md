# my-dired

Enhanced Emacs Dired configuration with improved navigation, file operations, and visual feedback.

## Key Features

- **Smart navigation**: `Home`/`End` skip headers, `q` goes up directory
- **Batch file rename**: Convert marked files to lowercase with dashes (`N`)
- **File concatenation**: Merge text files into `result.txt`
- **Clean interface**: Hide dotfiles and details by default
- **Directory cleanup**: Remove empty directories recursively
- **Symlink resolution**: Navigate directories with resolved paths

## Installation

Add this to your Emacs configuration:

```elisp
(add-to-list 'load-path "/path/to/my-dired")
(require 'my-dired)
```

Or with `use-package`:

```elisp
(use-package my-dired
  :load-path "/path/to/my-dired"
  :after dired)
```

## Key Bindings

| Key | Function |
|-----|----------|
| `Home` | Jump to first file |
| `End` | Jump to last file |
| `Enter` | Open with symlink resolution |
| `q` | Go up directory |
| `N` | Rename marked files |

## Requirements

- Emacs 24.1+
- Optional: `dired-narrow` package for filtering (`C-x /`)

## License

GPL-3.0 - see LICENSE file for details.

## Author

Raoul Comninos
