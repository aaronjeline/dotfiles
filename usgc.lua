-- Neovim colorscheme converted from iTerm2 theme
-- Usage: Place in ~/.config/nvim/colors/ and run :colorscheme iterm_theme

local M = {}

local colors = {
  bg = "#000000",
  fg = "#459a65",
  cursor = "#869c96",
  selection_bg = "#017b01",
  selection_fg = "#f6c443",
  comment = "#484847",
  link = "#47a0b2",
  match = "#fefc52",

  -- ANSI colors
  black = "#262626",
  bright_black = "#484847",
  red = "#cd0400",
  bright_red = "#cd0400",
  green = "#f6c443",
  bright_green = "#ebbf40",
  yellow = "#f6c443",
  bright_yellow = "#f6c543",
  blue = "#6b40ef",
  bright_blue = "#ee7b3b",
  magenta = "#ea3d8d",
  bright_magenta = "#ea3d8d",
  cyan = "#3377f7",
  bright_cyan = "#3377f7",
  white = "#ffffff",
  bright_white = "#fefefe",
}

local function highlight(group, opts)
  vim.api.nvim_set_hl(0, group, opts)
end

function M.setup()
  vim.cmd("hi clear")
  if vim.fn.exists("syntax_on") then
    vim.cmd("syntax reset")
  end
  vim.o.termguicolors = true
  vim.g.colors_name = "iterm_theme"

  -- Editor highlights
  highlight("Normal", { fg = colors.fg, bg = colors.bg })
  highlight("NormalFloat", { fg = colors.fg, bg = colors.black })
  highlight("FloatBorder", { fg = colors.comment, bg = colors.black })
  highlight("Cursor", { fg = colors.bg, bg = colors.cursor })
  highlight("CursorLine", { bg = "#0a0a0a" })
  highlight("CursorColumn", { bg = "#0a0a0a" })
  highlight("ColorColumn", { bg = "#0a0a0a" })
  highlight("LineNr", { fg = colors.comment })
  highlight("CursorLineNr", { fg = colors.yellow, bold = true })
  highlight("SignColumn", { fg = colors.comment, bg = colors.bg })
  highlight("VertSplit", { fg = colors.comment, bg = colors.bg })
  highlight("WinSeparator", { fg = colors.comment, bg = colors.bg })
  highlight("StatusLine", { fg = colors.fg, bg = colors.black })
  highlight("StatusLineNC", { fg = colors.comment, bg = colors.black })
  highlight("TabLine", { fg = colors.comment, bg = colors.black })
  highlight("TabLineFill", { bg = colors.black })
  highlight("TabLineSel", { fg = colors.fg, bg = colors.bg })
  highlight("Pmenu", { fg = colors.fg, bg = colors.black })
  highlight("PmenuSel", { fg = colors.selection_fg, bg = colors.selection_bg })
  highlight("PmenuSbar", { bg = colors.black })
  highlight("PmenuThumb", { bg = colors.comment })
  highlight("Visual", { fg = colors.selection_fg, bg = colors.selection_bg })
  highlight("VisualNOS", { fg = colors.selection_fg, bg = colors.selection_bg })
  highlight("Search", { fg = colors.bg, bg = colors.match })
  highlight("IncSearch", { fg = colors.bg, bg = colors.bright_blue })
  highlight("CurSearch", { fg = colors.bg, bg = colors.bright_blue })
  highlight("Substitute", { fg = colors.bg, bg = colors.red })
  highlight("MatchParen", { fg = colors.match, bold = true })
  highlight("Folded", { fg = colors.comment, bg = colors.black })
  highlight("FoldColumn", { fg = colors.comment, bg = colors.bg })
  highlight("NonText", { fg = colors.comment })
  highlight("SpecialKey", { fg = colors.comment })
  highlight("Whitespace", { fg = colors.comment })
  highlight("EndOfBuffer", { fg = colors.comment })
  highlight("Directory", { fg = colors.cyan })
  highlight("Title", { fg = colors.magenta, bold = true })
  highlight("ErrorMsg", { fg = colors.red, bold = true })
  highlight("WarningMsg", { fg = colors.yellow, bold = true })
  highlight("MoreMsg", { fg = colors.green })
  highlight("ModeMsg", { fg = colors.fg, bold = true })
  highlight("Question", { fg = colors.cyan })
  highlight("Conceal", { fg = colors.comment })
  highlight("WildMenu", { fg = colors.bg, bg = colors.yellow })

  -- Diff highlights
  highlight("DiffAdd", { fg = colors.green, bg = "#1a2e1a" })
  highlight("DiffChange", { fg = colors.yellow, bg = "#2e2e1a" })
  highlight("DiffDelete", { fg = colors.red, bg = "#2e1a1a" })
  highlight("DiffText", { fg = colors.bright_blue, bg = "#2e2a1a", bold = true })

  -- Spell highlights
  highlight("SpellBad", { undercurl = true, sp = colors.red })
  highlight("SpellCap", { undercurl = true, sp = colors.yellow })
  highlight("SpellLocal", { undercurl = true, sp = colors.cyan })
  highlight("SpellRare", { undercurl = true, sp = colors.magenta })

  -- Syntax highlights
  highlight("Comment", { fg = colors.comment, italic = true })
  highlight("Constant", { fg = colors.bright_blue })
  highlight("String", { fg = colors.green })
  highlight("Character", { fg = colors.green })
  highlight("Number", { fg = colors.bright_blue })
  highlight("Boolean", { fg = colors.bright_blue })
  highlight("Float", { fg = colors.bright_blue })
  highlight("Identifier", { fg = colors.fg })
  highlight("Function", { fg = colors.cyan })
  highlight("Statement", { fg = colors.magenta })
  highlight("Conditional", { fg = colors.magenta })
  highlight("Repeat", { fg = colors.magenta })
  highlight("Label", { fg = colors.magenta })
  highlight("Operator", { fg = colors.fg })
  highlight("Keyword", { fg = colors.magenta })
  highlight("Exception", { fg = colors.magenta })
  highlight("PreProc", { fg = colors.yellow })
  highlight("Include", { fg = colors.magenta })
  highlight("Define", { fg = colors.magenta })
  highlight("Macro", { fg = colors.yellow })
  highlight("PreCondit", { fg = colors.yellow })
  highlight("Type", { fg = colors.yellow })
  highlight("StorageClass", { fg = colors.yellow })
  highlight("Structure", { fg = colors.yellow })
  highlight("Typedef", { fg = colors.yellow })
  highlight("Special", { fg = colors.bright_blue })
  highlight("SpecialChar", { fg = colors.bright_blue })
  highlight("Tag", { fg = colors.magenta })
  highlight("Delimiter", { fg = colors.fg })
  highlight("SpecialComment", { fg = colors.comment, bold = true })
  highlight("Debug", { fg = colors.red })
  highlight("Underlined", { fg = colors.link, underline = true })
  highlight("Ignore", { fg = colors.comment })
  highlight("Error", { fg = colors.red, bold = true })
  highlight("Todo", { fg = colors.bg, bg = colors.yellow, bold = true })

  -- Treesitter highlights
  highlight("@comment", { link = "Comment" })
  highlight("@error", { link = "Error" })
  highlight("@punctuation.delimiter", { fg = colors.fg })
  highlight("@punctuation.bracket", { fg = colors.fg })
  highlight("@punctuation.special", { fg = colors.cyan })
  highlight("@constant", { link = "Constant" })
  highlight("@constant.builtin", { fg = colors.bright_blue })
  highlight("@constant.macro", { link = "Macro" })
  highlight("@string", { link = "String" })
  highlight("@string.regex", { fg = colors.cyan })
  highlight("@string.escape", { fg = colors.bright_blue })
  highlight("@character", { link = "Character" })
  highlight("@number", { link = "Number" })
  highlight("@boolean", { link = "Boolean" })
  highlight("@float", { link = "Float" })
  highlight("@function", { link = "Function" })
  highlight("@function.builtin", { fg = colors.cyan })
  highlight("@function.macro", { fg = colors.yellow })
  highlight("@parameter", { fg = colors.fg, italic = true })
  highlight("@method", { link = "Function" })
  highlight("@field", { fg = colors.fg })
  highlight("@property", { fg = colors.fg })
  highlight("@constructor", { fg = colors.yellow })
  highlight("@conditional", { link = "Conditional" })
  highlight("@repeat", { link = "Repeat" })
  highlight("@label", { link = "Label" })
  highlight("@operator", { link = "Operator" })
  highlight("@keyword", { link = "Keyword" })
  highlight("@keyword.function", { fg = colors.magenta })
  highlight("@keyword.operator", { fg = colors.magenta })
  highlight("@keyword.return", { fg = colors.magenta })
  highlight("@exception", { link = "Exception" })
  highlight("@variable", { fg = colors.fg })
  highlight("@variable.builtin", { fg = colors.bright_blue })
  highlight("@type", { link = "Type" })
  highlight("@type.builtin", { fg = colors.yellow })
  highlight("@type.definition", { link = "Typedef" })
  highlight("@storageclass", { link = "StorageClass" })
  highlight("@structure", { link = "Structure" })
  highlight("@namespace", { fg = colors.fg })
  highlight("@include", { link = "Include" })
  highlight("@preproc", { link = "PreProc" })
  highlight("@debug", { link = "Debug" })
  highlight("@tag", { link = "Tag" })
  highlight("@tag.attribute", { fg = colors.fg })
  highlight("@tag.delimiter", { fg = colors.fg })
  highlight("@text", { fg = colors.fg })
  highlight("@text.strong", { bold = true })
  highlight("@text.emphasis", { italic = true })
  highlight("@text.underline", { underline = true })
  highlight("@text.strike", { strikethrough = true })
  highlight("@text.title", { link = "Title" })
  highlight("@text.literal", { fg = colors.green })
  highlight("@text.uri", { link = "Underlined" })
  highlight("@text.todo", { link = "Todo" })
  highlight("@text.note", { fg = colors.bg, bg = colors.cyan })
  highlight("@text.warning", { fg = colors.bg, bg = colors.yellow })
  highlight("@text.danger", { fg = colors.bg, bg = colors.red })

  -- LSP highlights
  highlight("DiagnosticError", { fg = colors.red })
  highlight("DiagnosticWarn", { fg = colors.yellow })
  highlight("DiagnosticInfo", { fg = colors.cyan })
  highlight("DiagnosticHint", { fg = colors.fg })
  highlight("DiagnosticUnderlineError", { undercurl = true, sp = colors.red })
  highlight("DiagnosticUnderlineWarn", { undercurl = true, sp = colors.yellow })
  highlight("DiagnosticUnderlineInfo", { undercurl = true, sp = colors.cyan })
  highlight("DiagnosticUnderlineHint", { undercurl = true, sp = colors.fg })
  highlight("DiagnosticVirtualTextError", { fg = colors.red, bg = "#1a0a0a" })
  highlight("DiagnosticVirtualTextWarn", { fg = colors.yellow, bg = "#1a1a0a" })
  highlight("DiagnosticVirtualTextInfo", { fg = colors.cyan, bg = "#0a1a1a" })
  highlight("DiagnosticVirtualTextHint", { fg = colors.fg, bg = "#0a0a0a" })
  highlight("LspReferenceText", { bg = "#1a1a1a" })
  highlight("LspReferenceRead", { bg = "#1a1a1a" })
  highlight("LspReferenceWrite", { bg = "#1a1a1a" })
  highlight("LspSignatureActiveParameter", { fg = colors.yellow, bold = true })

  -- Git signs
  highlight("GitSignsAdd", { fg = colors.green })
  highlight("GitSignsChange", { fg = colors.yellow })
  highlight("GitSignsDelete", { fg = colors.red })

  -- Telescope
  highlight("TelescopeNormal", { fg = colors.fg, bg = colors.bg })
  highlight("TelescopeBorder", { fg = colors.comment })
  highlight("TelescopePromptNormal", { fg = colors.fg, bg = colors.black })
  highlight("TelescopePromptBorder", { fg = colors.comment, bg = colors.black })
  highlight("TelescopePromptTitle", { fg = colors.bg, bg = colors.magenta })
  highlight("TelescopePreviewTitle", { fg = colors.bg, bg = colors.green })
  highlight("TelescopeResultsTitle", { fg = colors.bg, bg = colors.cyan })
  highlight("TelescopeSelection", { fg = colors.selection_fg, bg = colors.selection_bg })
  highlight("TelescopeMatching", { fg = colors.match, bold = true })

  -- NvimTree
  highlight("NvimTreeNormal", { fg = colors.fg, bg = colors.bg })
  highlight("NvimTreeFolderIcon", { fg = colors.yellow })
  highlight("NvimTreeFolderName", { fg = colors.fg })
  highlight("NvimTreeOpenedFolderName", { fg = colors.yellow })
  highlight("NvimTreeRootFolder", { fg = colors.magenta })
  highlight("NvimTreeSpecialFile", { fg = colors.cyan })
  highlight("NvimTreeGitDirty", { fg = colors.yellow })
  highlight("NvimTreeGitNew", { fg = colors.green })
  highlight("NvimTreeGitDeleted", { fg = colors.red })

  -- Indent Blankline
  highlight("IndentBlanklineChar", { fg = "#1a1a1a" })
  highlight("IndentBlanklineContextChar", { fg = colors.comment })

  -- Which-key
  highlight("WhichKey", { fg = colors.magenta })
  highlight("WhichKeyGroup", { fg = colors.cyan })
  highlight("WhichKeyDesc", { fg = colors.fg })
  highlight("WhichKeySeparator", { fg = colors.comment })
  highlight("WhichKeyValue", { fg = colors.comment })
end

M.setup()

return M