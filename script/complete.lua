-- TODO: Add keywords with there is no base / scope

local keywords = {
  "and", "do", "else", "elseif", "end", "for", "function", "goto", "if",
  "in", "local", "not", "or", "repeat", "return", "then", "until", "while"
}

function __REPL_complete(line, initial_scope)
  local base, sep, rest = string.match(line, '^(.*)([.:])(.*)')
  if not base then rest = line end
  local prefix = string.match(rest, '^[%a_][%a%d_]*')
  if prefix and prefix ~= rest then return print() end
  local function get_scope(scope)
    if not scope or scope == '' then return _ENV or _G end
    local f = load('return ' .. scope)
    if f then
      local ok
      ok, scope = pcall(f)
      if ok then
        local typ = type(scope)
        if typ == 'string' then return string end
        if typ == 'thread' then return coroutine end
        return scope
      end
    end
    return {}
  end

  local scope = get_scope(initial_scope) --- @type table
  if not scope then return print() end
  local meth = sep == ':'
  if base then
    if meth and type(scope[base]) == 'string' then
      scope = string
    elseif meth and type(scope[base]) == 'thread' then
      scope = coroutine
    else
      local f = load('return ' .. base, nil, nil, scope)
      if not f then return print() end
      local ok
      ok, scope = pcall(f)
      if not ok then return print() end
    end
  else
    base = ''
    sep = ''
    scope = scope
  end
  local matches = {}
  local prop = sep ~= ':'
  while type(scope) == 'table' do
    for key, value in pairs(scope) do
      if (prop or (type(value) == 'function')) and
          ((not prefix) or (string.match(key, '^' .. prefix))) then
        matches[key] = true
      end
    end
    scope = getmetatable(scope)
    scope = scope and scope.__index
  end
  local items = {}
  for key in pairs(matches) do
    items[#items + 1] = key
  end
  table.sort(items)
  return print(table.concat(items, '\n'))
end
