-- TODO: Add keywords with there is no base / scope
-- local dbg = require 'debugger'

function __REPL_complete(line, initial_scope)
  local base, sep, rest = string.match(line, '^(.*)([.:])(.*)')
  if not base then
    rest = line
  end
  local prefix = string.match(rest, '^[%a_][%a%d_]*')
  if prefix and prefix ~= rest then return print() end
  local scope = initial_scope or _G
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
  if #items == 1 then
    return print(base .. sep .. items[1])
  elseif #items > 1 then
    for k, v in pairs(items) do
      items[k] = base .. sep .. v
    end
    return print(table.concat(items, '\n'))
  end
end
