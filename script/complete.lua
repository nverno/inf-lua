-- TODO: Add keywords with there is no base / scope

function __repl_complete(line, initial_scope)
  local base, sep, rest = string.match(line, '^(.*)([.:])(.*)')
  if not base then
    rest = line
  end
  local prefix = string.match(rest, '^[%a_][%a%d_]*')
  if prefix and prefix ~= rest then return end
  local scope = initial_scope or _G
  if base then
    local f = load('return ' .. base, nil, nil, scope)
    if not f then return end
    local ok
    ok, scope = pcall(f)
    if not ok then return end
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
    io.write(base .. sep .. items[1], '\n')
  elseif #items > 1 then
    for _, v in pairs(items) do
      io.write(base .. sep .. v, '\n')
    end
  end
end
