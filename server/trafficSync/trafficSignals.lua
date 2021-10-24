-- This Source Code Form is subject to the terms of the bCDDL, v. 1.1.
-- If a copy of the bCDDL was not distributed with this
-- file, You can obtain one at http://beamng.com/bCDDL-1.1.txt



--define missing functions
pluginPath = debug.getinfo(1).source:gsub("\\","/")
pluginPath = pluginPath:sub(2,(pluginPath:find("trafficSignals.lua"))-2)

package.path = package.path .. ";;" .. pluginPath .. "/?.lua;;".. pluginPath .. "/lua/?.lua"
package.cpath = package.cpath .. ";;" .. pluginPath .. "/?.dll;;" .. pluginPath .. "/lib/?.dll"
package.cpath = package.cpath .. ";;" .. pluginPath .. "/?.so;;" .. pluginPath .. "/lib/?.so"

jsonLib = require('json')

function log(...)
	print("[Traffic Sync] " .. tostring(...))
end

-- converts string str separated with separator sep to table
function stringToTable(str, sep)
  if sep == nil then
    sep = "%s"
  end

  local t = {}
  local i = 1
  for s in string.gmatch(str, "([^"..sep.."]+)") do
    t[i] = s
    i = i + 1
  end
  return t
end

function jsonReadFile(path)
	local jsonFile, error = io.open(path,"r")
	if error then return nil, error end

	local jsonText = jsonFile:read("*a")
	jsonFile:close()

	return jsonLib.parse(jsonText), false
end




local M = {}

local graphpath = require('graphpath')

local intersections, controllers, signalMetadata = {}, {}, {}
local defaultSignalType = 'lightsBasic'

local timer = 0
local loaded = false
local active = false

local logUpdates = false

local queue = graphpath.newMinheap()

-- Intersection
-- Contains main position, signal control data, traffic light objects, and directional signal nodes with phases
local Intersection = {}
Intersection.__index = Intersection

-- Signal Controller
-- Contains signal type and timing data; used within intersection data
local SignalController = {}
SignalController.__index = SignalController

function Intersection:new(data)
  local o = {}
  data = data or {}
  setmetatable(o, self)

  o.name = data.name
  o.controllerName = data.controllerName
  o.signalNodes = data.signalNodes or {}

  log('\t' .. (data.name or 'Intersection') .. ' created!')

  return o
end

function Intersection:addSignalNode(data)
  data = data or {}
  local new = {
    signalIdx = data.signalIdx or 1
  }
  table.insert(self.signalNodes, new)
end

function Intersection:deleteSignalNode(idx)
  if not self.signalNodes[idx] then return end
  table.remove(self.signalNodes, idx)
end

function Intersection:updateLights(idx, lights)

  local node --= self.signalNodes[idx]
  if node then
    for _, v in ipairs(node._objIds) do -- actual traffic signal objects
      --local obj = scenetree.findObjectById(v)
      for i, v in ipairs(lights) do -- dynamic light instances of the traffic light object
        local field = i > 1 and instanceStr..tostring(i - 1) or instanceStr -- 'instanceColor', 'instanceColor1', etc.
        if v ~= 'black' then
          --obj:setField(field, '0', lightOn)
        else
          --obj:setField(field, '0', lightOff)
        end
      end
    end
  end
end

function Intersection:onSerialize()
  local data = {
    name = self.name,
    controllerName = self.controllerName,
    signalNodes = deepcopy(self.signalNodes)
  }
  return data
end

function Intersection:onDeserialized(data)
  if not data then return end
  self.name = data.name or self.name
  self.controllerName = data.controllerName or self.controllerName
  self.signalNodes = data.signalNodes or self.signalNodes
end

function SignalController:new(data)
  local o = {}
  data = data or {}
  setmetatable(o, self)

  o.name = data.name
  o.signalStartIdx = data.signalStartIdx or 1
  o.lightStartIdx = data.lightStartIdx or 1
  o.startTime = data.startTime or 0
  o.skipStart = type(data.skipStart) == 'boolean' and data.skipStart or false
  o.skipTimer = type(data.skipTimer) == 'boolean' and data.skipTimer or false
  o.customTimings = type(data.customTimings) == 'boolean' and data.customTimings or false
  o.signalIdx = o.signalStartIdx
  o.signals = data.signals or {}

  log('\t' .. (data.name or 'SignalController') .. ' created!')

  return o
end

function SignalController:addSignal(data)
  data = data or {}
  data.signalType = data.signalType or defaultSignalType
  local signalProto = signalMetadata.types[data.signalType] or signalMetadata.types[defaultSignalType]
  local new = {
    prototype = data.signalType,
    lightIdx = 0,
    lightDefaultIdx = data.lightDefaultIdx or signalProto.defaultIdx,
    timings = data.timings or deepcopy(signalProto.timings),
    action = signalProto.action -- actions such as stop, slow, etc.
  }
  table.insert(self.signals, new)
end

function SignalController:deleteSignal(idx)
  if not self.signals[idx] then return end
  table.remove(self.signals, idx)
end

function SignalController:getMetadata(sigIdx) -- returns a table of the linked signal type, action, and light instances
  sigIdx = sigIdx or self.signalIdx
  local sig = self.signals[sigIdx]
  if sig then
    local sigType, action, lights

    if sig.timings then
      sigType = sig.timings[sig.lightIdx] and sig.timings[sig.lightIdx].type or 'none'
      if signalMetadata.states[sigType] then
        action = signalMetadata.states[sigType].action
        lights = signalMetadata.states[sigType].lights
      else
        action = 1
        lights = {}

        local altType = sig.timings[1] and sig.timings[1].type
        if altType and signalMetadata.states[altType] then
          for i = 1, #signalMetadata.states[altType].lights do
            table.insert(lights, 'black') -- set all light components to off
          end
        end
      end
    else
      sigType = sig.type or 'none'
      action = sig.action or 1
      lights = {}
    end

    return {type = sigType, action = action, lights = lights}
  end
end

function SignalController:onSignalUpdate(sigIdx, ignoreQueue) -- updates traffic signal objects and sends new data
  sigIdx = sigIdx or self.signalIdx
  local sig = self.signals[sigIdx]
  if sig then
    local md = self:getMetadata(sigIdx)
    if not md then return end

    for _, inter in pairs(intersections) do
      if inter.controllerName == self.name then
        inter:updateLights(sigIdx, md.lights)

		sendLightDataToClients(self.name, sigIdx, sig.lightIdx)

		local stateName = sig.timings[sig.lightIdx].type
		if logUpdates then log(self.name .. " is updating " .. inter.name ..' signal:'.. tostring(sigIdx) ..' to light index:'.. tostring(sig.lightIdx) ..' '..  stateName) end

        for i, node in ipairs(inter.signalNodes) do
          if node.signalIdx == sigIdx then
            --be:queueAllObjectLua('mapmgr.updateSignal("'..inter.mapNode..'", '..i..', "'..tostring(md.action)..'")') -- maybe avoid using queueAllObjectLua?
          end
        end
      end
    end

    if not ignoreQueue and self._queueId then
      self._queueId = self._queueId + 1
      if sig.timings and sig.timings[sig.lightIdx] then
        queue:insert(timer + sig.timings[sig.lightIdx].duration, self.name..'/'..self._queueId) -- name + unique id
      end
    end

    --extensions.hook('onTrafficSignalUpdate', {name = self.name, signal = sigIdx, light = sig.lightIdx, action = md.action}) -- info from controller
  end
end

function SignalController:setSignal(sIdx, lIdx) -- manual setting of signal and light indexes
  if not sIdx then return end
  self.signalIdx = self.signals[sIdx] and sIdx or 1
  local sig = self.signals[self.signalIdx]
  if sig and sig.timings then
    sig.lightIdx = lIdx
  end

  self:onSignalUpdate(sIdx)
end

function SignalController:advance() -- advances to next signal and/or light
  local sig = self.signals[self.signalIdx]
  if sig and sig.timings then
    if sig.lightIdx > 0 then
      if sig.timings[sig.lightIdx + 1] then -- next light index
        sig.lightIdx = sig.lightIdx + 1
      else -- next signal index, and reset light
        self.signalIdx = self.signals[self.signalIdx + 1] and self.signalIdx + 1 or 1
        sig = self.signals[self.signalIdx]
        sig.lightIdx = 1
      end
    end

    self:onSignalUpdate()
  end
end

function SignalController:activate() -- inserts controller into the main queue
  self._queueId = 0


  if self.signals[1] then
    self.signalIdx = self.signals[self.signalStartIdx] and self.signalStartIdx or 1 -- initial signal
    for i, signal in ipairs(self.signals) do
      if self.signalIdx == i then
        signal.lightIdx = self.lightStartIdx or 1 -- initial light
      else
        signal.lightIdx = signal.lightDefaultIdx or 1 -- sets all signals to default state (red light)
      end

      if signal.timings and not self.customTimings then
        --self:autoSetTimings(i)
		log("ERROR: map doesnt have custom timing data")
      end

      self:onSignalUpdate(i, true)
    end

    local sig = self.signals[self.signalIdx]
    sig.lightIdx = self.lightStartIdx

    if sig.timings and sig.timings[sig.lightIdx] then
      self._queueId = self._queueId + 1
      queue:insert(timer + sig.timings[sig.lightIdx].duration - self.startTime, self.name..'/'..self._queueId)
    end
  end
  log('\t\t' .. (self.name or 'SignalController') .. ' activated')
end

function SignalController:deactivate() -- disables the signal
  for i, sig in ipairs(self.signals) do
    sig.lightIdx = 0
    self:onSignalUpdate(i)
  end
end

function SignalController:onSerialize()
  local data = {
    name = self.name,
    signalStartIdx = self.signalStartIdx,
    lightStartIdx = self.lightStartIdx,
    startTime = self.startTime,
    skipStart = self.skipStart,
    skipTimer = self.skipTimer,
    customTimings = self.customTimings,
    signalIdx = self.signalIdx,
    signals = deepcopy(self.signals)
  }
  for k, v in ipairs(data.signals) do
    if string.find(k, '_') then v[k] = nil end
  end
  return data
end

function SignalController:onDeserialized(data)
  if not data then return end
  self.name = data.name or self.name
  self.signalStartIdx = data.signalStartIdx or self.signalStartIdx
  self.lightStartIdx = data.lightStartIdx or self.lightStartIdx
  self.startTime = data.startTime or self.startTime
  self.skipStart = type(data.skipStart) == 'boolean' and data.skipStart or self.skipStart
  self.skipTimer = type(data.skipTimer) == 'boolean' and data.skipTimer or self.skipTimer
  self.customTimings = type(data.customTimings) == 'boolean' and data.customTimings or self.customTimings
  self.signalIdx = data.signalIdx or self.signalIdx
  self.signals = data.signals or self.signals
end

local function resetTimer() -- resets the timer & queue, and activates the controllers
  timer = 0
  active = true
  queue = graphpath:newMinheap()

  for k, v in pairs(controllers) do
    if v.skipStart then
      v:deactivate()
    else
      v:activate()
    end
  end
end

local function setActive(val) -- sets the timer active state
  active = val and true or false
end

local function getSignalObjects(interName, sigIdx) -- gets all signal objects with valid dynamic fields and values
  local objects = {}
  if not interName then return objects end
  if sigIdx then sigIdx = tostring(sigIdx) end

  if getObjectsByClass('TSStatic') then
    for _, v in ipairs(getObjectsByClass('TSStatic')) do -- search for static objects with signal controller dynamic data
      if v.intersection == interName and (not sigIdx or v.signalNum == sigIdx) then
        table.insert(objects, v:getID())
      end
    end
  end
  return objects
end


local function setupSignalMetadata(data) -- loads default and custom signal metadata (signal definitions)
  local json = jsonReadFile(pluginPath..'/trafficSignalsDefault.json')
  signalMetadata = json--deepcopy(json)

  if data and data.metadata then
    local md = data.metadata -- maybe validate this?
    for k, v in pairs(md.states) do
      signalMetadata.states[k] = v
    end
    for k, v in pairs(md.types) do
      signalMetadata.types[k] = v
    end
  end
end

local function setupSignals(data) -- sets up intersections and controllers, and enables the system
  loaded = false

  if data then
    setupSignalMetadata(data)
    intersections = {}
    controllers = {}

	log("setting up signal contollers...")
    for _, v in ipairs(data.controllers) do
      controllers[v.name] = SignalController:new(v)
    end

	log("setting up intersections...")
    for _, v in ipairs(data.intersections) do
      if controllers[v.controllerName] then -- controller needs to exist
        intersections[v.name] = Intersection:new(v)
        local data = intersections[v.name]
        data.control = controllers[v.controllerName] -- current controller
        --data._visible = true

        for i, node in ipairs(data.signalNodes) do
          --node._objIds = getSignalObjects(v.name, i)
        end
      end
    end

    if next(intersections) and next(controllers) then
      resetTimer()
      loaded = true
	  return true
    else
	  return false
    end
  end
end

local function loadSignals(filePath) -- loads signals json file from given file path or default file path
  if filePath then
    return setupSignals(jsonReadFile(filePath))
  else
    log("NO PATH GIVEN")
  end
end

local function onExtensionLoaded()
  setupSignalMetadata()
end

local function onUpdate(dt, dtSim)
  if not loaded then return end

  --camPos:set(getCameraPosition())

  if active then
    while not queue:empty() and queue:peekKey() <= timer do -- while loop handles any concurrent timings, if any
      local _, v = queue:pop()
      local items = stringToTable(v, '/') -- split into key and id
      local ctrl = controllers[items[1]]

      if ctrl then
        local sig = ctrl.signals[ctrl.signalIdx]
        if sig and sig.timings[sig.lightIdx] then
          if not ctrl.skipTimer and ctrl._queueId == tonumber(items[2]) then
            ctrl:advance()
          end
        end
      end
    end

    timer = timer + dtSim
  end


end

local function onClientEndMission()
  intersections, controllers, signalMetadata = {}, {}, {}
  loaded, active = false, false
end

local function onSerialize()
  local intersectionsData, controllersData = {}, {}

  for k, v in pairs(intersections) do
    intersectionsData[k] = v:onSerialize()
  end
  for k, v in pairs(controllers) do
    controllersData[k] = v:onSerialize()
  end

  return {intersections = intersectionsData, controllers = controllersData, active = active}
end

local function onDeserialized(data)
  for k, v in pairs(data.intersections) do
    local new = Intersection:new()
    intersections[k] = new:onDeserialized(v)
  end
  for k, v in pairs(data.controllers) do
    local new = SignalController:new()
    controllers[k] = new:onDeserialized(v)

    if active then controllers[k]:activate() end
  end

  active = data.active
end

-- public interface
M.newIntersection = newIntersection
M.newSignalController = newSignalController
M.defaultSignalController = defaultSignalController
M.getIntersections = getIntersections
M.getControllers = getControllers
M.getSignalMetadata = getSignalMetadata
M.getSignalObjects = getSignalObjects
M.build = buildNodeDict
M.loadSignals = loadSignals
M.setupSignals = setupSignals
M.getValues = getValues
M.resetTimer = resetTimer
M.setActive = setActive
M.setDebugMode = setDebugMode

M.onExtensionLoaded = onExtensionLoaded
M.onUpdate = onUpdate
M.onClientEndMission = onClientEndMission
M.onSerialize = onSerialize
M.onDeserialized = onDeserialized

--return M


function sendLightDataToClients(controllerName, signalIndex, lightIndex)
	--local d = { controllerName = controllerName, signalIndex = signalIndex, lightIndex = lightIndex }
	local d = controllerName..'|'..signalIndex..'|'..lightIndex
	TriggerClientEvent(-1, "trafficSignalUpdate", d)--jsonLib.stringify(d))
end





function updateTimer()
	onUpdate(0.1,0.1)
end

local function getMapName(s)
  local c = s:match( "^%s*Map%s*=%s*(.-)%s*$" )
  if not c then return nil end

  c =c:sub(2,#c-1)
  c = c:match( "^/levels/(.+)/" )

  return c
end

function onInit()
	local mapName
	--get map name
	for line in io.lines("ServerConfig.toml") do
		if getMapName(line) then
			mapName = getMapName(line)
			break
		end
	end

	if mapName then
		if loadSignals(pluginPath .. '/'..mapName..'.json') then
			log('PLUGIN LOADED')
			RegisterEvent("updateTimer", "updateTimer")
			CreateThread("updateTimer", 10)
		else
			log('failed to load plugin')
		end
	else
		log("oopsie map cant be parsed")
	end
end
