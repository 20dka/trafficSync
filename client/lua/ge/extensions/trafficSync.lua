--====================================================================================
-- All work by 20dka.
--====================================================================================
-- Traffic signal sync for monitoring BeamMP
--====================================================================================

local M = {}
print("Loading trafficSync...")

local function receiveTrafficSync(d)
	d = stringToTable(d, '|')

	if not core_trafficSignals then return end

	local controllers = core_trafficSignals.getControllers()
	if not controllers then return end

	local c = controllers[d[1]]
	if not c then return end

	core_trafficSignals.setActive(false)
	c:setSignal(tonumber(d[2]),tonumber(d[3]))
end

local function onExtensionLoaded()
	if MPGameNetwork then AddEventHandler("trafficSignalUpdate", receiveTrafficSync) end
	if not core_trafficSignals then return end
	--core_trafficSignals.setDebugMode('extra')
	core_trafficSignals.setActive(false)
end

local function onPreRender()
	if not core_trafficSignals then return end
	if core_trafficSignals.getValues().state == true then
		onExtensionLoaded()
	end
end

M.onPreRender = onPreRender
M.onExtensionLoaded = onExtensionLoaded

print("trafficSync loaded")
return M