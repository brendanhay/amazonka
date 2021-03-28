{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.ClaimGameServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__ 
--
-- Locates an available game server and temporarily reserves it to host gameplay and players. This operation is called from a game client or client service (such as a matchmaker) to request hosting resources for a new game session. In response, GameLift FleetIQ locates an available game server, places it in @CLAIMED@ status for 60 seconds, and returns connection information that players can use to connect to the game server. 
-- To claim a game server, identify a game server group. You can also specify a game server ID, although this approach bypasses GameLift FleetIQ placement optimization. Optionally, include game data to pass to the game server at the start of a game session, such as a game map or player information. 
-- When a game server is successfully claimed, connection information is returned. A claimed game server's utilization status remains @AVAILABLE@ while the claim status is set to @CLAIMED@ for up to 60 seconds. This time period gives the game server time to update its status to @UTILIZED@ (using 'UpdateGameServer' ) once players join. If the game server's status is not updated within 60 seconds, the game server reverts to unclaimed status and is available to be claimed by another request. The claim time period is a fixed value and is not configurable.
-- If you try to claim a specific game server, this request will fail in the following cases:
--
--     * If the game server utilization status is @UTILIZED@ .
--
--
--     * If the game server claim status is @CLAIMED@ .
--
--
-- __Learn more__ 
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/gsg-intro.html GameLift FleetIQ Guide> 
-- __Related operations__ 
--
--     * 'RegisterGameServer' 
--
--
--     * 'ListGameServers' 
--
--
--     * 'ClaimGameServer' 
--
--
--     * 'DescribeGameServer' 
--
--
--     * 'UpdateGameServer' 
--
--
--     * 'DeregisterGameServer' 
--
--
module Network.AWS.GameLift.ClaimGameServer
    (
    -- * Creating a request
      ClaimGameServer (..)
    , mkClaimGameServer
    -- ** Request lenses
    , cgsGameServerGroupName
    , cgsGameServerData
    , cgsGameServerId

    -- * Destructuring the response
    , ClaimGameServerResponse (..)
    , mkClaimGameServerResponse
    -- ** Response lenses
    , crsGameServer
    , crsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkClaimGameServer' smart constructor.
data ClaimGameServer = ClaimGameServer'
  { gameServerGroupName :: Types.GameServerGroupName
    -- ^ A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.. If you are not specifying a game server to claim, this value identifies where you want GameLift FleetIQ to look for an available game server to claim. 
  , gameServerData :: Core.Maybe Types.GameServerData
    -- ^ A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service when it requests information on game servers using 'ListGameServers' or 'ClaimGameServer' . 
  , gameServerId :: Core.Maybe Types.GameServerId
    -- ^ A custom string that uniquely identifies the game server to claim. If this parameter is left empty, GameLift FleetIQ searches for an available game server in the specified game server group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClaimGameServer' value with any optional fields omitted.
mkClaimGameServer
    :: Types.GameServerGroupName -- ^ 'gameServerGroupName'
    -> ClaimGameServer
mkClaimGameServer gameServerGroupName
  = ClaimGameServer'{gameServerGroupName,
                     gameServerData = Core.Nothing, gameServerId = Core.Nothing}

-- | A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.. If you are not specifying a game server to claim, this value identifies where you want GameLift FleetIQ to look for an available game server to claim. 
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsGameServerGroupName :: Lens.Lens' ClaimGameServer Types.GameServerGroupName
cgsGameServerGroupName = Lens.field @"gameServerGroupName"
{-# INLINEABLE cgsGameServerGroupName #-}
{-# DEPRECATED gameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead"  #-}

-- | A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service when it requests information on game servers using 'ListGameServers' or 'ClaimGameServer' . 
--
-- /Note:/ Consider using 'gameServerData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsGameServerData :: Lens.Lens' ClaimGameServer (Core.Maybe Types.GameServerData)
cgsGameServerData = Lens.field @"gameServerData"
{-# INLINEABLE cgsGameServerData #-}
{-# DEPRECATED gameServerData "Use generic-lens or generic-optics with 'gameServerData' instead"  #-}

-- | A custom string that uniquely identifies the game server to claim. If this parameter is left empty, GameLift FleetIQ searches for an available game server in the specified game server group.
--
-- /Note:/ Consider using 'gameServerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsGameServerId :: Lens.Lens' ClaimGameServer (Core.Maybe Types.GameServerId)
cgsGameServerId = Lens.field @"gameServerId"
{-# INLINEABLE cgsGameServerId #-}
{-# DEPRECATED gameServerId "Use generic-lens or generic-optics with 'gameServerId' instead"  #-}

instance Core.ToQuery ClaimGameServer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ClaimGameServer where
        toHeaders ClaimGameServer{..}
          = Core.pure ("X-Amz-Target", "GameLift.ClaimGameServer") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ClaimGameServer where
        toJSON ClaimGameServer{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GameServerGroupName" Core..= gameServerGroupName),
                  ("GameServerData" Core..=) Core.<$> gameServerData,
                  ("GameServerId" Core..=) Core.<$> gameServerId])

instance Core.AWSRequest ClaimGameServer where
        type Rs ClaimGameServer = ClaimGameServerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ClaimGameServerResponse' Core.<$>
                   (x Core..:? "GameServer") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkClaimGameServerResponse' smart constructor.
data ClaimGameServerResponse = ClaimGameServerResponse'
  { gameServer :: Core.Maybe Types.GameServer
    -- ^ Object that describes the newly claimed game server.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ClaimGameServerResponse' value with any optional fields omitted.
mkClaimGameServerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ClaimGameServerResponse
mkClaimGameServerResponse responseStatus
  = ClaimGameServerResponse'{gameServer = Core.Nothing,
                             responseStatus}

-- | Object that describes the newly claimed game server.
--
-- /Note:/ Consider using 'gameServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsGameServer :: Lens.Lens' ClaimGameServerResponse (Core.Maybe Types.GameServer)
crsGameServer = Lens.field @"gameServer"
{-# INLINEABLE crsGameServer #-}
{-# DEPRECATED gameServer "Use generic-lens or generic-optics with 'gameServer' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' ClaimGameServerResponse Core.Int
crsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
