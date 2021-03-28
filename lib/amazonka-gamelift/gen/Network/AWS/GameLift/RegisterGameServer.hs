{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.RegisterGameServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__ 
--
-- Creates a new game server resource and notifies GameLift FleetIQ that the game server is ready to host gameplay and players. This operation is called by a game server process that is running on an instance in a game server group. Registering game servers enables GameLift FleetIQ to track available game servers and enables game clients and services to claim a game server for a new game session. 
-- To register a game server, identify the game server group and instance where the game server is running, and provide a unique identifier for the game server. You can also include connection and game server data. When a game client or service requests a game server by calling 'ClaimGameServer' , this information is returned in the response.
-- Once a game server is successfully registered, it is put in status @AVAILABLE@ . A request to register a game server may fail if the instance it is running on is in the process of shutting down as part of instance balancing or scale-down activity. 
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
module Network.AWS.GameLift.RegisterGameServer
    (
    -- * Creating a request
      RegisterGameServer (..)
    , mkRegisterGameServer
    -- ** Request lenses
    , rgsGameServerGroupName
    , rgsGameServerId
    , rgsInstanceId
    , rgsConnectionInfo
    , rgsGameServerData

    -- * Destructuring the response
    , RegisterGameServerResponse (..)
    , mkRegisterGameServerResponse
    -- ** Response lenses
    , rgsrrsGameServer
    , rgsrrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterGameServer' smart constructor.
data RegisterGameServer = RegisterGameServer'
  { gameServerGroupName :: Types.GameServerGroupName
    -- ^ A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
  , gameServerId :: Types.GameServerId
    -- ^ A custom string that uniquely identifies the game server to register. Game server IDs are developer-defined and must be unique across all game server groups in your AWS account.
  , instanceId :: Types.GameServerInstanceId
    -- ^ The unique identifier for the instance where the game server is running. This ID is available in the instance metadata. EC2 instance IDs use a 17-character format, for example: @i-1234567890abcdef0@ .
  , connectionInfo :: Core.Maybe Types.ConnectionInfo
    -- ^ Information that is needed to make inbound client connections to the game server. This might include the IP address and port, DNS name, and other information.
  , gameServerData :: Core.Maybe Types.GameServerData
    -- ^ A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service when it requests information on game servers using 'ListGameServers' or 'ClaimGameServer' . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterGameServer' value with any optional fields omitted.
mkRegisterGameServer
    :: Types.GameServerGroupName -- ^ 'gameServerGroupName'
    -> Types.GameServerId -- ^ 'gameServerId'
    -> Types.GameServerInstanceId -- ^ 'instanceId'
    -> RegisterGameServer
mkRegisterGameServer gameServerGroupName gameServerId instanceId
  = RegisterGameServer'{gameServerGroupName, gameServerId,
                        instanceId, connectionInfo = Core.Nothing,
                        gameServerData = Core.Nothing}

-- | A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsGameServerGroupName :: Lens.Lens' RegisterGameServer Types.GameServerGroupName
rgsGameServerGroupName = Lens.field @"gameServerGroupName"
{-# INLINEABLE rgsGameServerGroupName #-}
{-# DEPRECATED gameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead"  #-}

-- | A custom string that uniquely identifies the game server to register. Game server IDs are developer-defined and must be unique across all game server groups in your AWS account.
--
-- /Note:/ Consider using 'gameServerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsGameServerId :: Lens.Lens' RegisterGameServer Types.GameServerId
rgsGameServerId = Lens.field @"gameServerId"
{-# INLINEABLE rgsGameServerId #-}
{-# DEPRECATED gameServerId "Use generic-lens or generic-optics with 'gameServerId' instead"  #-}

-- | The unique identifier for the instance where the game server is running. This ID is available in the instance metadata. EC2 instance IDs use a 17-character format, for example: @i-1234567890abcdef0@ .
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsInstanceId :: Lens.Lens' RegisterGameServer Types.GameServerInstanceId
rgsInstanceId = Lens.field @"instanceId"
{-# INLINEABLE rgsInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | Information that is needed to make inbound client connections to the game server. This might include the IP address and port, DNS name, and other information.
--
-- /Note:/ Consider using 'connectionInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsConnectionInfo :: Lens.Lens' RegisterGameServer (Core.Maybe Types.ConnectionInfo)
rgsConnectionInfo = Lens.field @"connectionInfo"
{-# INLINEABLE rgsConnectionInfo #-}
{-# DEPRECATED connectionInfo "Use generic-lens or generic-optics with 'connectionInfo' instead"  #-}

-- | A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service when it requests information on game servers using 'ListGameServers' or 'ClaimGameServer' . 
--
-- /Note:/ Consider using 'gameServerData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsGameServerData :: Lens.Lens' RegisterGameServer (Core.Maybe Types.GameServerData)
rgsGameServerData = Lens.field @"gameServerData"
{-# INLINEABLE rgsGameServerData #-}
{-# DEPRECATED gameServerData "Use generic-lens or generic-optics with 'gameServerData' instead"  #-}

instance Core.ToQuery RegisterGameServer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RegisterGameServer where
        toHeaders RegisterGameServer{..}
          = Core.pure ("X-Amz-Target", "GameLift.RegisterGameServer") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RegisterGameServer where
        toJSON RegisterGameServer{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GameServerGroupName" Core..= gameServerGroupName),
                  Core.Just ("GameServerId" Core..= gameServerId),
                  Core.Just ("InstanceId" Core..= instanceId),
                  ("ConnectionInfo" Core..=) Core.<$> connectionInfo,
                  ("GameServerData" Core..=) Core.<$> gameServerData])

instance Core.AWSRequest RegisterGameServer where
        type Rs RegisterGameServer = RegisterGameServerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RegisterGameServerResponse' Core.<$>
                   (x Core..:? "GameServer") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRegisterGameServerResponse' smart constructor.
data RegisterGameServerResponse = RegisterGameServerResponse'
  { gameServer :: Core.Maybe Types.GameServer
    -- ^ Object that describes the newly registered game server.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RegisterGameServerResponse' value with any optional fields omitted.
mkRegisterGameServerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RegisterGameServerResponse
mkRegisterGameServerResponse responseStatus
  = RegisterGameServerResponse'{gameServer = Core.Nothing,
                                responseStatus}

-- | Object that describes the newly registered game server.
--
-- /Note:/ Consider using 'gameServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsrrsGameServer :: Lens.Lens' RegisterGameServerResponse (Core.Maybe Types.GameServer)
rgsrrsGameServer = Lens.field @"gameServer"
{-# INLINEABLE rgsrrsGameServer #-}
{-# DEPRECATED gameServer "Use generic-lens or generic-optics with 'gameServer' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsrrsResponseStatus :: Lens.Lens' RegisterGameServerResponse Core.Int
rgsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rgsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
