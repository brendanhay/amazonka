{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.UpdateGameServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__
--
-- Updates information about a registered game server to help GameLift FleetIQ to track game server availability. This operation is called by a game server process that is running on an instance in a game server group.
-- Use this operation to update the following types of game server information. You can make all three types of updates in the same request:
--
--     * To update the game server's utilization status, identify the game server and game server group and specify the current utilization status. Use this status to identify when game servers are currently hosting games and when they are available to be claimed.
--
--
--     * To report health status, identify the game server and game server group and set health check to @HEALTHY@ . If a game server does not report health status for a certain length of time, the game server is no longer considered healthy. As a result, it will be eventually deregistered from the game server group to avoid affecting utilization metrics. The best practice is to report health every 60 seconds.
--
--
--     * To change game server metadata, provide updated game server data.
--
--
-- Once a game server is successfully updated, the relevant statuses and timestamps are updated.
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
module Network.AWS.GameLift.UpdateGameServer
  ( -- * Creating a request
    UpdateGameServer (..),
    mkUpdateGameServer,

    -- ** Request lenses
    ugsGameServerGroupName,
    ugsGameServerId,
    ugsGameServerData,
    ugsHealthCheck,
    ugsUtilizationStatus,

    -- * Destructuring the response
    UpdateGameServerResponse (..),
    mkUpdateGameServerResponse,

    -- ** Response lenses
    ugsrrsGameServer,
    ugsrrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateGameServer' smart constructor.
data UpdateGameServer = UpdateGameServer'
  { -- | A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
    gameServerGroupName :: Types.GameServerGroupNameOrArn,
    -- | A custom string that uniquely identifies the game server to update.
    gameServerId :: Types.GameServerId,
    -- | A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service when it requests information on game servers using 'ListGameServers' or 'ClaimGameServer' .
    gameServerData :: Core.Maybe Types.GameServerData,
    -- | Indicates health status of the game server. A request that includes this parameter updates the game server's /LastHealthCheckTime/ timestamp.
    healthCheck :: Core.Maybe Types.GameServerHealthCheck,
    -- | Indicates whether the game server is available or is currently hosting gameplay.
    utilizationStatus :: Core.Maybe Types.GameServerUtilizationStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGameServer' value with any optional fields omitted.
mkUpdateGameServer ::
  -- | 'gameServerGroupName'
  Types.GameServerGroupNameOrArn ->
  -- | 'gameServerId'
  Types.GameServerId ->
  UpdateGameServer
mkUpdateGameServer gameServerGroupName gameServerId =
  UpdateGameServer'
    { gameServerGroupName,
      gameServerId,
      gameServerData = Core.Nothing,
      healthCheck = Core.Nothing,
      utilizationStatus = Core.Nothing
    }

-- | A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsGameServerGroupName :: Lens.Lens' UpdateGameServer Types.GameServerGroupNameOrArn
ugsGameServerGroupName = Lens.field @"gameServerGroupName"
{-# DEPRECATED ugsGameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead." #-}

-- | A custom string that uniquely identifies the game server to update.
--
-- /Note:/ Consider using 'gameServerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsGameServerId :: Lens.Lens' UpdateGameServer Types.GameServerId
ugsGameServerId = Lens.field @"gameServerId"
{-# DEPRECATED ugsGameServerId "Use generic-lens or generic-optics with 'gameServerId' instead." #-}

-- | A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service when it requests information on game servers using 'ListGameServers' or 'ClaimGameServer' .
--
-- /Note:/ Consider using 'gameServerData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsGameServerData :: Lens.Lens' UpdateGameServer (Core.Maybe Types.GameServerData)
ugsGameServerData = Lens.field @"gameServerData"
{-# DEPRECATED ugsGameServerData "Use generic-lens or generic-optics with 'gameServerData' instead." #-}

-- | Indicates health status of the game server. A request that includes this parameter updates the game server's /LastHealthCheckTime/ timestamp.
--
-- /Note:/ Consider using 'healthCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsHealthCheck :: Lens.Lens' UpdateGameServer (Core.Maybe Types.GameServerHealthCheck)
ugsHealthCheck = Lens.field @"healthCheck"
{-# DEPRECATED ugsHealthCheck "Use generic-lens or generic-optics with 'healthCheck' instead." #-}

-- | Indicates whether the game server is available or is currently hosting gameplay.
--
-- /Note:/ Consider using 'utilizationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsUtilizationStatus :: Lens.Lens' UpdateGameServer (Core.Maybe Types.GameServerUtilizationStatus)
ugsUtilizationStatus = Lens.field @"utilizationStatus"
{-# DEPRECATED ugsUtilizationStatus "Use generic-lens or generic-optics with 'utilizationStatus' instead." #-}

instance Core.FromJSON UpdateGameServer where
  toJSON UpdateGameServer {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GameServerGroupName" Core..= gameServerGroupName),
            Core.Just ("GameServerId" Core..= gameServerId),
            ("GameServerData" Core..=) Core.<$> gameServerData,
            ("HealthCheck" Core..=) Core.<$> healthCheck,
            ("UtilizationStatus" Core..=) Core.<$> utilizationStatus
          ]
      )

instance Core.AWSRequest UpdateGameServer where
  type Rs UpdateGameServer = UpdateGameServerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.UpdateGameServer")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGameServerResponse'
            Core.<$> (x Core..:? "GameServer") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateGameServerResponse' smart constructor.
data UpdateGameServerResponse = UpdateGameServerResponse'
  { -- | Object that describes the newly updated game server.
    gameServer :: Core.Maybe Types.GameServer,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateGameServerResponse' value with any optional fields omitted.
mkUpdateGameServerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateGameServerResponse
mkUpdateGameServerResponse responseStatus =
  UpdateGameServerResponse'
    { gameServer = Core.Nothing,
      responseStatus
    }

-- | Object that describes the newly updated game server.
--
-- /Note:/ Consider using 'gameServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsrrsGameServer :: Lens.Lens' UpdateGameServerResponse (Core.Maybe Types.GameServer)
ugsrrsGameServer = Lens.field @"gameServer"
{-# DEPRECATED ugsrrsGameServer "Use generic-lens or generic-optics with 'gameServer' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsrrsResponseStatus :: Lens.Lens' UpdateGameServerResponse Core.Int
ugsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ugsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
