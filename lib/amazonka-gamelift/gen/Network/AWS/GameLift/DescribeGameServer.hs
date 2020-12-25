{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeGameServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__
--
-- Retrieves information for a registered game server. Information includes game server status, health check info, and the instance that the game server is running on.
-- To retrieve game server information, specify the game server ID. If successful, the requested game server object is returned.
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
module Network.AWS.GameLift.DescribeGameServer
  ( -- * Creating a request
    DescribeGameServer (..),
    mkDescribeGameServer,

    -- ** Request lenses
    dGameServerGroupName,
    dGameServerId,

    -- * Destructuring the response
    DescribeGameServerResponse (..),
    mkDescribeGameServerResponse,

    -- ** Response lenses
    drsGameServer,
    drsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeGameServer' smart constructor.
data DescribeGameServer = DescribeGameServer'
  { -- | A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
    gameServerGroupName :: Types.GameServerGroupNameOrArn,
    -- | A custom string that uniquely identifies the game server information to be retrieved.
    gameServerId :: Types.GameServerId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeGameServer' value with any optional fields omitted.
mkDescribeGameServer ::
  -- | 'gameServerGroupName'
  Types.GameServerGroupNameOrArn ->
  -- | 'gameServerId'
  Types.GameServerId ->
  DescribeGameServer
mkDescribeGameServer gameServerGroupName gameServerId =
  DescribeGameServer' {gameServerGroupName, gameServerId}

-- | A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dGameServerGroupName :: Lens.Lens' DescribeGameServer Types.GameServerGroupNameOrArn
dGameServerGroupName = Lens.field @"gameServerGroupName"
{-# DEPRECATED dGameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead." #-}

-- | A custom string that uniquely identifies the game server information to be retrieved.
--
-- /Note:/ Consider using 'gameServerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dGameServerId :: Lens.Lens' DescribeGameServer Types.GameServerId
dGameServerId = Lens.field @"gameServerId"
{-# DEPRECATED dGameServerId "Use generic-lens or generic-optics with 'gameServerId' instead." #-}

instance Core.FromJSON DescribeGameServer where
  toJSON DescribeGameServer {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GameServerGroupName" Core..= gameServerGroupName),
            Core.Just ("GameServerId" Core..= gameServerId)
          ]
      )

instance Core.AWSRequest DescribeGameServer where
  type Rs DescribeGameServer = DescribeGameServerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.DescribeGameServer")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGameServerResponse'
            Core.<$> (x Core..:? "GameServer") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeGameServerResponse' smart constructor.
data DescribeGameServerResponse = DescribeGameServerResponse'
  { -- | Object that describes the requested game server.
    gameServer :: Core.Maybe Types.GameServer,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeGameServerResponse' value with any optional fields omitted.
mkDescribeGameServerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeGameServerResponse
mkDescribeGameServerResponse responseStatus =
  DescribeGameServerResponse'
    { gameServer = Core.Nothing,
      responseStatus
    }

-- | Object that describes the requested game server.
--
-- /Note:/ Consider using 'gameServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsGameServer :: Lens.Lens' DescribeGameServerResponse (Core.Maybe Types.GameServer)
drsGameServer = Lens.field @"gameServer"
{-# DEPRECATED drsGameServer "Use generic-lens or generic-optics with 'gameServer' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeGameServerResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
