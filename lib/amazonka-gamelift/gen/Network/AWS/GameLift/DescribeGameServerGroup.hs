{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeGameServerGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__
--
-- Retrieves information on a game server group. This operation returns only properties related to GameLift FleetIQ. To view or update properties for the corresponding Auto Scaling group, such as launch template, auto scaling policies, and maximum/minimum group size, access the Auto Scaling group directly.
-- To get attributes for a game server group, provide a group name or ARN value. If successful, a 'GameServerGroup' object is returned.
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/gsg-intro.html GameLift FleetIQ Guide>
-- __Related operations__
--
--     * 'CreateGameServerGroup'
--
--
--     * 'ListGameServerGroups'
--
--
--     * 'DescribeGameServerGroup'
--
--
--     * 'UpdateGameServerGroup'
--
--
--     * 'DeleteGameServerGroup'
--
--
--     * 'ResumeGameServerGroup'
--
--
--     * 'SuspendGameServerGroup'
--
--
--     * 'DescribeGameServerInstances'
module Network.AWS.GameLift.DescribeGameServerGroup
  ( -- * Creating a request
    DescribeGameServerGroup (..),
    mkDescribeGameServerGroup,

    -- ** Request lenses
    dgsgfGameServerGroupName,

    -- * Destructuring the response
    DescribeGameServerGroupResponse (..),
    mkDescribeGameServerGroupResponse,

    -- ** Response lenses
    dgsgrfrsGameServerGroup,
    dgsgrfrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeGameServerGroup' smart constructor.
newtype DescribeGameServerGroup = DescribeGameServerGroup'
  { -- | A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
    gameServerGroupName :: Types.GameServerGroupName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeGameServerGroup' value with any optional fields omitted.
mkDescribeGameServerGroup ::
  -- | 'gameServerGroupName'
  Types.GameServerGroupName ->
  DescribeGameServerGroup
mkDescribeGameServerGroup gameServerGroupName =
  DescribeGameServerGroup' {gameServerGroupName}

-- | A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsgfGameServerGroupName :: Lens.Lens' DescribeGameServerGroup Types.GameServerGroupName
dgsgfGameServerGroupName = Lens.field @"gameServerGroupName"
{-# DEPRECATED dgsgfGameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead." #-}

instance Core.FromJSON DescribeGameServerGroup where
  toJSON DescribeGameServerGroup {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("GameServerGroupName" Core..= gameServerGroupName)]
      )

instance Core.AWSRequest DescribeGameServerGroup where
  type Rs DescribeGameServerGroup = DescribeGameServerGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.DescribeGameServerGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGameServerGroupResponse'
            Core.<$> (x Core..:? "GameServerGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeGameServerGroupResponse' smart constructor.
data DescribeGameServerGroupResponse = DescribeGameServerGroupResponse'
  { -- | An object with the property settings for the requested game server group resource.
    gameServerGroup :: Core.Maybe Types.GameServerGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeGameServerGroupResponse' value with any optional fields omitted.
mkDescribeGameServerGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeGameServerGroupResponse
mkDescribeGameServerGroupResponse responseStatus =
  DescribeGameServerGroupResponse'
    { gameServerGroup = Core.Nothing,
      responseStatus
    }

-- | An object with the property settings for the requested game server group resource.
--
-- /Note:/ Consider using 'gameServerGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsgrfrsGameServerGroup :: Lens.Lens' DescribeGameServerGroupResponse (Core.Maybe Types.GameServerGroup)
dgsgrfrsGameServerGroup = Lens.field @"gameServerGroup"
{-# DEPRECATED dgsgrfrsGameServerGroup "Use generic-lens or generic-optics with 'gameServerGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsgrfrsResponseStatus :: Lens.Lens' DescribeGameServerGroupResponse Core.Int
dgsgrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dgsgrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
