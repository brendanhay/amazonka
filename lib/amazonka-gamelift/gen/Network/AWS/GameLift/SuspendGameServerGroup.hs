{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.SuspendGameServerGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__
--
-- Temporarily stops activity on a game server group without terminating instances or the game server group. You can restart activity by calling 'ResumeGameServerGroup' . You can suspend the following activity:
--
--     * __Instance type replacement__ - This activity evaluates the current game hosting viability of all Spot instance types that are defined for the game server group. It updates the Auto Scaling group to remove nonviable Spot Instance types, which have a higher chance of game server interruptions. It then balances capacity across the remaining viable Spot Instance types. When this activity is suspended, the Auto Scaling group continues with its current balance, regardless of viability. Instance protection, utilization metrics, and capacity scaling activities continue to be active.
--
--
-- To suspend activity, specify a game server group ARN and the type of activity to be suspended. If successful, a 'GameServerGroup' object is returned showing that the activity is listed in @SuspendedActions@ .
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
module Network.AWS.GameLift.SuspendGameServerGroup
  ( -- * Creating a request
    SuspendGameServerGroup (..),
    mkSuspendGameServerGroup,

    -- ** Request lenses
    sgsgGameServerGroupName,
    sgsgSuspendActions,

    -- * Destructuring the response
    SuspendGameServerGroupResponse (..),
    mkSuspendGameServerGroupResponse,

    -- ** Response lenses
    sgsgrrsGameServerGroup,
    sgsgrrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSuspendGameServerGroup' smart constructor.
data SuspendGameServerGroup = SuspendGameServerGroup'
  { -- | A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
    gameServerGroupName :: Types.GameServerGroupName,
    -- | The activity to suspend for this game server group.
    suspendActions :: Core.NonEmpty Types.GameServerGroupAction
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SuspendGameServerGroup' value with any optional fields omitted.
mkSuspendGameServerGroup ::
  -- | 'gameServerGroupName'
  Types.GameServerGroupName ->
  -- | 'suspendActions'
  Core.NonEmpty Types.GameServerGroupAction ->
  SuspendGameServerGroup
mkSuspendGameServerGroup gameServerGroupName suspendActions =
  SuspendGameServerGroup' {gameServerGroupName, suspendActions}

-- | A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsgGameServerGroupName :: Lens.Lens' SuspendGameServerGroup Types.GameServerGroupName
sgsgGameServerGroupName = Lens.field @"gameServerGroupName"
{-# DEPRECATED sgsgGameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead." #-}

-- | The activity to suspend for this game server group.
--
-- /Note:/ Consider using 'suspendActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsgSuspendActions :: Lens.Lens' SuspendGameServerGroup (Core.NonEmpty Types.GameServerGroupAction)
sgsgSuspendActions = Lens.field @"suspendActions"
{-# DEPRECATED sgsgSuspendActions "Use generic-lens or generic-optics with 'suspendActions' instead." #-}

instance Core.FromJSON SuspendGameServerGroup where
  toJSON SuspendGameServerGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GameServerGroupName" Core..= gameServerGroupName),
            Core.Just ("SuspendActions" Core..= suspendActions)
          ]
      )

instance Core.AWSRequest SuspendGameServerGroup where
  type Rs SuspendGameServerGroup = SuspendGameServerGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.SuspendGameServerGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          SuspendGameServerGroupResponse'
            Core.<$> (x Core..:? "GameServerGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkSuspendGameServerGroupResponse' smart constructor.
data SuspendGameServerGroupResponse = SuspendGameServerGroupResponse'
  { -- | An object that describes the game server group resource, with the @SuspendedActions@ property updated to reflect the suspended activity.
    gameServerGroup :: Core.Maybe Types.GameServerGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SuspendGameServerGroupResponse' value with any optional fields omitted.
mkSuspendGameServerGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SuspendGameServerGroupResponse
mkSuspendGameServerGroupResponse responseStatus =
  SuspendGameServerGroupResponse'
    { gameServerGroup = Core.Nothing,
      responseStatus
    }

-- | An object that describes the game server group resource, with the @SuspendedActions@ property updated to reflect the suspended activity.
--
-- /Note:/ Consider using 'gameServerGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsgrrsGameServerGroup :: Lens.Lens' SuspendGameServerGroupResponse (Core.Maybe Types.GameServerGroup)
sgsgrrsGameServerGroup = Lens.field @"gameServerGroup"
{-# DEPRECATED sgsgrrsGameServerGroup "Use generic-lens or generic-optics with 'gameServerGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsgrrsResponseStatus :: Lens.Lens' SuspendGameServerGroupResponse Core.Int
sgsgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sgsgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
