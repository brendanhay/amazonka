{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.ResumeGameServerGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__
--
-- Reinstates activity on a game server group after it has been suspended. A game server group might be suspended by the'SuspendGameServerGroup' operation, or it might be suspended involuntarily due to a configuration problem. In the second case, you can manually resume activity on the group once the configuration problem has been resolved. Refer to the game server group status and status reason for more information on why group activity is suspended.
-- To resume activity, specify a game server group ARN and the type of activity to be resumed. If successful, a 'GameServerGroup' object is returned showing that the resumed activity is no longer listed in @SuspendedActions@ .
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
module Network.AWS.GameLift.ResumeGameServerGroup
  ( -- * Creating a request
    ResumeGameServerGroup (..),
    mkResumeGameServerGroup,

    -- ** Request lenses
    rgsgGameServerGroupName,
    rgsgResumeActions,

    -- * Destructuring the response
    ResumeGameServerGroupResponse (..),
    mkResumeGameServerGroupResponse,

    -- ** Response lenses
    rgsgrrsGameServerGroup,
    rgsgrrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkResumeGameServerGroup' smart constructor.
data ResumeGameServerGroup = ResumeGameServerGroup'
  { -- | A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
    gameServerGroupName :: Types.GameServerGroupName,
    -- | The activity to resume for this game server group.
    resumeActions :: Core.NonEmpty Types.GameServerGroupAction
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResumeGameServerGroup' value with any optional fields omitted.
mkResumeGameServerGroup ::
  -- | 'gameServerGroupName'
  Types.GameServerGroupName ->
  -- | 'resumeActions'
  Core.NonEmpty Types.GameServerGroupAction ->
  ResumeGameServerGroup
mkResumeGameServerGroup gameServerGroupName resumeActions =
  ResumeGameServerGroup' {gameServerGroupName, resumeActions}

-- | A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsgGameServerGroupName :: Lens.Lens' ResumeGameServerGroup Types.GameServerGroupName
rgsgGameServerGroupName = Lens.field @"gameServerGroupName"
{-# DEPRECATED rgsgGameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead." #-}

-- | The activity to resume for this game server group.
--
-- /Note:/ Consider using 'resumeActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsgResumeActions :: Lens.Lens' ResumeGameServerGroup (Core.NonEmpty Types.GameServerGroupAction)
rgsgResumeActions = Lens.field @"resumeActions"
{-# DEPRECATED rgsgResumeActions "Use generic-lens or generic-optics with 'resumeActions' instead." #-}

instance Core.FromJSON ResumeGameServerGroup where
  toJSON ResumeGameServerGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GameServerGroupName" Core..= gameServerGroupName),
            Core.Just ("ResumeActions" Core..= resumeActions)
          ]
      )

instance Core.AWSRequest ResumeGameServerGroup where
  type Rs ResumeGameServerGroup = ResumeGameServerGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.ResumeGameServerGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ResumeGameServerGroupResponse'
            Core.<$> (x Core..:? "GameServerGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkResumeGameServerGroupResponse' smart constructor.
data ResumeGameServerGroupResponse = ResumeGameServerGroupResponse'
  { -- | An object that describes the game server group resource, with the @SuspendedActions@ property updated to reflect the resumed activity.
    gameServerGroup :: Core.Maybe Types.GameServerGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ResumeGameServerGroupResponse' value with any optional fields omitted.
mkResumeGameServerGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ResumeGameServerGroupResponse
mkResumeGameServerGroupResponse responseStatus =
  ResumeGameServerGroupResponse'
    { gameServerGroup = Core.Nothing,
      responseStatus
    }

-- | An object that describes the game server group resource, with the @SuspendedActions@ property updated to reflect the resumed activity.
--
-- /Note:/ Consider using 'gameServerGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsgrrsGameServerGroup :: Lens.Lens' ResumeGameServerGroupResponse (Core.Maybe Types.GameServerGroup)
rgsgrrsGameServerGroup = Lens.field @"gameServerGroup"
{-# DEPRECATED rgsgrrsGameServerGroup "Use generic-lens or generic-optics with 'gameServerGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsgrrsResponseStatus :: Lens.Lens' ResumeGameServerGroupResponse Core.Int
rgsgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rgsgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
