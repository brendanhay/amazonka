{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DeleteGameServerGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__
--
-- Terminates a game server group and permanently deletes the game server group record. You have several options for how these resources are impacted when deleting the game server group. Depending on the type of delete operation selected, this operation might affect these resources:
--
--     * The game server group
--
--
--     * The corresponding Auto Scaling group
--
--
--     * All game servers that are currently running in the group
--
--
-- To delete a game server group, identify the game server group to delete and specify the type of delete operation to initiate. Game server groups can only be deleted if they are in @ACTIVE@ or @ERROR@ status.
-- If the delete request is successful, a series of operations are kicked off. The game server group status is changed to @DELETE_SCHEDULED@ , which prevents new game servers from being registered and stops automatic scaling activity. Once all game servers in the game server group are deregistered, GameLift FleetIQ can begin deleting resources. If any of the delete operations fail, the game server group is placed in @ERROR@ status.
-- GameLift FleetIQ emits delete events to Amazon CloudWatch.
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
module Network.AWS.GameLift.DeleteGameServerGroup
  ( -- * Creating a request
    DeleteGameServerGroup (..),
    mkDeleteGameServerGroup,

    -- ** Request lenses
    dgsgGameServerGroupName,
    dgsgDeleteOption,

    -- * Destructuring the response
    DeleteGameServerGroupResponse (..),
    mkDeleteGameServerGroupResponse,

    -- ** Response lenses
    dgsgrrsGameServerGroup,
    dgsgrrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteGameServerGroup' smart constructor.
data DeleteGameServerGroup = DeleteGameServerGroup'
  { -- | A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
    gameServerGroupName :: Types.GameServerGroupName,
    -- | The type of delete to perform. Options include the following:
    --
    --
    --     * @SAFE_DELETE@ – (default) Terminates the game server group and EC2 Auto Scaling group only when it has no game servers that are in @UTILIZED@ status.
    --
    --
    --     * @FORCE_DELETE@ – Terminates the game server group, including all active game servers regardless of their utilization status, and the EC2 Auto Scaling group.
    --
    --
    --     * @RETAIN@ – Does a safe delete of the game server group but retains the EC2 Auto Scaling group as is.
    deleteOption :: Core.Maybe Types.GameServerGroupDeleteOption
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGameServerGroup' value with any optional fields omitted.
mkDeleteGameServerGroup ::
  -- | 'gameServerGroupName'
  Types.GameServerGroupName ->
  DeleteGameServerGroup
mkDeleteGameServerGroup gameServerGroupName =
  DeleteGameServerGroup'
    { gameServerGroupName,
      deleteOption = Core.Nothing
    }

-- | A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsgGameServerGroupName :: Lens.Lens' DeleteGameServerGroup Types.GameServerGroupName
dgsgGameServerGroupName = Lens.field @"gameServerGroupName"
{-# DEPRECATED dgsgGameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead." #-}

-- | The type of delete to perform. Options include the following:
--
--
--     * @SAFE_DELETE@ – (default) Terminates the game server group and EC2 Auto Scaling group only when it has no game servers that are in @UTILIZED@ status.
--
--
--     * @FORCE_DELETE@ – Terminates the game server group, including all active game servers regardless of their utilization status, and the EC2 Auto Scaling group.
--
--
--     * @RETAIN@ – Does a safe delete of the game server group but retains the EC2 Auto Scaling group as is.
--
--
--
-- /Note:/ Consider using 'deleteOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsgDeleteOption :: Lens.Lens' DeleteGameServerGroup (Core.Maybe Types.GameServerGroupDeleteOption)
dgsgDeleteOption = Lens.field @"deleteOption"
{-# DEPRECATED dgsgDeleteOption "Use generic-lens or generic-optics with 'deleteOption' instead." #-}

instance Core.FromJSON DeleteGameServerGroup where
  toJSON DeleteGameServerGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GameServerGroupName" Core..= gameServerGroupName),
            ("DeleteOption" Core..=) Core.<$> deleteOption
          ]
      )

instance Core.AWSRequest DeleteGameServerGroup where
  type Rs DeleteGameServerGroup = DeleteGameServerGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.DeleteGameServerGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteGameServerGroupResponse'
            Core.<$> (x Core..:? "GameServerGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteGameServerGroupResponse' smart constructor.
data DeleteGameServerGroupResponse = DeleteGameServerGroupResponse'
  { -- | An object that describes the deleted game server group resource, with status updated to @DELETE_SCHEDULED@ .
    gameServerGroup :: Core.Maybe Types.GameServerGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteGameServerGroupResponse' value with any optional fields omitted.
mkDeleteGameServerGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteGameServerGroupResponse
mkDeleteGameServerGroupResponse responseStatus =
  DeleteGameServerGroupResponse'
    { gameServerGroup = Core.Nothing,
      responseStatus
    }

-- | An object that describes the deleted game server group resource, with status updated to @DELETE_SCHEDULED@ .
--
-- /Note:/ Consider using 'gameServerGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsgrrsGameServerGroup :: Lens.Lens' DeleteGameServerGroupResponse (Core.Maybe Types.GameServerGroup)
dgsgrrsGameServerGroup = Lens.field @"gameServerGroup"
{-# DEPRECATED dgsgrrsGameServerGroup "Use generic-lens or generic-optics with 'gameServerGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsgrrsResponseStatus :: Lens.Lens' DeleteGameServerGroupResponse Core.Int
dgsgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dgsgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
