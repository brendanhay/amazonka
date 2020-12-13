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
    dgsgfDeleteOption,
    dgsgfGameServerGroupName,

    -- * Destructuring the response
    DeleteGameServerGroupResponse (..),
    mkDeleteGameServerGroupResponse,

    -- ** Response lenses
    dgsgrsGameServerGroup,
    dgsgrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteGameServerGroup' smart constructor.
data DeleteGameServerGroup = DeleteGameServerGroup'
  { -- | The type of delete to perform. Options include the following:
    --
    --
    --     * @SAFE_DELETE@ – (default) Terminates the game server group and EC2 Auto Scaling group only when it has no game servers that are in @UTILIZED@ status.
    --
    --
    --     * @FORCE_DELETE@ – Terminates the game server group, including all active game servers regardless of their utilization status, and the EC2 Auto Scaling group.
    --
    --
    --     * @RETAIN@ – Does a safe delete of the game server group but retains the EC2 Auto Scaling group as is.
    deleteOption :: Lude.Maybe GameServerGroupDeleteOption,
    -- | A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
    gameServerGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGameServerGroup' with the minimum fields required to make a request.
--
-- * 'deleteOption' - The type of delete to perform. Options include the following:
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
-- * 'gameServerGroupName' - A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
mkDeleteGameServerGroup ::
  -- | 'gameServerGroupName'
  Lude.Text ->
  DeleteGameServerGroup
mkDeleteGameServerGroup pGameServerGroupName_ =
  DeleteGameServerGroup'
    { deleteOption = Lude.Nothing,
      gameServerGroupName = pGameServerGroupName_
    }

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
dgsgfDeleteOption :: Lens.Lens' DeleteGameServerGroup (Lude.Maybe GameServerGroupDeleteOption)
dgsgfDeleteOption = Lens.lens (deleteOption :: DeleteGameServerGroup -> Lude.Maybe GameServerGroupDeleteOption) (\s a -> s {deleteOption = a} :: DeleteGameServerGroup)
{-# DEPRECATED dgsgfDeleteOption "Use generic-lens or generic-optics with 'deleteOption' instead." #-}

-- | A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsgfGameServerGroupName :: Lens.Lens' DeleteGameServerGroup Lude.Text
dgsgfGameServerGroupName = Lens.lens (gameServerGroupName :: DeleteGameServerGroup -> Lude.Text) (\s a -> s {gameServerGroupName = a} :: DeleteGameServerGroup)
{-# DEPRECATED dgsgfGameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead." #-}

instance Lude.AWSRequest DeleteGameServerGroup where
  type Rs DeleteGameServerGroup = DeleteGameServerGroupResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteGameServerGroupResponse'
            Lude.<$> (x Lude..?> "GameServerGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteGameServerGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DeleteGameServerGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteGameServerGroup where
  toJSON DeleteGameServerGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DeleteOption" Lude..=) Lude.<$> deleteOption,
            Lude.Just ("GameServerGroupName" Lude..= gameServerGroupName)
          ]
      )

instance Lude.ToPath DeleteGameServerGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteGameServerGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteGameServerGroupResponse' smart constructor.
data DeleteGameServerGroupResponse = DeleteGameServerGroupResponse'
  { -- | An object that describes the deleted game server group resource, with status updated to @DELETE_SCHEDULED@ .
    gameServerGroup :: Lude.Maybe GameServerGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGameServerGroupResponse' with the minimum fields required to make a request.
--
-- * 'gameServerGroup' - An object that describes the deleted game server group resource, with status updated to @DELETE_SCHEDULED@ .
-- * 'responseStatus' - The response status code.
mkDeleteGameServerGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteGameServerGroupResponse
mkDeleteGameServerGroupResponse pResponseStatus_ =
  DeleteGameServerGroupResponse'
    { gameServerGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that describes the deleted game server group resource, with status updated to @DELETE_SCHEDULED@ .
--
-- /Note:/ Consider using 'gameServerGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsgrsGameServerGroup :: Lens.Lens' DeleteGameServerGroupResponse (Lude.Maybe GameServerGroup)
dgsgrsGameServerGroup = Lens.lens (gameServerGroup :: DeleteGameServerGroupResponse -> Lude.Maybe GameServerGroup) (\s a -> s {gameServerGroup = a} :: DeleteGameServerGroupResponse)
{-# DEPRECATED dgsgrsGameServerGroup "Use generic-lens or generic-optics with 'gameServerGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsgrsResponseStatus :: Lens.Lens' DeleteGameServerGroupResponse Lude.Int
dgsgrsResponseStatus = Lens.lens (responseStatus :: DeleteGameServerGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteGameServerGroupResponse)
{-# DEPRECATED dgsgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
