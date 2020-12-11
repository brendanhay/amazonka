{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    sgsgrsGameServerGroup,
    sgsgrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSuspendGameServerGroup' smart constructor.
data SuspendGameServerGroup = SuspendGameServerGroup'
  { gameServerGroupName ::
      Lude.Text,
    suspendActions ::
      Lude.NonEmpty GameServerGroupAction
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SuspendGameServerGroup' with the minimum fields required to make a request.
--
-- * 'gameServerGroupName' - A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
-- * 'suspendActions' - The activity to suspend for this game server group.
mkSuspendGameServerGroup ::
  -- | 'gameServerGroupName'
  Lude.Text ->
  -- | 'suspendActions'
  Lude.NonEmpty GameServerGroupAction ->
  SuspendGameServerGroup
mkSuspendGameServerGroup pGameServerGroupName_ pSuspendActions_ =
  SuspendGameServerGroup'
    { gameServerGroupName =
        pGameServerGroupName_,
      suspendActions = pSuspendActions_
    }

-- | A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsgGameServerGroupName :: Lens.Lens' SuspendGameServerGroup Lude.Text
sgsgGameServerGroupName = Lens.lens (gameServerGroupName :: SuspendGameServerGroup -> Lude.Text) (\s a -> s {gameServerGroupName = a} :: SuspendGameServerGroup)
{-# DEPRECATED sgsgGameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead." #-}

-- | The activity to suspend for this game server group.
--
-- /Note:/ Consider using 'suspendActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsgSuspendActions :: Lens.Lens' SuspendGameServerGroup (Lude.NonEmpty GameServerGroupAction)
sgsgSuspendActions = Lens.lens (suspendActions :: SuspendGameServerGroup -> Lude.NonEmpty GameServerGroupAction) (\s a -> s {suspendActions = a} :: SuspendGameServerGroup)
{-# DEPRECATED sgsgSuspendActions "Use generic-lens or generic-optics with 'suspendActions' instead." #-}

instance Lude.AWSRequest SuspendGameServerGroup where
  type Rs SuspendGameServerGroup = SuspendGameServerGroupResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          SuspendGameServerGroupResponse'
            Lude.<$> (x Lude..?> "GameServerGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SuspendGameServerGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.SuspendGameServerGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SuspendGameServerGroup where
  toJSON SuspendGameServerGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GameServerGroupName" Lude..= gameServerGroupName),
            Lude.Just ("SuspendActions" Lude..= suspendActions)
          ]
      )

instance Lude.ToPath SuspendGameServerGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery SuspendGameServerGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSuspendGameServerGroupResponse' smart constructor.
data SuspendGameServerGroupResponse = SuspendGameServerGroupResponse'
  { gameServerGroup ::
      Lude.Maybe GameServerGroup,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SuspendGameServerGroupResponse' with the minimum fields required to make a request.
--
-- * 'gameServerGroup' - An object that describes the game server group resource, with the @SuspendedActions@ property updated to reflect the suspended activity.
-- * 'responseStatus' - The response status code.
mkSuspendGameServerGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SuspendGameServerGroupResponse
mkSuspendGameServerGroupResponse pResponseStatus_ =
  SuspendGameServerGroupResponse'
    { gameServerGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that describes the game server group resource, with the @SuspendedActions@ property updated to reflect the suspended activity.
--
-- /Note:/ Consider using 'gameServerGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsgrsGameServerGroup :: Lens.Lens' SuspendGameServerGroupResponse (Lude.Maybe GameServerGroup)
sgsgrsGameServerGroup = Lens.lens (gameServerGroup :: SuspendGameServerGroupResponse -> Lude.Maybe GameServerGroup) (\s a -> s {gameServerGroup = a} :: SuspendGameServerGroupResponse)
{-# DEPRECATED sgsgrsGameServerGroup "Use generic-lens or generic-optics with 'gameServerGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsgrsResponseStatus :: Lens.Lens' SuspendGameServerGroupResponse Lude.Int
sgsgrsResponseStatus = Lens.lens (responseStatus :: SuspendGameServerGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SuspendGameServerGroupResponse)
{-# DEPRECATED sgsgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
