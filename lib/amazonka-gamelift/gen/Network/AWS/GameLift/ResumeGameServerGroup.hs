{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    rgsgrsGameServerGroup,
    rgsgrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkResumeGameServerGroup' smart constructor.
data ResumeGameServerGroup = ResumeGameServerGroup'
  { gameServerGroupName ::
      Lude.Text,
    resumeActions ::
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

-- | Creates a value of 'ResumeGameServerGroup' with the minimum fields required to make a request.
--
-- * 'gameServerGroupName' - A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
-- * 'resumeActions' - The activity to resume for this game server group.
mkResumeGameServerGroup ::
  -- | 'gameServerGroupName'
  Lude.Text ->
  -- | 'resumeActions'
  Lude.NonEmpty GameServerGroupAction ->
  ResumeGameServerGroup
mkResumeGameServerGroup pGameServerGroupName_ pResumeActions_ =
  ResumeGameServerGroup'
    { gameServerGroupName =
        pGameServerGroupName_,
      resumeActions = pResumeActions_
    }

-- | A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsgGameServerGroupName :: Lens.Lens' ResumeGameServerGroup Lude.Text
rgsgGameServerGroupName = Lens.lens (gameServerGroupName :: ResumeGameServerGroup -> Lude.Text) (\s a -> s {gameServerGroupName = a} :: ResumeGameServerGroup)
{-# DEPRECATED rgsgGameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead." #-}

-- | The activity to resume for this game server group.
--
-- /Note:/ Consider using 'resumeActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsgResumeActions :: Lens.Lens' ResumeGameServerGroup (Lude.NonEmpty GameServerGroupAction)
rgsgResumeActions = Lens.lens (resumeActions :: ResumeGameServerGroup -> Lude.NonEmpty GameServerGroupAction) (\s a -> s {resumeActions = a} :: ResumeGameServerGroup)
{-# DEPRECATED rgsgResumeActions "Use generic-lens or generic-optics with 'resumeActions' instead." #-}

instance Lude.AWSRequest ResumeGameServerGroup where
  type Rs ResumeGameServerGroup = ResumeGameServerGroupResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          ResumeGameServerGroupResponse'
            Lude.<$> (x Lude..?> "GameServerGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ResumeGameServerGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.ResumeGameServerGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ResumeGameServerGroup where
  toJSON ResumeGameServerGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GameServerGroupName" Lude..= gameServerGroupName),
            Lude.Just ("ResumeActions" Lude..= resumeActions)
          ]
      )

instance Lude.ToPath ResumeGameServerGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery ResumeGameServerGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkResumeGameServerGroupResponse' smart constructor.
data ResumeGameServerGroupResponse = ResumeGameServerGroupResponse'
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

-- | Creates a value of 'ResumeGameServerGroupResponse' with the minimum fields required to make a request.
--
-- * 'gameServerGroup' - An object that describes the game server group resource, with the @SuspendedActions@ property updated to reflect the resumed activity.
-- * 'responseStatus' - The response status code.
mkResumeGameServerGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ResumeGameServerGroupResponse
mkResumeGameServerGroupResponse pResponseStatus_ =
  ResumeGameServerGroupResponse'
    { gameServerGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that describes the game server group resource, with the @SuspendedActions@ property updated to reflect the resumed activity.
--
-- /Note:/ Consider using 'gameServerGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsgrsGameServerGroup :: Lens.Lens' ResumeGameServerGroupResponse (Lude.Maybe GameServerGroup)
rgsgrsGameServerGroup = Lens.lens (gameServerGroup :: ResumeGameServerGroupResponse -> Lude.Maybe GameServerGroup) (\s a -> s {gameServerGroup = a} :: ResumeGameServerGroupResponse)
{-# DEPRECATED rgsgrsGameServerGroup "Use generic-lens or generic-optics with 'gameServerGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsgrsResponseStatus :: Lens.Lens' ResumeGameServerGroupResponse Lude.Int
rgsgrsResponseStatus = Lens.lens (responseStatus :: ResumeGameServerGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ResumeGameServerGroupResponse)
{-# DEPRECATED rgsgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
