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
    dgsgGameServerGroupName,

    -- * Destructuring the response
    DescribeGameServerGroupResponse (..),
    mkDescribeGameServerGroupResponse,

    -- ** Response lenses
    dgsgfrsGameServerGroup,
    dgsgfrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeGameServerGroup' smart constructor.
newtype DescribeGameServerGroup = DescribeGameServerGroup'
  { -- | A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
    gameServerGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeGameServerGroup' with the minimum fields required to make a request.
--
-- * 'gameServerGroupName' - A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
mkDescribeGameServerGroup ::
  -- | 'gameServerGroupName'
  Lude.Text ->
  DescribeGameServerGroup
mkDescribeGameServerGroup pGameServerGroupName_ =
  DescribeGameServerGroup'
    { gameServerGroupName =
        pGameServerGroupName_
    }

-- | A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsgGameServerGroupName :: Lens.Lens' DescribeGameServerGroup Lude.Text
dgsgGameServerGroupName = Lens.lens (gameServerGroupName :: DescribeGameServerGroup -> Lude.Text) (\s a -> s {gameServerGroupName = a} :: DescribeGameServerGroup)
{-# DEPRECATED dgsgGameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead." #-}

instance Lude.AWSRequest DescribeGameServerGroup where
  type Rs DescribeGameServerGroup = DescribeGameServerGroupResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeGameServerGroupResponse'
            Lude.<$> (x Lude..?> "GameServerGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeGameServerGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DescribeGameServerGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeGameServerGroup where
  toJSON DescribeGameServerGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("GameServerGroupName" Lude..= gameServerGroupName)]
      )

instance Lude.ToPath DescribeGameServerGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeGameServerGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeGameServerGroupResponse' smart constructor.
data DescribeGameServerGroupResponse = DescribeGameServerGroupResponse'
  { -- | An object with the property settings for the requested game server group resource.
    gameServerGroup :: Lude.Maybe GameServerGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeGameServerGroupResponse' with the minimum fields required to make a request.
--
-- * 'gameServerGroup' - An object with the property settings for the requested game server group resource.
-- * 'responseStatus' - The response status code.
mkDescribeGameServerGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeGameServerGroupResponse
mkDescribeGameServerGroupResponse pResponseStatus_ =
  DescribeGameServerGroupResponse'
    { gameServerGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object with the property settings for the requested game server group resource.
--
-- /Note:/ Consider using 'gameServerGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsgfrsGameServerGroup :: Lens.Lens' DescribeGameServerGroupResponse (Lude.Maybe GameServerGroup)
dgsgfrsGameServerGroup = Lens.lens (gameServerGroup :: DescribeGameServerGroupResponse -> Lude.Maybe GameServerGroup) (\s a -> s {gameServerGroup = a} :: DescribeGameServerGroupResponse)
{-# DEPRECATED dgsgfrsGameServerGroup "Use generic-lens or generic-optics with 'gameServerGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsgfrsResponseStatus :: Lens.Lens' DescribeGameServerGroupResponse Lude.Int
dgsgfrsResponseStatus = Lens.lens (responseStatus :: DescribeGameServerGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeGameServerGroupResponse)
{-# DEPRECATED dgsgfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
