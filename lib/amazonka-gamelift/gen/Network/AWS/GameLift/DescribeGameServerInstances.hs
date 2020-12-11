{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeGameServerInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__
--
-- Retrieves status information about the Amazon EC2 instances associated with a GameLift FleetIQ game server group. Use this operation to detect when instances are active or not available to host new game servers. If you are looking for instance configuration information, call 'DescribeGameServerGroup' or access the corresponding Auto Scaling group properties.
-- To request status for all instances in the game server group, provide a game server group ID only. To request status for specific instances, provide the game server group ID and one or more instance IDs. Use the pagination parameters to retrieve results in sequential segments. If successful, a collection of @GameServerInstance@ objects is returned.
-- This operation is not designed to be called with every game server claim request; this practice can cause you to exceed your API limit, which results in errors. Instead, as a best practice, cache the results and refresh your cache no more than once every 10 seconds.
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
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.DescribeGameServerInstances
  ( -- * Creating a request
    DescribeGameServerInstances (..),
    mkDescribeGameServerInstances,

    -- ** Request lenses
    dgsiNextToken,
    dgsiInstanceIds,
    dgsiLimit,
    dgsiGameServerGroupName,

    -- * Destructuring the response
    DescribeGameServerInstancesResponse (..),
    mkDescribeGameServerInstancesResponse,

    -- ** Response lenses
    dgsirsGameServerInstances,
    dgsirsNextToken,
    dgsirsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeGameServerInstances' smart constructor.
data DescribeGameServerInstances = DescribeGameServerInstances'
  { nextToken ::
      Lude.Maybe Lude.Text,
    instanceIds ::
      Lude.Maybe
        (Lude.NonEmpty Lude.Text),
    limit :: Lude.Maybe Lude.Natural,
    gameServerGroupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeGameServerInstances' with the minimum fields required to make a request.
--
-- * 'gameServerGroupName' - A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
-- * 'instanceIds' - The EC2 instance IDs that you want to retrieve status on. EC2 instance IDs use a 17-character format, for example: @i-1234567890abcdef0@ . To retrieve all instances in the game server group, leave this parameter empty.
-- * 'limit' - The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential segments.
-- * 'nextToken' - A token that indicates the start of the next sequential segment of results. Use the token returned with the previous call to this operation. To start at the beginning of the result set, do not specify a value.
mkDescribeGameServerInstances ::
  -- | 'gameServerGroupName'
  Lude.Text ->
  DescribeGameServerInstances
mkDescribeGameServerInstances pGameServerGroupName_ =
  DescribeGameServerInstances'
    { nextToken = Lude.Nothing,
      instanceIds = Lude.Nothing,
      limit = Lude.Nothing,
      gameServerGroupName = pGameServerGroupName_
    }

-- | A token that indicates the start of the next sequential segment of results. Use the token returned with the previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsiNextToken :: Lens.Lens' DescribeGameServerInstances (Lude.Maybe Lude.Text)
dgsiNextToken = Lens.lens (nextToken :: DescribeGameServerInstances -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeGameServerInstances)
{-# DEPRECATED dgsiNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The EC2 instance IDs that you want to retrieve status on. EC2 instance IDs use a 17-character format, for example: @i-1234567890abcdef0@ . To retrieve all instances in the game server group, leave this parameter empty.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsiInstanceIds :: Lens.Lens' DescribeGameServerInstances (Lude.Maybe (Lude.NonEmpty Lude.Text))
dgsiInstanceIds = Lens.lens (instanceIds :: DescribeGameServerInstances -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {instanceIds = a} :: DescribeGameServerInstances)
{-# DEPRECATED dgsiInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential segments.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsiLimit :: Lens.Lens' DescribeGameServerInstances (Lude.Maybe Lude.Natural)
dgsiLimit = Lens.lens (limit :: DescribeGameServerInstances -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeGameServerInstances)
{-# DEPRECATED dgsiLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsiGameServerGroupName :: Lens.Lens' DescribeGameServerInstances Lude.Text
dgsiGameServerGroupName = Lens.lens (gameServerGroupName :: DescribeGameServerInstances -> Lude.Text) (\s a -> s {gameServerGroupName = a} :: DescribeGameServerInstances)
{-# DEPRECATED dgsiGameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead." #-}

instance Page.AWSPager DescribeGameServerInstances where
  page rq rs
    | Page.stop (rs Lens.^. dgsirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dgsirsGameServerInstances) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dgsiNextToken Lens..~ rs Lens.^. dgsirsNextToken

instance Lude.AWSRequest DescribeGameServerInstances where
  type
    Rs DescribeGameServerInstances =
      DescribeGameServerInstancesResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeGameServerInstancesResponse'
            Lude.<$> (x Lude..?> "GameServerInstances" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeGameServerInstances where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DescribeGameServerInstances" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeGameServerInstances where
  toJSON DescribeGameServerInstances' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("InstanceIds" Lude..=) Lude.<$> instanceIds,
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just ("GameServerGroupName" Lude..= gameServerGroupName)
          ]
      )

instance Lude.ToPath DescribeGameServerInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeGameServerInstances where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeGameServerInstancesResponse' smart constructor.
data DescribeGameServerInstancesResponse = DescribeGameServerInstancesResponse'
  { gameServerInstances ::
      Lude.Maybe
        [GameServerInstance],
    nextToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeGameServerInstancesResponse' with the minimum fields required to make a request.
--
-- * 'gameServerInstances' - The collection of requested game server instances.
-- * 'nextToken' - A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
-- * 'responseStatus' - The response status code.
mkDescribeGameServerInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeGameServerInstancesResponse
mkDescribeGameServerInstancesResponse pResponseStatus_ =
  DescribeGameServerInstancesResponse'
    { gameServerInstances =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The collection of requested game server instances.
--
-- /Note:/ Consider using 'gameServerInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsirsGameServerInstances :: Lens.Lens' DescribeGameServerInstancesResponse (Lude.Maybe [GameServerInstance])
dgsirsGameServerInstances = Lens.lens (gameServerInstances :: DescribeGameServerInstancesResponse -> Lude.Maybe [GameServerInstance]) (\s a -> s {gameServerInstances = a} :: DescribeGameServerInstancesResponse)
{-# DEPRECATED dgsirsGameServerInstances "Use generic-lens or generic-optics with 'gameServerInstances' instead." #-}

-- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsirsNextToken :: Lens.Lens' DescribeGameServerInstancesResponse (Lude.Maybe Lude.Text)
dgsirsNextToken = Lens.lens (nextToken :: DescribeGameServerInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeGameServerInstancesResponse)
{-# DEPRECATED dgsirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsirsResponseStatus :: Lens.Lens' DescribeGameServerInstancesResponse Lude.Int
dgsirsResponseStatus = Lens.lens (responseStatus :: DescribeGameServerInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeGameServerInstancesResponse)
{-# DEPRECATED dgsirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
