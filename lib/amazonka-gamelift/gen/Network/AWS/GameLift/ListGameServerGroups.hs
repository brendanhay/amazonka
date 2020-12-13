{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.ListGameServerGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__
--
-- Retrieves information on all game servers groups that exist in the current AWS account for the selected Region. Use the pagination parameters to retrieve results in a set of sequential segments.
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
module Network.AWS.GameLift.ListGameServerGroups
  ( -- * Creating a request
    ListGameServerGroups (..),
    mkListGameServerGroups,

    -- ** Request lenses
    lgsgNextToken,
    lgsgLimit,

    -- * Destructuring the response
    ListGameServerGroupsResponse (..),
    mkListGameServerGroupsResponse,

    -- ** Response lenses
    lgsgrsGameServerGroups,
    lgsgrsNextToken,
    lgsgrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListGameServerGroups' smart constructor.
data ListGameServerGroups = ListGameServerGroups'
  { -- | A token that indicates the start of the next sequential segment of results. Use the token returned with the previous call to this operation. To start at the beginning of the result set, do not specify a value.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential segments.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGameServerGroups' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token that indicates the start of the next sequential segment of results. Use the token returned with the previous call to this operation. To start at the beginning of the result set, do not specify a value.
-- * 'limit' - The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential segments.
mkListGameServerGroups ::
  ListGameServerGroups
mkListGameServerGroups =
  ListGameServerGroups'
    { nextToken = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | A token that indicates the start of the next sequential segment of results. Use the token returned with the previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgsgNextToken :: Lens.Lens' ListGameServerGroups (Lude.Maybe Lude.Text)
lgsgNextToken = Lens.lens (nextToken :: ListGameServerGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListGameServerGroups)
{-# DEPRECATED lgsgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential segments.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgsgLimit :: Lens.Lens' ListGameServerGroups (Lude.Maybe Lude.Natural)
lgsgLimit = Lens.lens (limit :: ListGameServerGroups -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListGameServerGroups)
{-# DEPRECATED lgsgLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager ListGameServerGroups where
  page rq rs
    | Page.stop (rs Lens.^. lgsgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lgsgrsGameServerGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lgsgNextToken Lens..~ rs Lens.^. lgsgrsNextToken

instance Lude.AWSRequest ListGameServerGroups where
  type Rs ListGameServerGroups = ListGameServerGroupsResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListGameServerGroupsResponse'
            Lude.<$> (x Lude..?> "GameServerGroups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListGameServerGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.ListGameServerGroups" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListGameServerGroups where
  toJSON ListGameServerGroups' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListGameServerGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery ListGameServerGroups where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListGameServerGroupsResponse' smart constructor.
data ListGameServerGroupsResponse = ListGameServerGroupsResponse'
  { -- | A collection of game server group objects that match the request.
    gameServerGroups :: Lude.Maybe [GameServerGroup],
    -- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGameServerGroupsResponse' with the minimum fields required to make a request.
--
-- * 'gameServerGroups' - A collection of game server group objects that match the request.
-- * 'nextToken' - A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
-- * 'responseStatus' - The response status code.
mkListGameServerGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListGameServerGroupsResponse
mkListGameServerGroupsResponse pResponseStatus_ =
  ListGameServerGroupsResponse'
    { gameServerGroups = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A collection of game server group objects that match the request.
--
-- /Note:/ Consider using 'gameServerGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgsgrsGameServerGroups :: Lens.Lens' ListGameServerGroupsResponse (Lude.Maybe [GameServerGroup])
lgsgrsGameServerGroups = Lens.lens (gameServerGroups :: ListGameServerGroupsResponse -> Lude.Maybe [GameServerGroup]) (\s a -> s {gameServerGroups = a} :: ListGameServerGroupsResponse)
{-# DEPRECATED lgsgrsGameServerGroups "Use generic-lens or generic-optics with 'gameServerGroups' instead." #-}

-- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgsgrsNextToken :: Lens.Lens' ListGameServerGroupsResponse (Lude.Maybe Lude.Text)
lgsgrsNextToken = Lens.lens (nextToken :: ListGameServerGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListGameServerGroupsResponse)
{-# DEPRECATED lgsgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgsgrsResponseStatus :: Lens.Lens' ListGameServerGroupsResponse Lude.Int
lgsgrsResponseStatus = Lens.lens (responseStatus :: ListGameServerGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListGameServerGroupsResponse)
{-# DEPRECATED lgsgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
