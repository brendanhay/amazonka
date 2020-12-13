{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.ListGameServers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__
--
-- Retrieves information on all game servers that are currently active in a specified game server group. You can opt to sort the list by game server age. Use the pagination parameters to retrieve results in a set of sequential segments.
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/gsg-intro.html GameLift FleetIQ Guide>
-- __Related operations__
--
--     * 'RegisterGameServer'
--
--
--     * 'ListGameServers'
--
--
--     * 'ClaimGameServer'
--
--
--     * 'DescribeGameServer'
--
--
--     * 'UpdateGameServer'
--
--
--     * 'DeregisterGameServer'
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.ListGameServers
  ( -- * Creating a request
    ListGameServers (..),
    mkListGameServers,

    -- ** Request lenses
    lgsGameServerGroupName,
    lgsNextToken,
    lgsSortOrder,
    lgsLimit,

    -- * Destructuring the response
    ListGameServersResponse (..),
    mkListGameServersResponse,

    -- ** Response lenses
    lgsrsGameServers,
    lgsrsNextToken,
    lgsrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListGameServers' smart constructor.
data ListGameServers = ListGameServers'
  { -- | An identifier for the game server group to retrieve a list of game servers from. Use either the 'GameServerGroup' name or ARN value.
    gameServerGroupName :: Lude.Text,
    -- | A token that indicates the start of the next sequential segment of results. Use the token returned with the previous call to this operation. To start at the beginning of the result set, do not specify a value.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Indicates how to sort the returned data based on game server registration timestamp. Use ASCENDING to retrieve oldest game servers first, or use DESCENDING to retrieve newest game servers first. If this parameter is left empty, game servers are returned in no particular order.
    sortOrder :: Lude.Maybe SortOrder,
    -- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential segments.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGameServers' with the minimum fields required to make a request.
--
-- * 'gameServerGroupName' - An identifier for the game server group to retrieve a list of game servers from. Use either the 'GameServerGroup' name or ARN value.
-- * 'nextToken' - A token that indicates the start of the next sequential segment of results. Use the token returned with the previous call to this operation. To start at the beginning of the result set, do not specify a value.
-- * 'sortOrder' - Indicates how to sort the returned data based on game server registration timestamp. Use ASCENDING to retrieve oldest game servers first, or use DESCENDING to retrieve newest game servers first. If this parameter is left empty, game servers are returned in no particular order.
-- * 'limit' - The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential segments.
mkListGameServers ::
  -- | 'gameServerGroupName'
  Lude.Text ->
  ListGameServers
mkListGameServers pGameServerGroupName_ =
  ListGameServers'
    { gameServerGroupName = pGameServerGroupName_,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | An identifier for the game server group to retrieve a list of game servers from. Use either the 'GameServerGroup' name or ARN value.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgsGameServerGroupName :: Lens.Lens' ListGameServers Lude.Text
lgsGameServerGroupName = Lens.lens (gameServerGroupName :: ListGameServers -> Lude.Text) (\s a -> s {gameServerGroupName = a} :: ListGameServers)
{-# DEPRECATED lgsGameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead." #-}

-- | A token that indicates the start of the next sequential segment of results. Use the token returned with the previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgsNextToken :: Lens.Lens' ListGameServers (Lude.Maybe Lude.Text)
lgsNextToken = Lens.lens (nextToken :: ListGameServers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListGameServers)
{-# DEPRECATED lgsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Indicates how to sort the returned data based on game server registration timestamp. Use ASCENDING to retrieve oldest game servers first, or use DESCENDING to retrieve newest game servers first. If this parameter is left empty, game servers are returned in no particular order.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgsSortOrder :: Lens.Lens' ListGameServers (Lude.Maybe SortOrder)
lgsSortOrder = Lens.lens (sortOrder :: ListGameServers -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListGameServers)
{-# DEPRECATED lgsSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential segments.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgsLimit :: Lens.Lens' ListGameServers (Lude.Maybe Lude.Natural)
lgsLimit = Lens.lens (limit :: ListGameServers -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListGameServers)
{-# DEPRECATED lgsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager ListGameServers where
  page rq rs
    | Page.stop (rs Lens.^. lgsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lgsrsGameServers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lgsNextToken Lens..~ rs Lens.^. lgsrsNextToken

instance Lude.AWSRequest ListGameServers where
  type Rs ListGameServers = ListGameServersResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListGameServersResponse'
            Lude.<$> (x Lude..?> "GameServers" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListGameServers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.ListGameServers" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListGameServers where
  toJSON ListGameServers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GameServerGroupName" Lude..= gameServerGroupName),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListGameServers where
  toPath = Lude.const "/"

instance Lude.ToQuery ListGameServers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListGameServersResponse' smart constructor.
data ListGameServersResponse = ListGameServersResponse'
  { -- | A collection of game server objects that match the request.
    gameServers :: Lude.Maybe [GameServer],
    -- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGameServersResponse' with the minimum fields required to make a request.
--
-- * 'gameServers' - A collection of game server objects that match the request.
-- * 'nextToken' - A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
-- * 'responseStatus' - The response status code.
mkListGameServersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListGameServersResponse
mkListGameServersResponse pResponseStatus_ =
  ListGameServersResponse'
    { gameServers = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A collection of game server objects that match the request.
--
-- /Note:/ Consider using 'gameServers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgsrsGameServers :: Lens.Lens' ListGameServersResponse (Lude.Maybe [GameServer])
lgsrsGameServers = Lens.lens (gameServers :: ListGameServersResponse -> Lude.Maybe [GameServer]) (\s a -> s {gameServers = a} :: ListGameServersResponse)
{-# DEPRECATED lgsrsGameServers "Use generic-lens or generic-optics with 'gameServers' instead." #-}

-- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgsrsNextToken :: Lens.Lens' ListGameServersResponse (Lude.Maybe Lude.Text)
lgsrsNextToken = Lens.lens (nextToken :: ListGameServersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListGameServersResponse)
{-# DEPRECATED lgsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgsrsResponseStatus :: Lens.Lens' ListGameServersResponse Lude.Int
lgsrsResponseStatus = Lens.lens (responseStatus :: ListGameServersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListGameServersResponse)
{-# DEPRECATED lgsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
