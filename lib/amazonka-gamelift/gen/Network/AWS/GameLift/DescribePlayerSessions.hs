{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribePlayerSessions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves properties for one or more player sessions. This operation can be used in several ways: (1) provide a @PlayerSessionId@ to request properties for a specific player session; (2) provide a @GameSessionId@ to request properties for all player sessions in the specified game session; (3) provide a @PlayerId@ to request properties for all player sessions of a specified player.
--
-- To get game session record(s), specify only one of the following: a player session ID, a game session ID, or a player ID. You can filter this request by player session status. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, a 'PlayerSession' object is returned for each session matching the request.
-- /Available in Amazon GameLift Local./
--
--     * 'CreatePlayerSession'
--
--
--     * 'CreatePlayerSessions'
--
--
--     * 'DescribePlayerSessions'
--
--
--     * Game session placements
--
--     * 'StartGameSessionPlacement'
--
--
--     * 'DescribeGameSessionPlacement'
--
--
--     * 'StopGameSessionPlacement'
--
--
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.DescribePlayerSessions
  ( -- * Creating a request
    DescribePlayerSessions (..),
    mkDescribePlayerSessions,

    -- ** Request lenses
    dpssGameSessionId,
    dpssNextToken,
    dpssLimit,
    dpssPlayerSessionId,
    dpssPlayerId,
    dpssPlayerSessionStatusFilter,

    -- * Destructuring the response
    DescribePlayerSessionsResponse (..),
    mkDescribePlayerSessionsResponse,

    -- ** Response lenses
    dpsrsNextToken,
    dpsrsPlayerSessions,
    dpsrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDescribePlayerSessions' smart constructor.
data DescribePlayerSessions = DescribePlayerSessions'
  { gameSessionId ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    playerSessionId :: Lude.Maybe Lude.Text,
    playerId :: Lude.Maybe Lude.Text,
    playerSessionStatusFilter ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePlayerSessions' with the minimum fields required to make a request.
--
-- * 'gameSessionId' - A unique identifier for the game session to retrieve player sessions for.
-- * 'limit' - The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. If a player session ID is specified, this parameter is ignored.
-- * 'nextToken' - Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value. If a player session ID is specified, this parameter is ignored.
-- * 'playerId' - A unique identifier for a player to retrieve player sessions for.
-- * 'playerSessionId' - A unique identifier for a player session to retrieve.
-- * 'playerSessionStatusFilter' - Player session status to filter results on.
--
-- Possible player session statuses include the following:
--
--     * __RESERVED__ -- The player session request has been received, but the player has not yet connected to the server process and/or been validated.
--
--
--     * __ACTIVE__ -- The player has been validated by the server process and is currently connected.
--
--
--     * __COMPLETED__ -- The player connection has been dropped.
--
--
--     * __TIMEDOUT__ -- A player session request was received, but the player did not connect and/or was not validated within the timeout limit (60 seconds).
mkDescribePlayerSessions ::
  DescribePlayerSessions
mkDescribePlayerSessions =
  DescribePlayerSessions'
    { gameSessionId = Lude.Nothing,
      nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      playerSessionId = Lude.Nothing,
      playerId = Lude.Nothing,
      playerSessionStatusFilter = Lude.Nothing
    }

-- | A unique identifier for the game session to retrieve player sessions for.
--
-- /Note:/ Consider using 'gameSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpssGameSessionId :: Lens.Lens' DescribePlayerSessions (Lude.Maybe Lude.Text)
dpssGameSessionId = Lens.lens (gameSessionId :: DescribePlayerSessions -> Lude.Maybe Lude.Text) (\s a -> s {gameSessionId = a} :: DescribePlayerSessions)
{-# DEPRECATED dpssGameSessionId "Use generic-lens or generic-optics with 'gameSessionId' instead." #-}

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value. If a player session ID is specified, this parameter is ignored.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpssNextToken :: Lens.Lens' DescribePlayerSessions (Lude.Maybe Lude.Text)
dpssNextToken = Lens.lens (nextToken :: DescribePlayerSessions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribePlayerSessions)
{-# DEPRECATED dpssNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. If a player session ID is specified, this parameter is ignored.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpssLimit :: Lens.Lens' DescribePlayerSessions (Lude.Maybe Lude.Natural)
dpssLimit = Lens.lens (limit :: DescribePlayerSessions -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribePlayerSessions)
{-# DEPRECATED dpssLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | A unique identifier for a player session to retrieve.
--
-- /Note:/ Consider using 'playerSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpssPlayerSessionId :: Lens.Lens' DescribePlayerSessions (Lude.Maybe Lude.Text)
dpssPlayerSessionId = Lens.lens (playerSessionId :: DescribePlayerSessions -> Lude.Maybe Lude.Text) (\s a -> s {playerSessionId = a} :: DescribePlayerSessions)
{-# DEPRECATED dpssPlayerSessionId "Use generic-lens or generic-optics with 'playerSessionId' instead." #-}

-- | A unique identifier for a player to retrieve player sessions for.
--
-- /Note:/ Consider using 'playerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpssPlayerId :: Lens.Lens' DescribePlayerSessions (Lude.Maybe Lude.Text)
dpssPlayerId = Lens.lens (playerId :: DescribePlayerSessions -> Lude.Maybe Lude.Text) (\s a -> s {playerId = a} :: DescribePlayerSessions)
{-# DEPRECATED dpssPlayerId "Use generic-lens or generic-optics with 'playerId' instead." #-}

-- | Player session status to filter results on.
--
-- Possible player session statuses include the following:
--
--     * __RESERVED__ -- The player session request has been received, but the player has not yet connected to the server process and/or been validated.
--
--
--     * __ACTIVE__ -- The player has been validated by the server process and is currently connected.
--
--
--     * __COMPLETED__ -- The player connection has been dropped.
--
--
--     * __TIMEDOUT__ -- A player session request was received, but the player did not connect and/or was not validated within the timeout limit (60 seconds).
--
--
--
-- /Note:/ Consider using 'playerSessionStatusFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpssPlayerSessionStatusFilter :: Lens.Lens' DescribePlayerSessions (Lude.Maybe Lude.Text)
dpssPlayerSessionStatusFilter = Lens.lens (playerSessionStatusFilter :: DescribePlayerSessions -> Lude.Maybe Lude.Text) (\s a -> s {playerSessionStatusFilter = a} :: DescribePlayerSessions)
{-# DEPRECATED dpssPlayerSessionStatusFilter "Use generic-lens or generic-optics with 'playerSessionStatusFilter' instead." #-}

instance Page.AWSPager DescribePlayerSessions where
  page rq rs
    | Page.stop (rs Lens.^. dpsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dpsrsPlayerSessions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dpssNextToken Lens..~ rs Lens.^. dpsrsNextToken

instance Lude.AWSRequest DescribePlayerSessions where
  type Rs DescribePlayerSessions = DescribePlayerSessionsResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribePlayerSessionsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "PlayerSessions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribePlayerSessions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DescribePlayerSessions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribePlayerSessions where
  toJSON DescribePlayerSessions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("GameSessionId" Lude..=) Lude.<$> gameSessionId,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            ("PlayerSessionId" Lude..=) Lude.<$> playerSessionId,
            ("PlayerId" Lude..=) Lude.<$> playerId,
            ("PlayerSessionStatusFilter" Lude..=)
              Lude.<$> playerSessionStatusFilter
          ]
      )

instance Lude.ToPath DescribePlayerSessions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribePlayerSessions where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribePlayerSessionsResponse' smart constructor.
data DescribePlayerSessionsResponse = DescribePlayerSessionsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    playerSessions ::
      Lude.Maybe [PlayerSession],
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

-- | Creates a value of 'DescribePlayerSessionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
-- * 'playerSessions' - A collection of objects containing properties for each player session that matches the request.
-- * 'responseStatus' - The response status code.
mkDescribePlayerSessionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribePlayerSessionsResponse
mkDescribePlayerSessionsResponse pResponseStatus_ =
  DescribePlayerSessionsResponse'
    { nextToken = Lude.Nothing,
      playerSessions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsrsNextToken :: Lens.Lens' DescribePlayerSessionsResponse (Lude.Maybe Lude.Text)
dpsrsNextToken = Lens.lens (nextToken :: DescribePlayerSessionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribePlayerSessionsResponse)
{-# DEPRECATED dpsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A collection of objects containing properties for each player session that matches the request.
--
-- /Note:/ Consider using 'playerSessions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsrsPlayerSessions :: Lens.Lens' DescribePlayerSessionsResponse (Lude.Maybe [PlayerSession])
dpsrsPlayerSessions = Lens.lens (playerSessions :: DescribePlayerSessionsResponse -> Lude.Maybe [PlayerSession]) (\s a -> s {playerSessions = a} :: DescribePlayerSessionsResponse)
{-# DEPRECATED dpsrsPlayerSessions "Use generic-lens or generic-optics with 'playerSessions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsrsResponseStatus :: Lens.Lens' DescribePlayerSessionsResponse Lude.Int
dpsrsResponseStatus = Lens.lens (responseStatus :: DescribePlayerSessionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribePlayerSessionsResponse)
{-# DEPRECATED dpsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
