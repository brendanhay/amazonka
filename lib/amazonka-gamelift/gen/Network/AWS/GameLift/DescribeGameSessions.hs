{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeGameSessions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a set of one or more game sessions. Request a specific game session or request all game sessions on a fleet. Alternatively, use 'SearchGameSessions' to request a set of active game sessions that are filtered by certain criteria. To retrieve protection policy settings for game sessions, use 'DescribeGameSessionDetails' .
--
-- To get game sessions, specify one of the following: game session ID, fleet ID, or alias ID. You can filter this request by game session status. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, a 'GameSession' object is returned for each game session matching the request.
-- /Available in Amazon GameLift Local./
--
--     * 'CreateGameSession'
--
--
--     * 'DescribeGameSessions'
--
--
--     * 'DescribeGameSessionDetails'
--
--
--     * 'SearchGameSessions'
--
--
--     * 'UpdateGameSession'
--
--
--     * 'GetGameSessionLogUrl'
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
module Network.AWS.GameLift.DescribeGameSessions
  ( -- * Creating a request
    DescribeGameSessions (..),
    mkDescribeGameSessions,

    -- ** Request lenses
    dgsGameSessionId,
    dgsAliasId,
    dgsNextToken,
    dgsStatusFilter,
    dgsLimit,
    dgsFleetId,

    -- * Destructuring the response
    DescribeGameSessionsResponse (..),
    mkDescribeGameSessionsResponse,

    -- ** Response lenses
    dgssrsGameSessions,
    dgssrsNextToken,
    dgssrsResponseStatus,
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
-- /See:/ 'mkDescribeGameSessions' smart constructor.
data DescribeGameSessions = DescribeGameSessions'
  { -- | A unique identifier for the game session to retrieve.
    gameSessionId :: Lude.Maybe Lude.Text,
    -- | A unique identifier for an alias associated with the fleet to retrieve all game sessions for. You can use either the alias ID or ARN value.
    aliasId :: Lude.Maybe Lude.Text,
    -- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Game session status to filter results on. Possible game session statuses include @ACTIVE@ , @TERMINATED@ , @ACTIVATING@ , and @TERMINATING@ (the last two are transitory).
    statusFilter :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
    limit :: Lude.Maybe Lude.Natural,
    -- | A unique identifier for a fleet to retrieve all game sessions for. You can use either the fleet ID or ARN value.
    fleetId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeGameSessions' with the minimum fields required to make a request.
--
-- * 'gameSessionId' - A unique identifier for the game session to retrieve.
-- * 'aliasId' - A unique identifier for an alias associated with the fleet to retrieve all game sessions for. You can use either the alias ID or ARN value.
-- * 'nextToken' - Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
-- * 'statusFilter' - Game session status to filter results on. Possible game session statuses include @ACTIVE@ , @TERMINATED@ , @ACTIVATING@ , and @TERMINATING@ (the last two are transitory).
-- * 'limit' - The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
-- * 'fleetId' - A unique identifier for a fleet to retrieve all game sessions for. You can use either the fleet ID or ARN value.
mkDescribeGameSessions ::
  DescribeGameSessions
mkDescribeGameSessions =
  DescribeGameSessions'
    { gameSessionId = Lude.Nothing,
      aliasId = Lude.Nothing,
      nextToken = Lude.Nothing,
      statusFilter = Lude.Nothing,
      limit = Lude.Nothing,
      fleetId = Lude.Nothing
    }

-- | A unique identifier for the game session to retrieve.
--
-- /Note:/ Consider using 'gameSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsGameSessionId :: Lens.Lens' DescribeGameSessions (Lude.Maybe Lude.Text)
dgsGameSessionId = Lens.lens (gameSessionId :: DescribeGameSessions -> Lude.Maybe Lude.Text) (\s a -> s {gameSessionId = a} :: DescribeGameSessions)
{-# DEPRECATED dgsGameSessionId "Use generic-lens or generic-optics with 'gameSessionId' instead." #-}

-- | A unique identifier for an alias associated with the fleet to retrieve all game sessions for. You can use either the alias ID or ARN value.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsAliasId :: Lens.Lens' DescribeGameSessions (Lude.Maybe Lude.Text)
dgsAliasId = Lens.lens (aliasId :: DescribeGameSessions -> Lude.Maybe Lude.Text) (\s a -> s {aliasId = a} :: DescribeGameSessions)
{-# DEPRECATED dgsAliasId "Use generic-lens or generic-optics with 'aliasId' instead." #-}

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsNextToken :: Lens.Lens' DescribeGameSessions (Lude.Maybe Lude.Text)
dgsNextToken = Lens.lens (nextToken :: DescribeGameSessions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeGameSessions)
{-# DEPRECATED dgsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Game session status to filter results on. Possible game session statuses include @ACTIVE@ , @TERMINATED@ , @ACTIVATING@ , and @TERMINATING@ (the last two are transitory).
--
-- /Note:/ Consider using 'statusFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsStatusFilter :: Lens.Lens' DescribeGameSessions (Lude.Maybe Lude.Text)
dgsStatusFilter = Lens.lens (statusFilter :: DescribeGameSessions -> Lude.Maybe Lude.Text) (\s a -> s {statusFilter = a} :: DescribeGameSessions)
{-# DEPRECATED dgsStatusFilter "Use generic-lens or generic-optics with 'statusFilter' instead." #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsLimit :: Lens.Lens' DescribeGameSessions (Lude.Maybe Lude.Natural)
dgsLimit = Lens.lens (limit :: DescribeGameSessions -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeGameSessions)
{-# DEPRECATED dgsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | A unique identifier for a fleet to retrieve all game sessions for. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsFleetId :: Lens.Lens' DescribeGameSessions (Lude.Maybe Lude.Text)
dgsFleetId = Lens.lens (fleetId :: DescribeGameSessions -> Lude.Maybe Lude.Text) (\s a -> s {fleetId = a} :: DescribeGameSessions)
{-# DEPRECATED dgsFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

instance Page.AWSPager DescribeGameSessions where
  page rq rs
    | Page.stop (rs Lens.^. dgssrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dgssrsGameSessions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dgsNextToken Lens..~ rs Lens.^. dgssrsNextToken

instance Lude.AWSRequest DescribeGameSessions where
  type Rs DescribeGameSessions = DescribeGameSessionsResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeGameSessionsResponse'
            Lude.<$> (x Lude..?> "GameSessions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeGameSessions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DescribeGameSessions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeGameSessions where
  toJSON DescribeGameSessions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("GameSessionId" Lude..=) Lude.<$> gameSessionId,
            ("AliasId" Lude..=) Lude.<$> aliasId,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("StatusFilter" Lude..=) Lude.<$> statusFilter,
            ("Limit" Lude..=) Lude.<$> limit,
            ("FleetId" Lude..=) Lude.<$> fleetId
          ]
      )

instance Lude.ToPath DescribeGameSessions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeGameSessions where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeGameSessionsResponse' smart constructor.
data DescribeGameSessionsResponse = DescribeGameSessionsResponse'
  { -- | A collection of objects containing game session properties for each session matching the request.
    gameSessions :: Lude.Maybe [GameSession],
    -- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeGameSessionsResponse' with the minimum fields required to make a request.
--
-- * 'gameSessions' - A collection of objects containing game session properties for each session matching the request.
-- * 'nextToken' - Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
-- * 'responseStatus' - The response status code.
mkDescribeGameSessionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeGameSessionsResponse
mkDescribeGameSessionsResponse pResponseStatus_ =
  DescribeGameSessionsResponse'
    { gameSessions = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A collection of objects containing game session properties for each session matching the request.
--
-- /Note:/ Consider using 'gameSessions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgssrsGameSessions :: Lens.Lens' DescribeGameSessionsResponse (Lude.Maybe [GameSession])
dgssrsGameSessions = Lens.lens (gameSessions :: DescribeGameSessionsResponse -> Lude.Maybe [GameSession]) (\s a -> s {gameSessions = a} :: DescribeGameSessionsResponse)
{-# DEPRECATED dgssrsGameSessions "Use generic-lens or generic-optics with 'gameSessions' instead." #-}

-- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgssrsNextToken :: Lens.Lens' DescribeGameSessionsResponse (Lude.Maybe Lude.Text)
dgssrsNextToken = Lens.lens (nextToken :: DescribeGameSessionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeGameSessionsResponse)
{-# DEPRECATED dgssrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgssrsResponseStatus :: Lens.Lens' DescribeGameSessionsResponse Lude.Int
dgssrsResponseStatus = Lens.lens (responseStatus :: DescribeGameSessionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeGameSessionsResponse)
{-# DEPRECATED dgssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
