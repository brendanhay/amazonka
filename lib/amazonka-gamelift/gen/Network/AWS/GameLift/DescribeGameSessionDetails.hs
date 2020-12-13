{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeGameSessionDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves properties, including the protection policy in force, for one or more game sessions. This operation can be used in several ways: (1) provide a @GameSessionId@ or @GameSessionArn@ to request details for a specific game session; (2) provide either a @FleetId@ or an @AliasId@ to request properties for all game sessions running on a fleet.
--
-- To get game session record(s), specify just one of the following: game session ID, fleet ID, or alias ID. You can filter this request by game session status. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, a 'GameSessionDetail' object is returned for each session matching the request.
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
module Network.AWS.GameLift.DescribeGameSessionDetails
  ( -- * Creating a request
    DescribeGameSessionDetails (..),
    mkDescribeGameSessionDetails,

    -- ** Request lenses
    dgsdGameSessionId,
    dgsdAliasId,
    dgsdNextToken,
    dgsdStatusFilter,
    dgsdLimit,
    dgsdFleetId,

    -- * Destructuring the response
    DescribeGameSessionDetailsResponse (..),
    mkDescribeGameSessionDetailsResponse,

    -- ** Response lenses
    dgsdrsGameSessionDetails,
    dgsdrsNextToken,
    dgsdrsResponseStatus,
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
-- /See:/ 'mkDescribeGameSessionDetails' smart constructor.
data DescribeGameSessionDetails = DescribeGameSessionDetails'
  { -- | A unique identifier for the game session to retrieve.
    gameSessionId :: Lude.Maybe Lude.Text,
    -- | A unique identifier for an alias associated with the fleet to retrieve all game sessions for. You can use either the alias ID or ARN value.
    aliasId :: Lude.Maybe Lude.Text,
    -- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Game session status to filter results on. Possible game session statuses include @ACTIVE@ , @TERMINATED@ , @ACTIVATING@ and @TERMINATING@ (the last two are transitory).
    statusFilter :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
    limit :: Lude.Maybe Lude.Natural,
    -- | A unique identifier for a fleet to retrieve all game sessions active on the fleet. You can use either the fleet ID or ARN value.
    fleetId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeGameSessionDetails' with the minimum fields required to make a request.
--
-- * 'gameSessionId' - A unique identifier for the game session to retrieve.
-- * 'aliasId' - A unique identifier for an alias associated with the fleet to retrieve all game sessions for. You can use either the alias ID or ARN value.
-- * 'nextToken' - Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
-- * 'statusFilter' - Game session status to filter results on. Possible game session statuses include @ACTIVE@ , @TERMINATED@ , @ACTIVATING@ and @TERMINATING@ (the last two are transitory).
-- * 'limit' - The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
-- * 'fleetId' - A unique identifier for a fleet to retrieve all game sessions active on the fleet. You can use either the fleet ID or ARN value.
mkDescribeGameSessionDetails ::
  DescribeGameSessionDetails
mkDescribeGameSessionDetails =
  DescribeGameSessionDetails'
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
dgsdGameSessionId :: Lens.Lens' DescribeGameSessionDetails (Lude.Maybe Lude.Text)
dgsdGameSessionId = Lens.lens (gameSessionId :: DescribeGameSessionDetails -> Lude.Maybe Lude.Text) (\s a -> s {gameSessionId = a} :: DescribeGameSessionDetails)
{-# DEPRECATED dgsdGameSessionId "Use generic-lens or generic-optics with 'gameSessionId' instead." #-}

-- | A unique identifier for an alias associated with the fleet to retrieve all game sessions for. You can use either the alias ID or ARN value.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsdAliasId :: Lens.Lens' DescribeGameSessionDetails (Lude.Maybe Lude.Text)
dgsdAliasId = Lens.lens (aliasId :: DescribeGameSessionDetails -> Lude.Maybe Lude.Text) (\s a -> s {aliasId = a} :: DescribeGameSessionDetails)
{-# DEPRECATED dgsdAliasId "Use generic-lens or generic-optics with 'aliasId' instead." #-}

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsdNextToken :: Lens.Lens' DescribeGameSessionDetails (Lude.Maybe Lude.Text)
dgsdNextToken = Lens.lens (nextToken :: DescribeGameSessionDetails -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeGameSessionDetails)
{-# DEPRECATED dgsdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Game session status to filter results on. Possible game session statuses include @ACTIVE@ , @TERMINATED@ , @ACTIVATING@ and @TERMINATING@ (the last two are transitory).
--
-- /Note:/ Consider using 'statusFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsdStatusFilter :: Lens.Lens' DescribeGameSessionDetails (Lude.Maybe Lude.Text)
dgsdStatusFilter = Lens.lens (statusFilter :: DescribeGameSessionDetails -> Lude.Maybe Lude.Text) (\s a -> s {statusFilter = a} :: DescribeGameSessionDetails)
{-# DEPRECATED dgsdStatusFilter "Use generic-lens or generic-optics with 'statusFilter' instead." #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsdLimit :: Lens.Lens' DescribeGameSessionDetails (Lude.Maybe Lude.Natural)
dgsdLimit = Lens.lens (limit :: DescribeGameSessionDetails -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeGameSessionDetails)
{-# DEPRECATED dgsdLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | A unique identifier for a fleet to retrieve all game sessions active on the fleet. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsdFleetId :: Lens.Lens' DescribeGameSessionDetails (Lude.Maybe Lude.Text)
dgsdFleetId = Lens.lens (fleetId :: DescribeGameSessionDetails -> Lude.Maybe Lude.Text) (\s a -> s {fleetId = a} :: DescribeGameSessionDetails)
{-# DEPRECATED dgsdFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

instance Page.AWSPager DescribeGameSessionDetails where
  page rq rs
    | Page.stop (rs Lens.^. dgsdrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dgsdrsGameSessionDetails) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dgsdNextToken Lens..~ rs Lens.^. dgsdrsNextToken

instance Lude.AWSRequest DescribeGameSessionDetails where
  type
    Rs DescribeGameSessionDetails =
      DescribeGameSessionDetailsResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeGameSessionDetailsResponse'
            Lude.<$> (x Lude..?> "GameSessionDetails" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeGameSessionDetails where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DescribeGameSessionDetails" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeGameSessionDetails where
  toJSON DescribeGameSessionDetails' {..} =
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

instance Lude.ToPath DescribeGameSessionDetails where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeGameSessionDetails where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeGameSessionDetailsResponse' smart constructor.
data DescribeGameSessionDetailsResponse = DescribeGameSessionDetailsResponse'
  { -- | A collection of objects containing game session properties and the protection policy currently in force for each session matching the request.
    gameSessionDetails :: Lude.Maybe [GameSessionDetail],
    -- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeGameSessionDetailsResponse' with the minimum fields required to make a request.
--
-- * 'gameSessionDetails' - A collection of objects containing game session properties and the protection policy currently in force for each session matching the request.
-- * 'nextToken' - Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
-- * 'responseStatus' - The response status code.
mkDescribeGameSessionDetailsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeGameSessionDetailsResponse
mkDescribeGameSessionDetailsResponse pResponseStatus_ =
  DescribeGameSessionDetailsResponse'
    { gameSessionDetails =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A collection of objects containing game session properties and the protection policy currently in force for each session matching the request.
--
-- /Note:/ Consider using 'gameSessionDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsdrsGameSessionDetails :: Lens.Lens' DescribeGameSessionDetailsResponse (Lude.Maybe [GameSessionDetail])
dgsdrsGameSessionDetails = Lens.lens (gameSessionDetails :: DescribeGameSessionDetailsResponse -> Lude.Maybe [GameSessionDetail]) (\s a -> s {gameSessionDetails = a} :: DescribeGameSessionDetailsResponse)
{-# DEPRECATED dgsdrsGameSessionDetails "Use generic-lens or generic-optics with 'gameSessionDetails' instead." #-}

-- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsdrsNextToken :: Lens.Lens' DescribeGameSessionDetailsResponse (Lude.Maybe Lude.Text)
dgsdrsNextToken = Lens.lens (nextToken :: DescribeGameSessionDetailsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeGameSessionDetailsResponse)
{-# DEPRECATED dgsdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsdrsResponseStatus :: Lens.Lens' DescribeGameSessionDetailsResponse Lude.Int
dgsdrsResponseStatus = Lens.lens (responseStatus :: DescribeGameSessionDetailsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeGameSessionDetailsResponse)
{-# DEPRECATED dgsdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
