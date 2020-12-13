{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeGameSessionQueues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the properties for one or more game session queues. When requesting multiple queues, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a 'GameSessionQueue' object is returned for each requested queue. When specifying a list of queues, objects are returned only for queues that currently exist in the Region.
--
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queues-console.html View Your Queues>
-- __Related operations__
--
--     * 'CreateGameSessionQueue'
--
--
--     * 'DescribeGameSessionQueues'
--
--
--     * 'UpdateGameSessionQueue'
--
--
--     * 'DeleteGameSessionQueue'
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.DescribeGameSessionQueues
  ( -- * Creating a request
    DescribeGameSessionQueues (..),
    mkDescribeGameSessionQueues,

    -- ** Request lenses
    dgsqNextToken,
    dgsqNames,
    dgsqLimit,

    -- * Destructuring the response
    DescribeGameSessionQueuesResponse (..),
    mkDescribeGameSessionQueuesResponse,

    -- ** Response lenses
    dgsqrsNextToken,
    dgsqrsGameSessionQueues,
    dgsqrsResponseStatus,
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
-- /See:/ 'mkDescribeGameSessionQueues' smart constructor.
data DescribeGameSessionQueues = DescribeGameSessionQueues'
  { -- | A token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of queue names to retrieve information for. You can use either the queue ID or ARN value. To request settings for all queues, leave this parameter empty.
    names :: Lude.Maybe [Lude.Text],
    -- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. You can request up to 50 results.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeGameSessionQueues' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
-- * 'names' - A list of queue names to retrieve information for. You can use either the queue ID or ARN value. To request settings for all queues, leave this parameter empty.
-- * 'limit' - The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. You can request up to 50 results.
mkDescribeGameSessionQueues ::
  DescribeGameSessionQueues
mkDescribeGameSessionQueues =
  DescribeGameSessionQueues'
    { nextToken = Lude.Nothing,
      names = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | A token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsqNextToken :: Lens.Lens' DescribeGameSessionQueues (Lude.Maybe Lude.Text)
dgsqNextToken = Lens.lens (nextToken :: DescribeGameSessionQueues -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeGameSessionQueues)
{-# DEPRECATED dgsqNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of queue names to retrieve information for. You can use either the queue ID or ARN value. To request settings for all queues, leave this parameter empty.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsqNames :: Lens.Lens' DescribeGameSessionQueues (Lude.Maybe [Lude.Text])
dgsqNames = Lens.lens (names :: DescribeGameSessionQueues -> Lude.Maybe [Lude.Text]) (\s a -> s {names = a} :: DescribeGameSessionQueues)
{-# DEPRECATED dgsqNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. You can request up to 50 results.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsqLimit :: Lens.Lens' DescribeGameSessionQueues (Lude.Maybe Lude.Natural)
dgsqLimit = Lens.lens (limit :: DescribeGameSessionQueues -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeGameSessionQueues)
{-# DEPRECATED dgsqLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager DescribeGameSessionQueues where
  page rq rs
    | Page.stop (rs Lens.^. dgsqrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dgsqrsGameSessionQueues) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dgsqNextToken Lens..~ rs Lens.^. dgsqrsNextToken

instance Lude.AWSRequest DescribeGameSessionQueues where
  type
    Rs DescribeGameSessionQueues =
      DescribeGameSessionQueuesResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeGameSessionQueuesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "GameSessionQueues" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeGameSessionQueues where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DescribeGameSessionQueues" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeGameSessionQueues where
  toJSON DescribeGameSessionQueues' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Names" Lude..=) Lude.<$> names,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath DescribeGameSessionQueues where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeGameSessionQueues where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeGameSessionQueuesResponse' smart constructor.
data DescribeGameSessionQueuesResponse = DescribeGameSessionQueuesResponse'
  { -- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A collection of objects that describe the requested game session queues.
    gameSessionQueues :: Lude.Maybe [GameSessionQueue],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeGameSessionQueuesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
-- * 'gameSessionQueues' - A collection of objects that describe the requested game session queues.
-- * 'responseStatus' - The response status code.
mkDescribeGameSessionQueuesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeGameSessionQueuesResponse
mkDescribeGameSessionQueuesResponse pResponseStatus_ =
  DescribeGameSessionQueuesResponse'
    { nextToken = Lude.Nothing,
      gameSessionQueues = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsqrsNextToken :: Lens.Lens' DescribeGameSessionQueuesResponse (Lude.Maybe Lude.Text)
dgsqrsNextToken = Lens.lens (nextToken :: DescribeGameSessionQueuesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeGameSessionQueuesResponse)
{-# DEPRECATED dgsqrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A collection of objects that describe the requested game session queues.
--
-- /Note:/ Consider using 'gameSessionQueues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsqrsGameSessionQueues :: Lens.Lens' DescribeGameSessionQueuesResponse (Lude.Maybe [GameSessionQueue])
dgsqrsGameSessionQueues = Lens.lens (gameSessionQueues :: DescribeGameSessionQueuesResponse -> Lude.Maybe [GameSessionQueue]) (\s a -> s {gameSessionQueues = a} :: DescribeGameSessionQueuesResponse)
{-# DEPRECATED dgsqrsGameSessionQueues "Use generic-lens or generic-optics with 'gameSessionQueues' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsqrsResponseStatus :: Lens.Lens' DescribeGameSessionQueuesResponse Lude.Int
dgsqrsResponseStatus = Lens.lens (responseStatus :: DescribeGameSessionQueuesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeGameSessionQueuesResponse)
{-# DEPRECATED dgsqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
