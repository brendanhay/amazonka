{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.ListExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the executions of a state machine that meet the filtering criteria. Results are sorted by time, with the most recent execution first.
--
-- If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
-- This API action is not supported by @EXPRESS@ state machines.
--
-- This operation returns paginated results.
module Network.AWS.StepFunctions.ListExecutions
  ( -- * Creating a request
    ListExecutions (..),
    mkListExecutions,

    -- ** Request lenses
    leStatusFilter,
    leNextToken,
    leStateMachineARN,
    leMaxResults,

    -- * Destructuring the response
    ListExecutionsResponse (..),
    mkListExecutionsResponse,

    -- ** Response lenses
    lersExecutions,
    lersNextToken,
    lersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StepFunctions.Types

-- | /See:/ 'mkListExecutions' smart constructor.
data ListExecutions = ListExecutions'
  { -- | If specified, only list the executions whose current execution status matches the given filter.
    statusFilter :: Lude.Maybe ExecutionStatus,
    -- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the state machine whose executions is listed.
    stateMachineARN :: Lude.Text,
    -- | The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 1000. A value of 0 uses the default.
    --
    -- This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListExecutions' with the minimum fields required to make a request.
--
-- * 'statusFilter' - If specified, only list the executions whose current execution status matches the given filter.
-- * 'nextToken' - If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
-- * 'stateMachineARN' - The Amazon Resource Name (ARN) of the state machine whose executions is listed.
-- * 'maxResults' - The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
mkListExecutions ::
  -- | 'stateMachineARN'
  Lude.Text ->
  ListExecutions
mkListExecutions pStateMachineARN_ =
  ListExecutions'
    { statusFilter = Lude.Nothing,
      nextToken = Lude.Nothing,
      stateMachineARN = pStateMachineARN_,
      maxResults = Lude.Nothing
    }

-- | If specified, only list the executions whose current execution status matches the given filter.
--
-- /Note:/ Consider using 'statusFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leStatusFilter :: Lens.Lens' ListExecutions (Lude.Maybe ExecutionStatus)
leStatusFilter = Lens.lens (statusFilter :: ListExecutions -> Lude.Maybe ExecutionStatus) (\s a -> s {statusFilter = a} :: ListExecutions)
{-# DEPRECATED leStatusFilter "Use generic-lens or generic-optics with 'statusFilter' instead." #-}

-- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leNextToken :: Lens.Lens' ListExecutions (Lude.Maybe Lude.Text)
leNextToken = Lens.lens (nextToken :: ListExecutions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListExecutions)
{-# DEPRECATED leNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The Amazon Resource Name (ARN) of the state machine whose executions is listed.
--
-- /Note:/ Consider using 'stateMachineARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leStateMachineARN :: Lens.Lens' ListExecutions Lude.Text
leStateMachineARN = Lens.lens (stateMachineARN :: ListExecutions -> Lude.Text) (\s a -> s {stateMachineARN = a} :: ListExecutions)
{-# DEPRECATED leStateMachineARN "Use generic-lens or generic-optics with 'stateMachineARN' instead." #-}

-- | The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leMaxResults :: Lens.Lens' ListExecutions (Lude.Maybe Lude.Natural)
leMaxResults = Lens.lens (maxResults :: ListExecutions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListExecutions)
{-# DEPRECATED leMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListExecutions where
  page rq rs
    | Page.stop (rs Lens.^. lersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lersExecutions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& leNextToken Lens..~ rs Lens.^. lersNextToken

instance Lude.AWSRequest ListExecutions where
  type Rs ListExecutions = ListExecutionsResponse
  request = Req.postJSON stepFunctionsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListExecutionsResponse'
            Lude.<$> (x Lude..?> "executions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListExecutions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSStepFunctions.ListExecutions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListExecutions where
  toJSON ListExecutions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("statusFilter" Lude..=) Lude.<$> statusFilter,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("stateMachineArn" Lude..= stateMachineARN),
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListExecutions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListExecutions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListExecutionsResponse' smart constructor.
data ListExecutionsResponse = ListExecutionsResponse'
  { -- | The list of matching executions.
    executions :: [ExecutionListItem],
    -- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListExecutionsResponse' with the minimum fields required to make a request.
--
-- * 'executions' - The list of matching executions.
-- * 'nextToken' - If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
-- * 'responseStatus' - The response status code.
mkListExecutionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListExecutionsResponse
mkListExecutionsResponse pResponseStatus_ =
  ListExecutionsResponse'
    { executions = Lude.mempty,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of matching executions.
--
-- /Note:/ Consider using 'executions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lersExecutions :: Lens.Lens' ListExecutionsResponse [ExecutionListItem]
lersExecutions = Lens.lens (executions :: ListExecutionsResponse -> [ExecutionListItem]) (\s a -> s {executions = a} :: ListExecutionsResponse)
{-# DEPRECATED lersExecutions "Use generic-lens or generic-optics with 'executions' instead." #-}

-- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lersNextToken :: Lens.Lens' ListExecutionsResponse (Lude.Maybe Lude.Text)
lersNextToken = Lens.lens (nextToken :: ListExecutionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListExecutionsResponse)
{-# DEPRECATED lersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lersResponseStatus :: Lens.Lens' ListExecutionsResponse Lude.Int
lersResponseStatus = Lens.lens (responseStatus :: ListExecutionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListExecutionsResponse)
{-# DEPRECATED lersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
