{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.GetExecutionHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the history of the specified execution as a list of events. By default, the results are returned in ascending order of the @timeStamp@ of the events. Use the @reverseOrder@ parameter to get the latest events first.
--
-- If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
-- This API action is not supported by @EXPRESS@ state machines.
--
-- This operation returns paginated results.
module Network.AWS.StepFunctions.GetExecutionHistory
  ( -- * Creating a request
    GetExecutionHistory (..),
    mkGetExecutionHistory,

    -- ** Request lenses
    gehReverseOrder,
    gehIncludeExecutionData,
    gehNextToken,
    gehExecutionARN,
    gehMaxResults,

    -- * Destructuring the response
    GetExecutionHistoryResponse (..),
    mkGetExecutionHistoryResponse,

    -- ** Response lenses
    gehrsNextToken,
    gehrsEvents,
    gehrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StepFunctions.Types

-- | /See:/ 'mkGetExecutionHistory' smart constructor.
data GetExecutionHistory = GetExecutionHistory'
  { -- | Lists events in descending order of their @timeStamp@ .
    reverseOrder :: Lude.Maybe Lude.Bool,
    -- | You can select whether execution data (input or output of a history event) is returned. The default is @true@ .
    includeExecutionData :: Lude.Maybe Lude.Bool,
    -- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the execution.
    executionARN :: Lude.Text,
    -- | The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 1000. A value of 0 uses the default.
    --
    -- This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetExecutionHistory' with the minimum fields required to make a request.
--
-- * 'reverseOrder' - Lists events in descending order of their @timeStamp@ .
-- * 'includeExecutionData' - You can select whether execution data (input or output of a history event) is returned. The default is @true@ .
-- * 'nextToken' - If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
-- * 'executionARN' - The Amazon Resource Name (ARN) of the execution.
-- * 'maxResults' - The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
mkGetExecutionHistory ::
  -- | 'executionARN'
  Lude.Text ->
  GetExecutionHistory
mkGetExecutionHistory pExecutionARN_ =
  GetExecutionHistory'
    { reverseOrder = Lude.Nothing,
      includeExecutionData = Lude.Nothing,
      nextToken = Lude.Nothing,
      executionARN = pExecutionARN_,
      maxResults = Lude.Nothing
    }

-- | Lists events in descending order of their @timeStamp@ .
--
-- /Note:/ Consider using 'reverseOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gehReverseOrder :: Lens.Lens' GetExecutionHistory (Lude.Maybe Lude.Bool)
gehReverseOrder = Lens.lens (reverseOrder :: GetExecutionHistory -> Lude.Maybe Lude.Bool) (\s a -> s {reverseOrder = a} :: GetExecutionHistory)
{-# DEPRECATED gehReverseOrder "Use generic-lens or generic-optics with 'reverseOrder' instead." #-}

-- | You can select whether execution data (input or output of a history event) is returned. The default is @true@ .
--
-- /Note:/ Consider using 'includeExecutionData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gehIncludeExecutionData :: Lens.Lens' GetExecutionHistory (Lude.Maybe Lude.Bool)
gehIncludeExecutionData = Lens.lens (includeExecutionData :: GetExecutionHistory -> Lude.Maybe Lude.Bool) (\s a -> s {includeExecutionData = a} :: GetExecutionHistory)
{-# DEPRECATED gehIncludeExecutionData "Use generic-lens or generic-optics with 'includeExecutionData' instead." #-}

-- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gehNextToken :: Lens.Lens' GetExecutionHistory (Lude.Maybe Lude.Text)
gehNextToken = Lens.lens (nextToken :: GetExecutionHistory -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetExecutionHistory)
{-# DEPRECATED gehNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The Amazon Resource Name (ARN) of the execution.
--
-- /Note:/ Consider using 'executionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gehExecutionARN :: Lens.Lens' GetExecutionHistory Lude.Text
gehExecutionARN = Lens.lens (executionARN :: GetExecutionHistory -> Lude.Text) (\s a -> s {executionARN = a} :: GetExecutionHistory)
{-# DEPRECATED gehExecutionARN "Use generic-lens or generic-optics with 'executionARN' instead." #-}

-- | The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gehMaxResults :: Lens.Lens' GetExecutionHistory (Lude.Maybe Lude.Natural)
gehMaxResults = Lens.lens (maxResults :: GetExecutionHistory -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetExecutionHistory)
{-# DEPRECATED gehMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetExecutionHistory where
  page rq rs
    | Page.stop (rs Lens.^. gehrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gehrsEvents) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gehNextToken Lens..~ rs Lens.^. gehrsNextToken

instance Lude.AWSRequest GetExecutionHistory where
  type Rs GetExecutionHistory = GetExecutionHistoryResponse
  request = Req.postJSON stepFunctionsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetExecutionHistoryResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "events" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetExecutionHistory where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSStepFunctions.GetExecutionHistory" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetExecutionHistory where
  toJSON GetExecutionHistory' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("reverseOrder" Lude..=) Lude.<$> reverseOrder,
            ("includeExecutionData" Lude..=) Lude.<$> includeExecutionData,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("executionArn" Lude..= executionARN),
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetExecutionHistory where
  toPath = Lude.const "/"

instance Lude.ToQuery GetExecutionHistory where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetExecutionHistoryResponse' smart constructor.
data GetExecutionHistoryResponse = GetExecutionHistoryResponse'
  { -- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The list of events that occurred in the execution.
    events :: [HistoryEvent],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetExecutionHistoryResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
-- * 'events' - The list of events that occurred in the execution.
-- * 'responseStatus' - The response status code.
mkGetExecutionHistoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetExecutionHistoryResponse
mkGetExecutionHistoryResponse pResponseStatus_ =
  GetExecutionHistoryResponse'
    { nextToken = Lude.Nothing,
      events = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gehrsNextToken :: Lens.Lens' GetExecutionHistoryResponse (Lude.Maybe Lude.Text)
gehrsNextToken = Lens.lens (nextToken :: GetExecutionHistoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetExecutionHistoryResponse)
{-# DEPRECATED gehrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of events that occurred in the execution.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gehrsEvents :: Lens.Lens' GetExecutionHistoryResponse [HistoryEvent]
gehrsEvents = Lens.lens (events :: GetExecutionHistoryResponse -> [HistoryEvent]) (\s a -> s {events = a} :: GetExecutionHistoryResponse)
{-# DEPRECATED gehrsEvents "Use generic-lens or generic-optics with 'events' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gehrsResponseStatus :: Lens.Lens' GetExecutionHistoryResponse Lude.Int
gehrsResponseStatus = Lens.lens (responseStatus :: GetExecutionHistoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetExecutionHistoryResponse)
{-# DEPRECATED gehrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
