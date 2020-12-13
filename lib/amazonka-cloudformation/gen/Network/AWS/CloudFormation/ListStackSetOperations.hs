{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ListStackSetOperations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about operations performed on a stack set.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.ListStackSetOperations
  ( -- * Creating a request
    ListStackSetOperations (..),
    mkListStackSetOperations,

    -- ** Request lenses
    lssoNextToken,
    lssoStackSetName,
    lssoMaxResults,

    -- * Destructuring the response
    ListStackSetOperationsResponse (..),
    mkListStackSetOperationsResponse,

    -- ** Response lenses
    lssorsNextToken,
    lssorsSummaries,
    lssorsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListStackSetOperations' smart constructor.
data ListStackSetOperations = ListStackSetOperations'
  { -- | If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call @ListStackSetOperations@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | The name or unique ID of the stack set that you want to get operation summaries for.
    stackSetName :: Lude.Text,
    -- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListStackSetOperations' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call @ListStackSetOperations@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
-- * 'stackSetName' - The name or unique ID of the stack set that you want to get operation summaries for.
-- * 'maxResults' - The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
mkListStackSetOperations ::
  -- | 'stackSetName'
  Lude.Text ->
  ListStackSetOperations
mkListStackSetOperations pStackSetName_ =
  ListStackSetOperations'
    { nextToken = Lude.Nothing,
      stackSetName = pStackSetName_,
      maxResults = Lude.Nothing
    }

-- | If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call @ListStackSetOperations@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssoNextToken :: Lens.Lens' ListStackSetOperations (Lude.Maybe Lude.Text)
lssoNextToken = Lens.lens (nextToken :: ListStackSetOperations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListStackSetOperations)
{-# DEPRECATED lssoNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name or unique ID of the stack set that you want to get operation summaries for.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssoStackSetName :: Lens.Lens' ListStackSetOperations Lude.Text
lssoStackSetName = Lens.lens (stackSetName :: ListStackSetOperations -> Lude.Text) (\s a -> s {stackSetName = a} :: ListStackSetOperations)
{-# DEPRECATED lssoStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

-- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssoMaxResults :: Lens.Lens' ListStackSetOperations (Lude.Maybe Lude.Natural)
lssoMaxResults = Lens.lens (maxResults :: ListStackSetOperations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListStackSetOperations)
{-# DEPRECATED lssoMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListStackSetOperations where
  page rq rs
    | Page.stop (rs Lens.^. lssorsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lssorsSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lssoNextToken Lens..~ rs Lens.^. lssorsNextToken

instance Lude.AWSRequest ListStackSetOperations where
  type Rs ListStackSetOperations = ListStackSetOperationsResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "ListStackSetOperationsResult"
      ( \s h x ->
          ListStackSetOperationsResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> ( x Lude..@? "Summaries" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListStackSetOperations where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListStackSetOperations where
  toPath = Lude.const "/"

instance Lude.ToQuery ListStackSetOperations where
  toQuery ListStackSetOperations' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListStackSetOperations" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "StackSetName" Lude.=: stackSetName,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListStackSetOperationsResponse' smart constructor.
data ListStackSetOperationsResponse = ListStackSetOperationsResponse'
  { -- | If the request doesn't return all results, @NextToken@ is set to a token. To retrieve the next set of results, call @ListOperationResults@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, @NextToken@ is set to @null@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of @StackSetOperationSummary@ structures that contain summary information about operations for the specified stack set.
    summaries :: Lude.Maybe [StackSetOperationSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListStackSetOperationsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the request doesn't return all results, @NextToken@ is set to a token. To retrieve the next set of results, call @ListOperationResults@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, @NextToken@ is set to @null@ .
-- * 'summaries' - A list of @StackSetOperationSummary@ structures that contain summary information about operations for the specified stack set.
-- * 'responseStatus' - The response status code.
mkListStackSetOperationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListStackSetOperationsResponse
mkListStackSetOperationsResponse pResponseStatus_ =
  ListStackSetOperationsResponse'
    { nextToken = Lude.Nothing,
      summaries = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the request doesn't return all results, @NextToken@ is set to a token. To retrieve the next set of results, call @ListOperationResults@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, @NextToken@ is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssorsNextToken :: Lens.Lens' ListStackSetOperationsResponse (Lude.Maybe Lude.Text)
lssorsNextToken = Lens.lens (nextToken :: ListStackSetOperationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListStackSetOperationsResponse)
{-# DEPRECATED lssorsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @StackSetOperationSummary@ structures that contain summary information about operations for the specified stack set.
--
-- /Note:/ Consider using 'summaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssorsSummaries :: Lens.Lens' ListStackSetOperationsResponse (Lude.Maybe [StackSetOperationSummary])
lssorsSummaries = Lens.lens (summaries :: ListStackSetOperationsResponse -> Lude.Maybe [StackSetOperationSummary]) (\s a -> s {summaries = a} :: ListStackSetOperationsResponse)
{-# DEPRECATED lssorsSummaries "Use generic-lens or generic-optics with 'summaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssorsResponseStatus :: Lens.Lens' ListStackSetOperationsResponse Lude.Int
lssorsResponseStatus = Lens.lens (responseStatus :: ListStackSetOperationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListStackSetOperationsResponse)
{-# DEPRECATED lssorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
