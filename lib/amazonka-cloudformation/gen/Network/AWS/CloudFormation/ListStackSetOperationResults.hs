{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ListStackSetOperationResults
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about the results of a stack set operation.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.ListStackSetOperationResults
  ( -- * Creating a request
    ListStackSetOperationResults (..),
    mkListStackSetOperationResults,

    -- ** Request lenses
    lssorNextToken,
    lssorMaxResults,
    lssorStackSetName,
    lssorOperationId,

    -- * Destructuring the response
    ListStackSetOperationResultsResponse (..),
    mkListStackSetOperationResultsResponse,

    -- ** Response lenses
    lssorrsNextToken,
    lssorrsSummaries,
    lssorrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListStackSetOperationResults' smart constructor.
data ListStackSetOperationResults = ListStackSetOperationResults'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults ::
      Lude.Maybe Lude.Natural,
    stackSetName :: Lude.Text,
    operationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListStackSetOperationResults' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
-- * 'nextToken' - If the previous request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call @ListStackSetOperationResults@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
-- * 'operationId' - The ID of the stack set operation.
-- * 'stackSetName' - The name or unique ID of the stack set that you want to get operation results for.
mkListStackSetOperationResults ::
  -- | 'stackSetName'
  Lude.Text ->
  -- | 'operationId'
  Lude.Text ->
  ListStackSetOperationResults
mkListStackSetOperationResults pStackSetName_ pOperationId_ =
  ListStackSetOperationResults'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      stackSetName = pStackSetName_,
      operationId = pOperationId_
    }

-- | If the previous request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call @ListStackSetOperationResults@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssorNextToken :: Lens.Lens' ListStackSetOperationResults (Lude.Maybe Lude.Text)
lssorNextToken = Lens.lens (nextToken :: ListStackSetOperationResults -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListStackSetOperationResults)
{-# DEPRECATED lssorNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssorMaxResults :: Lens.Lens' ListStackSetOperationResults (Lude.Maybe Lude.Natural)
lssorMaxResults = Lens.lens (maxResults :: ListStackSetOperationResults -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListStackSetOperationResults)
{-# DEPRECATED lssorMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The name or unique ID of the stack set that you want to get operation results for.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssorStackSetName :: Lens.Lens' ListStackSetOperationResults Lude.Text
lssorStackSetName = Lens.lens (stackSetName :: ListStackSetOperationResults -> Lude.Text) (\s a -> s {stackSetName = a} :: ListStackSetOperationResults)
{-# DEPRECATED lssorStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

-- | The ID of the stack set operation.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssorOperationId :: Lens.Lens' ListStackSetOperationResults Lude.Text
lssorOperationId = Lens.lens (operationId :: ListStackSetOperationResults -> Lude.Text) (\s a -> s {operationId = a} :: ListStackSetOperationResults)
{-# DEPRECATED lssorOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

instance Page.AWSPager ListStackSetOperationResults where
  page rq rs
    | Page.stop (rs Lens.^. lssorrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lssorrsSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lssorNextToken Lens..~ rs Lens.^. lssorrsNextToken

instance Lude.AWSRequest ListStackSetOperationResults where
  type
    Rs ListStackSetOperationResults =
      ListStackSetOperationResultsResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "ListStackSetOperationResultsResult"
      ( \s h x ->
          ListStackSetOperationResultsResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> ( x Lude..@? "Summaries" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListStackSetOperationResults where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListStackSetOperationResults where
  toPath = Lude.const "/"

instance Lude.ToQuery ListStackSetOperationResults where
  toQuery ListStackSetOperationResults' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ListStackSetOperationResults" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "MaxResults" Lude.=: maxResults,
        "StackSetName" Lude.=: stackSetName,
        "OperationId" Lude.=: operationId
      ]

-- | /See:/ 'mkListStackSetOperationResultsResponse' smart constructor.
data ListStackSetOperationResultsResponse = ListStackSetOperationResultsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    summaries ::
      Lude.Maybe
        [StackSetOperationResultSummary],
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

-- | Creates a value of 'ListStackSetOperationResultsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the request doesn't return all results, @NextToken@ is set to a token. To retrieve the next set of results, call @ListOperationResults@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, @NextToken@ is set to @null@ .
-- * 'responseStatus' - The response status code.
-- * 'summaries' - A list of @StackSetOperationResultSummary@ structures that contain information about the specified operation results, for accounts and Regions that are included in the operation.
mkListStackSetOperationResultsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListStackSetOperationResultsResponse
mkListStackSetOperationResultsResponse pResponseStatus_ =
  ListStackSetOperationResultsResponse'
    { nextToken = Lude.Nothing,
      summaries = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the request doesn't return all results, @NextToken@ is set to a token. To retrieve the next set of results, call @ListOperationResults@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, @NextToken@ is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssorrsNextToken :: Lens.Lens' ListStackSetOperationResultsResponse (Lude.Maybe Lude.Text)
lssorrsNextToken = Lens.lens (nextToken :: ListStackSetOperationResultsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListStackSetOperationResultsResponse)
{-# DEPRECATED lssorrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @StackSetOperationResultSummary@ structures that contain information about the specified operation results, for accounts and Regions that are included in the operation.
--
-- /Note:/ Consider using 'summaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssorrsSummaries :: Lens.Lens' ListStackSetOperationResultsResponse (Lude.Maybe [StackSetOperationResultSummary])
lssorrsSummaries = Lens.lens (summaries :: ListStackSetOperationResultsResponse -> Lude.Maybe [StackSetOperationResultSummary]) (\s a -> s {summaries = a} :: ListStackSetOperationResultsResponse)
{-# DEPRECATED lssorrsSummaries "Use generic-lens or generic-optics with 'summaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssorrsResponseStatus :: Lens.Lens' ListStackSetOperationResultsResponse Lude.Int
lssorrsResponseStatus = Lens.lens (responseStatus :: ListStackSetOperationResultsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListStackSetOperationResultsResponse)
{-# DEPRECATED lssorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
