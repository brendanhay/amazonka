{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ListStackSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about stack sets that are associated with the user.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.ListStackSets
  ( -- * Creating a request
    ListStackSets (..),
    mkListStackSets,

    -- ** Request lenses
    lssStatus,
    lssNextToken,
    lssMaxResults,

    -- * Destructuring the response
    ListStackSetsResponse (..),
    mkListStackSetsResponse,

    -- ** Response lenses
    lssrsNextToken,
    lssrsSummaries,
    lssrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListStackSets' smart constructor.
data ListStackSets = ListStackSets'
  { status ::
      Lude.Maybe StackSetStatus,
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListStackSets' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
-- * 'nextToken' - If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call @ListStackSets@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
-- * 'status' - The status of the stack sets that you want to get summary information about.
mkListStackSets ::
  ListStackSets
mkListStackSets =
  ListStackSets'
    { status = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The status of the stack sets that you want to get summary information about.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssStatus :: Lens.Lens' ListStackSets (Lude.Maybe StackSetStatus)
lssStatus = Lens.lens (status :: ListStackSets -> Lude.Maybe StackSetStatus) (\s a -> s {status = a} :: ListStackSets)
{-# DEPRECATED lssStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call @ListStackSets@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssNextToken :: Lens.Lens' ListStackSets (Lude.Maybe Lude.Text)
lssNextToken = Lens.lens (nextToken :: ListStackSets -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListStackSets)
{-# DEPRECATED lssNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssMaxResults :: Lens.Lens' ListStackSets (Lude.Maybe Lude.Natural)
lssMaxResults = Lens.lens (maxResults :: ListStackSets -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListStackSets)
{-# DEPRECATED lssMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListStackSets where
  page rq rs
    | Page.stop (rs Lens.^. lssrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lssrsSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lssNextToken Lens..~ rs Lens.^. lssrsNextToken

instance Lude.AWSRequest ListStackSets where
  type Rs ListStackSets = ListStackSetsResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "ListStackSetsResult"
      ( \s h x ->
          ListStackSetsResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> ( x Lude..@? "Summaries" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListStackSets where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListStackSets where
  toPath = Lude.const "/"

instance Lude.ToQuery ListStackSets where
  toQuery ListStackSets' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListStackSets" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "Status" Lude.=: status,
        "NextToken" Lude.=: nextToken,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListStackSetsResponse' smart constructor.
data ListStackSetsResponse = ListStackSetsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    summaries :: Lude.Maybe [StackSetSummary],
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

-- | Creates a value of 'ListStackSetsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call @ListStackInstances@ again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
-- * 'responseStatus' - The response status code.
-- * 'summaries' - A list of @StackSetSummary@ structures that contain information about the user's stack sets.
mkListStackSetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListStackSetsResponse
mkListStackSetsResponse pResponseStatus_ =
  ListStackSetsResponse'
    { nextToken = Lude.Nothing,
      summaries = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call @ListStackInstances@ again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssrsNextToken :: Lens.Lens' ListStackSetsResponse (Lude.Maybe Lude.Text)
lssrsNextToken = Lens.lens (nextToken :: ListStackSetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListStackSetsResponse)
{-# DEPRECATED lssrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @StackSetSummary@ structures that contain information about the user's stack sets.
--
-- /Note:/ Consider using 'summaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssrsSummaries :: Lens.Lens' ListStackSetsResponse (Lude.Maybe [StackSetSummary])
lssrsSummaries = Lens.lens (summaries :: ListStackSetsResponse -> Lude.Maybe [StackSetSummary]) (\s a -> s {summaries = a} :: ListStackSetsResponse)
{-# DEPRECATED lssrsSummaries "Use generic-lens or generic-optics with 'summaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssrsResponseStatus :: Lens.Lens' ListStackSetsResponse Lude.Int
lssrsResponseStatus = Lens.lens (responseStatus :: ListStackSetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListStackSetsResponse)
{-# DEPRECATED lssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
