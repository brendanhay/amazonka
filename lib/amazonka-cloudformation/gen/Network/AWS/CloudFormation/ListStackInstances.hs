{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ListStackInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about stack instances that are associated with the specified stack set. You can filter for stack instances that are associated with a specific AWS account name or Region, or that have a specific status.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.ListStackInstances
  ( -- * Creating a request
    ListStackInstances (..),
    mkListStackInstances,

    -- ** Request lenses
    lsiStackInstanceRegion,
    lsiFilters,
    lsiNextToken,
    lsiStackSetName,
    lsiStackInstanceAccount,
    lsiMaxResults,

    -- * Destructuring the response
    ListStackInstancesResponse (..),
    mkListStackInstancesResponse,

    -- ** Response lenses
    lsirsNextToken,
    lsirsSummaries,
    lsirsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListStackInstances' smart constructor.
data ListStackInstances = ListStackInstances'
  { -- | The name of the Region where you want to list stack instances.
    stackInstanceRegion :: Lude.Maybe Lude.Text,
    -- | The status that stack instances are filtered by.
    filters :: Lude.Maybe [StackInstanceFilter],
    -- | If the previous request didn't return all of the remaining results, the response's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call @ListStackInstances@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | The name or unique ID of the stack set that you want to list stack instances for.
    stackSetName :: Lude.Text,
    -- | The name of the AWS account that you want to list stack instances for.
    stackInstanceAccount :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListStackInstances' with the minimum fields required to make a request.
--
-- * 'stackInstanceRegion' - The name of the Region where you want to list stack instances.
-- * 'filters' - The status that stack instances are filtered by.
-- * 'nextToken' - If the previous request didn't return all of the remaining results, the response's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call @ListStackInstances@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
-- * 'stackSetName' - The name or unique ID of the stack set that you want to list stack instances for.
-- * 'stackInstanceAccount' - The name of the AWS account that you want to list stack instances for.
-- * 'maxResults' - The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
mkListStackInstances ::
  -- | 'stackSetName'
  Lude.Text ->
  ListStackInstances
mkListStackInstances pStackSetName_ =
  ListStackInstances'
    { stackInstanceRegion = Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      stackSetName = pStackSetName_,
      stackInstanceAccount = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The name of the Region where you want to list stack instances.
--
-- /Note:/ Consider using 'stackInstanceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsiStackInstanceRegion :: Lens.Lens' ListStackInstances (Lude.Maybe Lude.Text)
lsiStackInstanceRegion = Lens.lens (stackInstanceRegion :: ListStackInstances -> Lude.Maybe Lude.Text) (\s a -> s {stackInstanceRegion = a} :: ListStackInstances)
{-# DEPRECATED lsiStackInstanceRegion "Use generic-lens or generic-optics with 'stackInstanceRegion' instead." #-}

-- | The status that stack instances are filtered by.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsiFilters :: Lens.Lens' ListStackInstances (Lude.Maybe [StackInstanceFilter])
lsiFilters = Lens.lens (filters :: ListStackInstances -> Lude.Maybe [StackInstanceFilter]) (\s a -> s {filters = a} :: ListStackInstances)
{-# DEPRECATED lsiFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | If the previous request didn't return all of the remaining results, the response's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call @ListStackInstances@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsiNextToken :: Lens.Lens' ListStackInstances (Lude.Maybe Lude.Text)
lsiNextToken = Lens.lens (nextToken :: ListStackInstances -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListStackInstances)
{-# DEPRECATED lsiNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name or unique ID of the stack set that you want to list stack instances for.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsiStackSetName :: Lens.Lens' ListStackInstances Lude.Text
lsiStackSetName = Lens.lens (stackSetName :: ListStackInstances -> Lude.Text) (\s a -> s {stackSetName = a} :: ListStackInstances)
{-# DEPRECATED lsiStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

-- | The name of the AWS account that you want to list stack instances for.
--
-- /Note:/ Consider using 'stackInstanceAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsiStackInstanceAccount :: Lens.Lens' ListStackInstances (Lude.Maybe Lude.Text)
lsiStackInstanceAccount = Lens.lens (stackInstanceAccount :: ListStackInstances -> Lude.Maybe Lude.Text) (\s a -> s {stackInstanceAccount = a} :: ListStackInstances)
{-# DEPRECATED lsiStackInstanceAccount "Use generic-lens or generic-optics with 'stackInstanceAccount' instead." #-}

-- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsiMaxResults :: Lens.Lens' ListStackInstances (Lude.Maybe Lude.Natural)
lsiMaxResults = Lens.lens (maxResults :: ListStackInstances -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListStackInstances)
{-# DEPRECATED lsiMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListStackInstances where
  page rq rs
    | Page.stop (rs Lens.^. lsirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsirsSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsiNextToken Lens..~ rs Lens.^. lsirsNextToken

instance Lude.AWSRequest ListStackInstances where
  type Rs ListStackInstances = ListStackInstancesResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "ListStackInstancesResult"
      ( \s h x ->
          ListStackInstancesResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> ( x Lude..@? "Summaries" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListStackInstances where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListStackInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery ListStackInstances where
  toQuery ListStackInstances' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListStackInstances" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "StackInstanceRegion" Lude.=: stackInstanceRegion,
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "StackSetName" Lude.=: stackSetName,
        "StackInstanceAccount" Lude.=: stackInstanceAccount,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListStackInstancesResponse' smart constructor.
data ListStackInstancesResponse = ListStackInstancesResponse'
  { -- | If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call @ListStackInstances@ again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of @StackInstanceSummary@ structures that contain information about the specified stack instances.
    summaries :: Lude.Maybe [StackInstanceSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListStackInstancesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call @ListStackInstances@ again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
-- * 'summaries' - A list of @StackInstanceSummary@ structures that contain information about the specified stack instances.
-- * 'responseStatus' - The response status code.
mkListStackInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListStackInstancesResponse
mkListStackInstancesResponse pResponseStatus_ =
  ListStackInstancesResponse'
    { nextToken = Lude.Nothing,
      summaries = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call @ListStackInstances@ again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsirsNextToken :: Lens.Lens' ListStackInstancesResponse (Lude.Maybe Lude.Text)
lsirsNextToken = Lens.lens (nextToken :: ListStackInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListStackInstancesResponse)
{-# DEPRECATED lsirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @StackInstanceSummary@ structures that contain information about the specified stack instances.
--
-- /Note:/ Consider using 'summaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsirsSummaries :: Lens.Lens' ListStackInstancesResponse (Lude.Maybe [StackInstanceSummary])
lsirsSummaries = Lens.lens (summaries :: ListStackInstancesResponse -> Lude.Maybe [StackInstanceSummary]) (\s a -> s {summaries = a} :: ListStackInstancesResponse)
{-# DEPRECATED lsirsSummaries "Use generic-lens or generic-optics with 'summaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsirsResponseStatus :: Lens.Lens' ListStackInstancesResponse Lude.Int
lsirsResponseStatus = Lens.lens (responseStatus :: ListStackInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListStackInstancesResponse)
{-# DEPRECATED lsirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
