{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeEcsClusters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes Amazon ECS clusters that are registered with a stack. If you specify only a stack ID, you can use the @MaxResults@ and @NextToken@ parameters to paginate the response. However, AWS OpsWorks Stacks currently supports only one cluster per layer, so the result set has a maximum of one element.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack or an attached policy that explicitly grants permission. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
-- This call accepts only one resource-identifying parameter.
--
-- This operation returns paginated results.
module Network.AWS.OpsWorks.DescribeEcsClusters
  ( -- * Creating a request
    DescribeEcsClusters (..),
    mkDescribeEcsClusters,

    -- ** Request lenses
    decNextToken,
    decStackId,
    decMaxResults,
    decEcsClusterARNs,

    -- * Destructuring the response
    DescribeEcsClustersResponse (..),
    mkDescribeEcsClustersResponse,

    -- ** Response lenses
    decrsNextToken,
    decrsEcsClusters,
    decrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeEcsClusters' smart constructor.
data DescribeEcsClusters = DescribeEcsClusters'
  { nextToken ::
      Lude.Maybe Lude.Text,
    stackId :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Int,
    ecsClusterARNs :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEcsClusters' with the minimum fields required to make a request.
--
-- * 'ecsClusterARNs' - A list of ARNs, one for each cluster to be described.
-- * 'maxResults' - To receive a paginated response, use this parameter to specify the maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
-- * 'nextToken' - If the previous paginated request did not return all of the remaining results, the response object's@NextToken@ parameter value is set to a token. To retrieve the next set of results, call @DescribeEcsClusters@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
-- * 'stackId' - A stack ID. @DescribeEcsClusters@ returns a description of the cluster that is registered with the stack.
mkDescribeEcsClusters ::
  DescribeEcsClusters
mkDescribeEcsClusters =
  DescribeEcsClusters'
    { nextToken = Lude.Nothing,
      stackId = Lude.Nothing,
      maxResults = Lude.Nothing,
      ecsClusterARNs = Lude.Nothing
    }

-- | If the previous paginated request did not return all of the remaining results, the response object's@NextToken@ parameter value is set to a token. To retrieve the next set of results, call @DescribeEcsClusters@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decNextToken :: Lens.Lens' DescribeEcsClusters (Lude.Maybe Lude.Text)
decNextToken = Lens.lens (nextToken :: DescribeEcsClusters -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEcsClusters)
{-# DEPRECATED decNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A stack ID. @DescribeEcsClusters@ returns a description of the cluster that is registered with the stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decStackId :: Lens.Lens' DescribeEcsClusters (Lude.Maybe Lude.Text)
decStackId = Lens.lens (stackId :: DescribeEcsClusters -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: DescribeEcsClusters)
{-# DEPRECATED decStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | To receive a paginated response, use this parameter to specify the maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decMaxResults :: Lens.Lens' DescribeEcsClusters (Lude.Maybe Lude.Int)
decMaxResults = Lens.lens (maxResults :: DescribeEcsClusters -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeEcsClusters)
{-# DEPRECATED decMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A list of ARNs, one for each cluster to be described.
--
-- /Note:/ Consider using 'ecsClusterARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decEcsClusterARNs :: Lens.Lens' DescribeEcsClusters (Lude.Maybe [Lude.Text])
decEcsClusterARNs = Lens.lens (ecsClusterARNs :: DescribeEcsClusters -> Lude.Maybe [Lude.Text]) (\s a -> s {ecsClusterARNs = a} :: DescribeEcsClusters)
{-# DEPRECATED decEcsClusterARNs "Use generic-lens or generic-optics with 'ecsClusterARNs' instead." #-}

instance Page.AWSPager DescribeEcsClusters where
  page rq rs
    | Page.stop (rs Lens.^. decrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. decrsEcsClusters) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& decNextToken Lens..~ rs Lens.^. decrsNextToken

instance Lude.AWSRequest DescribeEcsClusters where
  type Rs DescribeEcsClusters = DescribeEcsClustersResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEcsClustersResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "EcsClusters" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEcsClusters where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DescribeEcsClusters" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEcsClusters where
  toJSON DescribeEcsClusters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("StackId" Lude..=) Lude.<$> stackId,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("EcsClusterArns" Lude..=) Lude.<$> ecsClusterARNs
          ]
      )

instance Lude.ToPath DescribeEcsClusters where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEcsClusters where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @DescribeEcsClusters@ request.
--
-- /See:/ 'mkDescribeEcsClustersResponse' smart constructor.
data DescribeEcsClustersResponse = DescribeEcsClustersResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    ecsClusters ::
      Lude.Maybe [EcsCluster],
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

-- | Creates a value of 'DescribeEcsClustersResponse' with the minimum fields required to make a request.
--
-- * 'ecsClusters' - A list of @EcsCluster@ objects containing the cluster descriptions.
-- * 'nextToken' - If a paginated request does not return all of the remaining results, this parameter is set to a token that you can assign to the request object's @NextToken@ parameter to retrieve the next set of results. If the previous paginated request returned all of the remaining results, this parameter is set to @null@ .
-- * 'responseStatus' - The response status code.
mkDescribeEcsClustersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEcsClustersResponse
mkDescribeEcsClustersResponse pResponseStatus_ =
  DescribeEcsClustersResponse'
    { nextToken = Lude.Nothing,
      ecsClusters = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If a paginated request does not return all of the remaining results, this parameter is set to a token that you can assign to the request object's @NextToken@ parameter to retrieve the next set of results. If the previous paginated request returned all of the remaining results, this parameter is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrsNextToken :: Lens.Lens' DescribeEcsClustersResponse (Lude.Maybe Lude.Text)
decrsNextToken = Lens.lens (nextToken :: DescribeEcsClustersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEcsClustersResponse)
{-# DEPRECATED decrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @EcsCluster@ objects containing the cluster descriptions.
--
-- /Note:/ Consider using 'ecsClusters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrsEcsClusters :: Lens.Lens' DescribeEcsClustersResponse (Lude.Maybe [EcsCluster])
decrsEcsClusters = Lens.lens (ecsClusters :: DescribeEcsClustersResponse -> Lude.Maybe [EcsCluster]) (\s a -> s {ecsClusters = a} :: DescribeEcsClustersResponse)
{-# DEPRECATED decrsEcsClusters "Use generic-lens or generic-optics with 'ecsClusters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrsResponseStatus :: Lens.Lens' DescribeEcsClustersResponse Lude.Int
decrsResponseStatus = Lens.lens (responseStatus :: DescribeEcsClustersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEcsClustersResponse)
{-# DEPRECATED decrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
