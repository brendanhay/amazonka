{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.DescribeClusters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about AWS CloudHSM clusters.
--
-- This is a paginated operation, which means that each response might contain only a subset of all the clusters. When the response contains only a subset of clusters, it includes a @NextToken@ value. Use this value in a subsequent @DescribeClusters@ request to get more clusters. When you receive a response with no @NextToken@ (or an empty or null value), that means there are no more clusters to get.
--
-- This operation returns paginated results.
module Network.AWS.CloudHSMv2.DescribeClusters
  ( -- * Creating a request
    DescribeClusters (..),
    mkDescribeClusters,

    -- ** Request lenses
    dcFilters,
    dcNextToken,
    dcMaxResults,

    -- * Destructuring the response
    DescribeClustersResponse (..),
    mkDescribeClustersResponse,

    -- ** Response lenses
    dcrsNextToken,
    dcrsClusters,
    dcrsResponseStatus,
  )
where

import Network.AWS.CloudHSMv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeClusters' smart constructor.
data DescribeClusters = DescribeClusters'
  { -- | One or more filters to limit the items returned in the response.
    --
    -- Use the @clusterIds@ filter to return only the specified clusters. Specify clusters by their cluster identifier (ID).
    -- Use the @vpcIds@ filter to return only the clusters in the specified virtual private clouds (VPCs). Specify VPCs by their VPC identifier (ID).
    -- Use the @states@ filter to return only clusters that match the specified state.
    filters :: Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    -- | The @NextToken@ value that you received in the previous response. Use this value to get more clusters.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of clusters to return in the response. When there are more clusters than the number you specify, the response contains a @NextToken@ value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClusters' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters to limit the items returned in the response.
--
-- Use the @clusterIds@ filter to return only the specified clusters. Specify clusters by their cluster identifier (ID).
-- Use the @vpcIds@ filter to return only the clusters in the specified virtual private clouds (VPCs). Specify VPCs by their VPC identifier (ID).
-- Use the @states@ filter to return only clusters that match the specified state.
-- * 'nextToken' - The @NextToken@ value that you received in the previous response. Use this value to get more clusters.
-- * 'maxResults' - The maximum number of clusters to return in the response. When there are more clusters than the number you specify, the response contains a @NextToken@ value.
mkDescribeClusters ::
  DescribeClusters
mkDescribeClusters =
  DescribeClusters'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters to limit the items returned in the response.
--
-- Use the @clusterIds@ filter to return only the specified clusters. Specify clusters by their cluster identifier (ID).
-- Use the @vpcIds@ filter to return only the clusters in the specified virtual private clouds (VPCs). Specify VPCs by their VPC identifier (ID).
-- Use the @states@ filter to return only clusters that match the specified state.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcFilters :: Lens.Lens' DescribeClusters (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
dcFilters = Lens.lens (filters :: DescribeClusters -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {filters = a} :: DescribeClusters)
{-# DEPRECATED dcFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The @NextToken@ value that you received in the previous response. Use this value to get more clusters.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcNextToken :: Lens.Lens' DescribeClusters (Lude.Maybe Lude.Text)
dcNextToken = Lens.lens (nextToken :: DescribeClusters -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeClusters)
{-# DEPRECATED dcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of clusters to return in the response. When there are more clusters than the number you specify, the response contains a @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcMaxResults :: Lens.Lens' DescribeClusters (Lude.Maybe Lude.Natural)
dcMaxResults = Lens.lens (maxResults :: DescribeClusters -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeClusters)
{-# DEPRECATED dcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeClusters where
  page rq rs
    | Page.stop (rs Lens.^. dcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dcrsClusters) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dcNextToken Lens..~ rs Lens.^. dcrsNextToken

instance Lude.AWSRequest DescribeClusters where
  type Rs DescribeClusters = DescribeClustersResponse
  request = Req.postJSON cloudHSMv2Service
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeClustersResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Clusters" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeClusters where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("BaldrApiService.DescribeClusters" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeClusters where
  toJSON DescribeClusters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeClusters where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeClusters where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeClustersResponse' smart constructor.
data DescribeClustersResponse = DescribeClustersResponse'
  { -- | An opaque string that indicates that the response contains only a subset of clusters. Use this value in a subsequent @DescribeClusters@ request to get more clusters.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of clusters.
    clusters :: Lude.Maybe [Cluster],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClustersResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - An opaque string that indicates that the response contains only a subset of clusters. Use this value in a subsequent @DescribeClusters@ request to get more clusters.
-- * 'clusters' - A list of clusters.
-- * 'responseStatus' - The response status code.
mkDescribeClustersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeClustersResponse
mkDescribeClustersResponse pResponseStatus_ =
  DescribeClustersResponse'
    { nextToken = Lude.Nothing,
      clusters = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An opaque string that indicates that the response contains only a subset of clusters. Use this value in a subsequent @DescribeClusters@ request to get more clusters.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsNextToken :: Lens.Lens' DescribeClustersResponse (Lude.Maybe Lude.Text)
dcrsNextToken = Lens.lens (nextToken :: DescribeClustersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeClustersResponse)
{-# DEPRECATED dcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of clusters.
--
-- /Note:/ Consider using 'clusters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsClusters :: Lens.Lens' DescribeClustersResponse (Lude.Maybe [Cluster])
dcrsClusters = Lens.lens (clusters :: DescribeClustersResponse -> Lude.Maybe [Cluster]) (\s a -> s {clusters = a} :: DescribeClustersResponse)
{-# DEPRECATED dcrsClusters "Use generic-lens or generic-optics with 'clusters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsResponseStatus :: Lens.Lens' DescribeClustersResponse Lude.Int
dcrsResponseStatus = Lens.lens (responseStatus :: DescribeClustersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeClustersResponse)
{-# DEPRECATED dcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
