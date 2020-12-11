{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.DescribeClusters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all provisioned DAX clusters if no cluster identifier is specified, or about a specific DAX cluster if a cluster identifier is supplied.
--
-- If the cluster is in the CREATING state, only cluster level information will be displayed until all of the nodes are successfully provisioned.
-- If the cluster is in the DELETING state, only cluster level information will be displayed.
-- If nodes are currently being added to the DAX cluster, node endpoint information and creation time for the additional nodes will not be displayed until they are completely provisioned. When the DAX cluster state is /available/ , the cluster is ready for use.
-- If nodes are currently being removed from the DAX cluster, no endpoint information for the removed nodes is displayed.
--
-- This operation returns paginated results.
module Network.AWS.DAX.DescribeClusters
  ( -- * Creating a request
    DescribeClusters (..),
    mkDescribeClusters,

    -- ** Request lenses
    dcClusterNames,
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

import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeClusters' smart constructor.
data DescribeClusters = DescribeClusters'
  { clusterNames ::
      Lude.Maybe [Lude.Text],
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClusters' with the minimum fields required to make a request.
--
-- * 'clusterNames' - The names of the DAX clusters being described.
-- * 'maxResults' - The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
-- * 'nextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
mkDescribeClusters ::
  DescribeClusters
mkDescribeClusters =
  DescribeClusters'
    { clusterNames = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The names of the DAX clusters being described.
--
-- /Note:/ Consider using 'clusterNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcClusterNames :: Lens.Lens' DescribeClusters (Lude.Maybe [Lude.Text])
dcClusterNames = Lens.lens (clusterNames :: DescribeClusters -> Lude.Maybe [Lude.Text]) (\s a -> s {clusterNames = a} :: DescribeClusters)
{-# DEPRECATED dcClusterNames "Use generic-lens or generic-optics with 'clusterNames' instead." #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcNextToken :: Lens.Lens' DescribeClusters (Lude.Maybe Lude.Text)
dcNextToken = Lens.lens (nextToken :: DescribeClusters -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeClusters)
{-# DEPRECATED dcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcMaxResults :: Lens.Lens' DescribeClusters (Lude.Maybe Lude.Int)
dcMaxResults = Lens.lens (maxResults :: DescribeClusters -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeClusters)
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
  request = Req.postJSON daxService
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
              Lude.=# ("AmazonDAXV3.DescribeClusters" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeClusters where
  toJSON DescribeClusters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClusterNames" Lude..=) Lude.<$> clusterNames,
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
  { nextToken ::
      Lude.Maybe Lude.Text,
    clusters :: Lude.Maybe [Cluster],
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

-- | Creates a value of 'DescribeClustersResponse' with the minimum fields required to make a request.
--
-- * 'clusters' - The descriptions of your DAX clusters, in response to a /DescribeClusters/ request.
-- * 'nextToken' - Provides an identifier to allow retrieval of paginated results.
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

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsNextToken :: Lens.Lens' DescribeClustersResponse (Lude.Maybe Lude.Text)
dcrsNextToken = Lens.lens (nextToken :: DescribeClustersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeClustersResponse)
{-# DEPRECATED dcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The descriptions of your DAX clusters, in response to a /DescribeClusters/ request.
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
