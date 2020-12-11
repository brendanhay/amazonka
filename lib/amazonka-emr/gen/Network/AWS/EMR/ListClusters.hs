{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ListClusters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides the status of all clusters visible to this AWS account. Allows you to filter the list of clusters based on certain criteria; for example, filtering by cluster creation date and time or by status. This call returns a maximum of 50 clusters per call, but returns a marker to track the paging of the cluster list across multiple ListClusters calls.
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListClusters
  ( -- * Creating a request
    ListClusters (..),
    mkListClusters,

    -- ** Request lenses
    lcCreatedAfter,
    lcMarker,
    lcClusterStates,
    lcCreatedBefore,

    -- * Destructuring the response
    ListClustersResponse (..),
    mkListClustersResponse,

    -- ** Response lenses
    lcrsMarker,
    lcrsClusters,
    lcrsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | This input determines how the ListClusters action filters the list of clusters that it returns.
--
-- /See:/ 'mkListClusters' smart constructor.
data ListClusters = ListClusters'
  { createdAfter ::
      Lude.Maybe Lude.Timestamp,
    marker :: Lude.Maybe Lude.Text,
    clusterStates :: Lude.Maybe [ClusterState],
    createdBefore :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListClusters' with the minimum fields required to make a request.
--
-- * 'clusterStates' - The cluster state filters to apply when listing clusters.
-- * 'createdAfter' - The creation date and time beginning value filter for listing clusters.
-- * 'createdBefore' - The creation date and time end value filter for listing clusters.
-- * 'marker' - The pagination token that indicates the next set of results to retrieve.
mkListClusters ::
  ListClusters
mkListClusters =
  ListClusters'
    { createdAfter = Lude.Nothing,
      marker = Lude.Nothing,
      clusterStates = Lude.Nothing,
      createdBefore = Lude.Nothing
    }

-- | The creation date and time beginning value filter for listing clusters.
--
-- /Note:/ Consider using 'createdAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcCreatedAfter :: Lens.Lens' ListClusters (Lude.Maybe Lude.Timestamp)
lcCreatedAfter = Lens.lens (createdAfter :: ListClusters -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAfter = a} :: ListClusters)
{-# DEPRECATED lcCreatedAfter "Use generic-lens or generic-optics with 'createdAfter' instead." #-}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMarker :: Lens.Lens' ListClusters (Lude.Maybe Lude.Text)
lcMarker = Lens.lens (marker :: ListClusters -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListClusters)
{-# DEPRECATED lcMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The cluster state filters to apply when listing clusters.
--
-- /Note:/ Consider using 'clusterStates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcClusterStates :: Lens.Lens' ListClusters (Lude.Maybe [ClusterState])
lcClusterStates = Lens.lens (clusterStates :: ListClusters -> Lude.Maybe [ClusterState]) (\s a -> s {clusterStates = a} :: ListClusters)
{-# DEPRECATED lcClusterStates "Use generic-lens or generic-optics with 'clusterStates' instead." #-}

-- | The creation date and time end value filter for listing clusters.
--
-- /Note:/ Consider using 'createdBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcCreatedBefore :: Lens.Lens' ListClusters (Lude.Maybe Lude.Timestamp)
lcCreatedBefore = Lens.lens (createdBefore :: ListClusters -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdBefore = a} :: ListClusters)
{-# DEPRECATED lcCreatedBefore "Use generic-lens or generic-optics with 'createdBefore' instead." #-}

instance Page.AWSPager ListClusters where
  page rq rs
    | Page.stop (rs Lens.^. lcrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lcrsClusters) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& lcMarker Lens..~ rs Lens.^. lcrsMarker

instance Lude.AWSRequest ListClusters where
  type Rs ListClusters = ListClustersResponse
  request = Req.postJSON emrService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListClustersResponse'
            Lude.<$> (x Lude..?> "Marker")
            Lude.<*> (x Lude..?> "Clusters" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListClusters where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.ListClusters" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListClusters where
  toJSON ListClusters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CreatedAfter" Lude..=) Lude.<$> createdAfter,
            ("Marker" Lude..=) Lude.<$> marker,
            ("ClusterStates" Lude..=) Lude.<$> clusterStates,
            ("CreatedBefore" Lude..=) Lude.<$> createdBefore
          ]
      )

instance Lude.ToPath ListClusters where
  toPath = Lude.const "/"

instance Lude.ToQuery ListClusters where
  toQuery = Lude.const Lude.mempty

-- | This contains a ClusterSummaryList with the cluster details; for example, the cluster IDs, names, and status.
--
-- /See:/ 'mkListClustersResponse' smart constructor.
data ListClustersResponse = ListClustersResponse'
  { marker ::
      Lude.Maybe Lude.Text,
    clusters :: Lude.Maybe [ClusterSummary],
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

-- | Creates a value of 'ListClustersResponse' with the minimum fields required to make a request.
--
-- * 'clusters' - The list of clusters for the account based on the given filters.
-- * 'marker' - The pagination token that indicates the next set of results to retrieve.
-- * 'responseStatus' - The response status code.
mkListClustersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListClustersResponse
mkListClustersResponse pResponseStatus_ =
  ListClustersResponse'
    { marker = Lude.Nothing,
      clusters = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsMarker :: Lens.Lens' ListClustersResponse (Lude.Maybe Lude.Text)
lcrsMarker = Lens.lens (marker :: ListClustersResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListClustersResponse)
{-# DEPRECATED lcrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The list of clusters for the account based on the given filters.
--
-- /Note:/ Consider using 'clusters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsClusters :: Lens.Lens' ListClustersResponse (Lude.Maybe [ClusterSummary])
lcrsClusters = Lens.lens (clusters :: ListClustersResponse -> Lude.Maybe [ClusterSummary]) (\s a -> s {clusters = a} :: ListClustersResponse)
{-# DEPRECATED lcrsClusters "Use generic-lens or generic-optics with 'clusters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsResponseStatus :: Lens.Lens' ListClustersResponse Lude.Int
lcrsResponseStatus = Lens.lens (responseStatus :: ListClustersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListClustersResponse)
{-# DEPRECATED lcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
