{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.ListClusters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @ClusterListEntry@ objects of the specified length. Each @ClusterListEntry@ object contains a cluster's state, a cluster's ID, and other important status information.
--
-- This operation returns paginated results.
module Network.AWS.Snowball.ListClusters
  ( -- * Creating a request
    ListClusters (..),
    mkListClusters,

    -- ** Request lenses
    lcNextToken,
    lcMaxResults,

    -- * Destructuring the response
    ListClustersResponse (..),
    mkListClustersResponse,

    -- ** Response lenses
    lcrsClusterListEntries,
    lcrsNextToken,
    lcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Snowball.Types

-- | /See:/ 'mkListClusters' smart constructor.
data ListClusters = ListClusters'
  { nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListClusters' with the minimum fields required to make a request.
--
-- * 'maxResults' - The number of @ClusterListEntry@ objects to return.
-- * 'nextToken' - HTTP requests are stateless. To identify what object comes "next" in the list of @ClusterListEntry@ objects, you have the option of specifying @NextToken@ as the starting point for your returned list.
mkListClusters ::
  ListClusters
mkListClusters =
  ListClusters'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | HTTP requests are stateless. To identify what object comes "next" in the list of @ClusterListEntry@ objects, you have the option of specifying @NextToken@ as the starting point for your returned list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcNextToken :: Lens.Lens' ListClusters (Lude.Maybe Lude.Text)
lcNextToken = Lens.lens (nextToken :: ListClusters -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListClusters)
{-# DEPRECATED lcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The number of @ClusterListEntry@ objects to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMaxResults :: Lens.Lens' ListClusters (Lude.Maybe Lude.Natural)
lcMaxResults = Lens.lens (maxResults :: ListClusters -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListClusters)
{-# DEPRECATED lcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListClusters where
  page rq rs
    | Page.stop (rs Lens.^. lcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcrsClusterListEntries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcNextToken Lens..~ rs Lens.^. lcrsNextToken

instance Lude.AWSRequest ListClusters where
  type Rs ListClusters = ListClustersResponse
  request = Req.postJSON snowballService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListClustersResponse'
            Lude.<$> (x Lude..?> "ClusterListEntries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListClusters where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSIESnowballJobManagementService.ListClusters" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListClusters where
  toJSON ListClusters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListClusters where
  toPath = Lude.const "/"

instance Lude.ToQuery ListClusters where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListClustersResponse' smart constructor.
data ListClustersResponse = ListClustersResponse'
  { clusterListEntries ::
      Lude.Maybe [ClusterListEntry],
    nextToken :: Lude.Maybe Lude.Text,
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
-- * 'clusterListEntries' - Each @ClusterListEntry@ object contains a cluster's state, a cluster's ID, and other important status information.
-- * 'nextToken' - HTTP requests are stateless. If you use the automatically generated @NextToken@ value in your next @ClusterListEntry@ call, your list of returned clusters will start from this point in the array.
-- * 'responseStatus' - The response status code.
mkListClustersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListClustersResponse
mkListClustersResponse pResponseStatus_ =
  ListClustersResponse'
    { clusterListEntries = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Each @ClusterListEntry@ object contains a cluster's state, a cluster's ID, and other important status information.
--
-- /Note:/ Consider using 'clusterListEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsClusterListEntries :: Lens.Lens' ListClustersResponse (Lude.Maybe [ClusterListEntry])
lcrsClusterListEntries = Lens.lens (clusterListEntries :: ListClustersResponse -> Lude.Maybe [ClusterListEntry]) (\s a -> s {clusterListEntries = a} :: ListClustersResponse)
{-# DEPRECATED lcrsClusterListEntries "Use generic-lens or generic-optics with 'clusterListEntries' instead." #-}

-- | HTTP requests are stateless. If you use the automatically generated @NextToken@ value in your next @ClusterListEntry@ call, your list of returned clusters will start from this point in the array.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsNextToken :: Lens.Lens' ListClustersResponse (Lude.Maybe Lude.Text)
lcrsNextToken = Lens.lens (nextToken :: ListClustersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListClustersResponse)
{-# DEPRECATED lcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsResponseStatus :: Lens.Lens' ListClustersResponse Lude.Int
lcrsResponseStatus = Lens.lens (responseStatus :: ListClustersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListClustersResponse)
{-# DEPRECATED lcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
