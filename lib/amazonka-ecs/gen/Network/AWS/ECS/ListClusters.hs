{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.ListClusters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of existing clusters.
--
-- This operation returns paginated results.
module Network.AWS.ECS.ListClusters
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
    lcrsClusterARNs,
    lcrsNextToken,
    lcrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListClusters' smart constructor.
data ListClusters = ListClusters'
  { nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListClusters' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of cluster results returned by @ListClusters@ in paginated output. When this parameter is used, @ListClusters@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListClusters@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListClusters@ returns up to 100 results and a @nextToken@ value if applicable.
-- * 'nextToken' - The @nextToken@ value returned from a @ListClusters@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
mkListClusters ::
  ListClusters
mkListClusters =
  ListClusters'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The @nextToken@ value returned from a @ListClusters@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcNextToken :: Lens.Lens' ListClusters (Lude.Maybe Lude.Text)
lcNextToken = Lens.lens (nextToken :: ListClusters -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListClusters)
{-# DEPRECATED lcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of cluster results returned by @ListClusters@ in paginated output. When this parameter is used, @ListClusters@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListClusters@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListClusters@ returns up to 100 results and a @nextToken@ value if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMaxResults :: Lens.Lens' ListClusters (Lude.Maybe Lude.Int)
lcMaxResults = Lens.lens (maxResults :: ListClusters -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListClusters)
{-# DEPRECATED lcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListClusters where
  page rq rs
    | Page.stop (rs Lens.^. lcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcrsClusterARNs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcNextToken Lens..~ rs Lens.^. lcrsNextToken

instance Lude.AWSRequest ListClusters where
  type Rs ListClusters = ListClustersResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListClustersResponse'
            Lude.<$> (x Lude..?> "clusterArns" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListClusters where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.ListClusters" ::
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
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListClusters where
  toPath = Lude.const "/"

instance Lude.ToQuery ListClusters where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListClustersResponse' smart constructor.
data ListClustersResponse = ListClustersResponse'
  { clusterARNs ::
      Lude.Maybe [Lude.Text],
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
-- * 'clusterARNs' - The list of full Amazon Resource Name (ARN) entries for each cluster associated with your account.
-- * 'nextToken' - The @nextToken@ value to include in a future @ListClusters@ request. When the results of a @ListClusters@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkListClustersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListClustersResponse
mkListClustersResponse pResponseStatus_ =
  ListClustersResponse'
    { clusterARNs = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of full Amazon Resource Name (ARN) entries for each cluster associated with your account.
--
-- /Note:/ Consider using 'clusterARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsClusterARNs :: Lens.Lens' ListClustersResponse (Lude.Maybe [Lude.Text])
lcrsClusterARNs = Lens.lens (clusterARNs :: ListClustersResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {clusterARNs = a} :: ListClustersResponse)
{-# DEPRECATED lcrsClusterARNs "Use generic-lens or generic-optics with 'clusterARNs' instead." #-}

-- | The @nextToken@ value to include in a future @ListClusters@ request. When the results of a @ListClusters@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
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
