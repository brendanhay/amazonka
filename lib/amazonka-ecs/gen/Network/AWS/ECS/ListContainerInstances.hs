{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.ListContainerInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of container instances in a specified cluster. You can filter the results of a @ListContainerInstances@ operation with cluster query language statements inside the @filter@ parameter. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.ECS.ListContainerInstances
  ( -- * Creating a request
    ListContainerInstances (..),
    mkListContainerInstances,

    -- ** Request lenses
    lciStatus,
    lciCluster,
    lciNextToken,
    lciFilter,
    lciMaxResults,

    -- * Destructuring the response
    ListContainerInstancesResponse (..),
    mkListContainerInstancesResponse,

    -- ** Response lenses
    lcirsContainerInstanceARNs,
    lcirsNextToken,
    lcirsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListContainerInstances' smart constructor.
data ListContainerInstances = ListContainerInstances'
  { -- | Filters the container instances by status. For example, if you specify the @DRAINING@ status, the results include only container instances that have been set to @DRAINING@ using 'UpdateContainerInstancesState' . If you do not specify this parameter, the default is to include container instances set to all states other than @INACTIVE@ .
    status :: Lude.Maybe ContainerInstanceStatus,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the container instances to list. If you do not specify a cluster, the default cluster is assumed.
    cluster :: Lude.Maybe Lude.Text,
    -- | The @nextToken@ value returned from a @ListContainerInstances@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | You can filter the results of a @ListContainerInstances@ operation with cluster query language statements. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon Elastic Container Service Developer Guide/ .
    filter :: Lude.Maybe Lude.Text,
    -- | The maximum number of container instance results returned by @ListContainerInstances@ in paginated output. When this parameter is used, @ListContainerInstances@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListContainerInstances@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListContainerInstances@ returns up to 100 results and a @nextToken@ value if applicable.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListContainerInstances' with the minimum fields required to make a request.
--
-- * 'status' - Filters the container instances by status. For example, if you specify the @DRAINING@ status, the results include only container instances that have been set to @DRAINING@ using 'UpdateContainerInstancesState' . If you do not specify this parameter, the default is to include container instances set to all states other than @INACTIVE@ .
-- * 'cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that hosts the container instances to list. If you do not specify a cluster, the default cluster is assumed.
-- * 'nextToken' - The @nextToken@ value returned from a @ListContainerInstances@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
-- * 'filter' - You can filter the results of a @ListContainerInstances@ operation with cluster query language statements. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'maxResults' - The maximum number of container instance results returned by @ListContainerInstances@ in paginated output. When this parameter is used, @ListContainerInstances@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListContainerInstances@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListContainerInstances@ returns up to 100 results and a @nextToken@ value if applicable.
mkListContainerInstances ::
  ListContainerInstances
mkListContainerInstances =
  ListContainerInstances'
    { status = Lude.Nothing,
      cluster = Lude.Nothing,
      nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Filters the container instances by status. For example, if you specify the @DRAINING@ status, the results include only container instances that have been set to @DRAINING@ using 'UpdateContainerInstancesState' . If you do not specify this parameter, the default is to include container instances set to all states other than @INACTIVE@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciStatus :: Lens.Lens' ListContainerInstances (Lude.Maybe ContainerInstanceStatus)
lciStatus = Lens.lens (status :: ListContainerInstances -> Lude.Maybe ContainerInstanceStatus) (\s a -> s {status = a} :: ListContainerInstances)
{-# DEPRECATED lciStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the container instances to list. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciCluster :: Lens.Lens' ListContainerInstances (Lude.Maybe Lude.Text)
lciCluster = Lens.lens (cluster :: ListContainerInstances -> Lude.Maybe Lude.Text) (\s a -> s {cluster = a} :: ListContainerInstances)
{-# DEPRECATED lciCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The @nextToken@ value returned from a @ListContainerInstances@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciNextToken :: Lens.Lens' ListContainerInstances (Lude.Maybe Lude.Text)
lciNextToken = Lens.lens (nextToken :: ListContainerInstances -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListContainerInstances)
{-# DEPRECATED lciNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | You can filter the results of a @ListContainerInstances@ operation with cluster query language statements. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciFilter :: Lens.Lens' ListContainerInstances (Lude.Maybe Lude.Text)
lciFilter = Lens.lens (filter :: ListContainerInstances -> Lude.Maybe Lude.Text) (\s a -> s {filter = a} :: ListContainerInstances)
{-# DEPRECATED lciFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of container instance results returned by @ListContainerInstances@ in paginated output. When this parameter is used, @ListContainerInstances@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListContainerInstances@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListContainerInstances@ returns up to 100 results and a @nextToken@ value if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciMaxResults :: Lens.Lens' ListContainerInstances (Lude.Maybe Lude.Int)
lciMaxResults = Lens.lens (maxResults :: ListContainerInstances -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListContainerInstances)
{-# DEPRECATED lciMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListContainerInstances where
  page rq rs
    | Page.stop (rs Lens.^. lcirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcirsContainerInstanceARNs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lciNextToken Lens..~ rs Lens.^. lcirsNextToken

instance Lude.AWSRequest ListContainerInstances where
  type Rs ListContainerInstances = ListContainerInstancesResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListContainerInstancesResponse'
            Lude.<$> (x Lude..?> "containerInstanceArns" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListContainerInstances where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.ListContainerInstances" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListContainerInstances where
  toJSON ListContainerInstances' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("status" Lude..=) Lude.<$> status,
            ("cluster" Lude..=) Lude.<$> cluster,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("filter" Lude..=) Lude.<$> filter,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListContainerInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery ListContainerInstances where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListContainerInstancesResponse' smart constructor.
data ListContainerInstancesResponse = ListContainerInstancesResponse'
  { -- | The list of container instances with full ARN entries for each container instance associated with the specified cluster.
    containerInstanceARNs :: Lude.Maybe [Lude.Text],
    -- | The @nextToken@ value to include in a future @ListContainerInstances@ request. When the results of a @ListContainerInstances@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListContainerInstancesResponse' with the minimum fields required to make a request.
--
-- * 'containerInstanceARNs' - The list of container instances with full ARN entries for each container instance associated with the specified cluster.
-- * 'nextToken' - The @nextToken@ value to include in a future @ListContainerInstances@ request. When the results of a @ListContainerInstances@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkListContainerInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListContainerInstancesResponse
mkListContainerInstancesResponse pResponseStatus_ =
  ListContainerInstancesResponse'
    { containerInstanceARNs =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of container instances with full ARN entries for each container instance associated with the specified cluster.
--
-- /Note:/ Consider using 'containerInstanceARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcirsContainerInstanceARNs :: Lens.Lens' ListContainerInstancesResponse (Lude.Maybe [Lude.Text])
lcirsContainerInstanceARNs = Lens.lens (containerInstanceARNs :: ListContainerInstancesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {containerInstanceARNs = a} :: ListContainerInstancesResponse)
{-# DEPRECATED lcirsContainerInstanceARNs "Use generic-lens or generic-optics with 'containerInstanceARNs' instead." #-}

-- | The @nextToken@ value to include in a future @ListContainerInstances@ request. When the results of a @ListContainerInstances@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcirsNextToken :: Lens.Lens' ListContainerInstancesResponse (Lude.Maybe Lude.Text)
lcirsNextToken = Lens.lens (nextToken :: ListContainerInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListContainerInstancesResponse)
{-# DEPRECATED lcirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcirsResponseStatus :: Lens.Lens' ListContainerInstancesResponse Lude.Int
lcirsResponseStatus = Lens.lens (responseStatus :: ListContainerInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListContainerInstancesResponse)
{-# DEPRECATED lcirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
