{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.ListServices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the services that are running in a specified cluster.
--
-- This operation returns paginated results.
module Network.AWS.ECS.ListServices
  ( -- * Creating a request
    ListServices (..),
    mkListServices,

    -- ** Request lenses
    lsCluster,
    lsNextToken,
    lsLaunchType,
    lsSchedulingStrategy,
    lsMaxResults,

    -- * Destructuring the response
    ListServicesResponse (..),
    mkListServicesResponse,

    -- ** Response lenses
    lsrsServiceARNs,
    lsrsNextToken,
    lsrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListServices' smart constructor.
data ListServices = ListServices'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the services to list. If you do not specify a cluster, the default cluster is assumed.
    cluster :: Lude.Maybe Lude.Text,
    -- | The @nextToken@ value returned from a @ListServices@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | The launch type for the services to list.
    launchType :: Lude.Maybe LaunchType,
    -- | The scheduling strategy for services to list.
    schedulingStrategy :: Lude.Maybe SchedulingStrategy,
    -- | The maximum number of service results returned by @ListServices@ in paginated output. When this parameter is used, @ListServices@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListServices@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListServices@ returns up to 10 results and a @nextToken@ value if applicable.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListServices' with the minimum fields required to make a request.
--
-- * 'cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that hosts the services to list. If you do not specify a cluster, the default cluster is assumed.
-- * 'nextToken' - The @nextToken@ value returned from a @ListServices@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
-- * 'launchType' - The launch type for the services to list.
-- * 'schedulingStrategy' - The scheduling strategy for services to list.
-- * 'maxResults' - The maximum number of service results returned by @ListServices@ in paginated output. When this parameter is used, @ListServices@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListServices@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListServices@ returns up to 10 results and a @nextToken@ value if applicable.
mkListServices ::
  ListServices
mkListServices =
  ListServices'
    { cluster = Lude.Nothing,
      nextToken = Lude.Nothing,
      launchType = Lude.Nothing,
      schedulingStrategy = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the services to list. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsCluster :: Lens.Lens' ListServices (Lude.Maybe Lude.Text)
lsCluster = Lens.lens (cluster :: ListServices -> Lude.Maybe Lude.Text) (\s a -> s {cluster = a} :: ListServices)
{-# DEPRECATED lsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The @nextToken@ value returned from a @ListServices@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsNextToken :: Lens.Lens' ListServices (Lude.Maybe Lude.Text)
lsNextToken = Lens.lens (nextToken :: ListServices -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListServices)
{-# DEPRECATED lsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The launch type for the services to list.
--
-- /Note:/ Consider using 'launchType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsLaunchType :: Lens.Lens' ListServices (Lude.Maybe LaunchType)
lsLaunchType = Lens.lens (launchType :: ListServices -> Lude.Maybe LaunchType) (\s a -> s {launchType = a} :: ListServices)
{-# DEPRECATED lsLaunchType "Use generic-lens or generic-optics with 'launchType' instead." #-}

-- | The scheduling strategy for services to list.
--
-- /Note:/ Consider using 'schedulingStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsSchedulingStrategy :: Lens.Lens' ListServices (Lude.Maybe SchedulingStrategy)
lsSchedulingStrategy = Lens.lens (schedulingStrategy :: ListServices -> Lude.Maybe SchedulingStrategy) (\s a -> s {schedulingStrategy = a} :: ListServices)
{-# DEPRECATED lsSchedulingStrategy "Use generic-lens or generic-optics with 'schedulingStrategy' instead." #-}

-- | The maximum number of service results returned by @ListServices@ in paginated output. When this parameter is used, @ListServices@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListServices@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListServices@ returns up to 10 results and a @nextToken@ value if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsMaxResults :: Lens.Lens' ListServices (Lude.Maybe Lude.Int)
lsMaxResults = Lens.lens (maxResults :: ListServices -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListServices)
{-# DEPRECATED lsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListServices where
  page rq rs
    | Page.stop (rs Lens.^. lsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsrsServiceARNs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsNextToken Lens..~ rs Lens.^. lsrsNextToken

instance Lude.AWSRequest ListServices where
  type Rs ListServices = ListServicesResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListServicesResponse'
            Lude.<$> (x Lude..?> "serviceArns" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListServices where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.ListServices" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListServices where
  toJSON ListServices' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("cluster" Lude..=) Lude.<$> cluster,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("launchType" Lude..=) Lude.<$> launchType,
            ("schedulingStrategy" Lude..=) Lude.<$> schedulingStrategy,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListServices where
  toPath = Lude.const "/"

instance Lude.ToQuery ListServices where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListServicesResponse' smart constructor.
data ListServicesResponse = ListServicesResponse'
  { -- | The list of full ARN entries for each service associated with the specified cluster.
    serviceARNs :: Lude.Maybe [Lude.Text],
    -- | The @nextToken@ value to include in a future @ListServices@ request. When the results of a @ListServices@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListServicesResponse' with the minimum fields required to make a request.
--
-- * 'serviceARNs' - The list of full ARN entries for each service associated with the specified cluster.
-- * 'nextToken' - The @nextToken@ value to include in a future @ListServices@ request. When the results of a @ListServices@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkListServicesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListServicesResponse
mkListServicesResponse pResponseStatus_ =
  ListServicesResponse'
    { serviceARNs = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of full ARN entries for each service associated with the specified cluster.
--
-- /Note:/ Consider using 'serviceARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsServiceARNs :: Lens.Lens' ListServicesResponse (Lude.Maybe [Lude.Text])
lsrsServiceARNs = Lens.lens (serviceARNs :: ListServicesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {serviceARNs = a} :: ListServicesResponse)
{-# DEPRECATED lsrsServiceARNs "Use generic-lens or generic-optics with 'serviceARNs' instead." #-}

-- | The @nextToken@ value to include in a future @ListServices@ request. When the results of a @ListServices@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsNextToken :: Lens.Lens' ListServicesResponse (Lude.Maybe Lude.Text)
lsrsNextToken = Lens.lens (nextToken :: ListServicesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListServicesResponse)
{-# DEPRECATED lsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsResponseStatus :: Lens.Lens' ListServicesResponse Lude.Int
lsrsResponseStatus = Lens.lens (responseStatus :: ListServicesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListServicesResponse)
{-# DEPRECATED lsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
