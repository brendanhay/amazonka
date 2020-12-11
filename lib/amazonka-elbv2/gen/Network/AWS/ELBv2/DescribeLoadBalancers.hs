{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DescribeLoadBalancers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified load balancers or all of your load balancers.
--
-- This operation returns paginated results.
module Network.AWS.ELBv2.DescribeLoadBalancers
  ( -- * Creating a request
    DescribeLoadBalancers (..),
    mkDescribeLoadBalancers,

    -- ** Request lenses
    dlbNames,
    dlbLoadBalancerARNs,
    dlbMarker,
    dlbPageSize,

    -- * Destructuring the response
    DescribeLoadBalancersResponse (..),
    mkDescribeLoadBalancersResponse,

    -- ** Response lenses
    dlbrsLoadBalancers,
    dlbrsNextMarker,
    dlbrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeLoadBalancers' smart constructor.
data DescribeLoadBalancers = DescribeLoadBalancers'
  { names ::
      Lude.Maybe [Lude.Text],
    loadBalancerARNs :: Lude.Maybe [Lude.Text],
    marker :: Lude.Maybe Lude.Text,
    pageSize :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLoadBalancers' with the minimum fields required to make a request.
--
-- * 'loadBalancerARNs' - The Amazon Resource Names (ARN) of the load balancers. You can specify up to 20 load balancers in a single call.
-- * 'marker' - The marker for the next set of results. (You received this marker from a previous call.)
-- * 'names' - The names of the load balancers.
-- * 'pageSize' - The maximum number of results to return with this call.
mkDescribeLoadBalancers ::
  DescribeLoadBalancers
mkDescribeLoadBalancers =
  DescribeLoadBalancers'
    { names = Lude.Nothing,
      loadBalancerARNs = Lude.Nothing,
      marker = Lude.Nothing,
      pageSize = Lude.Nothing
    }

-- | The names of the load balancers.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbNames :: Lens.Lens' DescribeLoadBalancers (Lude.Maybe [Lude.Text])
dlbNames = Lens.lens (names :: DescribeLoadBalancers -> Lude.Maybe [Lude.Text]) (\s a -> s {names = a} :: DescribeLoadBalancers)
{-# DEPRECATED dlbNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | The Amazon Resource Names (ARN) of the load balancers. You can specify up to 20 load balancers in a single call.
--
-- /Note:/ Consider using 'loadBalancerARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbLoadBalancerARNs :: Lens.Lens' DescribeLoadBalancers (Lude.Maybe [Lude.Text])
dlbLoadBalancerARNs = Lens.lens (loadBalancerARNs :: DescribeLoadBalancers -> Lude.Maybe [Lude.Text]) (\s a -> s {loadBalancerARNs = a} :: DescribeLoadBalancers)
{-# DEPRECATED dlbLoadBalancerARNs "Use generic-lens or generic-optics with 'loadBalancerARNs' instead." #-}

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbMarker :: Lens.Lens' DescribeLoadBalancers (Lude.Maybe Lude.Text)
dlbMarker = Lens.lens (marker :: DescribeLoadBalancers -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeLoadBalancers)
{-# DEPRECATED dlbMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of results to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbPageSize :: Lens.Lens' DescribeLoadBalancers (Lude.Maybe Lude.Natural)
dlbPageSize = Lens.lens (pageSize :: DescribeLoadBalancers -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: DescribeLoadBalancers)
{-# DEPRECATED dlbPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Page.AWSPager DescribeLoadBalancers where
  page rq rs
    | Page.stop (rs Lens.^. dlbrsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dlbrsLoadBalancers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dlbMarker Lens..~ rs Lens.^. dlbrsNextMarker

instance Lude.AWSRequest DescribeLoadBalancers where
  type Rs DescribeLoadBalancers = DescribeLoadBalancersResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "DescribeLoadBalancersResult"
      ( \s h x ->
          DescribeLoadBalancersResponse'
            Lude.<$> ( x Lude..@? "LoadBalancers" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLoadBalancers where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeLoadBalancers where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLoadBalancers where
  toQuery DescribeLoadBalancers' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeLoadBalancers" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "Names"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> names),
        "LoadBalancerArns"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> loadBalancerARNs),
        "Marker" Lude.=: marker,
        "PageSize" Lude.=: pageSize
      ]

-- | /See:/ 'mkDescribeLoadBalancersResponse' smart constructor.
data DescribeLoadBalancersResponse = DescribeLoadBalancersResponse'
  { loadBalancers ::
      Lude.Maybe [LoadBalancer],
    nextMarker ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeLoadBalancersResponse' with the minimum fields required to make a request.
--
-- * 'loadBalancers' - Information about the load balancers.
-- * 'nextMarker' - If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
-- * 'responseStatus' - The response status code.
mkDescribeLoadBalancersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLoadBalancersResponse
mkDescribeLoadBalancersResponse pResponseStatus_ =
  DescribeLoadBalancersResponse'
    { loadBalancers = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the load balancers.
--
-- /Note:/ Consider using 'loadBalancers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbrsLoadBalancers :: Lens.Lens' DescribeLoadBalancersResponse (Lude.Maybe [LoadBalancer])
dlbrsLoadBalancers = Lens.lens (loadBalancers :: DescribeLoadBalancersResponse -> Lude.Maybe [LoadBalancer]) (\s a -> s {loadBalancers = a} :: DescribeLoadBalancersResponse)
{-# DEPRECATED dlbrsLoadBalancers "Use generic-lens or generic-optics with 'loadBalancers' instead." #-}

-- | If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbrsNextMarker :: Lens.Lens' DescribeLoadBalancersResponse (Lude.Maybe Lude.Text)
dlbrsNextMarker = Lens.lens (nextMarker :: DescribeLoadBalancersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: DescribeLoadBalancersResponse)
{-# DEPRECATED dlbrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbrsResponseStatus :: Lens.Lens' DescribeLoadBalancersResponse Lude.Int
dlbrsResponseStatus = Lens.lens (responseStatus :: DescribeLoadBalancersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLoadBalancersResponse)
{-# DEPRECATED dlbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
