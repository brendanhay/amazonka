{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DescribeLoadBalancers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified the load balancers. If no load balancers are specified, the call describes all of your load balancers.
--
-- This operation returns paginated results.
module Network.AWS.ELB.DescribeLoadBalancers
  ( -- * Creating a request
    DescribeLoadBalancers (..),
    mkDescribeLoadBalancers,

    -- ** Request lenses
    dlbMarker,
    dlbPageSize,
    dlbLoadBalancerNames,

    -- * Destructuring the response
    DescribeLoadBalancersResponse (..),
    mkDescribeLoadBalancersResponse,

    -- ** Response lenses
    dlbrsLoadBalancerDescriptions,
    dlbrsNextMarker,
    dlbrsResponseStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribeLoadBalancers.
--
-- /See:/ 'mkDescribeLoadBalancers' smart constructor.
data DescribeLoadBalancers = DescribeLoadBalancers'
  { -- | The marker for the next set of results. (You received this marker from a previous call.)
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return with this call (a number from 1 to 400). The default is 400.
    pageSize :: Lude.Maybe Lude.Natural,
    -- | The names of the load balancers.
    loadBalancerNames :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLoadBalancers' with the minimum fields required to make a request.
--
-- * 'marker' - The marker for the next set of results. (You received this marker from a previous call.)
-- * 'pageSize' - The maximum number of results to return with this call (a number from 1 to 400). The default is 400.
-- * 'loadBalancerNames' - The names of the load balancers.
mkDescribeLoadBalancers ::
  DescribeLoadBalancers
mkDescribeLoadBalancers =
  DescribeLoadBalancers'
    { marker = Lude.Nothing,
      pageSize = Lude.Nothing,
      loadBalancerNames = Lude.Nothing
    }

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbMarker :: Lens.Lens' DescribeLoadBalancers (Lude.Maybe Lude.Text)
dlbMarker = Lens.lens (marker :: DescribeLoadBalancers -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeLoadBalancers)
{-# DEPRECATED dlbMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of results to return with this call (a number from 1 to 400). The default is 400.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbPageSize :: Lens.Lens' DescribeLoadBalancers (Lude.Maybe Lude.Natural)
dlbPageSize = Lens.lens (pageSize :: DescribeLoadBalancers -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: DescribeLoadBalancers)
{-# DEPRECATED dlbPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The names of the load balancers.
--
-- /Note:/ Consider using 'loadBalancerNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbLoadBalancerNames :: Lens.Lens' DescribeLoadBalancers (Lude.Maybe [Lude.Text])
dlbLoadBalancerNames = Lens.lens (loadBalancerNames :: DescribeLoadBalancers -> Lude.Maybe [Lude.Text]) (\s a -> s {loadBalancerNames = a} :: DescribeLoadBalancers)
{-# DEPRECATED dlbLoadBalancerNames "Use generic-lens or generic-optics with 'loadBalancerNames' instead." #-}

instance Page.AWSPager DescribeLoadBalancers where
  page rq rs
    | Page.stop (rs Lens.^. dlbrsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dlbrsLoadBalancerDescriptions) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dlbMarker Lens..~ rs Lens.^. dlbrsNextMarker

instance Lude.AWSRequest DescribeLoadBalancers where
  type Rs DescribeLoadBalancers = DescribeLoadBalancersResponse
  request = Req.postQuery elbService
  response =
    Res.receiveXMLWrapper
      "DescribeLoadBalancersResult"
      ( \s h x ->
          DescribeLoadBalancersResponse'
            Lude.<$> ( x Lude..@? "LoadBalancerDescriptions" Lude..!@ Lude.mempty
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
        "Version" Lude.=: ("2012-06-01" :: Lude.ByteString),
        "Marker" Lude.=: marker,
        "PageSize" Lude.=: pageSize,
        "LoadBalancerNames"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> loadBalancerNames)
      ]

-- | Contains the parameters for DescribeLoadBalancers.
--
-- /See:/ 'mkDescribeLoadBalancersResponse' smart constructor.
data DescribeLoadBalancersResponse = DescribeLoadBalancersResponse'
  { -- | Information about the load balancers.
    loadBalancerDescriptions :: Lude.Maybe [LoadBalancerDescription],
    -- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLoadBalancersResponse' with the minimum fields required to make a request.
--
-- * 'loadBalancerDescriptions' - Information about the load balancers.
-- * 'nextMarker' - The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
-- * 'responseStatus' - The response status code.
mkDescribeLoadBalancersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLoadBalancersResponse
mkDescribeLoadBalancersResponse pResponseStatus_ =
  DescribeLoadBalancersResponse'
    { loadBalancerDescriptions =
        Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the load balancers.
--
-- /Note:/ Consider using 'loadBalancerDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbrsLoadBalancerDescriptions :: Lens.Lens' DescribeLoadBalancersResponse (Lude.Maybe [LoadBalancerDescription])
dlbrsLoadBalancerDescriptions = Lens.lens (loadBalancerDescriptions :: DescribeLoadBalancersResponse -> Lude.Maybe [LoadBalancerDescription]) (\s a -> s {loadBalancerDescriptions = a} :: DescribeLoadBalancersResponse)
{-# DEPRECATED dlbrsLoadBalancerDescriptions "Use generic-lens or generic-optics with 'loadBalancerDescriptions' instead." #-}

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
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
