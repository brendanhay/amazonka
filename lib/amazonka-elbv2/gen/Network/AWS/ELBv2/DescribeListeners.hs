{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DescribeListeners
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified listeners or the listeners for the specified Application Load Balancer, Network Load Balancer, or Gateway Load Balancer. You must specify either a load balancer or one or more listeners.
--
-- This operation returns paginated results.
module Network.AWS.ELBv2.DescribeListeners
  ( -- * Creating a request
    DescribeListeners (..),
    mkDescribeListeners,

    -- ** Request lenses
    dlListenerARNs,
    dlLoadBalancerARN,
    dlMarker,
    dlPageSize,

    -- * Destructuring the response
    DescribeListenersResponse (..),
    mkDescribeListenersResponse,

    -- ** Response lenses
    dlrsNextMarker,
    dlrsListeners,
    dlrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeListeners' smart constructor.
data DescribeListeners = DescribeListeners'
  { -- | The Amazon Resource Names (ARN) of the listeners.
    listenerARNs :: Lude.Maybe [Lude.Text],
    -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerARN :: Lude.Maybe Lude.Text,
    -- | The marker for the next set of results. (You received this marker from a previous call.)
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return with this call.
    pageSize :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeListeners' with the minimum fields required to make a request.
--
-- * 'listenerARNs' - The Amazon Resource Names (ARN) of the listeners.
-- * 'loadBalancerARN' - The Amazon Resource Name (ARN) of the load balancer.
-- * 'marker' - The marker for the next set of results. (You received this marker from a previous call.)
-- * 'pageSize' - The maximum number of results to return with this call.
mkDescribeListeners ::
  DescribeListeners
mkDescribeListeners =
  DescribeListeners'
    { listenerARNs = Lude.Nothing,
      loadBalancerARN = Lude.Nothing,
      marker = Lude.Nothing,
      pageSize = Lude.Nothing
    }

-- | The Amazon Resource Names (ARN) of the listeners.
--
-- /Note:/ Consider using 'listenerARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlListenerARNs :: Lens.Lens' DescribeListeners (Lude.Maybe [Lude.Text])
dlListenerARNs = Lens.lens (listenerARNs :: DescribeListeners -> Lude.Maybe [Lude.Text]) (\s a -> s {listenerARNs = a} :: DescribeListeners)
{-# DEPRECATED dlListenerARNs "Use generic-lens or generic-optics with 'listenerARNs' instead." #-}

-- | The Amazon Resource Name (ARN) of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlLoadBalancerARN :: Lens.Lens' DescribeListeners (Lude.Maybe Lude.Text)
dlLoadBalancerARN = Lens.lens (loadBalancerARN :: DescribeListeners -> Lude.Maybe Lude.Text) (\s a -> s {loadBalancerARN = a} :: DescribeListeners)
{-# DEPRECATED dlLoadBalancerARN "Use generic-lens or generic-optics with 'loadBalancerARN' instead." #-}

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlMarker :: Lens.Lens' DescribeListeners (Lude.Maybe Lude.Text)
dlMarker = Lens.lens (marker :: DescribeListeners -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeListeners)
{-# DEPRECATED dlMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of results to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlPageSize :: Lens.Lens' DescribeListeners (Lude.Maybe Lude.Natural)
dlPageSize = Lens.lens (pageSize :: DescribeListeners -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: DescribeListeners)
{-# DEPRECATED dlPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Page.AWSPager DescribeListeners where
  page rq rs
    | Page.stop (rs Lens.^. dlrsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dlrsListeners) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dlMarker Lens..~ rs Lens.^. dlrsNextMarker

instance Lude.AWSRequest DescribeListeners where
  type Rs DescribeListeners = DescribeListenersResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "DescribeListenersResult"
      ( \s h x ->
          DescribeListenersResponse'
            Lude.<$> (x Lude..@? "NextMarker")
            Lude.<*> ( x Lude..@? "Listeners" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeListeners where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeListeners where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeListeners where
  toQuery DescribeListeners' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeListeners" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "ListenerArns"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> listenerARNs),
        "LoadBalancerArn" Lude.=: loadBalancerARN,
        "Marker" Lude.=: marker,
        "PageSize" Lude.=: pageSize
      ]

-- | /See:/ 'mkDescribeListenersResponse' smart constructor.
data DescribeListenersResponse = DescribeListenersResponse'
  { -- | If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | Information about the listeners.
    listeners :: Lude.Maybe [Listener],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeListenersResponse' with the minimum fields required to make a request.
--
-- * 'nextMarker' - If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
-- * 'listeners' - Information about the listeners.
-- * 'responseStatus' - The response status code.
mkDescribeListenersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeListenersResponse
mkDescribeListenersResponse pResponseStatus_ =
  DescribeListenersResponse'
    { nextMarker = Lude.Nothing,
      listeners = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrsNextMarker :: Lens.Lens' DescribeListenersResponse (Lude.Maybe Lude.Text)
dlrsNextMarker = Lens.lens (nextMarker :: DescribeListenersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: DescribeListenersResponse)
{-# DEPRECATED dlrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | Information about the listeners.
--
-- /Note:/ Consider using 'listeners' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrsListeners :: Lens.Lens' DescribeListenersResponse (Lude.Maybe [Listener])
dlrsListeners = Lens.lens (listeners :: DescribeListenersResponse -> Lude.Maybe [Listener]) (\s a -> s {listeners = a} :: DescribeListenersResponse)
{-# DEPRECATED dlrsListeners "Use generic-lens or generic-optics with 'listeners' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrsResponseStatus :: Lens.Lens' DescribeListenersResponse Lude.Int
dlrsResponseStatus = Lens.lens (responseStatus :: DescribeListenersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeListenersResponse)
{-# DEPRECATED dlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
