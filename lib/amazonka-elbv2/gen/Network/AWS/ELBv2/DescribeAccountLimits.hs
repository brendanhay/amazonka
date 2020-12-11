{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DescribeAccountLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the current Elastic Load Balancing resource limits for your AWS account.
--
-- For more information, see the following:
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-limits.html Quotas for your Application Load Balancers>
--
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/load-balancer-limits.html Quotas for your Network Load Balancers>
--
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/gateway/quotas-limits.html Quotas for your Gateway Load Balancers>
--
--
--
-- This operation returns paginated results.
module Network.AWS.ELBv2.DescribeAccountLimits
  ( -- * Creating a request
    DescribeAccountLimits (..),
    mkDescribeAccountLimits,

    -- ** Request lenses
    dalMarker,
    dalPageSize,

    -- * Destructuring the response
    DescribeAccountLimitsResponse (..),
    mkDescribeAccountLimitsResponse,

    -- ** Response lenses
    dalrsLimits,
    dalrsNextMarker,
    dalrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAccountLimits' smart constructor.
data DescribeAccountLimits = DescribeAccountLimits'
  { marker ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeAccountLimits' with the minimum fields required to make a request.
--
-- * 'marker' - The marker for the next set of results. (You received this marker from a previous call.)
-- * 'pageSize' - The maximum number of results to return with this call.
mkDescribeAccountLimits ::
  DescribeAccountLimits
mkDescribeAccountLimits =
  DescribeAccountLimits'
    { marker = Lude.Nothing,
      pageSize = Lude.Nothing
    }

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalMarker :: Lens.Lens' DescribeAccountLimits (Lude.Maybe Lude.Text)
dalMarker = Lens.lens (marker :: DescribeAccountLimits -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeAccountLimits)
{-# DEPRECATED dalMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of results to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalPageSize :: Lens.Lens' DescribeAccountLimits (Lude.Maybe Lude.Natural)
dalPageSize = Lens.lens (pageSize :: DescribeAccountLimits -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: DescribeAccountLimits)
{-# DEPRECATED dalPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Page.AWSPager DescribeAccountLimits where
  page rq rs
    | Page.stop (rs Lens.^. dalrsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dalrsLimits) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dalMarker Lens..~ rs Lens.^. dalrsNextMarker

instance Lude.AWSRequest DescribeAccountLimits where
  type Rs DescribeAccountLimits = DescribeAccountLimitsResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "DescribeAccountLimitsResult"
      ( \s h x ->
          DescribeAccountLimitsResponse'
            Lude.<$> ( x Lude..@? "Limits" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAccountLimits where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeAccountLimits where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAccountLimits where
  toQuery DescribeAccountLimits' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeAccountLimits" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "Marker" Lude.=: marker,
        "PageSize" Lude.=: pageSize
      ]

-- | /See:/ 'mkDescribeAccountLimitsResponse' smart constructor.
data DescribeAccountLimitsResponse = DescribeAccountLimitsResponse'
  { limits ::
      Lude.Maybe [Limit],
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

-- | Creates a value of 'DescribeAccountLimitsResponse' with the minimum fields required to make a request.
--
-- * 'limits' - Information about the limits.
-- * 'nextMarker' - If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
-- * 'responseStatus' - The response status code.
mkDescribeAccountLimitsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAccountLimitsResponse
mkDescribeAccountLimitsResponse pResponseStatus_ =
  DescribeAccountLimitsResponse'
    { limits = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the limits.
--
-- /Note:/ Consider using 'limits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalrsLimits :: Lens.Lens' DescribeAccountLimitsResponse (Lude.Maybe [Limit])
dalrsLimits = Lens.lens (limits :: DescribeAccountLimitsResponse -> Lude.Maybe [Limit]) (\s a -> s {limits = a} :: DescribeAccountLimitsResponse)
{-# DEPRECATED dalrsLimits "Use generic-lens or generic-optics with 'limits' instead." #-}

-- | If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalrsNextMarker :: Lens.Lens' DescribeAccountLimitsResponse (Lude.Maybe Lude.Text)
dalrsNextMarker = Lens.lens (nextMarker :: DescribeAccountLimitsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: DescribeAccountLimitsResponse)
{-# DEPRECATED dalrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalrsResponseStatus :: Lens.Lens' DescribeAccountLimitsResponse Lude.Int
dalrsResponseStatus = Lens.lens (responseStatus :: DescribeAccountLimitsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAccountLimitsResponse)
{-# DEPRECATED dalrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
