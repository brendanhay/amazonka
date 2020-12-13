{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeTrafficMirrorSessions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Traffic Mirror sessions. By default, all Traffic Mirror sessions are described. Alternatively, you can filter the results.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTrafficMirrorSessions
  ( -- * Creating a request
    DescribeTrafficMirrorSessions (..),
    mkDescribeTrafficMirrorSessions,

    -- ** Request lenses
    dtmsFilters,
    dtmsNextToken,
    dtmsTrafficMirrorSessionIds,
    dtmsDryRun,
    dtmsMaxResults,

    -- * Destructuring the response
    DescribeTrafficMirrorSessionsResponse (..),
    mkDescribeTrafficMirrorSessionsResponse,

    -- ** Response lenses
    dtmssrsNextToken,
    dtmssrsTrafficMirrorSessions,
    dtmssrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeTrafficMirrorSessions' smart constructor.
data DescribeTrafficMirrorSessions = DescribeTrafficMirrorSessions'
  { -- | One or more filters. The possible values are:
    --
    --
    --     * @description@ : The Traffic Mirror session description.
    --
    --
    --     * @network-interface-id@ : The ID of the Traffic Mirror session network interface.
    --
    --
    --     * @owner-id@ : The ID of the account that owns the Traffic Mirror session.
    --
    --
    --     * @packet-length@ : The assigned number of packets to mirror.
    --
    --
    --     * @session-number@ : The assigned session number.
    --
    --
    --     * @traffic-mirror-filter-id@ : The ID of the Traffic Mirror filter.
    --
    --
    --     * @traffic-mirror-session-id@ : The ID of the Traffic Mirror session.
    --
    --
    --     * @traffic-mirror-target-id@ : The ID of the Traffic Mirror target.
    --
    --
    --     * @virtual-network-id@ : The virtual network ID of the Traffic Mirror session.
    filters :: Lude.Maybe [Filter],
    -- | The token for the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The ID of the Traffic Mirror session.
    trafficMirrorSessionIds :: Lude.Maybe [Lude.Text],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTrafficMirrorSessions' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters. The possible values are:
--
--
--     * @description@ : The Traffic Mirror session description.
--
--
--     * @network-interface-id@ : The ID of the Traffic Mirror session network interface.
--
--
--     * @owner-id@ : The ID of the account that owns the Traffic Mirror session.
--
--
--     * @packet-length@ : The assigned number of packets to mirror.
--
--
--     * @session-number@ : The assigned session number.
--
--
--     * @traffic-mirror-filter-id@ : The ID of the Traffic Mirror filter.
--
--
--     * @traffic-mirror-session-id@ : The ID of the Traffic Mirror session.
--
--
--     * @traffic-mirror-target-id@ : The ID of the Traffic Mirror target.
--
--
--     * @virtual-network-id@ : The virtual network ID of the Traffic Mirror session.
--
--
-- * 'nextToken' - The token for the next page of results.
-- * 'trafficMirrorSessionIds' - The ID of the Traffic Mirror session.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
mkDescribeTrafficMirrorSessions ::
  DescribeTrafficMirrorSessions
mkDescribeTrafficMirrorSessions =
  DescribeTrafficMirrorSessions'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      trafficMirrorSessionIds = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters. The possible values are:
--
--
--     * @description@ : The Traffic Mirror session description.
--
--
--     * @network-interface-id@ : The ID of the Traffic Mirror session network interface.
--
--
--     * @owner-id@ : The ID of the account that owns the Traffic Mirror session.
--
--
--     * @packet-length@ : The assigned number of packets to mirror.
--
--
--     * @session-number@ : The assigned session number.
--
--
--     * @traffic-mirror-filter-id@ : The ID of the Traffic Mirror filter.
--
--
--     * @traffic-mirror-session-id@ : The ID of the Traffic Mirror session.
--
--
--     * @traffic-mirror-target-id@ : The ID of the Traffic Mirror target.
--
--
--     * @virtual-network-id@ : The virtual network ID of the Traffic Mirror session.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmsFilters :: Lens.Lens' DescribeTrafficMirrorSessions (Lude.Maybe [Filter])
dtmsFilters = Lens.lens (filters :: DescribeTrafficMirrorSessions -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeTrafficMirrorSessions)
{-# DEPRECATED dtmsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmsNextToken :: Lens.Lens' DescribeTrafficMirrorSessions (Lude.Maybe Lude.Text)
dtmsNextToken = Lens.lens (nextToken :: DescribeTrafficMirrorSessions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTrafficMirrorSessions)
{-# DEPRECATED dtmsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the Traffic Mirror session.
--
-- /Note:/ Consider using 'trafficMirrorSessionIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmsTrafficMirrorSessionIds :: Lens.Lens' DescribeTrafficMirrorSessions (Lude.Maybe [Lude.Text])
dtmsTrafficMirrorSessionIds = Lens.lens (trafficMirrorSessionIds :: DescribeTrafficMirrorSessions -> Lude.Maybe [Lude.Text]) (\s a -> s {trafficMirrorSessionIds = a} :: DescribeTrafficMirrorSessions)
{-# DEPRECATED dtmsTrafficMirrorSessionIds "Use generic-lens or generic-optics with 'trafficMirrorSessionIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmsDryRun :: Lens.Lens' DescribeTrafficMirrorSessions (Lude.Maybe Lude.Bool)
dtmsDryRun = Lens.lens (dryRun :: DescribeTrafficMirrorSessions -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeTrafficMirrorSessions)
{-# DEPRECATED dtmsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmsMaxResults :: Lens.Lens' DescribeTrafficMirrorSessions (Lude.Maybe Lude.Natural)
dtmsMaxResults = Lens.lens (maxResults :: DescribeTrafficMirrorSessions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeTrafficMirrorSessions)
{-# DEPRECATED dtmsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeTrafficMirrorSessions where
  page rq rs
    | Page.stop (rs Lens.^. dtmssrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dtmssrsTrafficMirrorSessions) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dtmsNextToken Lens..~ rs Lens.^. dtmssrsNextToken

instance Lude.AWSRequest DescribeTrafficMirrorSessions where
  type
    Rs DescribeTrafficMirrorSessions =
      DescribeTrafficMirrorSessionsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeTrafficMirrorSessionsResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "trafficMirrorSessionSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTrafficMirrorSessions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeTrafficMirrorSessions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTrafficMirrorSessions where
  toQuery DescribeTrafficMirrorSessions' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeTrafficMirrorSessions" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        Lude.toQuery
          ( Lude.toQueryList "TrafficMirrorSessionId"
              Lude.<$> trafficMirrorSessionIds
          ),
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeTrafficMirrorSessionsResponse' smart constructor.
data DescribeTrafficMirrorSessionsResponse = DescribeTrafficMirrorSessionsResponse'
  { -- | The token to use to retrieve the next page of results. The value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Describes one or more Traffic Mirror sessions. By default, all Traffic Mirror sessions are described. Alternatively, you can filter the results.
    trafficMirrorSessions :: Lude.Maybe [TrafficMirrorSession],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTrafficMirrorSessionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. The value is @null@ when there are no more results to return.
-- * 'trafficMirrorSessions' - Describes one or more Traffic Mirror sessions. By default, all Traffic Mirror sessions are described. Alternatively, you can filter the results.
-- * 'responseStatus' - The response status code.
mkDescribeTrafficMirrorSessionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTrafficMirrorSessionsResponse
mkDescribeTrafficMirrorSessionsResponse pResponseStatus_ =
  DescribeTrafficMirrorSessionsResponse'
    { nextToken = Lude.Nothing,
      trafficMirrorSessions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. The value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmssrsNextToken :: Lens.Lens' DescribeTrafficMirrorSessionsResponse (Lude.Maybe Lude.Text)
dtmssrsNextToken = Lens.lens (nextToken :: DescribeTrafficMirrorSessionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTrafficMirrorSessionsResponse)
{-# DEPRECATED dtmssrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Describes one or more Traffic Mirror sessions. By default, all Traffic Mirror sessions are described. Alternatively, you can filter the results.
--
-- /Note:/ Consider using 'trafficMirrorSessions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmssrsTrafficMirrorSessions :: Lens.Lens' DescribeTrafficMirrorSessionsResponse (Lude.Maybe [TrafficMirrorSession])
dtmssrsTrafficMirrorSessions = Lens.lens (trafficMirrorSessions :: DescribeTrafficMirrorSessionsResponse -> Lude.Maybe [TrafficMirrorSession]) (\s a -> s {trafficMirrorSessions = a} :: DescribeTrafficMirrorSessionsResponse)
{-# DEPRECATED dtmssrsTrafficMirrorSessions "Use generic-lens or generic-optics with 'trafficMirrorSessions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmssrsResponseStatus :: Lens.Lens' DescribeTrafficMirrorSessionsResponse Lude.Int
dtmssrsResponseStatus = Lens.lens (responseStatus :: DescribeTrafficMirrorSessionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTrafficMirrorSessionsResponse)
{-# DEPRECATED dtmssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
