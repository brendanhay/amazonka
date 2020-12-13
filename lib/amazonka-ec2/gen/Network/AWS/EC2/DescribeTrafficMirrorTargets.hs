{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeTrafficMirrorTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Information about one or more Traffic Mirror targets.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTrafficMirrorTargets
  ( -- * Creating a request
    DescribeTrafficMirrorTargets (..),
    mkDescribeTrafficMirrorTargets,

    -- ** Request lenses
    dtmtFilters,
    dtmtNextToken,
    dtmtTrafficMirrorTargetIds,
    dtmtDryRun,
    dtmtMaxResults,

    -- * Destructuring the response
    DescribeTrafficMirrorTargetsResponse (..),
    mkDescribeTrafficMirrorTargetsResponse,

    -- ** Response lenses
    dtmtrsTrafficMirrorTargets,
    dtmtrsNextToken,
    dtmtrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeTrafficMirrorTargets' smart constructor.
data DescribeTrafficMirrorTargets = DescribeTrafficMirrorTargets'
  { -- | One or more filters. The possible values are:
    --
    --
    --     * @description@ : The Traffic Mirror target description.
    --
    --
    --     * @network-interface-id@ : The ID of the Traffic Mirror session network interface.
    --
    --
    --     * @network-load-balancer-arn@ : The Amazon Resource Name (ARN) of the Network Load Balancer that is associated with the session.
    --
    --
    --     * @owner-id@ : The ID of the account that owns the Traffic Mirror session.
    --
    --
    --     * @traffic-mirror-target-id@ : The ID of the Traffic Mirror target.
    filters :: Lude.Maybe [Filter],
    -- | The token for the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The ID of the Traffic Mirror targets.
    trafficMirrorTargetIds :: Lude.Maybe [Lude.Text],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTrafficMirrorTargets' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters. The possible values are:
--
--
--     * @description@ : The Traffic Mirror target description.
--
--
--     * @network-interface-id@ : The ID of the Traffic Mirror session network interface.
--
--
--     * @network-load-balancer-arn@ : The Amazon Resource Name (ARN) of the Network Load Balancer that is associated with the session.
--
--
--     * @owner-id@ : The ID of the account that owns the Traffic Mirror session.
--
--
--     * @traffic-mirror-target-id@ : The ID of the Traffic Mirror target.
--
--
-- * 'nextToken' - The token for the next page of results.
-- * 'trafficMirrorTargetIds' - The ID of the Traffic Mirror targets.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
mkDescribeTrafficMirrorTargets ::
  DescribeTrafficMirrorTargets
mkDescribeTrafficMirrorTargets =
  DescribeTrafficMirrorTargets'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      trafficMirrorTargetIds = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters. The possible values are:
--
--
--     * @description@ : The Traffic Mirror target description.
--
--
--     * @network-interface-id@ : The ID of the Traffic Mirror session network interface.
--
--
--     * @network-load-balancer-arn@ : The Amazon Resource Name (ARN) of the Network Load Balancer that is associated with the session.
--
--
--     * @owner-id@ : The ID of the account that owns the Traffic Mirror session.
--
--
--     * @traffic-mirror-target-id@ : The ID of the Traffic Mirror target.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmtFilters :: Lens.Lens' DescribeTrafficMirrorTargets (Lude.Maybe [Filter])
dtmtFilters = Lens.lens (filters :: DescribeTrafficMirrorTargets -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeTrafficMirrorTargets)
{-# DEPRECATED dtmtFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmtNextToken :: Lens.Lens' DescribeTrafficMirrorTargets (Lude.Maybe Lude.Text)
dtmtNextToken = Lens.lens (nextToken :: DescribeTrafficMirrorTargets -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTrafficMirrorTargets)
{-# DEPRECATED dtmtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the Traffic Mirror targets.
--
-- /Note:/ Consider using 'trafficMirrorTargetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmtTrafficMirrorTargetIds :: Lens.Lens' DescribeTrafficMirrorTargets (Lude.Maybe [Lude.Text])
dtmtTrafficMirrorTargetIds = Lens.lens (trafficMirrorTargetIds :: DescribeTrafficMirrorTargets -> Lude.Maybe [Lude.Text]) (\s a -> s {trafficMirrorTargetIds = a} :: DescribeTrafficMirrorTargets)
{-# DEPRECATED dtmtTrafficMirrorTargetIds "Use generic-lens or generic-optics with 'trafficMirrorTargetIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmtDryRun :: Lens.Lens' DescribeTrafficMirrorTargets (Lude.Maybe Lude.Bool)
dtmtDryRun = Lens.lens (dryRun :: DescribeTrafficMirrorTargets -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeTrafficMirrorTargets)
{-# DEPRECATED dtmtDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmtMaxResults :: Lens.Lens' DescribeTrafficMirrorTargets (Lude.Maybe Lude.Natural)
dtmtMaxResults = Lens.lens (maxResults :: DescribeTrafficMirrorTargets -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeTrafficMirrorTargets)
{-# DEPRECATED dtmtMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeTrafficMirrorTargets where
  page rq rs
    | Page.stop (rs Lens.^. dtmtrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dtmtrsTrafficMirrorTargets) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dtmtNextToken Lens..~ rs Lens.^. dtmtrsNextToken

instance Lude.AWSRequest DescribeTrafficMirrorTargets where
  type
    Rs DescribeTrafficMirrorTargets =
      DescribeTrafficMirrorTargetsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeTrafficMirrorTargetsResponse'
            Lude.<$> ( x Lude..@? "trafficMirrorTargetSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTrafficMirrorTargets where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeTrafficMirrorTargets where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTrafficMirrorTargets where
  toQuery DescribeTrafficMirrorTargets' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeTrafficMirrorTargets" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        Lude.toQuery
          ( Lude.toQueryList "TrafficMirrorTargetId"
              Lude.<$> trafficMirrorTargetIds
          ),
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeTrafficMirrorTargetsResponse' smart constructor.
data DescribeTrafficMirrorTargetsResponse = DescribeTrafficMirrorTargetsResponse'
  { -- | Information about one or more Traffic Mirror targets.
    trafficMirrorTargets :: Lude.Maybe [TrafficMirrorTarget],
    -- | The token to use to retrieve the next page of results. The value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTrafficMirrorTargetsResponse' with the minimum fields required to make a request.
--
-- * 'trafficMirrorTargets' - Information about one or more Traffic Mirror targets.
-- * 'nextToken' - The token to use to retrieve the next page of results. The value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeTrafficMirrorTargetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTrafficMirrorTargetsResponse
mkDescribeTrafficMirrorTargetsResponse pResponseStatus_ =
  DescribeTrafficMirrorTargetsResponse'
    { trafficMirrorTargets =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about one or more Traffic Mirror targets.
--
-- /Note:/ Consider using 'trafficMirrorTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmtrsTrafficMirrorTargets :: Lens.Lens' DescribeTrafficMirrorTargetsResponse (Lude.Maybe [TrafficMirrorTarget])
dtmtrsTrafficMirrorTargets = Lens.lens (trafficMirrorTargets :: DescribeTrafficMirrorTargetsResponse -> Lude.Maybe [TrafficMirrorTarget]) (\s a -> s {trafficMirrorTargets = a} :: DescribeTrafficMirrorTargetsResponse)
{-# DEPRECATED dtmtrsTrafficMirrorTargets "Use generic-lens or generic-optics with 'trafficMirrorTargets' instead." #-}

-- | The token to use to retrieve the next page of results. The value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmtrsNextToken :: Lens.Lens' DescribeTrafficMirrorTargetsResponse (Lude.Maybe Lude.Text)
dtmtrsNextToken = Lens.lens (nextToken :: DescribeTrafficMirrorTargetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTrafficMirrorTargetsResponse)
{-# DEPRECATED dtmtrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmtrsResponseStatus :: Lens.Lens' DescribeTrafficMirrorTargetsResponse Lude.Int
dtmtrsResponseStatus = Lens.lens (responseStatus :: DescribeTrafficMirrorTargetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTrafficMirrorTargetsResponse)
{-# DEPRECATED dtmtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
