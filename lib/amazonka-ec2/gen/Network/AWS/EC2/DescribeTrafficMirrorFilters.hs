{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeTrafficMirrorFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Traffic Mirror filters.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTrafficMirrorFilters
  ( -- * Creating a request
    DescribeTrafficMirrorFilters (..),
    mkDescribeTrafficMirrorFilters,

    -- ** Request lenses
    dtmfTrafficMirrorFilterIds,
    dtmfFilters,
    dtmfNextToken,
    dtmfDryRun,
    dtmfMaxResults,

    -- * Destructuring the response
    DescribeTrafficMirrorFiltersResponse (..),
    mkDescribeTrafficMirrorFiltersResponse,

    -- ** Response lenses
    dtmfsrsTrafficMirrorFilters,
    dtmfsrsNextToken,
    dtmfsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeTrafficMirrorFilters' smart constructor.
data DescribeTrafficMirrorFilters = DescribeTrafficMirrorFilters'
  { trafficMirrorFilterIds ::
      Lude.Maybe [Lude.Text],
    filters :: Lude.Maybe [Filter],
    nextToken :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    maxResults ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTrafficMirrorFilters' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters. The possible values are:
--
--
--     * @description@ : The Traffic Mirror filter description.
--
--
--     * @traffic-mirror-filter-id@ : The ID of the Traffic Mirror filter.
--
--
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
-- * 'nextToken' - The token for the next page of results.
-- * 'trafficMirrorFilterIds' - The ID of the Traffic Mirror filter.
mkDescribeTrafficMirrorFilters ::
  DescribeTrafficMirrorFilters
mkDescribeTrafficMirrorFilters =
  DescribeTrafficMirrorFilters'
    { trafficMirrorFilterIds =
        Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The ID of the Traffic Mirror filter.
--
-- /Note:/ Consider using 'trafficMirrorFilterIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfTrafficMirrorFilterIds :: Lens.Lens' DescribeTrafficMirrorFilters (Lude.Maybe [Lude.Text])
dtmfTrafficMirrorFilterIds = Lens.lens (trafficMirrorFilterIds :: DescribeTrafficMirrorFilters -> Lude.Maybe [Lude.Text]) (\s a -> s {trafficMirrorFilterIds = a} :: DescribeTrafficMirrorFilters)
{-# DEPRECATED dtmfTrafficMirrorFilterIds "Use generic-lens or generic-optics with 'trafficMirrorFilterIds' instead." #-}

-- | One or more filters. The possible values are:
--
--
--     * @description@ : The Traffic Mirror filter description.
--
--
--     * @traffic-mirror-filter-id@ : The ID of the Traffic Mirror filter.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfFilters :: Lens.Lens' DescribeTrafficMirrorFilters (Lude.Maybe [Filter])
dtmfFilters = Lens.lens (filters :: DescribeTrafficMirrorFilters -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeTrafficMirrorFilters)
{-# DEPRECATED dtmfFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfNextToken :: Lens.Lens' DescribeTrafficMirrorFilters (Lude.Maybe Lude.Text)
dtmfNextToken = Lens.lens (nextToken :: DescribeTrafficMirrorFilters -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTrafficMirrorFilters)
{-# DEPRECATED dtmfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfDryRun :: Lens.Lens' DescribeTrafficMirrorFilters (Lude.Maybe Lude.Bool)
dtmfDryRun = Lens.lens (dryRun :: DescribeTrafficMirrorFilters -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeTrafficMirrorFilters)
{-# DEPRECATED dtmfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfMaxResults :: Lens.Lens' DescribeTrafficMirrorFilters (Lude.Maybe Lude.Natural)
dtmfMaxResults = Lens.lens (maxResults :: DescribeTrafficMirrorFilters -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeTrafficMirrorFilters)
{-# DEPRECATED dtmfMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeTrafficMirrorFilters where
  page rq rs
    | Page.stop (rs Lens.^. dtmfsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dtmfsrsTrafficMirrorFilters) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dtmfNextToken Lens..~ rs Lens.^. dtmfsrsNextToken

instance Lude.AWSRequest DescribeTrafficMirrorFilters where
  type
    Rs DescribeTrafficMirrorFilters =
      DescribeTrafficMirrorFiltersResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeTrafficMirrorFiltersResponse'
            Lude.<$> ( x Lude..@? "trafficMirrorFilterSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTrafficMirrorFilters where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeTrafficMirrorFilters where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTrafficMirrorFilters where
  toQuery DescribeTrafficMirrorFilters' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeTrafficMirrorFilters" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          ( Lude.toQueryList "TrafficMirrorFilterId"
              Lude.<$> trafficMirrorFilterIds
          ),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeTrafficMirrorFiltersResponse' smart constructor.
data DescribeTrafficMirrorFiltersResponse = DescribeTrafficMirrorFiltersResponse'
  { trafficMirrorFilters ::
      Lude.Maybe
        [TrafficMirrorFilter],
    nextToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTrafficMirrorFiltersResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. The value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
-- * 'trafficMirrorFilters' - Information about one or more Traffic Mirror filters.
mkDescribeTrafficMirrorFiltersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTrafficMirrorFiltersResponse
mkDescribeTrafficMirrorFiltersResponse pResponseStatus_ =
  DescribeTrafficMirrorFiltersResponse'
    { trafficMirrorFilters =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about one or more Traffic Mirror filters.
--
-- /Note:/ Consider using 'trafficMirrorFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfsrsTrafficMirrorFilters :: Lens.Lens' DescribeTrafficMirrorFiltersResponse (Lude.Maybe [TrafficMirrorFilter])
dtmfsrsTrafficMirrorFilters = Lens.lens (trafficMirrorFilters :: DescribeTrafficMirrorFiltersResponse -> Lude.Maybe [TrafficMirrorFilter]) (\s a -> s {trafficMirrorFilters = a} :: DescribeTrafficMirrorFiltersResponse)
{-# DEPRECATED dtmfsrsTrafficMirrorFilters "Use generic-lens or generic-optics with 'trafficMirrorFilters' instead." #-}

-- | The token to use to retrieve the next page of results. The value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfsrsNextToken :: Lens.Lens' DescribeTrafficMirrorFiltersResponse (Lude.Maybe Lude.Text)
dtmfsrsNextToken = Lens.lens (nextToken :: DescribeTrafficMirrorFiltersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTrafficMirrorFiltersResponse)
{-# DEPRECATED dtmfsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfsrsResponseStatus :: Lens.Lens' DescribeTrafficMirrorFiltersResponse Lude.Int
dtmfsrsResponseStatus = Lens.lens (responseStatus :: DescribeTrafficMirrorFiltersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTrafficMirrorFiltersResponse)
{-# DEPRECATED dtmfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
