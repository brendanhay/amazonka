{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeFleets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified EC2 Fleets or all of your EC2 Fleets.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeFleets
  ( -- * Creating a request
    DescribeFleets (..),
    mkDescribeFleets,

    -- ** Request lenses
    dfsFilters,
    dfsNextToken,
    dfsFleetIds,
    dfsDryRun,
    dfsMaxResults,

    -- * Destructuring the response
    DescribeFleetsResponse (..),
    mkDescribeFleetsResponse,

    -- ** Response lenses
    dfsrsNextToken,
    dfsrsFleets,
    dfsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeFleets' smart constructor.
data DescribeFleets = DescribeFleets'
  { filters ::
      Lude.Maybe [Filter],
    nextToken :: Lude.Maybe Lude.Text,
    fleetIds :: Lude.Maybe [Lude.Text],
    dryRun :: Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'DescribeFleets' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - The filters.
--
--
--     * @activity-status@ - The progress of the EC2 Fleet ( @error@ | @pending-fulfillment@ | @pending-termination@ | @fulfilled@ ).
--
--
--     * @excess-capacity-termination-policy@ - Indicates whether to terminate running instances if the target capacity is decreased below the current EC2 Fleet size (@true@ | @false@ ).
--
--
--     * @fleet-state@ - The state of the EC2 Fleet (@submitted@ | @active@ | @deleted@ | @failed@ | @deleted-running@ | @deleted-terminating@ | @modifying@ ).
--
--
--     * @replace-unhealthy-instances@ - Indicates whether EC2 Fleet should replace unhealthy instances (@true@ | @false@ ).
--
--
--     * @type@ - The type of request (@instant@ | @request@ | @maintain@ ).
--
--
-- * 'fleetIds' - The ID of the EC2 Fleets.
-- * 'maxResults' - The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
-- * 'nextToken' - The token for the next set of results.
mkDescribeFleets ::
  DescribeFleets
mkDescribeFleets =
  DescribeFleets'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      fleetIds = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The filters.
--
--
--     * @activity-status@ - The progress of the EC2 Fleet ( @error@ | @pending-fulfillment@ | @pending-termination@ | @fulfilled@ ).
--
--
--     * @excess-capacity-termination-policy@ - Indicates whether to terminate running instances if the target capacity is decreased below the current EC2 Fleet size (@true@ | @false@ ).
--
--
--     * @fleet-state@ - The state of the EC2 Fleet (@submitted@ | @active@ | @deleted@ | @failed@ | @deleted-running@ | @deleted-terminating@ | @modifying@ ).
--
--
--     * @replace-unhealthy-instances@ - Indicates whether EC2 Fleet should replace unhealthy instances (@true@ | @false@ ).
--
--
--     * @type@ - The type of request (@instant@ | @request@ | @maintain@ ).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsFilters :: Lens.Lens' DescribeFleets (Lude.Maybe [Filter])
dfsFilters = Lens.lens (filters :: DescribeFleets -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeFleets)
{-# DEPRECATED dfsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsNextToken :: Lens.Lens' DescribeFleets (Lude.Maybe Lude.Text)
dfsNextToken = Lens.lens (nextToken :: DescribeFleets -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeFleets)
{-# DEPRECATED dfsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the EC2 Fleets.
--
-- /Note:/ Consider using 'fleetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsFleetIds :: Lens.Lens' DescribeFleets (Lude.Maybe [Lude.Text])
dfsFleetIds = Lens.lens (fleetIds :: DescribeFleets -> Lude.Maybe [Lude.Text]) (\s a -> s {fleetIds = a} :: DescribeFleets)
{-# DEPRECATED dfsFleetIds "Use generic-lens or generic-optics with 'fleetIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsDryRun :: Lens.Lens' DescribeFleets (Lude.Maybe Lude.Bool)
dfsDryRun = Lens.lens (dryRun :: DescribeFleets -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeFleets)
{-# DEPRECATED dfsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsMaxResults :: Lens.Lens' DescribeFleets (Lude.Maybe Lude.Int)
dfsMaxResults = Lens.lens (maxResults :: DescribeFleets -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeFleets)
{-# DEPRECATED dfsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeFleets where
  page rq rs
    | Page.stop (rs Lens.^. dfsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dfsrsFleets) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dfsNextToken Lens..~ rs Lens.^. dfsrsNextToken

instance Lude.AWSRequest DescribeFleets where
  type Rs DescribeFleets = DescribeFleetsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeFleetsResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "fleetSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeFleets where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeFleets where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeFleets where
  toQuery DescribeFleets' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeFleets" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        Lude.toQuery (Lude.toQueryList "FleetId" Lude.<$> fleetIds),
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeFleetsResponse' smart constructor.
data DescribeFleetsResponse = DescribeFleetsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    fleets :: Lude.Maybe [FleetData],
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

-- | Creates a value of 'DescribeFleetsResponse' with the minimum fields required to make a request.
--
-- * 'fleets' - Information about the EC2 Fleets.
-- * 'nextToken' - The token for the next set of results.
-- * 'responseStatus' - The response status code.
mkDescribeFleetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeFleetsResponse
mkDescribeFleetsResponse pResponseStatus_ =
  DescribeFleetsResponse'
    { nextToken = Lude.Nothing,
      fleets = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsNextToken :: Lens.Lens' DescribeFleetsResponse (Lude.Maybe Lude.Text)
dfsrsNextToken = Lens.lens (nextToken :: DescribeFleetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeFleetsResponse)
{-# DEPRECATED dfsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the EC2 Fleets.
--
-- /Note:/ Consider using 'fleets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsFleets :: Lens.Lens' DescribeFleetsResponse (Lude.Maybe [FleetData])
dfsrsFleets = Lens.lens (fleets :: DescribeFleetsResponse -> Lude.Maybe [FleetData]) (\s a -> s {fleets = a} :: DescribeFleetsResponse)
{-# DEPRECATED dfsrsFleets "Use generic-lens or generic-optics with 'fleets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsResponseStatus :: Lens.Lens' DescribeFleetsResponse Lude.Int
dfsrsResponseStatus = Lens.lens (responseStatus :: DescribeFleetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeFleetsResponse)
{-# DEPRECATED dfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
