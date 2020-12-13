{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeFleetUtilization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves utilization statistics for one or more fleets. These statistics provide insight into how available hosting resources are currently being used. To get statistics on available hosting resources, see 'DescribeFleetCapacity' .
--
-- You can request utilization data for all fleets, or specify a list of one or more fleet IDs. When requesting multiple fleets, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a 'FleetUtilization' object is returned for each requested fleet ID, unless the fleet identifier is not found.
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift Fleets>
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/monitoring-cloudwatch.html#gamelift-metrics-fleet GameLift Metrics for Fleets>
-- __Related operations__
--
--     * 'CreateFleet'
--
--
--     * 'ListFleets'
--
--
--     * 'DeleteFleet'
--
--
--     * Describe fleets:
--
--     * 'DescribeFleetAttributes'
--
--
--     * 'DescribeFleetCapacity'
--
--
--     * 'DescribeFleetPortSettings'
--
--
--     * 'DescribeFleetUtilization'
--
--
--     * 'DescribeRuntimeConfiguration'
--
--
--     * 'DescribeEC2InstanceLimits'
--
--
--     * 'DescribeFleetEvents'
--
--
--
--
--     * 'UpdateFleetAttributes'
--
--
--     * 'StartFleetActions' or 'StopFleetActions'
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.DescribeFleetUtilization
  ( -- * Creating a request
    DescribeFleetUtilization (..),
    mkDescribeFleetUtilization,

    -- ** Request lenses
    dfuNextToken,
    dfuLimit,
    dfuFleetIds,

    -- * Destructuring the response
    DescribeFleetUtilizationResponse (..),
    mkDescribeFleetUtilizationResponse,

    -- ** Response lenses
    dfursNextToken,
    dfursFleetUtilization,
    dfursResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDescribeFleetUtilization' smart constructor.
data DescribeFleetUtilization = DescribeFleetUtilization'
  { -- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value. This parameter is ignored when the request specifies one or a list of fleet IDs.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. This parameter is ignored when the request specifies one or a list of fleet IDs.
    limit :: Lude.Maybe Lude.Natural,
    -- | A unique identifier for a fleet(s) to retrieve utilization data for. You can use either the fleet ID or ARN value. To retrieve attributes for all current fleets, do not include this parameter. If the list of fleet identifiers includes fleets that don't currently exist, the request succeeds but no attributes for that fleet are returned.
    fleetIds :: Lude.Maybe (Lude.NonEmpty Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFleetUtilization' with the minimum fields required to make a request.
--
-- * 'nextToken' - Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value. This parameter is ignored when the request specifies one or a list of fleet IDs.
-- * 'limit' - The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. This parameter is ignored when the request specifies one or a list of fleet IDs.
-- * 'fleetIds' - A unique identifier for a fleet(s) to retrieve utilization data for. You can use either the fleet ID or ARN value. To retrieve attributes for all current fleets, do not include this parameter. If the list of fleet identifiers includes fleets that don't currently exist, the request succeeds but no attributes for that fleet are returned.
mkDescribeFleetUtilization ::
  DescribeFleetUtilization
mkDescribeFleetUtilization =
  DescribeFleetUtilization'
    { nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      fleetIds = Lude.Nothing
    }

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value. This parameter is ignored when the request specifies one or a list of fleet IDs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfuNextToken :: Lens.Lens' DescribeFleetUtilization (Lude.Maybe Lude.Text)
dfuNextToken = Lens.lens (nextToken :: DescribeFleetUtilization -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeFleetUtilization)
{-# DEPRECATED dfuNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. This parameter is ignored when the request specifies one or a list of fleet IDs.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfuLimit :: Lens.Lens' DescribeFleetUtilization (Lude.Maybe Lude.Natural)
dfuLimit = Lens.lens (limit :: DescribeFleetUtilization -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeFleetUtilization)
{-# DEPRECATED dfuLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | A unique identifier for a fleet(s) to retrieve utilization data for. You can use either the fleet ID or ARN value. To retrieve attributes for all current fleets, do not include this parameter. If the list of fleet identifiers includes fleets that don't currently exist, the request succeeds but no attributes for that fleet are returned.
--
-- /Note:/ Consider using 'fleetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfuFleetIds :: Lens.Lens' DescribeFleetUtilization (Lude.Maybe (Lude.NonEmpty Lude.Text))
dfuFleetIds = Lens.lens (fleetIds :: DescribeFleetUtilization -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {fleetIds = a} :: DescribeFleetUtilization)
{-# DEPRECATED dfuFleetIds "Use generic-lens or generic-optics with 'fleetIds' instead." #-}

instance Page.AWSPager DescribeFleetUtilization where
  page rq rs
    | Page.stop (rs Lens.^. dfursNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dfursFleetUtilization) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dfuNextToken Lens..~ rs Lens.^. dfursNextToken

instance Lude.AWSRequest DescribeFleetUtilization where
  type Rs DescribeFleetUtilization = DescribeFleetUtilizationResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeFleetUtilizationResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "FleetUtilization" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeFleetUtilization where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DescribeFleetUtilization" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeFleetUtilization where
  toJSON DescribeFleetUtilization' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            ("FleetIds" Lude..=) Lude.<$> fleetIds
          ]
      )

instance Lude.ToPath DescribeFleetUtilization where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeFleetUtilization where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeFleetUtilizationResponse' smart constructor.
data DescribeFleetUtilizationResponse = DescribeFleetUtilizationResponse'
  { -- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A collection of objects containing utilization information for each requested fleet ID.
    fleetUtilization :: Lude.Maybe [FleetUtilization],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFleetUtilizationResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
-- * 'fleetUtilization' - A collection of objects containing utilization information for each requested fleet ID.
-- * 'responseStatus' - The response status code.
mkDescribeFleetUtilizationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeFleetUtilizationResponse
mkDescribeFleetUtilizationResponse pResponseStatus_ =
  DescribeFleetUtilizationResponse'
    { nextToken = Lude.Nothing,
      fleetUtilization = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfursNextToken :: Lens.Lens' DescribeFleetUtilizationResponse (Lude.Maybe Lude.Text)
dfursNextToken = Lens.lens (nextToken :: DescribeFleetUtilizationResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeFleetUtilizationResponse)
{-# DEPRECATED dfursNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A collection of objects containing utilization information for each requested fleet ID.
--
-- /Note:/ Consider using 'fleetUtilization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfursFleetUtilization :: Lens.Lens' DescribeFleetUtilizationResponse (Lude.Maybe [FleetUtilization])
dfursFleetUtilization = Lens.lens (fleetUtilization :: DescribeFleetUtilizationResponse -> Lude.Maybe [FleetUtilization]) (\s a -> s {fleetUtilization = a} :: DescribeFleetUtilizationResponse)
{-# DEPRECATED dfursFleetUtilization "Use generic-lens or generic-optics with 'fleetUtilization' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfursResponseStatus :: Lens.Lens' DescribeFleetUtilizationResponse Lude.Int
dfursResponseStatus = Lens.lens (responseStatus :: DescribeFleetUtilizationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeFleetUtilizationResponse)
{-# DEPRECATED dfursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
