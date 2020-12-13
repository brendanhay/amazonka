{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeFleetCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current capacity statistics for one or more fleets. These statistics present a snapshot of the fleet's instances and provide insight on current or imminent scaling activity. To get statistics on game hosting activity in the fleet, see 'DescribeFleetUtilization' .
--
-- You can request capacity for all fleets or specify a list of one or more fleet identifiers. When requesting multiple fleets, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a 'FleetCapacity' object is returned for each requested fleet ID. When a list of fleet IDs is provided, attribute objects are returned only for fleets that currently exist.
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
module Network.AWS.GameLift.DescribeFleetCapacity
  ( -- * Creating a request
    DescribeFleetCapacity (..),
    mkDescribeFleetCapacity,

    -- ** Request lenses
    dfcNextToken,
    dfcLimit,
    dfcFleetIds,

    -- * Destructuring the response
    DescribeFleetCapacityResponse (..),
    mkDescribeFleetCapacityResponse,

    -- ** Response lenses
    dfcrsNextToken,
    dfcrsFleetCapacity,
    dfcrsResponseStatus,
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
-- /See:/ 'mkDescribeFleetCapacity' smart constructor.
data DescribeFleetCapacity = DescribeFleetCapacity'
  { -- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value. This parameter is ignored when the request specifies one or a list of fleet IDs.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. This parameter is ignored when the request specifies one or a list of fleet IDs.
    limit :: Lude.Maybe Lude.Natural,
    -- | A unique identifier for a fleet(s) to retrieve capacity information for. You can use either the fleet ID or ARN value.
    fleetIds :: Lude.Maybe (Lude.NonEmpty Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFleetCapacity' with the minimum fields required to make a request.
--
-- * 'nextToken' - Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value. This parameter is ignored when the request specifies one or a list of fleet IDs.
-- * 'limit' - The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. This parameter is ignored when the request specifies one or a list of fleet IDs.
-- * 'fleetIds' - A unique identifier for a fleet(s) to retrieve capacity information for. You can use either the fleet ID or ARN value.
mkDescribeFleetCapacity ::
  DescribeFleetCapacity
mkDescribeFleetCapacity =
  DescribeFleetCapacity'
    { nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      fleetIds = Lude.Nothing
    }

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value. This parameter is ignored when the request specifies one or a list of fleet IDs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcNextToken :: Lens.Lens' DescribeFleetCapacity (Lude.Maybe Lude.Text)
dfcNextToken = Lens.lens (nextToken :: DescribeFleetCapacity -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeFleetCapacity)
{-# DEPRECATED dfcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. This parameter is ignored when the request specifies one or a list of fleet IDs.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcLimit :: Lens.Lens' DescribeFleetCapacity (Lude.Maybe Lude.Natural)
dfcLimit = Lens.lens (limit :: DescribeFleetCapacity -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeFleetCapacity)
{-# DEPRECATED dfcLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | A unique identifier for a fleet(s) to retrieve capacity information for. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcFleetIds :: Lens.Lens' DescribeFleetCapacity (Lude.Maybe (Lude.NonEmpty Lude.Text))
dfcFleetIds = Lens.lens (fleetIds :: DescribeFleetCapacity -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {fleetIds = a} :: DescribeFleetCapacity)
{-# DEPRECATED dfcFleetIds "Use generic-lens or generic-optics with 'fleetIds' instead." #-}

instance Page.AWSPager DescribeFleetCapacity where
  page rq rs
    | Page.stop (rs Lens.^. dfcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dfcrsFleetCapacity) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dfcNextToken Lens..~ rs Lens.^. dfcrsNextToken

instance Lude.AWSRequest DescribeFleetCapacity where
  type Rs DescribeFleetCapacity = DescribeFleetCapacityResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeFleetCapacityResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "FleetCapacity" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeFleetCapacity where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DescribeFleetCapacity" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeFleetCapacity where
  toJSON DescribeFleetCapacity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            ("FleetIds" Lude..=) Lude.<$> fleetIds
          ]
      )

instance Lude.ToPath DescribeFleetCapacity where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeFleetCapacity where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeFleetCapacityResponse' smart constructor.
data DescribeFleetCapacityResponse = DescribeFleetCapacityResponse'
  { -- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A collection of objects containing capacity information for each requested fleet ID. Leave this parameter empty to retrieve capacity information for all fleets.
    fleetCapacity :: Lude.Maybe [FleetCapacity],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFleetCapacityResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
-- * 'fleetCapacity' - A collection of objects containing capacity information for each requested fleet ID. Leave this parameter empty to retrieve capacity information for all fleets.
-- * 'responseStatus' - The response status code.
mkDescribeFleetCapacityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeFleetCapacityResponse
mkDescribeFleetCapacityResponse pResponseStatus_ =
  DescribeFleetCapacityResponse'
    { nextToken = Lude.Nothing,
      fleetCapacity = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcrsNextToken :: Lens.Lens' DescribeFleetCapacityResponse (Lude.Maybe Lude.Text)
dfcrsNextToken = Lens.lens (nextToken :: DescribeFleetCapacityResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeFleetCapacityResponse)
{-# DEPRECATED dfcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A collection of objects containing capacity information for each requested fleet ID. Leave this parameter empty to retrieve capacity information for all fleets.
--
-- /Note:/ Consider using 'fleetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcrsFleetCapacity :: Lens.Lens' DescribeFleetCapacityResponse (Lude.Maybe [FleetCapacity])
dfcrsFleetCapacity = Lens.lens (fleetCapacity :: DescribeFleetCapacityResponse -> Lude.Maybe [FleetCapacity]) (\s a -> s {fleetCapacity = a} :: DescribeFleetCapacityResponse)
{-# DEPRECATED dfcrsFleetCapacity "Use generic-lens or generic-optics with 'fleetCapacity' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcrsResponseStatus :: Lens.Lens' DescribeFleetCapacityResponse Lude.Int
dfcrsResponseStatus = Lens.lens (responseStatus :: DescribeFleetCapacityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeFleetCapacityResponse)
{-# DEPRECATED dfcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
