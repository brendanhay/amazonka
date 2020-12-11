{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeFleetAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves core properties, including configuration, status, and metadata, for a fleet.
--
-- To get attributes for one or more fleets, provide a list of fleet IDs or fleet ARNs. To get attributes for all fleets, do not specify a fleet identifier. When requesting attributes for multiple fleets, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a 'FleetAttributes' object is returned for each fleet requested, unless the fleet identifier is not found.
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift Fleets>
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
module Network.AWS.GameLift.DescribeFleetAttributes
  ( -- * Creating a request
    DescribeFleetAttributes (..),
    mkDescribeFleetAttributes,

    -- ** Request lenses
    dfaNextToken,
    dfaLimit,
    dfaFleetIds,

    -- * Destructuring the response
    DescribeFleetAttributesResponse (..),
    mkDescribeFleetAttributesResponse,

    -- ** Response lenses
    dfarsNextToken,
    dfarsFleetAttributes,
    dfarsResponseStatus,
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
-- /See:/ 'mkDescribeFleetAttributes' smart constructor.
data DescribeFleetAttributes = DescribeFleetAttributes'
  { nextToken ::
      Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    fleetIds ::
      Lude.Maybe (Lude.NonEmpty Lude.Text)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFleetAttributes' with the minimum fields required to make a request.
--
-- * 'fleetIds' - A list of unique fleet identifiers to retrieve attributes for. You can use either the fleet ID or ARN value. To retrieve attributes for all current fleets, do not include this parameter. If the list of fleet identifiers includes fleets that don't currently exist, the request succeeds but no attributes for that fleet are returned.
-- * 'limit' - The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. This parameter is ignored when the request specifies one or a list of fleet IDs.
-- * 'nextToken' - Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value. This parameter is ignored when the request specifies one or a list of fleet IDs.
mkDescribeFleetAttributes ::
  DescribeFleetAttributes
mkDescribeFleetAttributes =
  DescribeFleetAttributes'
    { nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      fleetIds = Lude.Nothing
    }

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value. This parameter is ignored when the request specifies one or a list of fleet IDs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfaNextToken :: Lens.Lens' DescribeFleetAttributes (Lude.Maybe Lude.Text)
dfaNextToken = Lens.lens (nextToken :: DescribeFleetAttributes -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeFleetAttributes)
{-# DEPRECATED dfaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. This parameter is ignored when the request specifies one or a list of fleet IDs.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfaLimit :: Lens.Lens' DescribeFleetAttributes (Lude.Maybe Lude.Natural)
dfaLimit = Lens.lens (limit :: DescribeFleetAttributes -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeFleetAttributes)
{-# DEPRECATED dfaLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | A list of unique fleet identifiers to retrieve attributes for. You can use either the fleet ID or ARN value. To retrieve attributes for all current fleets, do not include this parameter. If the list of fleet identifiers includes fleets that don't currently exist, the request succeeds but no attributes for that fleet are returned.
--
-- /Note:/ Consider using 'fleetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfaFleetIds :: Lens.Lens' DescribeFleetAttributes (Lude.Maybe (Lude.NonEmpty Lude.Text))
dfaFleetIds = Lens.lens (fleetIds :: DescribeFleetAttributes -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {fleetIds = a} :: DescribeFleetAttributes)
{-# DEPRECATED dfaFleetIds "Use generic-lens or generic-optics with 'fleetIds' instead." #-}

instance Page.AWSPager DescribeFleetAttributes where
  page rq rs
    | Page.stop (rs Lens.^. dfarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dfarsFleetAttributes) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dfaNextToken Lens..~ rs Lens.^. dfarsNextToken

instance Lude.AWSRequest DescribeFleetAttributes where
  type Rs DescribeFleetAttributes = DescribeFleetAttributesResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeFleetAttributesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "FleetAttributes" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeFleetAttributes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DescribeFleetAttributes" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeFleetAttributes where
  toJSON DescribeFleetAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            ("FleetIds" Lude..=) Lude.<$> fleetIds
          ]
      )

instance Lude.ToPath DescribeFleetAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeFleetAttributes where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeFleetAttributesResponse' smart constructor.
data DescribeFleetAttributesResponse = DescribeFleetAttributesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    fleetAttributes ::
      Lude.Maybe
        [FleetAttributes],
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

-- | Creates a value of 'DescribeFleetAttributesResponse' with the minimum fields required to make a request.
--
-- * 'fleetAttributes' - A collection of objects containing attribute metadata for each requested fleet ID. Attribute objects are returned only for fleets that currently exist.
-- * 'nextToken' - Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
-- * 'responseStatus' - The response status code.
mkDescribeFleetAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeFleetAttributesResponse
mkDescribeFleetAttributesResponse pResponseStatus_ =
  DescribeFleetAttributesResponse'
    { nextToken = Lude.Nothing,
      fleetAttributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfarsNextToken :: Lens.Lens' DescribeFleetAttributesResponse (Lude.Maybe Lude.Text)
dfarsNextToken = Lens.lens (nextToken :: DescribeFleetAttributesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeFleetAttributesResponse)
{-# DEPRECATED dfarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A collection of objects containing attribute metadata for each requested fleet ID. Attribute objects are returned only for fleets that currently exist.
--
-- /Note:/ Consider using 'fleetAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfarsFleetAttributes :: Lens.Lens' DescribeFleetAttributesResponse (Lude.Maybe [FleetAttributes])
dfarsFleetAttributes = Lens.lens (fleetAttributes :: DescribeFleetAttributesResponse -> Lude.Maybe [FleetAttributes]) (\s a -> s {fleetAttributes = a} :: DescribeFleetAttributesResponse)
{-# DEPRECATED dfarsFleetAttributes "Use generic-lens or generic-optics with 'fleetAttributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfarsResponseStatus :: Lens.Lens' DescribeFleetAttributesResponse Lude.Int
dfarsResponseStatus = Lens.lens (responseStatus :: DescribeFleetAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeFleetAttributesResponse)
{-# DEPRECATED dfarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
