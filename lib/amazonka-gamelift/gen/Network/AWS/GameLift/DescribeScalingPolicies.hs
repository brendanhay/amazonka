{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeScalingPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all scaling policies applied to a fleet.
--
-- To get a fleet's scaling policies, specify the fleet ID. You can filter this request by policy status, such as to retrieve only active scaling policies. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, set of 'ScalingPolicy' objects is returned for the fleet.
-- A fleet may have all of its scaling policies suspended ('StopFleetActions' ). This operation does not affect the status of the scaling policies, which remains ACTIVE. To see whether a fleet's scaling policies are in force or suspended, call 'DescribeFleetAttributes' and check the stopped actions.
--
--     * 'DescribeFleetCapacity'
--
--
--     * 'UpdateFleetCapacity'
--
--
--     * 'DescribeEC2InstanceLimits'
--
--
--     * Manage scaling policies:
--
--     * 'PutScalingPolicy' (auto-scaling)
--
--
--     * 'DescribeScalingPolicies' (auto-scaling)
--
--
--     * 'DeleteScalingPolicy' (auto-scaling)
--
--
--
--
--     * Manage fleet actions:
--
--     * 'StartFleetActions'
--
--
--     * 'StopFleetActions'
--
--
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.DescribeScalingPolicies
  ( -- * Creating a request
    DescribeScalingPolicies (..),
    mkDescribeScalingPolicies,

    -- ** Request lenses
    dspNextToken,
    dspStatusFilter,
    dspLimit,
    dspFleetId,

    -- * Destructuring the response
    DescribeScalingPoliciesResponse (..),
    mkDescribeScalingPoliciesResponse,

    -- ** Response lenses
    dsprsNextToken,
    dsprsScalingPolicies,
    dsprsResponseStatus,
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
-- /See:/ 'mkDescribeScalingPolicies' smart constructor.
data DescribeScalingPolicies = DescribeScalingPolicies'
  { -- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Scaling policy status to filter results on. A scaling policy is only in force when in an @ACTIVE@ status.
    --
    --
    --     * __ACTIVE__ -- The scaling policy is currently in force.
    --
    --
    --     * __UPDATEREQUESTED__ -- A request to update the scaling policy has been received.
    --
    --
    --     * __UPDATING__ -- A change is being made to the scaling policy.
    --
    --
    --     * __DELETEREQUESTED__ -- A request to delete the scaling policy has been received.
    --
    --
    --     * __DELETING__ -- The scaling policy is being deleted.
    --
    --
    --     * __DELETED__ -- The scaling policy has been deleted.
    --
    --
    --     * __ERROR__ -- An error occurred in creating the policy. It should be removed and recreated.
    statusFilter :: Lude.Maybe ScalingStatusType,
    -- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
    limit :: Lude.Maybe Lude.Natural,
    -- | A unique identifier for a fleet to retrieve scaling policies for. You can use either the fleet ID or ARN value.
    fleetId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeScalingPolicies' with the minimum fields required to make a request.
--
-- * 'nextToken' - Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
-- * 'statusFilter' - Scaling policy status to filter results on. A scaling policy is only in force when in an @ACTIVE@ status.
--
--
--     * __ACTIVE__ -- The scaling policy is currently in force.
--
--
--     * __UPDATEREQUESTED__ -- A request to update the scaling policy has been received.
--
--
--     * __UPDATING__ -- A change is being made to the scaling policy.
--
--
--     * __DELETEREQUESTED__ -- A request to delete the scaling policy has been received.
--
--
--     * __DELETING__ -- The scaling policy is being deleted.
--
--
--     * __DELETED__ -- The scaling policy has been deleted.
--
--
--     * __ERROR__ -- An error occurred in creating the policy. It should be removed and recreated.
--
--
-- * 'limit' - The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
-- * 'fleetId' - A unique identifier for a fleet to retrieve scaling policies for. You can use either the fleet ID or ARN value.
mkDescribeScalingPolicies ::
  -- | 'fleetId'
  Lude.Text ->
  DescribeScalingPolicies
mkDescribeScalingPolicies pFleetId_ =
  DescribeScalingPolicies'
    { nextToken = Lude.Nothing,
      statusFilter = Lude.Nothing,
      limit = Lude.Nothing,
      fleetId = pFleetId_
    }

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspNextToken :: Lens.Lens' DescribeScalingPolicies (Lude.Maybe Lude.Text)
dspNextToken = Lens.lens (nextToken :: DescribeScalingPolicies -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeScalingPolicies)
{-# DEPRECATED dspNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Scaling policy status to filter results on. A scaling policy is only in force when in an @ACTIVE@ status.
--
--
--     * __ACTIVE__ -- The scaling policy is currently in force.
--
--
--     * __UPDATEREQUESTED__ -- A request to update the scaling policy has been received.
--
--
--     * __UPDATING__ -- A change is being made to the scaling policy.
--
--
--     * __DELETEREQUESTED__ -- A request to delete the scaling policy has been received.
--
--
--     * __DELETING__ -- The scaling policy is being deleted.
--
--
--     * __DELETED__ -- The scaling policy has been deleted.
--
--
--     * __ERROR__ -- An error occurred in creating the policy. It should be removed and recreated.
--
--
--
-- /Note:/ Consider using 'statusFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspStatusFilter :: Lens.Lens' DescribeScalingPolicies (Lude.Maybe ScalingStatusType)
dspStatusFilter = Lens.lens (statusFilter :: DescribeScalingPolicies -> Lude.Maybe ScalingStatusType) (\s a -> s {statusFilter = a} :: DescribeScalingPolicies)
{-# DEPRECATED dspStatusFilter "Use generic-lens or generic-optics with 'statusFilter' instead." #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspLimit :: Lens.Lens' DescribeScalingPolicies (Lude.Maybe Lude.Natural)
dspLimit = Lens.lens (limit :: DescribeScalingPolicies -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeScalingPolicies)
{-# DEPRECATED dspLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | A unique identifier for a fleet to retrieve scaling policies for. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspFleetId :: Lens.Lens' DescribeScalingPolicies Lude.Text
dspFleetId = Lens.lens (fleetId :: DescribeScalingPolicies -> Lude.Text) (\s a -> s {fleetId = a} :: DescribeScalingPolicies)
{-# DEPRECATED dspFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

instance Page.AWSPager DescribeScalingPolicies where
  page rq rs
    | Page.stop (rs Lens.^. dsprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dsprsScalingPolicies) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dspNextToken Lens..~ rs Lens.^. dsprsNextToken

instance Lude.AWSRequest DescribeScalingPolicies where
  type Rs DescribeScalingPolicies = DescribeScalingPoliciesResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeScalingPoliciesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "ScalingPolicies" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeScalingPolicies where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DescribeScalingPolicies" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeScalingPolicies where
  toJSON DescribeScalingPolicies' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("StatusFilter" Lude..=) Lude.<$> statusFilter,
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just ("FleetId" Lude..= fleetId)
          ]
      )

instance Lude.ToPath DescribeScalingPolicies where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeScalingPolicies where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeScalingPoliciesResponse' smart constructor.
data DescribeScalingPoliciesResponse = DescribeScalingPoliciesResponse'
  { -- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A collection of objects containing the scaling policies matching the request.
    scalingPolicies :: Lude.Maybe [ScalingPolicy],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeScalingPoliciesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
-- * 'scalingPolicies' - A collection of objects containing the scaling policies matching the request.
-- * 'responseStatus' - The response status code.
mkDescribeScalingPoliciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeScalingPoliciesResponse
mkDescribeScalingPoliciesResponse pResponseStatus_ =
  DescribeScalingPoliciesResponse'
    { nextToken = Lude.Nothing,
      scalingPolicies = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprsNextToken :: Lens.Lens' DescribeScalingPoliciesResponse (Lude.Maybe Lude.Text)
dsprsNextToken = Lens.lens (nextToken :: DescribeScalingPoliciesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeScalingPoliciesResponse)
{-# DEPRECATED dsprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A collection of objects containing the scaling policies matching the request.
--
-- /Note:/ Consider using 'scalingPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprsScalingPolicies :: Lens.Lens' DescribeScalingPoliciesResponse (Lude.Maybe [ScalingPolicy])
dsprsScalingPolicies = Lens.lens (scalingPolicies :: DescribeScalingPoliciesResponse -> Lude.Maybe [ScalingPolicy]) (\s a -> s {scalingPolicies = a} :: DescribeScalingPoliciesResponse)
{-# DEPRECATED dsprsScalingPolicies "Use generic-lens or generic-optics with 'scalingPolicies' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprsResponseStatus :: Lens.Lens' DescribeScalingPoliciesResponse Lude.Int
dsprsResponseStatus = Lens.lens (responseStatus :: DescribeScalingPoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeScalingPoliciesResponse)
{-# DEPRECATED dsprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
