{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.UpdateFleetCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates capacity settings for a fleet. Use this operation to specify the number of EC2 instances (hosts) that you want this fleet to contain. Before calling this operation, you may want to call 'DescribeEC2InstanceLimits' to get the maximum capacity based on the fleet's EC2 instance type.
--
-- Specify minimum and maximum number of instances. Amazon GameLift will not change fleet capacity to values fall outside of this range. This is particularly important when using auto-scaling (see 'PutScalingPolicy' ) to allow capacity to adjust based on player demand while imposing limits on automatic adjustments.
-- To update fleet capacity, specify the fleet ID and the number of instances you want the fleet to host. If successful, Amazon GameLift starts or terminates instances so that the fleet's active instance count matches the desired instance count. You can view a fleet's current capacity information by calling 'DescribeFleetCapacity' . If the desired instance count is higher than the instance type's limit, the "Limit Exceeded" exception occurs.
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
--     * 'DescribeFleetAttributes'
--
--
--     * Update fleets:
--
--     * 'UpdateFleetAttributes'
--
--
--     * 'UpdateFleetCapacity'
--
--
--     * 'UpdateFleetPortSettings'
--
--
--     * 'UpdateRuntimeConfiguration'
--
--
--
--
--     * 'StartFleetActions' or 'StopFleetActions'
module Network.AWS.GameLift.UpdateFleetCapacity
  ( -- * Creating a request
    UpdateFleetCapacity (..),
    mkUpdateFleetCapacity,

    -- ** Request lenses
    ufcMaxSize,
    ufcMinSize,
    ufcDesiredInstances,
    ufcFleetId,

    -- * Destructuring the response
    UpdateFleetCapacityResponse (..),
    mkUpdateFleetCapacityResponse,

    -- ** Response lenses
    ufcrsFleetId,
    ufcrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkUpdateFleetCapacity' smart constructor.
data UpdateFleetCapacity = UpdateFleetCapacity'
  { maxSize ::
      Lude.Maybe Lude.Natural,
    minSize :: Lude.Maybe Lude.Natural,
    desiredInstances :: Lude.Maybe Lude.Natural,
    fleetId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateFleetCapacity' with the minimum fields required to make a request.
--
-- * 'desiredInstances' - Number of EC2 instances you want this fleet to host.
-- * 'fleetId' - A unique identifier for a fleet to update capacity for. You can use either the fleet ID or ARN value.
-- * 'maxSize' - The maximum value allowed for the fleet's instance count. Default if not set is 1.
-- * 'minSize' - The minimum value allowed for the fleet's instance count. Default if not set is 0.
mkUpdateFleetCapacity ::
  -- | 'fleetId'
  Lude.Text ->
  UpdateFleetCapacity
mkUpdateFleetCapacity pFleetId_ =
  UpdateFleetCapacity'
    { maxSize = Lude.Nothing,
      minSize = Lude.Nothing,
      desiredInstances = Lude.Nothing,
      fleetId = pFleetId_
    }

-- | The maximum value allowed for the fleet's instance count. Default if not set is 1.
--
-- /Note:/ Consider using 'maxSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufcMaxSize :: Lens.Lens' UpdateFleetCapacity (Lude.Maybe Lude.Natural)
ufcMaxSize = Lens.lens (maxSize :: UpdateFleetCapacity -> Lude.Maybe Lude.Natural) (\s a -> s {maxSize = a} :: UpdateFleetCapacity)
{-# DEPRECATED ufcMaxSize "Use generic-lens or generic-optics with 'maxSize' instead." #-}

-- | The minimum value allowed for the fleet's instance count. Default if not set is 0.
--
-- /Note:/ Consider using 'minSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufcMinSize :: Lens.Lens' UpdateFleetCapacity (Lude.Maybe Lude.Natural)
ufcMinSize = Lens.lens (minSize :: UpdateFleetCapacity -> Lude.Maybe Lude.Natural) (\s a -> s {minSize = a} :: UpdateFleetCapacity)
{-# DEPRECATED ufcMinSize "Use generic-lens or generic-optics with 'minSize' instead." #-}

-- | Number of EC2 instances you want this fleet to host.
--
-- /Note:/ Consider using 'desiredInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufcDesiredInstances :: Lens.Lens' UpdateFleetCapacity (Lude.Maybe Lude.Natural)
ufcDesiredInstances = Lens.lens (desiredInstances :: UpdateFleetCapacity -> Lude.Maybe Lude.Natural) (\s a -> s {desiredInstances = a} :: UpdateFleetCapacity)
{-# DEPRECATED ufcDesiredInstances "Use generic-lens or generic-optics with 'desiredInstances' instead." #-}

-- | A unique identifier for a fleet to update capacity for. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufcFleetId :: Lens.Lens' UpdateFleetCapacity Lude.Text
ufcFleetId = Lens.lens (fleetId :: UpdateFleetCapacity -> Lude.Text) (\s a -> s {fleetId = a} :: UpdateFleetCapacity)
{-# DEPRECATED ufcFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

instance Lude.AWSRequest UpdateFleetCapacity where
  type Rs UpdateFleetCapacity = UpdateFleetCapacityResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateFleetCapacityResponse'
            Lude.<$> (x Lude..?> "FleetId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateFleetCapacity where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.UpdateFleetCapacity" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateFleetCapacity where
  toJSON UpdateFleetCapacity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MaxSize" Lude..=) Lude.<$> maxSize,
            ("MinSize" Lude..=) Lude.<$> minSize,
            ("DesiredInstances" Lude..=) Lude.<$> desiredInstances,
            Lude.Just ("FleetId" Lude..= fleetId)
          ]
      )

instance Lude.ToPath UpdateFleetCapacity where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateFleetCapacity where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkUpdateFleetCapacityResponse' smart constructor.
data UpdateFleetCapacityResponse = UpdateFleetCapacityResponse'
  { fleetId ::
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

-- | Creates a value of 'UpdateFleetCapacityResponse' with the minimum fields required to make a request.
--
-- * 'fleetId' - A unique identifier for a fleet that was updated.
-- * 'responseStatus' - The response status code.
mkUpdateFleetCapacityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateFleetCapacityResponse
mkUpdateFleetCapacityResponse pResponseStatus_ =
  UpdateFleetCapacityResponse'
    { fleetId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A unique identifier for a fleet that was updated.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufcrsFleetId :: Lens.Lens' UpdateFleetCapacityResponse (Lude.Maybe Lude.Text)
ufcrsFleetId = Lens.lens (fleetId :: UpdateFleetCapacityResponse -> Lude.Maybe Lude.Text) (\s a -> s {fleetId = a} :: UpdateFleetCapacityResponse)
{-# DEPRECATED ufcrsFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufcrsResponseStatus :: Lens.Lens' UpdateFleetCapacityResponse Lude.Int
ufcrsResponseStatus = Lens.lens (responseStatus :: UpdateFleetCapacityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateFleetCapacityResponse)
{-# DEPRECATED ufcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
