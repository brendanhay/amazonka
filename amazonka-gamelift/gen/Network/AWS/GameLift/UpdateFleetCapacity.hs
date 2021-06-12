{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.UpdateFleetCapacity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates capacity settings for a fleet. Use this operation to specify the
-- number of EC2 instances (hosts) that you want this fleet to contain.
-- Before calling this operation, you may want to call
-- DescribeEC2InstanceLimits to get the maximum capacity based on the
-- fleet\'s EC2 instance type.
--
-- Specify minimum and maximum number of instances. Amazon GameLift will
-- not change fleet capacity to values fall outside of this range. This is
-- particularly important when using auto-scaling (see PutScalingPolicy) to
-- allow capacity to adjust based on player demand while imposing limits on
-- automatic adjustments.
--
-- To update fleet capacity, specify the fleet ID and the number of
-- instances you want the fleet to host. If successful, Amazon GameLift
-- starts or terminates instances so that the fleet\'s active instance
-- count matches the desired instance count. You can view a fleet\'s
-- current capacity information by calling DescribeFleetCapacity. If the
-- desired instance count is higher than the instance type\'s limit, the
-- \"Limit Exceeded\" exception occurs.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift Fleets>
--
-- __Related operations__
--
-- -   CreateFleet
--
-- -   ListFleets
--
-- -   DeleteFleet
--
-- -   DescribeFleetAttributes
--
-- -   Update fleets:
--
--     -   UpdateFleetAttributes
--
--     -   UpdateFleetCapacity
--
--     -   UpdateFleetPortSettings
--
--     -   UpdateRuntimeConfiguration
--
-- -   StartFleetActions or StopFleetActions
module Network.AWS.GameLift.UpdateFleetCapacity
  ( -- * Creating a Request
    UpdateFleetCapacity (..),
    newUpdateFleetCapacity,

    -- * Request Lenses
    updateFleetCapacity_minSize,
    updateFleetCapacity_maxSize,
    updateFleetCapacity_desiredInstances,
    updateFleetCapacity_fleetId,

    -- * Destructuring the Response
    UpdateFleetCapacityResponse (..),
    newUpdateFleetCapacityResponse,

    -- * Response Lenses
    updateFleetCapacityResponse_fleetId,
    updateFleetCapacityResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newUpdateFleetCapacity' smart constructor.
data UpdateFleetCapacity = UpdateFleetCapacity'
  { -- | The minimum value allowed for the fleet\'s instance count. Default if
    -- not set is 0.
    minSize :: Core.Maybe Core.Natural,
    -- | The maximum value allowed for the fleet\'s instance count. Default if
    -- not set is 1.
    maxSize :: Core.Maybe Core.Natural,
    -- | Number of EC2 instances you want this fleet to host.
    desiredInstances :: Core.Maybe Core.Natural,
    -- | A unique identifier for a fleet to update capacity for. You can use
    -- either the fleet ID or ARN value.
    fleetId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateFleetCapacity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minSize', 'updateFleetCapacity_minSize' - The minimum value allowed for the fleet\'s instance count. Default if
-- not set is 0.
--
-- 'maxSize', 'updateFleetCapacity_maxSize' - The maximum value allowed for the fleet\'s instance count. Default if
-- not set is 1.
--
-- 'desiredInstances', 'updateFleetCapacity_desiredInstances' - Number of EC2 instances you want this fleet to host.
--
-- 'fleetId', 'updateFleetCapacity_fleetId' - A unique identifier for a fleet to update capacity for. You can use
-- either the fleet ID or ARN value.
newUpdateFleetCapacity ::
  -- | 'fleetId'
  Core.Text ->
  UpdateFleetCapacity
newUpdateFleetCapacity pFleetId_ =
  UpdateFleetCapacity'
    { minSize = Core.Nothing,
      maxSize = Core.Nothing,
      desiredInstances = Core.Nothing,
      fleetId = pFleetId_
    }

-- | The minimum value allowed for the fleet\'s instance count. Default if
-- not set is 0.
updateFleetCapacity_minSize :: Lens.Lens' UpdateFleetCapacity (Core.Maybe Core.Natural)
updateFleetCapacity_minSize = Lens.lens (\UpdateFleetCapacity' {minSize} -> minSize) (\s@UpdateFleetCapacity' {} a -> s {minSize = a} :: UpdateFleetCapacity)

-- | The maximum value allowed for the fleet\'s instance count. Default if
-- not set is 1.
updateFleetCapacity_maxSize :: Lens.Lens' UpdateFleetCapacity (Core.Maybe Core.Natural)
updateFleetCapacity_maxSize = Lens.lens (\UpdateFleetCapacity' {maxSize} -> maxSize) (\s@UpdateFleetCapacity' {} a -> s {maxSize = a} :: UpdateFleetCapacity)

-- | Number of EC2 instances you want this fleet to host.
updateFleetCapacity_desiredInstances :: Lens.Lens' UpdateFleetCapacity (Core.Maybe Core.Natural)
updateFleetCapacity_desiredInstances = Lens.lens (\UpdateFleetCapacity' {desiredInstances} -> desiredInstances) (\s@UpdateFleetCapacity' {} a -> s {desiredInstances = a} :: UpdateFleetCapacity)

-- | A unique identifier for a fleet to update capacity for. You can use
-- either the fleet ID or ARN value.
updateFleetCapacity_fleetId :: Lens.Lens' UpdateFleetCapacity Core.Text
updateFleetCapacity_fleetId = Lens.lens (\UpdateFleetCapacity' {fleetId} -> fleetId) (\s@UpdateFleetCapacity' {} a -> s {fleetId = a} :: UpdateFleetCapacity)

instance Core.AWSRequest UpdateFleetCapacity where
  type
    AWSResponse UpdateFleetCapacity =
      UpdateFleetCapacityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFleetCapacityResponse'
            Core.<$> (x Core..?> "FleetId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateFleetCapacity

instance Core.NFData UpdateFleetCapacity

instance Core.ToHeaders UpdateFleetCapacity where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.UpdateFleetCapacity" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateFleetCapacity where
  toJSON UpdateFleetCapacity' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MinSize" Core..=) Core.<$> minSize,
            ("MaxSize" Core..=) Core.<$> maxSize,
            ("DesiredInstances" Core..=)
              Core.<$> desiredInstances,
            Core.Just ("FleetId" Core..= fleetId)
          ]
      )

instance Core.ToPath UpdateFleetCapacity where
  toPath = Core.const "/"

instance Core.ToQuery UpdateFleetCapacity where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newUpdateFleetCapacityResponse' smart constructor.
data UpdateFleetCapacityResponse = UpdateFleetCapacityResponse'
  { -- | A unique identifier for a fleet that was updated.
    fleetId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateFleetCapacityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'updateFleetCapacityResponse_fleetId' - A unique identifier for a fleet that was updated.
--
-- 'httpStatus', 'updateFleetCapacityResponse_httpStatus' - The response's http status code.
newUpdateFleetCapacityResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateFleetCapacityResponse
newUpdateFleetCapacityResponse pHttpStatus_ =
  UpdateFleetCapacityResponse'
    { fleetId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier for a fleet that was updated.
updateFleetCapacityResponse_fleetId :: Lens.Lens' UpdateFleetCapacityResponse (Core.Maybe Core.Text)
updateFleetCapacityResponse_fleetId = Lens.lens (\UpdateFleetCapacityResponse' {fleetId} -> fleetId) (\s@UpdateFleetCapacityResponse' {} a -> s {fleetId = a} :: UpdateFleetCapacityResponse)

-- | The response's http status code.
updateFleetCapacityResponse_httpStatus :: Lens.Lens' UpdateFleetCapacityResponse Core.Int
updateFleetCapacityResponse_httpStatus = Lens.lens (\UpdateFleetCapacityResponse' {httpStatus} -> httpStatus) (\s@UpdateFleetCapacityResponse' {} a -> s {httpStatus = a} :: UpdateFleetCapacityResponse)

instance Core.NFData UpdateFleetCapacityResponse
