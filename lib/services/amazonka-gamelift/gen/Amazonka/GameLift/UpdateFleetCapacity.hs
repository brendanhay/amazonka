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
-- Module      : Amazonka.GameLift.UpdateFleetCapacity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates capacity settings for a fleet. For fleets with multiple
-- locations, use this operation to manage capacity settings in each
-- location individually. Fleet capacity determines the number of game
-- sessions and players that can be hosted based on the fleet
-- configuration. Use this operation to set the following fleet capacity
-- properties:
--
-- -   Minimum\/maximum size: Set hard limits on fleet capacity. Amazon
--     GameLift cannot set the fleet\'s capacity to a value outside of this
--     range, whether the capacity is changed manually or through automatic
--     scaling.
--
-- -   Desired capacity: Manually set the number of Amazon EC2 instances to
--     be maintained in a fleet location. Before changing a fleet\'s
--     desired capacity, you may want to call
--     <https://docs.aws.amazon.com/gamelift/latest/apireference/API_DescribeEC2InstanceLimits.html DescribeEC2InstanceLimits>
--     to get the maximum capacity of the fleet\'s Amazon EC2 instance
--     type. Alternatively, consider using automatic scaling to adjust
--     capacity based on player demand.
--
-- This operation can be used in the following ways:
--
-- -   To update capacity for a fleet\'s home Region, or if the fleet has
--     no remote locations, omit the @Location@ parameter. The fleet must
--     be in @ACTIVE@ status.
--
-- -   To update capacity for a fleet\'s remote location, include the
--     @Location@ parameter set to the location to be updated. The location
--     must be in @ACTIVE@ status.
--
-- If successful, capacity settings are updated immediately. In response a
-- change in desired capacity, Amazon GameLift initiates steps to start new
-- instances or terminate existing instances in the requested fleet
-- location. This continues until the location\'s active instance count
-- matches the new desired instance count. You can track a fleet\'s current
-- capacity by calling
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_DescribeFleetCapacity.html DescribeFleetCapacity>
-- or
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_DescribeFleetLocationCapacity.html DescribeFleetLocationCapacity>.
-- If the requested desired instance count is higher than the instance
-- type\'s limit, the @LimitExceeded@ exception occurs.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-manage-capacity.html Scaling fleet capacity>
module Amazonka.GameLift.UpdateFleetCapacity
  ( -- * Creating a Request
    UpdateFleetCapacity (..),
    newUpdateFleetCapacity,

    -- * Request Lenses
    updateFleetCapacity_desiredInstances,
    updateFleetCapacity_location,
    updateFleetCapacity_maxSize,
    updateFleetCapacity_minSize,
    updateFleetCapacity_fleetId,

    -- * Destructuring the Response
    UpdateFleetCapacityResponse (..),
    newUpdateFleetCapacityResponse,

    -- * Response Lenses
    updateFleetCapacityResponse_fleetArn,
    updateFleetCapacityResponse_fleetId,
    updateFleetCapacityResponse_location,
    updateFleetCapacityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFleetCapacity' smart constructor.
data UpdateFleetCapacity = UpdateFleetCapacity'
  { -- | The number of Amazon EC2 instances you want to maintain in the specified
    -- fleet location. This value must fall between the minimum and maximum
    -- size limits.
    desiredInstances :: Prelude.Maybe Prelude.Natural,
    -- | The name of a remote location to update fleet capacity settings for, in
    -- the form of an Amazon Web Services Region code such as @us-west-2@.
    location :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of instances that are allowed in the specified fleet
    -- location. If this parameter is not set, the default is 1.
    maxSize :: Prelude.Maybe Prelude.Natural,
    -- | The minimum number of instances that are allowed in the specified fleet
    -- location. If this parameter is not set, the default is 0.
    minSize :: Prelude.Maybe Prelude.Natural,
    -- | A unique identifier for the fleet to update capacity settings for. You
    -- can use either the fleet ID or ARN value.
    fleetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFleetCapacity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'desiredInstances', 'updateFleetCapacity_desiredInstances' - The number of Amazon EC2 instances you want to maintain in the specified
-- fleet location. This value must fall between the minimum and maximum
-- size limits.
--
-- 'location', 'updateFleetCapacity_location' - The name of a remote location to update fleet capacity settings for, in
-- the form of an Amazon Web Services Region code such as @us-west-2@.
--
-- 'maxSize', 'updateFleetCapacity_maxSize' - The maximum number of instances that are allowed in the specified fleet
-- location. If this parameter is not set, the default is 1.
--
-- 'minSize', 'updateFleetCapacity_minSize' - The minimum number of instances that are allowed in the specified fleet
-- location. If this parameter is not set, the default is 0.
--
-- 'fleetId', 'updateFleetCapacity_fleetId' - A unique identifier for the fleet to update capacity settings for. You
-- can use either the fleet ID or ARN value.
newUpdateFleetCapacity ::
  -- | 'fleetId'
  Prelude.Text ->
  UpdateFleetCapacity
newUpdateFleetCapacity pFleetId_ =
  UpdateFleetCapacity'
    { desiredInstances =
        Prelude.Nothing,
      location = Prelude.Nothing,
      maxSize = Prelude.Nothing,
      minSize = Prelude.Nothing,
      fleetId = pFleetId_
    }

-- | The number of Amazon EC2 instances you want to maintain in the specified
-- fleet location. This value must fall between the minimum and maximum
-- size limits.
updateFleetCapacity_desiredInstances :: Lens.Lens' UpdateFleetCapacity (Prelude.Maybe Prelude.Natural)
updateFleetCapacity_desiredInstances = Lens.lens (\UpdateFleetCapacity' {desiredInstances} -> desiredInstances) (\s@UpdateFleetCapacity' {} a -> s {desiredInstances = a} :: UpdateFleetCapacity)

-- | The name of a remote location to update fleet capacity settings for, in
-- the form of an Amazon Web Services Region code such as @us-west-2@.
updateFleetCapacity_location :: Lens.Lens' UpdateFleetCapacity (Prelude.Maybe Prelude.Text)
updateFleetCapacity_location = Lens.lens (\UpdateFleetCapacity' {location} -> location) (\s@UpdateFleetCapacity' {} a -> s {location = a} :: UpdateFleetCapacity)

-- | The maximum number of instances that are allowed in the specified fleet
-- location. If this parameter is not set, the default is 1.
updateFleetCapacity_maxSize :: Lens.Lens' UpdateFleetCapacity (Prelude.Maybe Prelude.Natural)
updateFleetCapacity_maxSize = Lens.lens (\UpdateFleetCapacity' {maxSize} -> maxSize) (\s@UpdateFleetCapacity' {} a -> s {maxSize = a} :: UpdateFleetCapacity)

-- | The minimum number of instances that are allowed in the specified fleet
-- location. If this parameter is not set, the default is 0.
updateFleetCapacity_minSize :: Lens.Lens' UpdateFleetCapacity (Prelude.Maybe Prelude.Natural)
updateFleetCapacity_minSize = Lens.lens (\UpdateFleetCapacity' {minSize} -> minSize) (\s@UpdateFleetCapacity' {} a -> s {minSize = a} :: UpdateFleetCapacity)

-- | A unique identifier for the fleet to update capacity settings for. You
-- can use either the fleet ID or ARN value.
updateFleetCapacity_fleetId :: Lens.Lens' UpdateFleetCapacity Prelude.Text
updateFleetCapacity_fleetId = Lens.lens (\UpdateFleetCapacity' {fleetId} -> fleetId) (\s@UpdateFleetCapacity' {} a -> s {fleetId = a} :: UpdateFleetCapacity)

instance Core.AWSRequest UpdateFleetCapacity where
  type
    AWSResponse UpdateFleetCapacity =
      UpdateFleetCapacityResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFleetCapacityResponse'
            Prelude.<$> (x Data..?> "FleetArn")
            Prelude.<*> (x Data..?> "FleetId")
            Prelude.<*> (x Data..?> "Location")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFleetCapacity where
  hashWithSalt _salt UpdateFleetCapacity' {..} =
    _salt
      `Prelude.hashWithSalt` desiredInstances
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` maxSize
      `Prelude.hashWithSalt` minSize
      `Prelude.hashWithSalt` fleetId

instance Prelude.NFData UpdateFleetCapacity where
  rnf UpdateFleetCapacity' {..} =
    Prelude.rnf desiredInstances
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf maxSize
      `Prelude.seq` Prelude.rnf minSize
      `Prelude.seq` Prelude.rnf fleetId

instance Data.ToHeaders UpdateFleetCapacity where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.UpdateFleetCapacity" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateFleetCapacity where
  toJSON UpdateFleetCapacity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DesiredInstances" Data..=)
              Prelude.<$> desiredInstances,
            ("Location" Data..=) Prelude.<$> location,
            ("MaxSize" Data..=) Prelude.<$> maxSize,
            ("MinSize" Data..=) Prelude.<$> minSize,
            Prelude.Just ("FleetId" Data..= fleetId)
          ]
      )

instance Data.ToPath UpdateFleetCapacity where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateFleetCapacity where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFleetCapacityResponse' smart constructor.
data UpdateFleetCapacityResponse = UpdateFleetCapacityResponse'
  { -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a Amazon GameLift fleet resource and uniquely
    -- identifies it. ARNs are unique across all Regions. Format is
    -- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the fleet that was updated.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | The remote location being updated, expressed as an Amazon Web Services
    -- Region code, such as @us-west-2@.
    location :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFleetCapacityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetArn', 'updateFleetCapacityResponse_fleetArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a Amazon GameLift fleet resource and uniquely
-- identifies it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
--
-- 'fleetId', 'updateFleetCapacityResponse_fleetId' - A unique identifier for the fleet that was updated.
--
-- 'location', 'updateFleetCapacityResponse_location' - The remote location being updated, expressed as an Amazon Web Services
-- Region code, such as @us-west-2@.
--
-- 'httpStatus', 'updateFleetCapacityResponse_httpStatus' - The response's http status code.
newUpdateFleetCapacityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFleetCapacityResponse
newUpdateFleetCapacityResponse pHttpStatus_ =
  UpdateFleetCapacityResponse'
    { fleetArn =
        Prelude.Nothing,
      fleetId = Prelude.Nothing,
      location = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a Amazon GameLift fleet resource and uniquely
-- identifies it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
updateFleetCapacityResponse_fleetArn :: Lens.Lens' UpdateFleetCapacityResponse (Prelude.Maybe Prelude.Text)
updateFleetCapacityResponse_fleetArn = Lens.lens (\UpdateFleetCapacityResponse' {fleetArn} -> fleetArn) (\s@UpdateFleetCapacityResponse' {} a -> s {fleetArn = a} :: UpdateFleetCapacityResponse)

-- | A unique identifier for the fleet that was updated.
updateFleetCapacityResponse_fleetId :: Lens.Lens' UpdateFleetCapacityResponse (Prelude.Maybe Prelude.Text)
updateFleetCapacityResponse_fleetId = Lens.lens (\UpdateFleetCapacityResponse' {fleetId} -> fleetId) (\s@UpdateFleetCapacityResponse' {} a -> s {fleetId = a} :: UpdateFleetCapacityResponse)

-- | The remote location being updated, expressed as an Amazon Web Services
-- Region code, such as @us-west-2@.
updateFleetCapacityResponse_location :: Lens.Lens' UpdateFleetCapacityResponse (Prelude.Maybe Prelude.Text)
updateFleetCapacityResponse_location = Lens.lens (\UpdateFleetCapacityResponse' {location} -> location) (\s@UpdateFleetCapacityResponse' {} a -> s {location = a} :: UpdateFleetCapacityResponse)

-- | The response's http status code.
updateFleetCapacityResponse_httpStatus :: Lens.Lens' UpdateFleetCapacityResponse Prelude.Int
updateFleetCapacityResponse_httpStatus = Lens.lens (\UpdateFleetCapacityResponse' {httpStatus} -> httpStatus) (\s@UpdateFleetCapacityResponse' {} a -> s {httpStatus = a} :: UpdateFleetCapacityResponse)

instance Prelude.NFData UpdateFleetCapacityResponse where
  rnf UpdateFleetCapacityResponse' {..} =
    Prelude.rnf fleetArn
      `Prelude.seq` Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf httpStatus
