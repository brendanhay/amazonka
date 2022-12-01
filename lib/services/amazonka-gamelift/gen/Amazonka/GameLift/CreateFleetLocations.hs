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
-- Module      : Amazonka.GameLift.CreateFleetLocations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds remote locations to a fleet and begins populating the new locations
-- with EC2 instances. The new instances conform to the fleet\'s instance
-- type, auto-scaling, and other configuration settings.
--
-- This operation cannot be used with fleets that don\'t support remote
-- locations. Fleets can have multiple locations only if they reside in
-- Amazon Web Services Regions that support this feature (see CreateFleet
-- for the complete list) and were created after the feature was released
-- in March 2021.
--
-- To add fleet locations, specify the fleet to be updated and provide a
-- list of one or more locations.
--
-- If successful, this operation returns the list of added locations with
-- their status set to @NEW@. GameLift initiates the process of starting an
-- instance in each added location. You can track the status of each new
-- location by monitoring location creation events using
-- DescribeFleetEvents. Alternatively, you can poll location status by
-- calling DescribeFleetLocationAttributes. After a location status becomes
-- @ACTIVE@, you can adjust the location\'s capacity as needed with
-- UpdateFleetCapacity.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up fleets>
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Multi-location fleets>
--
-- __Related actions__
--
-- CreateFleetLocations | DescribeFleetLocationAttributes |
-- DescribeFleetLocationCapacity | DescribeFleetLocationUtilization |
-- DescribeFleetAttributes | DescribeFleetCapacity |
-- DescribeFleetUtilization | UpdateFleetCapacity | StopFleetActions |
-- DeleteFleetLocations |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.CreateFleetLocations
  ( -- * Creating a Request
    CreateFleetLocations (..),
    newCreateFleetLocations,

    -- * Request Lenses
    createFleetLocations_fleetId,
    createFleetLocations_locations,

    -- * Destructuring the Response
    CreateFleetLocationsResponse (..),
    newCreateFleetLocationsResponse,

    -- * Response Lenses
    createFleetLocationsResponse_fleetId,
    createFleetLocationsResponse_locationStates,
    createFleetLocationsResponse_fleetArn,
    createFleetLocationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newCreateFleetLocations' smart constructor.
data CreateFleetLocations = CreateFleetLocations'
  { -- | A unique identifier for the fleet to add locations to. You can use
    -- either the fleet ID or ARN value.
    fleetId :: Prelude.Text,
    -- | A list of locations to deploy additional instances to and manage as part
    -- of the fleet. You can add any GameLift-supported Amazon Web Services
    -- Region as a remote location, in the form of an Amazon Web Services
    -- Region code such as @us-west-2@.
    locations :: Prelude.NonEmpty LocationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFleetLocations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'createFleetLocations_fleetId' - A unique identifier for the fleet to add locations to. You can use
-- either the fleet ID or ARN value.
--
-- 'locations', 'createFleetLocations_locations' - A list of locations to deploy additional instances to and manage as part
-- of the fleet. You can add any GameLift-supported Amazon Web Services
-- Region as a remote location, in the form of an Amazon Web Services
-- Region code such as @us-west-2@.
newCreateFleetLocations ::
  -- | 'fleetId'
  Prelude.Text ->
  -- | 'locations'
  Prelude.NonEmpty LocationConfiguration ->
  CreateFleetLocations
newCreateFleetLocations pFleetId_ pLocations_ =
  CreateFleetLocations'
    { fleetId = pFleetId_,
      locations = Lens.coerced Lens.# pLocations_
    }

-- | A unique identifier for the fleet to add locations to. You can use
-- either the fleet ID or ARN value.
createFleetLocations_fleetId :: Lens.Lens' CreateFleetLocations Prelude.Text
createFleetLocations_fleetId = Lens.lens (\CreateFleetLocations' {fleetId} -> fleetId) (\s@CreateFleetLocations' {} a -> s {fleetId = a} :: CreateFleetLocations)

-- | A list of locations to deploy additional instances to and manage as part
-- of the fleet. You can add any GameLift-supported Amazon Web Services
-- Region as a remote location, in the form of an Amazon Web Services
-- Region code such as @us-west-2@.
createFleetLocations_locations :: Lens.Lens' CreateFleetLocations (Prelude.NonEmpty LocationConfiguration)
createFleetLocations_locations = Lens.lens (\CreateFleetLocations' {locations} -> locations) (\s@CreateFleetLocations' {} a -> s {locations = a} :: CreateFleetLocations) Prelude.. Lens.coerced

instance Core.AWSRequest CreateFleetLocations where
  type
    AWSResponse CreateFleetLocations =
      CreateFleetLocationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFleetLocationsResponse'
            Prelude.<$> (x Core..?> "FleetId")
            Prelude.<*> (x Core..?> "LocationStates" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "FleetArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFleetLocations where
  hashWithSalt _salt CreateFleetLocations' {..} =
    _salt `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` locations

instance Prelude.NFData CreateFleetLocations where
  rnf CreateFleetLocations' {..} =
    Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf locations

instance Core.ToHeaders CreateFleetLocations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.CreateFleetLocations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateFleetLocations where
  toJSON CreateFleetLocations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetId" Core..= fleetId),
            Prelude.Just ("Locations" Core..= locations)
          ]
      )

instance Core.ToPath CreateFleetLocations where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateFleetLocations where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newCreateFleetLocationsResponse' smart constructor.
data CreateFleetLocationsResponse = CreateFleetLocationsResponse'
  { -- | A unique identifier for the fleet that was updated with new locations.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | The remote locations that are being added to the fleet, and the
    -- life-cycle status of each location. For new locations, the status is set
    -- to @NEW@. During location creation, GameLift updates each location\'s
    -- status as instances are deployed there and prepared for game hosting.
    -- This list does not include the fleet home Region or any remote locations
    -- that were already added to the fleet.
    locationStates :: Prelude.Maybe [LocationState],
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a GameLift fleet resource and uniquely identifies
    -- it. ARNs are unique across all Regions. Format is
    -- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFleetLocationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'createFleetLocationsResponse_fleetId' - A unique identifier for the fleet that was updated with new locations.
--
-- 'locationStates', 'createFleetLocationsResponse_locationStates' - The remote locations that are being added to the fleet, and the
-- life-cycle status of each location. For new locations, the status is set
-- to @NEW@. During location creation, GameLift updates each location\'s
-- status as instances are deployed there and prepared for game hosting.
-- This list does not include the fleet home Region or any remote locations
-- that were already added to the fleet.
--
-- 'fleetArn', 'createFleetLocationsResponse_fleetArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
--
-- 'httpStatus', 'createFleetLocationsResponse_httpStatus' - The response's http status code.
newCreateFleetLocationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFleetLocationsResponse
newCreateFleetLocationsResponse pHttpStatus_ =
  CreateFleetLocationsResponse'
    { fleetId =
        Prelude.Nothing,
      locationStates = Prelude.Nothing,
      fleetArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier for the fleet that was updated with new locations.
createFleetLocationsResponse_fleetId :: Lens.Lens' CreateFleetLocationsResponse (Prelude.Maybe Prelude.Text)
createFleetLocationsResponse_fleetId = Lens.lens (\CreateFleetLocationsResponse' {fleetId} -> fleetId) (\s@CreateFleetLocationsResponse' {} a -> s {fleetId = a} :: CreateFleetLocationsResponse)

-- | The remote locations that are being added to the fleet, and the
-- life-cycle status of each location. For new locations, the status is set
-- to @NEW@. During location creation, GameLift updates each location\'s
-- status as instances are deployed there and prepared for game hosting.
-- This list does not include the fleet home Region or any remote locations
-- that were already added to the fleet.
createFleetLocationsResponse_locationStates :: Lens.Lens' CreateFleetLocationsResponse (Prelude.Maybe [LocationState])
createFleetLocationsResponse_locationStates = Lens.lens (\CreateFleetLocationsResponse' {locationStates} -> locationStates) (\s@CreateFleetLocationsResponse' {} a -> s {locationStates = a} :: CreateFleetLocationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
createFleetLocationsResponse_fleetArn :: Lens.Lens' CreateFleetLocationsResponse (Prelude.Maybe Prelude.Text)
createFleetLocationsResponse_fleetArn = Lens.lens (\CreateFleetLocationsResponse' {fleetArn} -> fleetArn) (\s@CreateFleetLocationsResponse' {} a -> s {fleetArn = a} :: CreateFleetLocationsResponse)

-- | The response's http status code.
createFleetLocationsResponse_httpStatus :: Lens.Lens' CreateFleetLocationsResponse Prelude.Int
createFleetLocationsResponse_httpStatus = Lens.lens (\CreateFleetLocationsResponse' {httpStatus} -> httpStatus) (\s@CreateFleetLocationsResponse' {} a -> s {httpStatus = a} :: CreateFleetLocationsResponse)

instance Prelude.NFData CreateFleetLocationsResponse where
  rnf CreateFleetLocationsResponse' {..} =
    Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf locationStates
      `Prelude.seq` Prelude.rnf fleetArn
      `Prelude.seq` Prelude.rnf httpStatus
