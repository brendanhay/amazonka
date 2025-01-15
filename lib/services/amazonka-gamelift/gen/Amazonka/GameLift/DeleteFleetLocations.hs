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
-- Module      : Amazonka.GameLift.DeleteFleetLocations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes locations from a multi-location fleet. When deleting a location,
-- all game server process and all instances that are still active in the
-- location are shut down.
--
-- To delete fleet locations, identify the fleet ID and provide a list of
-- the locations to be deleted.
--
-- If successful, GameLift sets the location status to @DELETING@, and
-- begins to shut down existing server processes and terminate instances in
-- each location being deleted. When completed, the location status changes
-- to @TERMINATED@.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift fleets>
module Amazonka.GameLift.DeleteFleetLocations
  ( -- * Creating a Request
    DeleteFleetLocations (..),
    newDeleteFleetLocations,

    -- * Request Lenses
    deleteFleetLocations_fleetId,
    deleteFleetLocations_locations,

    -- * Destructuring the Response
    DeleteFleetLocationsResponse (..),
    newDeleteFleetLocationsResponse,

    -- * Response Lenses
    deleteFleetLocationsResponse_fleetArn,
    deleteFleetLocationsResponse_fleetId,
    deleteFleetLocationsResponse_locationStates,
    deleteFleetLocationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFleetLocations' smart constructor.
data DeleteFleetLocations = DeleteFleetLocations'
  { -- | A unique identifier for the fleet to delete locations for. You can use
    -- either the fleet ID or ARN value.
    fleetId :: Prelude.Text,
    -- | The list of fleet locations to delete. Specify locations in the form of
    -- an Amazon Web Services Region code, such as @us-west-2@.
    locations :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFleetLocations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'deleteFleetLocations_fleetId' - A unique identifier for the fleet to delete locations for. You can use
-- either the fleet ID or ARN value.
--
-- 'locations', 'deleteFleetLocations_locations' - The list of fleet locations to delete. Specify locations in the form of
-- an Amazon Web Services Region code, such as @us-west-2@.
newDeleteFleetLocations ::
  -- | 'fleetId'
  Prelude.Text ->
  -- | 'locations'
  Prelude.NonEmpty Prelude.Text ->
  DeleteFleetLocations
newDeleteFleetLocations pFleetId_ pLocations_ =
  DeleteFleetLocations'
    { fleetId = pFleetId_,
      locations = Lens.coerced Lens.# pLocations_
    }

-- | A unique identifier for the fleet to delete locations for. You can use
-- either the fleet ID or ARN value.
deleteFleetLocations_fleetId :: Lens.Lens' DeleteFleetLocations Prelude.Text
deleteFleetLocations_fleetId = Lens.lens (\DeleteFleetLocations' {fleetId} -> fleetId) (\s@DeleteFleetLocations' {} a -> s {fleetId = a} :: DeleteFleetLocations)

-- | The list of fleet locations to delete. Specify locations in the form of
-- an Amazon Web Services Region code, such as @us-west-2@.
deleteFleetLocations_locations :: Lens.Lens' DeleteFleetLocations (Prelude.NonEmpty Prelude.Text)
deleteFleetLocations_locations = Lens.lens (\DeleteFleetLocations' {locations} -> locations) (\s@DeleteFleetLocations' {} a -> s {locations = a} :: DeleteFleetLocations) Prelude.. Lens.coerced

instance Core.AWSRequest DeleteFleetLocations where
  type
    AWSResponse DeleteFleetLocations =
      DeleteFleetLocationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteFleetLocationsResponse'
            Prelude.<$> (x Data..?> "FleetArn")
            Prelude.<*> (x Data..?> "FleetId")
            Prelude.<*> (x Data..?> "LocationStates" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFleetLocations where
  hashWithSalt _salt DeleteFleetLocations' {..} =
    _salt
      `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` locations

instance Prelude.NFData DeleteFleetLocations where
  rnf DeleteFleetLocations' {..} =
    Prelude.rnf fleetId `Prelude.seq`
      Prelude.rnf locations

instance Data.ToHeaders DeleteFleetLocations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.DeleteFleetLocations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteFleetLocations where
  toJSON DeleteFleetLocations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetId" Data..= fleetId),
            Prelude.Just ("Locations" Data..= locations)
          ]
      )

instance Data.ToPath DeleteFleetLocations where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteFleetLocations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFleetLocationsResponse' smart constructor.
data DeleteFleetLocationsResponse = DeleteFleetLocationsResponse'
  { -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a GameLift fleet resource and uniquely identifies
    -- it. ARNs are unique across all Regions. Format is
    -- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the fleet that location attributes are being
    -- deleted for.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | The remote locations that are being deleted, with each location status
    -- set to @DELETING@.
    locationStates :: Prelude.Maybe [LocationState],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFleetLocationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetArn', 'deleteFleetLocationsResponse_fleetArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
--
-- 'fleetId', 'deleteFleetLocationsResponse_fleetId' - A unique identifier for the fleet that location attributes are being
-- deleted for.
--
-- 'locationStates', 'deleteFleetLocationsResponse_locationStates' - The remote locations that are being deleted, with each location status
-- set to @DELETING@.
--
-- 'httpStatus', 'deleteFleetLocationsResponse_httpStatus' - The response's http status code.
newDeleteFleetLocationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteFleetLocationsResponse
newDeleteFleetLocationsResponse pHttpStatus_ =
  DeleteFleetLocationsResponse'
    { fleetArn =
        Prelude.Nothing,
      fleetId = Prelude.Nothing,
      locationStates = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
deleteFleetLocationsResponse_fleetArn :: Lens.Lens' DeleteFleetLocationsResponse (Prelude.Maybe Prelude.Text)
deleteFleetLocationsResponse_fleetArn = Lens.lens (\DeleteFleetLocationsResponse' {fleetArn} -> fleetArn) (\s@DeleteFleetLocationsResponse' {} a -> s {fleetArn = a} :: DeleteFleetLocationsResponse)

-- | A unique identifier for the fleet that location attributes are being
-- deleted for.
deleteFleetLocationsResponse_fleetId :: Lens.Lens' DeleteFleetLocationsResponse (Prelude.Maybe Prelude.Text)
deleteFleetLocationsResponse_fleetId = Lens.lens (\DeleteFleetLocationsResponse' {fleetId} -> fleetId) (\s@DeleteFleetLocationsResponse' {} a -> s {fleetId = a} :: DeleteFleetLocationsResponse)

-- | The remote locations that are being deleted, with each location status
-- set to @DELETING@.
deleteFleetLocationsResponse_locationStates :: Lens.Lens' DeleteFleetLocationsResponse (Prelude.Maybe [LocationState])
deleteFleetLocationsResponse_locationStates = Lens.lens (\DeleteFleetLocationsResponse' {locationStates} -> locationStates) (\s@DeleteFleetLocationsResponse' {} a -> s {locationStates = a} :: DeleteFleetLocationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deleteFleetLocationsResponse_httpStatus :: Lens.Lens' DeleteFleetLocationsResponse Prelude.Int
deleteFleetLocationsResponse_httpStatus = Lens.lens (\DeleteFleetLocationsResponse' {httpStatus} -> httpStatus) (\s@DeleteFleetLocationsResponse' {} a -> s {httpStatus = a} :: DeleteFleetLocationsResponse)

instance Prelude.NFData DeleteFleetLocationsResponse where
  rnf DeleteFleetLocationsResponse' {..} =
    Prelude.rnf fleetArn `Prelude.seq`
      Prelude.rnf fleetId `Prelude.seq`
        Prelude.rnf locationStates `Prelude.seq`
          Prelude.rnf httpStatus
