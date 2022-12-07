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
-- Module      : Amazonka.GameLift.DeleteFleet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all resources and information related a fleet. Any current fleet
-- instances, including those in remote locations, are shut down. You
-- don\'t need to call @DeleteFleetLocations@ separately.
--
-- If the fleet being deleted has a VPC peering connection, you first need
-- to get a valid authorization (good for 24 hours) by calling
-- CreateVpcPeeringAuthorization. You do not need to explicitly delete the
-- VPC peering connection--this is done as part of the delete fleet
-- process.
--
-- To delete a fleet, specify the fleet ID to be terminated. During the
-- deletion process the fleet status is changed to @DELETING@. When
-- completed, the status switches to @TERMINATED@ and the fleet event
-- @FLEET_DELETED@ is sent.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift Fleets>
--
-- __Related actions__
--
-- CreateFleetLocations | UpdateFleetAttributes | UpdateFleetCapacity |
-- UpdateFleetPortSettings | UpdateRuntimeConfiguration | StopFleetActions
-- | StartFleetActions | PutScalingPolicy | DeleteFleet |
-- DeleteFleetLocations | DeleteScalingPolicy |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.DeleteFleet
  ( -- * Creating a Request
    DeleteFleet (..),
    newDeleteFleet,

    -- * Request Lenses
    deleteFleet_fleetId,

    -- * Destructuring the Response
    DeleteFleetResponse (..),
    newDeleteFleetResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDeleteFleet' smart constructor.
data DeleteFleet = DeleteFleet'
  { -- | A unique identifier for the fleet to be deleted. You can use either the
    -- fleet ID or ARN value.
    fleetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'deleteFleet_fleetId' - A unique identifier for the fleet to be deleted. You can use either the
-- fleet ID or ARN value.
newDeleteFleet ::
  -- | 'fleetId'
  Prelude.Text ->
  DeleteFleet
newDeleteFleet pFleetId_ =
  DeleteFleet' {fleetId = pFleetId_}

-- | A unique identifier for the fleet to be deleted. You can use either the
-- fleet ID or ARN value.
deleteFleet_fleetId :: Lens.Lens' DeleteFleet Prelude.Text
deleteFleet_fleetId = Lens.lens (\DeleteFleet' {fleetId} -> fleetId) (\s@DeleteFleet' {} a -> s {fleetId = a} :: DeleteFleet)

instance Core.AWSRequest DeleteFleet where
  type AWSResponse DeleteFleet = DeleteFleetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DeleteFleetResponse'

instance Prelude.Hashable DeleteFleet where
  hashWithSalt _salt DeleteFleet' {..} =
    _salt `Prelude.hashWithSalt` fleetId

instance Prelude.NFData DeleteFleet where
  rnf DeleteFleet' {..} = Prelude.rnf fleetId

instance Data.ToHeaders DeleteFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.DeleteFleet" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteFleet where
  toJSON DeleteFleet' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("FleetId" Data..= fleetId)]
      )

instance Data.ToPath DeleteFleet where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFleetResponse' smart constructor.
data DeleteFleetResponse = DeleteFleetResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteFleetResponse ::
  DeleteFleetResponse
newDeleteFleetResponse = DeleteFleetResponse'

instance Prelude.NFData DeleteFleetResponse where
  rnf _ = ()
