{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.GameLift.DeleteFleet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes everything related to a fleet. Before deleting a fleet, you must
-- set the fleet\'s desired capacity to zero. See UpdateFleetCapacity.
--
-- If the fleet being deleted has a VPC peering connection, you first need
-- to get a valid authorization (good for 24 hours) by calling
-- CreateVpcPeeringAuthorization. You do not need to explicitly delete the
-- VPC peering connection--this is done as part of the delete fleet
-- process.
--
-- This operation removes the fleet and its resources. Once a fleet is
-- deleted, you can no longer use any of the resource in that fleet.
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
-- -   UpdateFleetAttributes
--
-- -   StartFleetActions or StopFleetActions
module Network.AWS.GameLift.DeleteFleet
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

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDeleteFleet' smart constructor.
data DeleteFleet = DeleteFleet'
  { -- | A unique identifier for a fleet to be deleted. You can use either the
    -- fleet ID or ARN value.
    fleetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'deleteFleet_fleetId' - A unique identifier for a fleet to be deleted. You can use either the
-- fleet ID or ARN value.
newDeleteFleet ::
  -- | 'fleetId'
  Prelude.Text ->
  DeleteFleet
newDeleteFleet pFleetId_ =
  DeleteFleet' {fleetId = pFleetId_}

-- | A unique identifier for a fleet to be deleted. You can use either the
-- fleet ID or ARN value.
deleteFleet_fleetId :: Lens.Lens' DeleteFleet Prelude.Text
deleteFleet_fleetId = Lens.lens (\DeleteFleet' {fleetId} -> fleetId) (\s@DeleteFleet' {} a -> s {fleetId = a} :: DeleteFleet)

instance Prelude.AWSRequest DeleteFleet where
  type Rs DeleteFleet = DeleteFleetResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull DeleteFleetResponse'

instance Prelude.Hashable DeleteFleet

instance Prelude.NFData DeleteFleet

instance Prelude.ToHeaders DeleteFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("GameLift.DeleteFleet" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteFleet where
  toJSON DeleteFleet' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("FleetId" Prelude..= fleetId)]
      )

instance Prelude.ToPath DeleteFleet where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFleetResponse' smart constructor.
data DeleteFleetResponse = DeleteFleetResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteFleetResponse ::
  DeleteFleetResponse
newDeleteFleetResponse = DeleteFleetResponse'

instance Prelude.NFData DeleteFleetResponse
