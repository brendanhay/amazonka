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

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDeleteFleet' smart constructor.
data DeleteFleet = DeleteFleet'
  { -- | A unique identifier for a fleet to be deleted. You can use either the
    -- fleet ID or ARN value.
    fleetId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteFleet
newDeleteFleet pFleetId_ =
  DeleteFleet' {fleetId = pFleetId_}

-- | A unique identifier for a fleet to be deleted. You can use either the
-- fleet ID or ARN value.
deleteFleet_fleetId :: Lens.Lens' DeleteFleet Core.Text
deleteFleet_fleetId = Lens.lens (\DeleteFleet' {fleetId} -> fleetId) (\s@DeleteFleet' {} a -> s {fleetId = a} :: DeleteFleet)

instance Core.AWSRequest DeleteFleet where
  type AWSResponse DeleteFleet = DeleteFleetResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull DeleteFleetResponse'

instance Core.Hashable DeleteFleet

instance Core.NFData DeleteFleet

instance Core.ToHeaders DeleteFleet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.DeleteFleet" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteFleet where
  toJSON DeleteFleet' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("FleetId" Core..= fleetId)]
      )

instance Core.ToPath DeleteFleet where
  toPath = Core.const "/"

instance Core.ToQuery DeleteFleet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteFleetResponse' smart constructor.
data DeleteFleetResponse = DeleteFleetResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteFleetResponse ::
  DeleteFleetResponse
newDeleteFleetResponse = DeleteFleetResponse'

instance Core.NFData DeleteFleetResponse
