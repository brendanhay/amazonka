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
-- Module      : Amazonka.IoTFleetWise.DisassociateVehicleFleet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes, or disassociates, a vehicle from a fleet. Disassociating a
-- vehicle from a fleet doesn\'t delete the vehicle.
--
-- If the vehicle is successfully dissociated from a fleet, Amazon Web
-- Services IoT FleetWise sends back an HTTP 200 response with an empty
-- body.
module Amazonka.IoTFleetWise.DisassociateVehicleFleet
  ( -- * Creating a Request
    DisassociateVehicleFleet (..),
    newDisassociateVehicleFleet,

    -- * Request Lenses
    disassociateVehicleFleet_vehicleName,
    disassociateVehicleFleet_fleetId,

    -- * Destructuring the Response
    DisassociateVehicleFleetResponse (..),
    newDisassociateVehicleFleetResponse,

    -- * Response Lenses
    disassociateVehicleFleetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateVehicleFleet' smart constructor.
data DisassociateVehicleFleet = DisassociateVehicleFleet'
  { -- | The unique ID of the vehicle to disassociate from the fleet.
    vehicleName :: Prelude.Text,
    -- | The unique ID of a fleet.
    fleetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateVehicleFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vehicleName', 'disassociateVehicleFleet_vehicleName' - The unique ID of the vehicle to disassociate from the fleet.
--
-- 'fleetId', 'disassociateVehicleFleet_fleetId' - The unique ID of a fleet.
newDisassociateVehicleFleet ::
  -- | 'vehicleName'
  Prelude.Text ->
  -- | 'fleetId'
  Prelude.Text ->
  DisassociateVehicleFleet
newDisassociateVehicleFleet pVehicleName_ pFleetId_ =
  DisassociateVehicleFleet'
    { vehicleName =
        pVehicleName_,
      fleetId = pFleetId_
    }

-- | The unique ID of the vehicle to disassociate from the fleet.
disassociateVehicleFleet_vehicleName :: Lens.Lens' DisassociateVehicleFleet Prelude.Text
disassociateVehicleFleet_vehicleName = Lens.lens (\DisassociateVehicleFleet' {vehicleName} -> vehicleName) (\s@DisassociateVehicleFleet' {} a -> s {vehicleName = a} :: DisassociateVehicleFleet)

-- | The unique ID of a fleet.
disassociateVehicleFleet_fleetId :: Lens.Lens' DisassociateVehicleFleet Prelude.Text
disassociateVehicleFleet_fleetId = Lens.lens (\DisassociateVehicleFleet' {fleetId} -> fleetId) (\s@DisassociateVehicleFleet' {} a -> s {fleetId = a} :: DisassociateVehicleFleet)

instance Core.AWSRequest DisassociateVehicleFleet where
  type
    AWSResponse DisassociateVehicleFleet =
      DisassociateVehicleFleetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateVehicleFleetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateVehicleFleet where
  hashWithSalt _salt DisassociateVehicleFleet' {..} =
    _salt `Prelude.hashWithSalt` vehicleName
      `Prelude.hashWithSalt` fleetId

instance Prelude.NFData DisassociateVehicleFleet where
  rnf DisassociateVehicleFleet' {..} =
    Prelude.rnf vehicleName
      `Prelude.seq` Prelude.rnf fleetId

instance Data.ToHeaders DisassociateVehicleFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.DisassociateVehicleFleet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateVehicleFleet where
  toJSON DisassociateVehicleFleet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("vehicleName" Data..= vehicleName),
            Prelude.Just ("fleetId" Data..= fleetId)
          ]
      )

instance Data.ToPath DisassociateVehicleFleet where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateVehicleFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateVehicleFleetResponse' smart constructor.
data DisassociateVehicleFleetResponse = DisassociateVehicleFleetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateVehicleFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateVehicleFleetResponse_httpStatus' - The response's http status code.
newDisassociateVehicleFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateVehicleFleetResponse
newDisassociateVehicleFleetResponse pHttpStatus_ =
  DisassociateVehicleFleetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateVehicleFleetResponse_httpStatus :: Lens.Lens' DisassociateVehicleFleetResponse Prelude.Int
disassociateVehicleFleetResponse_httpStatus = Lens.lens (\DisassociateVehicleFleetResponse' {httpStatus} -> httpStatus) (\s@DisassociateVehicleFleetResponse' {} a -> s {httpStatus = a} :: DisassociateVehicleFleetResponse)

instance
  Prelude.NFData
    DisassociateVehicleFleetResponse
  where
  rnf DisassociateVehicleFleetResponse' {..} =
    Prelude.rnf httpStatus
