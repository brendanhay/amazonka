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
-- Module      : Amazonka.IoTFleetWise.AssociateVehicleFleet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds, or associates, a vehicle with a fleet.
module Amazonka.IoTFleetWise.AssociateVehicleFleet
  ( -- * Creating a Request
    AssociateVehicleFleet (..),
    newAssociateVehicleFleet,

    -- * Request Lenses
    associateVehicleFleet_vehicleName,
    associateVehicleFleet_fleetId,

    -- * Destructuring the Response
    AssociateVehicleFleetResponse (..),
    newAssociateVehicleFleetResponse,

    -- * Response Lenses
    associateVehicleFleetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateVehicleFleet' smart constructor.
data AssociateVehicleFleet = AssociateVehicleFleet'
  { -- | The unique ID of the vehicle to associate with the fleet.
    vehicleName :: Prelude.Text,
    -- | The ID of a fleet.
    fleetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateVehicleFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vehicleName', 'associateVehicleFleet_vehicleName' - The unique ID of the vehicle to associate with the fleet.
--
-- 'fleetId', 'associateVehicleFleet_fleetId' - The ID of a fleet.
newAssociateVehicleFleet ::
  -- | 'vehicleName'
  Prelude.Text ->
  -- | 'fleetId'
  Prelude.Text ->
  AssociateVehicleFleet
newAssociateVehicleFleet pVehicleName_ pFleetId_ =
  AssociateVehicleFleet'
    { vehicleName = pVehicleName_,
      fleetId = pFleetId_
    }

-- | The unique ID of the vehicle to associate with the fleet.
associateVehicleFleet_vehicleName :: Lens.Lens' AssociateVehicleFleet Prelude.Text
associateVehicleFleet_vehicleName = Lens.lens (\AssociateVehicleFleet' {vehicleName} -> vehicleName) (\s@AssociateVehicleFleet' {} a -> s {vehicleName = a} :: AssociateVehicleFleet)

-- | The ID of a fleet.
associateVehicleFleet_fleetId :: Lens.Lens' AssociateVehicleFleet Prelude.Text
associateVehicleFleet_fleetId = Lens.lens (\AssociateVehicleFleet' {fleetId} -> fleetId) (\s@AssociateVehicleFleet' {} a -> s {fleetId = a} :: AssociateVehicleFleet)

instance Core.AWSRequest AssociateVehicleFleet where
  type
    AWSResponse AssociateVehicleFleet =
      AssociateVehicleFleetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateVehicleFleetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateVehicleFleet where
  hashWithSalt _salt AssociateVehicleFleet' {..} =
    _salt
      `Prelude.hashWithSalt` vehicleName
      `Prelude.hashWithSalt` fleetId

instance Prelude.NFData AssociateVehicleFleet where
  rnf AssociateVehicleFleet' {..} =
    Prelude.rnf vehicleName
      `Prelude.seq` Prelude.rnf fleetId

instance Data.ToHeaders AssociateVehicleFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.AssociateVehicleFleet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateVehicleFleet where
  toJSON AssociateVehicleFleet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("vehicleName" Data..= vehicleName),
            Prelude.Just ("fleetId" Data..= fleetId)
          ]
      )

instance Data.ToPath AssociateVehicleFleet where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateVehicleFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateVehicleFleetResponse' smart constructor.
data AssociateVehicleFleetResponse = AssociateVehicleFleetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateVehicleFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateVehicleFleetResponse_httpStatus' - The response's http status code.
newAssociateVehicleFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateVehicleFleetResponse
newAssociateVehicleFleetResponse pHttpStatus_ =
  AssociateVehicleFleetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateVehicleFleetResponse_httpStatus :: Lens.Lens' AssociateVehicleFleetResponse Prelude.Int
associateVehicleFleetResponse_httpStatus = Lens.lens (\AssociateVehicleFleetResponse' {httpStatus} -> httpStatus) (\s@AssociateVehicleFleetResponse' {} a -> s {httpStatus = a} :: AssociateVehicleFleetResponse)

instance Prelude.NFData AssociateVehicleFleetResponse where
  rnf AssociateVehicleFleetResponse' {..} =
    Prelude.rnf httpStatus
