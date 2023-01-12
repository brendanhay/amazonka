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
-- Module      : Amazonka.IoTFleetWise.BatchUpdateVehicle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a group, or batch, of vehicles.
--
-- You must specify a decoder manifest and a vehicle model (model manifest)
-- for each vehicle.
--
-- For more information, see
-- <https://docs.aws.amazon.com/iot-fleetwise/latest/developerguide/update-vehicles-cli.html Update multiple vehicles (AWS CLI)>
-- in the /Amazon Web Services IoT FleetWise Developer Guide/.
module Amazonka.IoTFleetWise.BatchUpdateVehicle
  ( -- * Creating a Request
    BatchUpdateVehicle (..),
    newBatchUpdateVehicle,

    -- * Request Lenses
    batchUpdateVehicle_vehicles,

    -- * Destructuring the Response
    BatchUpdateVehicleResponse (..),
    newBatchUpdateVehicleResponse,

    -- * Response Lenses
    batchUpdateVehicleResponse_errors,
    batchUpdateVehicleResponse_vehicles,
    batchUpdateVehicleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchUpdateVehicle' smart constructor.
data BatchUpdateVehicle = BatchUpdateVehicle'
  { -- | A list of information about the vehicles to update. For more
    -- information, see the API data type.
    vehicles :: [UpdateVehicleRequestItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateVehicle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vehicles', 'batchUpdateVehicle_vehicles' - A list of information about the vehicles to update. For more
-- information, see the API data type.
newBatchUpdateVehicle ::
  BatchUpdateVehicle
newBatchUpdateVehicle =
  BatchUpdateVehicle' {vehicles = Prelude.mempty}

-- | A list of information about the vehicles to update. For more
-- information, see the API data type.
batchUpdateVehicle_vehicles :: Lens.Lens' BatchUpdateVehicle [UpdateVehicleRequestItem]
batchUpdateVehicle_vehicles = Lens.lens (\BatchUpdateVehicle' {vehicles} -> vehicles) (\s@BatchUpdateVehicle' {} a -> s {vehicles = a} :: BatchUpdateVehicle) Prelude.. Lens.coerced

instance Core.AWSRequest BatchUpdateVehicle where
  type
    AWSResponse BatchUpdateVehicle =
      BatchUpdateVehicleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchUpdateVehicleResponse'
            Prelude.<$> (x Data..?> "errors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "vehicles" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchUpdateVehicle where
  hashWithSalt _salt BatchUpdateVehicle' {..} =
    _salt `Prelude.hashWithSalt` vehicles

instance Prelude.NFData BatchUpdateVehicle where
  rnf BatchUpdateVehicle' {..} = Prelude.rnf vehicles

instance Data.ToHeaders BatchUpdateVehicle where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.BatchUpdateVehicle" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchUpdateVehicle where
  toJSON BatchUpdateVehicle' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("vehicles" Data..= vehicles)]
      )

instance Data.ToPath BatchUpdateVehicle where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchUpdateVehicle where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchUpdateVehicleResponse' smart constructor.
data BatchUpdateVehicleResponse = BatchUpdateVehicleResponse'
  { -- | A list of information about errors returned while updating a batch of
    -- vehicles, or, if there aren\'t any errors, an empty list.
    errors :: Prelude.Maybe [UpdateVehicleError],
    -- | A list of information about the batch of updated vehicles.
    --
    -- This list contains only unique IDs for the vehicles that were updated.
    vehicles :: Prelude.Maybe [UpdateVehicleResponseItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateVehicleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'batchUpdateVehicleResponse_errors' - A list of information about errors returned while updating a batch of
-- vehicles, or, if there aren\'t any errors, an empty list.
--
-- 'vehicles', 'batchUpdateVehicleResponse_vehicles' - A list of information about the batch of updated vehicles.
--
-- This list contains only unique IDs for the vehicles that were updated.
--
-- 'httpStatus', 'batchUpdateVehicleResponse_httpStatus' - The response's http status code.
newBatchUpdateVehicleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchUpdateVehicleResponse
newBatchUpdateVehicleResponse pHttpStatus_ =
  BatchUpdateVehicleResponse'
    { errors =
        Prelude.Nothing,
      vehicles = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of information about errors returned while updating a batch of
-- vehicles, or, if there aren\'t any errors, an empty list.
batchUpdateVehicleResponse_errors :: Lens.Lens' BatchUpdateVehicleResponse (Prelude.Maybe [UpdateVehicleError])
batchUpdateVehicleResponse_errors = Lens.lens (\BatchUpdateVehicleResponse' {errors} -> errors) (\s@BatchUpdateVehicleResponse' {} a -> s {errors = a} :: BatchUpdateVehicleResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of information about the batch of updated vehicles.
--
-- This list contains only unique IDs for the vehicles that were updated.
batchUpdateVehicleResponse_vehicles :: Lens.Lens' BatchUpdateVehicleResponse (Prelude.Maybe [UpdateVehicleResponseItem])
batchUpdateVehicleResponse_vehicles = Lens.lens (\BatchUpdateVehicleResponse' {vehicles} -> vehicles) (\s@BatchUpdateVehicleResponse' {} a -> s {vehicles = a} :: BatchUpdateVehicleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchUpdateVehicleResponse_httpStatus :: Lens.Lens' BatchUpdateVehicleResponse Prelude.Int
batchUpdateVehicleResponse_httpStatus = Lens.lens (\BatchUpdateVehicleResponse' {httpStatus} -> httpStatus) (\s@BatchUpdateVehicleResponse' {} a -> s {httpStatus = a} :: BatchUpdateVehicleResponse)

instance Prelude.NFData BatchUpdateVehicleResponse where
  rnf BatchUpdateVehicleResponse' {..} =
    Prelude.rnf errors
      `Prelude.seq` Prelude.rnf vehicles
      `Prelude.seq` Prelude.rnf httpStatus
