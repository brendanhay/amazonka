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
-- Module      : Amazonka.IoTFleetWise.BatchCreateVehicle
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a group, or batch, of vehicles.
--
-- You must specify a decoder manifest and a vehicle model (model manifest)
-- for each vehicle.
--
-- For more information, see
-- <https://docs.aws.amazon.com/iot-fleetwise/latest/developerguide/create-vehicles-cli.html Create multiple vehicles (AWS CLI)>
-- in the /Amazon Web Services IoT FleetWise Developer Guide/.
module Amazonka.IoTFleetWise.BatchCreateVehicle
  ( -- * Creating a Request
    BatchCreateVehicle (..),
    newBatchCreateVehicle,

    -- * Request Lenses
    batchCreateVehicle_vehicles,

    -- * Destructuring the Response
    BatchCreateVehicleResponse (..),
    newBatchCreateVehicleResponse,

    -- * Response Lenses
    batchCreateVehicleResponse_errors,
    batchCreateVehicleResponse_vehicles,
    batchCreateVehicleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchCreateVehicle' smart constructor.
data BatchCreateVehicle = BatchCreateVehicle'
  { -- | A list of information about each vehicle to create. For more
    -- information, see the API data type.
    vehicles :: [CreateVehicleRequestItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchCreateVehicle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vehicles', 'batchCreateVehicle_vehicles' - A list of information about each vehicle to create. For more
-- information, see the API data type.
newBatchCreateVehicle ::
  BatchCreateVehicle
newBatchCreateVehicle =
  BatchCreateVehicle' {vehicles = Prelude.mempty}

-- | A list of information about each vehicle to create. For more
-- information, see the API data type.
batchCreateVehicle_vehicles :: Lens.Lens' BatchCreateVehicle [CreateVehicleRequestItem]
batchCreateVehicle_vehicles = Lens.lens (\BatchCreateVehicle' {vehicles} -> vehicles) (\s@BatchCreateVehicle' {} a -> s {vehicles = a} :: BatchCreateVehicle) Prelude.. Lens.coerced

instance Core.AWSRequest BatchCreateVehicle where
  type
    AWSResponse BatchCreateVehicle =
      BatchCreateVehicleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchCreateVehicleResponse'
            Prelude.<$> (x Data..?> "errors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "vehicles" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchCreateVehicle where
  hashWithSalt _salt BatchCreateVehicle' {..} =
    _salt `Prelude.hashWithSalt` vehicles

instance Prelude.NFData BatchCreateVehicle where
  rnf BatchCreateVehicle' {..} = Prelude.rnf vehicles

instance Data.ToHeaders BatchCreateVehicle where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.BatchCreateVehicle" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchCreateVehicle where
  toJSON BatchCreateVehicle' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("vehicles" Data..= vehicles)]
      )

instance Data.ToPath BatchCreateVehicle where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchCreateVehicle where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchCreateVehicleResponse' smart constructor.
data BatchCreateVehicleResponse = BatchCreateVehicleResponse'
  { -- | A list of information about creation errors, or an empty list if there
    -- aren\'t any errors.
    errors :: Prelude.Maybe [CreateVehicleError],
    -- | A list of information about a batch of created vehicles. For more
    -- information, see the API data type.
    vehicles :: Prelude.Maybe [CreateVehicleResponseItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchCreateVehicleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'batchCreateVehicleResponse_errors' - A list of information about creation errors, or an empty list if there
-- aren\'t any errors.
--
-- 'vehicles', 'batchCreateVehicleResponse_vehicles' - A list of information about a batch of created vehicles. For more
-- information, see the API data type.
--
-- 'httpStatus', 'batchCreateVehicleResponse_httpStatus' - The response's http status code.
newBatchCreateVehicleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchCreateVehicleResponse
newBatchCreateVehicleResponse pHttpStatus_ =
  BatchCreateVehicleResponse'
    { errors =
        Prelude.Nothing,
      vehicles = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of information about creation errors, or an empty list if there
-- aren\'t any errors.
batchCreateVehicleResponse_errors :: Lens.Lens' BatchCreateVehicleResponse (Prelude.Maybe [CreateVehicleError])
batchCreateVehicleResponse_errors = Lens.lens (\BatchCreateVehicleResponse' {errors} -> errors) (\s@BatchCreateVehicleResponse' {} a -> s {errors = a} :: BatchCreateVehicleResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of information about a batch of created vehicles. For more
-- information, see the API data type.
batchCreateVehicleResponse_vehicles :: Lens.Lens' BatchCreateVehicleResponse (Prelude.Maybe [CreateVehicleResponseItem])
batchCreateVehicleResponse_vehicles = Lens.lens (\BatchCreateVehicleResponse' {vehicles} -> vehicles) (\s@BatchCreateVehicleResponse' {} a -> s {vehicles = a} :: BatchCreateVehicleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchCreateVehicleResponse_httpStatus :: Lens.Lens' BatchCreateVehicleResponse Prelude.Int
batchCreateVehicleResponse_httpStatus = Lens.lens (\BatchCreateVehicleResponse' {httpStatus} -> httpStatus) (\s@BatchCreateVehicleResponse' {} a -> s {httpStatus = a} :: BatchCreateVehicleResponse)

instance Prelude.NFData BatchCreateVehicleResponse where
  rnf BatchCreateVehicleResponse' {..} =
    Prelude.rnf errors
      `Prelude.seq` Prelude.rnf vehicles
      `Prelude.seq` Prelude.rnf httpStatus
