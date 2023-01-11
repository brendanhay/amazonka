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
-- Module      : Amazonka.IoTFleetWise.DeleteVehicle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a vehicle and removes it from any campaigns.
--
-- If the vehicle is successfully deleted, Amazon Web Services IoT
-- FleetWise sends back an HTTP 200 response with an empty body.
module Amazonka.IoTFleetWise.DeleteVehicle
  ( -- * Creating a Request
    DeleteVehicle (..),
    newDeleteVehicle,

    -- * Request Lenses
    deleteVehicle_vehicleName,

    -- * Destructuring the Response
    DeleteVehicleResponse (..),
    newDeleteVehicleResponse,

    -- * Response Lenses
    deleteVehicleResponse_httpStatus,
    deleteVehicleResponse_vehicleName,
    deleteVehicleResponse_arn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVehicle' smart constructor.
data DeleteVehicle = DeleteVehicle'
  { -- | The ID of the vehicle to delete.
    vehicleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVehicle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vehicleName', 'deleteVehicle_vehicleName' - The ID of the vehicle to delete.
newDeleteVehicle ::
  -- | 'vehicleName'
  Prelude.Text ->
  DeleteVehicle
newDeleteVehicle pVehicleName_ =
  DeleteVehicle' {vehicleName = pVehicleName_}

-- | The ID of the vehicle to delete.
deleteVehicle_vehicleName :: Lens.Lens' DeleteVehicle Prelude.Text
deleteVehicle_vehicleName = Lens.lens (\DeleteVehicle' {vehicleName} -> vehicleName) (\s@DeleteVehicle' {} a -> s {vehicleName = a} :: DeleteVehicle)

instance Core.AWSRequest DeleteVehicle where
  type
    AWSResponse DeleteVehicle =
      DeleteVehicleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteVehicleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "vehicleName")
            Prelude.<*> (x Data..:> "arn")
      )

instance Prelude.Hashable DeleteVehicle where
  hashWithSalt _salt DeleteVehicle' {..} =
    _salt `Prelude.hashWithSalt` vehicleName

instance Prelude.NFData DeleteVehicle where
  rnf DeleteVehicle' {..} = Prelude.rnf vehicleName

instance Data.ToHeaders DeleteVehicle where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.DeleteVehicle" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteVehicle where
  toJSON DeleteVehicle' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("vehicleName" Data..= vehicleName)]
      )

instance Data.ToPath DeleteVehicle where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteVehicle where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVehicleResponse' smart constructor.
data DeleteVehicleResponse = DeleteVehicleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the deleted vehicle.
    vehicleName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the deleted vehicle.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVehicleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteVehicleResponse_httpStatus' - The response's http status code.
--
-- 'vehicleName', 'deleteVehicleResponse_vehicleName' - The ID of the deleted vehicle.
--
-- 'arn', 'deleteVehicleResponse_arn' - The Amazon Resource Name (ARN) of the deleted vehicle.
newDeleteVehicleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'vehicleName'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  DeleteVehicleResponse
newDeleteVehicleResponse
  pHttpStatus_
  pVehicleName_
  pArn_ =
    DeleteVehicleResponse'
      { httpStatus = pHttpStatus_,
        vehicleName = pVehicleName_,
        arn = pArn_
      }

-- | The response's http status code.
deleteVehicleResponse_httpStatus :: Lens.Lens' DeleteVehicleResponse Prelude.Int
deleteVehicleResponse_httpStatus = Lens.lens (\DeleteVehicleResponse' {httpStatus} -> httpStatus) (\s@DeleteVehicleResponse' {} a -> s {httpStatus = a} :: DeleteVehicleResponse)

-- | The ID of the deleted vehicle.
deleteVehicleResponse_vehicleName :: Lens.Lens' DeleteVehicleResponse Prelude.Text
deleteVehicleResponse_vehicleName = Lens.lens (\DeleteVehicleResponse' {vehicleName} -> vehicleName) (\s@DeleteVehicleResponse' {} a -> s {vehicleName = a} :: DeleteVehicleResponse)

-- | The Amazon Resource Name (ARN) of the deleted vehicle.
deleteVehicleResponse_arn :: Lens.Lens' DeleteVehicleResponse Prelude.Text
deleteVehicleResponse_arn = Lens.lens (\DeleteVehicleResponse' {arn} -> arn) (\s@DeleteVehicleResponse' {} a -> s {arn = a} :: DeleteVehicleResponse)

instance Prelude.NFData DeleteVehicleResponse where
  rnf DeleteVehicleResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf vehicleName
      `Prelude.seq` Prelude.rnf arn
