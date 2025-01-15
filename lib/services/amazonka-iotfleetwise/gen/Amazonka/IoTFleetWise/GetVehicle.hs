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
-- Module      : Amazonka.IoTFleetWise.GetVehicle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a vehicle.
module Amazonka.IoTFleetWise.GetVehicle
  ( -- * Creating a Request
    GetVehicle (..),
    newGetVehicle,

    -- * Request Lenses
    getVehicle_vehicleName,

    -- * Destructuring the Response
    GetVehicleResponse (..),
    newGetVehicleResponse,

    -- * Response Lenses
    getVehicleResponse_arn,
    getVehicleResponse_attributes,
    getVehicleResponse_creationTime,
    getVehicleResponse_decoderManifestArn,
    getVehicleResponse_lastModificationTime,
    getVehicleResponse_modelManifestArn,
    getVehicleResponse_vehicleName,
    getVehicleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetVehicle' smart constructor.
data GetVehicle = GetVehicle'
  { -- | The ID of the vehicle to retrieve information about.
    vehicleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVehicle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vehicleName', 'getVehicle_vehicleName' - The ID of the vehicle to retrieve information about.
newGetVehicle ::
  -- | 'vehicleName'
  Prelude.Text ->
  GetVehicle
newGetVehicle pVehicleName_ =
  GetVehicle' {vehicleName = pVehicleName_}

-- | The ID of the vehicle to retrieve information about.
getVehicle_vehicleName :: Lens.Lens' GetVehicle Prelude.Text
getVehicle_vehicleName = Lens.lens (\GetVehicle' {vehicleName} -> vehicleName) (\s@GetVehicle' {} a -> s {vehicleName = a} :: GetVehicle)

instance Core.AWSRequest GetVehicle where
  type AWSResponse GetVehicle = GetVehicleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVehicleResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "attributes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "decoderManifestArn")
            Prelude.<*> (x Data..?> "lastModificationTime")
            Prelude.<*> (x Data..?> "modelManifestArn")
            Prelude.<*> (x Data..?> "vehicleName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetVehicle where
  hashWithSalt _salt GetVehicle' {..} =
    _salt `Prelude.hashWithSalt` vehicleName

instance Prelude.NFData GetVehicle where
  rnf GetVehicle' {..} = Prelude.rnf vehicleName

instance Data.ToHeaders GetVehicle where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.GetVehicle" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetVehicle where
  toJSON GetVehicle' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("vehicleName" Data..= vehicleName)]
      )

instance Data.ToPath GetVehicle where
  toPath = Prelude.const "/"

instance Data.ToQuery GetVehicle where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetVehicleResponse' smart constructor.
data GetVehicleResponse = GetVehicleResponse'
  { -- | The Amazon Resource Name (ARN) of the vehicle to retrieve information
    -- about.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Static information about a vehicle in a key-value pair. For example:
    --
    -- @\"engineType\"@ : @\"1.3 L R2\"@
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The time the vehicle was created in seconds since epoch (January 1, 1970
    -- at midnight UTC time).
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The ARN of a decoder manifest associated with the vehicle.
    decoderManifestArn :: Prelude.Maybe Prelude.Text,
    -- | The time the vehicle was last updated in seconds since epoch (January 1,
    -- 1970 at midnight UTC time).
    lastModificationTime :: Prelude.Maybe Data.POSIX,
    -- | The ARN of a vehicle model (model manifest) associated with the vehicle.
    modelManifestArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the vehicle.
    vehicleName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVehicleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getVehicleResponse_arn' - The Amazon Resource Name (ARN) of the vehicle to retrieve information
-- about.
--
-- 'attributes', 'getVehicleResponse_attributes' - Static information about a vehicle in a key-value pair. For example:
--
-- @\"engineType\"@ : @\"1.3 L R2\"@
--
-- 'creationTime', 'getVehicleResponse_creationTime' - The time the vehicle was created in seconds since epoch (January 1, 1970
-- at midnight UTC time).
--
-- 'decoderManifestArn', 'getVehicleResponse_decoderManifestArn' - The ARN of a decoder manifest associated with the vehicle.
--
-- 'lastModificationTime', 'getVehicleResponse_lastModificationTime' - The time the vehicle was last updated in seconds since epoch (January 1,
-- 1970 at midnight UTC time).
--
-- 'modelManifestArn', 'getVehicleResponse_modelManifestArn' - The ARN of a vehicle model (model manifest) associated with the vehicle.
--
-- 'vehicleName', 'getVehicleResponse_vehicleName' - The ID of the vehicle.
--
-- 'httpStatus', 'getVehicleResponse_httpStatus' - The response's http status code.
newGetVehicleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetVehicleResponse
newGetVehicleResponse pHttpStatus_ =
  GetVehicleResponse'
    { arn = Prelude.Nothing,
      attributes = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      decoderManifestArn = Prelude.Nothing,
      lastModificationTime = Prelude.Nothing,
      modelManifestArn = Prelude.Nothing,
      vehicleName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the vehicle to retrieve information
-- about.
getVehicleResponse_arn :: Lens.Lens' GetVehicleResponse (Prelude.Maybe Prelude.Text)
getVehicleResponse_arn = Lens.lens (\GetVehicleResponse' {arn} -> arn) (\s@GetVehicleResponse' {} a -> s {arn = a} :: GetVehicleResponse)

-- | Static information about a vehicle in a key-value pair. For example:
--
-- @\"engineType\"@ : @\"1.3 L R2\"@
getVehicleResponse_attributes :: Lens.Lens' GetVehicleResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getVehicleResponse_attributes = Lens.lens (\GetVehicleResponse' {attributes} -> attributes) (\s@GetVehicleResponse' {} a -> s {attributes = a} :: GetVehicleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The time the vehicle was created in seconds since epoch (January 1, 1970
-- at midnight UTC time).
getVehicleResponse_creationTime :: Lens.Lens' GetVehicleResponse (Prelude.Maybe Prelude.UTCTime)
getVehicleResponse_creationTime = Lens.lens (\GetVehicleResponse' {creationTime} -> creationTime) (\s@GetVehicleResponse' {} a -> s {creationTime = a} :: GetVehicleResponse) Prelude.. Lens.mapping Data._Time

-- | The ARN of a decoder manifest associated with the vehicle.
getVehicleResponse_decoderManifestArn :: Lens.Lens' GetVehicleResponse (Prelude.Maybe Prelude.Text)
getVehicleResponse_decoderManifestArn = Lens.lens (\GetVehicleResponse' {decoderManifestArn} -> decoderManifestArn) (\s@GetVehicleResponse' {} a -> s {decoderManifestArn = a} :: GetVehicleResponse)

-- | The time the vehicle was last updated in seconds since epoch (January 1,
-- 1970 at midnight UTC time).
getVehicleResponse_lastModificationTime :: Lens.Lens' GetVehicleResponse (Prelude.Maybe Prelude.UTCTime)
getVehicleResponse_lastModificationTime = Lens.lens (\GetVehicleResponse' {lastModificationTime} -> lastModificationTime) (\s@GetVehicleResponse' {} a -> s {lastModificationTime = a} :: GetVehicleResponse) Prelude.. Lens.mapping Data._Time

-- | The ARN of a vehicle model (model manifest) associated with the vehicle.
getVehicleResponse_modelManifestArn :: Lens.Lens' GetVehicleResponse (Prelude.Maybe Prelude.Text)
getVehicleResponse_modelManifestArn = Lens.lens (\GetVehicleResponse' {modelManifestArn} -> modelManifestArn) (\s@GetVehicleResponse' {} a -> s {modelManifestArn = a} :: GetVehicleResponse)

-- | The ID of the vehicle.
getVehicleResponse_vehicleName :: Lens.Lens' GetVehicleResponse (Prelude.Maybe Prelude.Text)
getVehicleResponse_vehicleName = Lens.lens (\GetVehicleResponse' {vehicleName} -> vehicleName) (\s@GetVehicleResponse' {} a -> s {vehicleName = a} :: GetVehicleResponse)

-- | The response's http status code.
getVehicleResponse_httpStatus :: Lens.Lens' GetVehicleResponse Prelude.Int
getVehicleResponse_httpStatus = Lens.lens (\GetVehicleResponse' {httpStatus} -> httpStatus) (\s@GetVehicleResponse' {} a -> s {httpStatus = a} :: GetVehicleResponse)

instance Prelude.NFData GetVehicleResponse where
  rnf GetVehicleResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf attributes `Prelude.seq`
        Prelude.rnf creationTime `Prelude.seq`
          Prelude.rnf decoderManifestArn `Prelude.seq`
            Prelude.rnf lastModificationTime `Prelude.seq`
              Prelude.rnf modelManifestArn `Prelude.seq`
                Prelude.rnf vehicleName `Prelude.seq`
                  Prelude.rnf httpStatus
