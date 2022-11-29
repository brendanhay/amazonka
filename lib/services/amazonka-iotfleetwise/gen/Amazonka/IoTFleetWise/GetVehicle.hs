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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    getVehicleResponse_lastModificationTime,
    getVehicleResponse_modelManifestArn,
    getVehicleResponse_arn,
    getVehicleResponse_vehicleName,
    getVehicleResponse_creationTime,
    getVehicleResponse_attributes,
    getVehicleResponse_decoderManifestArn,
    getVehicleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
            Prelude.<$> (x Core..?> "lastModificationTime")
            Prelude.<*> (x Core..?> "modelManifestArn")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "vehicleName")
            Prelude.<*> (x Core..?> "creationTime")
            Prelude.<*> (x Core..?> "attributes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "decoderManifestArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetVehicle where
  hashWithSalt _salt GetVehicle' {..} =
    _salt `Prelude.hashWithSalt` vehicleName

instance Prelude.NFData GetVehicle where
  rnf GetVehicle' {..} = Prelude.rnf vehicleName

instance Core.ToHeaders GetVehicle where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IoTAutobahnControlPlane.GetVehicle" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetVehicle where
  toJSON GetVehicle' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("vehicleName" Core..= vehicleName)]
      )

instance Core.ToPath GetVehicle where
  toPath = Prelude.const "/"

instance Core.ToQuery GetVehicle where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetVehicleResponse' smart constructor.
data GetVehicleResponse = GetVehicleResponse'
  { -- | The time the vehicle was last updated in seconds since epoch (January 1,
    -- 1970 at midnight UTC time).
    lastModificationTime :: Prelude.Maybe Core.POSIX,
    -- | The ARN of a vehicle model (model manifest) associated with the vehicle.
    modelManifestArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the vehicle to retrieve information
    -- about.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the vehicle.
    vehicleName :: Prelude.Maybe Prelude.Text,
    -- | The time the vehicle was created in seconds since epoch (January 1, 1970
    -- at midnight UTC time).
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | Static information about a vehicle in a key-value pair. For example:
    --
    -- @\"engineType\"@ : @\"1.3 L R2\"@
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ARN of a decoder manifest associated with the vehicle.
    decoderManifestArn :: Prelude.Maybe Prelude.Text,
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
-- 'lastModificationTime', 'getVehicleResponse_lastModificationTime' - The time the vehicle was last updated in seconds since epoch (January 1,
-- 1970 at midnight UTC time).
--
-- 'modelManifestArn', 'getVehicleResponse_modelManifestArn' - The ARN of a vehicle model (model manifest) associated with the vehicle.
--
-- 'arn', 'getVehicleResponse_arn' - The Amazon Resource Name (ARN) of the vehicle to retrieve information
-- about.
--
-- 'vehicleName', 'getVehicleResponse_vehicleName' - The ID of the vehicle.
--
-- 'creationTime', 'getVehicleResponse_creationTime' - The time the vehicle was created in seconds since epoch (January 1, 1970
-- at midnight UTC time).
--
-- 'attributes', 'getVehicleResponse_attributes' - Static information about a vehicle in a key-value pair. For example:
--
-- @\"engineType\"@ : @\"1.3 L R2\"@
--
-- 'decoderManifestArn', 'getVehicleResponse_decoderManifestArn' - The ARN of a decoder manifest associated with the vehicle.
--
-- 'httpStatus', 'getVehicleResponse_httpStatus' - The response's http status code.
newGetVehicleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetVehicleResponse
newGetVehicleResponse pHttpStatus_ =
  GetVehicleResponse'
    { lastModificationTime =
        Prelude.Nothing,
      modelManifestArn = Prelude.Nothing,
      arn = Prelude.Nothing,
      vehicleName = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      attributes = Prelude.Nothing,
      decoderManifestArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time the vehicle was last updated in seconds since epoch (January 1,
-- 1970 at midnight UTC time).
getVehicleResponse_lastModificationTime :: Lens.Lens' GetVehicleResponse (Prelude.Maybe Prelude.UTCTime)
getVehicleResponse_lastModificationTime = Lens.lens (\GetVehicleResponse' {lastModificationTime} -> lastModificationTime) (\s@GetVehicleResponse' {} a -> s {lastModificationTime = a} :: GetVehicleResponse) Prelude.. Lens.mapping Core._Time

-- | The ARN of a vehicle model (model manifest) associated with the vehicle.
getVehicleResponse_modelManifestArn :: Lens.Lens' GetVehicleResponse (Prelude.Maybe Prelude.Text)
getVehicleResponse_modelManifestArn = Lens.lens (\GetVehicleResponse' {modelManifestArn} -> modelManifestArn) (\s@GetVehicleResponse' {} a -> s {modelManifestArn = a} :: GetVehicleResponse)

-- | The Amazon Resource Name (ARN) of the vehicle to retrieve information
-- about.
getVehicleResponse_arn :: Lens.Lens' GetVehicleResponse (Prelude.Maybe Prelude.Text)
getVehicleResponse_arn = Lens.lens (\GetVehicleResponse' {arn} -> arn) (\s@GetVehicleResponse' {} a -> s {arn = a} :: GetVehicleResponse)

-- | The ID of the vehicle.
getVehicleResponse_vehicleName :: Lens.Lens' GetVehicleResponse (Prelude.Maybe Prelude.Text)
getVehicleResponse_vehicleName = Lens.lens (\GetVehicleResponse' {vehicleName} -> vehicleName) (\s@GetVehicleResponse' {} a -> s {vehicleName = a} :: GetVehicleResponse)

-- | The time the vehicle was created in seconds since epoch (January 1, 1970
-- at midnight UTC time).
getVehicleResponse_creationTime :: Lens.Lens' GetVehicleResponse (Prelude.Maybe Prelude.UTCTime)
getVehicleResponse_creationTime = Lens.lens (\GetVehicleResponse' {creationTime} -> creationTime) (\s@GetVehicleResponse' {} a -> s {creationTime = a} :: GetVehicleResponse) Prelude.. Lens.mapping Core._Time

-- | Static information about a vehicle in a key-value pair. For example:
--
-- @\"engineType\"@ : @\"1.3 L R2\"@
getVehicleResponse_attributes :: Lens.Lens' GetVehicleResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getVehicleResponse_attributes = Lens.lens (\GetVehicleResponse' {attributes} -> attributes) (\s@GetVehicleResponse' {} a -> s {attributes = a} :: GetVehicleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of a decoder manifest associated with the vehicle.
getVehicleResponse_decoderManifestArn :: Lens.Lens' GetVehicleResponse (Prelude.Maybe Prelude.Text)
getVehicleResponse_decoderManifestArn = Lens.lens (\GetVehicleResponse' {decoderManifestArn} -> decoderManifestArn) (\s@GetVehicleResponse' {} a -> s {decoderManifestArn = a} :: GetVehicleResponse)

-- | The response's http status code.
getVehicleResponse_httpStatus :: Lens.Lens' GetVehicleResponse Prelude.Int
getVehicleResponse_httpStatus = Lens.lens (\GetVehicleResponse' {httpStatus} -> httpStatus) (\s@GetVehicleResponse' {} a -> s {httpStatus = a} :: GetVehicleResponse)

instance Prelude.NFData GetVehicleResponse where
  rnf GetVehicleResponse' {..} =
    Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf modelManifestArn
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf vehicleName
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf decoderManifestArn
      `Prelude.seq` Prelude.rnf httpStatus
