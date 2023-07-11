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
-- Module      : Amazonka.Panorama.UpdateDeviceMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a device\'s metadata.
module Amazonka.Panorama.UpdateDeviceMetadata
  ( -- * Creating a Request
    UpdateDeviceMetadata (..),
    newUpdateDeviceMetadata,

    -- * Request Lenses
    updateDeviceMetadata_description,
    updateDeviceMetadata_deviceId,

    -- * Destructuring the Response
    UpdateDeviceMetadataResponse (..),
    newUpdateDeviceMetadataResponse,

    -- * Response Lenses
    updateDeviceMetadataResponse_deviceId,
    updateDeviceMetadataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDeviceMetadata' smart constructor.
data UpdateDeviceMetadata = UpdateDeviceMetadata'
  { -- | A description for the device.
    description :: Prelude.Maybe Prelude.Text,
    -- | The device\'s ID.
    deviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDeviceMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateDeviceMetadata_description' - A description for the device.
--
-- 'deviceId', 'updateDeviceMetadata_deviceId' - The device\'s ID.
newUpdateDeviceMetadata ::
  -- | 'deviceId'
  Prelude.Text ->
  UpdateDeviceMetadata
newUpdateDeviceMetadata pDeviceId_ =
  UpdateDeviceMetadata'
    { description =
        Prelude.Nothing,
      deviceId = pDeviceId_
    }

-- | A description for the device.
updateDeviceMetadata_description :: Lens.Lens' UpdateDeviceMetadata (Prelude.Maybe Prelude.Text)
updateDeviceMetadata_description = Lens.lens (\UpdateDeviceMetadata' {description} -> description) (\s@UpdateDeviceMetadata' {} a -> s {description = a} :: UpdateDeviceMetadata)

-- | The device\'s ID.
updateDeviceMetadata_deviceId :: Lens.Lens' UpdateDeviceMetadata Prelude.Text
updateDeviceMetadata_deviceId = Lens.lens (\UpdateDeviceMetadata' {deviceId} -> deviceId) (\s@UpdateDeviceMetadata' {} a -> s {deviceId = a} :: UpdateDeviceMetadata)

instance Core.AWSRequest UpdateDeviceMetadata where
  type
    AWSResponse UpdateDeviceMetadata =
      UpdateDeviceMetadataResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDeviceMetadataResponse'
            Prelude.<$> (x Data..?> "DeviceId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDeviceMetadata where
  hashWithSalt _salt UpdateDeviceMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` deviceId

instance Prelude.NFData UpdateDeviceMetadata where
  rnf UpdateDeviceMetadata' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf deviceId

instance Data.ToHeaders UpdateDeviceMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDeviceMetadata where
  toJSON UpdateDeviceMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Description" Data..=) Prelude.<$> description]
      )

instance Data.ToPath UpdateDeviceMetadata where
  toPath UpdateDeviceMetadata' {..} =
    Prelude.mconcat ["/devices/", Data.toBS deviceId]

instance Data.ToQuery UpdateDeviceMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDeviceMetadataResponse' smart constructor.
data UpdateDeviceMetadataResponse = UpdateDeviceMetadataResponse'
  { -- | The device\'s ID.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDeviceMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceId', 'updateDeviceMetadataResponse_deviceId' - The device\'s ID.
--
-- 'httpStatus', 'updateDeviceMetadataResponse_httpStatus' - The response's http status code.
newUpdateDeviceMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDeviceMetadataResponse
newUpdateDeviceMetadataResponse pHttpStatus_ =
  UpdateDeviceMetadataResponse'
    { deviceId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The device\'s ID.
updateDeviceMetadataResponse_deviceId :: Lens.Lens' UpdateDeviceMetadataResponse (Prelude.Maybe Prelude.Text)
updateDeviceMetadataResponse_deviceId = Lens.lens (\UpdateDeviceMetadataResponse' {deviceId} -> deviceId) (\s@UpdateDeviceMetadataResponse' {} a -> s {deviceId = a} :: UpdateDeviceMetadataResponse)

-- | The response's http status code.
updateDeviceMetadataResponse_httpStatus :: Lens.Lens' UpdateDeviceMetadataResponse Prelude.Int
updateDeviceMetadataResponse_httpStatus = Lens.lens (\UpdateDeviceMetadataResponse' {httpStatus} -> httpStatus) (\s@UpdateDeviceMetadataResponse' {} a -> s {httpStatus = a} :: UpdateDeviceMetadataResponse)

instance Prelude.NFData UpdateDeviceMetadataResponse where
  rnf UpdateDeviceMetadataResponse' {..} =
    Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf httpStatus
