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
-- Module      : Amazonka.NetworkManager.UpdateDevice
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the details for an existing device. To remove information for
-- any of the parameters, specify an empty string.
module Amazonka.NetworkManager.UpdateDevice
  ( -- * Creating a Request
    UpdateDevice (..),
    newUpdateDevice,

    -- * Request Lenses
    updateDevice_type,
    updateDevice_model,
    updateDevice_aWSLocation,
    updateDevice_description,
    updateDevice_siteId,
    updateDevice_location,
    updateDevice_serialNumber,
    updateDevice_vendor,
    updateDevice_globalNetworkId,
    updateDevice_deviceId,

    -- * Destructuring the Response
    UpdateDeviceResponse (..),
    newUpdateDeviceResponse,

    -- * Response Lenses
    updateDeviceResponse_device,
    updateDeviceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDevice' smart constructor.
data UpdateDevice = UpdateDevice'
  { -- | The type of the device.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The model of the device.
    --
    -- Constraints: Maximum length of 128 characters.
    model :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services location of the device, if applicable. For an
    -- on-premises device, you can omit this parameter.
    aWSLocation :: Prelude.Maybe AWSLocation,
    -- | A description of the device.
    --
    -- Constraints: Maximum length of 256 characters.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the site.
    siteId :: Prelude.Maybe Prelude.Text,
    location :: Prelude.Maybe (Core.Sensitive Location),
    -- | The serial number of the device.
    --
    -- Constraints: Maximum length of 128 characters.
    serialNumber :: Prelude.Maybe Prelude.Text,
    -- | The vendor of the device.
    --
    -- Constraints: Maximum length of 128 characters.
    vendor :: Prelude.Maybe Prelude.Text,
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Text,
    -- | The ID of the device.
    deviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'updateDevice_type' - The type of the device.
--
-- 'model', 'updateDevice_model' - The model of the device.
--
-- Constraints: Maximum length of 128 characters.
--
-- 'aWSLocation', 'updateDevice_aWSLocation' - The Amazon Web Services location of the device, if applicable. For an
-- on-premises device, you can omit this parameter.
--
-- 'description', 'updateDevice_description' - A description of the device.
--
-- Constraints: Maximum length of 256 characters.
--
-- 'siteId', 'updateDevice_siteId' - The ID of the site.
--
-- 'location', 'updateDevice_location' - Undocumented member.
--
-- 'serialNumber', 'updateDevice_serialNumber' - The serial number of the device.
--
-- Constraints: Maximum length of 128 characters.
--
-- 'vendor', 'updateDevice_vendor' - The vendor of the device.
--
-- Constraints: Maximum length of 128 characters.
--
-- 'globalNetworkId', 'updateDevice_globalNetworkId' - The ID of the global network.
--
-- 'deviceId', 'updateDevice_deviceId' - The ID of the device.
newUpdateDevice ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  -- | 'deviceId'
  Prelude.Text ->
  UpdateDevice
newUpdateDevice pGlobalNetworkId_ pDeviceId_ =
  UpdateDevice'
    { type' = Prelude.Nothing,
      model = Prelude.Nothing,
      aWSLocation = Prelude.Nothing,
      description = Prelude.Nothing,
      siteId = Prelude.Nothing,
      location = Prelude.Nothing,
      serialNumber = Prelude.Nothing,
      vendor = Prelude.Nothing,
      globalNetworkId = pGlobalNetworkId_,
      deviceId = pDeviceId_
    }

-- | The type of the device.
updateDevice_type :: Lens.Lens' UpdateDevice (Prelude.Maybe Prelude.Text)
updateDevice_type = Lens.lens (\UpdateDevice' {type'} -> type') (\s@UpdateDevice' {} a -> s {type' = a} :: UpdateDevice)

-- | The model of the device.
--
-- Constraints: Maximum length of 128 characters.
updateDevice_model :: Lens.Lens' UpdateDevice (Prelude.Maybe Prelude.Text)
updateDevice_model = Lens.lens (\UpdateDevice' {model} -> model) (\s@UpdateDevice' {} a -> s {model = a} :: UpdateDevice)

-- | The Amazon Web Services location of the device, if applicable. For an
-- on-premises device, you can omit this parameter.
updateDevice_aWSLocation :: Lens.Lens' UpdateDevice (Prelude.Maybe AWSLocation)
updateDevice_aWSLocation = Lens.lens (\UpdateDevice' {aWSLocation} -> aWSLocation) (\s@UpdateDevice' {} a -> s {aWSLocation = a} :: UpdateDevice)

-- | A description of the device.
--
-- Constraints: Maximum length of 256 characters.
updateDevice_description :: Lens.Lens' UpdateDevice (Prelude.Maybe Prelude.Text)
updateDevice_description = Lens.lens (\UpdateDevice' {description} -> description) (\s@UpdateDevice' {} a -> s {description = a} :: UpdateDevice)

-- | The ID of the site.
updateDevice_siteId :: Lens.Lens' UpdateDevice (Prelude.Maybe Prelude.Text)
updateDevice_siteId = Lens.lens (\UpdateDevice' {siteId} -> siteId) (\s@UpdateDevice' {} a -> s {siteId = a} :: UpdateDevice)

-- | Undocumented member.
updateDevice_location :: Lens.Lens' UpdateDevice (Prelude.Maybe Location)
updateDevice_location = Lens.lens (\UpdateDevice' {location} -> location) (\s@UpdateDevice' {} a -> s {location = a} :: UpdateDevice) Prelude.. Lens.mapping Core._Sensitive

-- | The serial number of the device.
--
-- Constraints: Maximum length of 128 characters.
updateDevice_serialNumber :: Lens.Lens' UpdateDevice (Prelude.Maybe Prelude.Text)
updateDevice_serialNumber = Lens.lens (\UpdateDevice' {serialNumber} -> serialNumber) (\s@UpdateDevice' {} a -> s {serialNumber = a} :: UpdateDevice)

-- | The vendor of the device.
--
-- Constraints: Maximum length of 128 characters.
updateDevice_vendor :: Lens.Lens' UpdateDevice (Prelude.Maybe Prelude.Text)
updateDevice_vendor = Lens.lens (\UpdateDevice' {vendor} -> vendor) (\s@UpdateDevice' {} a -> s {vendor = a} :: UpdateDevice)

-- | The ID of the global network.
updateDevice_globalNetworkId :: Lens.Lens' UpdateDevice Prelude.Text
updateDevice_globalNetworkId = Lens.lens (\UpdateDevice' {globalNetworkId} -> globalNetworkId) (\s@UpdateDevice' {} a -> s {globalNetworkId = a} :: UpdateDevice)

-- | The ID of the device.
updateDevice_deviceId :: Lens.Lens' UpdateDevice Prelude.Text
updateDevice_deviceId = Lens.lens (\UpdateDevice' {deviceId} -> deviceId) (\s@UpdateDevice' {} a -> s {deviceId = a} :: UpdateDevice)

instance Core.AWSRequest UpdateDevice where
  type AWSResponse UpdateDevice = UpdateDeviceResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDeviceResponse'
            Prelude.<$> (x Core..?> "Device")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDevice where
  hashWithSalt _salt UpdateDevice' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` model
      `Prelude.hashWithSalt` aWSLocation
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` siteId
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` serialNumber
      `Prelude.hashWithSalt` vendor
      `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` deviceId

instance Prelude.NFData UpdateDevice where
  rnf UpdateDevice' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf model
      `Prelude.seq` Prelude.rnf aWSLocation
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf siteId
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf serialNumber
      `Prelude.seq` Prelude.rnf vendor
      `Prelude.seq` Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf deviceId

instance Core.ToHeaders UpdateDevice where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateDevice where
  toJSON UpdateDevice' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Type" Core..=) Prelude.<$> type',
            ("Model" Core..=) Prelude.<$> model,
            ("AWSLocation" Core..=) Prelude.<$> aWSLocation,
            ("Description" Core..=) Prelude.<$> description,
            ("SiteId" Core..=) Prelude.<$> siteId,
            ("Location" Core..=) Prelude.<$> location,
            ("SerialNumber" Core..=) Prelude.<$> serialNumber,
            ("Vendor" Core..=) Prelude.<$> vendor
          ]
      )

instance Core.ToPath UpdateDevice where
  toPath UpdateDevice' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Core.toBS globalNetworkId,
        "/devices/",
        Core.toBS deviceId
      ]

instance Core.ToQuery UpdateDevice where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDeviceResponse' smart constructor.
data UpdateDeviceResponse = UpdateDeviceResponse'
  { -- | Information about the device.
    device :: Prelude.Maybe Device,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'device', 'updateDeviceResponse_device' - Information about the device.
--
-- 'httpStatus', 'updateDeviceResponse_httpStatus' - The response's http status code.
newUpdateDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDeviceResponse
newUpdateDeviceResponse pHttpStatus_ =
  UpdateDeviceResponse'
    { device = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the device.
updateDeviceResponse_device :: Lens.Lens' UpdateDeviceResponse (Prelude.Maybe Device)
updateDeviceResponse_device = Lens.lens (\UpdateDeviceResponse' {device} -> device) (\s@UpdateDeviceResponse' {} a -> s {device = a} :: UpdateDeviceResponse)

-- | The response's http status code.
updateDeviceResponse_httpStatus :: Lens.Lens' UpdateDeviceResponse Prelude.Int
updateDeviceResponse_httpStatus = Lens.lens (\UpdateDeviceResponse' {httpStatus} -> httpStatus) (\s@UpdateDeviceResponse' {} a -> s {httpStatus = a} :: UpdateDeviceResponse)

instance Prelude.NFData UpdateDeviceResponse where
  rnf UpdateDeviceResponse' {..} =
    Prelude.rnf device
      `Prelude.seq` Prelude.rnf httpStatus
