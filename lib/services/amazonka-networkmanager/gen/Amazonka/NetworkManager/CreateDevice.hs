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
-- Module      : Amazonka.NetworkManager.CreateDevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new device in a global network. If you specify both a site ID
-- and a location, the location of the site is used for visualization in
-- the Network Manager console.
module Amazonka.NetworkManager.CreateDevice
  ( -- * Creating a Request
    CreateDevice (..),
    newCreateDevice,

    -- * Request Lenses
    createDevice_vendor,
    createDevice_location,
    createDevice_aWSLocation,
    createDevice_model,
    createDevice_type,
    createDevice_serialNumber,
    createDevice_siteId,
    createDevice_description,
    createDevice_tags,
    createDevice_globalNetworkId,

    -- * Destructuring the Response
    CreateDeviceResponse (..),
    newCreateDeviceResponse,

    -- * Response Lenses
    createDeviceResponse_device,
    createDeviceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDevice' smart constructor.
data CreateDevice = CreateDevice'
  { -- | The vendor of the device.
    --
    -- Length Constraints: Maximum length of 128 characters.
    vendor :: Prelude.Maybe Prelude.Text,
    -- | The location of the device.
    location :: Prelude.Maybe (Core.Sensitive Location),
    -- | The AWS location of the device.
    aWSLocation :: Prelude.Maybe AWSLocation,
    -- | The model of the device.
    --
    -- Length Constraints: Maximum length of 128 characters.
    model :: Prelude.Maybe Prelude.Text,
    -- | The type of the device.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The serial number of the device.
    --
    -- Length Constraints: Maximum length of 128 characters.
    serialNumber :: Prelude.Maybe Prelude.Text,
    -- | The ID of the site.
    siteId :: Prelude.Maybe Prelude.Text,
    -- | A description of the device.
    --
    -- Length Constraints: Maximum length of 256 characters.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags to apply to the resource during creation.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vendor', 'createDevice_vendor' - The vendor of the device.
--
-- Length Constraints: Maximum length of 128 characters.
--
-- 'location', 'createDevice_location' - The location of the device.
--
-- 'aWSLocation', 'createDevice_aWSLocation' - The AWS location of the device.
--
-- 'model', 'createDevice_model' - The model of the device.
--
-- Length Constraints: Maximum length of 128 characters.
--
-- 'type'', 'createDevice_type' - The type of the device.
--
-- 'serialNumber', 'createDevice_serialNumber' - The serial number of the device.
--
-- Length Constraints: Maximum length of 128 characters.
--
-- 'siteId', 'createDevice_siteId' - The ID of the site.
--
-- 'description', 'createDevice_description' - A description of the device.
--
-- Length Constraints: Maximum length of 256 characters.
--
-- 'tags', 'createDevice_tags' - The tags to apply to the resource during creation.
--
-- 'globalNetworkId', 'createDevice_globalNetworkId' - The ID of the global network.
newCreateDevice ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  CreateDevice
newCreateDevice pGlobalNetworkId_ =
  CreateDevice'
    { vendor = Prelude.Nothing,
      location = Prelude.Nothing,
      aWSLocation = Prelude.Nothing,
      model = Prelude.Nothing,
      type' = Prelude.Nothing,
      serialNumber = Prelude.Nothing,
      siteId = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      globalNetworkId = pGlobalNetworkId_
    }

-- | The vendor of the device.
--
-- Length Constraints: Maximum length of 128 characters.
createDevice_vendor :: Lens.Lens' CreateDevice (Prelude.Maybe Prelude.Text)
createDevice_vendor = Lens.lens (\CreateDevice' {vendor} -> vendor) (\s@CreateDevice' {} a -> s {vendor = a} :: CreateDevice)

-- | The location of the device.
createDevice_location :: Lens.Lens' CreateDevice (Prelude.Maybe Location)
createDevice_location = Lens.lens (\CreateDevice' {location} -> location) (\s@CreateDevice' {} a -> s {location = a} :: CreateDevice) Prelude.. Lens.mapping Core._Sensitive

-- | The AWS location of the device.
createDevice_aWSLocation :: Lens.Lens' CreateDevice (Prelude.Maybe AWSLocation)
createDevice_aWSLocation = Lens.lens (\CreateDevice' {aWSLocation} -> aWSLocation) (\s@CreateDevice' {} a -> s {aWSLocation = a} :: CreateDevice)

-- | The model of the device.
--
-- Length Constraints: Maximum length of 128 characters.
createDevice_model :: Lens.Lens' CreateDevice (Prelude.Maybe Prelude.Text)
createDevice_model = Lens.lens (\CreateDevice' {model} -> model) (\s@CreateDevice' {} a -> s {model = a} :: CreateDevice)

-- | The type of the device.
createDevice_type :: Lens.Lens' CreateDevice (Prelude.Maybe Prelude.Text)
createDevice_type = Lens.lens (\CreateDevice' {type'} -> type') (\s@CreateDevice' {} a -> s {type' = a} :: CreateDevice)

-- | The serial number of the device.
--
-- Length Constraints: Maximum length of 128 characters.
createDevice_serialNumber :: Lens.Lens' CreateDevice (Prelude.Maybe Prelude.Text)
createDevice_serialNumber = Lens.lens (\CreateDevice' {serialNumber} -> serialNumber) (\s@CreateDevice' {} a -> s {serialNumber = a} :: CreateDevice)

-- | The ID of the site.
createDevice_siteId :: Lens.Lens' CreateDevice (Prelude.Maybe Prelude.Text)
createDevice_siteId = Lens.lens (\CreateDevice' {siteId} -> siteId) (\s@CreateDevice' {} a -> s {siteId = a} :: CreateDevice)

-- | A description of the device.
--
-- Length Constraints: Maximum length of 256 characters.
createDevice_description :: Lens.Lens' CreateDevice (Prelude.Maybe Prelude.Text)
createDevice_description = Lens.lens (\CreateDevice' {description} -> description) (\s@CreateDevice' {} a -> s {description = a} :: CreateDevice)

-- | The tags to apply to the resource during creation.
createDevice_tags :: Lens.Lens' CreateDevice (Prelude.Maybe [Tag])
createDevice_tags = Lens.lens (\CreateDevice' {tags} -> tags) (\s@CreateDevice' {} a -> s {tags = a} :: CreateDevice) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the global network.
createDevice_globalNetworkId :: Lens.Lens' CreateDevice Prelude.Text
createDevice_globalNetworkId = Lens.lens (\CreateDevice' {globalNetworkId} -> globalNetworkId) (\s@CreateDevice' {} a -> s {globalNetworkId = a} :: CreateDevice)

instance Core.AWSRequest CreateDevice where
  type AWSResponse CreateDevice = CreateDeviceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDeviceResponse'
            Prelude.<$> (x Core..?> "Device")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDevice where
  hashWithSalt _salt CreateDevice' {..} =
    _salt `Prelude.hashWithSalt` vendor
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` aWSLocation
      `Prelude.hashWithSalt` model
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` serialNumber
      `Prelude.hashWithSalt` siteId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` globalNetworkId

instance Prelude.NFData CreateDevice where
  rnf CreateDevice' {..} =
    Prelude.rnf vendor
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf aWSLocation
      `Prelude.seq` Prelude.rnf model
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf serialNumber
      `Prelude.seq` Prelude.rnf siteId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf globalNetworkId

instance Core.ToHeaders CreateDevice where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDevice where
  toJSON CreateDevice' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Vendor" Core..=) Prelude.<$> vendor,
            ("Location" Core..=) Prelude.<$> location,
            ("AWSLocation" Core..=) Prelude.<$> aWSLocation,
            ("Model" Core..=) Prelude.<$> model,
            ("Type" Core..=) Prelude.<$> type',
            ("SerialNumber" Core..=) Prelude.<$> serialNumber,
            ("SiteId" Core..=) Prelude.<$> siteId,
            ("Description" Core..=) Prelude.<$> description,
            ("Tags" Core..=) Prelude.<$> tags
          ]
      )

instance Core.ToPath CreateDevice where
  toPath CreateDevice' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Core.toBS globalNetworkId,
        "/devices"
      ]

instance Core.ToQuery CreateDevice where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDeviceResponse' smart constructor.
data CreateDeviceResponse = CreateDeviceResponse'
  { -- | Information about the device.
    device :: Prelude.Maybe Device,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'device', 'createDeviceResponse_device' - Information about the device.
--
-- 'httpStatus', 'createDeviceResponse_httpStatus' - The response's http status code.
newCreateDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDeviceResponse
newCreateDeviceResponse pHttpStatus_ =
  CreateDeviceResponse'
    { device = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the device.
createDeviceResponse_device :: Lens.Lens' CreateDeviceResponse (Prelude.Maybe Device)
createDeviceResponse_device = Lens.lens (\CreateDeviceResponse' {device} -> device) (\s@CreateDeviceResponse' {} a -> s {device = a} :: CreateDeviceResponse)

-- | The response's http status code.
createDeviceResponse_httpStatus :: Lens.Lens' CreateDeviceResponse Prelude.Int
createDeviceResponse_httpStatus = Lens.lens (\CreateDeviceResponse' {httpStatus} -> httpStatus) (\s@CreateDeviceResponse' {} a -> s {httpStatus = a} :: CreateDeviceResponse)

instance Prelude.NFData CreateDeviceResponse where
  rnf CreateDeviceResponse' {..} =
    Prelude.rnf device
      `Prelude.seq` Prelude.rnf httpStatus
