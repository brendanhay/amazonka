{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.NetworkManager.Types.Device
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.Device where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.AWSLocation
import Amazonka.NetworkManager.Types.DeviceState
import Amazonka.NetworkManager.Types.Location
import Amazonka.NetworkManager.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a device.
--
-- /See:/ 'newDevice' smart constructor.
data Device = Device'
  { -- | The ID of the global network.
    globalNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The tags for the device.
    tags :: Prelude.Maybe [Tag],
    -- | The device type.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The device model.
    model :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services location of the device.
    aWSLocation :: Prelude.Maybe AWSLocation,
    -- | The ID of the device.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The device state.
    state :: Prelude.Maybe DeviceState,
    -- | The description of the device.
    description :: Prelude.Maybe Prelude.Text,
    -- | The site ID.
    siteId :: Prelude.Maybe Prelude.Text,
    -- | The site location.
    location :: Prelude.Maybe (Data.Sensitive Location),
    -- | The device serial number.
    serialNumber :: Prelude.Maybe Prelude.Text,
    -- | The device vendor.
    vendor :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the site was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the device.
    deviceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Device' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalNetworkId', 'device_globalNetworkId' - The ID of the global network.
--
-- 'tags', 'device_tags' - The tags for the device.
--
-- 'type'', 'device_type' - The device type.
--
-- 'model', 'device_model' - The device model.
--
-- 'aWSLocation', 'device_aWSLocation' - The Amazon Web Services location of the device.
--
-- 'deviceId', 'device_deviceId' - The ID of the device.
--
-- 'state', 'device_state' - The device state.
--
-- 'description', 'device_description' - The description of the device.
--
-- 'siteId', 'device_siteId' - The site ID.
--
-- 'location', 'device_location' - The site location.
--
-- 'serialNumber', 'device_serialNumber' - The device serial number.
--
-- 'vendor', 'device_vendor' - The device vendor.
--
-- 'createdAt', 'device_createdAt' - The date and time that the site was created.
--
-- 'deviceArn', 'device_deviceArn' - The Amazon Resource Name (ARN) of the device.
newDevice ::
  Device
newDevice =
  Device'
    { globalNetworkId = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing,
      model = Prelude.Nothing,
      aWSLocation = Prelude.Nothing,
      deviceId = Prelude.Nothing,
      state = Prelude.Nothing,
      description = Prelude.Nothing,
      siteId = Prelude.Nothing,
      location = Prelude.Nothing,
      serialNumber = Prelude.Nothing,
      vendor = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      deviceArn = Prelude.Nothing
    }

-- | The ID of the global network.
device_globalNetworkId :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_globalNetworkId = Lens.lens (\Device' {globalNetworkId} -> globalNetworkId) (\s@Device' {} a -> s {globalNetworkId = a} :: Device)

-- | The tags for the device.
device_tags :: Lens.Lens' Device (Prelude.Maybe [Tag])
device_tags = Lens.lens (\Device' {tags} -> tags) (\s@Device' {} a -> s {tags = a} :: Device) Prelude.. Lens.mapping Lens.coerced

-- | The device type.
device_type :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_type = Lens.lens (\Device' {type'} -> type') (\s@Device' {} a -> s {type' = a} :: Device)

-- | The device model.
device_model :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_model = Lens.lens (\Device' {model} -> model) (\s@Device' {} a -> s {model = a} :: Device)

-- | The Amazon Web Services location of the device.
device_aWSLocation :: Lens.Lens' Device (Prelude.Maybe AWSLocation)
device_aWSLocation = Lens.lens (\Device' {aWSLocation} -> aWSLocation) (\s@Device' {} a -> s {aWSLocation = a} :: Device)

-- | The ID of the device.
device_deviceId :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_deviceId = Lens.lens (\Device' {deviceId} -> deviceId) (\s@Device' {} a -> s {deviceId = a} :: Device)

-- | The device state.
device_state :: Lens.Lens' Device (Prelude.Maybe DeviceState)
device_state = Lens.lens (\Device' {state} -> state) (\s@Device' {} a -> s {state = a} :: Device)

-- | The description of the device.
device_description :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_description = Lens.lens (\Device' {description} -> description) (\s@Device' {} a -> s {description = a} :: Device)

-- | The site ID.
device_siteId :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_siteId = Lens.lens (\Device' {siteId} -> siteId) (\s@Device' {} a -> s {siteId = a} :: Device)

-- | The site location.
device_location :: Lens.Lens' Device (Prelude.Maybe Location)
device_location = Lens.lens (\Device' {location} -> location) (\s@Device' {} a -> s {location = a} :: Device) Prelude.. Lens.mapping Data._Sensitive

-- | The device serial number.
device_serialNumber :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_serialNumber = Lens.lens (\Device' {serialNumber} -> serialNumber) (\s@Device' {} a -> s {serialNumber = a} :: Device)

-- | The device vendor.
device_vendor :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_vendor = Lens.lens (\Device' {vendor} -> vendor) (\s@Device' {} a -> s {vendor = a} :: Device)

-- | The date and time that the site was created.
device_createdAt :: Lens.Lens' Device (Prelude.Maybe Prelude.UTCTime)
device_createdAt = Lens.lens (\Device' {createdAt} -> createdAt) (\s@Device' {} a -> s {createdAt = a} :: Device) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the device.
device_deviceArn :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_deviceArn = Lens.lens (\Device' {deviceArn} -> deviceArn) (\s@Device' {} a -> s {deviceArn = a} :: Device)

instance Data.FromJSON Device where
  parseJSON =
    Data.withObject
      "Device"
      ( \x ->
          Device'
            Prelude.<$> (x Data..:? "GlobalNetworkId")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Model")
            Prelude.<*> (x Data..:? "AWSLocation")
            Prelude.<*> (x Data..:? "DeviceId")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "SiteId")
            Prelude.<*> (x Data..:? "Location")
            Prelude.<*> (x Data..:? "SerialNumber")
            Prelude.<*> (x Data..:? "Vendor")
            Prelude.<*> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "DeviceArn")
      )

instance Prelude.Hashable Device where
  hashWithSalt _salt Device' {..} =
    _salt `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` model
      `Prelude.hashWithSalt` aWSLocation
      `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` siteId
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` serialNumber
      `Prelude.hashWithSalt` vendor
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` deviceArn

instance Prelude.NFData Device where
  rnf Device' {..} =
    Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf model
      `Prelude.seq` Prelude.rnf aWSLocation
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf siteId
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf serialNumber
      `Prelude.seq` Prelude.rnf vendor
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf deviceArn
