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
-- Module      : Network.AWS.NetworkManager.Types.Device
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.NetworkManager.Types.Device where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.NetworkManager.Types.AWSLocation
import Network.AWS.NetworkManager.Types.DeviceState
import Network.AWS.NetworkManager.Types.Location
import Network.AWS.NetworkManager.Types.Tag
import qualified Network.AWS.Prelude as Prelude

-- | Describes a device.
--
-- /See:/ 'newDevice' smart constructor.
data Device = Device'
  { -- | The device vendor.
    vendor :: Prelude.Maybe Prelude.Text,
    -- | The device state.
    state :: Prelude.Maybe DeviceState,
    -- | The site location.
    location :: Prelude.Maybe (Core.Sensitive Location),
    -- | The date and time that the site was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the device.
    deviceArn :: Prelude.Maybe Prelude.Text,
    -- | The AWS location of the device.
    aWSLocation :: Prelude.Maybe AWSLocation,
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The device model.
    model :: Prelude.Maybe Prelude.Text,
    -- | The ID of the device.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The device type.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The device serial number.
    serialNumber :: Prelude.Maybe Prelude.Text,
    -- | The site ID.
    siteId :: Prelude.Maybe Prelude.Text,
    -- | The description of the device.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags for the device.
    tags :: Prelude.Maybe [Tag]
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
-- 'vendor', 'device_vendor' - The device vendor.
--
-- 'state', 'device_state' - The device state.
--
-- 'location', 'device_location' - The site location.
--
-- 'createdAt', 'device_createdAt' - The date and time that the site was created.
--
-- 'deviceArn', 'device_deviceArn' - The Amazon Resource Name (ARN) of the device.
--
-- 'aWSLocation', 'device_aWSLocation' - The AWS location of the device.
--
-- 'globalNetworkId', 'device_globalNetworkId' - The ID of the global network.
--
-- 'model', 'device_model' - The device model.
--
-- 'deviceId', 'device_deviceId' - The ID of the device.
--
-- 'type'', 'device_type' - The device type.
--
-- 'serialNumber', 'device_serialNumber' - The device serial number.
--
-- 'siteId', 'device_siteId' - The site ID.
--
-- 'description', 'device_description' - The description of the device.
--
-- 'tags', 'device_tags' - The tags for the device.
newDevice ::
  Device
newDevice =
  Device'
    { vendor = Prelude.Nothing,
      state = Prelude.Nothing,
      location = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      deviceArn = Prelude.Nothing,
      aWSLocation = Prelude.Nothing,
      globalNetworkId = Prelude.Nothing,
      model = Prelude.Nothing,
      deviceId = Prelude.Nothing,
      type' = Prelude.Nothing,
      serialNumber = Prelude.Nothing,
      siteId = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The device vendor.
device_vendor :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_vendor = Lens.lens (\Device' {vendor} -> vendor) (\s@Device' {} a -> s {vendor = a} :: Device)

-- | The device state.
device_state :: Lens.Lens' Device (Prelude.Maybe DeviceState)
device_state = Lens.lens (\Device' {state} -> state) (\s@Device' {} a -> s {state = a} :: Device)

-- | The site location.
device_location :: Lens.Lens' Device (Prelude.Maybe Location)
device_location = Lens.lens (\Device' {location} -> location) (\s@Device' {} a -> s {location = a} :: Device) Prelude.. Lens.mapping Core._Sensitive

-- | The date and time that the site was created.
device_createdAt :: Lens.Lens' Device (Prelude.Maybe Prelude.UTCTime)
device_createdAt = Lens.lens (\Device' {createdAt} -> createdAt) (\s@Device' {} a -> s {createdAt = a} :: Device) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the device.
device_deviceArn :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_deviceArn = Lens.lens (\Device' {deviceArn} -> deviceArn) (\s@Device' {} a -> s {deviceArn = a} :: Device)

-- | The AWS location of the device.
device_aWSLocation :: Lens.Lens' Device (Prelude.Maybe AWSLocation)
device_aWSLocation = Lens.lens (\Device' {aWSLocation} -> aWSLocation) (\s@Device' {} a -> s {aWSLocation = a} :: Device)

-- | The ID of the global network.
device_globalNetworkId :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_globalNetworkId = Lens.lens (\Device' {globalNetworkId} -> globalNetworkId) (\s@Device' {} a -> s {globalNetworkId = a} :: Device)

-- | The device model.
device_model :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_model = Lens.lens (\Device' {model} -> model) (\s@Device' {} a -> s {model = a} :: Device)

-- | The ID of the device.
device_deviceId :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_deviceId = Lens.lens (\Device' {deviceId} -> deviceId) (\s@Device' {} a -> s {deviceId = a} :: Device)

-- | The device type.
device_type :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_type = Lens.lens (\Device' {type'} -> type') (\s@Device' {} a -> s {type' = a} :: Device)

-- | The device serial number.
device_serialNumber :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_serialNumber = Lens.lens (\Device' {serialNumber} -> serialNumber) (\s@Device' {} a -> s {serialNumber = a} :: Device)

-- | The site ID.
device_siteId :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_siteId = Lens.lens (\Device' {siteId} -> siteId) (\s@Device' {} a -> s {siteId = a} :: Device)

-- | The description of the device.
device_description :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_description = Lens.lens (\Device' {description} -> description) (\s@Device' {} a -> s {description = a} :: Device)

-- | The tags for the device.
device_tags :: Lens.Lens' Device (Prelude.Maybe [Tag])
device_tags = Lens.lens (\Device' {tags} -> tags) (\s@Device' {} a -> s {tags = a} :: Device) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Device where
  parseJSON =
    Core.withObject
      "Device"
      ( \x ->
          Device'
            Prelude.<$> (x Core..:? "Vendor")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "Location")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "DeviceArn")
            Prelude.<*> (x Core..:? "AWSLocation")
            Prelude.<*> (x Core..:? "GlobalNetworkId")
            Prelude.<*> (x Core..:? "Model")
            Prelude.<*> (x Core..:? "DeviceId")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "SerialNumber")
            Prelude.<*> (x Core..:? "SiteId")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Device

instance Prelude.NFData Device
