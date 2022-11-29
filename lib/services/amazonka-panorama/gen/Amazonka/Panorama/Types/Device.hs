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
-- Module      : Amazonka.Panorama.Types.Device
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.Device where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Panorama.Types.DeviceAggregatedStatus
import Amazonka.Panorama.Types.DeviceBrand
import Amazonka.Panorama.Types.DeviceStatus
import Amazonka.Panorama.Types.DeviceType
import Amazonka.Panorama.Types.LatestDeviceJob
import qualified Amazonka.Prelude as Prelude

-- | A device.
--
-- /See:/ 'newDevice' smart constructor.
data Device = Device'
  { -- | The device\'s tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The device\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The device\'s type.
    type' :: Prelude.Maybe DeviceType,
    -- | When the device was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The device\'s lease expiration time.
    leaseExpirationTime :: Prelude.Maybe Core.POSIX,
    -- | The device\'s provisioning status.
    provisioningStatus :: Prelude.Maybe DeviceStatus,
    -- | The device\'s ID.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | A device\'s latest job. Includes the target image version, and the
    -- update job status.
    latestDeviceJob :: Prelude.Maybe LatestDeviceJob,
    -- | When the device was updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | A description for the device.
    description :: Prelude.Maybe Prelude.Text,
    -- | The device\'s maker.
    brand :: Prelude.Maybe DeviceBrand,
    -- | A device\'s aggregated status. Including the device\'s connection
    -- status, provisioning status, and lease status.
    deviceAggregatedStatus :: Prelude.Maybe DeviceAggregatedStatus,
    -- | A device\'s current software.
    currentSoftware :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Device' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'device_tags' - The device\'s tags.
--
-- 'name', 'device_name' - The device\'s name.
--
-- 'type'', 'device_type' - The device\'s type.
--
-- 'createdTime', 'device_createdTime' - When the device was created.
--
-- 'leaseExpirationTime', 'device_leaseExpirationTime' - The device\'s lease expiration time.
--
-- 'provisioningStatus', 'device_provisioningStatus' - The device\'s provisioning status.
--
-- 'deviceId', 'device_deviceId' - The device\'s ID.
--
-- 'latestDeviceJob', 'device_latestDeviceJob' - A device\'s latest job. Includes the target image version, and the
-- update job status.
--
-- 'lastUpdatedTime', 'device_lastUpdatedTime' - When the device was updated.
--
-- 'description', 'device_description' - A description for the device.
--
-- 'brand', 'device_brand' - The device\'s maker.
--
-- 'deviceAggregatedStatus', 'device_deviceAggregatedStatus' - A device\'s aggregated status. Including the device\'s connection
-- status, provisioning status, and lease status.
--
-- 'currentSoftware', 'device_currentSoftware' - A device\'s current software.
newDevice ::
  Device
newDevice =
  Device'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      leaseExpirationTime = Prelude.Nothing,
      provisioningStatus = Prelude.Nothing,
      deviceId = Prelude.Nothing,
      latestDeviceJob = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      description = Prelude.Nothing,
      brand = Prelude.Nothing,
      deviceAggregatedStatus = Prelude.Nothing,
      currentSoftware = Prelude.Nothing
    }

-- | The device\'s tags.
device_tags :: Lens.Lens' Device (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
device_tags = Lens.lens (\Device' {tags} -> tags) (\s@Device' {} a -> s {tags = a} :: Device) Prelude.. Lens.mapping Lens.coerced

-- | The device\'s name.
device_name :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_name = Lens.lens (\Device' {name} -> name) (\s@Device' {} a -> s {name = a} :: Device)

-- | The device\'s type.
device_type :: Lens.Lens' Device (Prelude.Maybe DeviceType)
device_type = Lens.lens (\Device' {type'} -> type') (\s@Device' {} a -> s {type' = a} :: Device)

-- | When the device was created.
device_createdTime :: Lens.Lens' Device (Prelude.Maybe Prelude.UTCTime)
device_createdTime = Lens.lens (\Device' {createdTime} -> createdTime) (\s@Device' {} a -> s {createdTime = a} :: Device) Prelude.. Lens.mapping Core._Time

-- | The device\'s lease expiration time.
device_leaseExpirationTime :: Lens.Lens' Device (Prelude.Maybe Prelude.UTCTime)
device_leaseExpirationTime = Lens.lens (\Device' {leaseExpirationTime} -> leaseExpirationTime) (\s@Device' {} a -> s {leaseExpirationTime = a} :: Device) Prelude.. Lens.mapping Core._Time

-- | The device\'s provisioning status.
device_provisioningStatus :: Lens.Lens' Device (Prelude.Maybe DeviceStatus)
device_provisioningStatus = Lens.lens (\Device' {provisioningStatus} -> provisioningStatus) (\s@Device' {} a -> s {provisioningStatus = a} :: Device)

-- | The device\'s ID.
device_deviceId :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_deviceId = Lens.lens (\Device' {deviceId} -> deviceId) (\s@Device' {} a -> s {deviceId = a} :: Device)

-- | A device\'s latest job. Includes the target image version, and the
-- update job status.
device_latestDeviceJob :: Lens.Lens' Device (Prelude.Maybe LatestDeviceJob)
device_latestDeviceJob = Lens.lens (\Device' {latestDeviceJob} -> latestDeviceJob) (\s@Device' {} a -> s {latestDeviceJob = a} :: Device)

-- | When the device was updated.
device_lastUpdatedTime :: Lens.Lens' Device (Prelude.Maybe Prelude.UTCTime)
device_lastUpdatedTime = Lens.lens (\Device' {lastUpdatedTime} -> lastUpdatedTime) (\s@Device' {} a -> s {lastUpdatedTime = a} :: Device) Prelude.. Lens.mapping Core._Time

-- | A description for the device.
device_description :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_description = Lens.lens (\Device' {description} -> description) (\s@Device' {} a -> s {description = a} :: Device)

-- | The device\'s maker.
device_brand :: Lens.Lens' Device (Prelude.Maybe DeviceBrand)
device_brand = Lens.lens (\Device' {brand} -> brand) (\s@Device' {} a -> s {brand = a} :: Device)

-- | A device\'s aggregated status. Including the device\'s connection
-- status, provisioning status, and lease status.
device_deviceAggregatedStatus :: Lens.Lens' Device (Prelude.Maybe DeviceAggregatedStatus)
device_deviceAggregatedStatus = Lens.lens (\Device' {deviceAggregatedStatus} -> deviceAggregatedStatus) (\s@Device' {} a -> s {deviceAggregatedStatus = a} :: Device)

-- | A device\'s current software.
device_currentSoftware :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_currentSoftware = Lens.lens (\Device' {currentSoftware} -> currentSoftware) (\s@Device' {} a -> s {currentSoftware = a} :: Device)

instance Core.FromJSON Device where
  parseJSON =
    Core.withObject
      "Device"
      ( \x ->
          Device'
            Prelude.<$> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "LeaseExpirationTime")
            Prelude.<*> (x Core..:? "ProvisioningStatus")
            Prelude.<*> (x Core..:? "DeviceId")
            Prelude.<*> (x Core..:? "LatestDeviceJob")
            Prelude.<*> (x Core..:? "LastUpdatedTime")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "Brand")
            Prelude.<*> (x Core..:? "DeviceAggregatedStatus")
            Prelude.<*> (x Core..:? "CurrentSoftware")
      )

instance Prelude.Hashable Device where
  hashWithSalt _salt Device' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` leaseExpirationTime
      `Prelude.hashWithSalt` provisioningStatus
      `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` latestDeviceJob
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` brand
      `Prelude.hashWithSalt` deviceAggregatedStatus
      `Prelude.hashWithSalt` currentSoftware

instance Prelude.NFData Device where
  rnf Device' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf leaseExpirationTime
      `Prelude.seq` Prelude.rnf provisioningStatus
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf latestDeviceJob
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf brand
      `Prelude.seq` Prelude.rnf deviceAggregatedStatus
      `Prelude.seq` Prelude.rnf currentSoftware
