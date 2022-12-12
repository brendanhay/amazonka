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
import qualified Amazonka.Data as Data
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
  { -- | The device\'s maker.
    brand :: Prelude.Maybe DeviceBrand,
    -- | When the device was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | A device\'s current software.
    currentSoftware :: Prelude.Maybe Prelude.Text,
    -- | A description for the device.
    description :: Prelude.Maybe Prelude.Text,
    -- | A device\'s aggregated status. Including the device\'s connection
    -- status, provisioning status, and lease status.
    deviceAggregatedStatus :: Prelude.Maybe DeviceAggregatedStatus,
    -- | The device\'s ID.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | When the device was updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | A device\'s latest job. Includes the target image version, and the
    -- update job status.
    latestDeviceJob :: Prelude.Maybe LatestDeviceJob,
    -- | The device\'s lease expiration time.
    leaseExpirationTime :: Prelude.Maybe Data.POSIX,
    -- | The device\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The device\'s provisioning status.
    provisioningStatus :: Prelude.Maybe DeviceStatus,
    -- | The device\'s tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The device\'s type.
    type' :: Prelude.Maybe DeviceType
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
-- 'brand', 'device_brand' - The device\'s maker.
--
-- 'createdTime', 'device_createdTime' - When the device was created.
--
-- 'currentSoftware', 'device_currentSoftware' - A device\'s current software.
--
-- 'description', 'device_description' - A description for the device.
--
-- 'deviceAggregatedStatus', 'device_deviceAggregatedStatus' - A device\'s aggregated status. Including the device\'s connection
-- status, provisioning status, and lease status.
--
-- 'deviceId', 'device_deviceId' - The device\'s ID.
--
-- 'lastUpdatedTime', 'device_lastUpdatedTime' - When the device was updated.
--
-- 'latestDeviceJob', 'device_latestDeviceJob' - A device\'s latest job. Includes the target image version, and the
-- update job status.
--
-- 'leaseExpirationTime', 'device_leaseExpirationTime' - The device\'s lease expiration time.
--
-- 'name', 'device_name' - The device\'s name.
--
-- 'provisioningStatus', 'device_provisioningStatus' - The device\'s provisioning status.
--
-- 'tags', 'device_tags' - The device\'s tags.
--
-- 'type'', 'device_type' - The device\'s type.
newDevice ::
  Device
newDevice =
  Device'
    { brand = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      currentSoftware = Prelude.Nothing,
      description = Prelude.Nothing,
      deviceAggregatedStatus = Prelude.Nothing,
      deviceId = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      latestDeviceJob = Prelude.Nothing,
      leaseExpirationTime = Prelude.Nothing,
      name = Prelude.Nothing,
      provisioningStatus = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The device\'s maker.
device_brand :: Lens.Lens' Device (Prelude.Maybe DeviceBrand)
device_brand = Lens.lens (\Device' {brand} -> brand) (\s@Device' {} a -> s {brand = a} :: Device)

-- | When the device was created.
device_createdTime :: Lens.Lens' Device (Prelude.Maybe Prelude.UTCTime)
device_createdTime = Lens.lens (\Device' {createdTime} -> createdTime) (\s@Device' {} a -> s {createdTime = a} :: Device) Prelude.. Lens.mapping Data._Time

-- | A device\'s current software.
device_currentSoftware :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_currentSoftware = Lens.lens (\Device' {currentSoftware} -> currentSoftware) (\s@Device' {} a -> s {currentSoftware = a} :: Device)

-- | A description for the device.
device_description :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_description = Lens.lens (\Device' {description} -> description) (\s@Device' {} a -> s {description = a} :: Device)

-- | A device\'s aggregated status. Including the device\'s connection
-- status, provisioning status, and lease status.
device_deviceAggregatedStatus :: Lens.Lens' Device (Prelude.Maybe DeviceAggregatedStatus)
device_deviceAggregatedStatus = Lens.lens (\Device' {deviceAggregatedStatus} -> deviceAggregatedStatus) (\s@Device' {} a -> s {deviceAggregatedStatus = a} :: Device)

-- | The device\'s ID.
device_deviceId :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_deviceId = Lens.lens (\Device' {deviceId} -> deviceId) (\s@Device' {} a -> s {deviceId = a} :: Device)

-- | When the device was updated.
device_lastUpdatedTime :: Lens.Lens' Device (Prelude.Maybe Prelude.UTCTime)
device_lastUpdatedTime = Lens.lens (\Device' {lastUpdatedTime} -> lastUpdatedTime) (\s@Device' {} a -> s {lastUpdatedTime = a} :: Device) Prelude.. Lens.mapping Data._Time

-- | A device\'s latest job. Includes the target image version, and the
-- update job status.
device_latestDeviceJob :: Lens.Lens' Device (Prelude.Maybe LatestDeviceJob)
device_latestDeviceJob = Lens.lens (\Device' {latestDeviceJob} -> latestDeviceJob) (\s@Device' {} a -> s {latestDeviceJob = a} :: Device)

-- | The device\'s lease expiration time.
device_leaseExpirationTime :: Lens.Lens' Device (Prelude.Maybe Prelude.UTCTime)
device_leaseExpirationTime = Lens.lens (\Device' {leaseExpirationTime} -> leaseExpirationTime) (\s@Device' {} a -> s {leaseExpirationTime = a} :: Device) Prelude.. Lens.mapping Data._Time

-- | The device\'s name.
device_name :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_name = Lens.lens (\Device' {name} -> name) (\s@Device' {} a -> s {name = a} :: Device)

-- | The device\'s provisioning status.
device_provisioningStatus :: Lens.Lens' Device (Prelude.Maybe DeviceStatus)
device_provisioningStatus = Lens.lens (\Device' {provisioningStatus} -> provisioningStatus) (\s@Device' {} a -> s {provisioningStatus = a} :: Device)

-- | The device\'s tags.
device_tags :: Lens.Lens' Device (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
device_tags = Lens.lens (\Device' {tags} -> tags) (\s@Device' {} a -> s {tags = a} :: Device) Prelude.. Lens.mapping Lens.coerced

-- | The device\'s type.
device_type :: Lens.Lens' Device (Prelude.Maybe DeviceType)
device_type = Lens.lens (\Device' {type'} -> type') (\s@Device' {} a -> s {type' = a} :: Device)

instance Data.FromJSON Device where
  parseJSON =
    Data.withObject
      "Device"
      ( \x ->
          Device'
            Prelude.<$> (x Data..:? "Brand")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "CurrentSoftware")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "DeviceAggregatedStatus")
            Prelude.<*> (x Data..:? "DeviceId")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "LatestDeviceJob")
            Prelude.<*> (x Data..:? "LeaseExpirationTime")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ProvisioningStatus")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable Device where
  hashWithSalt _salt Device' {..} =
    _salt `Prelude.hashWithSalt` brand
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` currentSoftware
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` deviceAggregatedStatus
      `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` latestDeviceJob
      `Prelude.hashWithSalt` leaseExpirationTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` provisioningStatus
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Device where
  rnf Device' {..} =
    Prelude.rnf brand
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf currentSoftware
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf deviceAggregatedStatus
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf latestDeviceJob
      `Prelude.seq` Prelude.rnf leaseExpirationTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf provisioningStatus
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
