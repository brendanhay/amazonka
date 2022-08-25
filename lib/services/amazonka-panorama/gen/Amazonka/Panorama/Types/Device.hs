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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.Device where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Panorama.Types.DeviceBrand
import Amazonka.Panorama.Types.DeviceStatus
import qualified Amazonka.Prelude as Prelude

-- | A device.
--
-- /See:/ 'newDevice' smart constructor.
data Device = Device'
  { -- | The device\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | When the device was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The device\'s lease expiration time.
    leaseExpirationTime :: Prelude.Maybe Core.POSIX,
    -- | The device\'s provisioning status.
    provisioningStatus :: Prelude.Maybe DeviceStatus,
    -- | The device\'s ID.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | When the device was updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The device\'s maker.
    brand :: Prelude.Maybe DeviceBrand
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
-- 'name', 'device_name' - The device\'s name.
--
-- 'createdTime', 'device_createdTime' - When the device was created.
--
-- 'leaseExpirationTime', 'device_leaseExpirationTime' - The device\'s lease expiration time.
--
-- 'provisioningStatus', 'device_provisioningStatus' - The device\'s provisioning status.
--
-- 'deviceId', 'device_deviceId' - The device\'s ID.
--
-- 'lastUpdatedTime', 'device_lastUpdatedTime' - When the device was updated.
--
-- 'brand', 'device_brand' - The device\'s maker.
newDevice ::
  Device
newDevice =
  Device'
    { name = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      leaseExpirationTime = Prelude.Nothing,
      provisioningStatus = Prelude.Nothing,
      deviceId = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      brand = Prelude.Nothing
    }

-- | The device\'s name.
device_name :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_name = Lens.lens (\Device' {name} -> name) (\s@Device' {} a -> s {name = a} :: Device)

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

-- | When the device was updated.
device_lastUpdatedTime :: Lens.Lens' Device (Prelude.Maybe Prelude.UTCTime)
device_lastUpdatedTime = Lens.lens (\Device' {lastUpdatedTime} -> lastUpdatedTime) (\s@Device' {} a -> s {lastUpdatedTime = a} :: Device) Prelude.. Lens.mapping Core._Time

-- | The device\'s maker.
device_brand :: Lens.Lens' Device (Prelude.Maybe DeviceBrand)
device_brand = Lens.lens (\Device' {brand} -> brand) (\s@Device' {} a -> s {brand = a} :: Device)

instance Core.FromJSON Device where
  parseJSON =
    Core.withObject
      "Device"
      ( \x ->
          Device'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "LeaseExpirationTime")
            Prelude.<*> (x Core..:? "ProvisioningStatus")
            Prelude.<*> (x Core..:? "DeviceId")
            Prelude.<*> (x Core..:? "LastUpdatedTime")
            Prelude.<*> (x Core..:? "Brand")
      )

instance Prelude.Hashable Device where
  hashWithSalt _salt Device' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` leaseExpirationTime
      `Prelude.hashWithSalt` provisioningStatus
      `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` brand

instance Prelude.NFData Device where
  rnf Device' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf leaseExpirationTime
      `Prelude.seq` Prelude.rnf provisioningStatus
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf brand
