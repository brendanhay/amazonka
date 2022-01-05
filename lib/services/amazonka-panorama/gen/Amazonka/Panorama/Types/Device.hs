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
import Amazonka.Panorama.Types.DeviceStatus
import qualified Amazonka.Prelude as Prelude

-- | A device.
--
-- /See:/ 'newDevice' smart constructor.
data Device = Device'
  { -- | When the device was updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The device\'s provisioning status.
    provisioningStatus :: Prelude.Maybe DeviceStatus,
    -- | When the device was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The device\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The device\'s ID.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The device\'s lease expiration time.
    leaseExpirationTime :: Prelude.Maybe Core.POSIX
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
-- 'lastUpdatedTime', 'device_lastUpdatedTime' - When the device was updated.
--
-- 'provisioningStatus', 'device_provisioningStatus' - The device\'s provisioning status.
--
-- 'createdTime', 'device_createdTime' - When the device was created.
--
-- 'name', 'device_name' - The device\'s name.
--
-- 'deviceId', 'device_deviceId' - The device\'s ID.
--
-- 'leaseExpirationTime', 'device_leaseExpirationTime' - The device\'s lease expiration time.
newDevice ::
  Device
newDevice =
  Device'
    { lastUpdatedTime = Prelude.Nothing,
      provisioningStatus = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      name = Prelude.Nothing,
      deviceId = Prelude.Nothing,
      leaseExpirationTime = Prelude.Nothing
    }

-- | When the device was updated.
device_lastUpdatedTime :: Lens.Lens' Device (Prelude.Maybe Prelude.UTCTime)
device_lastUpdatedTime = Lens.lens (\Device' {lastUpdatedTime} -> lastUpdatedTime) (\s@Device' {} a -> s {lastUpdatedTime = a} :: Device) Prelude.. Lens.mapping Core._Time

-- | The device\'s provisioning status.
device_provisioningStatus :: Lens.Lens' Device (Prelude.Maybe DeviceStatus)
device_provisioningStatus = Lens.lens (\Device' {provisioningStatus} -> provisioningStatus) (\s@Device' {} a -> s {provisioningStatus = a} :: Device)

-- | When the device was created.
device_createdTime :: Lens.Lens' Device (Prelude.Maybe Prelude.UTCTime)
device_createdTime = Lens.lens (\Device' {createdTime} -> createdTime) (\s@Device' {} a -> s {createdTime = a} :: Device) Prelude.. Lens.mapping Core._Time

-- | The device\'s name.
device_name :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_name = Lens.lens (\Device' {name} -> name) (\s@Device' {} a -> s {name = a} :: Device)

-- | The device\'s ID.
device_deviceId :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_deviceId = Lens.lens (\Device' {deviceId} -> deviceId) (\s@Device' {} a -> s {deviceId = a} :: Device)

-- | The device\'s lease expiration time.
device_leaseExpirationTime :: Lens.Lens' Device (Prelude.Maybe Prelude.UTCTime)
device_leaseExpirationTime = Lens.lens (\Device' {leaseExpirationTime} -> leaseExpirationTime) (\s@Device' {} a -> s {leaseExpirationTime = a} :: Device) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Device where
  parseJSON =
    Core.withObject
      "Device"
      ( \x ->
          Device'
            Prelude.<$> (x Core..:? "LastUpdatedTime")
            Prelude.<*> (x Core..:? "ProvisioningStatus")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "DeviceId")
            Prelude.<*> (x Core..:? "LeaseExpirationTime")
      )

instance Prelude.Hashable Device where
  hashWithSalt _salt Device' {..} =
    _salt `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` provisioningStatus
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` leaseExpirationTime

instance Prelude.NFData Device where
  rnf Device' {..} =
    Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf provisioningStatus
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf leaseExpirationTime
