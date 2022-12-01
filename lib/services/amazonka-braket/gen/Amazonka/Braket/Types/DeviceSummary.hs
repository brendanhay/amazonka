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
-- Module      : Amazonka.Braket.Types.DeviceSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Braket.Types.DeviceSummary where

import Amazonka.Braket.Types.DeviceStatus
import Amazonka.Braket.Types.DeviceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Includes information about the device.
--
-- /See:/ 'newDeviceSummary' smart constructor.
data DeviceSummary = DeviceSummary'
  { -- | The ARN of the device.
    deviceArn :: Prelude.Text,
    -- | The name of the device.
    deviceName :: Prelude.Text,
    -- | The status of the device.
    deviceStatus :: DeviceStatus,
    -- | The type of the device.
    deviceType :: DeviceType,
    -- | The provider of the device.
    providerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceArn', 'deviceSummary_deviceArn' - The ARN of the device.
--
-- 'deviceName', 'deviceSummary_deviceName' - The name of the device.
--
-- 'deviceStatus', 'deviceSummary_deviceStatus' - The status of the device.
--
-- 'deviceType', 'deviceSummary_deviceType' - The type of the device.
--
-- 'providerName', 'deviceSummary_providerName' - The provider of the device.
newDeviceSummary ::
  -- | 'deviceArn'
  Prelude.Text ->
  -- | 'deviceName'
  Prelude.Text ->
  -- | 'deviceStatus'
  DeviceStatus ->
  -- | 'deviceType'
  DeviceType ->
  -- | 'providerName'
  Prelude.Text ->
  DeviceSummary
newDeviceSummary
  pDeviceArn_
  pDeviceName_
  pDeviceStatus_
  pDeviceType_
  pProviderName_ =
    DeviceSummary'
      { deviceArn = pDeviceArn_,
        deviceName = pDeviceName_,
        deviceStatus = pDeviceStatus_,
        deviceType = pDeviceType_,
        providerName = pProviderName_
      }

-- | The ARN of the device.
deviceSummary_deviceArn :: Lens.Lens' DeviceSummary Prelude.Text
deviceSummary_deviceArn = Lens.lens (\DeviceSummary' {deviceArn} -> deviceArn) (\s@DeviceSummary' {} a -> s {deviceArn = a} :: DeviceSummary)

-- | The name of the device.
deviceSummary_deviceName :: Lens.Lens' DeviceSummary Prelude.Text
deviceSummary_deviceName = Lens.lens (\DeviceSummary' {deviceName} -> deviceName) (\s@DeviceSummary' {} a -> s {deviceName = a} :: DeviceSummary)

-- | The status of the device.
deviceSummary_deviceStatus :: Lens.Lens' DeviceSummary DeviceStatus
deviceSummary_deviceStatus = Lens.lens (\DeviceSummary' {deviceStatus} -> deviceStatus) (\s@DeviceSummary' {} a -> s {deviceStatus = a} :: DeviceSummary)

-- | The type of the device.
deviceSummary_deviceType :: Lens.Lens' DeviceSummary DeviceType
deviceSummary_deviceType = Lens.lens (\DeviceSummary' {deviceType} -> deviceType) (\s@DeviceSummary' {} a -> s {deviceType = a} :: DeviceSummary)

-- | The provider of the device.
deviceSummary_providerName :: Lens.Lens' DeviceSummary Prelude.Text
deviceSummary_providerName = Lens.lens (\DeviceSummary' {providerName} -> providerName) (\s@DeviceSummary' {} a -> s {providerName = a} :: DeviceSummary)

instance Core.FromJSON DeviceSummary where
  parseJSON =
    Core.withObject
      "DeviceSummary"
      ( \x ->
          DeviceSummary'
            Prelude.<$> (x Core..: "deviceArn")
            Prelude.<*> (x Core..: "deviceName")
            Prelude.<*> (x Core..: "deviceStatus")
            Prelude.<*> (x Core..: "deviceType")
            Prelude.<*> (x Core..: "providerName")
      )

instance Prelude.Hashable DeviceSummary where
  hashWithSalt _salt DeviceSummary' {..} =
    _salt `Prelude.hashWithSalt` deviceArn
      `Prelude.hashWithSalt` deviceName
      `Prelude.hashWithSalt` deviceStatus
      `Prelude.hashWithSalt` deviceType
      `Prelude.hashWithSalt` providerName

instance Prelude.NFData DeviceSummary where
  rnf DeviceSummary' {..} =
    Prelude.rnf deviceArn
      `Prelude.seq` Prelude.rnf deviceName
      `Prelude.seq` Prelude.rnf deviceStatus
      `Prelude.seq` Prelude.rnf deviceType
      `Prelude.seq` Prelude.rnf providerName
