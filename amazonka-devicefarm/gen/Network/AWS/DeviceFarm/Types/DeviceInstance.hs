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
-- Module      : Network.AWS.DeviceFarm.Types.DeviceInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DeviceInstance where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types.InstanceProfile
import Network.AWS.DeviceFarm.Types.InstanceStatus
import qualified Network.AWS.Lens as Lens

-- | Represents the device instance.
--
-- /See:/ 'newDeviceInstance' smart constructor.
data DeviceInstance = DeviceInstance'
  { -- | Unique device identifier for the device instance.
    udid :: Core.Maybe Core.Text,
    -- | The status of the device instance. Valid values are listed here.
    status :: Core.Maybe InstanceStatus,
    -- | The ARN of the device.
    deviceArn :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the device instance.
    arn :: Core.Maybe Core.Text,
    -- | An array of strings that describe the device instance.
    labels :: Core.Maybe [Core.Text],
    -- | A object that contains information about the instance profile.
    instanceProfile :: Core.Maybe InstanceProfile
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeviceInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'udid', 'deviceInstance_udid' - Unique device identifier for the device instance.
--
-- 'status', 'deviceInstance_status' - The status of the device instance. Valid values are listed here.
--
-- 'deviceArn', 'deviceInstance_deviceArn' - The ARN of the device.
--
-- 'arn', 'deviceInstance_arn' - The Amazon Resource Name (ARN) of the device instance.
--
-- 'labels', 'deviceInstance_labels' - An array of strings that describe the device instance.
--
-- 'instanceProfile', 'deviceInstance_instanceProfile' - A object that contains information about the instance profile.
newDeviceInstance ::
  DeviceInstance
newDeviceInstance =
  DeviceInstance'
    { udid = Core.Nothing,
      status = Core.Nothing,
      deviceArn = Core.Nothing,
      arn = Core.Nothing,
      labels = Core.Nothing,
      instanceProfile = Core.Nothing
    }

-- | Unique device identifier for the device instance.
deviceInstance_udid :: Lens.Lens' DeviceInstance (Core.Maybe Core.Text)
deviceInstance_udid = Lens.lens (\DeviceInstance' {udid} -> udid) (\s@DeviceInstance' {} a -> s {udid = a} :: DeviceInstance)

-- | The status of the device instance. Valid values are listed here.
deviceInstance_status :: Lens.Lens' DeviceInstance (Core.Maybe InstanceStatus)
deviceInstance_status = Lens.lens (\DeviceInstance' {status} -> status) (\s@DeviceInstance' {} a -> s {status = a} :: DeviceInstance)

-- | The ARN of the device.
deviceInstance_deviceArn :: Lens.Lens' DeviceInstance (Core.Maybe Core.Text)
deviceInstance_deviceArn = Lens.lens (\DeviceInstance' {deviceArn} -> deviceArn) (\s@DeviceInstance' {} a -> s {deviceArn = a} :: DeviceInstance)

-- | The Amazon Resource Name (ARN) of the device instance.
deviceInstance_arn :: Lens.Lens' DeviceInstance (Core.Maybe Core.Text)
deviceInstance_arn = Lens.lens (\DeviceInstance' {arn} -> arn) (\s@DeviceInstance' {} a -> s {arn = a} :: DeviceInstance)

-- | An array of strings that describe the device instance.
deviceInstance_labels :: Lens.Lens' DeviceInstance (Core.Maybe [Core.Text])
deviceInstance_labels = Lens.lens (\DeviceInstance' {labels} -> labels) (\s@DeviceInstance' {} a -> s {labels = a} :: DeviceInstance) Core.. Lens.mapping Lens._Coerce

-- | A object that contains information about the instance profile.
deviceInstance_instanceProfile :: Lens.Lens' DeviceInstance (Core.Maybe InstanceProfile)
deviceInstance_instanceProfile = Lens.lens (\DeviceInstance' {instanceProfile} -> instanceProfile) (\s@DeviceInstance' {} a -> s {instanceProfile = a} :: DeviceInstance)

instance Core.FromJSON DeviceInstance where
  parseJSON =
    Core.withObject
      "DeviceInstance"
      ( \x ->
          DeviceInstance'
            Core.<$> (x Core..:? "udid")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "deviceArn")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "labels" Core..!= Core.mempty)
            Core.<*> (x Core..:? "instanceProfile")
      )

instance Core.Hashable DeviceInstance

instance Core.NFData DeviceInstance
