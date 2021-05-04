{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DeviceFarm.Types.InstanceProfile
import Network.AWS.DeviceFarm.Types.InstanceStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the device instance.
--
-- /See:/ 'newDeviceInstance' smart constructor.
data DeviceInstance = DeviceInstance'
  { -- | Unique device identifier for the device instance.
    udid :: Prelude.Maybe Prelude.Text,
    -- | The status of the device instance. Valid values are listed here.
    status :: Prelude.Maybe InstanceStatus,
    -- | The ARN of the device.
    deviceArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the device instance.
    arn :: Prelude.Maybe Prelude.Text,
    -- | An array of strings that describe the device instance.
    labels :: Prelude.Maybe [Prelude.Text],
    -- | A object that contains information about the instance profile.
    instanceProfile :: Prelude.Maybe InstanceProfile
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { udid = Prelude.Nothing,
      status = Prelude.Nothing,
      deviceArn = Prelude.Nothing,
      arn = Prelude.Nothing,
      labels = Prelude.Nothing,
      instanceProfile = Prelude.Nothing
    }

-- | Unique device identifier for the device instance.
deviceInstance_udid :: Lens.Lens' DeviceInstance (Prelude.Maybe Prelude.Text)
deviceInstance_udid = Lens.lens (\DeviceInstance' {udid} -> udid) (\s@DeviceInstance' {} a -> s {udid = a} :: DeviceInstance)

-- | The status of the device instance. Valid values are listed here.
deviceInstance_status :: Lens.Lens' DeviceInstance (Prelude.Maybe InstanceStatus)
deviceInstance_status = Lens.lens (\DeviceInstance' {status} -> status) (\s@DeviceInstance' {} a -> s {status = a} :: DeviceInstance)

-- | The ARN of the device.
deviceInstance_deviceArn :: Lens.Lens' DeviceInstance (Prelude.Maybe Prelude.Text)
deviceInstance_deviceArn = Lens.lens (\DeviceInstance' {deviceArn} -> deviceArn) (\s@DeviceInstance' {} a -> s {deviceArn = a} :: DeviceInstance)

-- | The Amazon Resource Name (ARN) of the device instance.
deviceInstance_arn :: Lens.Lens' DeviceInstance (Prelude.Maybe Prelude.Text)
deviceInstance_arn = Lens.lens (\DeviceInstance' {arn} -> arn) (\s@DeviceInstance' {} a -> s {arn = a} :: DeviceInstance)

-- | An array of strings that describe the device instance.
deviceInstance_labels :: Lens.Lens' DeviceInstance (Prelude.Maybe [Prelude.Text])
deviceInstance_labels = Lens.lens (\DeviceInstance' {labels} -> labels) (\s@DeviceInstance' {} a -> s {labels = a} :: DeviceInstance) Prelude.. Lens.mapping Prelude._Coerce

-- | A object that contains information about the instance profile.
deviceInstance_instanceProfile :: Lens.Lens' DeviceInstance (Prelude.Maybe InstanceProfile)
deviceInstance_instanceProfile = Lens.lens (\DeviceInstance' {instanceProfile} -> instanceProfile) (\s@DeviceInstance' {} a -> s {instanceProfile = a} :: DeviceInstance)

instance Prelude.FromJSON DeviceInstance where
  parseJSON =
    Prelude.withObject
      "DeviceInstance"
      ( \x ->
          DeviceInstance'
            Prelude.<$> (x Prelude..:? "udid")
            Prelude.<*> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "deviceArn")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "labels" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "instanceProfile")
      )

instance Prelude.Hashable DeviceInstance

instance Prelude.NFData DeviceInstance
