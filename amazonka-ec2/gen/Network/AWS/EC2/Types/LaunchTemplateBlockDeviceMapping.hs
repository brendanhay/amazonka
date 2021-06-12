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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateBlockDeviceMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateBlockDeviceMapping where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.LaunchTemplateEbsBlockDevice
import qualified Network.AWS.Lens as Lens

-- | Describes a block device mapping.
--
-- /See:/ 'newLaunchTemplateBlockDeviceMapping' smart constructor.
data LaunchTemplateBlockDeviceMapping = LaunchTemplateBlockDeviceMapping'
  { -- | Information about the block device for an EBS volume.
    ebs :: Core.Maybe LaunchTemplateEbsBlockDevice,
    -- | To omit the device from the block device mapping, specify an empty
    -- string.
    noDevice :: Core.Maybe Core.Text,
    -- | The virtual device name (ephemeralN).
    virtualName :: Core.Maybe Core.Text,
    -- | The device name.
    deviceName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LaunchTemplateBlockDeviceMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebs', 'launchTemplateBlockDeviceMapping_ebs' - Information about the block device for an EBS volume.
--
-- 'noDevice', 'launchTemplateBlockDeviceMapping_noDevice' - To omit the device from the block device mapping, specify an empty
-- string.
--
-- 'virtualName', 'launchTemplateBlockDeviceMapping_virtualName' - The virtual device name (ephemeralN).
--
-- 'deviceName', 'launchTemplateBlockDeviceMapping_deviceName' - The device name.
newLaunchTemplateBlockDeviceMapping ::
  LaunchTemplateBlockDeviceMapping
newLaunchTemplateBlockDeviceMapping =
  LaunchTemplateBlockDeviceMapping'
    { ebs =
        Core.Nothing,
      noDevice = Core.Nothing,
      virtualName = Core.Nothing,
      deviceName = Core.Nothing
    }

-- | Information about the block device for an EBS volume.
launchTemplateBlockDeviceMapping_ebs :: Lens.Lens' LaunchTemplateBlockDeviceMapping (Core.Maybe LaunchTemplateEbsBlockDevice)
launchTemplateBlockDeviceMapping_ebs = Lens.lens (\LaunchTemplateBlockDeviceMapping' {ebs} -> ebs) (\s@LaunchTemplateBlockDeviceMapping' {} a -> s {ebs = a} :: LaunchTemplateBlockDeviceMapping)

-- | To omit the device from the block device mapping, specify an empty
-- string.
launchTemplateBlockDeviceMapping_noDevice :: Lens.Lens' LaunchTemplateBlockDeviceMapping (Core.Maybe Core.Text)
launchTemplateBlockDeviceMapping_noDevice = Lens.lens (\LaunchTemplateBlockDeviceMapping' {noDevice} -> noDevice) (\s@LaunchTemplateBlockDeviceMapping' {} a -> s {noDevice = a} :: LaunchTemplateBlockDeviceMapping)

-- | The virtual device name (ephemeralN).
launchTemplateBlockDeviceMapping_virtualName :: Lens.Lens' LaunchTemplateBlockDeviceMapping (Core.Maybe Core.Text)
launchTemplateBlockDeviceMapping_virtualName = Lens.lens (\LaunchTemplateBlockDeviceMapping' {virtualName} -> virtualName) (\s@LaunchTemplateBlockDeviceMapping' {} a -> s {virtualName = a} :: LaunchTemplateBlockDeviceMapping)

-- | The device name.
launchTemplateBlockDeviceMapping_deviceName :: Lens.Lens' LaunchTemplateBlockDeviceMapping (Core.Maybe Core.Text)
launchTemplateBlockDeviceMapping_deviceName = Lens.lens (\LaunchTemplateBlockDeviceMapping' {deviceName} -> deviceName) (\s@LaunchTemplateBlockDeviceMapping' {} a -> s {deviceName = a} :: LaunchTemplateBlockDeviceMapping)

instance
  Core.FromXML
    LaunchTemplateBlockDeviceMapping
  where
  parseXML x =
    LaunchTemplateBlockDeviceMapping'
      Core.<$> (x Core..@? "ebs")
      Core.<*> (x Core..@? "noDevice")
      Core.<*> (x Core..@? "virtualName")
      Core.<*> (x Core..@? "deviceName")

instance
  Core.Hashable
    LaunchTemplateBlockDeviceMapping

instance Core.NFData LaunchTemplateBlockDeviceMapping
