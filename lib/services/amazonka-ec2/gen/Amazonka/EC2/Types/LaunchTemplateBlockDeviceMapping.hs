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
-- Module      : Amazonka.EC2.Types.LaunchTemplateBlockDeviceMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateBlockDeviceMapping where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.LaunchTemplateEbsBlockDevice
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a block device mapping.
--
-- /See:/ 'newLaunchTemplateBlockDeviceMapping' smart constructor.
data LaunchTemplateBlockDeviceMapping = LaunchTemplateBlockDeviceMapping'
  { -- | The virtual device name (ephemeralN).
    virtualName :: Prelude.Maybe Prelude.Text,
    -- | To omit the device from the block device mapping, specify an empty
    -- string.
    noDevice :: Prelude.Maybe Prelude.Text,
    -- | Information about the block device for an EBS volume.
    ebs :: Prelude.Maybe LaunchTemplateEbsBlockDevice,
    -- | The device name.
    deviceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateBlockDeviceMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualName', 'launchTemplateBlockDeviceMapping_virtualName' - The virtual device name (ephemeralN).
--
-- 'noDevice', 'launchTemplateBlockDeviceMapping_noDevice' - To omit the device from the block device mapping, specify an empty
-- string.
--
-- 'ebs', 'launchTemplateBlockDeviceMapping_ebs' - Information about the block device for an EBS volume.
--
-- 'deviceName', 'launchTemplateBlockDeviceMapping_deviceName' - The device name.
newLaunchTemplateBlockDeviceMapping ::
  LaunchTemplateBlockDeviceMapping
newLaunchTemplateBlockDeviceMapping =
  LaunchTemplateBlockDeviceMapping'
    { virtualName =
        Prelude.Nothing,
      noDevice = Prelude.Nothing,
      ebs = Prelude.Nothing,
      deviceName = Prelude.Nothing
    }

-- | The virtual device name (ephemeralN).
launchTemplateBlockDeviceMapping_virtualName :: Lens.Lens' LaunchTemplateBlockDeviceMapping (Prelude.Maybe Prelude.Text)
launchTemplateBlockDeviceMapping_virtualName = Lens.lens (\LaunchTemplateBlockDeviceMapping' {virtualName} -> virtualName) (\s@LaunchTemplateBlockDeviceMapping' {} a -> s {virtualName = a} :: LaunchTemplateBlockDeviceMapping)

-- | To omit the device from the block device mapping, specify an empty
-- string.
launchTemplateBlockDeviceMapping_noDevice :: Lens.Lens' LaunchTemplateBlockDeviceMapping (Prelude.Maybe Prelude.Text)
launchTemplateBlockDeviceMapping_noDevice = Lens.lens (\LaunchTemplateBlockDeviceMapping' {noDevice} -> noDevice) (\s@LaunchTemplateBlockDeviceMapping' {} a -> s {noDevice = a} :: LaunchTemplateBlockDeviceMapping)

-- | Information about the block device for an EBS volume.
launchTemplateBlockDeviceMapping_ebs :: Lens.Lens' LaunchTemplateBlockDeviceMapping (Prelude.Maybe LaunchTemplateEbsBlockDevice)
launchTemplateBlockDeviceMapping_ebs = Lens.lens (\LaunchTemplateBlockDeviceMapping' {ebs} -> ebs) (\s@LaunchTemplateBlockDeviceMapping' {} a -> s {ebs = a} :: LaunchTemplateBlockDeviceMapping)

-- | The device name.
launchTemplateBlockDeviceMapping_deviceName :: Lens.Lens' LaunchTemplateBlockDeviceMapping (Prelude.Maybe Prelude.Text)
launchTemplateBlockDeviceMapping_deviceName = Lens.lens (\LaunchTemplateBlockDeviceMapping' {deviceName} -> deviceName) (\s@LaunchTemplateBlockDeviceMapping' {} a -> s {deviceName = a} :: LaunchTemplateBlockDeviceMapping)

instance
  Core.FromXML
    LaunchTemplateBlockDeviceMapping
  where
  parseXML x =
    LaunchTemplateBlockDeviceMapping'
      Prelude.<$> (x Core..@? "virtualName")
      Prelude.<*> (x Core..@? "noDevice")
      Prelude.<*> (x Core..@? "ebs")
      Prelude.<*> (x Core..@? "deviceName")

instance
  Prelude.Hashable
    LaunchTemplateBlockDeviceMapping
  where
  hashWithSalt
    _salt
    LaunchTemplateBlockDeviceMapping' {..} =
      _salt `Prelude.hashWithSalt` virtualName
        `Prelude.hashWithSalt` noDevice
        `Prelude.hashWithSalt` ebs
        `Prelude.hashWithSalt` deviceName

instance
  Prelude.NFData
    LaunchTemplateBlockDeviceMapping
  where
  rnf LaunchTemplateBlockDeviceMapping' {..} =
    Prelude.rnf virtualName
      `Prelude.seq` Prelude.rnf noDevice
      `Prelude.seq` Prelude.rnf ebs
      `Prelude.seq` Prelude.rnf deviceName
