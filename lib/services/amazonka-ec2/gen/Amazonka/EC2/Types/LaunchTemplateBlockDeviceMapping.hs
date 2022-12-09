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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateBlockDeviceMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.LaunchTemplateEbsBlockDevice
import qualified Amazonka.Prelude as Prelude

-- | Describes a block device mapping.
--
-- /See:/ 'newLaunchTemplateBlockDeviceMapping' smart constructor.
data LaunchTemplateBlockDeviceMapping = LaunchTemplateBlockDeviceMapping'
  { -- | The device name.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | Information about the block device for an EBS volume.
    ebs :: Prelude.Maybe LaunchTemplateEbsBlockDevice,
    -- | To omit the device from the block device mapping, specify an empty
    -- string.
    noDevice :: Prelude.Maybe Prelude.Text,
    -- | The virtual device name (ephemeralN).
    virtualName :: Prelude.Maybe Prelude.Text
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
-- 'deviceName', 'launchTemplateBlockDeviceMapping_deviceName' - The device name.
--
-- 'ebs', 'launchTemplateBlockDeviceMapping_ebs' - Information about the block device for an EBS volume.
--
-- 'noDevice', 'launchTemplateBlockDeviceMapping_noDevice' - To omit the device from the block device mapping, specify an empty
-- string.
--
-- 'virtualName', 'launchTemplateBlockDeviceMapping_virtualName' - The virtual device name (ephemeralN).
newLaunchTemplateBlockDeviceMapping ::
  LaunchTemplateBlockDeviceMapping
newLaunchTemplateBlockDeviceMapping =
  LaunchTemplateBlockDeviceMapping'
    { deviceName =
        Prelude.Nothing,
      ebs = Prelude.Nothing,
      noDevice = Prelude.Nothing,
      virtualName = Prelude.Nothing
    }

-- | The device name.
launchTemplateBlockDeviceMapping_deviceName :: Lens.Lens' LaunchTemplateBlockDeviceMapping (Prelude.Maybe Prelude.Text)
launchTemplateBlockDeviceMapping_deviceName = Lens.lens (\LaunchTemplateBlockDeviceMapping' {deviceName} -> deviceName) (\s@LaunchTemplateBlockDeviceMapping' {} a -> s {deviceName = a} :: LaunchTemplateBlockDeviceMapping)

-- | Information about the block device for an EBS volume.
launchTemplateBlockDeviceMapping_ebs :: Lens.Lens' LaunchTemplateBlockDeviceMapping (Prelude.Maybe LaunchTemplateEbsBlockDevice)
launchTemplateBlockDeviceMapping_ebs = Lens.lens (\LaunchTemplateBlockDeviceMapping' {ebs} -> ebs) (\s@LaunchTemplateBlockDeviceMapping' {} a -> s {ebs = a} :: LaunchTemplateBlockDeviceMapping)

-- | To omit the device from the block device mapping, specify an empty
-- string.
launchTemplateBlockDeviceMapping_noDevice :: Lens.Lens' LaunchTemplateBlockDeviceMapping (Prelude.Maybe Prelude.Text)
launchTemplateBlockDeviceMapping_noDevice = Lens.lens (\LaunchTemplateBlockDeviceMapping' {noDevice} -> noDevice) (\s@LaunchTemplateBlockDeviceMapping' {} a -> s {noDevice = a} :: LaunchTemplateBlockDeviceMapping)

-- | The virtual device name (ephemeralN).
launchTemplateBlockDeviceMapping_virtualName :: Lens.Lens' LaunchTemplateBlockDeviceMapping (Prelude.Maybe Prelude.Text)
launchTemplateBlockDeviceMapping_virtualName = Lens.lens (\LaunchTemplateBlockDeviceMapping' {virtualName} -> virtualName) (\s@LaunchTemplateBlockDeviceMapping' {} a -> s {virtualName = a} :: LaunchTemplateBlockDeviceMapping)

instance
  Data.FromXML
    LaunchTemplateBlockDeviceMapping
  where
  parseXML x =
    LaunchTemplateBlockDeviceMapping'
      Prelude.<$> (x Data..@? "deviceName")
      Prelude.<*> (x Data..@? "ebs")
      Prelude.<*> (x Data..@? "noDevice")
      Prelude.<*> (x Data..@? "virtualName")

instance
  Prelude.Hashable
    LaunchTemplateBlockDeviceMapping
  where
  hashWithSalt
    _salt
    LaunchTemplateBlockDeviceMapping' {..} =
      _salt `Prelude.hashWithSalt` deviceName
        `Prelude.hashWithSalt` ebs
        `Prelude.hashWithSalt` noDevice
        `Prelude.hashWithSalt` virtualName

instance
  Prelude.NFData
    LaunchTemplateBlockDeviceMapping
  where
  rnf LaunchTemplateBlockDeviceMapping' {..} =
    Prelude.rnf deviceName
      `Prelude.seq` Prelude.rnf ebs
      `Prelude.seq` Prelude.rnf noDevice
      `Prelude.seq` Prelude.rnf virtualName
