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
-- Module      : Amazonka.EC2.Types.LaunchTemplateBlockDeviceMappingRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateBlockDeviceMappingRequest where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.LaunchTemplateEbsBlockDeviceRequest
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a block device mapping.
--
-- /See:/ 'newLaunchTemplateBlockDeviceMappingRequest' smart constructor.
data LaunchTemplateBlockDeviceMappingRequest = LaunchTemplateBlockDeviceMappingRequest'
  { -- | The virtual device name (ephemeralN). Instance store volumes are
    -- numbered starting from 0. An instance type with 2 available instance
    -- store volumes can specify mappings for ephemeral0 and ephemeral1. The
    -- number of available instance store volumes depends on the instance type.
    -- After you connect to the instance, you must mount the volume.
    virtualName :: Prelude.Maybe Prelude.Text,
    -- | To omit the device from the block device mapping, specify an empty
    -- string.
    noDevice :: Prelude.Maybe Prelude.Text,
    -- | Parameters used to automatically set up EBS volumes when the instance is
    -- launched.
    ebs :: Prelude.Maybe LaunchTemplateEbsBlockDeviceRequest,
    -- | The device name (for example, \/dev\/sdh or xvdh).
    deviceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateBlockDeviceMappingRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualName', 'launchTemplateBlockDeviceMappingRequest_virtualName' - The virtual device name (ephemeralN). Instance store volumes are
-- numbered starting from 0. An instance type with 2 available instance
-- store volumes can specify mappings for ephemeral0 and ephemeral1. The
-- number of available instance store volumes depends on the instance type.
-- After you connect to the instance, you must mount the volume.
--
-- 'noDevice', 'launchTemplateBlockDeviceMappingRequest_noDevice' - To omit the device from the block device mapping, specify an empty
-- string.
--
-- 'ebs', 'launchTemplateBlockDeviceMappingRequest_ebs' - Parameters used to automatically set up EBS volumes when the instance is
-- launched.
--
-- 'deviceName', 'launchTemplateBlockDeviceMappingRequest_deviceName' - The device name (for example, \/dev\/sdh or xvdh).
newLaunchTemplateBlockDeviceMappingRequest ::
  LaunchTemplateBlockDeviceMappingRequest
newLaunchTemplateBlockDeviceMappingRequest =
  LaunchTemplateBlockDeviceMappingRequest'
    { virtualName =
        Prelude.Nothing,
      noDevice = Prelude.Nothing,
      ebs = Prelude.Nothing,
      deviceName = Prelude.Nothing
    }

-- | The virtual device name (ephemeralN). Instance store volumes are
-- numbered starting from 0. An instance type with 2 available instance
-- store volumes can specify mappings for ephemeral0 and ephemeral1. The
-- number of available instance store volumes depends on the instance type.
-- After you connect to the instance, you must mount the volume.
launchTemplateBlockDeviceMappingRequest_virtualName :: Lens.Lens' LaunchTemplateBlockDeviceMappingRequest (Prelude.Maybe Prelude.Text)
launchTemplateBlockDeviceMappingRequest_virtualName = Lens.lens (\LaunchTemplateBlockDeviceMappingRequest' {virtualName} -> virtualName) (\s@LaunchTemplateBlockDeviceMappingRequest' {} a -> s {virtualName = a} :: LaunchTemplateBlockDeviceMappingRequest)

-- | To omit the device from the block device mapping, specify an empty
-- string.
launchTemplateBlockDeviceMappingRequest_noDevice :: Lens.Lens' LaunchTemplateBlockDeviceMappingRequest (Prelude.Maybe Prelude.Text)
launchTemplateBlockDeviceMappingRequest_noDevice = Lens.lens (\LaunchTemplateBlockDeviceMappingRequest' {noDevice} -> noDevice) (\s@LaunchTemplateBlockDeviceMappingRequest' {} a -> s {noDevice = a} :: LaunchTemplateBlockDeviceMappingRequest)

-- | Parameters used to automatically set up EBS volumes when the instance is
-- launched.
launchTemplateBlockDeviceMappingRequest_ebs :: Lens.Lens' LaunchTemplateBlockDeviceMappingRequest (Prelude.Maybe LaunchTemplateEbsBlockDeviceRequest)
launchTemplateBlockDeviceMappingRequest_ebs = Lens.lens (\LaunchTemplateBlockDeviceMappingRequest' {ebs} -> ebs) (\s@LaunchTemplateBlockDeviceMappingRequest' {} a -> s {ebs = a} :: LaunchTemplateBlockDeviceMappingRequest)

-- | The device name (for example, \/dev\/sdh or xvdh).
launchTemplateBlockDeviceMappingRequest_deviceName :: Lens.Lens' LaunchTemplateBlockDeviceMappingRequest (Prelude.Maybe Prelude.Text)
launchTemplateBlockDeviceMappingRequest_deviceName = Lens.lens (\LaunchTemplateBlockDeviceMappingRequest' {deviceName} -> deviceName) (\s@LaunchTemplateBlockDeviceMappingRequest' {} a -> s {deviceName = a} :: LaunchTemplateBlockDeviceMappingRequest)

instance
  Prelude.Hashable
    LaunchTemplateBlockDeviceMappingRequest
  where
  hashWithSalt
    _salt
    LaunchTemplateBlockDeviceMappingRequest' {..} =
      _salt `Prelude.hashWithSalt` virtualName
        `Prelude.hashWithSalt` noDevice
        `Prelude.hashWithSalt` ebs
        `Prelude.hashWithSalt` deviceName

instance
  Prelude.NFData
    LaunchTemplateBlockDeviceMappingRequest
  where
  rnf LaunchTemplateBlockDeviceMappingRequest' {..} =
    Prelude.rnf virtualName
      `Prelude.seq` Prelude.rnf noDevice
      `Prelude.seq` Prelude.rnf ebs
      `Prelude.seq` Prelude.rnf deviceName

instance
  Core.ToQuery
    LaunchTemplateBlockDeviceMappingRequest
  where
  toQuery LaunchTemplateBlockDeviceMappingRequest' {..} =
    Prelude.mconcat
      [ "VirtualName" Core.=: virtualName,
        "NoDevice" Core.=: noDevice,
        "Ebs" Core.=: ebs,
        "DeviceName" Core.=: deviceName
      ]
