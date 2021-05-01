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
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesBlockDeviceMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstancesBlockDeviceMapping where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ScheduledInstancesEbs
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a block device mapping for a Scheduled Instance.
--
-- /See:/ 'newScheduledInstancesBlockDeviceMapping' smart constructor.
data ScheduledInstancesBlockDeviceMapping = ScheduledInstancesBlockDeviceMapping'
  { -- | Parameters used to set up EBS volumes automatically when the instance is
    -- launched.
    ebs :: Prelude.Maybe ScheduledInstancesEbs,
    -- | To omit the device from the block device mapping, specify an empty
    -- string.
    noDevice :: Prelude.Maybe Prelude.Text,
    -- | The virtual device name (@ephemeral@N). Instance store volumes are
    -- numbered starting from 0. An instance type with two available instance
    -- store volumes can specify mappings for @ephemeral0@ and @ephemeral1@.
    -- The number of available instance store volumes depends on the instance
    -- type. After you connect to the instance, you must mount the volume.
    --
    -- Constraints: For M3 instances, you must specify instance store volumes
    -- in the block device mapping for the instance. When you launch an M3
    -- instance, we ignore any instance store volumes specified in the block
    -- device mapping for the AMI.
    virtualName :: Prelude.Maybe Prelude.Text,
    -- | The device name (for example, @\/dev\/sdh@ or @xvdh@).
    deviceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ScheduledInstancesBlockDeviceMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebs', 'scheduledInstancesBlockDeviceMapping_ebs' - Parameters used to set up EBS volumes automatically when the instance is
-- launched.
--
-- 'noDevice', 'scheduledInstancesBlockDeviceMapping_noDevice' - To omit the device from the block device mapping, specify an empty
-- string.
--
-- 'virtualName', 'scheduledInstancesBlockDeviceMapping_virtualName' - The virtual device name (@ephemeral@N). Instance store volumes are
-- numbered starting from 0. An instance type with two available instance
-- store volumes can specify mappings for @ephemeral0@ and @ephemeral1@.
-- The number of available instance store volumes depends on the instance
-- type. After you connect to the instance, you must mount the volume.
--
-- Constraints: For M3 instances, you must specify instance store volumes
-- in the block device mapping for the instance. When you launch an M3
-- instance, we ignore any instance store volumes specified in the block
-- device mapping for the AMI.
--
-- 'deviceName', 'scheduledInstancesBlockDeviceMapping_deviceName' - The device name (for example, @\/dev\/sdh@ or @xvdh@).
newScheduledInstancesBlockDeviceMapping ::
  ScheduledInstancesBlockDeviceMapping
newScheduledInstancesBlockDeviceMapping =
  ScheduledInstancesBlockDeviceMapping'
    { ebs =
        Prelude.Nothing,
      noDevice = Prelude.Nothing,
      virtualName = Prelude.Nothing,
      deviceName = Prelude.Nothing
    }

-- | Parameters used to set up EBS volumes automatically when the instance is
-- launched.
scheduledInstancesBlockDeviceMapping_ebs :: Lens.Lens' ScheduledInstancesBlockDeviceMapping (Prelude.Maybe ScheduledInstancesEbs)
scheduledInstancesBlockDeviceMapping_ebs = Lens.lens (\ScheduledInstancesBlockDeviceMapping' {ebs} -> ebs) (\s@ScheduledInstancesBlockDeviceMapping' {} a -> s {ebs = a} :: ScheduledInstancesBlockDeviceMapping)

-- | To omit the device from the block device mapping, specify an empty
-- string.
scheduledInstancesBlockDeviceMapping_noDevice :: Lens.Lens' ScheduledInstancesBlockDeviceMapping (Prelude.Maybe Prelude.Text)
scheduledInstancesBlockDeviceMapping_noDevice = Lens.lens (\ScheduledInstancesBlockDeviceMapping' {noDevice} -> noDevice) (\s@ScheduledInstancesBlockDeviceMapping' {} a -> s {noDevice = a} :: ScheduledInstancesBlockDeviceMapping)

-- | The virtual device name (@ephemeral@N). Instance store volumes are
-- numbered starting from 0. An instance type with two available instance
-- store volumes can specify mappings for @ephemeral0@ and @ephemeral1@.
-- The number of available instance store volumes depends on the instance
-- type. After you connect to the instance, you must mount the volume.
--
-- Constraints: For M3 instances, you must specify instance store volumes
-- in the block device mapping for the instance. When you launch an M3
-- instance, we ignore any instance store volumes specified in the block
-- device mapping for the AMI.
scheduledInstancesBlockDeviceMapping_virtualName :: Lens.Lens' ScheduledInstancesBlockDeviceMapping (Prelude.Maybe Prelude.Text)
scheduledInstancesBlockDeviceMapping_virtualName = Lens.lens (\ScheduledInstancesBlockDeviceMapping' {virtualName} -> virtualName) (\s@ScheduledInstancesBlockDeviceMapping' {} a -> s {virtualName = a} :: ScheduledInstancesBlockDeviceMapping)

-- | The device name (for example, @\/dev\/sdh@ or @xvdh@).
scheduledInstancesBlockDeviceMapping_deviceName :: Lens.Lens' ScheduledInstancesBlockDeviceMapping (Prelude.Maybe Prelude.Text)
scheduledInstancesBlockDeviceMapping_deviceName = Lens.lens (\ScheduledInstancesBlockDeviceMapping' {deviceName} -> deviceName) (\s@ScheduledInstancesBlockDeviceMapping' {} a -> s {deviceName = a} :: ScheduledInstancesBlockDeviceMapping)

instance
  Prelude.Hashable
    ScheduledInstancesBlockDeviceMapping

instance
  Prelude.NFData
    ScheduledInstancesBlockDeviceMapping

instance
  Prelude.ToQuery
    ScheduledInstancesBlockDeviceMapping
  where
  toQuery ScheduledInstancesBlockDeviceMapping' {..} =
    Prelude.mconcat
      [ "Ebs" Prelude.=: ebs,
        "NoDevice" Prelude.=: noDevice,
        "VirtualName" Prelude.=: virtualName,
        "DeviceName" Prelude.=: deviceName
      ]
