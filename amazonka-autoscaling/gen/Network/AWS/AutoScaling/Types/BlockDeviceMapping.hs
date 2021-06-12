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
-- Module      : Network.AWS.AutoScaling.Types.BlockDeviceMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.BlockDeviceMapping where

import Network.AWS.AutoScaling.Types.Ebs
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a block device mapping.
--
-- /See:/ 'newBlockDeviceMapping' smart constructor.
data BlockDeviceMapping = BlockDeviceMapping'
  { -- | Parameters used to automatically set up EBS volumes when an instance is
    -- launched.
    --
    -- You can specify either @VirtualName@ or @Ebs@, but not both.
    ebs :: Core.Maybe Ebs,
    -- | Setting this value to @true@ suppresses the specified device included in
    -- the block device mapping of the AMI.
    --
    -- If @NoDevice@ is @true@ for the root device, instances might fail the
    -- EC2 health check. In that case, Amazon EC2 Auto Scaling launches
    -- replacement instances.
    --
    -- If you specify @NoDevice@, you cannot specify @Ebs@.
    noDevice :: Core.Maybe Core.Bool,
    -- | The name of the virtual device (for example, @ephemeral0@).
    --
    -- You can specify either @VirtualName@ or @Ebs@, but not both.
    virtualName :: Core.Maybe Core.Text,
    -- | The device name exposed to the EC2 instance (for example, @\/dev\/sdh@
    -- or @xvdh@). For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/device_naming.html Device Naming on Linux Instances>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    deviceName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BlockDeviceMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebs', 'blockDeviceMapping_ebs' - Parameters used to automatically set up EBS volumes when an instance is
-- launched.
--
-- You can specify either @VirtualName@ or @Ebs@, but not both.
--
-- 'noDevice', 'blockDeviceMapping_noDevice' - Setting this value to @true@ suppresses the specified device included in
-- the block device mapping of the AMI.
--
-- If @NoDevice@ is @true@ for the root device, instances might fail the
-- EC2 health check. In that case, Amazon EC2 Auto Scaling launches
-- replacement instances.
--
-- If you specify @NoDevice@, you cannot specify @Ebs@.
--
-- 'virtualName', 'blockDeviceMapping_virtualName' - The name of the virtual device (for example, @ephemeral0@).
--
-- You can specify either @VirtualName@ or @Ebs@, but not both.
--
-- 'deviceName', 'blockDeviceMapping_deviceName' - The device name exposed to the EC2 instance (for example, @\/dev\/sdh@
-- or @xvdh@). For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/device_naming.html Device Naming on Linux Instances>
-- in the /Amazon EC2 User Guide for Linux Instances/.
newBlockDeviceMapping ::
  -- | 'deviceName'
  Core.Text ->
  BlockDeviceMapping
newBlockDeviceMapping pDeviceName_ =
  BlockDeviceMapping'
    { ebs = Core.Nothing,
      noDevice = Core.Nothing,
      virtualName = Core.Nothing,
      deviceName = pDeviceName_
    }

-- | Parameters used to automatically set up EBS volumes when an instance is
-- launched.
--
-- You can specify either @VirtualName@ or @Ebs@, but not both.
blockDeviceMapping_ebs :: Lens.Lens' BlockDeviceMapping (Core.Maybe Ebs)
blockDeviceMapping_ebs = Lens.lens (\BlockDeviceMapping' {ebs} -> ebs) (\s@BlockDeviceMapping' {} a -> s {ebs = a} :: BlockDeviceMapping)

-- | Setting this value to @true@ suppresses the specified device included in
-- the block device mapping of the AMI.
--
-- If @NoDevice@ is @true@ for the root device, instances might fail the
-- EC2 health check. In that case, Amazon EC2 Auto Scaling launches
-- replacement instances.
--
-- If you specify @NoDevice@, you cannot specify @Ebs@.
blockDeviceMapping_noDevice :: Lens.Lens' BlockDeviceMapping (Core.Maybe Core.Bool)
blockDeviceMapping_noDevice = Lens.lens (\BlockDeviceMapping' {noDevice} -> noDevice) (\s@BlockDeviceMapping' {} a -> s {noDevice = a} :: BlockDeviceMapping)

-- | The name of the virtual device (for example, @ephemeral0@).
--
-- You can specify either @VirtualName@ or @Ebs@, but not both.
blockDeviceMapping_virtualName :: Lens.Lens' BlockDeviceMapping (Core.Maybe Core.Text)
blockDeviceMapping_virtualName = Lens.lens (\BlockDeviceMapping' {virtualName} -> virtualName) (\s@BlockDeviceMapping' {} a -> s {virtualName = a} :: BlockDeviceMapping)

-- | The device name exposed to the EC2 instance (for example, @\/dev\/sdh@
-- or @xvdh@). For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/device_naming.html Device Naming on Linux Instances>
-- in the /Amazon EC2 User Guide for Linux Instances/.
blockDeviceMapping_deviceName :: Lens.Lens' BlockDeviceMapping Core.Text
blockDeviceMapping_deviceName = Lens.lens (\BlockDeviceMapping' {deviceName} -> deviceName) (\s@BlockDeviceMapping' {} a -> s {deviceName = a} :: BlockDeviceMapping)

instance Core.FromXML BlockDeviceMapping where
  parseXML x =
    BlockDeviceMapping'
      Core.<$> (x Core..@? "Ebs")
      Core.<*> (x Core..@? "NoDevice")
      Core.<*> (x Core..@? "VirtualName")
      Core.<*> (x Core..@ "DeviceName")

instance Core.Hashable BlockDeviceMapping

instance Core.NFData BlockDeviceMapping

instance Core.ToQuery BlockDeviceMapping where
  toQuery BlockDeviceMapping' {..} =
    Core.mconcat
      [ "Ebs" Core.=: ebs,
        "NoDevice" Core.=: noDevice,
        "VirtualName" Core.=: virtualName,
        "DeviceName" Core.=: deviceName
      ]
