{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.BlockDeviceMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.BlockDeviceMapping
  ( BlockDeviceMapping (..),

    -- * Smart constructor
    mkBlockDeviceMapping,

    -- * Lenses
    bdmVirtualName,
    bdmNoDevice,
    bdmEBS,
    bdmDeviceName,
  )
where

import Network.AWS.EC2.Types.EBSBlockDevice
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a block device mapping.
--
-- /See:/ 'mkBlockDeviceMapping' smart constructor.
data BlockDeviceMapping = BlockDeviceMapping'
  { -- | The virtual device name (@ephemeral@ N). Instance store volumes are numbered starting from 0. An instance type with 2 available instance store volumes can specify mappings for @ephemeral0@ and @ephemeral1@ . The number of available instance store volumes depends on the instance type. After you connect to the instance, you must mount the volume.
    --
    -- NVMe instance store volumes are automatically enumerated and assigned a device name. Including them in your block device mapping has no effect.
    -- Constraints: For M3 instances, you must specify instance store volumes in the block device mapping for the instance. When you launch an M3 instance, we ignore any instance store volumes specified in the block device mapping for the AMI.
    virtualName :: Lude.Maybe Lude.Text,
    -- | Suppresses the specified device included in the block device mapping of the AMI.
    noDevice :: Lude.Maybe Lude.Text,
    -- | Parameters used to automatically set up EBS volumes when the instance is launched.
    ebs :: Lude.Maybe EBSBlockDevice,
    -- | The device name (for example, @/dev/sdh@ or @xvdh@ ).
    deviceName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BlockDeviceMapping' with the minimum fields required to make a request.
--
-- * 'virtualName' - The virtual device name (@ephemeral@ N). Instance store volumes are numbered starting from 0. An instance type with 2 available instance store volumes can specify mappings for @ephemeral0@ and @ephemeral1@ . The number of available instance store volumes depends on the instance type. After you connect to the instance, you must mount the volume.
--
-- NVMe instance store volumes are automatically enumerated and assigned a device name. Including them in your block device mapping has no effect.
-- Constraints: For M3 instances, you must specify instance store volumes in the block device mapping for the instance. When you launch an M3 instance, we ignore any instance store volumes specified in the block device mapping for the AMI.
-- * 'noDevice' - Suppresses the specified device included in the block device mapping of the AMI.
-- * 'ebs' - Parameters used to automatically set up EBS volumes when the instance is launched.
-- * 'deviceName' - The device name (for example, @/dev/sdh@ or @xvdh@ ).
mkBlockDeviceMapping ::
  -- | 'deviceName'
  Lude.Text ->
  BlockDeviceMapping
mkBlockDeviceMapping pDeviceName_ =
  BlockDeviceMapping'
    { virtualName = Lude.Nothing,
      noDevice = Lude.Nothing,
      ebs = Lude.Nothing,
      deviceName = pDeviceName_
    }

-- | The virtual device name (@ephemeral@ N). Instance store volumes are numbered starting from 0. An instance type with 2 available instance store volumes can specify mappings for @ephemeral0@ and @ephemeral1@ . The number of available instance store volumes depends on the instance type. After you connect to the instance, you must mount the volume.
--
-- NVMe instance store volumes are automatically enumerated and assigned a device name. Including them in your block device mapping has no effect.
-- Constraints: For M3 instances, you must specify instance store volumes in the block device mapping for the instance. When you launch an M3 instance, we ignore any instance store volumes specified in the block device mapping for the AMI.
--
-- /Note:/ Consider using 'virtualName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmVirtualName :: Lens.Lens' BlockDeviceMapping (Lude.Maybe Lude.Text)
bdmVirtualName = Lens.lens (virtualName :: BlockDeviceMapping -> Lude.Maybe Lude.Text) (\s a -> s {virtualName = a} :: BlockDeviceMapping)
{-# DEPRECATED bdmVirtualName "Use generic-lens or generic-optics with 'virtualName' instead." #-}

-- | Suppresses the specified device included in the block device mapping of the AMI.
--
-- /Note:/ Consider using 'noDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmNoDevice :: Lens.Lens' BlockDeviceMapping (Lude.Maybe Lude.Text)
bdmNoDevice = Lens.lens (noDevice :: BlockDeviceMapping -> Lude.Maybe Lude.Text) (\s a -> s {noDevice = a} :: BlockDeviceMapping)
{-# DEPRECATED bdmNoDevice "Use generic-lens or generic-optics with 'noDevice' instead." #-}

-- | Parameters used to automatically set up EBS volumes when the instance is launched.
--
-- /Note:/ Consider using 'ebs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmEBS :: Lens.Lens' BlockDeviceMapping (Lude.Maybe EBSBlockDevice)
bdmEBS = Lens.lens (ebs :: BlockDeviceMapping -> Lude.Maybe EBSBlockDevice) (\s a -> s {ebs = a} :: BlockDeviceMapping)
{-# DEPRECATED bdmEBS "Use generic-lens or generic-optics with 'ebs' instead." #-}

-- | The device name (for example, @/dev/sdh@ or @xvdh@ ).
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmDeviceName :: Lens.Lens' BlockDeviceMapping Lude.Text
bdmDeviceName = Lens.lens (deviceName :: BlockDeviceMapping -> Lude.Text) (\s a -> s {deviceName = a} :: BlockDeviceMapping)
{-# DEPRECATED bdmDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

instance Lude.FromXML BlockDeviceMapping where
  parseXML x =
    BlockDeviceMapping'
      Lude.<$> (x Lude..@? "virtualName")
      Lude.<*> (x Lude..@? "noDevice")
      Lude.<*> (x Lude..@? "ebs")
      Lude.<*> (x Lude..@ "deviceName")

instance Lude.ToQuery BlockDeviceMapping where
  toQuery BlockDeviceMapping' {..} =
    Lude.mconcat
      [ "VirtualName" Lude.=: virtualName,
        "NoDevice" Lude.=: noDevice,
        "Ebs" Lude.=: ebs,
        "DeviceName" Lude.=: deviceName
      ]
