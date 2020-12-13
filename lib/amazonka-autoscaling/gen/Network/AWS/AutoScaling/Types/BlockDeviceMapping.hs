{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.BlockDeviceMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.BlockDeviceMapping
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

import Network.AWS.AutoScaling.Types.EBS
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a block device mapping.
--
-- /See:/ 'mkBlockDeviceMapping' smart constructor.
data BlockDeviceMapping = BlockDeviceMapping'
  { -- | The name of the virtual device (for example, @ephemeral0@ ).
    --
    -- You can specify either @VirtualName@ or @Ebs@ , but not both.
    virtualName :: Lude.Maybe Lude.Text,
    -- | Setting this value to @true@ suppresses the specified device included in the block device mapping of the AMI.
    --
    -- If @NoDevice@ is @true@ for the root device, instances might fail the EC2 health check. In that case, Amazon EC2 Auto Scaling launches replacement instances.
    -- If you specify @NoDevice@ , you cannot specify @Ebs@ .
    noDevice :: Lude.Maybe Lude.Bool,
    -- | Parameters used to automatically set up EBS volumes when an instance is launched.
    --
    -- You can specify either @VirtualName@ or @Ebs@ , but not both.
    ebs :: Lude.Maybe EBS,
    -- | The device name exposed to the EC2 instance (for example, @/dev/sdh@ or @xvdh@ ). For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/device_naming.html Device Naming on Linux Instances> in the /Amazon EC2 User Guide for Linux Instances/ .
    deviceName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BlockDeviceMapping' with the minimum fields required to make a request.
--
-- * 'virtualName' - The name of the virtual device (for example, @ephemeral0@ ).
--
-- You can specify either @VirtualName@ or @Ebs@ , but not both.
-- * 'noDevice' - Setting this value to @true@ suppresses the specified device included in the block device mapping of the AMI.
--
-- If @NoDevice@ is @true@ for the root device, instances might fail the EC2 health check. In that case, Amazon EC2 Auto Scaling launches replacement instances.
-- If you specify @NoDevice@ , you cannot specify @Ebs@ .
-- * 'ebs' - Parameters used to automatically set up EBS volumes when an instance is launched.
--
-- You can specify either @VirtualName@ or @Ebs@ , but not both.
-- * 'deviceName' - The device name exposed to the EC2 instance (for example, @/dev/sdh@ or @xvdh@ ). For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/device_naming.html Device Naming on Linux Instances> in the /Amazon EC2 User Guide for Linux Instances/ .
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

-- | The name of the virtual device (for example, @ephemeral0@ ).
--
-- You can specify either @VirtualName@ or @Ebs@ , but not both.
--
-- /Note:/ Consider using 'virtualName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmVirtualName :: Lens.Lens' BlockDeviceMapping (Lude.Maybe Lude.Text)
bdmVirtualName = Lens.lens (virtualName :: BlockDeviceMapping -> Lude.Maybe Lude.Text) (\s a -> s {virtualName = a} :: BlockDeviceMapping)
{-# DEPRECATED bdmVirtualName "Use generic-lens or generic-optics with 'virtualName' instead." #-}

-- | Setting this value to @true@ suppresses the specified device included in the block device mapping of the AMI.
--
-- If @NoDevice@ is @true@ for the root device, instances might fail the EC2 health check. In that case, Amazon EC2 Auto Scaling launches replacement instances.
-- If you specify @NoDevice@ , you cannot specify @Ebs@ .
--
-- /Note:/ Consider using 'noDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmNoDevice :: Lens.Lens' BlockDeviceMapping (Lude.Maybe Lude.Bool)
bdmNoDevice = Lens.lens (noDevice :: BlockDeviceMapping -> Lude.Maybe Lude.Bool) (\s a -> s {noDevice = a} :: BlockDeviceMapping)
{-# DEPRECATED bdmNoDevice "Use generic-lens or generic-optics with 'noDevice' instead." #-}

-- | Parameters used to automatically set up EBS volumes when an instance is launched.
--
-- You can specify either @VirtualName@ or @Ebs@ , but not both.
--
-- /Note:/ Consider using 'ebs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmEBS :: Lens.Lens' BlockDeviceMapping (Lude.Maybe EBS)
bdmEBS = Lens.lens (ebs :: BlockDeviceMapping -> Lude.Maybe EBS) (\s a -> s {ebs = a} :: BlockDeviceMapping)
{-# DEPRECATED bdmEBS "Use generic-lens or generic-optics with 'ebs' instead." #-}

-- | The device name exposed to the EC2 instance (for example, @/dev/sdh@ or @xvdh@ ). For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/device_naming.html Device Naming on Linux Instances> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmDeviceName :: Lens.Lens' BlockDeviceMapping Lude.Text
bdmDeviceName = Lens.lens (deviceName :: BlockDeviceMapping -> Lude.Text) (\s a -> s {deviceName = a} :: BlockDeviceMapping)
{-# DEPRECATED bdmDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

instance Lude.FromXML BlockDeviceMapping where
  parseXML x =
    BlockDeviceMapping'
      Lude.<$> (x Lude..@? "VirtualName")
      Lude.<*> (x Lude..@? "NoDevice")
      Lude.<*> (x Lude..@? "Ebs")
      Lude.<*> (x Lude..@ "DeviceName")

instance Lude.ToQuery BlockDeviceMapping where
  toQuery BlockDeviceMapping' {..} =
    Lude.mconcat
      [ "VirtualName" Lude.=: virtualName,
        "NoDevice" Lude.=: noDevice,
        "Ebs" Lude.=: ebs,
        "DeviceName" Lude.=: deviceName
      ]
