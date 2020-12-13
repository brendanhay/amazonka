{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateBlockDeviceMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateBlockDeviceMapping
  ( LaunchTemplateBlockDeviceMapping (..),

    -- * Smart constructor
    mkLaunchTemplateBlockDeviceMapping,

    -- * Lenses
    ltbdmVirtualName,
    ltbdmNoDevice,
    ltbdmEBS,
    ltbdmDeviceName,
  )
where

import Network.AWS.EC2.Types.LaunchTemplateEBSBlockDevice
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a block device mapping.
--
-- /See:/ 'mkLaunchTemplateBlockDeviceMapping' smart constructor.
data LaunchTemplateBlockDeviceMapping = LaunchTemplateBlockDeviceMapping'
  { -- | The virtual device name (ephemeralN).
    virtualName :: Lude.Maybe Lude.Text,
    -- | Suppresses the specified device included in the block device mapping of the AMI.
    noDevice :: Lude.Maybe Lude.Text,
    -- | Information about the block device for an EBS volume.
    ebs :: Lude.Maybe LaunchTemplateEBSBlockDevice,
    -- | The device name.
    deviceName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateBlockDeviceMapping' with the minimum fields required to make a request.
--
-- * 'virtualName' - The virtual device name (ephemeralN).
-- * 'noDevice' - Suppresses the specified device included in the block device mapping of the AMI.
-- * 'ebs' - Information about the block device for an EBS volume.
-- * 'deviceName' - The device name.
mkLaunchTemplateBlockDeviceMapping ::
  LaunchTemplateBlockDeviceMapping
mkLaunchTemplateBlockDeviceMapping =
  LaunchTemplateBlockDeviceMapping'
    { virtualName = Lude.Nothing,
      noDevice = Lude.Nothing,
      ebs = Lude.Nothing,
      deviceName = Lude.Nothing
    }

-- | The virtual device name (ephemeralN).
--
-- /Note:/ Consider using 'virtualName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbdmVirtualName :: Lens.Lens' LaunchTemplateBlockDeviceMapping (Lude.Maybe Lude.Text)
ltbdmVirtualName = Lens.lens (virtualName :: LaunchTemplateBlockDeviceMapping -> Lude.Maybe Lude.Text) (\s a -> s {virtualName = a} :: LaunchTemplateBlockDeviceMapping)
{-# DEPRECATED ltbdmVirtualName "Use generic-lens or generic-optics with 'virtualName' instead." #-}

-- | Suppresses the specified device included in the block device mapping of the AMI.
--
-- /Note:/ Consider using 'noDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbdmNoDevice :: Lens.Lens' LaunchTemplateBlockDeviceMapping (Lude.Maybe Lude.Text)
ltbdmNoDevice = Lens.lens (noDevice :: LaunchTemplateBlockDeviceMapping -> Lude.Maybe Lude.Text) (\s a -> s {noDevice = a} :: LaunchTemplateBlockDeviceMapping)
{-# DEPRECATED ltbdmNoDevice "Use generic-lens or generic-optics with 'noDevice' instead." #-}

-- | Information about the block device for an EBS volume.
--
-- /Note:/ Consider using 'ebs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbdmEBS :: Lens.Lens' LaunchTemplateBlockDeviceMapping (Lude.Maybe LaunchTemplateEBSBlockDevice)
ltbdmEBS = Lens.lens (ebs :: LaunchTemplateBlockDeviceMapping -> Lude.Maybe LaunchTemplateEBSBlockDevice) (\s a -> s {ebs = a} :: LaunchTemplateBlockDeviceMapping)
{-# DEPRECATED ltbdmEBS "Use generic-lens or generic-optics with 'ebs' instead." #-}

-- | The device name.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbdmDeviceName :: Lens.Lens' LaunchTemplateBlockDeviceMapping (Lude.Maybe Lude.Text)
ltbdmDeviceName = Lens.lens (deviceName :: LaunchTemplateBlockDeviceMapping -> Lude.Maybe Lude.Text) (\s a -> s {deviceName = a} :: LaunchTemplateBlockDeviceMapping)
{-# DEPRECATED ltbdmDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

instance Lude.FromXML LaunchTemplateBlockDeviceMapping where
  parseXML x =
    LaunchTemplateBlockDeviceMapping'
      Lude.<$> (x Lude..@? "virtualName")
      Lude.<*> (x Lude..@? "noDevice")
      Lude.<*> (x Lude..@? "ebs")
      Lude.<*> (x Lude..@? "deviceName")
