-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesBlockDeviceMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstancesBlockDeviceMapping
  ( ScheduledInstancesBlockDeviceMapping (..),

    -- * Smart constructor
    mkScheduledInstancesBlockDeviceMapping,

    -- * Lenses
    sibdmVirtualName,
    sibdmNoDevice,
    sibdmEBS,
    sibdmDeviceName,
  )
where

import Network.AWS.EC2.Types.ScheduledInstancesEBS
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a block device mapping for a Scheduled Instance.
--
-- /See:/ 'mkScheduledInstancesBlockDeviceMapping' smart constructor.
data ScheduledInstancesBlockDeviceMapping = ScheduledInstancesBlockDeviceMapping'
  { virtualName ::
      Lude.Maybe
        Lude.Text,
    noDevice ::
      Lude.Maybe
        Lude.Text,
    ebs ::
      Lude.Maybe
        ScheduledInstancesEBS,
    deviceName ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScheduledInstancesBlockDeviceMapping' with the minimum fields required to make a request.
--
-- * 'deviceName' - The device name (for example, @/dev/sdh@ or @xvdh@ ).
-- * 'ebs' - Parameters used to set up EBS volumes automatically when the instance is launched.
-- * 'noDevice' - Suppresses the specified device included in the block device mapping of the AMI.
-- * 'virtualName' - The virtual device name (@ephemeral@ N). Instance store volumes are numbered starting from 0. An instance type with two available instance store volumes can specify mappings for @ephemeral0@ and @ephemeral1@ . The number of available instance store volumes depends on the instance type. After you connect to the instance, you must mount the volume.
--
-- Constraints: For M3 instances, you must specify instance store volumes in the block device mapping for the instance. When you launch an M3 instance, we ignore any instance store volumes specified in the block device mapping for the AMI.
mkScheduledInstancesBlockDeviceMapping ::
  ScheduledInstancesBlockDeviceMapping
mkScheduledInstancesBlockDeviceMapping =
  ScheduledInstancesBlockDeviceMapping'
    { virtualName = Lude.Nothing,
      noDevice = Lude.Nothing,
      ebs = Lude.Nothing,
      deviceName = Lude.Nothing
    }

-- | The virtual device name (@ephemeral@ N). Instance store volumes are numbered starting from 0. An instance type with two available instance store volumes can specify mappings for @ephemeral0@ and @ephemeral1@ . The number of available instance store volumes depends on the instance type. After you connect to the instance, you must mount the volume.
--
-- Constraints: For M3 instances, you must specify instance store volumes in the block device mapping for the instance. When you launch an M3 instance, we ignore any instance store volumes specified in the block device mapping for the AMI.
--
-- /Note:/ Consider using 'virtualName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sibdmVirtualName :: Lens.Lens' ScheduledInstancesBlockDeviceMapping (Lude.Maybe Lude.Text)
sibdmVirtualName = Lens.lens (virtualName :: ScheduledInstancesBlockDeviceMapping -> Lude.Maybe Lude.Text) (\s a -> s {virtualName = a} :: ScheduledInstancesBlockDeviceMapping)
{-# DEPRECATED sibdmVirtualName "Use generic-lens or generic-optics with 'virtualName' instead." #-}

-- | Suppresses the specified device included in the block device mapping of the AMI.
--
-- /Note:/ Consider using 'noDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sibdmNoDevice :: Lens.Lens' ScheduledInstancesBlockDeviceMapping (Lude.Maybe Lude.Text)
sibdmNoDevice = Lens.lens (noDevice :: ScheduledInstancesBlockDeviceMapping -> Lude.Maybe Lude.Text) (\s a -> s {noDevice = a} :: ScheduledInstancesBlockDeviceMapping)
{-# DEPRECATED sibdmNoDevice "Use generic-lens or generic-optics with 'noDevice' instead." #-}

-- | Parameters used to set up EBS volumes automatically when the instance is launched.
--
-- /Note:/ Consider using 'ebs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sibdmEBS :: Lens.Lens' ScheduledInstancesBlockDeviceMapping (Lude.Maybe ScheduledInstancesEBS)
sibdmEBS = Lens.lens (ebs :: ScheduledInstancesBlockDeviceMapping -> Lude.Maybe ScheduledInstancesEBS) (\s a -> s {ebs = a} :: ScheduledInstancesBlockDeviceMapping)
{-# DEPRECATED sibdmEBS "Use generic-lens or generic-optics with 'ebs' instead." #-}

-- | The device name (for example, @/dev/sdh@ or @xvdh@ ).
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sibdmDeviceName :: Lens.Lens' ScheduledInstancesBlockDeviceMapping (Lude.Maybe Lude.Text)
sibdmDeviceName = Lens.lens (deviceName :: ScheduledInstancesBlockDeviceMapping -> Lude.Maybe Lude.Text) (\s a -> s {deviceName = a} :: ScheduledInstancesBlockDeviceMapping)
{-# DEPRECATED sibdmDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

instance Lude.ToQuery ScheduledInstancesBlockDeviceMapping where
  toQuery ScheduledInstancesBlockDeviceMapping' {..} =
    Lude.mconcat
      [ "VirtualName" Lude.=: virtualName,
        "NoDevice" Lude.=: noDevice,
        "Ebs" Lude.=: ebs,
        "DeviceName" Lude.=: deviceName
      ]
