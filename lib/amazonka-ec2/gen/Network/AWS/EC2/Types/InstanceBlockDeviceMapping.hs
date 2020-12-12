{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceBlockDeviceMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceBlockDeviceMapping
  ( InstanceBlockDeviceMapping (..),

    -- * Smart constructor
    mkInstanceBlockDeviceMapping,

    -- * Lenses
    ibdmEBS,
    ibdmDeviceName,
  )
where

import Network.AWS.EC2.Types.EBSInstanceBlockDevice
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a block device mapping.
--
-- /See:/ 'mkInstanceBlockDeviceMapping' smart constructor.
data InstanceBlockDeviceMapping = InstanceBlockDeviceMapping'
  { ebs ::
      Lude.Maybe EBSInstanceBlockDevice,
    deviceName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceBlockDeviceMapping' with the minimum fields required to make a request.
--
-- * 'deviceName' - The device name (for example, @/dev/sdh@ or @xvdh@ ).
-- * 'ebs' - Parameters used to automatically set up EBS volumes when the instance is launched.
mkInstanceBlockDeviceMapping ::
  InstanceBlockDeviceMapping
mkInstanceBlockDeviceMapping =
  InstanceBlockDeviceMapping'
    { ebs = Lude.Nothing,
      deviceName = Lude.Nothing
    }

-- | Parameters used to automatically set up EBS volumes when the instance is launched.
--
-- /Note:/ Consider using 'ebs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibdmEBS :: Lens.Lens' InstanceBlockDeviceMapping (Lude.Maybe EBSInstanceBlockDevice)
ibdmEBS = Lens.lens (ebs :: InstanceBlockDeviceMapping -> Lude.Maybe EBSInstanceBlockDevice) (\s a -> s {ebs = a} :: InstanceBlockDeviceMapping)
{-# DEPRECATED ibdmEBS "Use generic-lens or generic-optics with 'ebs' instead." #-}

-- | The device name (for example, @/dev/sdh@ or @xvdh@ ).
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibdmDeviceName :: Lens.Lens' InstanceBlockDeviceMapping (Lude.Maybe Lude.Text)
ibdmDeviceName = Lens.lens (deviceName :: InstanceBlockDeviceMapping -> Lude.Maybe Lude.Text) (\s a -> s {deviceName = a} :: InstanceBlockDeviceMapping)
{-# DEPRECATED ibdmDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

instance Lude.FromXML InstanceBlockDeviceMapping where
  parseXML x =
    InstanceBlockDeviceMapping'
      Lude.<$> (x Lude..@? "ebs") Lude.<*> (x Lude..@? "deviceName")
