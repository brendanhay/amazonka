{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceBlockDeviceMappingSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceBlockDeviceMappingSpecification
  ( InstanceBlockDeviceMappingSpecification (..),

    -- * Smart constructor
    mkInstanceBlockDeviceMappingSpecification,

    -- * Lenses
    ibdmsVirtualName,
    ibdmsNoDevice,
    ibdmsEBS,
    ibdmsDeviceName,
  )
where

import Network.AWS.EC2.Types.EBSInstanceBlockDeviceSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a block device mapping entry.
--
-- /See:/ 'mkInstanceBlockDeviceMappingSpecification' smart constructor.
data InstanceBlockDeviceMappingSpecification = InstanceBlockDeviceMappingSpecification'
  { virtualName ::
      Lude.Maybe
        Lude.Text,
    noDevice ::
      Lude.Maybe
        Lude.Text,
    ebs ::
      Lude.Maybe
        EBSInstanceBlockDeviceSpecification,
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

-- | Creates a value of 'InstanceBlockDeviceMappingSpecification' with the minimum fields required to make a request.
--
-- * 'deviceName' - The device name (for example, @/dev/sdh@ or @xvdh@ ).
-- * 'ebs' - Parameters used to automatically set up EBS volumes when the instance is launched.
-- * 'noDevice' - suppress the specified device included in the block device mapping.
-- * 'virtualName' - The virtual device name.
mkInstanceBlockDeviceMappingSpecification ::
  InstanceBlockDeviceMappingSpecification
mkInstanceBlockDeviceMappingSpecification =
  InstanceBlockDeviceMappingSpecification'
    { virtualName =
        Lude.Nothing,
      noDevice = Lude.Nothing,
      ebs = Lude.Nothing,
      deviceName = Lude.Nothing
    }

-- | The virtual device name.
--
-- /Note:/ Consider using 'virtualName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibdmsVirtualName :: Lens.Lens' InstanceBlockDeviceMappingSpecification (Lude.Maybe Lude.Text)
ibdmsVirtualName = Lens.lens (virtualName :: InstanceBlockDeviceMappingSpecification -> Lude.Maybe Lude.Text) (\s a -> s {virtualName = a} :: InstanceBlockDeviceMappingSpecification)
{-# DEPRECATED ibdmsVirtualName "Use generic-lens or generic-optics with 'virtualName' instead." #-}

-- | suppress the specified device included in the block device mapping.
--
-- /Note:/ Consider using 'noDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibdmsNoDevice :: Lens.Lens' InstanceBlockDeviceMappingSpecification (Lude.Maybe Lude.Text)
ibdmsNoDevice = Lens.lens (noDevice :: InstanceBlockDeviceMappingSpecification -> Lude.Maybe Lude.Text) (\s a -> s {noDevice = a} :: InstanceBlockDeviceMappingSpecification)
{-# DEPRECATED ibdmsNoDevice "Use generic-lens or generic-optics with 'noDevice' instead." #-}

-- | Parameters used to automatically set up EBS volumes when the instance is launched.
--
-- /Note:/ Consider using 'ebs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibdmsEBS :: Lens.Lens' InstanceBlockDeviceMappingSpecification (Lude.Maybe EBSInstanceBlockDeviceSpecification)
ibdmsEBS = Lens.lens (ebs :: InstanceBlockDeviceMappingSpecification -> Lude.Maybe EBSInstanceBlockDeviceSpecification) (\s a -> s {ebs = a} :: InstanceBlockDeviceMappingSpecification)
{-# DEPRECATED ibdmsEBS "Use generic-lens or generic-optics with 'ebs' instead." #-}

-- | The device name (for example, @/dev/sdh@ or @xvdh@ ).
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibdmsDeviceName :: Lens.Lens' InstanceBlockDeviceMappingSpecification (Lude.Maybe Lude.Text)
ibdmsDeviceName = Lens.lens (deviceName :: InstanceBlockDeviceMappingSpecification -> Lude.Maybe Lude.Text) (\s a -> s {deviceName = a} :: InstanceBlockDeviceMappingSpecification)
{-# DEPRECATED ibdmsDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

instance Lude.ToQuery InstanceBlockDeviceMappingSpecification where
  toQuery InstanceBlockDeviceMappingSpecification' {..} =
    Lude.mconcat
      [ "VirtualName" Lude.=: virtualName,
        "NoDevice" Lude.=: noDevice,
        "Ebs" Lude.=: ebs,
        "DeviceName" Lude.=: deviceName
      ]
