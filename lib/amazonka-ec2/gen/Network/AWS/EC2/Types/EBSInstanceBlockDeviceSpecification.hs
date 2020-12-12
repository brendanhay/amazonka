{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EBSInstanceBlockDeviceSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EBSInstanceBlockDeviceSpecification
  ( EBSInstanceBlockDeviceSpecification (..),

    -- * Smart constructor
    mkEBSInstanceBlockDeviceSpecification,

    -- * Lenses
    eibdsDeleteOnTermination,
    eibdsVolumeId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes information used to set up an EBS volume specified in a block device mapping.
--
-- /See:/ 'mkEBSInstanceBlockDeviceSpecification' smart constructor.
data EBSInstanceBlockDeviceSpecification = EBSInstanceBlockDeviceSpecification'
  { deleteOnTermination ::
      Lude.Maybe
        Lude.Bool,
    volumeId ::
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

-- | Creates a value of 'EBSInstanceBlockDeviceSpecification' with the minimum fields required to make a request.
--
-- * 'deleteOnTermination' - Indicates whether the volume is deleted on instance termination.
-- * 'volumeId' - The ID of the EBS volume.
mkEBSInstanceBlockDeviceSpecification ::
  EBSInstanceBlockDeviceSpecification
mkEBSInstanceBlockDeviceSpecification =
  EBSInstanceBlockDeviceSpecification'
    { deleteOnTermination =
        Lude.Nothing,
      volumeId = Lude.Nothing
    }

-- | Indicates whether the volume is deleted on instance termination.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eibdsDeleteOnTermination :: Lens.Lens' EBSInstanceBlockDeviceSpecification (Lude.Maybe Lude.Bool)
eibdsDeleteOnTermination = Lens.lens (deleteOnTermination :: EBSInstanceBlockDeviceSpecification -> Lude.Maybe Lude.Bool) (\s a -> s {deleteOnTermination = a} :: EBSInstanceBlockDeviceSpecification)
{-# DEPRECATED eibdsDeleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead." #-}

-- | The ID of the EBS volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eibdsVolumeId :: Lens.Lens' EBSInstanceBlockDeviceSpecification (Lude.Maybe Lude.Text)
eibdsVolumeId = Lens.lens (volumeId :: EBSInstanceBlockDeviceSpecification -> Lude.Maybe Lude.Text) (\s a -> s {volumeId = a} :: EBSInstanceBlockDeviceSpecification)
{-# DEPRECATED eibdsVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

instance Lude.ToQuery EBSInstanceBlockDeviceSpecification where
  toQuery EBSInstanceBlockDeviceSpecification' {..} =
    Lude.mconcat
      [ "DeleteOnTermination" Lude.=: deleteOnTermination,
        "VolumeId" Lude.=: volumeId
      ]
