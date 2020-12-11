-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EBSInstanceBlockDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EBSInstanceBlockDevice
  ( EBSInstanceBlockDevice (..),

    -- * Smart constructor
    mkEBSInstanceBlockDevice,

    -- * Lenses
    eibdStatus,
    eibdDeleteOnTermination,
    eibdVolumeId,
    eibdAttachTime,
  )
where

import Network.AWS.EC2.Types.AttachmentStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a parameter used to set up an EBS volume in a block device mapping.
--
-- /See:/ 'mkEBSInstanceBlockDevice' smart constructor.
data EBSInstanceBlockDevice = EBSInstanceBlockDevice'
  { status ::
      Lude.Maybe AttachmentStatus,
    deleteOnTermination :: Lude.Maybe Lude.Bool,
    volumeId :: Lude.Maybe Lude.Text,
    attachTime :: Lude.Maybe Lude.ISO8601
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EBSInstanceBlockDevice' with the minimum fields required to make a request.
--
-- * 'attachTime' - The time stamp when the attachment initiated.
-- * 'deleteOnTermination' - Indicates whether the volume is deleted on instance termination.
-- * 'status' - The attachment state.
-- * 'volumeId' - The ID of the EBS volume.
mkEBSInstanceBlockDevice ::
  EBSInstanceBlockDevice
mkEBSInstanceBlockDevice =
  EBSInstanceBlockDevice'
    { status = Lude.Nothing,
      deleteOnTermination = Lude.Nothing,
      volumeId = Lude.Nothing,
      attachTime = Lude.Nothing
    }

-- | The attachment state.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eibdStatus :: Lens.Lens' EBSInstanceBlockDevice (Lude.Maybe AttachmentStatus)
eibdStatus = Lens.lens (status :: EBSInstanceBlockDevice -> Lude.Maybe AttachmentStatus) (\s a -> s {status = a} :: EBSInstanceBlockDevice)
{-# DEPRECATED eibdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Indicates whether the volume is deleted on instance termination.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eibdDeleteOnTermination :: Lens.Lens' EBSInstanceBlockDevice (Lude.Maybe Lude.Bool)
eibdDeleteOnTermination = Lens.lens (deleteOnTermination :: EBSInstanceBlockDevice -> Lude.Maybe Lude.Bool) (\s a -> s {deleteOnTermination = a} :: EBSInstanceBlockDevice)
{-# DEPRECATED eibdDeleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead." #-}

-- | The ID of the EBS volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eibdVolumeId :: Lens.Lens' EBSInstanceBlockDevice (Lude.Maybe Lude.Text)
eibdVolumeId = Lens.lens (volumeId :: EBSInstanceBlockDevice -> Lude.Maybe Lude.Text) (\s a -> s {volumeId = a} :: EBSInstanceBlockDevice)
{-# DEPRECATED eibdVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | The time stamp when the attachment initiated.
--
-- /Note:/ Consider using 'attachTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eibdAttachTime :: Lens.Lens' EBSInstanceBlockDevice (Lude.Maybe Lude.ISO8601)
eibdAttachTime = Lens.lens (attachTime :: EBSInstanceBlockDevice -> Lude.Maybe Lude.ISO8601) (\s a -> s {attachTime = a} :: EBSInstanceBlockDevice)
{-# DEPRECATED eibdAttachTime "Use generic-lens or generic-optics with 'attachTime' instead." #-}

instance Lude.FromXML EBSInstanceBlockDevice where
  parseXML x =
    EBSInstanceBlockDevice'
      Lude.<$> (x Lude..@? "status")
      Lude.<*> (x Lude..@? "deleteOnTermination")
      Lude.<*> (x Lude..@? "volumeId")
      Lude.<*> (x Lude..@? "attachTime")
