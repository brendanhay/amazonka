-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeAttachment
  ( VolumeAttachment (..),

    -- * Smart constructor
    mkVolumeAttachment,

    -- * Lenses
    volInstanceId,
    volDeleteOnTermination,
    volState,
    volDevice,
    volVolumeId,
    volAttachTime,
  )
where

import Network.AWS.EC2.Types.VolumeAttachmentState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes volume attachment details.
--
-- /See:/ 'mkVolumeAttachment' smart constructor.
data VolumeAttachment = VolumeAttachment'
  { instanceId ::
      Lude.Maybe Lude.Text,
    deleteOnTermination :: Lude.Maybe Lude.Bool,
    state :: Lude.Maybe VolumeAttachmentState,
    device :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'VolumeAttachment' with the minimum fields required to make a request.
--
-- * 'attachTime' - The time stamp when the attachment initiated.
-- * 'deleteOnTermination' - Indicates whether the EBS volume is deleted on instance termination.
-- * 'device' - The device name.
-- * 'instanceId' - The ID of the instance.
-- * 'state' - The attachment state of the volume.
-- * 'volumeId' - The ID of the volume.
mkVolumeAttachment ::
  VolumeAttachment
mkVolumeAttachment =
  VolumeAttachment'
    { instanceId = Lude.Nothing,
      deleteOnTermination = Lude.Nothing,
      state = Lude.Nothing,
      device = Lude.Nothing,
      volumeId = Lude.Nothing,
      attachTime = Lude.Nothing
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
volInstanceId :: Lens.Lens' VolumeAttachment (Lude.Maybe Lude.Text)
volInstanceId = Lens.lens (instanceId :: VolumeAttachment -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: VolumeAttachment)
{-# DEPRECATED volInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Indicates whether the EBS volume is deleted on instance termination.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
volDeleteOnTermination :: Lens.Lens' VolumeAttachment (Lude.Maybe Lude.Bool)
volDeleteOnTermination = Lens.lens (deleteOnTermination :: VolumeAttachment -> Lude.Maybe Lude.Bool) (\s a -> s {deleteOnTermination = a} :: VolumeAttachment)
{-# DEPRECATED volDeleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead." #-}

-- | The attachment state of the volume.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
volState :: Lens.Lens' VolumeAttachment (Lude.Maybe VolumeAttachmentState)
volState = Lens.lens (state :: VolumeAttachment -> Lude.Maybe VolumeAttachmentState) (\s a -> s {state = a} :: VolumeAttachment)
{-# DEPRECATED volState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The device name.
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
volDevice :: Lens.Lens' VolumeAttachment (Lude.Maybe Lude.Text)
volDevice = Lens.lens (device :: VolumeAttachment -> Lude.Maybe Lude.Text) (\s a -> s {device = a} :: VolumeAttachment)
{-# DEPRECATED volDevice "Use generic-lens or generic-optics with 'device' instead." #-}

-- | The ID of the volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
volVolumeId :: Lens.Lens' VolumeAttachment (Lude.Maybe Lude.Text)
volVolumeId = Lens.lens (volumeId :: VolumeAttachment -> Lude.Maybe Lude.Text) (\s a -> s {volumeId = a} :: VolumeAttachment)
{-# DEPRECATED volVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | The time stamp when the attachment initiated.
--
-- /Note:/ Consider using 'attachTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
volAttachTime :: Lens.Lens' VolumeAttachment (Lude.Maybe Lude.ISO8601)
volAttachTime = Lens.lens (attachTime :: VolumeAttachment -> Lude.Maybe Lude.ISO8601) (\s a -> s {attachTime = a} :: VolumeAttachment)
{-# DEPRECATED volAttachTime "Use generic-lens or generic-optics with 'attachTime' instead." #-}

instance Lude.FromXML VolumeAttachment where
  parseXML x =
    VolumeAttachment'
      Lude.<$> (x Lude..@? "instanceId")
      Lude.<*> (x Lude..@? "deleteOnTermination")
      Lude.<*> (x Lude..@? "status")
      Lude.<*> (x Lude..@? "device")
      Lude.<*> (x Lude..@? "volumeId")
      Lude.<*> (x Lude..@? "attachTime")
