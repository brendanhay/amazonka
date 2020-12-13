{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    vafInstanceId,
    vafDeleteOnTermination,
    vafState,
    vafDevice,
    vafVolumeId,
    vafAttachTime,
  )
where

import Network.AWS.EC2.Types.VolumeAttachmentState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes volume attachment details.
--
-- /See:/ 'mkVolumeAttachment' smart constructor.
data VolumeAttachment = VolumeAttachment'
  { -- | The ID of the instance.
    instanceId :: Lude.Maybe Lude.Text,
    -- | Indicates whether the EBS volume is deleted on instance termination.
    deleteOnTermination :: Lude.Maybe Lude.Bool,
    -- | The attachment state of the volume.
    state :: Lude.Maybe VolumeAttachmentState,
    -- | The device name.
    device :: Lude.Maybe Lude.Text,
    -- | The ID of the volume.
    volumeId :: Lude.Maybe Lude.Text,
    -- | The time stamp when the attachment initiated.
    attachTime :: Lude.Maybe Lude.DateTime
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VolumeAttachment' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance.
-- * 'deleteOnTermination' - Indicates whether the EBS volume is deleted on instance termination.
-- * 'state' - The attachment state of the volume.
-- * 'device' - The device name.
-- * 'volumeId' - The ID of the volume.
-- * 'attachTime' - The time stamp when the attachment initiated.
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
vafInstanceId :: Lens.Lens' VolumeAttachment (Lude.Maybe Lude.Text)
vafInstanceId = Lens.lens (instanceId :: VolumeAttachment -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: VolumeAttachment)
{-# DEPRECATED vafInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Indicates whether the EBS volume is deleted on instance termination.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vafDeleteOnTermination :: Lens.Lens' VolumeAttachment (Lude.Maybe Lude.Bool)
vafDeleteOnTermination = Lens.lens (deleteOnTermination :: VolumeAttachment -> Lude.Maybe Lude.Bool) (\s a -> s {deleteOnTermination = a} :: VolumeAttachment)
{-# DEPRECATED vafDeleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead." #-}

-- | The attachment state of the volume.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vafState :: Lens.Lens' VolumeAttachment (Lude.Maybe VolumeAttachmentState)
vafState = Lens.lens (state :: VolumeAttachment -> Lude.Maybe VolumeAttachmentState) (\s a -> s {state = a} :: VolumeAttachment)
{-# DEPRECATED vafState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The device name.
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vafDevice :: Lens.Lens' VolumeAttachment (Lude.Maybe Lude.Text)
vafDevice = Lens.lens (device :: VolumeAttachment -> Lude.Maybe Lude.Text) (\s a -> s {device = a} :: VolumeAttachment)
{-# DEPRECATED vafDevice "Use generic-lens or generic-optics with 'device' instead." #-}

-- | The ID of the volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vafVolumeId :: Lens.Lens' VolumeAttachment (Lude.Maybe Lude.Text)
vafVolumeId = Lens.lens (volumeId :: VolumeAttachment -> Lude.Maybe Lude.Text) (\s a -> s {volumeId = a} :: VolumeAttachment)
{-# DEPRECATED vafVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | The time stamp when the attachment initiated.
--
-- /Note:/ Consider using 'attachTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vafAttachTime :: Lens.Lens' VolumeAttachment (Lude.Maybe Lude.DateTime)
vafAttachTime = Lens.lens (attachTime :: VolumeAttachment -> Lude.Maybe Lude.DateTime) (\s a -> s {attachTime = a} :: VolumeAttachment)
{-# DEPRECATED vafAttachTime "Use generic-lens or generic-optics with 'attachTime' instead." #-}

instance Lude.FromXML VolumeAttachment where
  parseXML x =
    VolumeAttachment'
      Lude.<$> (x Lude..@? "instanceId")
      Lude.<*> (x Lude..@? "deleteOnTermination")
      Lude.<*> (x Lude..@? "status")
      Lude.<*> (x Lude..@? "device")
      Lude.<*> (x Lude..@? "volumeId")
      Lude.<*> (x Lude..@? "attachTime")
