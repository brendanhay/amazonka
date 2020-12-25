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
    vaAttachTime,
    vaDeleteOnTermination,
    vaDevice,
    vaInstanceId,
    vaState,
    vaVolumeId,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.VolumeAttachmentState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes volume attachment details.
--
-- /See:/ 'mkVolumeAttachment' smart constructor.
data VolumeAttachment = VolumeAttachment'
  { -- | The time stamp when the attachment initiated.
    attachTime :: Core.Maybe Core.UTCTime,
    -- | Indicates whether the EBS volume is deleted on instance termination.
    deleteOnTermination :: Core.Maybe Core.Bool,
    -- | The device name.
    device :: Core.Maybe Types.String,
    -- | The ID of the instance.
    instanceId :: Core.Maybe Types.String,
    -- | The attachment state of the volume.
    state :: Core.Maybe Types.VolumeAttachmentState,
    -- | The ID of the volume.
    volumeId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'VolumeAttachment' value with any optional fields omitted.
mkVolumeAttachment ::
  VolumeAttachment
mkVolumeAttachment =
  VolumeAttachment'
    { attachTime = Core.Nothing,
      deleteOnTermination = Core.Nothing,
      device = Core.Nothing,
      instanceId = Core.Nothing,
      state = Core.Nothing,
      volumeId = Core.Nothing
    }

-- | The time stamp when the attachment initiated.
--
-- /Note:/ Consider using 'attachTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vaAttachTime :: Lens.Lens' VolumeAttachment (Core.Maybe Core.UTCTime)
vaAttachTime = Lens.field @"attachTime"
{-# DEPRECATED vaAttachTime "Use generic-lens or generic-optics with 'attachTime' instead." #-}

-- | Indicates whether the EBS volume is deleted on instance termination.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vaDeleteOnTermination :: Lens.Lens' VolumeAttachment (Core.Maybe Core.Bool)
vaDeleteOnTermination = Lens.field @"deleteOnTermination"
{-# DEPRECATED vaDeleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead." #-}

-- | The device name.
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vaDevice :: Lens.Lens' VolumeAttachment (Core.Maybe Types.String)
vaDevice = Lens.field @"device"
{-# DEPRECATED vaDevice "Use generic-lens or generic-optics with 'device' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vaInstanceId :: Lens.Lens' VolumeAttachment (Core.Maybe Types.String)
vaInstanceId = Lens.field @"instanceId"
{-# DEPRECATED vaInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The attachment state of the volume.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vaState :: Lens.Lens' VolumeAttachment (Core.Maybe Types.VolumeAttachmentState)
vaState = Lens.field @"state"
{-# DEPRECATED vaState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vaVolumeId :: Lens.Lens' VolumeAttachment (Core.Maybe Types.String)
vaVolumeId = Lens.field @"volumeId"
{-# DEPRECATED vaVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

instance Core.FromXML VolumeAttachment where
  parseXML x =
    VolumeAttachment'
      Core.<$> (x Core..@? "attachTime")
      Core.<*> (x Core..@? "deleteOnTermination")
      Core.<*> (x Core..@? "device")
      Core.<*> (x Core..@? "instanceId")
      Core.<*> (x Core..@? "status")
      Core.<*> (x Core..@? "volumeId")
