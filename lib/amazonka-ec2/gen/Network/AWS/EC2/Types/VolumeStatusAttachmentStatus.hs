{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeStatusAttachmentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeStatusAttachmentStatus
  ( VolumeStatusAttachmentStatus (..),

    -- * Smart constructor
    mkVolumeStatusAttachmentStatus,

    -- * Lenses
    vsasInstanceId,
    vsasIoPerformance,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the instances to which the volume is attached.
--
-- /See:/ 'mkVolumeStatusAttachmentStatus' smart constructor.
data VolumeStatusAttachmentStatus = VolumeStatusAttachmentStatus'
  { -- | The ID of the attached instance.
    instanceId :: Core.Maybe Types.String,
    -- | The maximum IOPS supported by the attached instance.
    ioPerformance :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VolumeStatusAttachmentStatus' value with any optional fields omitted.
mkVolumeStatusAttachmentStatus ::
  VolumeStatusAttachmentStatus
mkVolumeStatusAttachmentStatus =
  VolumeStatusAttachmentStatus'
    { instanceId = Core.Nothing,
      ioPerformance = Core.Nothing
    }

-- | The ID of the attached instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsasInstanceId :: Lens.Lens' VolumeStatusAttachmentStatus (Core.Maybe Types.String)
vsasInstanceId = Lens.field @"instanceId"
{-# DEPRECATED vsasInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The maximum IOPS supported by the attached instance.
--
-- /Note:/ Consider using 'ioPerformance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsasIoPerformance :: Lens.Lens' VolumeStatusAttachmentStatus (Core.Maybe Types.String)
vsasIoPerformance = Lens.field @"ioPerformance"
{-# DEPRECATED vsasIoPerformance "Use generic-lens or generic-optics with 'ioPerformance' instead." #-}

instance Core.FromXML VolumeStatusAttachmentStatus where
  parseXML x =
    VolumeStatusAttachmentStatus'
      Core.<$> (x Core..@? "instanceId") Core.<*> (x Core..@? "ioPerformance")
