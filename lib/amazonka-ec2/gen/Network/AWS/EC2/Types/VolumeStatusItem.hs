{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeStatusItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeStatusItem
  ( VolumeStatusItem (..),

    -- * Smart constructor
    mkVolumeStatusItem,

    -- * Lenses
    vsiActions,
    vsiAttachmentStatuses,
    vsiAvailabilityZone,
    vsiEvents,
    vsiOutpostArn,
    vsiVolumeId,
    vsiVolumeStatus,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.VolumeStatusAction as Types
import qualified Network.AWS.EC2.Types.VolumeStatusAttachmentStatus as Types
import qualified Network.AWS.EC2.Types.VolumeStatusEvent as Types
import qualified Network.AWS.EC2.Types.VolumeStatusInfo as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the volume status.
--
-- /See:/ 'mkVolumeStatusItem' smart constructor.
data VolumeStatusItem = VolumeStatusItem'
  { -- | The details of the operation.
    actions :: Core.Maybe [Types.VolumeStatusAction],
    -- | Information about the instances to which the volume is attached.
    attachmentStatuses :: Core.Maybe [Types.VolumeStatusAttachmentStatus],
    -- | The Availability Zone of the volume.
    availabilityZone :: Core.Maybe Types.String,
    -- | A list of events associated with the volume.
    events :: Core.Maybe [Types.VolumeStatusEvent],
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostArn :: Core.Maybe Types.String,
    -- | The volume ID.
    volumeId :: Core.Maybe Types.String,
    -- | The volume status.
    volumeStatus :: Core.Maybe Types.VolumeStatusInfo
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'VolumeStatusItem' value with any optional fields omitted.
mkVolumeStatusItem ::
  VolumeStatusItem
mkVolumeStatusItem =
  VolumeStatusItem'
    { actions = Core.Nothing,
      attachmentStatuses = Core.Nothing,
      availabilityZone = Core.Nothing,
      events = Core.Nothing,
      outpostArn = Core.Nothing,
      volumeId = Core.Nothing,
      volumeStatus = Core.Nothing
    }

-- | The details of the operation.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsiActions :: Lens.Lens' VolumeStatusItem (Core.Maybe [Types.VolumeStatusAction])
vsiActions = Lens.field @"actions"
{-# DEPRECATED vsiActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | Information about the instances to which the volume is attached.
--
-- /Note:/ Consider using 'attachmentStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsiAttachmentStatuses :: Lens.Lens' VolumeStatusItem (Core.Maybe [Types.VolumeStatusAttachmentStatus])
vsiAttachmentStatuses = Lens.field @"attachmentStatuses"
{-# DEPRECATED vsiAttachmentStatuses "Use generic-lens or generic-optics with 'attachmentStatuses' instead." #-}

-- | The Availability Zone of the volume.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsiAvailabilityZone :: Lens.Lens' VolumeStatusItem (Core.Maybe Types.String)
vsiAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED vsiAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | A list of events associated with the volume.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsiEvents :: Lens.Lens' VolumeStatusItem (Core.Maybe [Types.VolumeStatusEvent])
vsiEvents = Lens.field @"events"
{-# DEPRECATED vsiEvents "Use generic-lens or generic-optics with 'events' instead." #-}

-- | The Amazon Resource Name (ARN) of the Outpost.
--
-- /Note:/ Consider using 'outpostArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsiOutpostArn :: Lens.Lens' VolumeStatusItem (Core.Maybe Types.String)
vsiOutpostArn = Lens.field @"outpostArn"
{-# DEPRECATED vsiOutpostArn "Use generic-lens or generic-optics with 'outpostArn' instead." #-}

-- | The volume ID.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsiVolumeId :: Lens.Lens' VolumeStatusItem (Core.Maybe Types.String)
vsiVolumeId = Lens.field @"volumeId"
{-# DEPRECATED vsiVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | The volume status.
--
-- /Note:/ Consider using 'volumeStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsiVolumeStatus :: Lens.Lens' VolumeStatusItem (Core.Maybe Types.VolumeStatusInfo)
vsiVolumeStatus = Lens.field @"volumeStatus"
{-# DEPRECATED vsiVolumeStatus "Use generic-lens or generic-optics with 'volumeStatus' instead." #-}

instance Core.FromXML VolumeStatusItem where
  parseXML x =
    VolumeStatusItem'
      Core.<$> (x Core..@? "actionsSet" Core..<@> Core.parseXMLList "item")
      Core.<*> ( x Core..@? "attachmentStatuses"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "availabilityZone")
      Core.<*> (x Core..@? "eventsSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "outpostArn")
      Core.<*> (x Core..@? "volumeId")
      Core.<*> (x Core..@? "volumeStatus")
