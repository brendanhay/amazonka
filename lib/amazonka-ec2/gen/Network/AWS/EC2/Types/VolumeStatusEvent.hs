{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeStatusEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeStatusEvent
  ( VolumeStatusEvent (..),

    -- * Smart constructor
    mkVolumeStatusEvent,

    -- * Lenses
    vseDescription,
    vseEventId,
    vseEventType,
    vseInstanceId,
    vseNotAfter,
    vseNotBefore,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a volume status event.
--
-- /See:/ 'mkVolumeStatusEvent' smart constructor.
data VolumeStatusEvent = VolumeStatusEvent'
  { -- | A description of the event.
    description :: Core.Maybe Types.String,
    -- | The ID of this event.
    eventId :: Core.Maybe Types.String,
    -- | The type of this event.
    eventType :: Core.Maybe Types.String,
    -- | The ID of the instance associated with the event.
    instanceId :: Core.Maybe Types.String,
    -- | The latest end time of the event.
    notAfter :: Core.Maybe Core.UTCTime,
    -- | The earliest start time of the event.
    notBefore :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'VolumeStatusEvent' value with any optional fields omitted.
mkVolumeStatusEvent ::
  VolumeStatusEvent
mkVolumeStatusEvent =
  VolumeStatusEvent'
    { description = Core.Nothing,
      eventId = Core.Nothing,
      eventType = Core.Nothing,
      instanceId = Core.Nothing,
      notAfter = Core.Nothing,
      notBefore = Core.Nothing
    }

-- | A description of the event.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vseDescription :: Lens.Lens' VolumeStatusEvent (Core.Maybe Types.String)
vseDescription = Lens.field @"description"
{-# DEPRECATED vseDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of this event.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vseEventId :: Lens.Lens' VolumeStatusEvent (Core.Maybe Types.String)
vseEventId = Lens.field @"eventId"
{-# DEPRECATED vseEventId "Use generic-lens or generic-optics with 'eventId' instead." #-}

-- | The type of this event.
--
-- /Note:/ Consider using 'eventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vseEventType :: Lens.Lens' VolumeStatusEvent (Core.Maybe Types.String)
vseEventType = Lens.field @"eventType"
{-# DEPRECATED vseEventType "Use generic-lens or generic-optics with 'eventType' instead." #-}

-- | The ID of the instance associated with the event.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vseInstanceId :: Lens.Lens' VolumeStatusEvent (Core.Maybe Types.String)
vseInstanceId = Lens.field @"instanceId"
{-# DEPRECATED vseInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The latest end time of the event.
--
-- /Note:/ Consider using 'notAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vseNotAfter :: Lens.Lens' VolumeStatusEvent (Core.Maybe Core.UTCTime)
vseNotAfter = Lens.field @"notAfter"
{-# DEPRECATED vseNotAfter "Use generic-lens or generic-optics with 'notAfter' instead." #-}

-- | The earliest start time of the event.
--
-- /Note:/ Consider using 'notBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vseNotBefore :: Lens.Lens' VolumeStatusEvent (Core.Maybe Core.UTCTime)
vseNotBefore = Lens.field @"notBefore"
{-# DEPRECATED vseNotBefore "Use generic-lens or generic-optics with 'notBefore' instead." #-}

instance Core.FromXML VolumeStatusEvent where
  parseXML x =
    VolumeStatusEvent'
      Core.<$> (x Core..@? "description")
      Core.<*> (x Core..@? "eventId")
      Core.<*> (x Core..@? "eventType")
      Core.<*> (x Core..@? "instanceId")
      Core.<*> (x Core..@? "notAfter")
      Core.<*> (x Core..@? "notBefore")
