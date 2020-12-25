{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceStatusEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceStatusEvent
  ( InstanceStatusEvent (..),

    -- * Smart constructor
    mkInstanceStatusEvent,

    -- * Lenses
    iseCode,
    iseDescription,
    iseInstanceEventId,
    iseNotAfter,
    iseNotBefore,
    iseNotBeforeDeadline,
  )
where

import qualified Network.AWS.EC2.Types.EventCode as Types
import qualified Network.AWS.EC2.Types.InstanceEventId as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a scheduled event for an instance.
--
-- /See:/ 'mkInstanceStatusEvent' smart constructor.
data InstanceStatusEvent = InstanceStatusEvent'
  { -- | The event code.
    code :: Core.Maybe Types.EventCode,
    -- | A description of the event.
    --
    -- After a scheduled event is completed, it can still be described for up to a week. If the event has been completed, this description starts with the following text: [Completed].
    description :: Core.Maybe Types.String,
    -- | The ID of the event.
    instanceEventId :: Core.Maybe Types.InstanceEventId,
    -- | The latest scheduled end time for the event.
    notAfter :: Core.Maybe Core.UTCTime,
    -- | The earliest scheduled start time for the event.
    notBefore :: Core.Maybe Core.UTCTime,
    -- | The deadline for starting the event.
    notBeforeDeadline :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'InstanceStatusEvent' value with any optional fields omitted.
mkInstanceStatusEvent ::
  InstanceStatusEvent
mkInstanceStatusEvent =
  InstanceStatusEvent'
    { code = Core.Nothing,
      description = Core.Nothing,
      instanceEventId = Core.Nothing,
      notAfter = Core.Nothing,
      notBefore = Core.Nothing,
      notBeforeDeadline = Core.Nothing
    }

-- | The event code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iseCode :: Lens.Lens' InstanceStatusEvent (Core.Maybe Types.EventCode)
iseCode = Lens.field @"code"
{-# DEPRECATED iseCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | A description of the event.
--
-- After a scheduled event is completed, it can still be described for up to a week. If the event has been completed, this description starts with the following text: [Completed].
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iseDescription :: Lens.Lens' InstanceStatusEvent (Core.Maybe Types.String)
iseDescription = Lens.field @"description"
{-# DEPRECATED iseDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the event.
--
-- /Note:/ Consider using 'instanceEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iseInstanceEventId :: Lens.Lens' InstanceStatusEvent (Core.Maybe Types.InstanceEventId)
iseInstanceEventId = Lens.field @"instanceEventId"
{-# DEPRECATED iseInstanceEventId "Use generic-lens or generic-optics with 'instanceEventId' instead." #-}

-- | The latest scheduled end time for the event.
--
-- /Note:/ Consider using 'notAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iseNotAfter :: Lens.Lens' InstanceStatusEvent (Core.Maybe Core.UTCTime)
iseNotAfter = Lens.field @"notAfter"
{-# DEPRECATED iseNotAfter "Use generic-lens or generic-optics with 'notAfter' instead." #-}

-- | The earliest scheduled start time for the event.
--
-- /Note:/ Consider using 'notBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iseNotBefore :: Lens.Lens' InstanceStatusEvent (Core.Maybe Core.UTCTime)
iseNotBefore = Lens.field @"notBefore"
{-# DEPRECATED iseNotBefore "Use generic-lens or generic-optics with 'notBefore' instead." #-}

-- | The deadline for starting the event.
--
-- /Note:/ Consider using 'notBeforeDeadline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iseNotBeforeDeadline :: Lens.Lens' InstanceStatusEvent (Core.Maybe Core.UTCTime)
iseNotBeforeDeadline = Lens.field @"notBeforeDeadline"
{-# DEPRECATED iseNotBeforeDeadline "Use generic-lens or generic-optics with 'notBeforeDeadline' instead." #-}

instance Core.FromXML InstanceStatusEvent where
  parseXML x =
    InstanceStatusEvent'
      Core.<$> (x Core..@? "code")
      Core.<*> (x Core..@? "description")
      Core.<*> (x Core..@? "instanceEventId")
      Core.<*> (x Core..@? "notAfter")
      Core.<*> (x Core..@? "notBefore")
      Core.<*> (x Core..@? "notBeforeDeadline")
