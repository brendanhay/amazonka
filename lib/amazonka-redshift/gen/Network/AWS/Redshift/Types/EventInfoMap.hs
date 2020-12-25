{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.EventInfoMap
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.EventInfoMap
  ( EventInfoMap (..),

    -- * Smart constructor
    mkEventInfoMap,

    -- * Lenses
    eimEventCategories,
    eimEventDescription,
    eimEventId,
    eimSeverity,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.String as Types

-- | Describes event information.
--
-- /See:/ 'mkEventInfoMap' smart constructor.
data EventInfoMap = EventInfoMap'
  { -- | The category of an Amazon Redshift event.
    eventCategories :: Core.Maybe [Types.String],
    -- | The description of an Amazon Redshift event.
    eventDescription :: Core.Maybe Types.String,
    -- | The identifier of an Amazon Redshift event.
    eventId :: Core.Maybe Types.String,
    -- | The severity of the event.
    --
    -- Values: ERROR, INFO
    severity :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EventInfoMap' value with any optional fields omitted.
mkEventInfoMap ::
  EventInfoMap
mkEventInfoMap =
  EventInfoMap'
    { eventCategories = Core.Nothing,
      eventDescription = Core.Nothing,
      eventId = Core.Nothing,
      severity = Core.Nothing
    }

-- | The category of an Amazon Redshift event.
--
-- /Note:/ Consider using 'eventCategories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eimEventCategories :: Lens.Lens' EventInfoMap (Core.Maybe [Types.String])
eimEventCategories = Lens.field @"eventCategories"
{-# DEPRECATED eimEventCategories "Use generic-lens or generic-optics with 'eventCategories' instead." #-}

-- | The description of an Amazon Redshift event.
--
-- /Note:/ Consider using 'eventDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eimEventDescription :: Lens.Lens' EventInfoMap (Core.Maybe Types.String)
eimEventDescription = Lens.field @"eventDescription"
{-# DEPRECATED eimEventDescription "Use generic-lens or generic-optics with 'eventDescription' instead." #-}

-- | The identifier of an Amazon Redshift event.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eimEventId :: Lens.Lens' EventInfoMap (Core.Maybe Types.String)
eimEventId = Lens.field @"eventId"
{-# DEPRECATED eimEventId "Use generic-lens or generic-optics with 'eventId' instead." #-}

-- | The severity of the event.
--
-- Values: ERROR, INFO
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eimSeverity :: Lens.Lens' EventInfoMap (Core.Maybe Types.String)
eimSeverity = Lens.field @"severity"
{-# DEPRECATED eimSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

instance Core.FromXML EventInfoMap where
  parseXML x =
    EventInfoMap'
      Core.<$> ( x Core..@? "EventCategories"
                   Core..<@> Core.parseXMLList "EventCategory"
               )
      Core.<*> (x Core..@? "EventDescription")
      Core.<*> (x Core..@? "EventId")
      Core.<*> (x Core..@? "Severity")
