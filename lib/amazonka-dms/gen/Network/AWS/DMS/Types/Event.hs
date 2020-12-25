{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.Event
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.Event
  ( Event (..),

    -- * Smart constructor
    mkEvent,

    -- * Lenses
    eDate,
    eEventCategories,
    eMessage,
    eSourceIdentifier,
    eSourceType,
  )
where

import qualified Network.AWS.DMS.Types.Message as Types
import qualified Network.AWS.DMS.Types.SourceIdentifier as Types
import qualified Network.AWS.DMS.Types.SourceType as Types
import qualified Network.AWS.DMS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an identifiable significant activity that affects a replication instance or task. This object can provide the message, the available event categories, the date and source of the event, and the AWS DMS resource type.
--
-- /See:/ 'mkEvent' smart constructor.
data Event = Event'
  { -- | The date of the event.
    date :: Core.Maybe Core.NominalDiffTime,
    -- | The event categories available for the specified source type.
    eventCategories :: Core.Maybe [Types.String],
    -- | The event message.
    message :: Core.Maybe Types.Message,
    -- | The identifier of an event source.
    sourceIdentifier :: Core.Maybe Types.SourceIdentifier,
    -- | The type of AWS DMS resource that generates events.
    --
    -- Valid values: replication-instance | endpoint | replication-task
    sourceType :: Core.Maybe Types.SourceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Event' value with any optional fields omitted.
mkEvent ::
  Event
mkEvent =
  Event'
    { date = Core.Nothing,
      eventCategories = Core.Nothing,
      message = Core.Nothing,
      sourceIdentifier = Core.Nothing,
      sourceType = Core.Nothing
    }

-- | The date of the event.
--
-- /Note:/ Consider using 'date' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDate :: Lens.Lens' Event (Core.Maybe Core.NominalDiffTime)
eDate = Lens.field @"date"
{-# DEPRECATED eDate "Use generic-lens or generic-optics with 'date' instead." #-}

-- | The event categories available for the specified source type.
--
-- /Note:/ Consider using 'eventCategories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventCategories :: Lens.Lens' Event (Core.Maybe [Types.String])
eEventCategories = Lens.field @"eventCategories"
{-# DEPRECATED eEventCategories "Use generic-lens or generic-optics with 'eventCategories' instead." #-}

-- | The event message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMessage :: Lens.Lens' Event (Core.Maybe Types.Message)
eMessage = Lens.field @"message"
{-# DEPRECATED eMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The identifier of an event source.
--
-- /Note:/ Consider using 'sourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceIdentifier :: Lens.Lens' Event (Core.Maybe Types.SourceIdentifier)
eSourceIdentifier = Lens.field @"sourceIdentifier"
{-# DEPRECATED eSourceIdentifier "Use generic-lens or generic-optics with 'sourceIdentifier' instead." #-}

-- | The type of AWS DMS resource that generates events.
--
-- Valid values: replication-instance | endpoint | replication-task
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceType :: Lens.Lens' Event (Core.Maybe Types.SourceType)
eSourceType = Lens.field @"sourceType"
{-# DEPRECATED eSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

instance Core.FromJSON Event where
  parseJSON =
    Core.withObject "Event" Core.$
      \x ->
        Event'
          Core.<$> (x Core..:? "Date")
          Core.<*> (x Core..:? "EventCategories")
          Core.<*> (x Core..:? "Message")
          Core.<*> (x Core..:? "SourceIdentifier")
          Core.<*> (x Core..:? "SourceType")
