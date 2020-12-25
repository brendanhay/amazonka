{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Event
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Event
  ( Event (..),

    -- * Smart constructor
    mkEvent,

    -- * Lenses
    eDate,
    eEventCategories,
    eMessage,
    eSourceArn,
    eSourceIdentifier,
    eSourceType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.Message as Types
import qualified Network.AWS.RDS.Types.SourceArn as Types
import qualified Network.AWS.RDS.Types.SourceIdentifier as Types
import qualified Network.AWS.RDS.Types.SourceType as Types
import qualified Network.AWS.RDS.Types.String as Types

-- | This data type is used as a response element in the @DescribeEvents@ action.
--
-- /See:/ 'mkEvent' smart constructor.
data Event = Event'
  { -- | Specifies the date and time of the event.
    date :: Core.Maybe Core.UTCTime,
    -- | Specifies the category for the event.
    eventCategories :: Core.Maybe [Types.String],
    -- | Provides the text of this event.
    message :: Core.Maybe Types.Message,
    -- | The Amazon Resource Name (ARN) for the event.
    sourceArn :: Core.Maybe Types.SourceArn,
    -- | Provides the identifier for the source of the event.
    sourceIdentifier :: Core.Maybe Types.SourceIdentifier,
    -- | Specifies the source type for this event.
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
      sourceArn = Core.Nothing,
      sourceIdentifier = Core.Nothing,
      sourceType = Core.Nothing
    }

-- | Specifies the date and time of the event.
--
-- /Note:/ Consider using 'date' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDate :: Lens.Lens' Event (Core.Maybe Core.UTCTime)
eDate = Lens.field @"date"
{-# DEPRECATED eDate "Use generic-lens or generic-optics with 'date' instead." #-}

-- | Specifies the category for the event.
--
-- /Note:/ Consider using 'eventCategories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventCategories :: Lens.Lens' Event (Core.Maybe [Types.String])
eEventCategories = Lens.field @"eventCategories"
{-# DEPRECATED eEventCategories "Use generic-lens or generic-optics with 'eventCategories' instead." #-}

-- | Provides the text of this event.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMessage :: Lens.Lens' Event (Core.Maybe Types.Message)
eMessage = Lens.field @"message"
{-# DEPRECATED eMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The Amazon Resource Name (ARN) for the event.
--
-- /Note:/ Consider using 'sourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceArn :: Lens.Lens' Event (Core.Maybe Types.SourceArn)
eSourceArn = Lens.field @"sourceArn"
{-# DEPRECATED eSourceArn "Use generic-lens or generic-optics with 'sourceArn' instead." #-}

-- | Provides the identifier for the source of the event.
--
-- /Note:/ Consider using 'sourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceIdentifier :: Lens.Lens' Event (Core.Maybe Types.SourceIdentifier)
eSourceIdentifier = Lens.field @"sourceIdentifier"
{-# DEPRECATED eSourceIdentifier "Use generic-lens or generic-optics with 'sourceIdentifier' instead." #-}

-- | Specifies the source type for this event.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceType :: Lens.Lens' Event (Core.Maybe Types.SourceType)
eSourceType = Lens.field @"sourceType"
{-# DEPRECATED eSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

instance Core.FromXML Event where
  parseXML x =
    Event'
      Core.<$> (x Core..@? "Date")
      Core.<*> ( x Core..@? "EventCategories"
                   Core..<@> Core.parseXMLList "EventCategory"
               )
      Core.<*> (x Core..@? "Message")
      Core.<*> (x Core..@? "SourceArn")
      Core.<*> (x Core..@? "SourceIdentifier")
      Core.<*> (x Core..@? "SourceType")
