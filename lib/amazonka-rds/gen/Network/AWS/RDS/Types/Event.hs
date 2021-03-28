{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Event
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.Event
  ( Event (..)
  -- * Smart constructor
  , mkEvent
  -- * Lenses
  , eDate
  , eEventCategories
  , eMessage
  , eSourceArn
  , eSourceIdentifier
  , eSourceType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.SourceType as Types

-- | This data type is used as a response element in the @DescribeEvents@ action. 
--
-- /See:/ 'mkEvent' smart constructor.
data Event = Event'
  { date :: Core.Maybe Core.UTCTime
    -- ^ Specifies the date and time of the event.
  , eventCategories :: Core.Maybe [Core.Text]
    -- ^ Specifies the category for the event.
  , message :: Core.Maybe Core.Text
    -- ^ Provides the text of this event.
  , sourceArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) for the event.
  , sourceIdentifier :: Core.Maybe Core.Text
    -- ^ Provides the identifier for the source of the event.
  , sourceType :: Core.Maybe Types.SourceType
    -- ^ Specifies the source type for this event.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Event' value with any optional fields omitted.
mkEvent
    :: Event
mkEvent
  = Event'{date = Core.Nothing, eventCategories = Core.Nothing,
           message = Core.Nothing, sourceArn = Core.Nothing,
           sourceIdentifier = Core.Nothing, sourceType = Core.Nothing}

-- | Specifies the date and time of the event.
--
-- /Note:/ Consider using 'date' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDate :: Lens.Lens' Event (Core.Maybe Core.UTCTime)
eDate = Lens.field @"date"
{-# INLINEABLE eDate #-}
{-# DEPRECATED date "Use generic-lens or generic-optics with 'date' instead"  #-}

-- | Specifies the category for the event.
--
-- /Note:/ Consider using 'eventCategories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventCategories :: Lens.Lens' Event (Core.Maybe [Core.Text])
eEventCategories = Lens.field @"eventCategories"
{-# INLINEABLE eEventCategories #-}
{-# DEPRECATED eventCategories "Use generic-lens or generic-optics with 'eventCategories' instead"  #-}

-- | Provides the text of this event.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMessage :: Lens.Lens' Event (Core.Maybe Core.Text)
eMessage = Lens.field @"message"
{-# INLINEABLE eMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | The Amazon Resource Name (ARN) for the event.
--
-- /Note:/ Consider using 'sourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceArn :: Lens.Lens' Event (Core.Maybe Core.Text)
eSourceArn = Lens.field @"sourceArn"
{-# INLINEABLE eSourceArn #-}
{-# DEPRECATED sourceArn "Use generic-lens or generic-optics with 'sourceArn' instead"  #-}

-- | Provides the identifier for the source of the event.
--
-- /Note:/ Consider using 'sourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceIdentifier :: Lens.Lens' Event (Core.Maybe Core.Text)
eSourceIdentifier = Lens.field @"sourceIdentifier"
{-# INLINEABLE eSourceIdentifier #-}
{-# DEPRECATED sourceIdentifier "Use generic-lens or generic-optics with 'sourceIdentifier' instead"  #-}

-- | Specifies the source type for this event.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceType :: Lens.Lens' Event (Core.Maybe Types.SourceType)
eSourceType = Lens.field @"sourceType"
{-# INLINEABLE eSourceType #-}
{-# DEPRECATED sourceType "Use generic-lens or generic-optics with 'sourceType' instead"  #-}

instance Core.FromXML Event where
        parseXML x
          = Event' Core.<$>
              (x Core..@? "Date") Core.<*>
                x Core..@? "EventCategories" Core..<@>
                  Core.parseXMLList "EventCategory"
                Core.<*> x Core..@? "Message"
                Core.<*> x Core..@? "SourceArn"
                Core.<*> x Core..@? "SourceIdentifier"
                Core.<*> x Core..@? "SourceType"
