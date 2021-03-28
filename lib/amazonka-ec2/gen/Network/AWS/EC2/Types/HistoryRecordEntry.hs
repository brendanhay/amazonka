{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.HistoryRecordEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.HistoryRecordEntry
  ( HistoryRecordEntry (..)
  -- * Smart constructor
  , mkHistoryRecordEntry
  -- * Lenses
  , hreEventInformation
  , hreEventType
  , hreTimestamp
  ) where

import qualified Network.AWS.EC2.Types.EventInformation as Types
import qualified Network.AWS.EC2.Types.FleetEventType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an event in the history of an EC2 Fleet.
--
-- /See:/ 'mkHistoryRecordEntry' smart constructor.
data HistoryRecordEntry = HistoryRecordEntry'
  { eventInformation :: Core.Maybe Types.EventInformation
    -- ^ Information about the event.
  , eventType :: Core.Maybe Types.FleetEventType
    -- ^ The event type.
  , timestamp :: Core.Maybe Core.UTCTime
    -- ^ The date and time of the event, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'HistoryRecordEntry' value with any optional fields omitted.
mkHistoryRecordEntry
    :: HistoryRecordEntry
mkHistoryRecordEntry
  = HistoryRecordEntry'{eventInformation = Core.Nothing,
                        eventType = Core.Nothing, timestamp = Core.Nothing}

-- | Information about the event.
--
-- /Note:/ Consider using 'eventInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hreEventInformation :: Lens.Lens' HistoryRecordEntry (Core.Maybe Types.EventInformation)
hreEventInformation = Lens.field @"eventInformation"
{-# INLINEABLE hreEventInformation #-}
{-# DEPRECATED eventInformation "Use generic-lens or generic-optics with 'eventInformation' instead"  #-}

-- | The event type.
--
-- /Note:/ Consider using 'eventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hreEventType :: Lens.Lens' HistoryRecordEntry (Core.Maybe Types.FleetEventType)
hreEventType = Lens.field @"eventType"
{-# INLINEABLE hreEventType #-}
{-# DEPRECATED eventType "Use generic-lens or generic-optics with 'eventType' instead"  #-}

-- | The date and time of the event, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hreTimestamp :: Lens.Lens' HistoryRecordEntry (Core.Maybe Core.UTCTime)
hreTimestamp = Lens.field @"timestamp"
{-# INLINEABLE hreTimestamp #-}
{-# DEPRECATED timestamp "Use generic-lens or generic-optics with 'timestamp' instead"  #-}

instance Core.FromXML HistoryRecordEntry where
        parseXML x
          = HistoryRecordEntry' Core.<$>
              (x Core..@? "eventInformation") Core.<*> x Core..@? "eventType"
                Core.<*> x Core..@? "timestamp"
