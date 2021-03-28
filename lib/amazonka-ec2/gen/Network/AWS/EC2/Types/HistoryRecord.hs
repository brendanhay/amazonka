{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.HistoryRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.HistoryRecord
  ( HistoryRecord (..)
  -- * Smart constructor
  , mkHistoryRecord
  -- * Lenses
  , hrEventInformation
  , hrEventType
  , hrTimestamp
  ) where

import qualified Network.AWS.EC2.Types.EventInformation as Types
import qualified Network.AWS.EC2.Types.EventType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an event in the history of the Spot Fleet request.
--
-- /See:/ 'mkHistoryRecord' smart constructor.
data HistoryRecord = HistoryRecord'
  { eventInformation :: Core.Maybe Types.EventInformation
    -- ^ Information about the event.
  , eventType :: Core.Maybe Types.EventType
    -- ^ The event type.
--
--
--     * @error@ - An error with the Spot Fleet request.
--
--
--     * @fleetRequestChange@ - A change in the status or configuration of the Spot Fleet request.
--
--
--     * @instanceChange@ - An instance was launched or terminated.
--
--
--     * @Information@ - An informational event.
--
--
  , timestamp :: Core.Maybe Core.UTCTime
    -- ^ The date and time of the event, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'HistoryRecord' value with any optional fields omitted.
mkHistoryRecord
    :: HistoryRecord
mkHistoryRecord
  = HistoryRecord'{eventInformation = Core.Nothing,
                   eventType = Core.Nothing, timestamp = Core.Nothing}

-- | Information about the event.
--
-- /Note:/ Consider using 'eventInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrEventInformation :: Lens.Lens' HistoryRecord (Core.Maybe Types.EventInformation)
hrEventInformation = Lens.field @"eventInformation"
{-# INLINEABLE hrEventInformation #-}
{-# DEPRECATED eventInformation "Use generic-lens or generic-optics with 'eventInformation' instead"  #-}

-- | The event type.
--
--
--     * @error@ - An error with the Spot Fleet request.
--
--
--     * @fleetRequestChange@ - A change in the status or configuration of the Spot Fleet request.
--
--
--     * @instanceChange@ - An instance was launched or terminated.
--
--
--     * @Information@ - An informational event.
--
--
--
-- /Note:/ Consider using 'eventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrEventType :: Lens.Lens' HistoryRecord (Core.Maybe Types.EventType)
hrEventType = Lens.field @"eventType"
{-# INLINEABLE hrEventType #-}
{-# DEPRECATED eventType "Use generic-lens or generic-optics with 'eventType' instead"  #-}

-- | The date and time of the event, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrTimestamp :: Lens.Lens' HistoryRecord (Core.Maybe Core.UTCTime)
hrTimestamp = Lens.field @"timestamp"
{-# INLINEABLE hrTimestamp #-}
{-# DEPRECATED timestamp "Use generic-lens or generic-optics with 'timestamp' instead"  #-}

instance Core.FromXML HistoryRecord where
        parseXML x
          = HistoryRecord' Core.<$>
              (x Core..@? "eventInformation") Core.<*> x Core..@? "eventType"
                Core.<*> x Core..@? "timestamp"
