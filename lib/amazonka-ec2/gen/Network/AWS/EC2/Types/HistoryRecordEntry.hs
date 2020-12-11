-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.HistoryRecordEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.HistoryRecordEntry
  ( HistoryRecordEntry (..),

    -- * Smart constructor
    mkHistoryRecordEntry,

    -- * Lenses
    hreEventType,
    hreEventInformation,
    hreTimestamp,
  )
where

import Network.AWS.EC2.Types.EventInformation
import Network.AWS.EC2.Types.FleetEventType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an event in the history of an EC2 Fleet.
--
-- /See:/ 'mkHistoryRecordEntry' smart constructor.
data HistoryRecordEntry = HistoryRecordEntry'
  { eventType ::
      Lude.Maybe FleetEventType,
    eventInformation :: Lude.Maybe EventInformation,
    timestamp :: Lude.Maybe Lude.ISO8601
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HistoryRecordEntry' with the minimum fields required to make a request.
--
-- * 'eventInformation' - Information about the event.
-- * 'eventType' - The event type.
-- * 'timestamp' - The date and time of the event, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
mkHistoryRecordEntry ::
  HistoryRecordEntry
mkHistoryRecordEntry =
  HistoryRecordEntry'
    { eventType = Lude.Nothing,
      eventInformation = Lude.Nothing,
      timestamp = Lude.Nothing
    }

-- | The event type.
--
-- /Note:/ Consider using 'eventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hreEventType :: Lens.Lens' HistoryRecordEntry (Lude.Maybe FleetEventType)
hreEventType = Lens.lens (eventType :: HistoryRecordEntry -> Lude.Maybe FleetEventType) (\s a -> s {eventType = a} :: HistoryRecordEntry)
{-# DEPRECATED hreEventType "Use generic-lens or generic-optics with 'eventType' instead." #-}

-- | Information about the event.
--
-- /Note:/ Consider using 'eventInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hreEventInformation :: Lens.Lens' HistoryRecordEntry (Lude.Maybe EventInformation)
hreEventInformation = Lens.lens (eventInformation :: HistoryRecordEntry -> Lude.Maybe EventInformation) (\s a -> s {eventInformation = a} :: HistoryRecordEntry)
{-# DEPRECATED hreEventInformation "Use generic-lens or generic-optics with 'eventInformation' instead." #-}

-- | The date and time of the event, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hreTimestamp :: Lens.Lens' HistoryRecordEntry (Lude.Maybe Lude.ISO8601)
hreTimestamp = Lens.lens (timestamp :: HistoryRecordEntry -> Lude.Maybe Lude.ISO8601) (\s a -> s {timestamp = a} :: HistoryRecordEntry)
{-# DEPRECATED hreTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.FromXML HistoryRecordEntry where
  parseXML x =
    HistoryRecordEntry'
      Lude.<$> (x Lude..@? "eventType")
      Lude.<*> (x Lude..@? "eventInformation")
      Lude.<*> (x Lude..@? "timestamp")
