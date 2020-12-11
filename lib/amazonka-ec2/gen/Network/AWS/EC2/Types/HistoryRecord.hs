-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.HistoryRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.HistoryRecord
  ( HistoryRecord (..),

    -- * Smart constructor
    mkHistoryRecord,

    -- * Lenses
    hrEventType,
    hrEventInformation,
    hrTimestamp,
  )
where

import Network.AWS.EC2.Types.EventInformation
import Network.AWS.EC2.Types.EventType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an event in the history of the Spot Fleet request.
--
-- /See:/ 'mkHistoryRecord' smart constructor.
data HistoryRecord = HistoryRecord'
  { eventType ::
      Lude.Maybe EventType,
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

-- | Creates a value of 'HistoryRecord' with the minimum fields required to make a request.
--
-- * 'eventInformation' - Information about the event.
-- * 'eventType' - The event type.
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
-- * 'timestamp' - The date and time of the event, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
mkHistoryRecord ::
  HistoryRecord
mkHistoryRecord =
  HistoryRecord'
    { eventType = Lude.Nothing,
      eventInformation = Lude.Nothing,
      timestamp = Lude.Nothing
    }

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
hrEventType :: Lens.Lens' HistoryRecord (Lude.Maybe EventType)
hrEventType = Lens.lens (eventType :: HistoryRecord -> Lude.Maybe EventType) (\s a -> s {eventType = a} :: HistoryRecord)
{-# DEPRECATED hrEventType "Use generic-lens or generic-optics with 'eventType' instead." #-}

-- | Information about the event.
--
-- /Note:/ Consider using 'eventInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrEventInformation :: Lens.Lens' HistoryRecord (Lude.Maybe EventInformation)
hrEventInformation = Lens.lens (eventInformation :: HistoryRecord -> Lude.Maybe EventInformation) (\s a -> s {eventInformation = a} :: HistoryRecord)
{-# DEPRECATED hrEventInformation "Use generic-lens or generic-optics with 'eventInformation' instead." #-}

-- | The date and time of the event, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrTimestamp :: Lens.Lens' HistoryRecord (Lude.Maybe Lude.ISO8601)
hrTimestamp = Lens.lens (timestamp :: HistoryRecord -> Lude.Maybe Lude.ISO8601) (\s a -> s {timestamp = a} :: HistoryRecord)
{-# DEPRECATED hrTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.FromXML HistoryRecord where
  parseXML x =
    HistoryRecord'
      Lude.<$> (x Lude..@? "eventType")
      Lude.<*> (x Lude..@? "eventInformation")
      Lude.<*> (x Lude..@? "timestamp")
