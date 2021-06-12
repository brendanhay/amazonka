{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.HistoryRecordEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.HistoryRecordEntry where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.EventInformation
import Network.AWS.EC2.Types.FleetEventType
import qualified Network.AWS.Lens as Lens

-- | Describes an event in the history of an EC2 Fleet.
--
-- /See:/ 'newHistoryRecordEntry' smart constructor.
data HistoryRecordEntry = HistoryRecordEntry'
  { -- | The event type.
    eventType :: Core.Maybe FleetEventType,
    -- | The date and time of the event, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
    timestamp :: Core.Maybe Core.ISO8601,
    -- | Information about the event.
    eventInformation :: Core.Maybe EventInformation
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HistoryRecordEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventType', 'historyRecordEntry_eventType' - The event type.
--
-- 'timestamp', 'historyRecordEntry_timestamp' - The date and time of the event, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
--
-- 'eventInformation', 'historyRecordEntry_eventInformation' - Information about the event.
newHistoryRecordEntry ::
  HistoryRecordEntry
newHistoryRecordEntry =
  HistoryRecordEntry'
    { eventType = Core.Nothing,
      timestamp = Core.Nothing,
      eventInformation = Core.Nothing
    }

-- | The event type.
historyRecordEntry_eventType :: Lens.Lens' HistoryRecordEntry (Core.Maybe FleetEventType)
historyRecordEntry_eventType = Lens.lens (\HistoryRecordEntry' {eventType} -> eventType) (\s@HistoryRecordEntry' {} a -> s {eventType = a} :: HistoryRecordEntry)

-- | The date and time of the event, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
historyRecordEntry_timestamp :: Lens.Lens' HistoryRecordEntry (Core.Maybe Core.UTCTime)
historyRecordEntry_timestamp = Lens.lens (\HistoryRecordEntry' {timestamp} -> timestamp) (\s@HistoryRecordEntry' {} a -> s {timestamp = a} :: HistoryRecordEntry) Core.. Lens.mapping Core._Time

-- | Information about the event.
historyRecordEntry_eventInformation :: Lens.Lens' HistoryRecordEntry (Core.Maybe EventInformation)
historyRecordEntry_eventInformation = Lens.lens (\HistoryRecordEntry' {eventInformation} -> eventInformation) (\s@HistoryRecordEntry' {} a -> s {eventInformation = a} :: HistoryRecordEntry)

instance Core.FromXML HistoryRecordEntry where
  parseXML x =
    HistoryRecordEntry'
      Core.<$> (x Core..@? "eventType")
      Core.<*> (x Core..@? "timestamp")
      Core.<*> (x Core..@? "eventInformation")

instance Core.Hashable HistoryRecordEntry

instance Core.NFData HistoryRecordEntry
