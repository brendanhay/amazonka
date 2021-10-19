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
import qualified Network.AWS.Prelude as Prelude

-- | Describes an event in the history of an EC2 Fleet.
--
-- /See:/ 'newHistoryRecordEntry' smart constructor.
data HistoryRecordEntry = HistoryRecordEntry'
  { -- | The event type.
    eventType :: Prelude.Maybe FleetEventType,
    -- | Information about the event.
    eventInformation :: Prelude.Maybe EventInformation,
    -- | The date and time of the event, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
    timestamp :: Prelude.Maybe Core.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'eventInformation', 'historyRecordEntry_eventInformation' - Information about the event.
--
-- 'timestamp', 'historyRecordEntry_timestamp' - The date and time of the event, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
newHistoryRecordEntry ::
  HistoryRecordEntry
newHistoryRecordEntry =
  HistoryRecordEntry'
    { eventType = Prelude.Nothing,
      eventInformation = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | The event type.
historyRecordEntry_eventType :: Lens.Lens' HistoryRecordEntry (Prelude.Maybe FleetEventType)
historyRecordEntry_eventType = Lens.lens (\HistoryRecordEntry' {eventType} -> eventType) (\s@HistoryRecordEntry' {} a -> s {eventType = a} :: HistoryRecordEntry)

-- | Information about the event.
historyRecordEntry_eventInformation :: Lens.Lens' HistoryRecordEntry (Prelude.Maybe EventInformation)
historyRecordEntry_eventInformation = Lens.lens (\HistoryRecordEntry' {eventInformation} -> eventInformation) (\s@HistoryRecordEntry' {} a -> s {eventInformation = a} :: HistoryRecordEntry)

-- | The date and time of the event, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
historyRecordEntry_timestamp :: Lens.Lens' HistoryRecordEntry (Prelude.Maybe Prelude.UTCTime)
historyRecordEntry_timestamp = Lens.lens (\HistoryRecordEntry' {timestamp} -> timestamp) (\s@HistoryRecordEntry' {} a -> s {timestamp = a} :: HistoryRecordEntry) Prelude.. Lens.mapping Core._Time

instance Core.FromXML HistoryRecordEntry where
  parseXML x =
    HistoryRecordEntry'
      Prelude.<$> (x Core..@? "eventType")
      Prelude.<*> (x Core..@? "eventInformation")
      Prelude.<*> (x Core..@? "timestamp")

instance Prelude.Hashable HistoryRecordEntry

instance Prelude.NFData HistoryRecordEntry
