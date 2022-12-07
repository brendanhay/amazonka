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
-- Module      : Amazonka.EC2.Types.HistoryRecord
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.HistoryRecord where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.EventInformation
import Amazonka.EC2.Types.EventType
import qualified Amazonka.Prelude as Prelude

-- | Describes an event in the history of the Spot Fleet request.
--
-- /See:/ 'newHistoryRecord' smart constructor.
data HistoryRecord = HistoryRecord'
  { -- | The event type.
    --
    -- -   @error@ - An error with the Spot Fleet request.
    --
    -- -   @fleetRequestChange@ - A change in the status or configuration of
    --     the Spot Fleet request.
    --
    -- -   @instanceChange@ - An instance was launched or terminated.
    --
    -- -   @Information@ - An informational event.
    eventType :: Prelude.Maybe EventType,
    -- | Information about the event.
    eventInformation :: Prelude.Maybe EventInformation,
    -- | The date and time of the event, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
    timestamp :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HistoryRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventType', 'historyRecord_eventType' - The event type.
--
-- -   @error@ - An error with the Spot Fleet request.
--
-- -   @fleetRequestChange@ - A change in the status or configuration of
--     the Spot Fleet request.
--
-- -   @instanceChange@ - An instance was launched or terminated.
--
-- -   @Information@ - An informational event.
--
-- 'eventInformation', 'historyRecord_eventInformation' - Information about the event.
--
-- 'timestamp', 'historyRecord_timestamp' - The date and time of the event, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
newHistoryRecord ::
  HistoryRecord
newHistoryRecord =
  HistoryRecord'
    { eventType = Prelude.Nothing,
      eventInformation = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | The event type.
--
-- -   @error@ - An error with the Spot Fleet request.
--
-- -   @fleetRequestChange@ - A change in the status or configuration of
--     the Spot Fleet request.
--
-- -   @instanceChange@ - An instance was launched or terminated.
--
-- -   @Information@ - An informational event.
historyRecord_eventType :: Lens.Lens' HistoryRecord (Prelude.Maybe EventType)
historyRecord_eventType = Lens.lens (\HistoryRecord' {eventType} -> eventType) (\s@HistoryRecord' {} a -> s {eventType = a} :: HistoryRecord)

-- | Information about the event.
historyRecord_eventInformation :: Lens.Lens' HistoryRecord (Prelude.Maybe EventInformation)
historyRecord_eventInformation = Lens.lens (\HistoryRecord' {eventInformation} -> eventInformation) (\s@HistoryRecord' {} a -> s {eventInformation = a} :: HistoryRecord)

-- | The date and time of the event, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
historyRecord_timestamp :: Lens.Lens' HistoryRecord (Prelude.Maybe Prelude.UTCTime)
historyRecord_timestamp = Lens.lens (\HistoryRecord' {timestamp} -> timestamp) (\s@HistoryRecord' {} a -> s {timestamp = a} :: HistoryRecord) Prelude.. Lens.mapping Data._Time

instance Data.FromXML HistoryRecord where
  parseXML x =
    HistoryRecord'
      Prelude.<$> (x Data..@? "eventType")
      Prelude.<*> (x Data..@? "eventInformation")
      Prelude.<*> (x Data..@? "timestamp")

instance Prelude.Hashable HistoryRecord where
  hashWithSalt _salt HistoryRecord' {..} =
    _salt `Prelude.hashWithSalt` eventType
      `Prelude.hashWithSalt` eventInformation
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData HistoryRecord where
  rnf HistoryRecord' {..} =
    Prelude.rnf eventType
      `Prelude.seq` Prelude.rnf eventInformation
      `Prelude.seq` Prelude.rnf timestamp
