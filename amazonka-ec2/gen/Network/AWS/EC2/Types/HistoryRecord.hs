{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.HistoryRecord
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.HistoryRecord where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.EventInformation
import Network.AWS.EC2.Types.EventType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    -- | The date and time of the event, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
    timestamp :: Prelude.Maybe Prelude.ISO8601,
    -- | Information about the event.
    eventInformation :: Prelude.Maybe EventInformation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'timestamp', 'historyRecord_timestamp' - The date and time of the event, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
--
-- 'eventInformation', 'historyRecord_eventInformation' - Information about the event.
newHistoryRecord ::
  HistoryRecord
newHistoryRecord =
  HistoryRecord'
    { eventType = Prelude.Nothing,
      timestamp = Prelude.Nothing,
      eventInformation = Prelude.Nothing
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

-- | The date and time of the event, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
historyRecord_timestamp :: Lens.Lens' HistoryRecord (Prelude.Maybe Prelude.UTCTime)
historyRecord_timestamp = Lens.lens (\HistoryRecord' {timestamp} -> timestamp) (\s@HistoryRecord' {} a -> s {timestamp = a} :: HistoryRecord) Prelude.. Lens.mapping Prelude._Time

-- | Information about the event.
historyRecord_eventInformation :: Lens.Lens' HistoryRecord (Prelude.Maybe EventInformation)
historyRecord_eventInformation = Lens.lens (\HistoryRecord' {eventInformation} -> eventInformation) (\s@HistoryRecord' {} a -> s {eventInformation = a} :: HistoryRecord)

instance Prelude.FromXML HistoryRecord where
  parseXML x =
    HistoryRecord'
      Prelude.<$> (x Prelude..@? "eventType")
      Prelude.<*> (x Prelude..@? "timestamp")
      Prelude.<*> (x Prelude..@? "eventInformation")

instance Prelude.Hashable HistoryRecord

instance Prelude.NFData HistoryRecord
