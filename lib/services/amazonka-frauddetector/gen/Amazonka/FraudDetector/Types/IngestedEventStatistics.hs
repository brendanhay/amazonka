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
-- Module      : Amazonka.FraudDetector.Types.IngestedEventStatistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.IngestedEventStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Data about the stored events.
--
-- /See:/ 'newIngestedEventStatistics' smart constructor.
data IngestedEventStatistics = IngestedEventStatistics'
  { -- | The total size of the stored events.
    eventDataSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | Timestamp of when the stored event was last updated.
    lastUpdatedTime :: Prelude.Maybe Prelude.Text,
    -- | The oldest stored event.
    leastRecentEvent :: Prelude.Maybe Prelude.Text,
    -- | The newest stored event.
    mostRecentEvent :: Prelude.Maybe Prelude.Text,
    -- | The number of stored events.
    numberOfEvents :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IngestedEventStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventDataSizeInBytes', 'ingestedEventStatistics_eventDataSizeInBytes' - The total size of the stored events.
--
-- 'lastUpdatedTime', 'ingestedEventStatistics_lastUpdatedTime' - Timestamp of when the stored event was last updated.
--
-- 'leastRecentEvent', 'ingestedEventStatistics_leastRecentEvent' - The oldest stored event.
--
-- 'mostRecentEvent', 'ingestedEventStatistics_mostRecentEvent' - The newest stored event.
--
-- 'numberOfEvents', 'ingestedEventStatistics_numberOfEvents' - The number of stored events.
newIngestedEventStatistics ::
  IngestedEventStatistics
newIngestedEventStatistics =
  IngestedEventStatistics'
    { eventDataSizeInBytes =
        Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      leastRecentEvent = Prelude.Nothing,
      mostRecentEvent = Prelude.Nothing,
      numberOfEvents = Prelude.Nothing
    }

-- | The total size of the stored events.
ingestedEventStatistics_eventDataSizeInBytes :: Lens.Lens' IngestedEventStatistics (Prelude.Maybe Prelude.Integer)
ingestedEventStatistics_eventDataSizeInBytes = Lens.lens (\IngestedEventStatistics' {eventDataSizeInBytes} -> eventDataSizeInBytes) (\s@IngestedEventStatistics' {} a -> s {eventDataSizeInBytes = a} :: IngestedEventStatistics)

-- | Timestamp of when the stored event was last updated.
ingestedEventStatistics_lastUpdatedTime :: Lens.Lens' IngestedEventStatistics (Prelude.Maybe Prelude.Text)
ingestedEventStatistics_lastUpdatedTime = Lens.lens (\IngestedEventStatistics' {lastUpdatedTime} -> lastUpdatedTime) (\s@IngestedEventStatistics' {} a -> s {lastUpdatedTime = a} :: IngestedEventStatistics)

-- | The oldest stored event.
ingestedEventStatistics_leastRecentEvent :: Lens.Lens' IngestedEventStatistics (Prelude.Maybe Prelude.Text)
ingestedEventStatistics_leastRecentEvent = Lens.lens (\IngestedEventStatistics' {leastRecentEvent} -> leastRecentEvent) (\s@IngestedEventStatistics' {} a -> s {leastRecentEvent = a} :: IngestedEventStatistics)

-- | The newest stored event.
ingestedEventStatistics_mostRecentEvent :: Lens.Lens' IngestedEventStatistics (Prelude.Maybe Prelude.Text)
ingestedEventStatistics_mostRecentEvent = Lens.lens (\IngestedEventStatistics' {mostRecentEvent} -> mostRecentEvent) (\s@IngestedEventStatistics' {} a -> s {mostRecentEvent = a} :: IngestedEventStatistics)

-- | The number of stored events.
ingestedEventStatistics_numberOfEvents :: Lens.Lens' IngestedEventStatistics (Prelude.Maybe Prelude.Integer)
ingestedEventStatistics_numberOfEvents = Lens.lens (\IngestedEventStatistics' {numberOfEvents} -> numberOfEvents) (\s@IngestedEventStatistics' {} a -> s {numberOfEvents = a} :: IngestedEventStatistics)

instance Data.FromJSON IngestedEventStatistics where
  parseJSON =
    Data.withObject
      "IngestedEventStatistics"
      ( \x ->
          IngestedEventStatistics'
            Prelude.<$> (x Data..:? "eventDataSizeInBytes")
            Prelude.<*> (x Data..:? "lastUpdatedTime")
            Prelude.<*> (x Data..:? "leastRecentEvent")
            Prelude.<*> (x Data..:? "mostRecentEvent")
            Prelude.<*> (x Data..:? "numberOfEvents")
      )

instance Prelude.Hashable IngestedEventStatistics where
  hashWithSalt _salt IngestedEventStatistics' {..} =
    _salt
      `Prelude.hashWithSalt` eventDataSizeInBytes
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` leastRecentEvent
      `Prelude.hashWithSalt` mostRecentEvent
      `Prelude.hashWithSalt` numberOfEvents

instance Prelude.NFData IngestedEventStatistics where
  rnf IngestedEventStatistics' {..} =
    Prelude.rnf eventDataSizeInBytes `Prelude.seq`
      Prelude.rnf lastUpdatedTime `Prelude.seq`
        Prelude.rnf leastRecentEvent `Prelude.seq`
          Prelude.rnf mostRecentEvent `Prelude.seq`
            Prelude.rnf numberOfEvents
