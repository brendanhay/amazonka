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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.IngestedEventStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Data about the stored events.
--
-- /See:/ 'newIngestedEventStatistics' smart constructor.
data IngestedEventStatistics = IngestedEventStatistics'
  { -- | The total size of the stored events.
    eventDataSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The newest stored event.
    mostRecentEvent :: Prelude.Maybe Prelude.Text,
    -- | Timestamp of when the stored event was last updated.
    lastUpdatedTime :: Prelude.Maybe Prelude.Text,
    -- | The number of stored events.
    numberOfEvents :: Prelude.Maybe Prelude.Integer,
    -- | The oldest stored event.
    leastRecentEvent :: Prelude.Maybe Prelude.Text
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
-- 'mostRecentEvent', 'ingestedEventStatistics_mostRecentEvent' - The newest stored event.
--
-- 'lastUpdatedTime', 'ingestedEventStatistics_lastUpdatedTime' - Timestamp of when the stored event was last updated.
--
-- 'numberOfEvents', 'ingestedEventStatistics_numberOfEvents' - The number of stored events.
--
-- 'leastRecentEvent', 'ingestedEventStatistics_leastRecentEvent' - The oldest stored event.
newIngestedEventStatistics ::
  IngestedEventStatistics
newIngestedEventStatistics =
  IngestedEventStatistics'
    { eventDataSizeInBytes =
        Prelude.Nothing,
      mostRecentEvent = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      numberOfEvents = Prelude.Nothing,
      leastRecentEvent = Prelude.Nothing
    }

-- | The total size of the stored events.
ingestedEventStatistics_eventDataSizeInBytes :: Lens.Lens' IngestedEventStatistics (Prelude.Maybe Prelude.Integer)
ingestedEventStatistics_eventDataSizeInBytes = Lens.lens (\IngestedEventStatistics' {eventDataSizeInBytes} -> eventDataSizeInBytes) (\s@IngestedEventStatistics' {} a -> s {eventDataSizeInBytes = a} :: IngestedEventStatistics)

-- | The newest stored event.
ingestedEventStatistics_mostRecentEvent :: Lens.Lens' IngestedEventStatistics (Prelude.Maybe Prelude.Text)
ingestedEventStatistics_mostRecentEvent = Lens.lens (\IngestedEventStatistics' {mostRecentEvent} -> mostRecentEvent) (\s@IngestedEventStatistics' {} a -> s {mostRecentEvent = a} :: IngestedEventStatistics)

-- | Timestamp of when the stored event was last updated.
ingestedEventStatistics_lastUpdatedTime :: Lens.Lens' IngestedEventStatistics (Prelude.Maybe Prelude.Text)
ingestedEventStatistics_lastUpdatedTime = Lens.lens (\IngestedEventStatistics' {lastUpdatedTime} -> lastUpdatedTime) (\s@IngestedEventStatistics' {} a -> s {lastUpdatedTime = a} :: IngestedEventStatistics)

-- | The number of stored events.
ingestedEventStatistics_numberOfEvents :: Lens.Lens' IngestedEventStatistics (Prelude.Maybe Prelude.Integer)
ingestedEventStatistics_numberOfEvents = Lens.lens (\IngestedEventStatistics' {numberOfEvents} -> numberOfEvents) (\s@IngestedEventStatistics' {} a -> s {numberOfEvents = a} :: IngestedEventStatistics)

-- | The oldest stored event.
ingestedEventStatistics_leastRecentEvent :: Lens.Lens' IngestedEventStatistics (Prelude.Maybe Prelude.Text)
ingestedEventStatistics_leastRecentEvent = Lens.lens (\IngestedEventStatistics' {leastRecentEvent} -> leastRecentEvent) (\s@IngestedEventStatistics' {} a -> s {leastRecentEvent = a} :: IngestedEventStatistics)

instance Core.FromJSON IngestedEventStatistics where
  parseJSON =
    Core.withObject
      "IngestedEventStatistics"
      ( \x ->
          IngestedEventStatistics'
            Prelude.<$> (x Core..:? "eventDataSizeInBytes")
            Prelude.<*> (x Core..:? "mostRecentEvent")
            Prelude.<*> (x Core..:? "lastUpdatedTime")
            Prelude.<*> (x Core..:? "numberOfEvents")
            Prelude.<*> (x Core..:? "leastRecentEvent")
      )

instance Prelude.Hashable IngestedEventStatistics

instance Prelude.NFData IngestedEventStatistics
