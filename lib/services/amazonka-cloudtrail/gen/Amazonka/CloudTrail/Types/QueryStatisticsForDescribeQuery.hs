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
-- Module      : Amazonka.CloudTrail.Types.QueryStatisticsForDescribeQuery
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Types.QueryStatisticsForDescribeQuery where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Gets metadata about a query, including the number of events that were
-- matched, the total number of events scanned, the query run time in
-- milliseconds, and the query\'s creation time.
--
-- /See:/ 'newQueryStatisticsForDescribeQuery' smart constructor.
data QueryStatisticsForDescribeQuery = QueryStatisticsForDescribeQuery'
  { -- | The number of events that the query scanned in the event data store.
    eventsScanned :: Prelude.Maybe Prelude.Integer,
    -- | The total bytes that the query scanned in the event data store. This
    -- value matches the number of bytes for which your account is billed for
    -- the query, unless the query is still running.
    bytesScanned :: Prelude.Maybe Prelude.Integer,
    -- | The query\'s run time, in milliseconds.
    executionTimeInMillis :: Prelude.Maybe Prelude.Int,
    -- | The number of events that matched a query.
    eventsMatched :: Prelude.Maybe Prelude.Integer,
    -- | The creation time of the query.
    creationTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryStatisticsForDescribeQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventsScanned', 'queryStatisticsForDescribeQuery_eventsScanned' - The number of events that the query scanned in the event data store.
--
-- 'bytesScanned', 'queryStatisticsForDescribeQuery_bytesScanned' - The total bytes that the query scanned in the event data store. This
-- value matches the number of bytes for which your account is billed for
-- the query, unless the query is still running.
--
-- 'executionTimeInMillis', 'queryStatisticsForDescribeQuery_executionTimeInMillis' - The query\'s run time, in milliseconds.
--
-- 'eventsMatched', 'queryStatisticsForDescribeQuery_eventsMatched' - The number of events that matched a query.
--
-- 'creationTime', 'queryStatisticsForDescribeQuery_creationTime' - The creation time of the query.
newQueryStatisticsForDescribeQuery ::
  QueryStatisticsForDescribeQuery
newQueryStatisticsForDescribeQuery =
  QueryStatisticsForDescribeQuery'
    { eventsScanned =
        Prelude.Nothing,
      bytesScanned = Prelude.Nothing,
      executionTimeInMillis = Prelude.Nothing,
      eventsMatched = Prelude.Nothing,
      creationTime = Prelude.Nothing
    }

-- | The number of events that the query scanned in the event data store.
queryStatisticsForDescribeQuery_eventsScanned :: Lens.Lens' QueryStatisticsForDescribeQuery (Prelude.Maybe Prelude.Integer)
queryStatisticsForDescribeQuery_eventsScanned = Lens.lens (\QueryStatisticsForDescribeQuery' {eventsScanned} -> eventsScanned) (\s@QueryStatisticsForDescribeQuery' {} a -> s {eventsScanned = a} :: QueryStatisticsForDescribeQuery)

-- | The total bytes that the query scanned in the event data store. This
-- value matches the number of bytes for which your account is billed for
-- the query, unless the query is still running.
queryStatisticsForDescribeQuery_bytesScanned :: Lens.Lens' QueryStatisticsForDescribeQuery (Prelude.Maybe Prelude.Integer)
queryStatisticsForDescribeQuery_bytesScanned = Lens.lens (\QueryStatisticsForDescribeQuery' {bytesScanned} -> bytesScanned) (\s@QueryStatisticsForDescribeQuery' {} a -> s {bytesScanned = a} :: QueryStatisticsForDescribeQuery)

-- | The query\'s run time, in milliseconds.
queryStatisticsForDescribeQuery_executionTimeInMillis :: Lens.Lens' QueryStatisticsForDescribeQuery (Prelude.Maybe Prelude.Int)
queryStatisticsForDescribeQuery_executionTimeInMillis = Lens.lens (\QueryStatisticsForDescribeQuery' {executionTimeInMillis} -> executionTimeInMillis) (\s@QueryStatisticsForDescribeQuery' {} a -> s {executionTimeInMillis = a} :: QueryStatisticsForDescribeQuery)

-- | The number of events that matched a query.
queryStatisticsForDescribeQuery_eventsMatched :: Lens.Lens' QueryStatisticsForDescribeQuery (Prelude.Maybe Prelude.Integer)
queryStatisticsForDescribeQuery_eventsMatched = Lens.lens (\QueryStatisticsForDescribeQuery' {eventsMatched} -> eventsMatched) (\s@QueryStatisticsForDescribeQuery' {} a -> s {eventsMatched = a} :: QueryStatisticsForDescribeQuery)

-- | The creation time of the query.
queryStatisticsForDescribeQuery_creationTime :: Lens.Lens' QueryStatisticsForDescribeQuery (Prelude.Maybe Prelude.UTCTime)
queryStatisticsForDescribeQuery_creationTime = Lens.lens (\QueryStatisticsForDescribeQuery' {creationTime} -> creationTime) (\s@QueryStatisticsForDescribeQuery' {} a -> s {creationTime = a} :: QueryStatisticsForDescribeQuery) Prelude.. Lens.mapping Data._Time

instance
  Data.FromJSON
    QueryStatisticsForDescribeQuery
  where
  parseJSON =
    Data.withObject
      "QueryStatisticsForDescribeQuery"
      ( \x ->
          QueryStatisticsForDescribeQuery'
            Prelude.<$> (x Data..:? "EventsScanned")
            Prelude.<*> (x Data..:? "BytesScanned")
            Prelude.<*> (x Data..:? "ExecutionTimeInMillis")
            Prelude.<*> (x Data..:? "EventsMatched")
            Prelude.<*> (x Data..:? "CreationTime")
      )

instance
  Prelude.Hashable
    QueryStatisticsForDescribeQuery
  where
  hashWithSalt
    _salt
    QueryStatisticsForDescribeQuery' {..} =
      _salt `Prelude.hashWithSalt` eventsScanned
        `Prelude.hashWithSalt` bytesScanned
        `Prelude.hashWithSalt` executionTimeInMillis
        `Prelude.hashWithSalt` eventsMatched
        `Prelude.hashWithSalt` creationTime

instance
  Prelude.NFData
    QueryStatisticsForDescribeQuery
  where
  rnf QueryStatisticsForDescribeQuery' {..} =
    Prelude.rnf eventsScanned
      `Prelude.seq` Prelude.rnf bytesScanned
      `Prelude.seq` Prelude.rnf executionTimeInMillis
      `Prelude.seq` Prelude.rnf eventsMatched
      `Prelude.seq` Prelude.rnf creationTime
