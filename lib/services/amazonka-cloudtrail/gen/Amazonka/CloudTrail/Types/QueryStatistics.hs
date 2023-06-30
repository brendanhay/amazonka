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
-- Module      : Amazonka.CloudTrail.Types.QueryStatistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Types.QueryStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Metadata about a query, such as the number of results.
--
-- /See:/ 'newQueryStatistics' smart constructor.
data QueryStatistics = QueryStatistics'
  { -- | The total bytes that the query scanned in the event data store. This
    -- value matches the number of bytes for which your account is billed for
    -- the query, unless the query is still running.
    bytesScanned :: Prelude.Maybe Prelude.Integer,
    -- | The number of results returned.
    resultsCount :: Prelude.Maybe Prelude.Int,
    -- | The total number of results returned by a query.
    totalResultsCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bytesScanned', 'queryStatistics_bytesScanned' - The total bytes that the query scanned in the event data store. This
-- value matches the number of bytes for which your account is billed for
-- the query, unless the query is still running.
--
-- 'resultsCount', 'queryStatistics_resultsCount' - The number of results returned.
--
-- 'totalResultsCount', 'queryStatistics_totalResultsCount' - The total number of results returned by a query.
newQueryStatistics ::
  QueryStatistics
newQueryStatistics =
  QueryStatistics'
    { bytesScanned = Prelude.Nothing,
      resultsCount = Prelude.Nothing,
      totalResultsCount = Prelude.Nothing
    }

-- | The total bytes that the query scanned in the event data store. This
-- value matches the number of bytes for which your account is billed for
-- the query, unless the query is still running.
queryStatistics_bytesScanned :: Lens.Lens' QueryStatistics (Prelude.Maybe Prelude.Integer)
queryStatistics_bytesScanned = Lens.lens (\QueryStatistics' {bytesScanned} -> bytesScanned) (\s@QueryStatistics' {} a -> s {bytesScanned = a} :: QueryStatistics)

-- | The number of results returned.
queryStatistics_resultsCount :: Lens.Lens' QueryStatistics (Prelude.Maybe Prelude.Int)
queryStatistics_resultsCount = Lens.lens (\QueryStatistics' {resultsCount} -> resultsCount) (\s@QueryStatistics' {} a -> s {resultsCount = a} :: QueryStatistics)

-- | The total number of results returned by a query.
queryStatistics_totalResultsCount :: Lens.Lens' QueryStatistics (Prelude.Maybe Prelude.Int)
queryStatistics_totalResultsCount = Lens.lens (\QueryStatistics' {totalResultsCount} -> totalResultsCount) (\s@QueryStatistics' {} a -> s {totalResultsCount = a} :: QueryStatistics)

instance Data.FromJSON QueryStatistics where
  parseJSON =
    Data.withObject
      "QueryStatistics"
      ( \x ->
          QueryStatistics'
            Prelude.<$> (x Data..:? "BytesScanned")
            Prelude.<*> (x Data..:? "ResultsCount")
            Prelude.<*> (x Data..:? "TotalResultsCount")
      )

instance Prelude.Hashable QueryStatistics where
  hashWithSalt _salt QueryStatistics' {..} =
    _salt
      `Prelude.hashWithSalt` bytesScanned
      `Prelude.hashWithSalt` resultsCount
      `Prelude.hashWithSalt` totalResultsCount

instance Prelude.NFData QueryStatistics where
  rnf QueryStatistics' {..} =
    Prelude.rnf bytesScanned
      `Prelude.seq` Prelude.rnf resultsCount
      `Prelude.seq` Prelude.rnf totalResultsCount
