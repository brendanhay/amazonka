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
-- Module      : Amazonka.CloudWatchLogs.Types.QueryStatistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchLogs.Types.QueryStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the number of log events scanned by the query, the number of
-- log events that matched the query criteria, and the total number of
-- bytes in the log events that were scanned.
--
-- /See:/ 'newQueryStatistics' smart constructor.
data QueryStatistics = QueryStatistics'
  { -- | The total number of bytes in the log events scanned during the query.
    bytesScanned :: Prelude.Maybe Prelude.Double,
    -- | The number of log events that matched the query string.
    recordsMatched :: Prelude.Maybe Prelude.Double,
    -- | The total number of log events scanned during the query.
    recordsScanned :: Prelude.Maybe Prelude.Double
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
-- 'bytesScanned', 'queryStatistics_bytesScanned' - The total number of bytes in the log events scanned during the query.
--
-- 'recordsMatched', 'queryStatistics_recordsMatched' - The number of log events that matched the query string.
--
-- 'recordsScanned', 'queryStatistics_recordsScanned' - The total number of log events scanned during the query.
newQueryStatistics ::
  QueryStatistics
newQueryStatistics =
  QueryStatistics'
    { bytesScanned = Prelude.Nothing,
      recordsMatched = Prelude.Nothing,
      recordsScanned = Prelude.Nothing
    }

-- | The total number of bytes in the log events scanned during the query.
queryStatistics_bytesScanned :: Lens.Lens' QueryStatistics (Prelude.Maybe Prelude.Double)
queryStatistics_bytesScanned = Lens.lens (\QueryStatistics' {bytesScanned} -> bytesScanned) (\s@QueryStatistics' {} a -> s {bytesScanned = a} :: QueryStatistics)

-- | The number of log events that matched the query string.
queryStatistics_recordsMatched :: Lens.Lens' QueryStatistics (Prelude.Maybe Prelude.Double)
queryStatistics_recordsMatched = Lens.lens (\QueryStatistics' {recordsMatched} -> recordsMatched) (\s@QueryStatistics' {} a -> s {recordsMatched = a} :: QueryStatistics)

-- | The total number of log events scanned during the query.
queryStatistics_recordsScanned :: Lens.Lens' QueryStatistics (Prelude.Maybe Prelude.Double)
queryStatistics_recordsScanned = Lens.lens (\QueryStatistics' {recordsScanned} -> recordsScanned) (\s@QueryStatistics' {} a -> s {recordsScanned = a} :: QueryStatistics)

instance Data.FromJSON QueryStatistics where
  parseJSON =
    Data.withObject
      "QueryStatistics"
      ( \x ->
          QueryStatistics'
            Prelude.<$> (x Data..:? "bytesScanned")
            Prelude.<*> (x Data..:? "recordsMatched")
            Prelude.<*> (x Data..:? "recordsScanned")
      )

instance Prelude.Hashable QueryStatistics where
  hashWithSalt _salt QueryStatistics' {..} =
    _salt `Prelude.hashWithSalt` bytesScanned
      `Prelude.hashWithSalt` recordsMatched
      `Prelude.hashWithSalt` recordsScanned

instance Prelude.NFData QueryStatistics where
  rnf QueryStatistics' {..} =
    Prelude.rnf bytesScanned
      `Prelude.seq` Prelude.rnf recordsMatched
      `Prelude.seq` Prelude.rnf recordsScanned
