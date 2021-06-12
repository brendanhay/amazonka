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
-- Module      : Network.AWS.CloudWatchLogs.Types.QueryStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.QueryStatistics where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the number of log events scanned by the query, the number of
-- log events that matched the query criteria, and the total number of
-- bytes in the log events that were scanned.
--
-- /See:/ 'newQueryStatistics' smart constructor.
data QueryStatistics = QueryStatistics'
  { -- | The total number of bytes in the log events scanned during the query.
    bytesScanned :: Core.Maybe Core.Double,
    -- | The number of log events that matched the query string.
    recordsMatched :: Core.Maybe Core.Double,
    -- | The total number of log events scanned during the query.
    recordsScanned :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { bytesScanned = Core.Nothing,
      recordsMatched = Core.Nothing,
      recordsScanned = Core.Nothing
    }

-- | The total number of bytes in the log events scanned during the query.
queryStatistics_bytesScanned :: Lens.Lens' QueryStatistics (Core.Maybe Core.Double)
queryStatistics_bytesScanned = Lens.lens (\QueryStatistics' {bytesScanned} -> bytesScanned) (\s@QueryStatistics' {} a -> s {bytesScanned = a} :: QueryStatistics)

-- | The number of log events that matched the query string.
queryStatistics_recordsMatched :: Lens.Lens' QueryStatistics (Core.Maybe Core.Double)
queryStatistics_recordsMatched = Lens.lens (\QueryStatistics' {recordsMatched} -> recordsMatched) (\s@QueryStatistics' {} a -> s {recordsMatched = a} :: QueryStatistics)

-- | The total number of log events scanned during the query.
queryStatistics_recordsScanned :: Lens.Lens' QueryStatistics (Core.Maybe Core.Double)
queryStatistics_recordsScanned = Lens.lens (\QueryStatistics' {recordsScanned} -> recordsScanned) (\s@QueryStatistics' {} a -> s {recordsScanned = a} :: QueryStatistics)

instance Core.FromJSON QueryStatistics where
  parseJSON =
    Core.withObject
      "QueryStatistics"
      ( \x ->
          QueryStatistics'
            Core.<$> (x Core..:? "bytesScanned")
            Core.<*> (x Core..:? "recordsMatched")
            Core.<*> (x Core..:? "recordsScanned")
      )

instance Core.Hashable QueryStatistics

instance Core.NFData QueryStatistics
