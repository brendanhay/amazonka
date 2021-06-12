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
-- Module      : Network.AWS.Athena.Types.QueryExecutionStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.QueryExecutionStatistics where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The amount of data scanned during the query execution and the amount of
-- time that it took to execute, and the type of statement that was run.
--
-- /See:/ 'newQueryExecutionStatistics' smart constructor.
data QueryExecutionStatistics = QueryExecutionStatistics'
  { -- | The number of milliseconds that Athena took to run the query.
    totalExecutionTimeInMillis :: Core.Maybe Core.Integer,
    -- | The number of milliseconds that Athena took to finalize and publish the
    -- query results after the query engine finished running the query.
    serviceProcessingTimeInMillis :: Core.Maybe Core.Integer,
    -- | The number of milliseconds that the query was in your query queue
    -- waiting for resources. Note that if transient errors occur, Athena might
    -- automatically add the query back to the queue.
    queryQueueTimeInMillis :: Core.Maybe Core.Integer,
    -- | The number of bytes in the data that was queried.
    dataScannedInBytes :: Core.Maybe Core.Integer,
    -- | The number of milliseconds that Athena took to plan the query processing
    -- flow. This includes the time spent retrieving table partitions from the
    -- data source. Note that because the query engine performs the query
    -- planning, query planning time is a subset of engine processing time.
    queryPlanningTimeInMillis :: Core.Maybe Core.Integer,
    -- | The number of milliseconds that the query took to execute.
    engineExecutionTimeInMillis :: Core.Maybe Core.Integer,
    -- | The location and file name of a data manifest file. The manifest file is
    -- saved to the Athena query results location in Amazon S3. The manifest
    -- file tracks files that the query wrote to Amazon S3. If the query fails,
    -- the manifest file also tracks files that the query intended to write.
    -- The manifest is useful for identifying orphaned files resulting from a
    -- failed query. For more information, see
    -- <https://docs.aws.amazon.com/athena/latest/ug/querying.html Working with Query Results, Output Files, and Query History>
    -- in the /Amazon Athena User Guide/.
    dataManifestLocation :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'QueryExecutionStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalExecutionTimeInMillis', 'queryExecutionStatistics_totalExecutionTimeInMillis' - The number of milliseconds that Athena took to run the query.
--
-- 'serviceProcessingTimeInMillis', 'queryExecutionStatistics_serviceProcessingTimeInMillis' - The number of milliseconds that Athena took to finalize and publish the
-- query results after the query engine finished running the query.
--
-- 'queryQueueTimeInMillis', 'queryExecutionStatistics_queryQueueTimeInMillis' - The number of milliseconds that the query was in your query queue
-- waiting for resources. Note that if transient errors occur, Athena might
-- automatically add the query back to the queue.
--
-- 'dataScannedInBytes', 'queryExecutionStatistics_dataScannedInBytes' - The number of bytes in the data that was queried.
--
-- 'queryPlanningTimeInMillis', 'queryExecutionStatistics_queryPlanningTimeInMillis' - The number of milliseconds that Athena took to plan the query processing
-- flow. This includes the time spent retrieving table partitions from the
-- data source. Note that because the query engine performs the query
-- planning, query planning time is a subset of engine processing time.
--
-- 'engineExecutionTimeInMillis', 'queryExecutionStatistics_engineExecutionTimeInMillis' - The number of milliseconds that the query took to execute.
--
-- 'dataManifestLocation', 'queryExecutionStatistics_dataManifestLocation' - The location and file name of a data manifest file. The manifest file is
-- saved to the Athena query results location in Amazon S3. The manifest
-- file tracks files that the query wrote to Amazon S3. If the query fails,
-- the manifest file also tracks files that the query intended to write.
-- The manifest is useful for identifying orphaned files resulting from a
-- failed query. For more information, see
-- <https://docs.aws.amazon.com/athena/latest/ug/querying.html Working with Query Results, Output Files, and Query History>
-- in the /Amazon Athena User Guide/.
newQueryExecutionStatistics ::
  QueryExecutionStatistics
newQueryExecutionStatistics =
  QueryExecutionStatistics'
    { totalExecutionTimeInMillis =
        Core.Nothing,
      serviceProcessingTimeInMillis = Core.Nothing,
      queryQueueTimeInMillis = Core.Nothing,
      dataScannedInBytes = Core.Nothing,
      queryPlanningTimeInMillis = Core.Nothing,
      engineExecutionTimeInMillis = Core.Nothing,
      dataManifestLocation = Core.Nothing
    }

-- | The number of milliseconds that Athena took to run the query.
queryExecutionStatistics_totalExecutionTimeInMillis :: Lens.Lens' QueryExecutionStatistics (Core.Maybe Core.Integer)
queryExecutionStatistics_totalExecutionTimeInMillis = Lens.lens (\QueryExecutionStatistics' {totalExecutionTimeInMillis} -> totalExecutionTimeInMillis) (\s@QueryExecutionStatistics' {} a -> s {totalExecutionTimeInMillis = a} :: QueryExecutionStatistics)

-- | The number of milliseconds that Athena took to finalize and publish the
-- query results after the query engine finished running the query.
queryExecutionStatistics_serviceProcessingTimeInMillis :: Lens.Lens' QueryExecutionStatistics (Core.Maybe Core.Integer)
queryExecutionStatistics_serviceProcessingTimeInMillis = Lens.lens (\QueryExecutionStatistics' {serviceProcessingTimeInMillis} -> serviceProcessingTimeInMillis) (\s@QueryExecutionStatistics' {} a -> s {serviceProcessingTimeInMillis = a} :: QueryExecutionStatistics)

-- | The number of milliseconds that the query was in your query queue
-- waiting for resources. Note that if transient errors occur, Athena might
-- automatically add the query back to the queue.
queryExecutionStatistics_queryQueueTimeInMillis :: Lens.Lens' QueryExecutionStatistics (Core.Maybe Core.Integer)
queryExecutionStatistics_queryQueueTimeInMillis = Lens.lens (\QueryExecutionStatistics' {queryQueueTimeInMillis} -> queryQueueTimeInMillis) (\s@QueryExecutionStatistics' {} a -> s {queryQueueTimeInMillis = a} :: QueryExecutionStatistics)

-- | The number of bytes in the data that was queried.
queryExecutionStatistics_dataScannedInBytes :: Lens.Lens' QueryExecutionStatistics (Core.Maybe Core.Integer)
queryExecutionStatistics_dataScannedInBytes = Lens.lens (\QueryExecutionStatistics' {dataScannedInBytes} -> dataScannedInBytes) (\s@QueryExecutionStatistics' {} a -> s {dataScannedInBytes = a} :: QueryExecutionStatistics)

-- | The number of milliseconds that Athena took to plan the query processing
-- flow. This includes the time spent retrieving table partitions from the
-- data source. Note that because the query engine performs the query
-- planning, query planning time is a subset of engine processing time.
queryExecutionStatistics_queryPlanningTimeInMillis :: Lens.Lens' QueryExecutionStatistics (Core.Maybe Core.Integer)
queryExecutionStatistics_queryPlanningTimeInMillis = Lens.lens (\QueryExecutionStatistics' {queryPlanningTimeInMillis} -> queryPlanningTimeInMillis) (\s@QueryExecutionStatistics' {} a -> s {queryPlanningTimeInMillis = a} :: QueryExecutionStatistics)

-- | The number of milliseconds that the query took to execute.
queryExecutionStatistics_engineExecutionTimeInMillis :: Lens.Lens' QueryExecutionStatistics (Core.Maybe Core.Integer)
queryExecutionStatistics_engineExecutionTimeInMillis = Lens.lens (\QueryExecutionStatistics' {engineExecutionTimeInMillis} -> engineExecutionTimeInMillis) (\s@QueryExecutionStatistics' {} a -> s {engineExecutionTimeInMillis = a} :: QueryExecutionStatistics)

-- | The location and file name of a data manifest file. The manifest file is
-- saved to the Athena query results location in Amazon S3. The manifest
-- file tracks files that the query wrote to Amazon S3. If the query fails,
-- the manifest file also tracks files that the query intended to write.
-- The manifest is useful for identifying orphaned files resulting from a
-- failed query. For more information, see
-- <https://docs.aws.amazon.com/athena/latest/ug/querying.html Working with Query Results, Output Files, and Query History>
-- in the /Amazon Athena User Guide/.
queryExecutionStatistics_dataManifestLocation :: Lens.Lens' QueryExecutionStatistics (Core.Maybe Core.Text)
queryExecutionStatistics_dataManifestLocation = Lens.lens (\QueryExecutionStatistics' {dataManifestLocation} -> dataManifestLocation) (\s@QueryExecutionStatistics' {} a -> s {dataManifestLocation = a} :: QueryExecutionStatistics)

instance Core.FromJSON QueryExecutionStatistics where
  parseJSON =
    Core.withObject
      "QueryExecutionStatistics"
      ( \x ->
          QueryExecutionStatistics'
            Core.<$> (x Core..:? "TotalExecutionTimeInMillis")
            Core.<*> (x Core..:? "ServiceProcessingTimeInMillis")
            Core.<*> (x Core..:? "QueryQueueTimeInMillis")
            Core.<*> (x Core..:? "DataScannedInBytes")
            Core.<*> (x Core..:? "QueryPlanningTimeInMillis")
            Core.<*> (x Core..:? "EngineExecutionTimeInMillis")
            Core.<*> (x Core..:? "DataManifestLocation")
      )

instance Core.Hashable QueryExecutionStatistics

instance Core.NFData QueryExecutionStatistics
