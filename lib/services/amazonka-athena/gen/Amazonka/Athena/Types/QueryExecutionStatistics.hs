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
-- Module      : Amazonka.Athena.Types.QueryExecutionStatistics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.QueryExecutionStatistics where

import Amazonka.Athena.Types.ResultReuseInformation
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The amount of data scanned during the query execution and the amount of
-- time that it took to execute, and the type of statement that was run.
--
-- /See:/ 'newQueryExecutionStatistics' smart constructor.
data QueryExecutionStatistics = QueryExecutionStatistics'
  { -- | The number of bytes in the data that was queried.
    dataScannedInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The number of milliseconds that the query was in your query queue
    -- waiting for resources. Note that if transient errors occur, Athena might
    -- automatically add the query back to the queue.
    queryQueueTimeInMillis :: Prelude.Maybe Prelude.Integer,
    -- | Contains information about whether previous query results were reused
    -- for the query.
    resultReuseInformation :: Prelude.Maybe ResultReuseInformation,
    -- | The number of milliseconds that Athena took to finalize and publish the
    -- query results after the query engine finished running the query.
    serviceProcessingTimeInMillis :: Prelude.Maybe Prelude.Integer,
    -- | The location and file name of a data manifest file. The manifest file is
    -- saved to the Athena query results location in Amazon S3. The manifest
    -- file tracks files that the query wrote to Amazon S3. If the query fails,
    -- the manifest file also tracks files that the query intended to write.
    -- The manifest is useful for identifying orphaned files resulting from a
    -- failed query. For more information, see
    -- <https://docs.aws.amazon.com/athena/latest/ug/querying.html Working with Query Results, Output Files, and Query History>
    -- in the /Amazon Athena User Guide/.
    dataManifestLocation :: Prelude.Maybe Prelude.Text,
    -- | The number of milliseconds that Athena took to run the query.
    totalExecutionTimeInMillis :: Prelude.Maybe Prelude.Integer,
    -- | The number of milliseconds that the query took to execute.
    engineExecutionTimeInMillis :: Prelude.Maybe Prelude.Integer,
    -- | The number of milliseconds that Athena took to plan the query processing
    -- flow. This includes the time spent retrieving table partitions from the
    -- data source. Note that because the query engine performs the query
    -- planning, query planning time is a subset of engine processing time.
    queryPlanningTimeInMillis :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryExecutionStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataScannedInBytes', 'queryExecutionStatistics_dataScannedInBytes' - The number of bytes in the data that was queried.
--
-- 'queryQueueTimeInMillis', 'queryExecutionStatistics_queryQueueTimeInMillis' - The number of milliseconds that the query was in your query queue
-- waiting for resources. Note that if transient errors occur, Athena might
-- automatically add the query back to the queue.
--
-- 'resultReuseInformation', 'queryExecutionStatistics_resultReuseInformation' - Contains information about whether previous query results were reused
-- for the query.
--
-- 'serviceProcessingTimeInMillis', 'queryExecutionStatistics_serviceProcessingTimeInMillis' - The number of milliseconds that Athena took to finalize and publish the
-- query results after the query engine finished running the query.
--
-- 'dataManifestLocation', 'queryExecutionStatistics_dataManifestLocation' - The location and file name of a data manifest file. The manifest file is
-- saved to the Athena query results location in Amazon S3. The manifest
-- file tracks files that the query wrote to Amazon S3. If the query fails,
-- the manifest file also tracks files that the query intended to write.
-- The manifest is useful for identifying orphaned files resulting from a
-- failed query. For more information, see
-- <https://docs.aws.amazon.com/athena/latest/ug/querying.html Working with Query Results, Output Files, and Query History>
-- in the /Amazon Athena User Guide/.
--
-- 'totalExecutionTimeInMillis', 'queryExecutionStatistics_totalExecutionTimeInMillis' - The number of milliseconds that Athena took to run the query.
--
-- 'engineExecutionTimeInMillis', 'queryExecutionStatistics_engineExecutionTimeInMillis' - The number of milliseconds that the query took to execute.
--
-- 'queryPlanningTimeInMillis', 'queryExecutionStatistics_queryPlanningTimeInMillis' - The number of milliseconds that Athena took to plan the query processing
-- flow. This includes the time spent retrieving table partitions from the
-- data source. Note that because the query engine performs the query
-- planning, query planning time is a subset of engine processing time.
newQueryExecutionStatistics ::
  QueryExecutionStatistics
newQueryExecutionStatistics =
  QueryExecutionStatistics'
    { dataScannedInBytes =
        Prelude.Nothing,
      queryQueueTimeInMillis = Prelude.Nothing,
      resultReuseInformation = Prelude.Nothing,
      serviceProcessingTimeInMillis = Prelude.Nothing,
      dataManifestLocation = Prelude.Nothing,
      totalExecutionTimeInMillis = Prelude.Nothing,
      engineExecutionTimeInMillis = Prelude.Nothing,
      queryPlanningTimeInMillis = Prelude.Nothing
    }

-- | The number of bytes in the data that was queried.
queryExecutionStatistics_dataScannedInBytes :: Lens.Lens' QueryExecutionStatistics (Prelude.Maybe Prelude.Integer)
queryExecutionStatistics_dataScannedInBytes = Lens.lens (\QueryExecutionStatistics' {dataScannedInBytes} -> dataScannedInBytes) (\s@QueryExecutionStatistics' {} a -> s {dataScannedInBytes = a} :: QueryExecutionStatistics)

-- | The number of milliseconds that the query was in your query queue
-- waiting for resources. Note that if transient errors occur, Athena might
-- automatically add the query back to the queue.
queryExecutionStatistics_queryQueueTimeInMillis :: Lens.Lens' QueryExecutionStatistics (Prelude.Maybe Prelude.Integer)
queryExecutionStatistics_queryQueueTimeInMillis = Lens.lens (\QueryExecutionStatistics' {queryQueueTimeInMillis} -> queryQueueTimeInMillis) (\s@QueryExecutionStatistics' {} a -> s {queryQueueTimeInMillis = a} :: QueryExecutionStatistics)

-- | Contains information about whether previous query results were reused
-- for the query.
queryExecutionStatistics_resultReuseInformation :: Lens.Lens' QueryExecutionStatistics (Prelude.Maybe ResultReuseInformation)
queryExecutionStatistics_resultReuseInformation = Lens.lens (\QueryExecutionStatistics' {resultReuseInformation} -> resultReuseInformation) (\s@QueryExecutionStatistics' {} a -> s {resultReuseInformation = a} :: QueryExecutionStatistics)

-- | The number of milliseconds that Athena took to finalize and publish the
-- query results after the query engine finished running the query.
queryExecutionStatistics_serviceProcessingTimeInMillis :: Lens.Lens' QueryExecutionStatistics (Prelude.Maybe Prelude.Integer)
queryExecutionStatistics_serviceProcessingTimeInMillis = Lens.lens (\QueryExecutionStatistics' {serviceProcessingTimeInMillis} -> serviceProcessingTimeInMillis) (\s@QueryExecutionStatistics' {} a -> s {serviceProcessingTimeInMillis = a} :: QueryExecutionStatistics)

-- | The location and file name of a data manifest file. The manifest file is
-- saved to the Athena query results location in Amazon S3. The manifest
-- file tracks files that the query wrote to Amazon S3. If the query fails,
-- the manifest file also tracks files that the query intended to write.
-- The manifest is useful for identifying orphaned files resulting from a
-- failed query. For more information, see
-- <https://docs.aws.amazon.com/athena/latest/ug/querying.html Working with Query Results, Output Files, and Query History>
-- in the /Amazon Athena User Guide/.
queryExecutionStatistics_dataManifestLocation :: Lens.Lens' QueryExecutionStatistics (Prelude.Maybe Prelude.Text)
queryExecutionStatistics_dataManifestLocation = Lens.lens (\QueryExecutionStatistics' {dataManifestLocation} -> dataManifestLocation) (\s@QueryExecutionStatistics' {} a -> s {dataManifestLocation = a} :: QueryExecutionStatistics)

-- | The number of milliseconds that Athena took to run the query.
queryExecutionStatistics_totalExecutionTimeInMillis :: Lens.Lens' QueryExecutionStatistics (Prelude.Maybe Prelude.Integer)
queryExecutionStatistics_totalExecutionTimeInMillis = Lens.lens (\QueryExecutionStatistics' {totalExecutionTimeInMillis} -> totalExecutionTimeInMillis) (\s@QueryExecutionStatistics' {} a -> s {totalExecutionTimeInMillis = a} :: QueryExecutionStatistics)

-- | The number of milliseconds that the query took to execute.
queryExecutionStatistics_engineExecutionTimeInMillis :: Lens.Lens' QueryExecutionStatistics (Prelude.Maybe Prelude.Integer)
queryExecutionStatistics_engineExecutionTimeInMillis = Lens.lens (\QueryExecutionStatistics' {engineExecutionTimeInMillis} -> engineExecutionTimeInMillis) (\s@QueryExecutionStatistics' {} a -> s {engineExecutionTimeInMillis = a} :: QueryExecutionStatistics)

-- | The number of milliseconds that Athena took to plan the query processing
-- flow. This includes the time spent retrieving table partitions from the
-- data source. Note that because the query engine performs the query
-- planning, query planning time is a subset of engine processing time.
queryExecutionStatistics_queryPlanningTimeInMillis :: Lens.Lens' QueryExecutionStatistics (Prelude.Maybe Prelude.Integer)
queryExecutionStatistics_queryPlanningTimeInMillis = Lens.lens (\QueryExecutionStatistics' {queryPlanningTimeInMillis} -> queryPlanningTimeInMillis) (\s@QueryExecutionStatistics' {} a -> s {queryPlanningTimeInMillis = a} :: QueryExecutionStatistics)

instance Core.FromJSON QueryExecutionStatistics where
  parseJSON =
    Core.withObject
      "QueryExecutionStatistics"
      ( \x ->
          QueryExecutionStatistics'
            Prelude.<$> (x Core..:? "DataScannedInBytes")
            Prelude.<*> (x Core..:? "QueryQueueTimeInMillis")
            Prelude.<*> (x Core..:? "ResultReuseInformation")
            Prelude.<*> (x Core..:? "ServiceProcessingTimeInMillis")
            Prelude.<*> (x Core..:? "DataManifestLocation")
            Prelude.<*> (x Core..:? "TotalExecutionTimeInMillis")
            Prelude.<*> (x Core..:? "EngineExecutionTimeInMillis")
            Prelude.<*> (x Core..:? "QueryPlanningTimeInMillis")
      )

instance Prelude.Hashable QueryExecutionStatistics where
  hashWithSalt _salt QueryExecutionStatistics' {..} =
    _salt `Prelude.hashWithSalt` dataScannedInBytes
      `Prelude.hashWithSalt` queryQueueTimeInMillis
      `Prelude.hashWithSalt` resultReuseInformation
      `Prelude.hashWithSalt` serviceProcessingTimeInMillis
      `Prelude.hashWithSalt` dataManifestLocation
      `Prelude.hashWithSalt` totalExecutionTimeInMillis
      `Prelude.hashWithSalt` engineExecutionTimeInMillis
      `Prelude.hashWithSalt` queryPlanningTimeInMillis

instance Prelude.NFData QueryExecutionStatistics where
  rnf QueryExecutionStatistics' {..} =
    Prelude.rnf dataScannedInBytes
      `Prelude.seq` Prelude.rnf queryQueueTimeInMillis
      `Prelude.seq` Prelude.rnf resultReuseInformation
      `Prelude.seq` Prelude.rnf serviceProcessingTimeInMillis
      `Prelude.seq` Prelude.rnf dataManifestLocation
      `Prelude.seq` Prelude.rnf totalExecutionTimeInMillis
      `Prelude.seq` Prelude.rnf engineExecutionTimeInMillis
      `Prelude.seq` Prelude.rnf queryPlanningTimeInMillis
