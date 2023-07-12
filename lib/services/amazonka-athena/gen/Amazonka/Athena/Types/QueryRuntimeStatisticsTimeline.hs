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
-- Module      : Amazonka.Athena.Types.QueryRuntimeStatisticsTimeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.QueryRuntimeStatisticsTimeline where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Timeline statistics such as query queue time, planning time, execution
-- time, service processing time, and total execution time.
--
-- /See:/ 'newQueryRuntimeStatisticsTimeline' smart constructor.
data QueryRuntimeStatisticsTimeline = QueryRuntimeStatisticsTimeline'
  { -- | The number of milliseconds that the query took to execute.
    engineExecutionTimeInMillis :: Prelude.Maybe Prelude.Integer,
    -- | The number of milliseconds that Athena took to plan the query processing
    -- flow. This includes the time spent retrieving table partitions from the
    -- data source. Note that because the query engine performs the query
    -- planning, query planning time is a subset of engine processing time.
    queryPlanningTimeInMillis :: Prelude.Maybe Prelude.Integer,
    -- | The number of milliseconds that the query was in your query queue
    -- waiting for resources. Note that if transient errors occur, Athena might
    -- automatically add the query back to the queue.
    queryQueueTimeInMillis :: Prelude.Maybe Prelude.Integer,
    -- | The number of milliseconds that Athena took to finalize and publish the
    -- query results after the query engine finished running the query.
    serviceProcessingTimeInMillis :: Prelude.Maybe Prelude.Integer,
    -- | The number of milliseconds that Athena took to run the query.
    totalExecutionTimeInMillis :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryRuntimeStatisticsTimeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineExecutionTimeInMillis', 'queryRuntimeStatisticsTimeline_engineExecutionTimeInMillis' - The number of milliseconds that the query took to execute.
--
-- 'queryPlanningTimeInMillis', 'queryRuntimeStatisticsTimeline_queryPlanningTimeInMillis' - The number of milliseconds that Athena took to plan the query processing
-- flow. This includes the time spent retrieving table partitions from the
-- data source. Note that because the query engine performs the query
-- planning, query planning time is a subset of engine processing time.
--
-- 'queryQueueTimeInMillis', 'queryRuntimeStatisticsTimeline_queryQueueTimeInMillis' - The number of milliseconds that the query was in your query queue
-- waiting for resources. Note that if transient errors occur, Athena might
-- automatically add the query back to the queue.
--
-- 'serviceProcessingTimeInMillis', 'queryRuntimeStatisticsTimeline_serviceProcessingTimeInMillis' - The number of milliseconds that Athena took to finalize and publish the
-- query results after the query engine finished running the query.
--
-- 'totalExecutionTimeInMillis', 'queryRuntimeStatisticsTimeline_totalExecutionTimeInMillis' - The number of milliseconds that Athena took to run the query.
newQueryRuntimeStatisticsTimeline ::
  QueryRuntimeStatisticsTimeline
newQueryRuntimeStatisticsTimeline =
  QueryRuntimeStatisticsTimeline'
    { engineExecutionTimeInMillis =
        Prelude.Nothing,
      queryPlanningTimeInMillis = Prelude.Nothing,
      queryQueueTimeInMillis = Prelude.Nothing,
      serviceProcessingTimeInMillis =
        Prelude.Nothing,
      totalExecutionTimeInMillis =
        Prelude.Nothing
    }

-- | The number of milliseconds that the query took to execute.
queryRuntimeStatisticsTimeline_engineExecutionTimeInMillis :: Lens.Lens' QueryRuntimeStatisticsTimeline (Prelude.Maybe Prelude.Integer)
queryRuntimeStatisticsTimeline_engineExecutionTimeInMillis = Lens.lens (\QueryRuntimeStatisticsTimeline' {engineExecutionTimeInMillis} -> engineExecutionTimeInMillis) (\s@QueryRuntimeStatisticsTimeline' {} a -> s {engineExecutionTimeInMillis = a} :: QueryRuntimeStatisticsTimeline)

-- | The number of milliseconds that Athena took to plan the query processing
-- flow. This includes the time spent retrieving table partitions from the
-- data source. Note that because the query engine performs the query
-- planning, query planning time is a subset of engine processing time.
queryRuntimeStatisticsTimeline_queryPlanningTimeInMillis :: Lens.Lens' QueryRuntimeStatisticsTimeline (Prelude.Maybe Prelude.Integer)
queryRuntimeStatisticsTimeline_queryPlanningTimeInMillis = Lens.lens (\QueryRuntimeStatisticsTimeline' {queryPlanningTimeInMillis} -> queryPlanningTimeInMillis) (\s@QueryRuntimeStatisticsTimeline' {} a -> s {queryPlanningTimeInMillis = a} :: QueryRuntimeStatisticsTimeline)

-- | The number of milliseconds that the query was in your query queue
-- waiting for resources. Note that if transient errors occur, Athena might
-- automatically add the query back to the queue.
queryRuntimeStatisticsTimeline_queryQueueTimeInMillis :: Lens.Lens' QueryRuntimeStatisticsTimeline (Prelude.Maybe Prelude.Integer)
queryRuntimeStatisticsTimeline_queryQueueTimeInMillis = Lens.lens (\QueryRuntimeStatisticsTimeline' {queryQueueTimeInMillis} -> queryQueueTimeInMillis) (\s@QueryRuntimeStatisticsTimeline' {} a -> s {queryQueueTimeInMillis = a} :: QueryRuntimeStatisticsTimeline)

-- | The number of milliseconds that Athena took to finalize and publish the
-- query results after the query engine finished running the query.
queryRuntimeStatisticsTimeline_serviceProcessingTimeInMillis :: Lens.Lens' QueryRuntimeStatisticsTimeline (Prelude.Maybe Prelude.Integer)
queryRuntimeStatisticsTimeline_serviceProcessingTimeInMillis = Lens.lens (\QueryRuntimeStatisticsTimeline' {serviceProcessingTimeInMillis} -> serviceProcessingTimeInMillis) (\s@QueryRuntimeStatisticsTimeline' {} a -> s {serviceProcessingTimeInMillis = a} :: QueryRuntimeStatisticsTimeline)

-- | The number of milliseconds that Athena took to run the query.
queryRuntimeStatisticsTimeline_totalExecutionTimeInMillis :: Lens.Lens' QueryRuntimeStatisticsTimeline (Prelude.Maybe Prelude.Integer)
queryRuntimeStatisticsTimeline_totalExecutionTimeInMillis = Lens.lens (\QueryRuntimeStatisticsTimeline' {totalExecutionTimeInMillis} -> totalExecutionTimeInMillis) (\s@QueryRuntimeStatisticsTimeline' {} a -> s {totalExecutionTimeInMillis = a} :: QueryRuntimeStatisticsTimeline)

instance Data.FromJSON QueryRuntimeStatisticsTimeline where
  parseJSON =
    Data.withObject
      "QueryRuntimeStatisticsTimeline"
      ( \x ->
          QueryRuntimeStatisticsTimeline'
            Prelude.<$> (x Data..:? "EngineExecutionTimeInMillis")
            Prelude.<*> (x Data..:? "QueryPlanningTimeInMillis")
            Prelude.<*> (x Data..:? "QueryQueueTimeInMillis")
            Prelude.<*> (x Data..:? "ServiceProcessingTimeInMillis")
            Prelude.<*> (x Data..:? "TotalExecutionTimeInMillis")
      )

instance
  Prelude.Hashable
    QueryRuntimeStatisticsTimeline
  where
  hashWithSalt
    _salt
    QueryRuntimeStatisticsTimeline' {..} =
      _salt
        `Prelude.hashWithSalt` engineExecutionTimeInMillis
        `Prelude.hashWithSalt` queryPlanningTimeInMillis
        `Prelude.hashWithSalt` queryQueueTimeInMillis
        `Prelude.hashWithSalt` serviceProcessingTimeInMillis
        `Prelude.hashWithSalt` totalExecutionTimeInMillis

instance
  Prelude.NFData
    QueryRuntimeStatisticsTimeline
  where
  rnf QueryRuntimeStatisticsTimeline' {..} =
    Prelude.rnf engineExecutionTimeInMillis
      `Prelude.seq` Prelude.rnf queryPlanningTimeInMillis
      `Prelude.seq` Prelude.rnf queryQueueTimeInMillis
      `Prelude.seq` Prelude.rnf serviceProcessingTimeInMillis
      `Prelude.seq` Prelude.rnf totalExecutionTimeInMillis
