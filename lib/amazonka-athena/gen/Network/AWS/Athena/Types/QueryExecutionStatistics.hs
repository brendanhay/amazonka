{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.QueryExecutionStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.QueryExecutionStatistics
  ( QueryExecutionStatistics (..),

    -- * Smart constructor
    mkQueryExecutionStatistics,

    -- * Lenses
    qesDataManifestLocation,
    qesDataScannedInBytes,
    qesEngineExecutionTimeInMillis,
    qesQueryPlanningTimeInMillis,
    qesQueryQueueTimeInMillis,
    qesServiceProcessingTimeInMillis,
    qesTotalExecutionTimeInMillis,
  )
where

import qualified Network.AWS.Athena.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The amount of data scanned during the query execution and the amount of time that it took to execute, and the type of statement that was run.
--
-- /See:/ 'mkQueryExecutionStatistics' smart constructor.
data QueryExecutionStatistics = QueryExecutionStatistics'
  { -- | The location and file name of a data manifest file. The manifest file is saved to the Athena query results location in Amazon S3. The manifest file tracks files that the query wrote to Amazon S3. If the query fails, the manifest file also tracks files that the query intended to write. The manifest is useful for identifying orphaned files resulting from a failed query. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Working with Query Results, Output Files, and Query History> in the /Amazon Athena User Guide/ .
    dataManifestLocation :: Core.Maybe Types.String,
    -- | The number of bytes in the data that was queried.
    dataScannedInBytes :: Core.Maybe Core.Integer,
    -- | The number of milliseconds that the query took to execute.
    engineExecutionTimeInMillis :: Core.Maybe Core.Integer,
    -- | The number of milliseconds that Athena took to plan the query processing flow. This includes the time spent retrieving table partitions from the data source. Note that because the query engine performs the query planning, query planning time is a subset of engine processing time.
    queryPlanningTimeInMillis :: Core.Maybe Core.Integer,
    -- | The number of milliseconds that the query was in your query queue waiting for resources. Note that if transient errors occur, Athena might automatically add the query back to the queue.
    queryQueueTimeInMillis :: Core.Maybe Core.Integer,
    -- | The number of milliseconds that Athena took to finalize and publish the query results after the query engine finished running the query.
    serviceProcessingTimeInMillis :: Core.Maybe Core.Integer,
    -- | The number of milliseconds that Athena took to run the query.
    totalExecutionTimeInMillis :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QueryExecutionStatistics' value with any optional fields omitted.
mkQueryExecutionStatistics ::
  QueryExecutionStatistics
mkQueryExecutionStatistics =
  QueryExecutionStatistics'
    { dataManifestLocation = Core.Nothing,
      dataScannedInBytes = Core.Nothing,
      engineExecutionTimeInMillis = Core.Nothing,
      queryPlanningTimeInMillis = Core.Nothing,
      queryQueueTimeInMillis = Core.Nothing,
      serviceProcessingTimeInMillis = Core.Nothing,
      totalExecutionTimeInMillis = Core.Nothing
    }

-- | The location and file name of a data manifest file. The manifest file is saved to the Athena query results location in Amazon S3. The manifest file tracks files that the query wrote to Amazon S3. If the query fails, the manifest file also tracks files that the query intended to write. The manifest is useful for identifying orphaned files resulting from a failed query. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Working with Query Results, Output Files, and Query History> in the /Amazon Athena User Guide/ .
--
-- /Note:/ Consider using 'dataManifestLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qesDataManifestLocation :: Lens.Lens' QueryExecutionStatistics (Core.Maybe Types.String)
qesDataManifestLocation = Lens.field @"dataManifestLocation"
{-# DEPRECATED qesDataManifestLocation "Use generic-lens or generic-optics with 'dataManifestLocation' instead." #-}

-- | The number of bytes in the data that was queried.
--
-- /Note:/ Consider using 'dataScannedInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qesDataScannedInBytes :: Lens.Lens' QueryExecutionStatistics (Core.Maybe Core.Integer)
qesDataScannedInBytes = Lens.field @"dataScannedInBytes"
{-# DEPRECATED qesDataScannedInBytes "Use generic-lens or generic-optics with 'dataScannedInBytes' instead." #-}

-- | The number of milliseconds that the query took to execute.
--
-- /Note:/ Consider using 'engineExecutionTimeInMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qesEngineExecutionTimeInMillis :: Lens.Lens' QueryExecutionStatistics (Core.Maybe Core.Integer)
qesEngineExecutionTimeInMillis = Lens.field @"engineExecutionTimeInMillis"
{-# DEPRECATED qesEngineExecutionTimeInMillis "Use generic-lens or generic-optics with 'engineExecutionTimeInMillis' instead." #-}

-- | The number of milliseconds that Athena took to plan the query processing flow. This includes the time spent retrieving table partitions from the data source. Note that because the query engine performs the query planning, query planning time is a subset of engine processing time.
--
-- /Note:/ Consider using 'queryPlanningTimeInMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qesQueryPlanningTimeInMillis :: Lens.Lens' QueryExecutionStatistics (Core.Maybe Core.Integer)
qesQueryPlanningTimeInMillis = Lens.field @"queryPlanningTimeInMillis"
{-# DEPRECATED qesQueryPlanningTimeInMillis "Use generic-lens or generic-optics with 'queryPlanningTimeInMillis' instead." #-}

-- | The number of milliseconds that the query was in your query queue waiting for resources. Note that if transient errors occur, Athena might automatically add the query back to the queue.
--
-- /Note:/ Consider using 'queryQueueTimeInMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qesQueryQueueTimeInMillis :: Lens.Lens' QueryExecutionStatistics (Core.Maybe Core.Integer)
qesQueryQueueTimeInMillis = Lens.field @"queryQueueTimeInMillis"
{-# DEPRECATED qesQueryQueueTimeInMillis "Use generic-lens or generic-optics with 'queryQueueTimeInMillis' instead." #-}

-- | The number of milliseconds that Athena took to finalize and publish the query results after the query engine finished running the query.
--
-- /Note:/ Consider using 'serviceProcessingTimeInMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qesServiceProcessingTimeInMillis :: Lens.Lens' QueryExecutionStatistics (Core.Maybe Core.Integer)
qesServiceProcessingTimeInMillis = Lens.field @"serviceProcessingTimeInMillis"
{-# DEPRECATED qesServiceProcessingTimeInMillis "Use generic-lens or generic-optics with 'serviceProcessingTimeInMillis' instead." #-}

-- | The number of milliseconds that Athena took to run the query.
--
-- /Note:/ Consider using 'totalExecutionTimeInMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qesTotalExecutionTimeInMillis :: Lens.Lens' QueryExecutionStatistics (Core.Maybe Core.Integer)
qesTotalExecutionTimeInMillis = Lens.field @"totalExecutionTimeInMillis"
{-# DEPRECATED qesTotalExecutionTimeInMillis "Use generic-lens or generic-optics with 'totalExecutionTimeInMillis' instead." #-}

instance Core.FromJSON QueryExecutionStatistics where
  parseJSON =
    Core.withObject "QueryExecutionStatistics" Core.$
      \x ->
        QueryExecutionStatistics'
          Core.<$> (x Core..:? "DataManifestLocation")
          Core.<*> (x Core..:? "DataScannedInBytes")
          Core.<*> (x Core..:? "EngineExecutionTimeInMillis")
          Core.<*> (x Core..:? "QueryPlanningTimeInMillis")
          Core.<*> (x Core..:? "QueryQueueTimeInMillis")
          Core.<*> (x Core..:? "ServiceProcessingTimeInMillis")
          Core.<*> (x Core..:? "TotalExecutionTimeInMillis")
