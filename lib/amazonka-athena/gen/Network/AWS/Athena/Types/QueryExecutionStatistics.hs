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
    qesTotalExecutionTimeInMillis,
    qesEngineExecutionTimeInMillis,
    qesQueryPlanningTimeInMillis,
    qesDataScannedInBytes,
    qesQueryQueueTimeInMillis,
    qesDataManifestLocation,
    qesServiceProcessingTimeInMillis,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The amount of data scanned during the query execution and the amount of time that it took to execute, and the type of statement that was run.
--
-- /See:/ 'mkQueryExecutionStatistics' smart constructor.
data QueryExecutionStatistics = QueryExecutionStatistics'
  { totalExecutionTimeInMillis ::
      Lude.Maybe Lude.Integer,
    engineExecutionTimeInMillis ::
      Lude.Maybe Lude.Integer,
    queryPlanningTimeInMillis ::
      Lude.Maybe Lude.Integer,
    dataScannedInBytes ::
      Lude.Maybe Lude.Integer,
    queryQueueTimeInMillis ::
      Lude.Maybe Lude.Integer,
    dataManifestLocation ::
      Lude.Maybe Lude.Text,
    serviceProcessingTimeInMillis ::
      Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QueryExecutionStatistics' with the minimum fields required to make a request.
--
-- * 'dataManifestLocation' - The location and file name of a data manifest file. The manifest file is saved to the Athena query results location in Amazon S3. The manifest file tracks files that the query wrote to Amazon S3. If the query fails, the manifest file also tracks files that the query intended to write. The manifest is useful for identifying orphaned files resulting from a failed query. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Working with Query Results, Output Files, and Query History> in the /Amazon Athena User Guide/ .
-- * 'dataScannedInBytes' - The number of bytes in the data that was queried.
-- * 'engineExecutionTimeInMillis' - The number of milliseconds that the query took to execute.
-- * 'queryPlanningTimeInMillis' - The number of milliseconds that Athena took to plan the query processing flow. This includes the time spent retrieving table partitions from the data source. Note that because the query engine performs the query planning, query planning time is a subset of engine processing time.
-- * 'queryQueueTimeInMillis' - The number of milliseconds that the query was in your query queue waiting for resources. Note that if transient errors occur, Athena might automatically add the query back to the queue.
-- * 'serviceProcessingTimeInMillis' - The number of milliseconds that Athena took to finalize and publish the query results after the query engine finished running the query.
-- * 'totalExecutionTimeInMillis' - The number of milliseconds that Athena took to run the query.
mkQueryExecutionStatistics ::
  QueryExecutionStatistics
mkQueryExecutionStatistics =
  QueryExecutionStatistics'
    { totalExecutionTimeInMillis =
        Lude.Nothing,
      engineExecutionTimeInMillis = Lude.Nothing,
      queryPlanningTimeInMillis = Lude.Nothing,
      dataScannedInBytes = Lude.Nothing,
      queryQueueTimeInMillis = Lude.Nothing,
      dataManifestLocation = Lude.Nothing,
      serviceProcessingTimeInMillis = Lude.Nothing
    }

-- | The number of milliseconds that Athena took to run the query.
--
-- /Note:/ Consider using 'totalExecutionTimeInMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qesTotalExecutionTimeInMillis :: Lens.Lens' QueryExecutionStatistics (Lude.Maybe Lude.Integer)
qesTotalExecutionTimeInMillis = Lens.lens (totalExecutionTimeInMillis :: QueryExecutionStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {totalExecutionTimeInMillis = a} :: QueryExecutionStatistics)
{-# DEPRECATED qesTotalExecutionTimeInMillis "Use generic-lens or generic-optics with 'totalExecutionTimeInMillis' instead." #-}

-- | The number of milliseconds that the query took to execute.
--
-- /Note:/ Consider using 'engineExecutionTimeInMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qesEngineExecutionTimeInMillis :: Lens.Lens' QueryExecutionStatistics (Lude.Maybe Lude.Integer)
qesEngineExecutionTimeInMillis = Lens.lens (engineExecutionTimeInMillis :: QueryExecutionStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {engineExecutionTimeInMillis = a} :: QueryExecutionStatistics)
{-# DEPRECATED qesEngineExecutionTimeInMillis "Use generic-lens or generic-optics with 'engineExecutionTimeInMillis' instead." #-}

-- | The number of milliseconds that Athena took to plan the query processing flow. This includes the time spent retrieving table partitions from the data source. Note that because the query engine performs the query planning, query planning time is a subset of engine processing time.
--
-- /Note:/ Consider using 'queryPlanningTimeInMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qesQueryPlanningTimeInMillis :: Lens.Lens' QueryExecutionStatistics (Lude.Maybe Lude.Integer)
qesQueryPlanningTimeInMillis = Lens.lens (queryPlanningTimeInMillis :: QueryExecutionStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {queryPlanningTimeInMillis = a} :: QueryExecutionStatistics)
{-# DEPRECATED qesQueryPlanningTimeInMillis "Use generic-lens or generic-optics with 'queryPlanningTimeInMillis' instead." #-}

-- | The number of bytes in the data that was queried.
--
-- /Note:/ Consider using 'dataScannedInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qesDataScannedInBytes :: Lens.Lens' QueryExecutionStatistics (Lude.Maybe Lude.Integer)
qesDataScannedInBytes = Lens.lens (dataScannedInBytes :: QueryExecutionStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {dataScannedInBytes = a} :: QueryExecutionStatistics)
{-# DEPRECATED qesDataScannedInBytes "Use generic-lens or generic-optics with 'dataScannedInBytes' instead." #-}

-- | The number of milliseconds that the query was in your query queue waiting for resources. Note that if transient errors occur, Athena might automatically add the query back to the queue.
--
-- /Note:/ Consider using 'queryQueueTimeInMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qesQueryQueueTimeInMillis :: Lens.Lens' QueryExecutionStatistics (Lude.Maybe Lude.Integer)
qesQueryQueueTimeInMillis = Lens.lens (queryQueueTimeInMillis :: QueryExecutionStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {queryQueueTimeInMillis = a} :: QueryExecutionStatistics)
{-# DEPRECATED qesQueryQueueTimeInMillis "Use generic-lens or generic-optics with 'queryQueueTimeInMillis' instead." #-}

-- | The location and file name of a data manifest file. The manifest file is saved to the Athena query results location in Amazon S3. The manifest file tracks files that the query wrote to Amazon S3. If the query fails, the manifest file also tracks files that the query intended to write. The manifest is useful for identifying orphaned files resulting from a failed query. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Working with Query Results, Output Files, and Query History> in the /Amazon Athena User Guide/ .
--
-- /Note:/ Consider using 'dataManifestLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qesDataManifestLocation :: Lens.Lens' QueryExecutionStatistics (Lude.Maybe Lude.Text)
qesDataManifestLocation = Lens.lens (dataManifestLocation :: QueryExecutionStatistics -> Lude.Maybe Lude.Text) (\s a -> s {dataManifestLocation = a} :: QueryExecutionStatistics)
{-# DEPRECATED qesDataManifestLocation "Use generic-lens or generic-optics with 'dataManifestLocation' instead." #-}

-- | The number of milliseconds that Athena took to finalize and publish the query results after the query engine finished running the query.
--
-- /Note:/ Consider using 'serviceProcessingTimeInMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qesServiceProcessingTimeInMillis :: Lens.Lens' QueryExecutionStatistics (Lude.Maybe Lude.Integer)
qesServiceProcessingTimeInMillis = Lens.lens (serviceProcessingTimeInMillis :: QueryExecutionStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {serviceProcessingTimeInMillis = a} :: QueryExecutionStatistics)
{-# DEPRECATED qesServiceProcessingTimeInMillis "Use generic-lens or generic-optics with 'serviceProcessingTimeInMillis' instead." #-}

instance Lude.FromJSON QueryExecutionStatistics where
  parseJSON =
    Lude.withObject
      "QueryExecutionStatistics"
      ( \x ->
          QueryExecutionStatistics'
            Lude.<$> (x Lude..:? "TotalExecutionTimeInMillis")
            Lude.<*> (x Lude..:? "EngineExecutionTimeInMillis")
            Lude.<*> (x Lude..:? "QueryPlanningTimeInMillis")
            Lude.<*> (x Lude..:? "DataScannedInBytes")
            Lude.<*> (x Lude..:? "QueryQueueTimeInMillis")
            Lude.<*> (x Lude..:? "DataManifestLocation")
            Lude.<*> (x Lude..:? "ServiceProcessingTimeInMillis")
      )
