{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.QueryExecutionStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.QueryExecutionStatistics where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The amount of data scanned during the query execution and the amount of time that it took to execute, and the type of statement that was run.
--
--
--
-- /See:/ 'queryExecutionStatistics' smart constructor.
data QueryExecutionStatistics = QueryExecutionStatistics'
  { _qesTotalExecutionTimeInMillis ::
      !(Maybe Integer),
    _qesEngineExecutionTimeInMillis ::
      !(Maybe Integer),
    _qesQueryPlanningTimeInMillis ::
      !(Maybe Integer),
    _qesDataScannedInBytes ::
      !(Maybe Integer),
    _qesQueryQueueTimeInMillis ::
      !(Maybe Integer),
    _qesDataManifestLocation :: !(Maybe Text),
    _qesServiceProcessingTimeInMillis ::
      !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'QueryExecutionStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qesTotalExecutionTimeInMillis' - The number of milliseconds that Athena took to run the query.
--
-- * 'qesEngineExecutionTimeInMillis' - The number of milliseconds that the query took to execute.
--
-- * 'qesQueryPlanningTimeInMillis' - The number of milliseconds that Athena took to plan the query processing flow. This includes the time spent retrieving table partitions from the data source. Note that because the query engine performs the query planning, query planning time is a subset of engine processing time.
--
-- * 'qesDataScannedInBytes' - The number of bytes in the data that was queried.
--
-- * 'qesQueryQueueTimeInMillis' - The number of milliseconds that the query was in your query queue waiting for resources. Note that if transient errors occur, Athena might automatically add the query back to the queue.
--
-- * 'qesDataManifestLocation' - The location and file name of a data manifest file. The manifest file is saved to the Athena query results location in Amazon S3. The manifest file tracks files that the query wrote to Amazon S3. If the query fails, the manifest file also tracks files that the query intended to write. The manifest is useful for identifying orphaned files resulting from a failed query. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Working with Query Results, Output Files, and Query History> in the /Amazon Athena User Guide/ .
--
-- * 'qesServiceProcessingTimeInMillis' - The number of milliseconds that Athena took to finalize and publish the query results after the query engine finished running the query.
queryExecutionStatistics ::
  QueryExecutionStatistics
queryExecutionStatistics =
  QueryExecutionStatistics'
    { _qesTotalExecutionTimeInMillis =
        Nothing,
      _qesEngineExecutionTimeInMillis = Nothing,
      _qesQueryPlanningTimeInMillis = Nothing,
      _qesDataScannedInBytes = Nothing,
      _qesQueryQueueTimeInMillis = Nothing,
      _qesDataManifestLocation = Nothing,
      _qesServiceProcessingTimeInMillis = Nothing
    }

-- | The number of milliseconds that Athena took to run the query.
qesTotalExecutionTimeInMillis :: Lens' QueryExecutionStatistics (Maybe Integer)
qesTotalExecutionTimeInMillis = lens _qesTotalExecutionTimeInMillis (\s a -> s {_qesTotalExecutionTimeInMillis = a})

-- | The number of milliseconds that the query took to execute.
qesEngineExecutionTimeInMillis :: Lens' QueryExecutionStatistics (Maybe Integer)
qesEngineExecutionTimeInMillis = lens _qesEngineExecutionTimeInMillis (\s a -> s {_qesEngineExecutionTimeInMillis = a})

-- | The number of milliseconds that Athena took to plan the query processing flow. This includes the time spent retrieving table partitions from the data source. Note that because the query engine performs the query planning, query planning time is a subset of engine processing time.
qesQueryPlanningTimeInMillis :: Lens' QueryExecutionStatistics (Maybe Integer)
qesQueryPlanningTimeInMillis = lens _qesQueryPlanningTimeInMillis (\s a -> s {_qesQueryPlanningTimeInMillis = a})

-- | The number of bytes in the data that was queried.
qesDataScannedInBytes :: Lens' QueryExecutionStatistics (Maybe Integer)
qesDataScannedInBytes = lens _qesDataScannedInBytes (\s a -> s {_qesDataScannedInBytes = a})

-- | The number of milliseconds that the query was in your query queue waiting for resources. Note that if transient errors occur, Athena might automatically add the query back to the queue.
qesQueryQueueTimeInMillis :: Lens' QueryExecutionStatistics (Maybe Integer)
qesQueryQueueTimeInMillis = lens _qesQueryQueueTimeInMillis (\s a -> s {_qesQueryQueueTimeInMillis = a})

-- | The location and file name of a data manifest file. The manifest file is saved to the Athena query results location in Amazon S3. The manifest file tracks files that the query wrote to Amazon S3. If the query fails, the manifest file also tracks files that the query intended to write. The manifest is useful for identifying orphaned files resulting from a failed query. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Working with Query Results, Output Files, and Query History> in the /Amazon Athena User Guide/ .
qesDataManifestLocation :: Lens' QueryExecutionStatistics (Maybe Text)
qesDataManifestLocation = lens _qesDataManifestLocation (\s a -> s {_qesDataManifestLocation = a})

-- | The number of milliseconds that Athena took to finalize and publish the query results after the query engine finished running the query.
qesServiceProcessingTimeInMillis :: Lens' QueryExecutionStatistics (Maybe Integer)
qesServiceProcessingTimeInMillis = lens _qesServiceProcessingTimeInMillis (\s a -> s {_qesServiceProcessingTimeInMillis = a})

instance FromJSON QueryExecutionStatistics where
  parseJSON =
    withObject
      "QueryExecutionStatistics"
      ( \x ->
          QueryExecutionStatistics'
            <$> (x .:? "TotalExecutionTimeInMillis")
            <*> (x .:? "EngineExecutionTimeInMillis")
            <*> (x .:? "QueryPlanningTimeInMillis")
            <*> (x .:? "DataScannedInBytes")
            <*> (x .:? "QueryQueueTimeInMillis")
            <*> (x .:? "DataManifestLocation")
            <*> (x .:? "ServiceProcessingTimeInMillis")
      )

instance Hashable QueryExecutionStatistics

instance NFData QueryExecutionStatistics
