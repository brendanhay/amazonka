{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.ExportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.ExportTask where

import Network.AWS.CloudWatchLogs.Types.ExportTaskExecutionInfo
import Network.AWS.CloudWatchLogs.Types.ExportTaskStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents an export task.
--
--
--
-- /See:/ 'exportTask' smart constructor.
data ExportTask = ExportTask'
  { _etDestinationPrefix ::
      !(Maybe Text),
    _etDestination :: !(Maybe Text),
    _etStatus :: !(Maybe ExportTaskStatus),
    _etTaskName :: !(Maybe Text),
    _etTaskId :: !(Maybe Text),
    _etTo :: !(Maybe Nat),
    _etFrom :: !(Maybe Nat),
    _etLogGroupName :: !(Maybe Text),
    _etExecutionInfo :: !(Maybe ExportTaskExecutionInfo)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExportTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etDestinationPrefix' - The prefix that was used as the start of Amazon S3 key for every object exported.
--
-- * 'etDestination' - The name of the S3 bucket to which the log data was exported.
--
-- * 'etStatus' - The status of the export task.
--
-- * 'etTaskName' - The name of the export task.
--
-- * 'etTaskId' - The ID of the export task.
--
-- * 'etTo' - The end time, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp later than this time are not exported.
--
-- * 'etFrom' - The start time, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp before this time are not exported.
--
-- * 'etLogGroupName' - The name of the log group from which logs data was exported.
--
-- * 'etExecutionInfo' - Execution information about the export task.
exportTask ::
  ExportTask
exportTask =
  ExportTask'
    { _etDestinationPrefix = Nothing,
      _etDestination = Nothing,
      _etStatus = Nothing,
      _etTaskName = Nothing,
      _etTaskId = Nothing,
      _etTo = Nothing,
      _etFrom = Nothing,
      _etLogGroupName = Nothing,
      _etExecutionInfo = Nothing
    }

-- | The prefix that was used as the start of Amazon S3 key for every object exported.
etDestinationPrefix :: Lens' ExportTask (Maybe Text)
etDestinationPrefix = lens _etDestinationPrefix (\s a -> s {_etDestinationPrefix = a})

-- | The name of the S3 bucket to which the log data was exported.
etDestination :: Lens' ExportTask (Maybe Text)
etDestination = lens _etDestination (\s a -> s {_etDestination = a})

-- | The status of the export task.
etStatus :: Lens' ExportTask (Maybe ExportTaskStatus)
etStatus = lens _etStatus (\s a -> s {_etStatus = a})

-- | The name of the export task.
etTaskName :: Lens' ExportTask (Maybe Text)
etTaskName = lens _etTaskName (\s a -> s {_etTaskName = a})

-- | The ID of the export task.
etTaskId :: Lens' ExportTask (Maybe Text)
etTaskId = lens _etTaskId (\s a -> s {_etTaskId = a})

-- | The end time, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp later than this time are not exported.
etTo :: Lens' ExportTask (Maybe Natural)
etTo = lens _etTo (\s a -> s {_etTo = a}) . mapping _Nat

-- | The start time, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp before this time are not exported.
etFrom :: Lens' ExportTask (Maybe Natural)
etFrom = lens _etFrom (\s a -> s {_etFrom = a}) . mapping _Nat

-- | The name of the log group from which logs data was exported.
etLogGroupName :: Lens' ExportTask (Maybe Text)
etLogGroupName = lens _etLogGroupName (\s a -> s {_etLogGroupName = a})

-- | Execution information about the export task.
etExecutionInfo :: Lens' ExportTask (Maybe ExportTaskExecutionInfo)
etExecutionInfo = lens _etExecutionInfo (\s a -> s {_etExecutionInfo = a})

instance FromJSON ExportTask where
  parseJSON =
    withObject
      "ExportTask"
      ( \x ->
          ExportTask'
            <$> (x .:? "destinationPrefix")
            <*> (x .:? "destination")
            <*> (x .:? "status")
            <*> (x .:? "taskName")
            <*> (x .:? "taskId")
            <*> (x .:? "to")
            <*> (x .:? "from")
            <*> (x .:? "logGroupName")
            <*> (x .:? "executionInfo")
      )

instance Hashable ExportTask

instance NFData ExportTask
