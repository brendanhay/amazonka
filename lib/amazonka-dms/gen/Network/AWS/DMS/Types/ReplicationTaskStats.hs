{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationTaskStats
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ReplicationTaskStats where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | In response to a request by the @DescribeReplicationTasks@ operation, this object provides a collection of statistics about a replication task.
--
--
--
-- /See:/ 'replicationTaskStats' smart constructor.
data ReplicationTaskStats = ReplicationTaskStats'
  { _rtsStopDate ::
      !(Maybe POSIX),
    _rtsFullLoadProgressPercent :: !(Maybe Int),
    _rtsFullLoadStartDate :: !(Maybe POSIX),
    _rtsElapsedTimeMillis :: !(Maybe Integer),
    _rtsStartDate :: !(Maybe POSIX),
    _rtsTablesErrored :: !(Maybe Int),
    _rtsFullLoadFinishDate :: !(Maybe POSIX),
    _rtsTablesLoaded :: !(Maybe Int),
    _rtsTablesQueued :: !(Maybe Int),
    _rtsTablesLoading :: !(Maybe Int),
    _rtsFreshStartDate :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicationTaskStats' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtsStopDate' - The date the replication task was stopped.
--
-- * 'rtsFullLoadProgressPercent' - The percent complete for the full load migration task.
--
-- * 'rtsFullLoadStartDate' - The date the replication task full load was started.
--
-- * 'rtsElapsedTimeMillis' - The elapsed time of the task, in milliseconds.
--
-- * 'rtsStartDate' - The date the replication task was started either with a fresh start or a resume. For more information, see <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html#DMS-StartReplicationTask-request-StartReplicationTaskType StartReplicationTaskType> .
--
-- * 'rtsTablesErrored' - The number of errors that have occurred during this task.
--
-- * 'rtsFullLoadFinishDate' - The date the replication task full load was completed.
--
-- * 'rtsTablesLoaded' - The number of tables loaded for this task.
--
-- * 'rtsTablesQueued' - The number of tables queued for this task.
--
-- * 'rtsTablesLoading' - The number of tables currently loading for this task.
--
-- * 'rtsFreshStartDate' - The date the replication task was started either with a fresh start or a target reload.
replicationTaskStats ::
  ReplicationTaskStats
replicationTaskStats =
  ReplicationTaskStats'
    { _rtsStopDate = Nothing,
      _rtsFullLoadProgressPercent = Nothing,
      _rtsFullLoadStartDate = Nothing,
      _rtsElapsedTimeMillis = Nothing,
      _rtsStartDate = Nothing,
      _rtsTablesErrored = Nothing,
      _rtsFullLoadFinishDate = Nothing,
      _rtsTablesLoaded = Nothing,
      _rtsTablesQueued = Nothing,
      _rtsTablesLoading = Nothing,
      _rtsFreshStartDate = Nothing
    }

-- | The date the replication task was stopped.
rtsStopDate :: Lens' ReplicationTaskStats (Maybe UTCTime)
rtsStopDate = lens _rtsStopDate (\s a -> s {_rtsStopDate = a}) . mapping _Time

-- | The percent complete for the full load migration task.
rtsFullLoadProgressPercent :: Lens' ReplicationTaskStats (Maybe Int)
rtsFullLoadProgressPercent = lens _rtsFullLoadProgressPercent (\s a -> s {_rtsFullLoadProgressPercent = a})

-- | The date the replication task full load was started.
rtsFullLoadStartDate :: Lens' ReplicationTaskStats (Maybe UTCTime)
rtsFullLoadStartDate = lens _rtsFullLoadStartDate (\s a -> s {_rtsFullLoadStartDate = a}) . mapping _Time

-- | The elapsed time of the task, in milliseconds.
rtsElapsedTimeMillis :: Lens' ReplicationTaskStats (Maybe Integer)
rtsElapsedTimeMillis = lens _rtsElapsedTimeMillis (\s a -> s {_rtsElapsedTimeMillis = a})

-- | The date the replication task was started either with a fresh start or a resume. For more information, see <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html#DMS-StartReplicationTask-request-StartReplicationTaskType StartReplicationTaskType> .
rtsStartDate :: Lens' ReplicationTaskStats (Maybe UTCTime)
rtsStartDate = lens _rtsStartDate (\s a -> s {_rtsStartDate = a}) . mapping _Time

-- | The number of errors that have occurred during this task.
rtsTablesErrored :: Lens' ReplicationTaskStats (Maybe Int)
rtsTablesErrored = lens _rtsTablesErrored (\s a -> s {_rtsTablesErrored = a})

-- | The date the replication task full load was completed.
rtsFullLoadFinishDate :: Lens' ReplicationTaskStats (Maybe UTCTime)
rtsFullLoadFinishDate = lens _rtsFullLoadFinishDate (\s a -> s {_rtsFullLoadFinishDate = a}) . mapping _Time

-- | The number of tables loaded for this task.
rtsTablesLoaded :: Lens' ReplicationTaskStats (Maybe Int)
rtsTablesLoaded = lens _rtsTablesLoaded (\s a -> s {_rtsTablesLoaded = a})

-- | The number of tables queued for this task.
rtsTablesQueued :: Lens' ReplicationTaskStats (Maybe Int)
rtsTablesQueued = lens _rtsTablesQueued (\s a -> s {_rtsTablesQueued = a})

-- | The number of tables currently loading for this task.
rtsTablesLoading :: Lens' ReplicationTaskStats (Maybe Int)
rtsTablesLoading = lens _rtsTablesLoading (\s a -> s {_rtsTablesLoading = a})

-- | The date the replication task was started either with a fresh start or a target reload.
rtsFreshStartDate :: Lens' ReplicationTaskStats (Maybe UTCTime)
rtsFreshStartDate = lens _rtsFreshStartDate (\s a -> s {_rtsFreshStartDate = a}) . mapping _Time

instance FromJSON ReplicationTaskStats where
  parseJSON =
    withObject
      "ReplicationTaskStats"
      ( \x ->
          ReplicationTaskStats'
            <$> (x .:? "StopDate")
            <*> (x .:? "FullLoadProgressPercent")
            <*> (x .:? "FullLoadStartDate")
            <*> (x .:? "ElapsedTimeMillis")
            <*> (x .:? "StartDate")
            <*> (x .:? "TablesErrored")
            <*> (x .:? "FullLoadFinishDate")
            <*> (x .:? "TablesLoaded")
            <*> (x .:? "TablesQueued")
            <*> (x .:? "TablesLoading")
            <*> (x .:? "FreshStartDate")
      )

instance Hashable ReplicationTaskStats

instance NFData ReplicationTaskStats
