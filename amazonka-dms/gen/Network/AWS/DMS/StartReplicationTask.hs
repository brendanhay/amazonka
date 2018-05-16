{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.StartReplicationTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the replication task.
--
--
-- For more information about AWS DMS tasks, see the AWS DMS user guide at <http://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.html Working with Migration Tasks >
--
module Network.AWS.DMS.StartReplicationTask
    (
    -- * Creating a Request
      startReplicationTask
    , StartReplicationTask
    -- * Request Lenses
    , srtCdcStartPosition
    , srtCdcStopPosition
    , srtCdcStartTime
    , srtReplicationTaskARN
    , srtStartReplicationTaskType

    -- * Destructuring the Response
    , startReplicationTaskResponse
    , StartReplicationTaskResponse
    -- * Response Lenses
    , srtrsReplicationTask
    , srtrsResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'startReplicationTask' smart constructor.
data StartReplicationTask = StartReplicationTask'
  { _srtCdcStartPosition         :: !(Maybe Text)
  , _srtCdcStopPosition          :: !(Maybe Text)
  , _srtCdcStartTime             :: !(Maybe POSIX)
  , _srtReplicationTaskARN       :: !Text
  , _srtStartReplicationTaskType :: !StartReplicationTaskTypeValue
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartReplicationTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srtCdcStartPosition' - Indicates when you want a change data capture (CDC) operation to start. Use either CdcStartPosition or CdcStartTime to specify when you want a CDC operation to start. Specifying both values results in an error. The value can be in date, checkpoint, or LSN/SCN format. Date Example: --cdc-start-position “2018-03-08T12:12:12” Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93" LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
--
-- * 'srtCdcStopPosition' - Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time. Server time example: --cdc-stop-position “server_time:3018-02-09T12:12:12” Commit time example: --cdc-stop-position “commit_time: 3018-02-09T12:12:12 “
--
-- * 'srtCdcStartTime' - Indicates the start time for a change data capture (CDC) operation. Use either CdcStartTime or CdcStartPosition to specify when you want a CDC operation to start. Specifying both values results in an error.
--
-- * 'srtReplicationTaskARN' - The Amazon Resource Name (ARN) of the replication task to be started.
--
-- * 'srtStartReplicationTaskType' - The type of replication task.
startReplicationTask
    :: Text -- ^ 'srtReplicationTaskARN'
    -> StartReplicationTaskTypeValue -- ^ 'srtStartReplicationTaskType'
    -> StartReplicationTask
startReplicationTask pReplicationTaskARN_ pStartReplicationTaskType_ =
  StartReplicationTask'
    { _srtCdcStartPosition = Nothing
    , _srtCdcStopPosition = Nothing
    , _srtCdcStartTime = Nothing
    , _srtReplicationTaskARN = pReplicationTaskARN_
    , _srtStartReplicationTaskType = pStartReplicationTaskType_
    }


-- | Indicates when you want a change data capture (CDC) operation to start. Use either CdcStartPosition or CdcStartTime to specify when you want a CDC operation to start. Specifying both values results in an error. The value can be in date, checkpoint, or LSN/SCN format. Date Example: --cdc-start-position “2018-03-08T12:12:12” Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93" LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
srtCdcStartPosition :: Lens' StartReplicationTask (Maybe Text)
srtCdcStartPosition = lens _srtCdcStartPosition (\ s a -> s{_srtCdcStartPosition = a})

-- | Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time. Server time example: --cdc-stop-position “server_time:3018-02-09T12:12:12” Commit time example: --cdc-stop-position “commit_time: 3018-02-09T12:12:12 “
srtCdcStopPosition :: Lens' StartReplicationTask (Maybe Text)
srtCdcStopPosition = lens _srtCdcStopPosition (\ s a -> s{_srtCdcStopPosition = a})

-- | Indicates the start time for a change data capture (CDC) operation. Use either CdcStartTime or CdcStartPosition to specify when you want a CDC operation to start. Specifying both values results in an error.
srtCdcStartTime :: Lens' StartReplicationTask (Maybe UTCTime)
srtCdcStartTime = lens _srtCdcStartTime (\ s a -> s{_srtCdcStartTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the replication task to be started.
srtReplicationTaskARN :: Lens' StartReplicationTask Text
srtReplicationTaskARN = lens _srtReplicationTaskARN (\ s a -> s{_srtReplicationTaskARN = a})

-- | The type of replication task.
srtStartReplicationTaskType :: Lens' StartReplicationTask StartReplicationTaskTypeValue
srtStartReplicationTaskType = lens _srtStartReplicationTaskType (\ s a -> s{_srtStartReplicationTaskType = a})

instance AWSRequest StartReplicationTask where
        type Rs StartReplicationTask =
             StartReplicationTaskResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 StartReplicationTaskResponse' <$>
                   (x .?> "ReplicationTask") <*> (pure (fromEnum s)))

instance Hashable StartReplicationTask where

instance NFData StartReplicationTask where

instance ToHeaders StartReplicationTask where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.StartReplicationTask" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartReplicationTask where
        toJSON StartReplicationTask'{..}
          = object
              (catMaybes
                 [("CdcStartPosition" .=) <$> _srtCdcStartPosition,
                  ("CdcStopPosition" .=) <$> _srtCdcStopPosition,
                  ("CdcStartTime" .=) <$> _srtCdcStartTime,
                  Just
                    ("ReplicationTaskArn" .= _srtReplicationTaskARN),
                  Just
                    ("StartReplicationTaskType" .=
                       _srtStartReplicationTaskType)])

instance ToPath StartReplicationTask where
        toPath = const "/"

instance ToQuery StartReplicationTask where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'startReplicationTaskResponse' smart constructor.
data StartReplicationTaskResponse = StartReplicationTaskResponse'
  { _srtrsReplicationTask :: !(Maybe ReplicationTask)
  , _srtrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartReplicationTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srtrsReplicationTask' - The replication task started.
--
-- * 'srtrsResponseStatus' - -- | The response status code.
startReplicationTaskResponse
    :: Int -- ^ 'srtrsResponseStatus'
    -> StartReplicationTaskResponse
startReplicationTaskResponse pResponseStatus_ =
  StartReplicationTaskResponse'
    {_srtrsReplicationTask = Nothing, _srtrsResponseStatus = pResponseStatus_}


-- | The replication task started.
srtrsReplicationTask :: Lens' StartReplicationTaskResponse (Maybe ReplicationTask)
srtrsReplicationTask = lens _srtrsReplicationTask (\ s a -> s{_srtrsReplicationTask = a})

-- | -- | The response status code.
srtrsResponseStatus :: Lens' StartReplicationTaskResponse Int
srtrsResponseStatus = lens _srtrsResponseStatus (\ s a -> s{_srtrsResponseStatus = a})

instance NFData StartReplicationTaskResponse where
