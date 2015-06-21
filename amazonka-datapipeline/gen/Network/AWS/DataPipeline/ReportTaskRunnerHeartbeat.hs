{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.DataPipeline.ReportTaskRunnerHeartbeat
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Task runners call @ReportTaskRunnerHeartbeat@ every 15 minutes to
-- indicate that they are operational. If the AWS Data Pipeline Task Runner
-- is launched on a resource managed by AWS Data Pipeline, the web service
-- can use this call to detect when the task runner application has failed
-- and restart a new instance.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_ReportTaskRunnerHeartbeat.html>
module Network.AWS.DataPipeline.ReportTaskRunnerHeartbeat
    (
    -- * Request
      ReportTaskRunnerHeartbeat
    -- ** Request constructor
    , reportTaskRunnerHeartbeat
    -- ** Request lenses
    , rtrhHostname
    , rtrhWorkerGroup
    , rtrhTaskrunnerId

    -- * Response
    , ReportTaskRunnerHeartbeatResponse
    -- ** Response constructor
    , reportTaskRunnerHeartbeatResponse
    -- ** Response lenses
    , rtrhrTerminate
    ) where

import Network.AWS.DataPipeline.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'reportTaskRunnerHeartbeat' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtrhHostname'
--
-- * 'rtrhWorkerGroup'
--
-- * 'rtrhTaskrunnerId'
data ReportTaskRunnerHeartbeat = ReportTaskRunnerHeartbeat'{_rtrhHostname :: Maybe Text, _rtrhWorkerGroup :: Maybe Text, _rtrhTaskrunnerId :: Text} deriving (Eq, Read, Show)

-- | 'ReportTaskRunnerHeartbeat' smart constructor.
reportTaskRunnerHeartbeat :: Text -> ReportTaskRunnerHeartbeat
reportTaskRunnerHeartbeat pTaskrunnerId = ReportTaskRunnerHeartbeat'{_rtrhHostname = Nothing, _rtrhWorkerGroup = Nothing, _rtrhTaskrunnerId = pTaskrunnerId};

-- | The public DNS name of the task runner.
rtrhHostname :: Lens' ReportTaskRunnerHeartbeat (Maybe Text)
rtrhHostname = lens _rtrhHostname (\ s a -> s{_rtrhHostname = a});

-- | The type of task the task runner is configured to accept and process.
-- The worker group is set as a field on objects in the pipeline when they
-- are created. You can only specify a single value for @workerGroup@.
-- There are no wildcard values permitted in @workerGroup@; the string must
-- be an exact, case-sensitive, match.
rtrhWorkerGroup :: Lens' ReportTaskRunnerHeartbeat (Maybe Text)
rtrhWorkerGroup = lens _rtrhWorkerGroup (\ s a -> s{_rtrhWorkerGroup = a});

-- | The ID of the task runner. This value should be unique across your AWS
-- account. In the case of AWS Data Pipeline Task Runner launched on a
-- resource managed by AWS Data Pipeline, the web service provides a unique
-- identifier when it launches the application. If you have written a
-- custom task runner, you should assign a unique identifier for the task
-- runner.
rtrhTaskrunnerId :: Lens' ReportTaskRunnerHeartbeat Text
rtrhTaskrunnerId = lens _rtrhTaskrunnerId (\ s a -> s{_rtrhTaskrunnerId = a});

instance AWSRequest ReportTaskRunnerHeartbeat where
        type Sv ReportTaskRunnerHeartbeat = DataPipeline
        type Rs ReportTaskRunnerHeartbeat =
             ReportTaskRunnerHeartbeatResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ReportTaskRunnerHeartbeatResponse' <$>
                   (x .:> "terminate"))

instance ToHeaders ReportTaskRunnerHeartbeat where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DataPipeline.ReportTaskRunnerHeartbeat" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ReportTaskRunnerHeartbeat where
        toJSON ReportTaskRunnerHeartbeat'{..}
          = object
              ["hostname" .= _rtrhHostname,
               "workerGroup" .= _rtrhWorkerGroup,
               "taskrunnerId" .= _rtrhTaskrunnerId]

instance ToPath ReportTaskRunnerHeartbeat where
        toPath = const "/"

instance ToQuery ReportTaskRunnerHeartbeat where
        toQuery = const mempty

-- | /See:/ 'reportTaskRunnerHeartbeatResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtrhrTerminate'
newtype ReportTaskRunnerHeartbeatResponse = ReportTaskRunnerHeartbeatResponse'{_rtrhrTerminate :: Bool} deriving (Eq, Read, Show)

-- | 'ReportTaskRunnerHeartbeatResponse' smart constructor.
reportTaskRunnerHeartbeatResponse :: Bool -> ReportTaskRunnerHeartbeatResponse
reportTaskRunnerHeartbeatResponse pTerminate = ReportTaskRunnerHeartbeatResponse'{_rtrhrTerminate = pTerminate};

-- | Indicates whether the calling task runner should terminate.
rtrhrTerminate :: Lens' ReportTaskRunnerHeartbeatResponse Bool
rtrhrTerminate = lens _rtrhrTerminate (\ s a -> s{_rtrhrTerminate = a});
