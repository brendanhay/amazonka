{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.V2012_10_29.ReportTaskRunnerHeartbeat
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Task runners call ReportTaskRunnerHeartbeat every 15 minutes to indicate
-- that they are operational. In the case of AWS Data Pipeline Task Runner
-- launched on a resource managed by AWS Data Pipeline, the web service can
-- use this call to detect when the task runner application has failed and
-- restart a new instance. POST / HTTP/1.1 Content-Type:
-- application/x-amz-json-1.1 X-Amz-Target:
-- DataPipeline.ReportTaskRunnerHeartbeat Content-Length: 84 Host:
-- datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012 17:49:52
-- GMT Authorization: AuthParams {"taskrunnerId": "1234567890", "workerGroup":
-- "wg-12345", "hostname": "example.com"} Status: x-amzn-RequestId:
-- b3104dc5-0734-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 20 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"terminate": false}.
module Network.AWS.DataPipeline.V2012_10_29.ReportTaskRunnerHeartbeat
    (
    -- * Request
      ReportTaskRunnerHeartbeat
    -- ** Request constructor
    , mkReportTaskRunnerHeartbeat
    -- ** Request lenses
    , rtrhTaskrunnerId
    , rtrhWorkerGroup
    , rtrhHostname

    -- * Response
    , ReportTaskRunnerHeartbeatResponse
    -- ** Response constructor
    , mkReportTaskRunnerHeartbeatResponse
    -- ** Response lenses
    , rtrhrTerminate
    ) where

import Network.AWS.DataPipeline.V2012_10_29.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The input for the ReportTaskRunnerHeartbeat action.
data ReportTaskRunnerHeartbeat = ReportTaskRunnerHeartbeat
    { _rtrhTaskrunnerId :: Text
    , _rtrhWorkerGroup :: Maybe Text
    , _rtrhHostname :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ReportTaskRunnerHeartbeat' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TaskrunnerId ::@ @Text@
--
-- * @WorkerGroup ::@ @Maybe Text@
--
-- * @Hostname ::@ @Maybe Text@
--
mkReportTaskRunnerHeartbeat :: Text -- ^ 'rtrhTaskrunnerId'
                            -> ReportTaskRunnerHeartbeat
mkReportTaskRunnerHeartbeat p1 = ReportTaskRunnerHeartbeat
    { _rtrhTaskrunnerId = p1
    , _rtrhWorkerGroup = Nothing
    , _rtrhHostname = Nothing
    }

-- | The identifier of the task runner. This value should be unique across your
-- AWS account. In the case of AWS Data Pipeline Task Runner launched on a
-- resource managed by AWS Data Pipeline, the web service provides a unique
-- identifier when it launches the application. If you have written a custom
-- task runner, you should assign a unique identifier for the task runner.
rtrhTaskrunnerId :: Lens' ReportTaskRunnerHeartbeat Text
rtrhTaskrunnerId =
    lens _rtrhTaskrunnerId (\s a -> s { _rtrhTaskrunnerId = a })

-- | Indicates the type of task the task runner is configured to accept and
-- process. The worker group is set as a field on objects in the pipeline when
-- they are created. You can only specify a single value for workerGroup in
-- the call to ReportTaskRunnerHeartbeat. There are no wildcard values
-- permitted in workerGroup, the string must be an exact, case-sensitive,
-- match.
rtrhWorkerGroup :: Lens' ReportTaskRunnerHeartbeat (Maybe Text)
rtrhWorkerGroup = lens _rtrhWorkerGroup (\s a -> s { _rtrhWorkerGroup = a })

-- | The public DNS name of the calling task runner.
rtrhHostname :: Lens' ReportTaskRunnerHeartbeat (Maybe Text)
rtrhHostname = lens _rtrhHostname (\s a -> s { _rtrhHostname = a })

instance ToPath ReportTaskRunnerHeartbeat

instance ToQuery ReportTaskRunnerHeartbeat

instance ToHeaders ReportTaskRunnerHeartbeat

instance ToJSON ReportTaskRunnerHeartbeat

-- | Contains the output from the ReportTaskRunnerHeartbeat action.
newtype ReportTaskRunnerHeartbeatResponse = ReportTaskRunnerHeartbeatResponse
    { _rtrhrTerminate :: Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ReportTaskRunnerHeartbeatResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Terminate ::@ @Bool@
--
mkReportTaskRunnerHeartbeatResponse :: Bool -- ^ 'rtrhrTerminate'
                                    -> ReportTaskRunnerHeartbeatResponse
mkReportTaskRunnerHeartbeatResponse p1 = ReportTaskRunnerHeartbeatResponse
    { _rtrhrTerminate = p1
    }

-- | Indicates whether the calling task runner should terminate. If True, the
-- task runner that called ReportTaskRunnerHeartbeat should terminate.
rtrhrTerminate :: Lens' ReportTaskRunnerHeartbeatResponse Bool
rtrhrTerminate = lens _rtrhrTerminate (\s a -> s { _rtrhrTerminate = a })

instance FromJSON ReportTaskRunnerHeartbeatResponse

instance AWSRequest ReportTaskRunnerHeartbeat where
    type Sv ReportTaskRunnerHeartbeat = DataPipeline
    type Rs ReportTaskRunnerHeartbeat = ReportTaskRunnerHeartbeatResponse

    request = get
    response _ = jsonResponse
