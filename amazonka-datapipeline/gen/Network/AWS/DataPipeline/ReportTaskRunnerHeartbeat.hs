{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.ReportTaskRunnerHeartbeat
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Task runners call 'ReportTaskRunnerHeartbeat' every 15 minutes to indicate that
-- they are operational. If the AWS Data Pipeline Task Runner is launched on a
-- resource managed by AWS Data Pipeline, the web service can use this call to
-- detect when the task runner application has failed and restart a new instance.
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
    , rtrhTaskrunnerId
    , rtrhWorkerGroup

    -- * Response
    , ReportTaskRunnerHeartbeatResponse
    -- ** Response constructor
    , reportTaskRunnerHeartbeatResponse
    -- ** Response lenses
    , rtrhrTerminate
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DataPipeline.Types
import qualified GHC.Exts

data ReportTaskRunnerHeartbeat = ReportTaskRunnerHeartbeat
    { _rtrhHostname     :: Maybe Text
    , _rtrhTaskrunnerId :: Text
    , _rtrhWorkerGroup  :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ReportTaskRunnerHeartbeat' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtrhHostname' @::@ 'Maybe' 'Text'
--
-- * 'rtrhTaskrunnerId' @::@ 'Text'
--
-- * 'rtrhWorkerGroup' @::@ 'Maybe' 'Text'
--
reportTaskRunnerHeartbeat :: Text -- ^ 'rtrhTaskrunnerId'
                          -> ReportTaskRunnerHeartbeat
reportTaskRunnerHeartbeat p1 = ReportTaskRunnerHeartbeat
    { _rtrhTaskrunnerId = p1
    , _rtrhWorkerGroup  = Nothing
    , _rtrhHostname     = Nothing
    }

-- | The public DNS name of the task runner.
rtrhHostname :: Lens' ReportTaskRunnerHeartbeat (Maybe Text)
rtrhHostname = lens _rtrhHostname (\s a -> s { _rtrhHostname = a })

-- | The ID of the task runner. This value should be unique across your AWS
-- account. In the case of AWS Data Pipeline Task Runner launched on a resource
-- managed by AWS Data Pipeline, the web service provides a unique identifier
-- when it launches the application. If you have written a custom task runner,
-- you should assign a unique identifier for the task runner.
rtrhTaskrunnerId :: Lens' ReportTaskRunnerHeartbeat Text
rtrhTaskrunnerId = lens _rtrhTaskrunnerId (\s a -> s { _rtrhTaskrunnerId = a })

-- | The type of task the task runner is configured to accept and process. The
-- worker group is set as a field on objects in the pipeline when they are
-- created. You can only specify a single value for 'workerGroup'. There are no
-- wildcard values permitted in 'workerGroup'; the string must be an exact,
-- case-sensitive, match.
rtrhWorkerGroup :: Lens' ReportTaskRunnerHeartbeat (Maybe Text)
rtrhWorkerGroup = lens _rtrhWorkerGroup (\s a -> s { _rtrhWorkerGroup = a })

newtype ReportTaskRunnerHeartbeatResponse = ReportTaskRunnerHeartbeatResponse
    { _rtrhrTerminate :: Bool
    } deriving (Eq, Ord, Read, Show, Enum)

-- | 'ReportTaskRunnerHeartbeatResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtrhrTerminate' @::@ 'Bool'
--
reportTaskRunnerHeartbeatResponse :: Bool -- ^ 'rtrhrTerminate'
                                  -> ReportTaskRunnerHeartbeatResponse
reportTaskRunnerHeartbeatResponse p1 = ReportTaskRunnerHeartbeatResponse
    { _rtrhrTerminate = p1
    }

-- | Indicates whether the calling task runner should terminate.
rtrhrTerminate :: Lens' ReportTaskRunnerHeartbeatResponse Bool
rtrhrTerminate = lens _rtrhrTerminate (\s a -> s { _rtrhrTerminate = a })

instance ToPath ReportTaskRunnerHeartbeat where
    toPath = const "/"

instance ToQuery ReportTaskRunnerHeartbeat where
    toQuery = const mempty

instance ToHeaders ReportTaskRunnerHeartbeat

instance ToJSON ReportTaskRunnerHeartbeat where
    toJSON ReportTaskRunnerHeartbeat{..} = object
        [ "taskrunnerId" .= _rtrhTaskrunnerId
        , "workerGroup"  .= _rtrhWorkerGroup
        , "hostname"     .= _rtrhHostname
        ]

instance AWSRequest ReportTaskRunnerHeartbeat where
    type Sv ReportTaskRunnerHeartbeat = DataPipeline
    type Rs ReportTaskRunnerHeartbeat = ReportTaskRunnerHeartbeatResponse

    request  = post "ReportTaskRunnerHeartbeat"
    response = jsonResponse

instance FromJSON ReportTaskRunnerHeartbeatResponse where
    parseJSON = withObject "ReportTaskRunnerHeartbeatResponse" $ \o -> ReportTaskRunnerHeartbeatResponse
        <$> o .:  "terminate"
