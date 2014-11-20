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

-- Module      : Network.AWS.DataPipeline.PollForTask
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Task runners call this action to receive a task to perform from AWS Data
-- Pipeline. The task runner specifies which tasks it can perform by setting a
-- value for the workerGroup parameter of the PollForTask call. The task
-- returned by PollForTask may come from any of the pipelines that match the
-- workerGroup value passed in by the task runner and that was launched using
-- the IAM user credentials specified by the task runner. If tasks are ready
-- in the work queue, PollForTask returns a response immediately. If no tasks
-- are available in the queue, PollForTask uses long-polling and holds on to a
-- poll connection for up to a 90 seconds during which time the first newly
-- scheduled task is handed to the task runner. To accomodate this, set the
-- socket timeout in your task runner to 90 seconds. The task runner should
-- not call PollForTask again on the same workerGroup until it receives a
-- response, and this may take up to 90 seconds.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_PollForTask.html>
module Network.AWS.DataPipeline.PollForTask
    (
    -- * Request
      PollForTask
    -- ** Request constructor
    , pollForTask
    -- ** Request lenses
    , pftHostname
    , pftInstanceIdentity
    , pftWorkerGroup

    -- * Response
    , PollForTaskResponse
    -- ** Response constructor
    , pollForTaskResponse
    -- ** Response lenses
    , pftrTaskObject
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DataPipeline.Types
import qualified GHC.Exts

data PollForTask = PollForTask
    { _pftHostname         :: Maybe Text
    , _pftInstanceIdentity :: Maybe InstanceIdentity
    , _pftWorkerGroup      :: Text
    } deriving (Eq, Show)

-- | 'PollForTask' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pftHostname' @::@ 'Maybe' 'Text'
--
-- * 'pftInstanceIdentity' @::@ 'Maybe' 'InstanceIdentity'
--
-- * 'pftWorkerGroup' @::@ 'Text'
--
pollForTask :: Text -- ^ 'pftWorkerGroup'
            -> PollForTask
pollForTask p1 = PollForTask
    { _pftWorkerGroup      = p1
    , _pftHostname         = Nothing
    , _pftInstanceIdentity = Nothing
    }

-- | The public DNS name of the calling task runner.
pftHostname :: Lens' PollForTask (Maybe Text)
pftHostname = lens _pftHostname (\s a -> s { _pftHostname = a })

-- | Identity information for the Amazon EC2 instance that is hosting the task
-- runner. You can get this value by calling the URI,
-- http://169.254.169.254/latest/meta-data/instance-id, from the EC2
-- instance. For more information, go to Instance Metadata in the Amazon
-- Elastic Compute Cloud User Guide. Passing in this value proves that your
-- task runner is running on an EC2 instance, and ensures the proper AWS
-- Data Pipeline service charges are applied to your pipeline.
pftInstanceIdentity :: Lens' PollForTask (Maybe InstanceIdentity)
pftInstanceIdentity =
    lens _pftInstanceIdentity (\s a -> s { _pftInstanceIdentity = a })

-- | Indicates the type of task the task runner is configured to accept and
-- process. The worker group is set as a field on objects in the pipeline
-- when they are created. You can only specify a single value for
-- workerGroup in the call to PollForTask. There are no wildcard values
-- permitted in workerGroup, the string must be an exact, case-sensitive,
-- match.
pftWorkerGroup :: Lens' PollForTask Text
pftWorkerGroup = lens _pftWorkerGroup (\s a -> s { _pftWorkerGroup = a })

newtype PollForTaskResponse = PollForTaskResponse
    { _pftrTaskObject :: Maybe TaskObject
    } deriving (Eq, Show)

-- | 'PollForTaskResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pftrTaskObject' @::@ 'Maybe' 'TaskObject'
--
pollForTaskResponse :: PollForTaskResponse
pollForTaskResponse = PollForTaskResponse
    { _pftrTaskObject = Nothing
    }

-- | An instance of PollForTaskResult, which contains an instance of
-- TaskObject. The returned object contains all the information needed to
-- complete the task that is being assigned to the task runner. One of the
-- fields returned in this object is taskId, which contains an identifier
-- for the task being assigned. The calling task runner uses taskId in
-- subsequent calls to ReportTaskProgress and SetTaskStatus.
pftrTaskObject :: Lens' PollForTaskResponse (Maybe TaskObject)
pftrTaskObject = lens _pftrTaskObject (\s a -> s { _pftrTaskObject = a })

instance ToPath PollForTask where
    toPath = const "/"

instance ToQuery PollForTask where
    toQuery = const mempty

instance ToHeaders PollForTask

instance ToJSON PollForTask where
    toJSON PollForTask{..} = object
        [ "workerGroup"      .= _pftWorkerGroup
        , "hostname"         .= _pftHostname
        , "instanceIdentity" .= _pftInstanceIdentity
        ]

instance AWSRequest PollForTask where
    type Sv PollForTask = DataPipeline
    type Rs PollForTask = PollForTaskResponse

    request  = post "PollForTask"
    response = jsonResponse

instance FromJSON PollForTaskResponse where
    parseJSON = withObject "PollForTaskResponse" $ \o -> PollForTaskResponse
        <$> o .:? "taskObject"


Some kind of operator / class to check the types whether to continue?
