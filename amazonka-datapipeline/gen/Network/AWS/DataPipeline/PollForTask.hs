{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline
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
-- response, and this may take up to 90 seconds. POST / HTTP/1.1 Content-Type:
-- application/x-amz-json-1.1 X-Amz-Target: DataPipeline.PollForTask
-- Content-Length: 59 Host: datapipeline.us-east-1.amazonaws.com X-Amz-Date:
-- Mon, 12 Nov 2012 17:49:52 GMT Authorization: AuthParams {"workerGroup":
-- "MyworkerGroup", "hostname": "example.com"} x-amzn-RequestId:
-- 41c713d2-0775-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 39 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"taskObject": {"attemptId":
-- "@SayHello_2012-12-12T00:00:00_Attempt=1", "objects":
-- {"@SayHello_2012-12-12T00:00:00_Attempt=1": {"fields": [ {"key":
-- "@componentParent", "refValue": "SayHello"}, {"key": "@scheduledStartTime",
-- "stringValue": "2012-12-12T00:00:00"}, {"key": "parent", "refValue":
-- "SayHello"}, {"key": "@sphere", "stringValue": "ATTEMPT"}, {"key":
-- "workerGroup", "stringValue": "workerGroup"}, {"key": "@instanceParent",
-- "refValue": "@SayHello_2012-12-12T00:00:00"}, {"key": "type",
-- "stringValue": "ShellCommandActivity"}, {"key": "@status", "stringValue":
-- "WAITING_FOR_RUNNER"}, {"key": "@version", "stringValue": "1"}, {"key":
-- "schedule", "refValue": "Schedule"}, {"key": "@actualStartTime",
-- "stringValue": "2012-12-13T01:40:50"}, {"key": "command", "stringValue":
-- "echo hello"}, {"key": "@scheduledEndTime", "stringValue":
-- "2012-12-12T01:00:00"}, {"key": "@activeInstances", "refValue":
-- "@SayHello_2012-12-12T00:00:00"}, {"key": "@pipelineId", "stringValue":
-- "df-0937003356ZJEXAMPLE"} ], "id":
-- "@SayHello_2012-12-12T00:00:00_Attempt=1", "name":
-- "@SayHello_2012-12-12T00:00:00_Attempt=1"} }, "pipelineId":
-- "df-0937003356ZJEXAMPLE", "taskId":
-- "2xaM4wRs5zOsIH+g9U3oVHfAgAlbSqU6XduncB0HhZ3xMnmvfePZPn4dIbYXHyWyRK+cU15MqDHwdrvftx/4wv+sNS4w34vJfv7QA9aOoOazW28l1GYSb2ZRR0N0paiQp+d1MhSKo10hOTWOsVK5S5Lnx9Qm6omFgXHyIvZRIvTlrQMpr1xuUrflyGOfbFOGpOLpvPE172MYdqpZKnbSS4TcuqgQKSWV2833fEubI57DPOP7ghWa2TcYeSIv4pdLYG53fTuwfbnbdc98g2LNUQzSVhSnt7BoqyNwht2aQ6b/UHg9A80+KVpuXuqmz3m1MXwHFgxjdmuesXNOrrlGpeLCcRWD+aGo0RN1NqhQRzNAig8V4GlaPTQzMsRCljKqvrIyAoP3Tt2XEGsHkkQo12rEX8Z90957XX2qKRwhruwYzqGkSLWjINoLdAxUJdpRXRc5DJTrBd3D5mdzn7kY1l7NEh4kFHJDt3Cx4Z3Mk8MYCACyCk/CEyy9DwuPi66cLz0NBcgbCM5LKjTBOwo1m+am+pvM1kSposE9FPP1+RFGb8k6jQBTJx3TRz1yKilnGXQTZ5xvdOFpJrklIT0OXP1MG3+auM9FlJA+1dX90QoNJE5z7axmK//MOGXUdkqFe2kiDkorqjxwDvc0Js9pVKfKvAmW8YqUbmI9l0ERpWCXXnLVHNmPWz3jaPY+OBAmuJWDmxB/Z8p94aEDg4BVXQ7LvsKQ3DLYhaB7yJ390CJT+i0mm+EBqY60V6YikPSWDFrYQ/NPi2b1DgE19mX8zHqw8qprIl4yh1Ckx2Iige4En/N5ktOoIxnASxAw/TzcE2skxdw5KlHDF+UTj71m16CR/dIaKlXijlfNlNzUBo/bNSadCQn3G5NoO501wPKI:XO50TgDNyo8EXAMPLE/g==:1"}
-- }.
module Network.AWS.DataPipeline
    (
    -- * Request
      PollForTask
    -- ** Request constructor
    , mkPollForTask
    -- ** Request lenses
    , pftWorkerGroup
    , pftHostname
    , pftInstanceIdentity

    -- * Response
    , PollForTaskResponse
    -- ** Response constructor
    , mkPollForTaskResponse
    -- ** Response lenses
    , pftrTaskObject
    ) where

import Network.AWS.DataPipeline.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The data type passed in as input to the PollForTask action.
data PollForTask = PollForTask
    { _pftWorkerGroup :: !Text
    , _pftHostname :: !(Maybe Text)
    , _pftInstanceIdentity :: Maybe InstanceIdentity
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PollForTask' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @WorkerGroup ::@ @Text@
--
-- * @Hostname ::@ @Maybe Text@
--
-- * @InstanceIdentity ::@ @Maybe InstanceIdentity@
--
mkPollForTask :: Text -- ^ 'pftWorkerGroup'
              -> PollForTask
mkPollForTask p1 = PollForTask
    { _pftWorkerGroup = p1
    , _pftHostname = Nothing
    , _pftInstanceIdentity = Nothing
    }

-- | Indicates the type of task the task runner is configured to accept and
-- process. The worker group is set as a field on objects in the pipeline when
-- they are created. You can only specify a single value for workerGroup in
-- the call to PollForTask. There are no wildcard values permitted in
-- workerGroup, the string must be an exact, case-sensitive, match.
pftWorkerGroup :: Lens' PollForTask Text
pftWorkerGroup = lens _pftWorkerGroup (\s a -> s { _pftWorkerGroup = a })

-- | The public DNS name of the calling task runner.
pftHostname :: Lens' PollForTask (Maybe Text)
pftHostname = lens _pftHostname (\s a -> s { _pftHostname = a })

-- | Identity information for the Amazon EC2 instance that is hosting the task
-- runner. You can get this value by calling the URI,
-- http://169.254.169.254/latest/meta-data/instance-id, from the EC2 instance.
-- For more information, go to Instance Metadata in the Amazon Elastic Compute
-- Cloud User Guide. Passing in this value proves that your task runner is
-- running on an EC2 instance, and ensures the proper AWS Data Pipeline
-- service charges are applied to your pipeline.
pftInstanceIdentity :: Lens' PollForTask (Maybe InstanceIdentity)
pftInstanceIdentity =
    lens _pftInstanceIdentity (\s a -> s { _pftInstanceIdentity = a })

instance ToPath PollForTask

instance ToQuery PollForTask

instance ToHeaders PollForTask

instance ToJSON PollForTask

-- | Contains the output from the PollForTask action.
newtype PollForTaskResponse = PollForTaskResponse
    { _pftrTaskObject :: Maybe TaskObject
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PollForTaskResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TaskObject ::@ @Maybe TaskObject@
--
mkPollForTaskResponse :: PollForTaskResponse
mkPollForTaskResponse = PollForTaskResponse
    { _pftrTaskObject = Nothing
    }

-- | An instance of PollForTaskResult, which contains an instance of TaskObject.
-- The returned object contains all the information needed to complete the
-- task that is being assigned to the task runner. One of the fields returned
-- in this object is taskId, which contains an identifier for the task being
-- assigned. The calling task runner uses taskId in subsequent calls to
-- ReportTaskProgress and SetTaskStatus.
pftrTaskObject :: Lens' PollForTaskResponse (Maybe TaskObject)
pftrTaskObject = lens _pftrTaskObject (\s a -> s { _pftrTaskObject = a })

instance FromJSON PollForTaskResponse

instance AWSRequest PollForTask where
    type Sv PollForTask = DataPipeline
    type Rs PollForTask = PollForTaskResponse

    request = get
    response _ = jsonResponse
