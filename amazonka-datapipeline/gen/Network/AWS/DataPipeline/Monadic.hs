{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.DataPipeline.Monadic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This module is provided for convenience. It offers an alternative to the
-- common idiom of supplying required fields to an operations's smart constructor,
-- using the operation's lenses to modify additional fields, and then sending
-- or paginating the request.
--
-- As an example: using "Network.AWS.DataPipeline" with the smart constructor and
-- basic lens syntax, before explicitly calling 'send':
--
-- @
-- import Control.Monad.Trans.AWS
-- import Network.AWS.DataPipeline
--
-- send $ (mkOperationName w x)
--      & onLensField1 .~ y
--      & onLensField2 .~ z
-- @
--
-- Versus using "Network.AWS.DataPipeline.Monadic" with the 'State' operator variants from
-- "Control.Lens.Setter" such as '.=' to modify any additional request
-- parameters before sending:
--
-- @
-- import Control.Applicative
-- import Network.AWS.DataPipeline.Monadic
--
-- operationName w x $ do
--     onLensField1 .= y
--     onLensField2 .= z
--
-- -- Or to void any additional parameters outside of those required using 'Control.Applicative.empty':
-- operationName w x empty
-- @
--
module Network.AWS.DataPipeline.Monadic
    (
    -- * ActivatePipeline
    -- $ActivatePipeline
      activatePipeline
    , activatePipelineCatch

    -- * CreatePipeline
    -- $CreatePipeline
    , createPipeline
    , createPipelineCatch

    -- * DeletePipeline
    -- $DeletePipeline
    , deletePipeline
    , deletePipelineCatch

    -- * DescribeObjects
    -- $DescribeObjects
    , describeObjects
    , describeObjectsCatch

    -- * DescribePipelines
    -- $DescribePipelines
    , describePipelines
    , describePipelinesCatch

    -- * EvaluateExpression
    -- $EvaluateExpression
    , evaluateExpression
    , evaluateExpressionCatch

    -- * GetPipelineDefinition
    -- $GetPipelineDefinition
    , getPipelineDefinition
    , getPipelineDefinitionCatch

    -- * ListPipelines
    -- $ListPipelines
    , listPipelines
    , listPipelinesCatch

    -- * PollForTask
    -- $PollForTask
    , pollForTask
    , pollForTaskCatch

    -- * PutPipelineDefinition
    -- $PutPipelineDefinition
    , putPipelineDefinition
    , putPipelineDefinitionCatch

    -- * QueryObjects
    -- $QueryObjects
    , queryObjects
    , queryObjectsCatch

    -- * ReportTaskProgress
    -- $ReportTaskProgress
    , reportTaskProgress
    , reportTaskProgressCatch

    -- * ReportTaskRunnerHeartbeat
    -- $ReportTaskRunnerHeartbeat
    , reportTaskRunnerHeartbeat
    , reportTaskRunnerHeartbeatCatch

    -- * SetStatus
    -- $SetStatus
    , setStatus
    , setStatusCatch

    -- * SetTaskStatus
    -- $SetTaskStatus
    , setTaskStatus
    , setTaskStatusCatch

    -- * ValidatePipelineDefinition
    -- $ValidatePipelineDefinition
    , validatePipelineDefinition
    , validatePipelineDefinitionCatch

    -- * Re-exported
    , module Network.AWS.DataPipeline

    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.DataPipeline

type ServiceEr = Er DataPipeline

-- $ActivatePipeline
-- Validates a pipeline and initiates processing. If the pipeline does not
-- pass validation, activation fails. Call this action to start processing
-- pipeline tasks of a pipeline you've created using the CreatePipeline and
-- PutPipelineDefinition actions. A pipeline cannot be modified after it has
-- been successfully activated. POST / HTTP/1.1 Content-Type:
-- application/x-amz-json-1.1 X-Amz-Target: DataPipeline.ActivatePipeline
-- Content-Length: 39 Host: datapipeline.us-east-1.amazonaws.com X-Amz-Date:
-- Mon, 12 Nov 2012 17:49:52 GMT Authorization: AuthParams {"pipelineId":
-- "df-06372391ZG65EXAMPLE"} HTTP/1.1 200 x-amzn-RequestId:
-- ee19d5bf-074e-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 2 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {}.
--
-- See: 'Network.AWS.DataPipeline.ActivatePipeline'

activatePipeline :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'apPipelineId'
    -> m ActivatePipelineResponse
activatePipeline p1 =
    send (mkActivatePipeline p1)

activatePipelineCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'apPipelineId'
    -> m (Either ServiceEr ActivatePipelineResponse)
activatePipelineCatch p1 =
    sendCatch (mkActivatePipeline p1)

-- $CreatePipeline
-- Creates a new empty pipeline. When this action succeeds, you can then use
-- the PutPipelineDefinition action to populate the pipeline. POST / HTTP/1.1
-- Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- DataPipeline.CreatePipeline Content-Length: 91 Host:
-- datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012 17:49:52
-- GMT Authorization: AuthParams {"name": "myPipeline", "uniqueId":
-- "123456789", "description": "This is my first pipeline"} HTTP/1.1 200
-- x-amzn-RequestId: b16911ce-0774-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 40 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"pipelineId": "df-06372391ZG65EXAMPLE"}.
--
-- See: 'Network.AWS.DataPipeline.CreatePipeline'

createPipeline :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'cpName'
    -> Text -- ^ 'cpUniqueId'
    -> State CreatePipeline a
    -> m CreatePipelineResponse
createPipeline p1 p2 s =
    send $ (mkCreatePipeline p1 p2) &~ s

createPipelineCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'cpName'
    -> Text -- ^ 'cpUniqueId'
    -> State CreatePipeline a
    -> m (Either ServiceEr CreatePipelineResponse)
createPipelineCatch p1 p2 s =
    sendCatch $ (mkCreatePipeline p1 p2) &~ s

-- $DeletePipeline
-- Permanently deletes a pipeline, its pipeline definition and its run
-- history. You cannot query or restore a deleted pipeline. AWS Data Pipeline
-- will attempt to cancel instances associated with the pipeline that are
-- currently being processed by task runners. Deleting a pipeline cannot be
-- undone. To temporarily pause a pipeline instead of deleting it, call
-- SetStatus with the status set to Pause on individual components. Components
-- that are paused by SetStatus can be resumed. POST / HTTP/1.1 Content-Type:
-- application/x-amz-json-1.1 X-Amz-Target: DataPipeline.DeletePipeline
-- Content-Length: 50 Host: datapipeline.us-east-1.amazonaws.com X-Amz-Date:
-- Mon, 12 Nov 2012 17:49:52 GMT Authorization: AuthParams {"pipelineId":
-- "df-06372391ZG65EXAMPLE"} x-amzn-RequestId:
-- b7a88c81-0754-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 0 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT Unexpected response: 200, OK, undefined.
--
-- See: 'Network.AWS.DataPipeline.DeletePipeline'

deletePipeline :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'dpPipelineId'
    -> m DeletePipelineResponse
deletePipeline p1 =
    send (mkDeletePipeline p1)

deletePipelineCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'dpPipelineId'
    -> m (Either ServiceEr DeletePipelineResponse)
deletePipelineCatch p1 =
    sendCatch (mkDeletePipeline p1)

-- $DescribeObjects
-- Returns the object definitions for a set of objects associated with the
-- pipeline. Object definitions are composed of a set of fields that define
-- the properties of the object. POST / HTTP/1.1 Content-Type:
-- application/x-amz-json-1.1 X-Amz-Target: DataPipeline.DescribeObjects
-- Content-Length: 98 Host: datapipeline.us-east-1.amazonaws.com X-Amz-Date:
-- Mon, 12 Nov 2012 17:49:52 GMT Authorization: AuthParams {"pipelineId":
-- "df-06372391ZG65EXAMPLE", "objectIds": ["Schedule"], "evaluateExpressions":
-- true} x-amzn-RequestId: 4c18ea5d-0777-11e2-8a14-21bb8a1f50ef Content-Type:
-- application/x-amz-json-1.1 Content-Length: 1488 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"hasMoreResults": false, "pipelineObjects": [ {"fields": [
-- {"key": "startDateTime", "stringValue": "2012-12-12T00:00:00"}, {"key":
-- "parent", "refValue": "Default"}, {"key": "@sphere", "stringValue":
-- "COMPONENT"}, {"key": "type", "stringValue": "Schedule"}, {"key": "period",
-- "stringValue": "1 hour"}, {"key": "endDateTime", "stringValue":
-- "2012-12-21T18:00:00"}, {"key": "@version", "stringValue": "1"}, {"key":
-- "@status", "stringValue": "PENDING"}, {"key": "@pipelineId", "stringValue":
-- "df-06372391ZG65EXAMPLE"} ], "id": "Schedule", "name": "Schedule"} ] }.
--
-- See: 'Network.AWS.DataPipeline.DescribeObjects'

describeObjects :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => Text -- ^ 'doPipelineId'
    -> [Text] -- ^ 'doObjectIds'
    -> State DescribeObjects a
    -> Source m DescribeObjectsResponse
describeObjects p1 p2 s =
    paginate $ (mkDescribeObjects p1 p2) &~ s

describeObjectsCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => Text -- ^ 'doPipelineId'
    -> [Text] -- ^ 'doObjectIds'
    -> State DescribeObjects a
    -> Source m (Either ServiceEr DescribeObjectsResponse)
describeObjectsCatch p1 p2 s =
    paginateCatch $ (mkDescribeObjects p1 p2) &~ s

-- $DescribePipelines
-- Retrieve metadata about one or more pipelines. The information retrieved
-- includes the name of the pipeline, the pipeline identifier, its current
-- state, and the user account that owns the pipeline. Using account
-- credentials, you can retrieve metadata about pipelines that you or your IAM
-- users have created. If you are using an IAM user account, you can retrieve
-- metadata about only those pipelines you have read permission for. To
-- retrieve the full pipeline definition instead of metadata about the
-- pipeline, call the GetPipelineDefinition action. POST / HTTP/1.1
-- Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- DataPipeline.DescribePipelines Content-Length: 70 Host:
-- datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012 17:49:52
-- GMT Authorization: AuthParams {"pipelineIds": ["df-08785951KAKJEXAMPLE"] }
-- x-amzn-RequestId: 02870eb7-0736-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 767 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"pipelineDescriptionList": [ {"description": "This is my
-- first pipeline", "fields": [ {"key": "@pipelineState", "stringValue":
-- "SCHEDULED"}, {"key": "description", "stringValue": "This is my first
-- pipeline"}, {"key": "name", "stringValue": "myPipeline"}, {"key":
-- "@creationTime", "stringValue": "2012-12-13T01:24:06"}, {"key": "@id",
-- "stringValue": "df-0937003356ZJEXAMPLE"}, {"key": "@sphere", "stringValue":
-- "PIPELINE"}, {"key": "@version", "stringValue": "1"}, {"key": "@userId",
-- "stringValue": "924374875933"}, {"key": "@accountId", "stringValue":
-- "924374875933"}, {"key": "uniqueId", "stringValue": "1234567890"} ],
-- "name": "myPipeline", "pipelineId": "df-0937003356ZJEXAMPLE"} ] }.
--
-- See: 'Network.AWS.DataPipeline.DescribePipelines'

describePipelines :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
    => [Text] -- ^ 'dp1PipelineIds'
    -> m DescribePipelinesResponse
describePipelines p1 =
    send (mkDescribePipelines p1)

describePipelinesCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env m
                          )
    => [Text] -- ^ 'dp1PipelineIds'
    -> m (Either ServiceEr DescribePipelinesResponse)
describePipelinesCatch p1 =
    sendCatch (mkDescribePipelines p1)

-- $EvaluateExpression
-- Evaluates a string in the context of a specified object. A task runner can
-- use this action to evaluate SQL queries stored in Amazon S3. POST /
-- HTTP/1.1 Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- DataPipeline.DescribePipelines Content-Length: 164 Host:
-- datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012 17:49:52
-- GMT Authorization: AuthParams {"pipelineId": "df-08785951KAKJEXAMPLE",
-- "objectId": "Schedule", "expression": "Transform started at
-- #{startDateTime} and finished at #{endDateTime}"} x-amzn-RequestId:
-- 02870eb7-0736-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 103 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"evaluatedExpression": "Transform started at
-- 2012-12-12T00:00:00 and finished at 2012-12-21T18:00:00"}.
--
-- See: 'Network.AWS.DataPipeline.EvaluateExpression'

evaluateExpression :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => Text -- ^ 'eePipelineId'
    -> Text -- ^ 'eeObjectId'
    -> Text -- ^ 'eeExpression'
    -> m EvaluateExpressionResponse
evaluateExpression p1 p2 p3 =
    send (mkEvaluateExpression p1 p2 p3)

evaluateExpressionCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => Text -- ^ 'eePipelineId'
    -> Text -- ^ 'eeObjectId'
    -> Text -- ^ 'eeExpression'
    -> m (Either ServiceEr EvaluateExpressionResponse)
evaluateExpressionCatch p1 p2 p3 =
    sendCatch (mkEvaluateExpression p1 p2 p3)

-- $GetPipelineDefinition
-- Returns the definition of the specified pipeline. You can call
-- GetPipelineDefinition to retrieve the pipeline definition you provided
-- using PutPipelineDefinition. POST / HTTP/1.1 Content-Type:
-- application/x-amz-json-1.1 X-Amz-Target: DataPipeline.GetPipelineDefinition
-- Content-Length: 40 Host: datapipeline.us-east-1.amazonaws.com X-Amz-Date:
-- Mon, 12 Nov 2012 17:49:52 GMT Authorization: AuthParams {"pipelineId":
-- "df-06372391ZG65EXAMPLE"} x-amzn-RequestId:
-- e28309e5-0776-11e2-8a14-21bb8a1f50ef Content-Type:
-- application/x-amz-json-1.1 Content-Length: 890 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"pipelineObjects": [ {"fields": [ {"key": "workerGroup",
-- "stringValue": "workerGroup"} ], "id": "Default", "name": "Default"},
-- {"fields": [ {"key": "startDateTime", "stringValue":
-- "2012-09-25T17:00:00"}, {"key": "type", "stringValue": "Schedule"}, {"key":
-- "period", "stringValue": "1 hour"}, {"key": "endDateTime", "stringValue":
-- "2012-09-25T18:00:00"} ], "id": "Schedule", "name": "Schedule"}, {"fields":
-- [ {"key": "schedule", "refValue": "Schedule"}, {"key": "command",
-- "stringValue": "echo hello"}, {"key": "parent", "refValue": "Default"},
-- {"key": "type", "stringValue": "ShellCommandActivity"} ], "id": "SayHello",
-- "name": "SayHello"} ] }.
--
-- See: 'Network.AWS.DataPipeline.GetPipelineDefinition'

getPipelineDefinition :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => Text -- ^ 'gpdPipelineId'
    -> State GetPipelineDefinition a
    -> m GetPipelineDefinitionResponse
getPipelineDefinition p1 s =
    send $ (mkGetPipelineDefinition p1) &~ s

getPipelineDefinitionCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => Text -- ^ 'gpdPipelineId'
    -> State GetPipelineDefinition a
    -> m (Either ServiceEr GetPipelineDefinitionResponse)
getPipelineDefinitionCatch p1 s =
    sendCatch $ (mkGetPipelineDefinition p1) &~ s

-- $ListPipelines
-- Returns a list of pipeline identifiers for all active pipelines.
-- Identifiers are returned only for pipelines you have permission to access.
-- POST / HTTP/1.1 Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- DataPipeline.ListPipelines Content-Length: 14 Host:
-- datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012 17:49:52
-- GMT Authorization: AuthParams {} Status: x-amzn-RequestId:
-- b3104dc5-0734-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 39 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"PipelineIdList": [ {"id": "df-08785951KAKJEXAMPLE", "name":
-- "MyPipeline"}, {"id": "df-08662578ISYEXAMPLE", "name": "MySecondPipeline"}
-- ] }.
--
-- See: 'Network.AWS.DataPipeline.ListPipelines'

listPipelines :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 )
    => State ListPipelines a
    -> Source m ListPipelinesResponse
listPipelines s =
    paginate (mkListPipelines &~ s)

listPipelinesCatch :: ( MonadCatch m
                      , MonadResource m
                      , MonadReader Env m
                      )
    => State ListPipelines a
    -> Source m (Either ServiceEr ListPipelinesResponse)
listPipelinesCatch s =
    paginateCatch (mkListPipelines &~ s)

-- $PollForTask
-- Task runners call this action to receive a task to perform from AWS Data
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
--
-- See: 'Network.AWS.DataPipeline.PollForTask'

pollForTask :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
    => Text -- ^ 'pftWorkerGroup'
    -> State PollForTask a
    -> m PollForTaskResponse
pollForTask p1 s =
    send $ (mkPollForTask p1) &~ s

pollForTaskCatch :: ( MonadCatch m
                    , MonadResource m
                    , MonadReader Env m
                    )
    => Text -- ^ 'pftWorkerGroup'
    -> State PollForTask a
    -> m (Either ServiceEr PollForTaskResponse)
pollForTaskCatch p1 s =
    sendCatch $ (mkPollForTask p1) &~ s

-- $PutPipelineDefinition
-- Adds tasks, schedules, and preconditions that control the behavior of the
-- pipeline. You can use PutPipelineDefinition to populate a new pipeline.
-- PutPipelineDefinition also validates the configuration as it adds it to the
-- pipeline. Changes to the pipeline are saved unless one of the following
-- three validation errors exists in the pipeline. An object is missing a name
-- or identifier field. A string or reference field is empty. The number of
-- objects in the pipeline exceeds the maximum allowed objects. Pipeline
-- object definitions are passed to the PutPipelineDefinition action and
-- returned by the GetPipelineDefinition action. Example 1 This example sets
-- an valid pipeline configuration and returns success. POST / HTTP/1.1
-- Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- DataPipeline.PutPipelineDefinition Content-Length: 914 Host:
-- datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012 17:49:52
-- GMT Authorization: AuthParams {"pipelineId": "df-0937003356ZJEXAMPLE",
-- "pipelineObjects": [ {"id": "Default", "name": "Default", "fields": [
-- {"key": "workerGroup", "stringValue": "workerGroup"} ] }, {"id":
-- "Schedule", "name": "Schedule", "fields": [ {"key": "startDateTime",
-- "stringValue": "2012-12-12T00:00:00"}, {"key": "type", "stringValue":
-- "Schedule"}, {"key": "period", "stringValue": "1 hour"}, {"key":
-- "endDateTime", "stringValue": "2012-12-21T18:00:00"} ] }, {"id":
-- "SayHello", "name": "SayHello", "fields": [ {"key": "type", "stringValue":
-- "ShellCommandActivity"}, {"key": "command", "stringValue": "echo hello"},
-- {"key": "parent", "refValue": "Default"}, {"key": "schedule", "refValue":
-- "Schedule"} ] } ] } HTTP/1.1 200 x-amzn-RequestId:
-- f74afc14-0754-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 18 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"errored": false} Example 2 This example sets an invalid
-- pipeline configuration (the value for workerGroup is an empty string) and
-- returns an error message. POST / HTTP/1.1 Content-Type:
-- application/x-amz-json-1.1 X-Amz-Target: DataPipeline.PutPipelineDefinition
-- Content-Length: 903 Host: datapipeline.us-east-1.amazonaws.com X-Amz-Date:
-- Mon, 12 Nov 2012 17:49:52 GMT Authorization: AuthParams {"pipelineId":
-- "df-06372391ZG65EXAMPLE", "pipelineObjects": [ {"id": "Default", "name":
-- "Default", "fields": [ {"key": "workerGroup", "stringValue": ""} ] },
-- {"id": "Schedule", "name": "Schedule", "fields": [ {"key": "startDateTime",
-- "stringValue": "2012-09-25T17:00:00"}, {"key": "type", "stringValue":
-- "Schedule"}, {"key": "period", "stringValue": "1 hour"}, {"key":
-- "endDateTime", "stringValue": "2012-09-25T18:00:00"} ] }, {"id":
-- "SayHello", "name": "SayHello", "fields": [ {"key": "type", "stringValue":
-- "ShellCommandActivity"}, {"key": "command", "stringValue": "echo hello"},
-- {"key": "parent", "refValue": "Default"}, {"key": "schedule", "refValue":
-- "Schedule"} ] } ] } HTTP/1.1 200 x-amzn-RequestId:
-- f74afc14-0754-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 18 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"__type":
-- "com.amazon.setl.webservice#InvalidRequestException", "message": "Pipeline
-- definition has errors: Could not save the pipeline definition due to FATAL
-- errors: [com.amazon.setl.webservice.ValidationError@108d7ea9] Please call
-- Validate to validate your pipeline"}.
--
-- See: 'Network.AWS.DataPipeline.PutPipelineDefinition'

putPipelineDefinition :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => Text -- ^ 'ppdPipelineId'
    -> [PipelineObject] -- ^ 'ppdPipelineObjects'
    -> m PutPipelineDefinitionResponse
putPipelineDefinition p1 p2 =
    send (mkPutPipelineDefinition p1 p2)

putPipelineDefinitionCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => Text -- ^ 'ppdPipelineId'
    -> [PipelineObject] -- ^ 'ppdPipelineObjects'
    -> m (Either ServiceEr PutPipelineDefinitionResponse)
putPipelineDefinitionCatch p1 p2 =
    sendCatch (mkPutPipelineDefinition p1 p2)

-- $QueryObjects
-- Queries a pipeline for the names of objects that match a specified set of
-- conditions. The objects returned by QueryObjects are paginated and then
-- filtered by the value you set for query. This means the action may return
-- an empty result set with a value set for marker. If HasMoreResults is set
-- to True, you should continue to call QueryObjects, passing in the returned
-- value for marker, until HasMoreResults returns False. POST / HTTP/1.1
-- Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- DataPipeline.QueryObjects Content-Length: 123 Host:
-- datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012 17:49:52
-- GMT Authorization: AuthParams {"pipelineId": "df-06372391ZG65EXAMPLE",
-- "query": {"selectors": [ ] }, "sphere": "PO", "marker": "", "limit": 10}
-- x-amzn-RequestId: 14d704c1-0775-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 72 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"hasMoreResults": false, "ids":
-- ["@SayHello_1_2012-09-25T17:00:00"] }.
--
-- See: 'Network.AWS.DataPipeline.QueryObjects'

queryObjects :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => Text -- ^ 'qoPipelineId'
    -> Text -- ^ 'qoSphere'
    -> State QueryObjects a
    -> Source m QueryObjectsResponse
queryObjects p1 p3 s =
    paginate $ (mkQueryObjects p1 p3) &~ s

queryObjectsCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => Text -- ^ 'qoPipelineId'
    -> Text -- ^ 'qoSphere'
    -> State QueryObjects a
    -> Source m (Either ServiceEr QueryObjectsResponse)
queryObjectsCatch p1 p3 s =
    paginateCatch $ (mkQueryObjects p1 p3) &~ s

-- $ReportTaskProgress
-- Updates the AWS Data Pipeline service on the progress of the calling task
-- runner. When the task runner is assigned a task, it should call
-- ReportTaskProgress to acknowledge that it has the task within 2 minutes. If
-- the web service does not recieve this acknowledgement within the 2 minute
-- window, it will assign the task in a subsequent PollForTask call. After
-- this initial acknowledgement, the task runner only needs to report progress
-- every 15 minutes to maintain its ownership of the task. You can change this
-- reporting time from 15 minutes by specifying a reportProgressTimeout field
-- in your pipeline. If a task runner does not report its status after 5
-- minutes, AWS Data Pipeline will assume that the task runner is unable to
-- process the task and will reassign the task in a subsequent response to
-- PollForTask. task runners should call ReportTaskProgress every 60 seconds.
-- POST / HTTP/1.1 Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- DataPipeline.ReportTaskProgress Content-Length: 832 Host:
-- datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012 17:49:52
-- GMT Authorization: AuthParams {"taskId":
-- "aaGgHT4LuH0T0Y0oLrJRjas5qH0d8cDPADxqq3tn+zCWGELkCdV2JprLreXm1oxeP5EFZHFLJ69kjSsLYE0iYHYBYVGBrB+E/pYq7ANEEeGJFnSBMRiXZVA+8UJ3OzcInvXeinqBmBaKwii7hnnKb/AXjXiNTXyxgydX1KAyg1AxkwBYG4cfPYMZbuEbQJFJvv5C/2+GVXz1w94nKYTeUeepwUOFOuRLS6JVtZoYwpF56E+Yfk1IcGpFOvCZ01B4Bkuu7x3J+MD/j6kJgZLAgbCJQtI3eiW3kdGmX0p0I2BdY1ZsX6b4UiSvM3OMj6NEHJCJL4E0ZfitnhCoe24Kvjo6C2hFbZq+ei/HPgSXBQMSagkr4vS9c0ChzxH2+LNYvec6bY4kymkaZI1dvOzmpa0FcnGf5AjSK4GpsViZ/ujz6zxFv81qBXzjF0/4M1775rjV1VUdyKaixiA/sJiACNezqZqETidp8d24BDPRhGsj6pBCrnelqGFrk/gXEXUsJ+xwMifRC8UVwiKekpAvHUywVk7Ku4jH/n3i2VoLRP6FXwpUbelu34iiZ9czpXyLtyPKwxa87dlrnRVURwkcVjOt2Mcrcaqe+cbWHvNRhyrPkkdfSF3ac8/wfgVbXvLEB2k9mKc67aD9rvdc1PKX09Tk8BKklsMTpZ3TRCd4NzQlJKigMe8Jat9+1tKj4Ole5ZzW6uyTu2s2iFjEV8KXu4MaiRJyNKCdKeGhhZWY37Qk4NBK4Ppgu+C6Y41dpfOh288SLDEVx0/UySlqOEdhba7c6BiPp5r3hKj3mk9lFy5OYp1aoGLeeFmjXveTnPdf2gkWqXXg7AUbJ7jEs1F0lKZQg4szep2gcKyAJXgvXLfJJHcha8Lfb/Ee7wYmyOcAaRpDBoFNSbtoVXar46teIrpho+ZDvynUXvU0grHWGOk=:wn3SgymHZM99bEXAMPLE",
-- "fields": [ {"key": "percentComplete", "stringValue": "50"} ] }
-- x-amzn-RequestId: 640bd023-0775-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 18 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"canceled": false}.
--
-- See: 'Network.AWS.DataPipeline.ReportTaskProgress'

reportTaskProgress :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => Text -- ^ 'rtpTaskId'
    -> m ReportTaskProgressResponse
reportTaskProgress p1 =
    send (mkReportTaskProgress p1)

reportTaskProgressCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => Text -- ^ 'rtpTaskId'
    -> m (Either ServiceEr ReportTaskProgressResponse)
reportTaskProgressCatch p1 =
    sendCatch (mkReportTaskProgress p1)

-- $ReportTaskRunnerHeartbeat
-- Task runners call ReportTaskRunnerHeartbeat every 15 minutes to indicate
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
--
-- See: 'Network.AWS.DataPipeline.ReportTaskRunnerHeartbeat'

reportTaskRunnerHeartbeat :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             )
    => Text -- ^ 'rtrhTaskrunnerId'
    -> State ReportTaskRunnerHeartbeat a
    -> m ReportTaskRunnerHeartbeatResponse
reportTaskRunnerHeartbeat p1 s =
    send $ (mkReportTaskRunnerHeartbeat p1) &~ s

reportTaskRunnerHeartbeatCatch :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadReader Env m
                                  )
    => Text -- ^ 'rtrhTaskrunnerId'
    -> State ReportTaskRunnerHeartbeat a
    -> m (Either ServiceEr ReportTaskRunnerHeartbeatResponse)
reportTaskRunnerHeartbeatCatch p1 s =
    sendCatch $ (mkReportTaskRunnerHeartbeat p1) &~ s

-- $SetStatus
-- Requests that the status of an array of physical or logical pipeline
-- objects be updated in the pipeline. This update may not occur immediately,
-- but is eventually consistent. The status that can be set depends on the
-- type of object. POST / HTTP/1.1 Content-Type: application/x-amz-json-1.1
-- X-Amz-Target: DataPipeline.SetStatus Content-Length: 100 Host:
-- datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012 17:49:52
-- GMT Authorization: AuthParams {"pipelineId": "df-0634701J7KEXAMPLE",
-- "objectIds": ["o-08600941GHJWMBR9E2"], "status": "pause"} x-amzn-RequestId:
-- e83b8ab7-076a-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 0 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT Unexpected response: 200, OK, undefined.
--
-- See: 'Network.AWS.DataPipeline.SetStatus'

setStatus :: ( MonadCatch m
             , MonadResource m
             , MonadError AWS.Error m
             , MonadReader Env m
             )
    => Text -- ^ 'ssPipelineId'
    -> [Text] -- ^ 'ssObjectIds'
    -> Text -- ^ 'ssStatus'
    -> m SetStatusResponse
setStatus p1 p2 p3 =
    send (mkSetStatus p1 p2 p3)

setStatusCatch :: ( MonadCatch m
                  , MonadResource m
                  , MonadReader Env m
                  )
    => Text -- ^ 'ssPipelineId'
    -> [Text] -- ^ 'ssObjectIds'
    -> Text -- ^ 'ssStatus'
    -> m (Either ServiceEr SetStatusResponse)
setStatusCatch p1 p2 p3 =
    sendCatch (mkSetStatus p1 p2 p3)

-- $SetTaskStatus
-- Notifies AWS Data Pipeline that a task is completed and provides
-- information about the final status. The task runner calls this action
-- regardless of whether the task was sucessful. The task runner does not need
-- to call SetTaskStatus for tasks that are canceled by the web service during
-- a call to ReportTaskProgress. POST / HTTP/1.1 Content-Type:
-- application/x-amz-json-1.1 X-Amz-Target: DataPipeline.SetTaskStatus
-- Content-Length: 847 Host: datapipeline.us-east-1.amazonaws.com X-Amz-Date:
-- Mon, 12 Nov 2012 17:49:52 GMT Authorization: AuthParams {"taskId":
-- "aaGgHT4LuH0T0Y0oLrJRjas5qH0d8cDPADxqq3tn+zCWGELkCdV2JprLreXm1oxeP5EFZHFLJ69kjSsLYE0iYHYBYVGBrB+E/pYq7ANEEeGJFnSBMRiXZVA+8UJ3OzcInvXeinqBmBaKwii7hnnKb/AXjXiNTXyxgydX1KAyg1AxkwBYG4cfPYMZbuEbQJFJvv5C/2+GVXz1w94nKYTeUeepwUOFOuRLS6JVtZoYwpF56E+Yfk1IcGpFOvCZ01B4Bkuu7x3J+MD/j6kJgZLAgbCJQtI3eiW3kdGmX0p0I2BdY1ZsX6b4UiSvM3OMj6NEHJCJL4E0ZfitnhCoe24Kvjo6C2hFbZq+ei/HPgSXBQMSagkr4vS9c0ChzxH2+LNYvec6bY4kymkaZI1dvOzmpa0FcnGf5AjSK4GpsViZ/ujz6zxFv81qBXzjF0/4M1775rjV1VUdyKaixiA/sJiACNezqZqETidp8d24BDPRhGsj6pBCrnelqGFrk/gXEXUsJ+xwMifRC8UVwiKekpAvHUywVk7Ku4jH/n3i2VoLRP6FXwpUbelu34iiZ9czpXyLtyPKwxa87dlrnRVURwkcVjOt2Mcrcaqe+cbWHvNRhyrPkkdfSF3ac8/wfgVbXvLEB2k9mKc67aD9rvdc1PKX09Tk8BKklsMTpZ3TRCd4NzQlJKigMe8Jat9+1tKj4Ole5ZzW6uyTu2s2iFjEV8KXu4MaiRJyNKCdKeGhhZWY37Qk4NBK4Ppgu+C6Y41dpfOh288SLDEVx0/UySlqOEdhba7c6BiPp5r3hKj3mk9lFy5OYp1aoGLeeFmjXveTnPdf2gkWqXXg7AUbJ7jEs1F0lKZQg4szep2gcKyAJXgvXLfJJHcha8Lfb/Ee7wYmyOcAaRpDBoFNSbtoVXar46teIrpho+ZDvynUXvU0grHWGOk=:wn3SgymHZM99bEXAMPLE",
-- "taskStatus": "FINISHED"} x-amzn-RequestId:
-- 8c8deb53-0788-11e2-af9c-6bc7a6be6qr8 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 0 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {}.
--
-- See: 'Network.AWS.DataPipeline.SetTaskStatus'

setTaskStatus :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 )
    => Text -- ^ 'stsTaskId'
    -> TaskStatus -- ^ 'stsTaskStatus'
    -> State SetTaskStatus a
    -> m SetTaskStatusResponse
setTaskStatus p1 p2 s =
    send $ (mkSetTaskStatus p1 p2) &~ s

setTaskStatusCatch :: ( MonadCatch m
                      , MonadResource m
                      , MonadReader Env m
                      )
    => Text -- ^ 'stsTaskId'
    -> TaskStatus -- ^ 'stsTaskStatus'
    -> State SetTaskStatus a
    -> m (Either ServiceEr SetTaskStatusResponse)
setTaskStatusCatch p1 p2 s =
    sendCatch $ (mkSetTaskStatus p1 p2) &~ s

-- $ValidatePipelineDefinition
-- Tests the pipeline definition with a set of validation checks to ensure
-- that it is well formed and can run without error. Example 1 This example
-- sets an valid pipeline configuration and returns success. POST / HTTP/1.1
-- Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- DataPipeline.ValidatePipelineDefinition Content-Length: 936 Host:
-- datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012 17:49:52
-- GMT Authorization: AuthParams {"pipelineId": "df-06372391ZG65EXAMPLE",
-- "pipelineObjects": [ {"id": "Default", "name": "Default", "fields": [
-- {"key": "workerGroup", "stringValue": "MyworkerGroup"} ] }, {"id":
-- "Schedule", "name": "Schedule", "fields": [ {"key": "startDateTime",
-- "stringValue": "2012-09-25T17:00:00"}, {"key": "type", "stringValue":
-- "Schedule"}, {"key": "period", "stringValue": "1 hour"}, {"key":
-- "endDateTime", "stringValue": "2012-09-25T18:00:00"} ] }, {"id":
-- "SayHello", "name": "SayHello", "fields": [ {"key": "type", "stringValue":
-- "ShellCommandActivity"}, {"key": "command", "stringValue": "echo hello"},
-- {"key": "parent", "refValue": "Default"}, {"key": "schedule", "refValue":
-- "Schedule"} ] } ] } x-amzn-RequestId: 92c9f347-0776-11e2-8a14-21bb8a1f50ef
-- Content-Type: application/x-amz-json-1.1 Content-Length: 18 Date: Mon, 12
-- Nov 2012 17:50:53 GMT {"errored": false} Example 2 This example sets an
-- invalid pipeline configuration and returns the associated set of validation
-- errors. POST / HTTP/1.1 Content-Type: application/x-amz-json-1.1
-- X-Amz-Target: DataPipeline.ValidatePipelineDefinition Content-Length: 903
-- Host: datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012
-- 17:49:52 GMT Authorization: AuthParams {"pipelineId":
-- "df-06372391ZG65EXAMPLE", "pipelineObjects": [ {"id": "Default", "name":
-- "Default", "fields": [ {"key": "workerGroup", "stringValue":
-- "MyworkerGroup"} ] }, {"id": "Schedule", "name": "Schedule", "fields": [
-- {"key": "startDateTime", "stringValue": "bad-time"}, {"key": "type",
-- "stringValue": "Schedule"}, {"key": "period", "stringValue": "1 hour"},
-- {"key": "endDateTime", "stringValue": "2012-09-25T18:00:00"} ] }, {"id":
-- "SayHello", "name": "SayHello", "fields": [ {"key": "type", "stringValue":
-- "ShellCommandActivity"}, {"key": "command", "stringValue": "echo hello"},
-- {"key": "parent", "refValue": "Default"}, {"key": "schedule", "refValue":
-- "Schedule"} ] } ] } x-amzn-RequestId: 496a1f5a-0e6a-11e2-a61c-bd6312c92ddd
-- Content-Type: application/x-amz-json-1.1 Content-Length: 278 Date: Mon, 12
-- Nov 2012 17:50:53 GMT {"errored": true, "validationErrors": [ {"errors":
-- ["INVALID_FIELD_VALUE: 'startDateTime' value must be a literal datetime
-- value."], "id": "Schedule"} ] }.
--
-- See: 'Network.AWS.DataPipeline.ValidatePipelineDefinition'

validatePipelineDefinition :: ( MonadCatch m
                              , MonadResource m
                              , MonadError AWS.Error m
                              , MonadReader Env m
                              )
    => Text -- ^ 'vpdPipelineId'
    -> [PipelineObject] -- ^ 'vpdPipelineObjects'
    -> m ValidatePipelineDefinitionResponse
validatePipelineDefinition p1 p2 =
    send (mkValidatePipelineDefinition p1 p2)

validatePipelineDefinitionCatch :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadReader Env m
                                   )
    => Text -- ^ 'vpdPipelineId'
    -> [PipelineObject] -- ^ 'vpdPipelineObjects'
    -> m (Either ServiceEr ValidatePipelineDefinitionResponse)
validatePipelineDefinitionCatch p1 p2 =
    sendCatch (mkValidatePipelineDefinition p1 p2)
