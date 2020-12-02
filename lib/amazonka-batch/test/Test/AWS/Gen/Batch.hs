{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Batch
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Batch where

import Data.Proxy
import Network.AWS.Batch
import Test.AWS.Batch.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateComputeEnvironment $
--             createComputeEnvironment
--
--         , requestRegisterJobDefinition $
--             registerJobDefinition
--
--         , requestSubmitJob $
--             submitJob
--
--         , requestListJobs $
--             listJobs
--
--         , requestTerminateJob $
--             terminateJob
--
--         , requestDescribeJobs $
--             describeJobs
--
--         , requestDeleteComputeEnvironment $
--             deleteComputeEnvironment
--
--         , requestUpdateComputeEnvironment $
--             updateComputeEnvironment
--
--         , requestDescribeJobDefinitions $
--             describeJobDefinitions
--
--         , requestUpdateJobQueue $
--             updateJobQueue
--
--         , requestDeleteJobQueue $
--             deleteJobQueue
--
--         , requestCreateJobQueue $
--             createJobQueue
--
--         , requestDeregisterJobDefinition $
--             deregisterJobDefinition
--
--         , requestDescribeJobQueues $
--             describeJobQueues
--
--         , requestDescribeComputeEnvironments $
--             describeComputeEnvironments
--
--         , requestCancelJob $
--             cancelJob
--
--           ]

--     , testGroup "response"
--         [ responseCreateComputeEnvironment $
--             createComputeEnvironmentResponse
--
--         , responseRegisterJobDefinition $
--             registerJobDefinitionResponse
--
--         , responseSubmitJob $
--             submitJobResponse
--
--         , responseListJobs $
--             listJobsResponse
--
--         , responseTerminateJob $
--             terminateJobResponse
--
--         , responseDescribeJobs $
--             describeJobsResponse
--
--         , responseDeleteComputeEnvironment $
--             deleteComputeEnvironmentResponse
--
--         , responseUpdateComputeEnvironment $
--             updateComputeEnvironmentResponse
--
--         , responseDescribeJobDefinitions $
--             describeJobDefinitionsResponse
--
--         , responseUpdateJobQueue $
--             updateJobQueueResponse
--
--         , responseDeleteJobQueue $
--             deleteJobQueueResponse
--
--         , responseCreateJobQueue $
--             createJobQueueResponse
--
--         , responseDeregisterJobDefinition $
--             deregisterJobDefinitionResponse
--
--         , responseDescribeJobQueues $
--             describeJobQueuesResponse
--
--         , responseDescribeComputeEnvironments $
--             describeComputeEnvironmentsResponse
--
--         , responseCancelJob $
--             cancelJobResponse
--
--           ]
--     ]

-- Requests

requestCreateComputeEnvironment :: CreateComputeEnvironment -> TestTree
requestCreateComputeEnvironment = req
    "CreateComputeEnvironment"
    "fixture/CreateComputeEnvironment.yaml"

requestRegisterJobDefinition :: RegisterJobDefinition -> TestTree
requestRegisterJobDefinition = req
    "RegisterJobDefinition"
    "fixture/RegisterJobDefinition.yaml"

requestSubmitJob :: SubmitJob -> TestTree
requestSubmitJob = req
    "SubmitJob"
    "fixture/SubmitJob.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs = req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestTerminateJob :: TerminateJob -> TestTree
requestTerminateJob = req
    "TerminateJob"
    "fixture/TerminateJob.yaml"

requestDescribeJobs :: DescribeJobs -> TestTree
requestDescribeJobs = req
    "DescribeJobs"
    "fixture/DescribeJobs.yaml"

requestDeleteComputeEnvironment :: DeleteComputeEnvironment -> TestTree
requestDeleteComputeEnvironment = req
    "DeleteComputeEnvironment"
    "fixture/DeleteComputeEnvironment.yaml"

requestUpdateComputeEnvironment :: UpdateComputeEnvironment -> TestTree
requestUpdateComputeEnvironment = req
    "UpdateComputeEnvironment"
    "fixture/UpdateComputeEnvironment.yaml"

requestDescribeJobDefinitions :: DescribeJobDefinitions -> TestTree
requestDescribeJobDefinitions = req
    "DescribeJobDefinitions"
    "fixture/DescribeJobDefinitions.yaml"

requestUpdateJobQueue :: UpdateJobQueue -> TestTree
requestUpdateJobQueue = req
    "UpdateJobQueue"
    "fixture/UpdateJobQueue.yaml"

requestDeleteJobQueue :: DeleteJobQueue -> TestTree
requestDeleteJobQueue = req
    "DeleteJobQueue"
    "fixture/DeleteJobQueue.yaml"

requestCreateJobQueue :: CreateJobQueue -> TestTree
requestCreateJobQueue = req
    "CreateJobQueue"
    "fixture/CreateJobQueue.yaml"

requestDeregisterJobDefinition :: DeregisterJobDefinition -> TestTree
requestDeregisterJobDefinition = req
    "DeregisterJobDefinition"
    "fixture/DeregisterJobDefinition.yaml"

requestDescribeJobQueues :: DescribeJobQueues -> TestTree
requestDescribeJobQueues = req
    "DescribeJobQueues"
    "fixture/DescribeJobQueues.yaml"

requestDescribeComputeEnvironments :: DescribeComputeEnvironments -> TestTree
requestDescribeComputeEnvironments = req
    "DescribeComputeEnvironments"
    "fixture/DescribeComputeEnvironments.yaml"

requestCancelJob :: CancelJob -> TestTree
requestCancelJob = req
    "CancelJob"
    "fixture/CancelJob.yaml"

-- Responses

responseCreateComputeEnvironment :: CreateComputeEnvironmentResponse -> TestTree
responseCreateComputeEnvironment = res
    "CreateComputeEnvironmentResponse"
    "fixture/CreateComputeEnvironmentResponse.proto"
    batch
    (Proxy :: Proxy CreateComputeEnvironment)

responseRegisterJobDefinition :: RegisterJobDefinitionResponse -> TestTree
responseRegisterJobDefinition = res
    "RegisterJobDefinitionResponse"
    "fixture/RegisterJobDefinitionResponse.proto"
    batch
    (Proxy :: Proxy RegisterJobDefinition)

responseSubmitJob :: SubmitJobResponse -> TestTree
responseSubmitJob = res
    "SubmitJobResponse"
    "fixture/SubmitJobResponse.proto"
    batch
    (Proxy :: Proxy SubmitJob)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs = res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    batch
    (Proxy :: Proxy ListJobs)

responseTerminateJob :: TerminateJobResponse -> TestTree
responseTerminateJob = res
    "TerminateJobResponse"
    "fixture/TerminateJobResponse.proto"
    batch
    (Proxy :: Proxy TerminateJob)

responseDescribeJobs :: DescribeJobsResponse -> TestTree
responseDescribeJobs = res
    "DescribeJobsResponse"
    "fixture/DescribeJobsResponse.proto"
    batch
    (Proxy :: Proxy DescribeJobs)

responseDeleteComputeEnvironment :: DeleteComputeEnvironmentResponse -> TestTree
responseDeleteComputeEnvironment = res
    "DeleteComputeEnvironmentResponse"
    "fixture/DeleteComputeEnvironmentResponse.proto"
    batch
    (Proxy :: Proxy DeleteComputeEnvironment)

responseUpdateComputeEnvironment :: UpdateComputeEnvironmentResponse -> TestTree
responseUpdateComputeEnvironment = res
    "UpdateComputeEnvironmentResponse"
    "fixture/UpdateComputeEnvironmentResponse.proto"
    batch
    (Proxy :: Proxy UpdateComputeEnvironment)

responseDescribeJobDefinitions :: DescribeJobDefinitionsResponse -> TestTree
responseDescribeJobDefinitions = res
    "DescribeJobDefinitionsResponse"
    "fixture/DescribeJobDefinitionsResponse.proto"
    batch
    (Proxy :: Proxy DescribeJobDefinitions)

responseUpdateJobQueue :: UpdateJobQueueResponse -> TestTree
responseUpdateJobQueue = res
    "UpdateJobQueueResponse"
    "fixture/UpdateJobQueueResponse.proto"
    batch
    (Proxy :: Proxy UpdateJobQueue)

responseDeleteJobQueue :: DeleteJobQueueResponse -> TestTree
responseDeleteJobQueue = res
    "DeleteJobQueueResponse"
    "fixture/DeleteJobQueueResponse.proto"
    batch
    (Proxy :: Proxy DeleteJobQueue)

responseCreateJobQueue :: CreateJobQueueResponse -> TestTree
responseCreateJobQueue = res
    "CreateJobQueueResponse"
    "fixture/CreateJobQueueResponse.proto"
    batch
    (Proxy :: Proxy CreateJobQueue)

responseDeregisterJobDefinition :: DeregisterJobDefinitionResponse -> TestTree
responseDeregisterJobDefinition = res
    "DeregisterJobDefinitionResponse"
    "fixture/DeregisterJobDefinitionResponse.proto"
    batch
    (Proxy :: Proxy DeregisterJobDefinition)

responseDescribeJobQueues :: DescribeJobQueuesResponse -> TestTree
responseDescribeJobQueues = res
    "DescribeJobQueuesResponse"
    "fixture/DescribeJobQueuesResponse.proto"
    batch
    (Proxy :: Proxy DescribeJobQueues)

responseDescribeComputeEnvironments :: DescribeComputeEnvironmentsResponse -> TestTree
responseDescribeComputeEnvironments = res
    "DescribeComputeEnvironmentsResponse"
    "fixture/DescribeComputeEnvironmentsResponse.proto"
    batch
    (Proxy :: Proxy DescribeComputeEnvironments)

responseCancelJob :: CancelJobResponse -> TestTree
responseCancelJob = res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    batch
    (Proxy :: Proxy CancelJob)
