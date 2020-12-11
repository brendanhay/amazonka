{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Batch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--             mkCreateComputeEnvironment
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestRegisterJobDefinition $
--             mkRegisterJobDefinition
--
--         , requestSubmitJob $
--             mkSubmitJob
--
--         , requestListJobs $
--             mkListJobs
--
--         , requestTerminateJob $
--             mkTerminateJob
--
--         , requestDescribeJobs $
--             mkDescribeJobs
--
--         , requestDeleteComputeEnvironment $
--             mkDeleteComputeEnvironment
--
--         , requestUpdateComputeEnvironment $
--             mkUpdateComputeEnvironment
--
--         , requestDescribeJobDefinitions $
--             mkDescribeJobDefinitions
--
--         , requestUpdateJobQueue $
--             mkUpdateJobQueue
--
--         , requestDeleteJobQueue $
--             mkDeleteJobQueue
--
--         , requestCreateJobQueue $
--             mkCreateJobQueue
--
--         , requestDeregisterJobDefinition $
--             mkDeregisterJobDefinition
--
--         , requestDescribeJobQueues $
--             mkDescribeJobQueues
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestDescribeComputeEnvironments $
--             mkDescribeComputeEnvironments
--
--         , requestCancelJob $
--             mkCancelJob
--
--           ]

--     , testGroup "response"
--         [ responseCreateComputeEnvironment $
--             mkCreateComputeEnvironmentResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseRegisterJobDefinition $
--             mkRegisterJobDefinitionResponse
--
--         , responseSubmitJob $
--             mkSubmitJobResponse
--
--         , responseListJobs $
--             mkListJobsResponse
--
--         , responseTerminateJob $
--             mkTerminateJobResponse
--
--         , responseDescribeJobs $
--             mkDescribeJobsResponse
--
--         , responseDeleteComputeEnvironment $
--             mkDeleteComputeEnvironmentResponse
--
--         , responseUpdateComputeEnvironment $
--             mkUpdateComputeEnvironmentResponse
--
--         , responseDescribeJobDefinitions $
--             mkDescribeJobDefinitionsResponse
--
--         , responseUpdateJobQueue $
--             mkUpdateJobQueueResponse
--
--         , responseDeleteJobQueue $
--             mkDeleteJobQueueResponse
--
--         , responseCreateJobQueue $
--             mkCreateJobQueueResponse
--
--         , responseDeregisterJobDefinition $
--             mkDeregisterJobDefinitionResponse
--
--         , responseDescribeJobQueues $
--             mkDescribeJobQueuesResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseDescribeComputeEnvironments $
--             mkDescribeComputeEnvironmentsResponse
--
--         , responseCancelJob $
--             mkCancelJobResponse
--
--           ]
--     ]

-- Requests

requestCreateComputeEnvironment :: CreateComputeEnvironment -> TestTree
requestCreateComputeEnvironment =
  req
    "CreateComputeEnvironment"
    "fixture/CreateComputeEnvironment.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRegisterJobDefinition :: RegisterJobDefinition -> TestTree
requestRegisterJobDefinition =
  req
    "RegisterJobDefinition"
    "fixture/RegisterJobDefinition.yaml"

requestSubmitJob :: SubmitJob -> TestTree
requestSubmitJob =
  req
    "SubmitJob"
    "fixture/SubmitJob.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestTerminateJob :: TerminateJob -> TestTree
requestTerminateJob =
  req
    "TerminateJob"
    "fixture/TerminateJob.yaml"

requestDescribeJobs :: DescribeJobs -> TestTree
requestDescribeJobs =
  req
    "DescribeJobs"
    "fixture/DescribeJobs.yaml"

requestDeleteComputeEnvironment :: DeleteComputeEnvironment -> TestTree
requestDeleteComputeEnvironment =
  req
    "DeleteComputeEnvironment"
    "fixture/DeleteComputeEnvironment.yaml"

requestUpdateComputeEnvironment :: UpdateComputeEnvironment -> TestTree
requestUpdateComputeEnvironment =
  req
    "UpdateComputeEnvironment"
    "fixture/UpdateComputeEnvironment.yaml"

requestDescribeJobDefinitions :: DescribeJobDefinitions -> TestTree
requestDescribeJobDefinitions =
  req
    "DescribeJobDefinitions"
    "fixture/DescribeJobDefinitions.yaml"

requestUpdateJobQueue :: UpdateJobQueue -> TestTree
requestUpdateJobQueue =
  req
    "UpdateJobQueue"
    "fixture/UpdateJobQueue.yaml"

requestDeleteJobQueue :: DeleteJobQueue -> TestTree
requestDeleteJobQueue =
  req
    "DeleteJobQueue"
    "fixture/DeleteJobQueue.yaml"

requestCreateJobQueue :: CreateJobQueue -> TestTree
requestCreateJobQueue =
  req
    "CreateJobQueue"
    "fixture/CreateJobQueue.yaml"

requestDeregisterJobDefinition :: DeregisterJobDefinition -> TestTree
requestDeregisterJobDefinition =
  req
    "DeregisterJobDefinition"
    "fixture/DeregisterJobDefinition.yaml"

requestDescribeJobQueues :: DescribeJobQueues -> TestTree
requestDescribeJobQueues =
  req
    "DescribeJobQueues"
    "fixture/DescribeJobQueues.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDescribeComputeEnvironments :: DescribeComputeEnvironments -> TestTree
requestDescribeComputeEnvironments =
  req
    "DescribeComputeEnvironments"
    "fixture/DescribeComputeEnvironments.yaml"

requestCancelJob :: CancelJob -> TestTree
requestCancelJob =
  req
    "CancelJob"
    "fixture/CancelJob.yaml"

-- Responses

responseCreateComputeEnvironment :: CreateComputeEnvironmentResponse -> TestTree
responseCreateComputeEnvironment =
  res
    "CreateComputeEnvironmentResponse"
    "fixture/CreateComputeEnvironmentResponse.proto"
    batchService
    (Proxy :: Proxy CreateComputeEnvironment)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    batchService
    (Proxy :: Proxy ListTagsForResource)

responseRegisterJobDefinition :: RegisterJobDefinitionResponse -> TestTree
responseRegisterJobDefinition =
  res
    "RegisterJobDefinitionResponse"
    "fixture/RegisterJobDefinitionResponse.proto"
    batchService
    (Proxy :: Proxy RegisterJobDefinition)

responseSubmitJob :: SubmitJobResponse -> TestTree
responseSubmitJob =
  res
    "SubmitJobResponse"
    "fixture/SubmitJobResponse.proto"
    batchService
    (Proxy :: Proxy SubmitJob)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    batchService
    (Proxy :: Proxy ListJobs)

responseTerminateJob :: TerminateJobResponse -> TestTree
responseTerminateJob =
  res
    "TerminateJobResponse"
    "fixture/TerminateJobResponse.proto"
    batchService
    (Proxy :: Proxy TerminateJob)

responseDescribeJobs :: DescribeJobsResponse -> TestTree
responseDescribeJobs =
  res
    "DescribeJobsResponse"
    "fixture/DescribeJobsResponse.proto"
    batchService
    (Proxy :: Proxy DescribeJobs)

responseDeleteComputeEnvironment :: DeleteComputeEnvironmentResponse -> TestTree
responseDeleteComputeEnvironment =
  res
    "DeleteComputeEnvironmentResponse"
    "fixture/DeleteComputeEnvironmentResponse.proto"
    batchService
    (Proxy :: Proxy DeleteComputeEnvironment)

responseUpdateComputeEnvironment :: UpdateComputeEnvironmentResponse -> TestTree
responseUpdateComputeEnvironment =
  res
    "UpdateComputeEnvironmentResponse"
    "fixture/UpdateComputeEnvironmentResponse.proto"
    batchService
    (Proxy :: Proxy UpdateComputeEnvironment)

responseDescribeJobDefinitions :: DescribeJobDefinitionsResponse -> TestTree
responseDescribeJobDefinitions =
  res
    "DescribeJobDefinitionsResponse"
    "fixture/DescribeJobDefinitionsResponse.proto"
    batchService
    (Proxy :: Proxy DescribeJobDefinitions)

responseUpdateJobQueue :: UpdateJobQueueResponse -> TestTree
responseUpdateJobQueue =
  res
    "UpdateJobQueueResponse"
    "fixture/UpdateJobQueueResponse.proto"
    batchService
    (Proxy :: Proxy UpdateJobQueue)

responseDeleteJobQueue :: DeleteJobQueueResponse -> TestTree
responseDeleteJobQueue =
  res
    "DeleteJobQueueResponse"
    "fixture/DeleteJobQueueResponse.proto"
    batchService
    (Proxy :: Proxy DeleteJobQueue)

responseCreateJobQueue :: CreateJobQueueResponse -> TestTree
responseCreateJobQueue =
  res
    "CreateJobQueueResponse"
    "fixture/CreateJobQueueResponse.proto"
    batchService
    (Proxy :: Proxy CreateJobQueue)

responseDeregisterJobDefinition :: DeregisterJobDefinitionResponse -> TestTree
responseDeregisterJobDefinition =
  res
    "DeregisterJobDefinitionResponse"
    "fixture/DeregisterJobDefinitionResponse.proto"
    batchService
    (Proxy :: Proxy DeregisterJobDefinition)

responseDescribeJobQueues :: DescribeJobQueuesResponse -> TestTree
responseDescribeJobQueues =
  res
    "DescribeJobQueuesResponse"
    "fixture/DescribeJobQueuesResponse.proto"
    batchService
    (Proxy :: Proxy DescribeJobQueues)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    batchService
    (Proxy :: Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    batchService
    (Proxy :: Proxy UntagResource)

responseDescribeComputeEnvironments :: DescribeComputeEnvironmentsResponse -> TestTree
responseDescribeComputeEnvironments =
  res
    "DescribeComputeEnvironmentsResponse"
    "fixture/DescribeComputeEnvironmentsResponse.proto"
    batchService
    (Proxy :: Proxy DescribeComputeEnvironments)

responseCancelJob :: CancelJobResponse -> TestTree
responseCancelJob =
  res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    batchService
    (Proxy :: Proxy CancelJob)
