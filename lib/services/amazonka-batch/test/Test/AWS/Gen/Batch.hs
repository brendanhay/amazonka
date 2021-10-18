{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Batch
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--             newCreateComputeEnvironment
--
--         , requestDescribeJobs $
--             newDescribeJobs
--
--         , requestUpdateComputeEnvironment $
--             newUpdateComputeEnvironment
--
--         , requestCancelJob $
--             newCancelJob
--
--         , requestDeleteComputeEnvironment $
--             newDeleteComputeEnvironment
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeJobQueues $
--             newDescribeJobQueues
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDeregisterJobDefinition $
--             newDeregisterJobDefinition
--
--         , requestUpdateJobQueue $
--             newUpdateJobQueue
--
--         , requestDeleteJobQueue $
--             newDeleteJobQueue
--
--         , requestDescribeJobDefinitions $
--             newDescribeJobDefinitions
--
--         , requestTerminateJob $
--             newTerminateJob
--
--         , requestDescribeComputeEnvironments $
--             newDescribeComputeEnvironments
--
--         , requestListJobs $
--             newListJobs
--
--         , requestSubmitJob $
--             newSubmitJob
--
--         , requestRegisterJobDefinition $
--             newRegisterJobDefinition
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreateJobQueue $
--             newCreateJobQueue
--
--           ]

--     , testGroup "response"
--         [ responseCreateComputeEnvironment $
--             newCreateComputeEnvironmentResponse
--
--         , responseDescribeJobs $
--             newDescribeJobsResponse
--
--         , responseUpdateComputeEnvironment $
--             newUpdateComputeEnvironmentResponse
--
--         , responseCancelJob $
--             newCancelJobResponse
--
--         , responseDeleteComputeEnvironment $
--             newDeleteComputeEnvironmentResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeJobQueues $
--             newDescribeJobQueuesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDeregisterJobDefinition $
--             newDeregisterJobDefinitionResponse
--
--         , responseUpdateJobQueue $
--             newUpdateJobQueueResponse
--
--         , responseDeleteJobQueue $
--             newDeleteJobQueueResponse
--
--         , responseDescribeJobDefinitions $
--             newDescribeJobDefinitionsResponse
--
--         , responseTerminateJob $
--             newTerminateJobResponse
--
--         , responseDescribeComputeEnvironments $
--             newDescribeComputeEnvironmentsResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseSubmitJob $
--             newSubmitJobResponse
--
--         , responseRegisterJobDefinition $
--             newRegisterJobDefinitionResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreateJobQueue $
--             newCreateJobQueueResponse
--
--           ]
--     ]

-- Requests

requestCreateComputeEnvironment :: CreateComputeEnvironment -> TestTree
requestCreateComputeEnvironment =
  req
    "CreateComputeEnvironment"
    "fixture/CreateComputeEnvironment.yaml"

requestDescribeJobs :: DescribeJobs -> TestTree
requestDescribeJobs =
  req
    "DescribeJobs"
    "fixture/DescribeJobs.yaml"

requestUpdateComputeEnvironment :: UpdateComputeEnvironment -> TestTree
requestUpdateComputeEnvironment =
  req
    "UpdateComputeEnvironment"
    "fixture/UpdateComputeEnvironment.yaml"

requestCancelJob :: CancelJob -> TestTree
requestCancelJob =
  req
    "CancelJob"
    "fixture/CancelJob.yaml"

requestDeleteComputeEnvironment :: DeleteComputeEnvironment -> TestTree
requestDeleteComputeEnvironment =
  req
    "DeleteComputeEnvironment"
    "fixture/DeleteComputeEnvironment.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

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

requestDeregisterJobDefinition :: DeregisterJobDefinition -> TestTree
requestDeregisterJobDefinition =
  req
    "DeregisterJobDefinition"
    "fixture/DeregisterJobDefinition.yaml"

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

requestDescribeJobDefinitions :: DescribeJobDefinitions -> TestTree
requestDescribeJobDefinitions =
  req
    "DescribeJobDefinitions"
    "fixture/DescribeJobDefinitions.yaml"

requestTerminateJob :: TerminateJob -> TestTree
requestTerminateJob =
  req
    "TerminateJob"
    "fixture/TerminateJob.yaml"

requestDescribeComputeEnvironments :: DescribeComputeEnvironments -> TestTree
requestDescribeComputeEnvironments =
  req
    "DescribeComputeEnvironments"
    "fixture/DescribeComputeEnvironments.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestSubmitJob :: SubmitJob -> TestTree
requestSubmitJob =
  req
    "SubmitJob"
    "fixture/SubmitJob.yaml"

requestRegisterJobDefinition :: RegisterJobDefinition -> TestTree
requestRegisterJobDefinition =
  req
    "RegisterJobDefinition"
    "fixture/RegisterJobDefinition.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreateJobQueue :: CreateJobQueue -> TestTree
requestCreateJobQueue =
  req
    "CreateJobQueue"
    "fixture/CreateJobQueue.yaml"

-- Responses

responseCreateComputeEnvironment :: CreateComputeEnvironmentResponse -> TestTree
responseCreateComputeEnvironment =
  res
    "CreateComputeEnvironmentResponse"
    "fixture/CreateComputeEnvironmentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateComputeEnvironment)

responseDescribeJobs :: DescribeJobsResponse -> TestTree
responseDescribeJobs =
  res
    "DescribeJobsResponse"
    "fixture/DescribeJobsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeJobs)

responseUpdateComputeEnvironment :: UpdateComputeEnvironmentResponse -> TestTree
responseUpdateComputeEnvironment =
  res
    "UpdateComputeEnvironmentResponse"
    "fixture/UpdateComputeEnvironmentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateComputeEnvironment)

responseCancelJob :: CancelJobResponse -> TestTree
responseCancelJob =
  res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    defaultService
    (Proxy :: Proxy CancelJob)

responseDeleteComputeEnvironment :: DeleteComputeEnvironmentResponse -> TestTree
responseDeleteComputeEnvironment =
  res
    "DeleteComputeEnvironmentResponse"
    "fixture/DeleteComputeEnvironmentResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteComputeEnvironment)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDescribeJobQueues :: DescribeJobQueuesResponse -> TestTree
responseDescribeJobQueues =
  res
    "DescribeJobQueuesResponse"
    "fixture/DescribeJobQueuesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeJobQueues)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseDeregisterJobDefinition :: DeregisterJobDefinitionResponse -> TestTree
responseDeregisterJobDefinition =
  res
    "DeregisterJobDefinitionResponse"
    "fixture/DeregisterJobDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterJobDefinition)

responseUpdateJobQueue :: UpdateJobQueueResponse -> TestTree
responseUpdateJobQueue =
  res
    "UpdateJobQueueResponse"
    "fixture/UpdateJobQueueResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateJobQueue)

responseDeleteJobQueue :: DeleteJobQueueResponse -> TestTree
responseDeleteJobQueue =
  res
    "DeleteJobQueueResponse"
    "fixture/DeleteJobQueueResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteJobQueue)

responseDescribeJobDefinitions :: DescribeJobDefinitionsResponse -> TestTree
responseDescribeJobDefinitions =
  res
    "DescribeJobDefinitionsResponse"
    "fixture/DescribeJobDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeJobDefinitions)

responseTerminateJob :: TerminateJobResponse -> TestTree
responseTerminateJob =
  res
    "TerminateJobResponse"
    "fixture/TerminateJobResponse.proto"
    defaultService
    (Proxy :: Proxy TerminateJob)

responseDescribeComputeEnvironments :: DescribeComputeEnvironmentsResponse -> TestTree
responseDescribeComputeEnvironments =
  res
    "DescribeComputeEnvironmentsResponse"
    "fixture/DescribeComputeEnvironmentsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeComputeEnvironments)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListJobs)

responseSubmitJob :: SubmitJobResponse -> TestTree
responseSubmitJob =
  res
    "SubmitJobResponse"
    "fixture/SubmitJobResponse.proto"
    defaultService
    (Proxy :: Proxy SubmitJob)

responseRegisterJobDefinition :: RegisterJobDefinitionResponse -> TestTree
responseRegisterJobDefinition =
  res
    "RegisterJobDefinitionResponse"
    "fixture/RegisterJobDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterJobDefinition)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseCreateJobQueue :: CreateJobQueueResponse -> TestTree
responseCreateJobQueue =
  res
    "CreateJobQueueResponse"
    "fixture/CreateJobQueueResponse.proto"
    defaultService
    (Proxy :: Proxy CreateJobQueue)
