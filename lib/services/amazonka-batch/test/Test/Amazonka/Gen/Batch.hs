{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Batch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Batch where

import Amazonka.Batch
import qualified Data.Proxy as Proxy
import Test.Amazonka.Batch.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCancelJob $
--             newCancelJob
--
--         , requestCreateComputeEnvironment $
--             newCreateComputeEnvironment
--
--         , requestCreateJobQueue $
--             newCreateJobQueue
--
--         , requestCreateSchedulingPolicy $
--             newCreateSchedulingPolicy
--
--         , requestDeleteComputeEnvironment $
--             newDeleteComputeEnvironment
--
--         , requestDeleteJobQueue $
--             newDeleteJobQueue
--
--         , requestDeleteSchedulingPolicy $
--             newDeleteSchedulingPolicy
--
--         , requestDeregisterJobDefinition $
--             newDeregisterJobDefinition
--
--         , requestDescribeComputeEnvironments $
--             newDescribeComputeEnvironments
--
--         , requestDescribeJobDefinitions $
--             newDescribeJobDefinitions
--
--         , requestDescribeJobQueues $
--             newDescribeJobQueues
--
--         , requestDescribeJobs $
--             newDescribeJobs
--
--         , requestDescribeSchedulingPolicies $
--             newDescribeSchedulingPolicies
--
--         , requestListJobs $
--             newListJobs
--
--         , requestListSchedulingPolicies $
--             newListSchedulingPolicies
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRegisterJobDefinition $
--             newRegisterJobDefinition
--
--         , requestSubmitJob $
--             newSubmitJob
--
--         , requestTagResource $
--             newTagResource
--
--         , requestTerminateJob $
--             newTerminateJob
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateComputeEnvironment $
--             newUpdateComputeEnvironment
--
--         , requestUpdateJobQueue $
--             newUpdateJobQueue
--
--         , requestUpdateSchedulingPolicy $
--             newUpdateSchedulingPolicy
--
--           ]

--     , testGroup "response"
--         [ responseCancelJob $
--             newCancelJobResponse
--
--         , responseCreateComputeEnvironment $
--             newCreateComputeEnvironmentResponse
--
--         , responseCreateJobQueue $
--             newCreateJobQueueResponse
--
--         , responseCreateSchedulingPolicy $
--             newCreateSchedulingPolicyResponse
--
--         , responseDeleteComputeEnvironment $
--             newDeleteComputeEnvironmentResponse
--
--         , responseDeleteJobQueue $
--             newDeleteJobQueueResponse
--
--         , responseDeleteSchedulingPolicy $
--             newDeleteSchedulingPolicyResponse
--
--         , responseDeregisterJobDefinition $
--             newDeregisterJobDefinitionResponse
--
--         , responseDescribeComputeEnvironments $
--             newDescribeComputeEnvironmentsResponse
--
--         , responseDescribeJobDefinitions $
--             newDescribeJobDefinitionsResponse
--
--         , responseDescribeJobQueues $
--             newDescribeJobQueuesResponse
--
--         , responseDescribeJobs $
--             newDescribeJobsResponse
--
--         , responseDescribeSchedulingPolicies $
--             newDescribeSchedulingPoliciesResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseListSchedulingPolicies $
--             newListSchedulingPoliciesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseRegisterJobDefinition $
--             newRegisterJobDefinitionResponse
--
--         , responseSubmitJob $
--             newSubmitJobResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseTerminateJob $
--             newTerminateJobResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateComputeEnvironment $
--             newUpdateComputeEnvironmentResponse
--
--         , responseUpdateJobQueue $
--             newUpdateJobQueueResponse
--
--         , responseUpdateSchedulingPolicy $
--             newUpdateSchedulingPolicyResponse
--
--           ]
--     ]

-- Requests

requestCancelJob :: CancelJob -> TestTree
requestCancelJob =
  req
    "CancelJob"
    "fixture/CancelJob.yaml"

requestCreateComputeEnvironment :: CreateComputeEnvironment -> TestTree
requestCreateComputeEnvironment =
  req
    "CreateComputeEnvironment"
    "fixture/CreateComputeEnvironment.yaml"

requestCreateJobQueue :: CreateJobQueue -> TestTree
requestCreateJobQueue =
  req
    "CreateJobQueue"
    "fixture/CreateJobQueue.yaml"

requestCreateSchedulingPolicy :: CreateSchedulingPolicy -> TestTree
requestCreateSchedulingPolicy =
  req
    "CreateSchedulingPolicy"
    "fixture/CreateSchedulingPolicy.yaml"

requestDeleteComputeEnvironment :: DeleteComputeEnvironment -> TestTree
requestDeleteComputeEnvironment =
  req
    "DeleteComputeEnvironment"
    "fixture/DeleteComputeEnvironment.yaml"

requestDeleteJobQueue :: DeleteJobQueue -> TestTree
requestDeleteJobQueue =
  req
    "DeleteJobQueue"
    "fixture/DeleteJobQueue.yaml"

requestDeleteSchedulingPolicy :: DeleteSchedulingPolicy -> TestTree
requestDeleteSchedulingPolicy =
  req
    "DeleteSchedulingPolicy"
    "fixture/DeleteSchedulingPolicy.yaml"

requestDeregisterJobDefinition :: DeregisterJobDefinition -> TestTree
requestDeregisterJobDefinition =
  req
    "DeregisterJobDefinition"
    "fixture/DeregisterJobDefinition.yaml"

requestDescribeComputeEnvironments :: DescribeComputeEnvironments -> TestTree
requestDescribeComputeEnvironments =
  req
    "DescribeComputeEnvironments"
    "fixture/DescribeComputeEnvironments.yaml"

requestDescribeJobDefinitions :: DescribeJobDefinitions -> TestTree
requestDescribeJobDefinitions =
  req
    "DescribeJobDefinitions"
    "fixture/DescribeJobDefinitions.yaml"

requestDescribeJobQueues :: DescribeJobQueues -> TestTree
requestDescribeJobQueues =
  req
    "DescribeJobQueues"
    "fixture/DescribeJobQueues.yaml"

requestDescribeJobs :: DescribeJobs -> TestTree
requestDescribeJobs =
  req
    "DescribeJobs"
    "fixture/DescribeJobs.yaml"

requestDescribeSchedulingPolicies :: DescribeSchedulingPolicies -> TestTree
requestDescribeSchedulingPolicies =
  req
    "DescribeSchedulingPolicies"
    "fixture/DescribeSchedulingPolicies.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestListSchedulingPolicies :: ListSchedulingPolicies -> TestTree
requestListSchedulingPolicies =
  req
    "ListSchedulingPolicies"
    "fixture/ListSchedulingPolicies.yaml"

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

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestTerminateJob :: TerminateJob -> TestTree
requestTerminateJob =
  req
    "TerminateJob"
    "fixture/TerminateJob.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateComputeEnvironment :: UpdateComputeEnvironment -> TestTree
requestUpdateComputeEnvironment =
  req
    "UpdateComputeEnvironment"
    "fixture/UpdateComputeEnvironment.yaml"

requestUpdateJobQueue :: UpdateJobQueue -> TestTree
requestUpdateJobQueue =
  req
    "UpdateJobQueue"
    "fixture/UpdateJobQueue.yaml"

requestUpdateSchedulingPolicy :: UpdateSchedulingPolicy -> TestTree
requestUpdateSchedulingPolicy =
  req
    "UpdateSchedulingPolicy"
    "fixture/UpdateSchedulingPolicy.yaml"

-- Responses

responseCancelJob :: CancelJobResponse -> TestTree
responseCancelJob =
  res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelJob)

responseCreateComputeEnvironment :: CreateComputeEnvironmentResponse -> TestTree
responseCreateComputeEnvironment =
  res
    "CreateComputeEnvironmentResponse"
    "fixture/CreateComputeEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateComputeEnvironment)

responseCreateJobQueue :: CreateJobQueueResponse -> TestTree
responseCreateJobQueue =
  res
    "CreateJobQueueResponse"
    "fixture/CreateJobQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateJobQueue)

responseCreateSchedulingPolicy :: CreateSchedulingPolicyResponse -> TestTree
responseCreateSchedulingPolicy =
  res
    "CreateSchedulingPolicyResponse"
    "fixture/CreateSchedulingPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSchedulingPolicy)

responseDeleteComputeEnvironment :: DeleteComputeEnvironmentResponse -> TestTree
responseDeleteComputeEnvironment =
  res
    "DeleteComputeEnvironmentResponse"
    "fixture/DeleteComputeEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteComputeEnvironment)

responseDeleteJobQueue :: DeleteJobQueueResponse -> TestTree
responseDeleteJobQueue =
  res
    "DeleteJobQueueResponse"
    "fixture/DeleteJobQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteJobQueue)

responseDeleteSchedulingPolicy :: DeleteSchedulingPolicyResponse -> TestTree
responseDeleteSchedulingPolicy =
  res
    "DeleteSchedulingPolicyResponse"
    "fixture/DeleteSchedulingPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSchedulingPolicy)

responseDeregisterJobDefinition :: DeregisterJobDefinitionResponse -> TestTree
responseDeregisterJobDefinition =
  res
    "DeregisterJobDefinitionResponse"
    "fixture/DeregisterJobDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterJobDefinition)

responseDescribeComputeEnvironments :: DescribeComputeEnvironmentsResponse -> TestTree
responseDescribeComputeEnvironments =
  res
    "DescribeComputeEnvironmentsResponse"
    "fixture/DescribeComputeEnvironmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeComputeEnvironments)

responseDescribeJobDefinitions :: DescribeJobDefinitionsResponse -> TestTree
responseDescribeJobDefinitions =
  res
    "DescribeJobDefinitionsResponse"
    "fixture/DescribeJobDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeJobDefinitions)

responseDescribeJobQueues :: DescribeJobQueuesResponse -> TestTree
responseDescribeJobQueues =
  res
    "DescribeJobQueuesResponse"
    "fixture/DescribeJobQueuesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeJobQueues)

responseDescribeJobs :: DescribeJobsResponse -> TestTree
responseDescribeJobs =
  res
    "DescribeJobsResponse"
    "fixture/DescribeJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeJobs)

responseDescribeSchedulingPolicies :: DescribeSchedulingPoliciesResponse -> TestTree
responseDescribeSchedulingPolicies =
  res
    "DescribeSchedulingPoliciesResponse"
    "fixture/DescribeSchedulingPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSchedulingPolicies)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobs)

responseListSchedulingPolicies :: ListSchedulingPoliciesResponse -> TestTree
responseListSchedulingPolicies =
  res
    "ListSchedulingPoliciesResponse"
    "fixture/ListSchedulingPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSchedulingPolicies)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseRegisterJobDefinition :: RegisterJobDefinitionResponse -> TestTree
responseRegisterJobDefinition =
  res
    "RegisterJobDefinitionResponse"
    "fixture/RegisterJobDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterJobDefinition)

responseSubmitJob :: SubmitJobResponse -> TestTree
responseSubmitJob =
  res
    "SubmitJobResponse"
    "fixture/SubmitJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SubmitJob)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseTerminateJob :: TerminateJobResponse -> TestTree
responseTerminateJob =
  res
    "TerminateJobResponse"
    "fixture/TerminateJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TerminateJob)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateComputeEnvironment :: UpdateComputeEnvironmentResponse -> TestTree
responseUpdateComputeEnvironment =
  res
    "UpdateComputeEnvironmentResponse"
    "fixture/UpdateComputeEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateComputeEnvironment)

responseUpdateJobQueue :: UpdateJobQueueResponse -> TestTree
responseUpdateJobQueue =
  res
    "UpdateJobQueueResponse"
    "fixture/UpdateJobQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateJobQueue)

responseUpdateSchedulingPolicy :: UpdateSchedulingPolicyResponse -> TestTree
responseUpdateSchedulingPolicy =
  res
    "UpdateSchedulingPolicyResponse"
    "fixture/UpdateSchedulingPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSchedulingPolicy)
