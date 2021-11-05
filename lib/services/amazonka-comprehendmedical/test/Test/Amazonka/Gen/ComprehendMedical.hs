{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ComprehendMedical
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ComprehendMedical where

import Amazonka.ComprehendMedical
import qualified Data.Proxy as Proxy
import Test.Amazonka.ComprehendMedical.Internal
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
--         [ requestDescribeEntitiesDetectionV2Job $
--             newDescribeEntitiesDetectionV2Job
--
--         , requestDescribePHIDetectionJob $
--             newDescribePHIDetectionJob
--
--         , requestListICD10CMInferenceJobs $
--             newListICD10CMInferenceJobs
--
--         , requestInferICD10CM $
--             newInferICD10CM
--
--         , requestInferRxNorm $
--             newInferRxNorm
--
--         , requestStartICD10CMInferenceJob $
--             newStartICD10CMInferenceJob
--
--         , requestStartRxNormInferenceJob $
--             newStartRxNormInferenceJob
--
--         , requestListPHIDetectionJobs $
--             newListPHIDetectionJobs
--
--         , requestDescribeICD10CMInferenceJob $
--             newDescribeICD10CMInferenceJob
--
--         , requestStartPHIDetectionJob $
--             newStartPHIDetectionJob
--
--         , requestStopEntitiesDetectionV2Job $
--             newStopEntitiesDetectionV2Job
--
--         , requestDescribeRxNormInferenceJob $
--             newDescribeRxNormInferenceJob
--
--         , requestStopICD10CMInferenceJob $
--             newStopICD10CMInferenceJob
--
--         , requestListEntitiesDetectionV2Jobs $
--             newListEntitiesDetectionV2Jobs
--
--         , requestStopRxNormInferenceJob $
--             newStopRxNormInferenceJob
--
--         , requestDetectPHI $
--             newDetectPHI
--
--         , requestDetectEntitiesV2 $
--             newDetectEntitiesV2
--
--         , requestStopPHIDetectionJob $
--             newStopPHIDetectionJob
--
--         , requestStartEntitiesDetectionV2Job $
--             newStartEntitiesDetectionV2Job
--
--         , requestListRxNormInferenceJobs $
--             newListRxNormInferenceJobs
--
--           ]

--     , testGroup "response"
--         [ responseDescribeEntitiesDetectionV2Job $
--             newDescribeEntitiesDetectionV2JobResponse
--
--         , responseDescribePHIDetectionJob $
--             newDescribePHIDetectionJobResponse
--
--         , responseListICD10CMInferenceJobs $
--             newListICD10CMInferenceJobsResponse
--
--         , responseInferICD10CM $
--             newInferICD10CMResponse
--
--         , responseInferRxNorm $
--             newInferRxNormResponse
--
--         , responseStartICD10CMInferenceJob $
--             newStartICD10CMInferenceJobResponse
--
--         , responseStartRxNormInferenceJob $
--             newStartRxNormInferenceJobResponse
--
--         , responseListPHIDetectionJobs $
--             newListPHIDetectionJobsResponse
--
--         , responseDescribeICD10CMInferenceJob $
--             newDescribeICD10CMInferenceJobResponse
--
--         , responseStartPHIDetectionJob $
--             newStartPHIDetectionJobResponse
--
--         , responseStopEntitiesDetectionV2Job $
--             newStopEntitiesDetectionV2JobResponse
--
--         , responseDescribeRxNormInferenceJob $
--             newDescribeRxNormInferenceJobResponse
--
--         , responseStopICD10CMInferenceJob $
--             newStopICD10CMInferenceJobResponse
--
--         , responseListEntitiesDetectionV2Jobs $
--             newListEntitiesDetectionV2JobsResponse
--
--         , responseStopRxNormInferenceJob $
--             newStopRxNormInferenceJobResponse
--
--         , responseDetectPHI $
--             newDetectPHIResponse
--
--         , responseDetectEntitiesV2 $
--             newDetectEntitiesV2Response
--
--         , responseStopPHIDetectionJob $
--             newStopPHIDetectionJobResponse
--
--         , responseStartEntitiesDetectionV2Job $
--             newStartEntitiesDetectionV2JobResponse
--
--         , responseListRxNormInferenceJobs $
--             newListRxNormInferenceJobsResponse
--
--           ]
--     ]

-- Requests

requestDescribeEntitiesDetectionV2Job :: DescribeEntitiesDetectionV2Job -> TestTree
requestDescribeEntitiesDetectionV2Job =
  req
    "DescribeEntitiesDetectionV2Job"
    "fixture/DescribeEntitiesDetectionV2Job.yaml"

requestDescribePHIDetectionJob :: DescribePHIDetectionJob -> TestTree
requestDescribePHIDetectionJob =
  req
    "DescribePHIDetectionJob"
    "fixture/DescribePHIDetectionJob.yaml"

requestListICD10CMInferenceJobs :: ListICD10CMInferenceJobs -> TestTree
requestListICD10CMInferenceJobs =
  req
    "ListICD10CMInferenceJobs"
    "fixture/ListICD10CMInferenceJobs.yaml"

requestInferICD10CM :: InferICD10CM -> TestTree
requestInferICD10CM =
  req
    "InferICD10CM"
    "fixture/InferICD10CM.yaml"

requestInferRxNorm :: InferRxNorm -> TestTree
requestInferRxNorm =
  req
    "InferRxNorm"
    "fixture/InferRxNorm.yaml"

requestStartICD10CMInferenceJob :: StartICD10CMInferenceJob -> TestTree
requestStartICD10CMInferenceJob =
  req
    "StartICD10CMInferenceJob"
    "fixture/StartICD10CMInferenceJob.yaml"

requestStartRxNormInferenceJob :: StartRxNormInferenceJob -> TestTree
requestStartRxNormInferenceJob =
  req
    "StartRxNormInferenceJob"
    "fixture/StartRxNormInferenceJob.yaml"

requestListPHIDetectionJobs :: ListPHIDetectionJobs -> TestTree
requestListPHIDetectionJobs =
  req
    "ListPHIDetectionJobs"
    "fixture/ListPHIDetectionJobs.yaml"

requestDescribeICD10CMInferenceJob :: DescribeICD10CMInferenceJob -> TestTree
requestDescribeICD10CMInferenceJob =
  req
    "DescribeICD10CMInferenceJob"
    "fixture/DescribeICD10CMInferenceJob.yaml"

requestStartPHIDetectionJob :: StartPHIDetectionJob -> TestTree
requestStartPHIDetectionJob =
  req
    "StartPHIDetectionJob"
    "fixture/StartPHIDetectionJob.yaml"

requestStopEntitiesDetectionV2Job :: StopEntitiesDetectionV2Job -> TestTree
requestStopEntitiesDetectionV2Job =
  req
    "StopEntitiesDetectionV2Job"
    "fixture/StopEntitiesDetectionV2Job.yaml"

requestDescribeRxNormInferenceJob :: DescribeRxNormInferenceJob -> TestTree
requestDescribeRxNormInferenceJob =
  req
    "DescribeRxNormInferenceJob"
    "fixture/DescribeRxNormInferenceJob.yaml"

requestStopICD10CMInferenceJob :: StopICD10CMInferenceJob -> TestTree
requestStopICD10CMInferenceJob =
  req
    "StopICD10CMInferenceJob"
    "fixture/StopICD10CMInferenceJob.yaml"

requestListEntitiesDetectionV2Jobs :: ListEntitiesDetectionV2Jobs -> TestTree
requestListEntitiesDetectionV2Jobs =
  req
    "ListEntitiesDetectionV2Jobs"
    "fixture/ListEntitiesDetectionV2Jobs.yaml"

requestStopRxNormInferenceJob :: StopRxNormInferenceJob -> TestTree
requestStopRxNormInferenceJob =
  req
    "StopRxNormInferenceJob"
    "fixture/StopRxNormInferenceJob.yaml"

requestDetectPHI :: DetectPHI -> TestTree
requestDetectPHI =
  req
    "DetectPHI"
    "fixture/DetectPHI.yaml"

requestDetectEntitiesV2 :: DetectEntitiesV2 -> TestTree
requestDetectEntitiesV2 =
  req
    "DetectEntitiesV2"
    "fixture/DetectEntitiesV2.yaml"

requestStopPHIDetectionJob :: StopPHIDetectionJob -> TestTree
requestStopPHIDetectionJob =
  req
    "StopPHIDetectionJob"
    "fixture/StopPHIDetectionJob.yaml"

requestStartEntitiesDetectionV2Job :: StartEntitiesDetectionV2Job -> TestTree
requestStartEntitiesDetectionV2Job =
  req
    "StartEntitiesDetectionV2Job"
    "fixture/StartEntitiesDetectionV2Job.yaml"

requestListRxNormInferenceJobs :: ListRxNormInferenceJobs -> TestTree
requestListRxNormInferenceJobs =
  req
    "ListRxNormInferenceJobs"
    "fixture/ListRxNormInferenceJobs.yaml"

-- Responses

responseDescribeEntitiesDetectionV2Job :: DescribeEntitiesDetectionV2JobResponse -> TestTree
responseDescribeEntitiesDetectionV2Job =
  res
    "DescribeEntitiesDetectionV2JobResponse"
    "fixture/DescribeEntitiesDetectionV2JobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEntitiesDetectionV2Job)

responseDescribePHIDetectionJob :: DescribePHIDetectionJobResponse -> TestTree
responseDescribePHIDetectionJob =
  res
    "DescribePHIDetectionJobResponse"
    "fixture/DescribePHIDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePHIDetectionJob)

responseListICD10CMInferenceJobs :: ListICD10CMInferenceJobsResponse -> TestTree
responseListICD10CMInferenceJobs =
  res
    "ListICD10CMInferenceJobsResponse"
    "fixture/ListICD10CMInferenceJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListICD10CMInferenceJobs)

responseInferICD10CM :: InferICD10CMResponse -> TestTree
responseInferICD10CM =
  res
    "InferICD10CMResponse"
    "fixture/InferICD10CMResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InferICD10CM)

responseInferRxNorm :: InferRxNormResponse -> TestTree
responseInferRxNorm =
  res
    "InferRxNormResponse"
    "fixture/InferRxNormResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InferRxNorm)

responseStartICD10CMInferenceJob :: StartICD10CMInferenceJobResponse -> TestTree
responseStartICD10CMInferenceJob =
  res
    "StartICD10CMInferenceJobResponse"
    "fixture/StartICD10CMInferenceJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartICD10CMInferenceJob)

responseStartRxNormInferenceJob :: StartRxNormInferenceJobResponse -> TestTree
responseStartRxNormInferenceJob =
  res
    "StartRxNormInferenceJobResponse"
    "fixture/StartRxNormInferenceJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartRxNormInferenceJob)

responseListPHIDetectionJobs :: ListPHIDetectionJobsResponse -> TestTree
responseListPHIDetectionJobs =
  res
    "ListPHIDetectionJobsResponse"
    "fixture/ListPHIDetectionJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPHIDetectionJobs)

responseDescribeICD10CMInferenceJob :: DescribeICD10CMInferenceJobResponse -> TestTree
responseDescribeICD10CMInferenceJob =
  res
    "DescribeICD10CMInferenceJobResponse"
    "fixture/DescribeICD10CMInferenceJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeICD10CMInferenceJob)

responseStartPHIDetectionJob :: StartPHIDetectionJobResponse -> TestTree
responseStartPHIDetectionJob =
  res
    "StartPHIDetectionJobResponse"
    "fixture/StartPHIDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartPHIDetectionJob)

responseStopEntitiesDetectionV2Job :: StopEntitiesDetectionV2JobResponse -> TestTree
responseStopEntitiesDetectionV2Job =
  res
    "StopEntitiesDetectionV2JobResponse"
    "fixture/StopEntitiesDetectionV2JobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopEntitiesDetectionV2Job)

responseDescribeRxNormInferenceJob :: DescribeRxNormInferenceJobResponse -> TestTree
responseDescribeRxNormInferenceJob =
  res
    "DescribeRxNormInferenceJobResponse"
    "fixture/DescribeRxNormInferenceJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRxNormInferenceJob)

responseStopICD10CMInferenceJob :: StopICD10CMInferenceJobResponse -> TestTree
responseStopICD10CMInferenceJob =
  res
    "StopICD10CMInferenceJobResponse"
    "fixture/StopICD10CMInferenceJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopICD10CMInferenceJob)

responseListEntitiesDetectionV2Jobs :: ListEntitiesDetectionV2JobsResponse -> TestTree
responseListEntitiesDetectionV2Jobs =
  res
    "ListEntitiesDetectionV2JobsResponse"
    "fixture/ListEntitiesDetectionV2JobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEntitiesDetectionV2Jobs)

responseStopRxNormInferenceJob :: StopRxNormInferenceJobResponse -> TestTree
responseStopRxNormInferenceJob =
  res
    "StopRxNormInferenceJobResponse"
    "fixture/StopRxNormInferenceJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopRxNormInferenceJob)

responseDetectPHI :: DetectPHIResponse -> TestTree
responseDetectPHI =
  res
    "DetectPHIResponse"
    "fixture/DetectPHIResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectPHI)

responseDetectEntitiesV2 :: DetectEntitiesV2Response -> TestTree
responseDetectEntitiesV2 =
  res
    "DetectEntitiesV2Response"
    "fixture/DetectEntitiesV2Response.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectEntitiesV2)

responseStopPHIDetectionJob :: StopPHIDetectionJobResponse -> TestTree
responseStopPHIDetectionJob =
  res
    "StopPHIDetectionJobResponse"
    "fixture/StopPHIDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopPHIDetectionJob)

responseStartEntitiesDetectionV2Job :: StartEntitiesDetectionV2JobResponse -> TestTree
responseStartEntitiesDetectionV2Job =
  res
    "StartEntitiesDetectionV2JobResponse"
    "fixture/StartEntitiesDetectionV2JobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartEntitiesDetectionV2Job)

responseListRxNormInferenceJobs :: ListRxNormInferenceJobsResponse -> TestTree
responseListRxNormInferenceJobs =
  res
    "ListRxNormInferenceJobsResponse"
    "fixture/ListRxNormInferenceJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRxNormInferenceJobs)
