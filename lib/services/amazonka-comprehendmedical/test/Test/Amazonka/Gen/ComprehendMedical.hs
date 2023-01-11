{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ComprehendMedical
-- Copyright   : (c) 2013-2023 Brendan Hay
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
--         , requestDescribeICD10CMInferenceJob $
--             newDescribeICD10CMInferenceJob
--
--         , requestDescribePHIDetectionJob $
--             newDescribePHIDetectionJob
--
--         , requestDescribeRxNormInferenceJob $
--             newDescribeRxNormInferenceJob
--
--         , requestDescribeSNOMEDCTInferenceJob $
--             newDescribeSNOMEDCTInferenceJob
--
--         , requestDetectEntitiesV2 $
--             newDetectEntitiesV2
--
--         , requestDetectPHI $
--             newDetectPHI
--
--         , requestInferICD10CM $
--             newInferICD10CM
--
--         , requestInferRxNorm $
--             newInferRxNorm
--
--         , requestInferSNOMEDCT $
--             newInferSNOMEDCT
--
--         , requestListEntitiesDetectionV2Jobs $
--             newListEntitiesDetectionV2Jobs
--
--         , requestListICD10CMInferenceJobs $
--             newListICD10CMInferenceJobs
--
--         , requestListPHIDetectionJobs $
--             newListPHIDetectionJobs
--
--         , requestListRxNormInferenceJobs $
--             newListRxNormInferenceJobs
--
--         , requestListSNOMEDCTInferenceJobs $
--             newListSNOMEDCTInferenceJobs
--
--         , requestStartEntitiesDetectionV2Job $
--             newStartEntitiesDetectionV2Job
--
--         , requestStartICD10CMInferenceJob $
--             newStartICD10CMInferenceJob
--
--         , requestStartPHIDetectionJob $
--             newStartPHIDetectionJob
--
--         , requestStartRxNormInferenceJob $
--             newStartRxNormInferenceJob
--
--         , requestStartSNOMEDCTInferenceJob $
--             newStartSNOMEDCTInferenceJob
--
--         , requestStopEntitiesDetectionV2Job $
--             newStopEntitiesDetectionV2Job
--
--         , requestStopICD10CMInferenceJob $
--             newStopICD10CMInferenceJob
--
--         , requestStopPHIDetectionJob $
--             newStopPHIDetectionJob
--
--         , requestStopRxNormInferenceJob $
--             newStopRxNormInferenceJob
--
--         , requestStopSNOMEDCTInferenceJob $
--             newStopSNOMEDCTInferenceJob
--
--           ]

--     , testGroup "response"
--         [ responseDescribeEntitiesDetectionV2Job $
--             newDescribeEntitiesDetectionV2JobResponse
--
--         , responseDescribeICD10CMInferenceJob $
--             newDescribeICD10CMInferenceJobResponse
--
--         , responseDescribePHIDetectionJob $
--             newDescribePHIDetectionJobResponse
--
--         , responseDescribeRxNormInferenceJob $
--             newDescribeRxNormInferenceJobResponse
--
--         , responseDescribeSNOMEDCTInferenceJob $
--             newDescribeSNOMEDCTInferenceJobResponse
--
--         , responseDetectEntitiesV2 $
--             newDetectEntitiesV2Response
--
--         , responseDetectPHI $
--             newDetectPHIResponse
--
--         , responseInferICD10CM $
--             newInferICD10CMResponse
--
--         , responseInferRxNorm $
--             newInferRxNormResponse
--
--         , responseInferSNOMEDCT $
--             newInferSNOMEDCTResponse
--
--         , responseListEntitiesDetectionV2Jobs $
--             newListEntitiesDetectionV2JobsResponse
--
--         , responseListICD10CMInferenceJobs $
--             newListICD10CMInferenceJobsResponse
--
--         , responseListPHIDetectionJobs $
--             newListPHIDetectionJobsResponse
--
--         , responseListRxNormInferenceJobs $
--             newListRxNormInferenceJobsResponse
--
--         , responseListSNOMEDCTInferenceJobs $
--             newListSNOMEDCTInferenceJobsResponse
--
--         , responseStartEntitiesDetectionV2Job $
--             newStartEntitiesDetectionV2JobResponse
--
--         , responseStartICD10CMInferenceJob $
--             newStartICD10CMInferenceJobResponse
--
--         , responseStartPHIDetectionJob $
--             newStartPHIDetectionJobResponse
--
--         , responseStartRxNormInferenceJob $
--             newStartRxNormInferenceJobResponse
--
--         , responseStartSNOMEDCTInferenceJob $
--             newStartSNOMEDCTInferenceJobResponse
--
--         , responseStopEntitiesDetectionV2Job $
--             newStopEntitiesDetectionV2JobResponse
--
--         , responseStopICD10CMInferenceJob $
--             newStopICD10CMInferenceJobResponse
--
--         , responseStopPHIDetectionJob $
--             newStopPHIDetectionJobResponse
--
--         , responseStopRxNormInferenceJob $
--             newStopRxNormInferenceJobResponse
--
--         , responseStopSNOMEDCTInferenceJob $
--             newStopSNOMEDCTInferenceJobResponse
--
--           ]
--     ]

-- Requests

requestDescribeEntitiesDetectionV2Job :: DescribeEntitiesDetectionV2Job -> TestTree
requestDescribeEntitiesDetectionV2Job =
  req
    "DescribeEntitiesDetectionV2Job"
    "fixture/DescribeEntitiesDetectionV2Job.yaml"

requestDescribeICD10CMInferenceJob :: DescribeICD10CMInferenceJob -> TestTree
requestDescribeICD10CMInferenceJob =
  req
    "DescribeICD10CMInferenceJob"
    "fixture/DescribeICD10CMInferenceJob.yaml"

requestDescribePHIDetectionJob :: DescribePHIDetectionJob -> TestTree
requestDescribePHIDetectionJob =
  req
    "DescribePHIDetectionJob"
    "fixture/DescribePHIDetectionJob.yaml"

requestDescribeRxNormInferenceJob :: DescribeRxNormInferenceJob -> TestTree
requestDescribeRxNormInferenceJob =
  req
    "DescribeRxNormInferenceJob"
    "fixture/DescribeRxNormInferenceJob.yaml"

requestDescribeSNOMEDCTInferenceJob :: DescribeSNOMEDCTInferenceJob -> TestTree
requestDescribeSNOMEDCTInferenceJob =
  req
    "DescribeSNOMEDCTInferenceJob"
    "fixture/DescribeSNOMEDCTInferenceJob.yaml"

requestDetectEntitiesV2 :: DetectEntitiesV2 -> TestTree
requestDetectEntitiesV2 =
  req
    "DetectEntitiesV2"
    "fixture/DetectEntitiesV2.yaml"

requestDetectPHI :: DetectPHI -> TestTree
requestDetectPHI =
  req
    "DetectPHI"
    "fixture/DetectPHI.yaml"

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

requestInferSNOMEDCT :: InferSNOMEDCT -> TestTree
requestInferSNOMEDCT =
  req
    "InferSNOMEDCT"
    "fixture/InferSNOMEDCT.yaml"

requestListEntitiesDetectionV2Jobs :: ListEntitiesDetectionV2Jobs -> TestTree
requestListEntitiesDetectionV2Jobs =
  req
    "ListEntitiesDetectionV2Jobs"
    "fixture/ListEntitiesDetectionV2Jobs.yaml"

requestListICD10CMInferenceJobs :: ListICD10CMInferenceJobs -> TestTree
requestListICD10CMInferenceJobs =
  req
    "ListICD10CMInferenceJobs"
    "fixture/ListICD10CMInferenceJobs.yaml"

requestListPHIDetectionJobs :: ListPHIDetectionJobs -> TestTree
requestListPHIDetectionJobs =
  req
    "ListPHIDetectionJobs"
    "fixture/ListPHIDetectionJobs.yaml"

requestListRxNormInferenceJobs :: ListRxNormInferenceJobs -> TestTree
requestListRxNormInferenceJobs =
  req
    "ListRxNormInferenceJobs"
    "fixture/ListRxNormInferenceJobs.yaml"

requestListSNOMEDCTInferenceJobs :: ListSNOMEDCTInferenceJobs -> TestTree
requestListSNOMEDCTInferenceJobs =
  req
    "ListSNOMEDCTInferenceJobs"
    "fixture/ListSNOMEDCTInferenceJobs.yaml"

requestStartEntitiesDetectionV2Job :: StartEntitiesDetectionV2Job -> TestTree
requestStartEntitiesDetectionV2Job =
  req
    "StartEntitiesDetectionV2Job"
    "fixture/StartEntitiesDetectionV2Job.yaml"

requestStartICD10CMInferenceJob :: StartICD10CMInferenceJob -> TestTree
requestStartICD10CMInferenceJob =
  req
    "StartICD10CMInferenceJob"
    "fixture/StartICD10CMInferenceJob.yaml"

requestStartPHIDetectionJob :: StartPHIDetectionJob -> TestTree
requestStartPHIDetectionJob =
  req
    "StartPHIDetectionJob"
    "fixture/StartPHIDetectionJob.yaml"

requestStartRxNormInferenceJob :: StartRxNormInferenceJob -> TestTree
requestStartRxNormInferenceJob =
  req
    "StartRxNormInferenceJob"
    "fixture/StartRxNormInferenceJob.yaml"

requestStartSNOMEDCTInferenceJob :: StartSNOMEDCTInferenceJob -> TestTree
requestStartSNOMEDCTInferenceJob =
  req
    "StartSNOMEDCTInferenceJob"
    "fixture/StartSNOMEDCTInferenceJob.yaml"

requestStopEntitiesDetectionV2Job :: StopEntitiesDetectionV2Job -> TestTree
requestStopEntitiesDetectionV2Job =
  req
    "StopEntitiesDetectionV2Job"
    "fixture/StopEntitiesDetectionV2Job.yaml"

requestStopICD10CMInferenceJob :: StopICD10CMInferenceJob -> TestTree
requestStopICD10CMInferenceJob =
  req
    "StopICD10CMInferenceJob"
    "fixture/StopICD10CMInferenceJob.yaml"

requestStopPHIDetectionJob :: StopPHIDetectionJob -> TestTree
requestStopPHIDetectionJob =
  req
    "StopPHIDetectionJob"
    "fixture/StopPHIDetectionJob.yaml"

requestStopRxNormInferenceJob :: StopRxNormInferenceJob -> TestTree
requestStopRxNormInferenceJob =
  req
    "StopRxNormInferenceJob"
    "fixture/StopRxNormInferenceJob.yaml"

requestStopSNOMEDCTInferenceJob :: StopSNOMEDCTInferenceJob -> TestTree
requestStopSNOMEDCTInferenceJob =
  req
    "StopSNOMEDCTInferenceJob"
    "fixture/StopSNOMEDCTInferenceJob.yaml"

-- Responses

responseDescribeEntitiesDetectionV2Job :: DescribeEntitiesDetectionV2JobResponse -> TestTree
responseDescribeEntitiesDetectionV2Job =
  res
    "DescribeEntitiesDetectionV2JobResponse"
    "fixture/DescribeEntitiesDetectionV2JobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEntitiesDetectionV2Job)

responseDescribeICD10CMInferenceJob :: DescribeICD10CMInferenceJobResponse -> TestTree
responseDescribeICD10CMInferenceJob =
  res
    "DescribeICD10CMInferenceJobResponse"
    "fixture/DescribeICD10CMInferenceJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeICD10CMInferenceJob)

responseDescribePHIDetectionJob :: DescribePHIDetectionJobResponse -> TestTree
responseDescribePHIDetectionJob =
  res
    "DescribePHIDetectionJobResponse"
    "fixture/DescribePHIDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePHIDetectionJob)

responseDescribeRxNormInferenceJob :: DescribeRxNormInferenceJobResponse -> TestTree
responseDescribeRxNormInferenceJob =
  res
    "DescribeRxNormInferenceJobResponse"
    "fixture/DescribeRxNormInferenceJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRxNormInferenceJob)

responseDescribeSNOMEDCTInferenceJob :: DescribeSNOMEDCTInferenceJobResponse -> TestTree
responseDescribeSNOMEDCTInferenceJob =
  res
    "DescribeSNOMEDCTInferenceJobResponse"
    "fixture/DescribeSNOMEDCTInferenceJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSNOMEDCTInferenceJob)

responseDetectEntitiesV2 :: DetectEntitiesV2Response -> TestTree
responseDetectEntitiesV2 =
  res
    "DetectEntitiesV2Response"
    "fixture/DetectEntitiesV2Response.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectEntitiesV2)

responseDetectPHI :: DetectPHIResponse -> TestTree
responseDetectPHI =
  res
    "DetectPHIResponse"
    "fixture/DetectPHIResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectPHI)

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

responseInferSNOMEDCT :: InferSNOMEDCTResponse -> TestTree
responseInferSNOMEDCT =
  res
    "InferSNOMEDCTResponse"
    "fixture/InferSNOMEDCTResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InferSNOMEDCT)

responseListEntitiesDetectionV2Jobs :: ListEntitiesDetectionV2JobsResponse -> TestTree
responseListEntitiesDetectionV2Jobs =
  res
    "ListEntitiesDetectionV2JobsResponse"
    "fixture/ListEntitiesDetectionV2JobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEntitiesDetectionV2Jobs)

responseListICD10CMInferenceJobs :: ListICD10CMInferenceJobsResponse -> TestTree
responseListICD10CMInferenceJobs =
  res
    "ListICD10CMInferenceJobsResponse"
    "fixture/ListICD10CMInferenceJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListICD10CMInferenceJobs)

responseListPHIDetectionJobs :: ListPHIDetectionJobsResponse -> TestTree
responseListPHIDetectionJobs =
  res
    "ListPHIDetectionJobsResponse"
    "fixture/ListPHIDetectionJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPHIDetectionJobs)

responseListRxNormInferenceJobs :: ListRxNormInferenceJobsResponse -> TestTree
responseListRxNormInferenceJobs =
  res
    "ListRxNormInferenceJobsResponse"
    "fixture/ListRxNormInferenceJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRxNormInferenceJobs)

responseListSNOMEDCTInferenceJobs :: ListSNOMEDCTInferenceJobsResponse -> TestTree
responseListSNOMEDCTInferenceJobs =
  res
    "ListSNOMEDCTInferenceJobsResponse"
    "fixture/ListSNOMEDCTInferenceJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSNOMEDCTInferenceJobs)

responseStartEntitiesDetectionV2Job :: StartEntitiesDetectionV2JobResponse -> TestTree
responseStartEntitiesDetectionV2Job =
  res
    "StartEntitiesDetectionV2JobResponse"
    "fixture/StartEntitiesDetectionV2JobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartEntitiesDetectionV2Job)

responseStartICD10CMInferenceJob :: StartICD10CMInferenceJobResponse -> TestTree
responseStartICD10CMInferenceJob =
  res
    "StartICD10CMInferenceJobResponse"
    "fixture/StartICD10CMInferenceJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartICD10CMInferenceJob)

responseStartPHIDetectionJob :: StartPHIDetectionJobResponse -> TestTree
responseStartPHIDetectionJob =
  res
    "StartPHIDetectionJobResponse"
    "fixture/StartPHIDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartPHIDetectionJob)

responseStartRxNormInferenceJob :: StartRxNormInferenceJobResponse -> TestTree
responseStartRxNormInferenceJob =
  res
    "StartRxNormInferenceJobResponse"
    "fixture/StartRxNormInferenceJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartRxNormInferenceJob)

responseStartSNOMEDCTInferenceJob :: StartSNOMEDCTInferenceJobResponse -> TestTree
responseStartSNOMEDCTInferenceJob =
  res
    "StartSNOMEDCTInferenceJobResponse"
    "fixture/StartSNOMEDCTInferenceJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartSNOMEDCTInferenceJob)

responseStopEntitiesDetectionV2Job :: StopEntitiesDetectionV2JobResponse -> TestTree
responseStopEntitiesDetectionV2Job =
  res
    "StopEntitiesDetectionV2JobResponse"
    "fixture/StopEntitiesDetectionV2JobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopEntitiesDetectionV2Job)

responseStopICD10CMInferenceJob :: StopICD10CMInferenceJobResponse -> TestTree
responseStopICD10CMInferenceJob =
  res
    "StopICD10CMInferenceJobResponse"
    "fixture/StopICD10CMInferenceJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopICD10CMInferenceJob)

responseStopPHIDetectionJob :: StopPHIDetectionJobResponse -> TestTree
responseStopPHIDetectionJob =
  res
    "StopPHIDetectionJobResponse"
    "fixture/StopPHIDetectionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopPHIDetectionJob)

responseStopRxNormInferenceJob :: StopRxNormInferenceJobResponse -> TestTree
responseStopRxNormInferenceJob =
  res
    "StopRxNormInferenceJobResponse"
    "fixture/StopRxNormInferenceJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopRxNormInferenceJob)

responseStopSNOMEDCTInferenceJob :: StopSNOMEDCTInferenceJobResponse -> TestTree
responseStopSNOMEDCTInferenceJob =
  res
    "StopSNOMEDCTInferenceJobResponse"
    "fixture/StopSNOMEDCTInferenceJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopSNOMEDCTInferenceJob)
