{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Translate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Translate where

import Data.Proxy
import Network.AWS.Translate
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.Translate.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateParallelData $
--             newCreateParallelData
--
--         , requestDescribeTextTranslationJob $
--             newDescribeTextTranslationJob
--
--         , requestStopTextTranslationJob $
--             newStopTextTranslationJob
--
--         , requestStartTextTranslationJob $
--             newStartTextTranslationJob
--
--         , requestImportTerminology $
--             newImportTerminology
--
--         , requestListTextTranslationJobs $
--             newListTextTranslationJobs
--
--         , requestGetParallelData $
--             newGetParallelData
--
--         , requestDeleteParallelData $
--             newDeleteParallelData
--
--         , requestUpdateParallelData $
--             newUpdateParallelData
--
--         , requestDeleteTerminology $
--             newDeleteTerminology
--
--         , requestListTerminologies $
--             newListTerminologies
--
--         , requestListParallelData $
--             newListParallelData
--
--         , requestTranslateText $
--             newTranslateText
--
--         , requestGetTerminology $
--             newGetTerminology
--
--           ]

--     , testGroup "response"
--         [ responseCreateParallelData $
--             newCreateParallelDataResponse
--
--         , responseDescribeTextTranslationJob $
--             newDescribeTextTranslationJobResponse
--
--         , responseStopTextTranslationJob $
--             newStopTextTranslationJobResponse
--
--         , responseStartTextTranslationJob $
--             newStartTextTranslationJobResponse
--
--         , responseImportTerminology $
--             newImportTerminologyResponse
--
--         , responseListTextTranslationJobs $
--             newListTextTranslationJobsResponse
--
--         , responseGetParallelData $
--             newGetParallelDataResponse
--
--         , responseDeleteParallelData $
--             newDeleteParallelDataResponse
--
--         , responseUpdateParallelData $
--             newUpdateParallelDataResponse
--
--         , responseDeleteTerminology $
--             newDeleteTerminologyResponse
--
--         , responseListTerminologies $
--             newListTerminologiesResponse
--
--         , responseListParallelData $
--             newListParallelDataResponse
--
--         , responseTranslateText $
--             newTranslateTextResponse
--
--         , responseGetTerminology $
--             newGetTerminologyResponse
--
--           ]
--     ]

-- Requests

requestCreateParallelData :: CreateParallelData -> TestTree
requestCreateParallelData =
  req
    "CreateParallelData"
    "fixture/CreateParallelData.yaml"

requestDescribeTextTranslationJob :: DescribeTextTranslationJob -> TestTree
requestDescribeTextTranslationJob =
  req
    "DescribeTextTranslationJob"
    "fixture/DescribeTextTranslationJob.yaml"

requestStopTextTranslationJob :: StopTextTranslationJob -> TestTree
requestStopTextTranslationJob =
  req
    "StopTextTranslationJob"
    "fixture/StopTextTranslationJob.yaml"

requestStartTextTranslationJob :: StartTextTranslationJob -> TestTree
requestStartTextTranslationJob =
  req
    "StartTextTranslationJob"
    "fixture/StartTextTranslationJob.yaml"

requestImportTerminology :: ImportTerminology -> TestTree
requestImportTerminology =
  req
    "ImportTerminology"
    "fixture/ImportTerminology.yaml"

requestListTextTranslationJobs :: ListTextTranslationJobs -> TestTree
requestListTextTranslationJobs =
  req
    "ListTextTranslationJobs"
    "fixture/ListTextTranslationJobs.yaml"

requestGetParallelData :: GetParallelData -> TestTree
requestGetParallelData =
  req
    "GetParallelData"
    "fixture/GetParallelData.yaml"

requestDeleteParallelData :: DeleteParallelData -> TestTree
requestDeleteParallelData =
  req
    "DeleteParallelData"
    "fixture/DeleteParallelData.yaml"

requestUpdateParallelData :: UpdateParallelData -> TestTree
requestUpdateParallelData =
  req
    "UpdateParallelData"
    "fixture/UpdateParallelData.yaml"

requestDeleteTerminology :: DeleteTerminology -> TestTree
requestDeleteTerminology =
  req
    "DeleteTerminology"
    "fixture/DeleteTerminology.yaml"

requestListTerminologies :: ListTerminologies -> TestTree
requestListTerminologies =
  req
    "ListTerminologies"
    "fixture/ListTerminologies.yaml"

requestListParallelData :: ListParallelData -> TestTree
requestListParallelData =
  req
    "ListParallelData"
    "fixture/ListParallelData.yaml"

requestTranslateText :: TranslateText -> TestTree
requestTranslateText =
  req
    "TranslateText"
    "fixture/TranslateText.yaml"

requestGetTerminology :: GetTerminology -> TestTree
requestGetTerminology =
  req
    "GetTerminology"
    "fixture/GetTerminology.yaml"

-- Responses

responseCreateParallelData :: CreateParallelDataResponse -> TestTree
responseCreateParallelData =
  res
    "CreateParallelDataResponse"
    "fixture/CreateParallelDataResponse.proto"
    defaultService
    (Proxy :: Proxy CreateParallelData)

responseDescribeTextTranslationJob :: DescribeTextTranslationJobResponse -> TestTree
responseDescribeTextTranslationJob =
  res
    "DescribeTextTranslationJobResponse"
    "fixture/DescribeTextTranslationJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTextTranslationJob)

responseStopTextTranslationJob :: StopTextTranslationJobResponse -> TestTree
responseStopTextTranslationJob =
  res
    "StopTextTranslationJobResponse"
    "fixture/StopTextTranslationJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopTextTranslationJob)

responseStartTextTranslationJob :: StartTextTranslationJobResponse -> TestTree
responseStartTextTranslationJob =
  res
    "StartTextTranslationJobResponse"
    "fixture/StartTextTranslationJobResponse.proto"
    defaultService
    (Proxy :: Proxy StartTextTranslationJob)

responseImportTerminology :: ImportTerminologyResponse -> TestTree
responseImportTerminology =
  res
    "ImportTerminologyResponse"
    "fixture/ImportTerminologyResponse.proto"
    defaultService
    (Proxy :: Proxy ImportTerminology)

responseListTextTranslationJobs :: ListTextTranslationJobsResponse -> TestTree
responseListTextTranslationJobs =
  res
    "ListTextTranslationJobsResponse"
    "fixture/ListTextTranslationJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTextTranslationJobs)

responseGetParallelData :: GetParallelDataResponse -> TestTree
responseGetParallelData =
  res
    "GetParallelDataResponse"
    "fixture/GetParallelDataResponse.proto"
    defaultService
    (Proxy :: Proxy GetParallelData)

responseDeleteParallelData :: DeleteParallelDataResponse -> TestTree
responseDeleteParallelData =
  res
    "DeleteParallelDataResponse"
    "fixture/DeleteParallelDataResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteParallelData)

responseUpdateParallelData :: UpdateParallelDataResponse -> TestTree
responseUpdateParallelData =
  res
    "UpdateParallelDataResponse"
    "fixture/UpdateParallelDataResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateParallelData)

responseDeleteTerminology :: DeleteTerminologyResponse -> TestTree
responseDeleteTerminology =
  res
    "DeleteTerminologyResponse"
    "fixture/DeleteTerminologyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTerminology)

responseListTerminologies :: ListTerminologiesResponse -> TestTree
responseListTerminologies =
  res
    "ListTerminologiesResponse"
    "fixture/ListTerminologiesResponse.proto"
    defaultService
    (Proxy :: Proxy ListTerminologies)

responseListParallelData :: ListParallelDataResponse -> TestTree
responseListParallelData =
  res
    "ListParallelDataResponse"
    "fixture/ListParallelDataResponse.proto"
    defaultService
    (Proxy :: Proxy ListParallelData)

responseTranslateText :: TranslateTextResponse -> TestTree
responseTranslateText =
  res
    "TranslateTextResponse"
    "fixture/TranslateTextResponse.proto"
    defaultService
    (Proxy :: Proxy TranslateText)

responseGetTerminology :: GetTerminologyResponse -> TestTree
responseGetTerminology =
  res
    "GetTerminologyResponse"
    "fixture/GetTerminologyResponse.proto"
    defaultService
    (Proxy :: Proxy GetTerminology)
