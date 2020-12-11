{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Translate
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestDescribeTextTranslationJob $
--             mkDescribeTextTranslationJob
--
--         , requestListTerminologies $
--             mkListTerminologies
--
--         , requestCreateParallelData $
--             mkCreateParallelData
--
--         , requestUpdateParallelData $
--             mkUpdateParallelData
--
--         , requestDeleteParallelData $
--             mkDeleteParallelData
--
--         , requestGetParallelData $
--             mkGetParallelData
--
--         , requestGetTerminology $
--             mkGetTerminology
--
--         , requestTranslateText $
--             mkTranslateText
--
--         , requestImportTerminology $
--             mkImportTerminology
--
--         , requestStopTextTranslationJob $
--             mkStopTextTranslationJob
--
--         , requestDeleteTerminology $
--             mkDeleteTerminology
--
--         , requestListTextTranslationJobs $
--             mkListTextTranslationJobs
--
--         , requestStartTextTranslationJob $
--             mkStartTextTranslationJob
--
--         , requestListParallelData $
--             mkListParallelData
--
--           ]

--     , testGroup "response"
--         [ responseDescribeTextTranslationJob $
--             mkDescribeTextTranslationJobResponse
--
--         , responseListTerminologies $
--             mkListTerminologiesResponse
--
--         , responseCreateParallelData $
--             mkCreateParallelDataResponse
--
--         , responseUpdateParallelData $
--             mkUpdateParallelDataResponse
--
--         , responseDeleteParallelData $
--             mkDeleteParallelDataResponse
--
--         , responseGetParallelData $
--             mkGetParallelDataResponse
--
--         , responseGetTerminology $
--             mkGetTerminologyResponse
--
--         , responseTranslateText $
--             mkTranslateTextResponse
--
--         , responseImportTerminology $
--             mkImportTerminologyResponse
--
--         , responseStopTextTranslationJob $
--             mkStopTextTranslationJobResponse
--
--         , responseDeleteTerminology $
--             mkDeleteTerminologyResponse
--
--         , responseListTextTranslationJobs $
--             mkListTextTranslationJobsResponse
--
--         , responseStartTextTranslationJob $
--             mkStartTextTranslationJobResponse
--
--         , responseListParallelData $
--             mkListParallelDataResponse
--
--           ]
--     ]

-- Requests

requestDescribeTextTranslationJob :: DescribeTextTranslationJob -> TestTree
requestDescribeTextTranslationJob =
  req
    "DescribeTextTranslationJob"
    "fixture/DescribeTextTranslationJob.yaml"

requestListTerminologies :: ListTerminologies -> TestTree
requestListTerminologies =
  req
    "ListTerminologies"
    "fixture/ListTerminologies.yaml"

requestCreateParallelData :: CreateParallelData -> TestTree
requestCreateParallelData =
  req
    "CreateParallelData"
    "fixture/CreateParallelData.yaml"

requestUpdateParallelData :: UpdateParallelData -> TestTree
requestUpdateParallelData =
  req
    "UpdateParallelData"
    "fixture/UpdateParallelData.yaml"

requestDeleteParallelData :: DeleteParallelData -> TestTree
requestDeleteParallelData =
  req
    "DeleteParallelData"
    "fixture/DeleteParallelData.yaml"

requestGetParallelData :: GetParallelData -> TestTree
requestGetParallelData =
  req
    "GetParallelData"
    "fixture/GetParallelData.yaml"

requestGetTerminology :: GetTerminology -> TestTree
requestGetTerminology =
  req
    "GetTerminology"
    "fixture/GetTerminology.yaml"

requestTranslateText :: TranslateText -> TestTree
requestTranslateText =
  req
    "TranslateText"
    "fixture/TranslateText.yaml"

requestImportTerminology :: ImportTerminology -> TestTree
requestImportTerminology =
  req
    "ImportTerminology"
    "fixture/ImportTerminology.yaml"

requestStopTextTranslationJob :: StopTextTranslationJob -> TestTree
requestStopTextTranslationJob =
  req
    "StopTextTranslationJob"
    "fixture/StopTextTranslationJob.yaml"

requestDeleteTerminology :: DeleteTerminology -> TestTree
requestDeleteTerminology =
  req
    "DeleteTerminology"
    "fixture/DeleteTerminology.yaml"

requestListTextTranslationJobs :: ListTextTranslationJobs -> TestTree
requestListTextTranslationJobs =
  req
    "ListTextTranslationJobs"
    "fixture/ListTextTranslationJobs.yaml"

requestStartTextTranslationJob :: StartTextTranslationJob -> TestTree
requestStartTextTranslationJob =
  req
    "StartTextTranslationJob"
    "fixture/StartTextTranslationJob.yaml"

requestListParallelData :: ListParallelData -> TestTree
requestListParallelData =
  req
    "ListParallelData"
    "fixture/ListParallelData.yaml"

-- Responses

responseDescribeTextTranslationJob :: DescribeTextTranslationJobResponse -> TestTree
responseDescribeTextTranslationJob =
  res
    "DescribeTextTranslationJobResponse"
    "fixture/DescribeTextTranslationJobResponse.proto"
    translateService
    (Proxy :: Proxy DescribeTextTranslationJob)

responseListTerminologies :: ListTerminologiesResponse -> TestTree
responseListTerminologies =
  res
    "ListTerminologiesResponse"
    "fixture/ListTerminologiesResponse.proto"
    translateService
    (Proxy :: Proxy ListTerminologies)

responseCreateParallelData :: CreateParallelDataResponse -> TestTree
responseCreateParallelData =
  res
    "CreateParallelDataResponse"
    "fixture/CreateParallelDataResponse.proto"
    translateService
    (Proxy :: Proxy CreateParallelData)

responseUpdateParallelData :: UpdateParallelDataResponse -> TestTree
responseUpdateParallelData =
  res
    "UpdateParallelDataResponse"
    "fixture/UpdateParallelDataResponse.proto"
    translateService
    (Proxy :: Proxy UpdateParallelData)

responseDeleteParallelData :: DeleteParallelDataResponse -> TestTree
responseDeleteParallelData =
  res
    "DeleteParallelDataResponse"
    "fixture/DeleteParallelDataResponse.proto"
    translateService
    (Proxy :: Proxy DeleteParallelData)

responseGetParallelData :: GetParallelDataResponse -> TestTree
responseGetParallelData =
  res
    "GetParallelDataResponse"
    "fixture/GetParallelDataResponse.proto"
    translateService
    (Proxy :: Proxy GetParallelData)

responseGetTerminology :: GetTerminologyResponse -> TestTree
responseGetTerminology =
  res
    "GetTerminologyResponse"
    "fixture/GetTerminologyResponse.proto"
    translateService
    (Proxy :: Proxy GetTerminology)

responseTranslateText :: TranslateTextResponse -> TestTree
responseTranslateText =
  res
    "TranslateTextResponse"
    "fixture/TranslateTextResponse.proto"
    translateService
    (Proxy :: Proxy TranslateText)

responseImportTerminology :: ImportTerminologyResponse -> TestTree
responseImportTerminology =
  res
    "ImportTerminologyResponse"
    "fixture/ImportTerminologyResponse.proto"
    translateService
    (Proxy :: Proxy ImportTerminology)

responseStopTextTranslationJob :: StopTextTranslationJobResponse -> TestTree
responseStopTextTranslationJob =
  res
    "StopTextTranslationJobResponse"
    "fixture/StopTextTranslationJobResponse.proto"
    translateService
    (Proxy :: Proxy StopTextTranslationJob)

responseDeleteTerminology :: DeleteTerminologyResponse -> TestTree
responseDeleteTerminology =
  res
    "DeleteTerminologyResponse"
    "fixture/DeleteTerminologyResponse.proto"
    translateService
    (Proxy :: Proxy DeleteTerminology)

responseListTextTranslationJobs :: ListTextTranslationJobsResponse -> TestTree
responseListTextTranslationJobs =
  res
    "ListTextTranslationJobsResponse"
    "fixture/ListTextTranslationJobsResponse.proto"
    translateService
    (Proxy :: Proxy ListTextTranslationJobs)

responseStartTextTranslationJob :: StartTextTranslationJobResponse -> TestTree
responseStartTextTranslationJob =
  res
    "StartTextTranslationJobResponse"
    "fixture/StartTextTranslationJobResponse.proto"
    translateService
    (Proxy :: Proxy StartTextTranslationJob)

responseListParallelData :: ListParallelDataResponse -> TestTree
responseListParallelData =
  res
    "ListParallelDataResponse"
    "fixture/ListParallelDataResponse.proto"
    translateService
    (Proxy :: Proxy ListParallelData)
