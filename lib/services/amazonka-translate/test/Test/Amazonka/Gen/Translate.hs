{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Translate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Translate where

import Amazonka.Translate
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.Translate.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribeTextTranslationJob $
--             newDescribeTextTranslationJob
--
--         , requestListTerminologies $
--             newListTerminologies
--
--         , requestCreateParallelData $
--             newCreateParallelData
--
--         , requestUpdateParallelData $
--             newUpdateParallelData
--
--         , requestDeleteParallelData $
--             newDeleteParallelData
--
--         , requestGetParallelData $
--             newGetParallelData
--
--         , requestGetTerminology $
--             newGetTerminology
--
--         , requestTranslateText $
--             newTranslateText
--
--         , requestImportTerminology $
--             newImportTerminology
--
--         , requestStopTextTranslationJob $
--             newStopTextTranslationJob
--
--         , requestDeleteTerminology $
--             newDeleteTerminology
--
--         , requestListTextTranslationJobs $
--             newListTextTranslationJobs
--
--         , requestStartTextTranslationJob $
--             newStartTextTranslationJob
--
--         , requestListParallelData $
--             newListParallelData
--
--           ]

--     , testGroup "response"
--         [ responseDescribeTextTranslationJob $
--             newDescribeTextTranslationJobResponse
--
--         , responseListTerminologies $
--             newListTerminologiesResponse
--
--         , responseCreateParallelData $
--             newCreateParallelDataResponse
--
--         , responseUpdateParallelData $
--             newUpdateParallelDataResponse
--
--         , responseDeleteParallelData $
--             newDeleteParallelDataResponse
--
--         , responseGetParallelData $
--             newGetParallelDataResponse
--
--         , responseGetTerminology $
--             newGetTerminologyResponse
--
--         , responseTranslateText $
--             newTranslateTextResponse
--
--         , responseImportTerminology $
--             newImportTerminologyResponse
--
--         , responseStopTextTranslationJob $
--             newStopTextTranslationJobResponse
--
--         , responseDeleteTerminology $
--             newDeleteTerminologyResponse
--
--         , responseListTextTranslationJobs $
--             newListTextTranslationJobsResponse
--
--         , responseStartTextTranslationJob $
--             newStartTextTranslationJobResponse
--
--         , responseListParallelData $
--             newListParallelDataResponse
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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTextTranslationJob)

responseListTerminologies :: ListTerminologiesResponse -> TestTree
responseListTerminologies =
  res
    "ListTerminologiesResponse"
    "fixture/ListTerminologiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTerminologies)

responseCreateParallelData :: CreateParallelDataResponse -> TestTree
responseCreateParallelData =
  res
    "CreateParallelDataResponse"
    "fixture/CreateParallelDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateParallelData)

responseUpdateParallelData :: UpdateParallelDataResponse -> TestTree
responseUpdateParallelData =
  res
    "UpdateParallelDataResponse"
    "fixture/UpdateParallelDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateParallelData)

responseDeleteParallelData :: DeleteParallelDataResponse -> TestTree
responseDeleteParallelData =
  res
    "DeleteParallelDataResponse"
    "fixture/DeleteParallelDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteParallelData)

responseGetParallelData :: GetParallelDataResponse -> TestTree
responseGetParallelData =
  res
    "GetParallelDataResponse"
    "fixture/GetParallelDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetParallelData)

responseGetTerminology :: GetTerminologyResponse -> TestTree
responseGetTerminology =
  res
    "GetTerminologyResponse"
    "fixture/GetTerminologyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTerminology)

responseTranslateText :: TranslateTextResponse -> TestTree
responseTranslateText =
  res
    "TranslateTextResponse"
    "fixture/TranslateTextResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TranslateText)

responseImportTerminology :: ImportTerminologyResponse -> TestTree
responseImportTerminology =
  res
    "ImportTerminologyResponse"
    "fixture/ImportTerminologyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportTerminology)

responseStopTextTranslationJob :: StopTextTranslationJobResponse -> TestTree
responseStopTextTranslationJob =
  res
    "StopTextTranslationJobResponse"
    "fixture/StopTextTranslationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopTextTranslationJob)

responseDeleteTerminology :: DeleteTerminologyResponse -> TestTree
responseDeleteTerminology =
  res
    "DeleteTerminologyResponse"
    "fixture/DeleteTerminologyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTerminology)

responseListTextTranslationJobs :: ListTextTranslationJobsResponse -> TestTree
responseListTextTranslationJobs =
  res
    "ListTextTranslationJobsResponse"
    "fixture/ListTextTranslationJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTextTranslationJobs)

responseStartTextTranslationJob :: StartTextTranslationJobResponse -> TestTree
responseStartTextTranslationJob =
  res
    "StartTextTranslationJobResponse"
    "fixture/StartTextTranslationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartTextTranslationJob)

responseListParallelData :: ListParallelDataResponse -> TestTree
responseListParallelData =
  res
    "ListParallelDataResponse"
    "fixture/ListParallelDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListParallelData)
