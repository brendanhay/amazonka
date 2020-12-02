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
--             describeTextTranslationJob
--
--         , requestListTerminologies $
--             listTerminologies
--
--         , requestCreateParallelData $
--             createParallelData
--
--         , requestUpdateParallelData $
--             updateParallelData
--
--         , requestDeleteParallelData $
--             deleteParallelData
--
--         , requestGetParallelData $
--             getParallelData
--
--         , requestGetTerminology $
--             getTerminology
--
--         , requestTranslateText $
--             translateText
--
--         , requestImportTerminology $
--             importTerminology
--
--         , requestStopTextTranslationJob $
--             stopTextTranslationJob
--
--         , requestDeleteTerminology $
--             deleteTerminology
--
--         , requestListTextTranslationJobs $
--             listTextTranslationJobs
--
--         , requestStartTextTranslationJob $
--             startTextTranslationJob
--
--         , requestListParallelData $
--             listParallelData
--
--           ]

--     , testGroup "response"
--         [ responseDescribeTextTranslationJob $
--             describeTextTranslationJobResponse
--
--         , responseListTerminologies $
--             listTerminologiesResponse
--
--         , responseCreateParallelData $
--             createParallelDataResponse
--
--         , responseUpdateParallelData $
--             updateParallelDataResponse
--
--         , responseDeleteParallelData $
--             deleteParallelDataResponse
--
--         , responseGetParallelData $
--             getParallelDataResponse
--
--         , responseGetTerminology $
--             getTerminologyResponse
--
--         , responseTranslateText $
--             translateTextResponse
--
--         , responseImportTerminology $
--             importTerminologyResponse
--
--         , responseStopTextTranslationJob $
--             stopTextTranslationJobResponse
--
--         , responseDeleteTerminology $
--             deleteTerminologyResponse
--
--         , responseListTextTranslationJobs $
--             listTextTranslationJobsResponse
--
--         , responseStartTextTranslationJob $
--             startTextTranslationJobResponse
--
--         , responseListParallelData $
--             listParallelDataResponse
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
    translate
    (Proxy :: Proxy DescribeTextTranslationJob)

responseListTerminologies :: ListTerminologiesResponse -> TestTree
responseListTerminologies =
  res
    "ListTerminologiesResponse"
    "fixture/ListTerminologiesResponse.proto"
    translate
    (Proxy :: Proxy ListTerminologies)

responseCreateParallelData :: CreateParallelDataResponse -> TestTree
responseCreateParallelData =
  res
    "CreateParallelDataResponse"
    "fixture/CreateParallelDataResponse.proto"
    translate
    (Proxy :: Proxy CreateParallelData)

responseUpdateParallelData :: UpdateParallelDataResponse -> TestTree
responseUpdateParallelData =
  res
    "UpdateParallelDataResponse"
    "fixture/UpdateParallelDataResponse.proto"
    translate
    (Proxy :: Proxy UpdateParallelData)

responseDeleteParallelData :: DeleteParallelDataResponse -> TestTree
responseDeleteParallelData =
  res
    "DeleteParallelDataResponse"
    "fixture/DeleteParallelDataResponse.proto"
    translate
    (Proxy :: Proxy DeleteParallelData)

responseGetParallelData :: GetParallelDataResponse -> TestTree
responseGetParallelData =
  res
    "GetParallelDataResponse"
    "fixture/GetParallelDataResponse.proto"
    translate
    (Proxy :: Proxy GetParallelData)

responseGetTerminology :: GetTerminologyResponse -> TestTree
responseGetTerminology =
  res
    "GetTerminologyResponse"
    "fixture/GetTerminologyResponse.proto"
    translate
    (Proxy :: Proxy GetTerminology)

responseTranslateText :: TranslateTextResponse -> TestTree
responseTranslateText =
  res
    "TranslateTextResponse"
    "fixture/TranslateTextResponse.proto"
    translate
    (Proxy :: Proxy TranslateText)

responseImportTerminology :: ImportTerminologyResponse -> TestTree
responseImportTerminology =
  res
    "ImportTerminologyResponse"
    "fixture/ImportTerminologyResponse.proto"
    translate
    (Proxy :: Proxy ImportTerminology)

responseStopTextTranslationJob :: StopTextTranslationJobResponse -> TestTree
responseStopTextTranslationJob =
  res
    "StopTextTranslationJobResponse"
    "fixture/StopTextTranslationJobResponse.proto"
    translate
    (Proxy :: Proxy StopTextTranslationJob)

responseDeleteTerminology :: DeleteTerminologyResponse -> TestTree
responseDeleteTerminology =
  res
    "DeleteTerminologyResponse"
    "fixture/DeleteTerminologyResponse.proto"
    translate
    (Proxy :: Proxy DeleteTerminology)

responseListTextTranslationJobs :: ListTextTranslationJobsResponse -> TestTree
responseListTextTranslationJobs =
  res
    "ListTextTranslationJobsResponse"
    "fixture/ListTextTranslationJobsResponse.proto"
    translate
    (Proxy :: Proxy ListTextTranslationJobs)

responseStartTextTranslationJob :: StartTextTranslationJobResponse -> TestTree
responseStartTextTranslationJob =
  res
    "StartTextTranslationJobResponse"
    "fixture/StartTextTranslationJobResponse.proto"
    translate
    (Proxy :: Proxy StartTextTranslationJob)

responseListParallelData :: ListParallelDataResponse -> TestTree
responseListParallelData =
  res
    "ListParallelDataResponse"
    "fixture/ListParallelDataResponse.proto"
    translate
    (Proxy :: Proxy ListParallelData)
