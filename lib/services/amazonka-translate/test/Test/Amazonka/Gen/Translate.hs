{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Translate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
--         [ requestCreateParallelData $
--             newCreateParallelData
--
--         , requestDeleteParallelData $
--             newDeleteParallelData
--
--         , requestDeleteTerminology $
--             newDeleteTerminology
--
--         , requestDescribeTextTranslationJob $
--             newDescribeTextTranslationJob
--
--         , requestGetParallelData $
--             newGetParallelData
--
--         , requestGetTerminology $
--             newGetTerminology
--
--         , requestImportTerminology $
--             newImportTerminology
--
--         , requestListLanguages $
--             newListLanguages
--
--         , requestListParallelData $
--             newListParallelData
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTerminologies $
--             newListTerminologies
--
--         , requestListTextTranslationJobs $
--             newListTextTranslationJobs
--
--         , requestStartTextTranslationJob $
--             newStartTextTranslationJob
--
--         , requestStopTextTranslationJob $
--             newStopTextTranslationJob
--
--         , requestTagResource $
--             newTagResource
--
--         , requestTranslateText $
--             newTranslateText
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateParallelData $
--             newUpdateParallelData
--
--           ]

--     , testGroup "response"
--         [ responseCreateParallelData $
--             newCreateParallelDataResponse
--
--         , responseDeleteParallelData $
--             newDeleteParallelDataResponse
--
--         , responseDeleteTerminology $
--             newDeleteTerminologyResponse
--
--         , responseDescribeTextTranslationJob $
--             newDescribeTextTranslationJobResponse
--
--         , responseGetParallelData $
--             newGetParallelDataResponse
--
--         , responseGetTerminology $
--             newGetTerminologyResponse
--
--         , responseImportTerminology $
--             newImportTerminologyResponse
--
--         , responseListLanguages $
--             newListLanguagesResponse
--
--         , responseListParallelData $
--             newListParallelDataResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTerminologies $
--             newListTerminologiesResponse
--
--         , responseListTextTranslationJobs $
--             newListTextTranslationJobsResponse
--
--         , responseStartTextTranslationJob $
--             newStartTextTranslationJobResponse
--
--         , responseStopTextTranslationJob $
--             newStopTextTranslationJobResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseTranslateText $
--             newTranslateTextResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateParallelData $
--             newUpdateParallelDataResponse
--
--           ]
--     ]

-- Requests

requestCreateParallelData :: CreateParallelData -> TestTree
requestCreateParallelData =
  req
    "CreateParallelData"
    "fixture/CreateParallelData.yaml"

requestDeleteParallelData :: DeleteParallelData -> TestTree
requestDeleteParallelData =
  req
    "DeleteParallelData"
    "fixture/DeleteParallelData.yaml"

requestDeleteTerminology :: DeleteTerminology -> TestTree
requestDeleteTerminology =
  req
    "DeleteTerminology"
    "fixture/DeleteTerminology.yaml"

requestDescribeTextTranslationJob :: DescribeTextTranslationJob -> TestTree
requestDescribeTextTranslationJob =
  req
    "DescribeTextTranslationJob"
    "fixture/DescribeTextTranslationJob.yaml"

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

requestImportTerminology :: ImportTerminology -> TestTree
requestImportTerminology =
  req
    "ImportTerminology"
    "fixture/ImportTerminology.yaml"

requestListLanguages :: ListLanguages -> TestTree
requestListLanguages =
  req
    "ListLanguages"
    "fixture/ListLanguages.yaml"

requestListParallelData :: ListParallelData -> TestTree
requestListParallelData =
  req
    "ListParallelData"
    "fixture/ListParallelData.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTerminologies :: ListTerminologies -> TestTree
requestListTerminologies =
  req
    "ListTerminologies"
    "fixture/ListTerminologies.yaml"

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

requestStopTextTranslationJob :: StopTextTranslationJob -> TestTree
requestStopTextTranslationJob =
  req
    "StopTextTranslationJob"
    "fixture/StopTextTranslationJob.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestTranslateText :: TranslateText -> TestTree
requestTranslateText =
  req
    "TranslateText"
    "fixture/TranslateText.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateParallelData :: UpdateParallelData -> TestTree
requestUpdateParallelData =
  req
    "UpdateParallelData"
    "fixture/UpdateParallelData.yaml"

-- Responses

responseCreateParallelData :: CreateParallelDataResponse -> TestTree
responseCreateParallelData =
  res
    "CreateParallelDataResponse"
    "fixture/CreateParallelDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateParallelData)

responseDeleteParallelData :: DeleteParallelDataResponse -> TestTree
responseDeleteParallelData =
  res
    "DeleteParallelDataResponse"
    "fixture/DeleteParallelDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteParallelData)

responseDeleteTerminology :: DeleteTerminologyResponse -> TestTree
responseDeleteTerminology =
  res
    "DeleteTerminologyResponse"
    "fixture/DeleteTerminologyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTerminology)

responseDescribeTextTranslationJob :: DescribeTextTranslationJobResponse -> TestTree
responseDescribeTextTranslationJob =
  res
    "DescribeTextTranslationJobResponse"
    "fixture/DescribeTextTranslationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTextTranslationJob)

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

responseImportTerminology :: ImportTerminologyResponse -> TestTree
responseImportTerminology =
  res
    "ImportTerminologyResponse"
    "fixture/ImportTerminologyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportTerminology)

responseListLanguages :: ListLanguagesResponse -> TestTree
responseListLanguages =
  res
    "ListLanguagesResponse"
    "fixture/ListLanguagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLanguages)

responseListParallelData :: ListParallelDataResponse -> TestTree
responseListParallelData =
  res
    "ListParallelDataResponse"
    "fixture/ListParallelDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListParallelData)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTerminologies :: ListTerminologiesResponse -> TestTree
responseListTerminologies =
  res
    "ListTerminologiesResponse"
    "fixture/ListTerminologiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTerminologies)

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

responseStopTextTranslationJob :: StopTextTranslationJobResponse -> TestTree
responseStopTextTranslationJob =
  res
    "StopTextTranslationJobResponse"
    "fixture/StopTextTranslationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopTextTranslationJob)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseTranslateText :: TranslateTextResponse -> TestTree
responseTranslateText =
  res
    "TranslateTextResponse"
    "fixture/TranslateTextResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TranslateText)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateParallelData :: UpdateParallelDataResponse -> TestTree
responseUpdateParallelData =
  res
    "UpdateParallelDataResponse"
    "fixture/UpdateParallelDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateParallelData)
