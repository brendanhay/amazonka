{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.IoTEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.IoTEvents where

import Amazonka.IoTEvents
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.IoTEvents.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListInputs $
--             newListInputs
--
--         , requestStartDetectorModelAnalysis $
--             newStartDetectorModelAnalysis
--
--         , requestPutLoggingOptions $
--             newPutLoggingOptions
--
--         , requestDescribeDetectorModelAnalysis $
--             newDescribeDetectorModelAnalysis
--
--         , requestCreateInput $
--             newCreateInput
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListAlarmModels $
--             newListAlarmModels
--
--         , requestDeleteAlarmModel $
--             newDeleteAlarmModel
--
--         , requestUpdateAlarmModel $
--             newUpdateAlarmModel
--
--         , requestCreateAlarmModel $
--             newCreateAlarmModel
--
--         , requestGetDetectorModelAnalysisResults $
--             newGetDetectorModelAnalysisResults
--
--         , requestListDetectorModelVersions $
--             newListDetectorModelVersions
--
--         , requestDescribeAlarmModel $
--             newDescribeAlarmModel
--
--         , requestCreateDetectorModel $
--             newCreateDetectorModel
--
--         , requestListDetectorModels $
--             newListDetectorModels
--
--         , requestUpdateDetectorModel $
--             newUpdateDetectorModel
--
--         , requestDeleteDetectorModel $
--             newDeleteDetectorModel
--
--         , requestDeleteInput $
--             newDeleteInput
--
--         , requestUpdateInput $
--             newUpdateInput
--
--         , requestListAlarmModelVersions $
--             newListAlarmModelVersions
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDescribeDetectorModel $
--             newDescribeDetectorModel
--
--         , requestDescribeInput $
--             newDescribeInput
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListInputRoutings $
--             newListInputRoutings
--
--         , requestDescribeLoggingOptions $
--             newDescribeLoggingOptions
--
--           ]

--     , testGroup "response"
--         [ responseListInputs $
--             newListInputsResponse
--
--         , responseStartDetectorModelAnalysis $
--             newStartDetectorModelAnalysisResponse
--
--         , responsePutLoggingOptions $
--             newPutLoggingOptionsResponse
--
--         , responseDescribeDetectorModelAnalysis $
--             newDescribeDetectorModelAnalysisResponse
--
--         , responseCreateInput $
--             newCreateInputResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListAlarmModels $
--             newListAlarmModelsResponse
--
--         , responseDeleteAlarmModel $
--             newDeleteAlarmModelResponse
--
--         , responseUpdateAlarmModel $
--             newUpdateAlarmModelResponse
--
--         , responseCreateAlarmModel $
--             newCreateAlarmModelResponse
--
--         , responseGetDetectorModelAnalysisResults $
--             newGetDetectorModelAnalysisResultsResponse
--
--         , responseListDetectorModelVersions $
--             newListDetectorModelVersionsResponse
--
--         , responseDescribeAlarmModel $
--             newDescribeAlarmModelResponse
--
--         , responseCreateDetectorModel $
--             newCreateDetectorModelResponse
--
--         , responseListDetectorModels $
--             newListDetectorModelsResponse
--
--         , responseUpdateDetectorModel $
--             newUpdateDetectorModelResponse
--
--         , responseDeleteDetectorModel $
--             newDeleteDetectorModelResponse
--
--         , responseDeleteInput $
--             newDeleteInputResponse
--
--         , responseUpdateInput $
--             newUpdateInputResponse
--
--         , responseListAlarmModelVersions $
--             newListAlarmModelVersionsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDescribeDetectorModel $
--             newDescribeDetectorModelResponse
--
--         , responseDescribeInput $
--             newDescribeInputResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListInputRoutings $
--             newListInputRoutingsResponse
--
--         , responseDescribeLoggingOptions $
--             newDescribeLoggingOptionsResponse
--
--           ]
--     ]

-- Requests

requestListInputs :: ListInputs -> TestTree
requestListInputs =
  req
    "ListInputs"
    "fixture/ListInputs.yaml"

requestStartDetectorModelAnalysis :: StartDetectorModelAnalysis -> TestTree
requestStartDetectorModelAnalysis =
  req
    "StartDetectorModelAnalysis"
    "fixture/StartDetectorModelAnalysis.yaml"

requestPutLoggingOptions :: PutLoggingOptions -> TestTree
requestPutLoggingOptions =
  req
    "PutLoggingOptions"
    "fixture/PutLoggingOptions.yaml"

requestDescribeDetectorModelAnalysis :: DescribeDetectorModelAnalysis -> TestTree
requestDescribeDetectorModelAnalysis =
  req
    "DescribeDetectorModelAnalysis"
    "fixture/DescribeDetectorModelAnalysis.yaml"

requestCreateInput :: CreateInput -> TestTree
requestCreateInput =
  req
    "CreateInput"
    "fixture/CreateInput.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListAlarmModels :: ListAlarmModels -> TestTree
requestListAlarmModels =
  req
    "ListAlarmModels"
    "fixture/ListAlarmModels.yaml"

requestDeleteAlarmModel :: DeleteAlarmModel -> TestTree
requestDeleteAlarmModel =
  req
    "DeleteAlarmModel"
    "fixture/DeleteAlarmModel.yaml"

requestUpdateAlarmModel :: UpdateAlarmModel -> TestTree
requestUpdateAlarmModel =
  req
    "UpdateAlarmModel"
    "fixture/UpdateAlarmModel.yaml"

requestCreateAlarmModel :: CreateAlarmModel -> TestTree
requestCreateAlarmModel =
  req
    "CreateAlarmModel"
    "fixture/CreateAlarmModel.yaml"

requestGetDetectorModelAnalysisResults :: GetDetectorModelAnalysisResults -> TestTree
requestGetDetectorModelAnalysisResults =
  req
    "GetDetectorModelAnalysisResults"
    "fixture/GetDetectorModelAnalysisResults.yaml"

requestListDetectorModelVersions :: ListDetectorModelVersions -> TestTree
requestListDetectorModelVersions =
  req
    "ListDetectorModelVersions"
    "fixture/ListDetectorModelVersions.yaml"

requestDescribeAlarmModel :: DescribeAlarmModel -> TestTree
requestDescribeAlarmModel =
  req
    "DescribeAlarmModel"
    "fixture/DescribeAlarmModel.yaml"

requestCreateDetectorModel :: CreateDetectorModel -> TestTree
requestCreateDetectorModel =
  req
    "CreateDetectorModel"
    "fixture/CreateDetectorModel.yaml"

requestListDetectorModels :: ListDetectorModels -> TestTree
requestListDetectorModels =
  req
    "ListDetectorModels"
    "fixture/ListDetectorModels.yaml"

requestUpdateDetectorModel :: UpdateDetectorModel -> TestTree
requestUpdateDetectorModel =
  req
    "UpdateDetectorModel"
    "fixture/UpdateDetectorModel.yaml"

requestDeleteDetectorModel :: DeleteDetectorModel -> TestTree
requestDeleteDetectorModel =
  req
    "DeleteDetectorModel"
    "fixture/DeleteDetectorModel.yaml"

requestDeleteInput :: DeleteInput -> TestTree
requestDeleteInput =
  req
    "DeleteInput"
    "fixture/DeleteInput.yaml"

requestUpdateInput :: UpdateInput -> TestTree
requestUpdateInput =
  req
    "UpdateInput"
    "fixture/UpdateInput.yaml"

requestListAlarmModelVersions :: ListAlarmModelVersions -> TestTree
requestListAlarmModelVersions =
  req
    "ListAlarmModelVersions"
    "fixture/ListAlarmModelVersions.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDescribeDetectorModel :: DescribeDetectorModel -> TestTree
requestDescribeDetectorModel =
  req
    "DescribeDetectorModel"
    "fixture/DescribeDetectorModel.yaml"

requestDescribeInput :: DescribeInput -> TestTree
requestDescribeInput =
  req
    "DescribeInput"
    "fixture/DescribeInput.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestListInputRoutings :: ListInputRoutings -> TestTree
requestListInputRoutings =
  req
    "ListInputRoutings"
    "fixture/ListInputRoutings.yaml"

requestDescribeLoggingOptions :: DescribeLoggingOptions -> TestTree
requestDescribeLoggingOptions =
  req
    "DescribeLoggingOptions"
    "fixture/DescribeLoggingOptions.yaml"

-- Responses

responseListInputs :: ListInputsResponse -> TestTree
responseListInputs =
  res
    "ListInputsResponse"
    "fixture/ListInputsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInputs)

responseStartDetectorModelAnalysis :: StartDetectorModelAnalysisResponse -> TestTree
responseStartDetectorModelAnalysis =
  res
    "StartDetectorModelAnalysisResponse"
    "fixture/StartDetectorModelAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDetectorModelAnalysis)

responsePutLoggingOptions :: PutLoggingOptionsResponse -> TestTree
responsePutLoggingOptions =
  res
    "PutLoggingOptionsResponse"
    "fixture/PutLoggingOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutLoggingOptions)

responseDescribeDetectorModelAnalysis :: DescribeDetectorModelAnalysisResponse -> TestTree
responseDescribeDetectorModelAnalysis =
  res
    "DescribeDetectorModelAnalysisResponse"
    "fixture/DescribeDetectorModelAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDetectorModelAnalysis)

responseCreateInput :: CreateInputResponse -> TestTree
responseCreateInput =
  res
    "CreateInputResponse"
    "fixture/CreateInputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInput)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListAlarmModels :: ListAlarmModelsResponse -> TestTree
responseListAlarmModels =
  res
    "ListAlarmModelsResponse"
    "fixture/ListAlarmModelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAlarmModels)

responseDeleteAlarmModel :: DeleteAlarmModelResponse -> TestTree
responseDeleteAlarmModel =
  res
    "DeleteAlarmModelResponse"
    "fixture/DeleteAlarmModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAlarmModel)

responseUpdateAlarmModel :: UpdateAlarmModelResponse -> TestTree
responseUpdateAlarmModel =
  res
    "UpdateAlarmModelResponse"
    "fixture/UpdateAlarmModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAlarmModel)

responseCreateAlarmModel :: CreateAlarmModelResponse -> TestTree
responseCreateAlarmModel =
  res
    "CreateAlarmModelResponse"
    "fixture/CreateAlarmModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAlarmModel)

responseGetDetectorModelAnalysisResults :: GetDetectorModelAnalysisResultsResponse -> TestTree
responseGetDetectorModelAnalysisResults =
  res
    "GetDetectorModelAnalysisResultsResponse"
    "fixture/GetDetectorModelAnalysisResultsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDetectorModelAnalysisResults)

responseListDetectorModelVersions :: ListDetectorModelVersionsResponse -> TestTree
responseListDetectorModelVersions =
  res
    "ListDetectorModelVersionsResponse"
    "fixture/ListDetectorModelVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDetectorModelVersions)

responseDescribeAlarmModel :: DescribeAlarmModelResponse -> TestTree
responseDescribeAlarmModel =
  res
    "DescribeAlarmModelResponse"
    "fixture/DescribeAlarmModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAlarmModel)

responseCreateDetectorModel :: CreateDetectorModelResponse -> TestTree
responseCreateDetectorModel =
  res
    "CreateDetectorModelResponse"
    "fixture/CreateDetectorModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDetectorModel)

responseListDetectorModels :: ListDetectorModelsResponse -> TestTree
responseListDetectorModels =
  res
    "ListDetectorModelsResponse"
    "fixture/ListDetectorModelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDetectorModels)

responseUpdateDetectorModel :: UpdateDetectorModelResponse -> TestTree
responseUpdateDetectorModel =
  res
    "UpdateDetectorModelResponse"
    "fixture/UpdateDetectorModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDetectorModel)

responseDeleteDetectorModel :: DeleteDetectorModelResponse -> TestTree
responseDeleteDetectorModel =
  res
    "DeleteDetectorModelResponse"
    "fixture/DeleteDetectorModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDetectorModel)

responseDeleteInput :: DeleteInputResponse -> TestTree
responseDeleteInput =
  res
    "DeleteInputResponse"
    "fixture/DeleteInputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInput)

responseUpdateInput :: UpdateInputResponse -> TestTree
responseUpdateInput =
  res
    "UpdateInputResponse"
    "fixture/UpdateInputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateInput)

responseListAlarmModelVersions :: ListAlarmModelVersionsResponse -> TestTree
responseListAlarmModelVersions =
  res
    "ListAlarmModelVersionsResponse"
    "fixture/ListAlarmModelVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAlarmModelVersions)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseDescribeDetectorModel :: DescribeDetectorModelResponse -> TestTree
responseDescribeDetectorModel =
  res
    "DescribeDetectorModelResponse"
    "fixture/DescribeDetectorModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDetectorModel)

responseDescribeInput :: DescribeInputResponse -> TestTree
responseDescribeInput =
  res
    "DescribeInputResponse"
    "fixture/DescribeInputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInput)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseListInputRoutings :: ListInputRoutingsResponse -> TestTree
responseListInputRoutings =
  res
    "ListInputRoutingsResponse"
    "fixture/ListInputRoutingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInputRoutings)

responseDescribeLoggingOptions :: DescribeLoggingOptionsResponse -> TestTree
responseDescribeLoggingOptions =
  res
    "DescribeLoggingOptionsResponse"
    "fixture/DescribeLoggingOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLoggingOptions)
