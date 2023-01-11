{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.IoTEvents
-- Copyright   : (c) 2013-2023 Brendan Hay
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
--         [ requestCreateAlarmModel $
--             newCreateAlarmModel
--
--         , requestCreateDetectorModel $
--             newCreateDetectorModel
--
--         , requestCreateInput $
--             newCreateInput
--
--         , requestDeleteAlarmModel $
--             newDeleteAlarmModel
--
--         , requestDeleteDetectorModel $
--             newDeleteDetectorModel
--
--         , requestDeleteInput $
--             newDeleteInput
--
--         , requestDescribeAlarmModel $
--             newDescribeAlarmModel
--
--         , requestDescribeDetectorModel $
--             newDescribeDetectorModel
--
--         , requestDescribeDetectorModelAnalysis $
--             newDescribeDetectorModelAnalysis
--
--         , requestDescribeInput $
--             newDescribeInput
--
--         , requestDescribeLoggingOptions $
--             newDescribeLoggingOptions
--
--         , requestGetDetectorModelAnalysisResults $
--             newGetDetectorModelAnalysisResults
--
--         , requestListAlarmModelVersions $
--             newListAlarmModelVersions
--
--         , requestListAlarmModels $
--             newListAlarmModels
--
--         , requestListDetectorModelVersions $
--             newListDetectorModelVersions
--
--         , requestListDetectorModels $
--             newListDetectorModels
--
--         , requestListInputRoutings $
--             newListInputRoutings
--
--         , requestListInputs $
--             newListInputs
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutLoggingOptions $
--             newPutLoggingOptions
--
--         , requestStartDetectorModelAnalysis $
--             newStartDetectorModelAnalysis
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAlarmModel $
--             newUpdateAlarmModel
--
--         , requestUpdateDetectorModel $
--             newUpdateDetectorModel
--
--         , requestUpdateInput $
--             newUpdateInput
--
--           ]

--     , testGroup "response"
--         [ responseCreateAlarmModel $
--             newCreateAlarmModelResponse
--
--         , responseCreateDetectorModel $
--             newCreateDetectorModelResponse
--
--         , responseCreateInput $
--             newCreateInputResponse
--
--         , responseDeleteAlarmModel $
--             newDeleteAlarmModelResponse
--
--         , responseDeleteDetectorModel $
--             newDeleteDetectorModelResponse
--
--         , responseDeleteInput $
--             newDeleteInputResponse
--
--         , responseDescribeAlarmModel $
--             newDescribeAlarmModelResponse
--
--         , responseDescribeDetectorModel $
--             newDescribeDetectorModelResponse
--
--         , responseDescribeDetectorModelAnalysis $
--             newDescribeDetectorModelAnalysisResponse
--
--         , responseDescribeInput $
--             newDescribeInputResponse
--
--         , responseDescribeLoggingOptions $
--             newDescribeLoggingOptionsResponse
--
--         , responseGetDetectorModelAnalysisResults $
--             newGetDetectorModelAnalysisResultsResponse
--
--         , responseListAlarmModelVersions $
--             newListAlarmModelVersionsResponse
--
--         , responseListAlarmModels $
--             newListAlarmModelsResponse
--
--         , responseListDetectorModelVersions $
--             newListDetectorModelVersionsResponse
--
--         , responseListDetectorModels $
--             newListDetectorModelsResponse
--
--         , responseListInputRoutings $
--             newListInputRoutingsResponse
--
--         , responseListInputs $
--             newListInputsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutLoggingOptions $
--             newPutLoggingOptionsResponse
--
--         , responseStartDetectorModelAnalysis $
--             newStartDetectorModelAnalysisResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAlarmModel $
--             newUpdateAlarmModelResponse
--
--         , responseUpdateDetectorModel $
--             newUpdateDetectorModelResponse
--
--         , responseUpdateInput $
--             newUpdateInputResponse
--
--           ]
--     ]

-- Requests

requestCreateAlarmModel :: CreateAlarmModel -> TestTree
requestCreateAlarmModel =
  req
    "CreateAlarmModel"
    "fixture/CreateAlarmModel.yaml"

requestCreateDetectorModel :: CreateDetectorModel -> TestTree
requestCreateDetectorModel =
  req
    "CreateDetectorModel"
    "fixture/CreateDetectorModel.yaml"

requestCreateInput :: CreateInput -> TestTree
requestCreateInput =
  req
    "CreateInput"
    "fixture/CreateInput.yaml"

requestDeleteAlarmModel :: DeleteAlarmModel -> TestTree
requestDeleteAlarmModel =
  req
    "DeleteAlarmModel"
    "fixture/DeleteAlarmModel.yaml"

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

requestDescribeAlarmModel :: DescribeAlarmModel -> TestTree
requestDescribeAlarmModel =
  req
    "DescribeAlarmModel"
    "fixture/DescribeAlarmModel.yaml"

requestDescribeDetectorModel :: DescribeDetectorModel -> TestTree
requestDescribeDetectorModel =
  req
    "DescribeDetectorModel"
    "fixture/DescribeDetectorModel.yaml"

requestDescribeDetectorModelAnalysis :: DescribeDetectorModelAnalysis -> TestTree
requestDescribeDetectorModelAnalysis =
  req
    "DescribeDetectorModelAnalysis"
    "fixture/DescribeDetectorModelAnalysis.yaml"

requestDescribeInput :: DescribeInput -> TestTree
requestDescribeInput =
  req
    "DescribeInput"
    "fixture/DescribeInput.yaml"

requestDescribeLoggingOptions :: DescribeLoggingOptions -> TestTree
requestDescribeLoggingOptions =
  req
    "DescribeLoggingOptions"
    "fixture/DescribeLoggingOptions.yaml"

requestGetDetectorModelAnalysisResults :: GetDetectorModelAnalysisResults -> TestTree
requestGetDetectorModelAnalysisResults =
  req
    "GetDetectorModelAnalysisResults"
    "fixture/GetDetectorModelAnalysisResults.yaml"

requestListAlarmModelVersions :: ListAlarmModelVersions -> TestTree
requestListAlarmModelVersions =
  req
    "ListAlarmModelVersions"
    "fixture/ListAlarmModelVersions.yaml"

requestListAlarmModels :: ListAlarmModels -> TestTree
requestListAlarmModels =
  req
    "ListAlarmModels"
    "fixture/ListAlarmModels.yaml"

requestListDetectorModelVersions :: ListDetectorModelVersions -> TestTree
requestListDetectorModelVersions =
  req
    "ListDetectorModelVersions"
    "fixture/ListDetectorModelVersions.yaml"

requestListDetectorModels :: ListDetectorModels -> TestTree
requestListDetectorModels =
  req
    "ListDetectorModels"
    "fixture/ListDetectorModels.yaml"

requestListInputRoutings :: ListInputRoutings -> TestTree
requestListInputRoutings =
  req
    "ListInputRoutings"
    "fixture/ListInputRoutings.yaml"

requestListInputs :: ListInputs -> TestTree
requestListInputs =
  req
    "ListInputs"
    "fixture/ListInputs.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutLoggingOptions :: PutLoggingOptions -> TestTree
requestPutLoggingOptions =
  req
    "PutLoggingOptions"
    "fixture/PutLoggingOptions.yaml"

requestStartDetectorModelAnalysis :: StartDetectorModelAnalysis -> TestTree
requestStartDetectorModelAnalysis =
  req
    "StartDetectorModelAnalysis"
    "fixture/StartDetectorModelAnalysis.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateAlarmModel :: UpdateAlarmModel -> TestTree
requestUpdateAlarmModel =
  req
    "UpdateAlarmModel"
    "fixture/UpdateAlarmModel.yaml"

requestUpdateDetectorModel :: UpdateDetectorModel -> TestTree
requestUpdateDetectorModel =
  req
    "UpdateDetectorModel"
    "fixture/UpdateDetectorModel.yaml"

requestUpdateInput :: UpdateInput -> TestTree
requestUpdateInput =
  req
    "UpdateInput"
    "fixture/UpdateInput.yaml"

-- Responses

responseCreateAlarmModel :: CreateAlarmModelResponse -> TestTree
responseCreateAlarmModel =
  res
    "CreateAlarmModelResponse"
    "fixture/CreateAlarmModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAlarmModel)

responseCreateDetectorModel :: CreateDetectorModelResponse -> TestTree
responseCreateDetectorModel =
  res
    "CreateDetectorModelResponse"
    "fixture/CreateDetectorModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDetectorModel)

responseCreateInput :: CreateInputResponse -> TestTree
responseCreateInput =
  res
    "CreateInputResponse"
    "fixture/CreateInputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInput)

responseDeleteAlarmModel :: DeleteAlarmModelResponse -> TestTree
responseDeleteAlarmModel =
  res
    "DeleteAlarmModelResponse"
    "fixture/DeleteAlarmModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAlarmModel)

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

responseDescribeAlarmModel :: DescribeAlarmModelResponse -> TestTree
responseDescribeAlarmModel =
  res
    "DescribeAlarmModelResponse"
    "fixture/DescribeAlarmModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAlarmModel)

responseDescribeDetectorModel :: DescribeDetectorModelResponse -> TestTree
responseDescribeDetectorModel =
  res
    "DescribeDetectorModelResponse"
    "fixture/DescribeDetectorModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDetectorModel)

responseDescribeDetectorModelAnalysis :: DescribeDetectorModelAnalysisResponse -> TestTree
responseDescribeDetectorModelAnalysis =
  res
    "DescribeDetectorModelAnalysisResponse"
    "fixture/DescribeDetectorModelAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDetectorModelAnalysis)

responseDescribeInput :: DescribeInputResponse -> TestTree
responseDescribeInput =
  res
    "DescribeInputResponse"
    "fixture/DescribeInputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInput)

responseDescribeLoggingOptions :: DescribeLoggingOptionsResponse -> TestTree
responseDescribeLoggingOptions =
  res
    "DescribeLoggingOptionsResponse"
    "fixture/DescribeLoggingOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLoggingOptions)

responseGetDetectorModelAnalysisResults :: GetDetectorModelAnalysisResultsResponse -> TestTree
responseGetDetectorModelAnalysisResults =
  res
    "GetDetectorModelAnalysisResultsResponse"
    "fixture/GetDetectorModelAnalysisResultsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDetectorModelAnalysisResults)

responseListAlarmModelVersions :: ListAlarmModelVersionsResponse -> TestTree
responseListAlarmModelVersions =
  res
    "ListAlarmModelVersionsResponse"
    "fixture/ListAlarmModelVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAlarmModelVersions)

responseListAlarmModels :: ListAlarmModelsResponse -> TestTree
responseListAlarmModels =
  res
    "ListAlarmModelsResponse"
    "fixture/ListAlarmModelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAlarmModels)

responseListDetectorModelVersions :: ListDetectorModelVersionsResponse -> TestTree
responseListDetectorModelVersions =
  res
    "ListDetectorModelVersionsResponse"
    "fixture/ListDetectorModelVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDetectorModelVersions)

responseListDetectorModels :: ListDetectorModelsResponse -> TestTree
responseListDetectorModels =
  res
    "ListDetectorModelsResponse"
    "fixture/ListDetectorModelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDetectorModels)

responseListInputRoutings :: ListInputRoutingsResponse -> TestTree
responseListInputRoutings =
  res
    "ListInputRoutingsResponse"
    "fixture/ListInputRoutingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInputRoutings)

responseListInputs :: ListInputsResponse -> TestTree
responseListInputs =
  res
    "ListInputsResponse"
    "fixture/ListInputsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInputs)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutLoggingOptions :: PutLoggingOptionsResponse -> TestTree
responsePutLoggingOptions =
  res
    "PutLoggingOptionsResponse"
    "fixture/PutLoggingOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutLoggingOptions)

responseStartDetectorModelAnalysis :: StartDetectorModelAnalysisResponse -> TestTree
responseStartDetectorModelAnalysis =
  res
    "StartDetectorModelAnalysisResponse"
    "fixture/StartDetectorModelAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDetectorModelAnalysis)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateAlarmModel :: UpdateAlarmModelResponse -> TestTree
responseUpdateAlarmModel =
  res
    "UpdateAlarmModelResponse"
    "fixture/UpdateAlarmModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAlarmModel)

responseUpdateDetectorModel :: UpdateDetectorModelResponse -> TestTree
responseUpdateDetectorModel =
  res
    "UpdateDetectorModelResponse"
    "fixture/UpdateDetectorModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDetectorModel)

responseUpdateInput :: UpdateInputResponse -> TestTree
responseUpdateInput =
  res
    "UpdateInputResponse"
    "fixture/UpdateInputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateInput)
