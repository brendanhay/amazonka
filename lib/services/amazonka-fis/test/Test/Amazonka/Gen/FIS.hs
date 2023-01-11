{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.FIS
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.FIS where

import Amazonka.FIS
import qualified Data.Proxy as Proxy
import Test.Amazonka.FIS.Internal
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
--         [ requestCreateExperimentTemplate $
--             newCreateExperimentTemplate
--
--         , requestDeleteExperimentTemplate $
--             newDeleteExperimentTemplate
--
--         , requestGetAction $
--             newGetAction
--
--         , requestGetExperiment $
--             newGetExperiment
--
--         , requestGetExperimentTemplate $
--             newGetExperimentTemplate
--
--         , requestGetTargetResourceType $
--             newGetTargetResourceType
--
--         , requestListActions $
--             newListActions
--
--         , requestListExperimentTemplates $
--             newListExperimentTemplates
--
--         , requestListExperiments $
--             newListExperiments
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTargetResourceTypes $
--             newListTargetResourceTypes
--
--         , requestStartExperiment $
--             newStartExperiment
--
--         , requestStopExperiment $
--             newStopExperiment
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateExperimentTemplate $
--             newUpdateExperimentTemplate
--
--           ]

--     , testGroup "response"
--         [ responseCreateExperimentTemplate $
--             newCreateExperimentTemplateResponse
--
--         , responseDeleteExperimentTemplate $
--             newDeleteExperimentTemplateResponse
--
--         , responseGetAction $
--             newGetActionResponse
--
--         , responseGetExperiment $
--             newGetExperimentResponse
--
--         , responseGetExperimentTemplate $
--             newGetExperimentTemplateResponse
--
--         , responseGetTargetResourceType $
--             newGetTargetResourceTypeResponse
--
--         , responseListActions $
--             newListActionsResponse
--
--         , responseListExperimentTemplates $
--             newListExperimentTemplatesResponse
--
--         , responseListExperiments $
--             newListExperimentsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTargetResourceTypes $
--             newListTargetResourceTypesResponse
--
--         , responseStartExperiment $
--             newStartExperimentResponse
--
--         , responseStopExperiment $
--             newStopExperimentResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateExperimentTemplate $
--             newUpdateExperimentTemplateResponse
--
--           ]
--     ]

-- Requests

requestCreateExperimentTemplate :: CreateExperimentTemplate -> TestTree
requestCreateExperimentTemplate =
  req
    "CreateExperimentTemplate"
    "fixture/CreateExperimentTemplate.yaml"

requestDeleteExperimentTemplate :: DeleteExperimentTemplate -> TestTree
requestDeleteExperimentTemplate =
  req
    "DeleteExperimentTemplate"
    "fixture/DeleteExperimentTemplate.yaml"

requestGetAction :: GetAction -> TestTree
requestGetAction =
  req
    "GetAction"
    "fixture/GetAction.yaml"

requestGetExperiment :: GetExperiment -> TestTree
requestGetExperiment =
  req
    "GetExperiment"
    "fixture/GetExperiment.yaml"

requestGetExperimentTemplate :: GetExperimentTemplate -> TestTree
requestGetExperimentTemplate =
  req
    "GetExperimentTemplate"
    "fixture/GetExperimentTemplate.yaml"

requestGetTargetResourceType :: GetTargetResourceType -> TestTree
requestGetTargetResourceType =
  req
    "GetTargetResourceType"
    "fixture/GetTargetResourceType.yaml"

requestListActions :: ListActions -> TestTree
requestListActions =
  req
    "ListActions"
    "fixture/ListActions.yaml"

requestListExperimentTemplates :: ListExperimentTemplates -> TestTree
requestListExperimentTemplates =
  req
    "ListExperimentTemplates"
    "fixture/ListExperimentTemplates.yaml"

requestListExperiments :: ListExperiments -> TestTree
requestListExperiments =
  req
    "ListExperiments"
    "fixture/ListExperiments.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTargetResourceTypes :: ListTargetResourceTypes -> TestTree
requestListTargetResourceTypes =
  req
    "ListTargetResourceTypes"
    "fixture/ListTargetResourceTypes.yaml"

requestStartExperiment :: StartExperiment -> TestTree
requestStartExperiment =
  req
    "StartExperiment"
    "fixture/StartExperiment.yaml"

requestStopExperiment :: StopExperiment -> TestTree
requestStopExperiment =
  req
    "StopExperiment"
    "fixture/StopExperiment.yaml"

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

requestUpdateExperimentTemplate :: UpdateExperimentTemplate -> TestTree
requestUpdateExperimentTemplate =
  req
    "UpdateExperimentTemplate"
    "fixture/UpdateExperimentTemplate.yaml"

-- Responses

responseCreateExperimentTemplate :: CreateExperimentTemplateResponse -> TestTree
responseCreateExperimentTemplate =
  res
    "CreateExperimentTemplateResponse"
    "fixture/CreateExperimentTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateExperimentTemplate)

responseDeleteExperimentTemplate :: DeleteExperimentTemplateResponse -> TestTree
responseDeleteExperimentTemplate =
  res
    "DeleteExperimentTemplateResponse"
    "fixture/DeleteExperimentTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteExperimentTemplate)

responseGetAction :: GetActionResponse -> TestTree
responseGetAction =
  res
    "GetActionResponse"
    "fixture/GetActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAction)

responseGetExperiment :: GetExperimentResponse -> TestTree
responseGetExperiment =
  res
    "GetExperimentResponse"
    "fixture/GetExperimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetExperiment)

responseGetExperimentTemplate :: GetExperimentTemplateResponse -> TestTree
responseGetExperimentTemplate =
  res
    "GetExperimentTemplateResponse"
    "fixture/GetExperimentTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetExperimentTemplate)

responseGetTargetResourceType :: GetTargetResourceTypeResponse -> TestTree
responseGetTargetResourceType =
  res
    "GetTargetResourceTypeResponse"
    "fixture/GetTargetResourceTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTargetResourceType)

responseListActions :: ListActionsResponse -> TestTree
responseListActions =
  res
    "ListActionsResponse"
    "fixture/ListActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListActions)

responseListExperimentTemplates :: ListExperimentTemplatesResponse -> TestTree
responseListExperimentTemplates =
  res
    "ListExperimentTemplatesResponse"
    "fixture/ListExperimentTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExperimentTemplates)

responseListExperiments :: ListExperimentsResponse -> TestTree
responseListExperiments =
  res
    "ListExperimentsResponse"
    "fixture/ListExperimentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExperiments)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTargetResourceTypes :: ListTargetResourceTypesResponse -> TestTree
responseListTargetResourceTypes =
  res
    "ListTargetResourceTypesResponse"
    "fixture/ListTargetResourceTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTargetResourceTypes)

responseStartExperiment :: StartExperimentResponse -> TestTree
responseStartExperiment =
  res
    "StartExperimentResponse"
    "fixture/StartExperimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartExperiment)

responseStopExperiment :: StopExperimentResponse -> TestTree
responseStopExperiment =
  res
    "StopExperimentResponse"
    "fixture/StopExperimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopExperiment)

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

responseUpdateExperimentTemplate :: UpdateExperimentTemplateResponse -> TestTree
responseUpdateExperimentTemplate =
  res
    "UpdateExperimentTemplateResponse"
    "fixture/UpdateExperimentTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateExperimentTemplate)
