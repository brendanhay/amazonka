{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.FIS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.FIS where

import qualified Data.Proxy as Proxy
import Network.AWS.FIS
import Test.AWS.FIS.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetExperimentTemplate $
--             newGetExperimentTemplate
--
--         , requestListActions $
--             newListActions
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreateExperimentTemplate $
--             newCreateExperimentTemplate
--
--         , requestListExperiments $
--             newListExperiments
--
--         , requestUpdateExperimentTemplate $
--             newUpdateExperimentTemplate
--
--         , requestGetAction $
--             newGetAction
--
--         , requestDeleteExperimentTemplate $
--             newDeleteExperimentTemplate
--
--         , requestStartExperiment $
--             newStartExperiment
--
--         , requestGetExperiment $
--             newGetExperiment
--
--         , requestListExperimentTemplates $
--             newListExperimentTemplates
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestStopExperiment $
--             newStopExperiment
--
--           ]

--     , testGroup "response"
--         [ responseGetExperimentTemplate $
--             newGetExperimentTemplateResponse
--
--         , responseListActions $
--             newListActionsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreateExperimentTemplate $
--             newCreateExperimentTemplateResponse
--
--         , responseListExperiments $
--             newListExperimentsResponse
--
--         , responseUpdateExperimentTemplate $
--             newUpdateExperimentTemplateResponse
--
--         , responseGetAction $
--             newGetActionResponse
--
--         , responseDeleteExperimentTemplate $
--             newDeleteExperimentTemplateResponse
--
--         , responseStartExperiment $
--             newStartExperimentResponse
--
--         , responseGetExperiment $
--             newGetExperimentResponse
--
--         , responseListExperimentTemplates $
--             newListExperimentTemplatesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseStopExperiment $
--             newStopExperimentResponse
--
--           ]
--     ]

-- Requests

requestGetExperimentTemplate :: GetExperimentTemplate -> TestTree
requestGetExperimentTemplate =
  req
    "GetExperimentTemplate"
    "fixture/GetExperimentTemplate.yaml"

requestListActions :: ListActions -> TestTree
requestListActions =
  req
    "ListActions"
    "fixture/ListActions.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreateExperimentTemplate :: CreateExperimentTemplate -> TestTree
requestCreateExperimentTemplate =
  req
    "CreateExperimentTemplate"
    "fixture/CreateExperimentTemplate.yaml"

requestListExperiments :: ListExperiments -> TestTree
requestListExperiments =
  req
    "ListExperiments"
    "fixture/ListExperiments.yaml"

requestUpdateExperimentTemplate :: UpdateExperimentTemplate -> TestTree
requestUpdateExperimentTemplate =
  req
    "UpdateExperimentTemplate"
    "fixture/UpdateExperimentTemplate.yaml"

requestGetAction :: GetAction -> TestTree
requestGetAction =
  req
    "GetAction"
    "fixture/GetAction.yaml"

requestDeleteExperimentTemplate :: DeleteExperimentTemplate -> TestTree
requestDeleteExperimentTemplate =
  req
    "DeleteExperimentTemplate"
    "fixture/DeleteExperimentTemplate.yaml"

requestStartExperiment :: StartExperiment -> TestTree
requestStartExperiment =
  req
    "StartExperiment"
    "fixture/StartExperiment.yaml"

requestGetExperiment :: GetExperiment -> TestTree
requestGetExperiment =
  req
    "GetExperiment"
    "fixture/GetExperiment.yaml"

requestListExperimentTemplates :: ListExperimentTemplates -> TestTree
requestListExperimentTemplates =
  req
    "ListExperimentTemplates"
    "fixture/ListExperimentTemplates.yaml"

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

requestStopExperiment :: StopExperiment -> TestTree
requestStopExperiment =
  req
    "StopExperiment"
    "fixture/StopExperiment.yaml"

-- Responses

responseGetExperimentTemplate :: GetExperimentTemplateResponse -> TestTree
responseGetExperimentTemplate =
  res
    "GetExperimentTemplateResponse"
    "fixture/GetExperimentTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetExperimentTemplate)

responseListActions :: ListActionsResponse -> TestTree
responseListActions =
  res
    "ListActionsResponse"
    "fixture/ListActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListActions)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseCreateExperimentTemplate :: CreateExperimentTemplateResponse -> TestTree
responseCreateExperimentTemplate =
  res
    "CreateExperimentTemplateResponse"
    "fixture/CreateExperimentTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateExperimentTemplate)

responseListExperiments :: ListExperimentsResponse -> TestTree
responseListExperiments =
  res
    "ListExperimentsResponse"
    "fixture/ListExperimentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExperiments)

responseUpdateExperimentTemplate :: UpdateExperimentTemplateResponse -> TestTree
responseUpdateExperimentTemplate =
  res
    "UpdateExperimentTemplateResponse"
    "fixture/UpdateExperimentTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateExperimentTemplate)

responseGetAction :: GetActionResponse -> TestTree
responseGetAction =
  res
    "GetActionResponse"
    "fixture/GetActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAction)

responseDeleteExperimentTemplate :: DeleteExperimentTemplateResponse -> TestTree
responseDeleteExperimentTemplate =
  res
    "DeleteExperimentTemplateResponse"
    "fixture/DeleteExperimentTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteExperimentTemplate)

responseStartExperiment :: StartExperimentResponse -> TestTree
responseStartExperiment =
  res
    "StartExperimentResponse"
    "fixture/StartExperimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartExperiment)

responseGetExperiment :: GetExperimentResponse -> TestTree
responseGetExperiment =
  res
    "GetExperimentResponse"
    "fixture/GetExperimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetExperiment)

responseListExperimentTemplates :: ListExperimentTemplatesResponse -> TestTree
responseListExperimentTemplates =
  res
    "ListExperimentTemplatesResponse"
    "fixture/ListExperimentTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExperimentTemplates)

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

responseStopExperiment :: StopExperimentResponse -> TestTree
responseStopExperiment =
  res
    "StopExperimentResponse"
    "fixture/StopExperimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopExperiment)
