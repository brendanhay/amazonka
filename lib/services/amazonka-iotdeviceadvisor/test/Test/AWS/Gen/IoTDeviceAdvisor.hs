{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.IoTDeviceAdvisor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.IoTDeviceAdvisor where

import qualified Data.Proxy as Proxy
import Network.AWS.IoTDeviceAdvisor
import Test.AWS.Fixture
import Test.AWS.IoTDeviceAdvisor.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetSuiteRunReport $
--             newGetSuiteRunReport
--
--         , requestStartSuiteRun $
--             newStartSuiteRun
--
--         , requestListSuiteDefinitions $
--             newListSuiteDefinitions
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDeleteSuiteDefinition $
--             newDeleteSuiteDefinition
--
--         , requestUpdateSuiteDefinition $
--             newUpdateSuiteDefinition
--
--         , requestCreateSuiteDefinition $
--             newCreateSuiteDefinition
--
--         , requestStopSuiteRun $
--             newStopSuiteRun
--
--         , requestGetSuiteDefinition $
--             newGetSuiteDefinition
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListSuiteRuns $
--             newListSuiteRuns
--
--         , requestGetSuiteRun $
--             newGetSuiteRun
--
--           ]

--     , testGroup "response"
--         [ responseGetSuiteRunReport $
--             newGetSuiteRunReportResponse
--
--         , responseStartSuiteRun $
--             newStartSuiteRunResponse
--
--         , responseListSuiteDefinitions $
--             newListSuiteDefinitionsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDeleteSuiteDefinition $
--             newDeleteSuiteDefinitionResponse
--
--         , responseUpdateSuiteDefinition $
--             newUpdateSuiteDefinitionResponse
--
--         , responseCreateSuiteDefinition $
--             newCreateSuiteDefinitionResponse
--
--         , responseStopSuiteRun $
--             newStopSuiteRunResponse
--
--         , responseGetSuiteDefinition $
--             newGetSuiteDefinitionResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListSuiteRuns $
--             newListSuiteRunsResponse
--
--         , responseGetSuiteRun $
--             newGetSuiteRunResponse
--
--           ]
--     ]

-- Requests

requestGetSuiteRunReport :: GetSuiteRunReport -> TestTree
requestGetSuiteRunReport =
  req
    "GetSuiteRunReport"
    "fixture/GetSuiteRunReport.yaml"

requestStartSuiteRun :: StartSuiteRun -> TestTree
requestStartSuiteRun =
  req
    "StartSuiteRun"
    "fixture/StartSuiteRun.yaml"

requestListSuiteDefinitions :: ListSuiteDefinitions -> TestTree
requestListSuiteDefinitions =
  req
    "ListSuiteDefinitions"
    "fixture/ListSuiteDefinitions.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDeleteSuiteDefinition :: DeleteSuiteDefinition -> TestTree
requestDeleteSuiteDefinition =
  req
    "DeleteSuiteDefinition"
    "fixture/DeleteSuiteDefinition.yaml"

requestUpdateSuiteDefinition :: UpdateSuiteDefinition -> TestTree
requestUpdateSuiteDefinition =
  req
    "UpdateSuiteDefinition"
    "fixture/UpdateSuiteDefinition.yaml"

requestCreateSuiteDefinition :: CreateSuiteDefinition -> TestTree
requestCreateSuiteDefinition =
  req
    "CreateSuiteDefinition"
    "fixture/CreateSuiteDefinition.yaml"

requestStopSuiteRun :: StopSuiteRun -> TestTree
requestStopSuiteRun =
  req
    "StopSuiteRun"
    "fixture/StopSuiteRun.yaml"

requestGetSuiteDefinition :: GetSuiteDefinition -> TestTree
requestGetSuiteDefinition =
  req
    "GetSuiteDefinition"
    "fixture/GetSuiteDefinition.yaml"

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

requestListSuiteRuns :: ListSuiteRuns -> TestTree
requestListSuiteRuns =
  req
    "ListSuiteRuns"
    "fixture/ListSuiteRuns.yaml"

requestGetSuiteRun :: GetSuiteRun -> TestTree
requestGetSuiteRun =
  req
    "GetSuiteRun"
    "fixture/GetSuiteRun.yaml"

-- Responses

responseGetSuiteRunReport :: GetSuiteRunReportResponse -> TestTree
responseGetSuiteRunReport =
  res
    "GetSuiteRunReportResponse"
    "fixture/GetSuiteRunReportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSuiteRunReport)

responseStartSuiteRun :: StartSuiteRunResponse -> TestTree
responseStartSuiteRun =
  res
    "StartSuiteRunResponse"
    "fixture/StartSuiteRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartSuiteRun)

responseListSuiteDefinitions :: ListSuiteDefinitionsResponse -> TestTree
responseListSuiteDefinitions =
  res
    "ListSuiteDefinitionsResponse"
    "fixture/ListSuiteDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSuiteDefinitions)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseDeleteSuiteDefinition :: DeleteSuiteDefinitionResponse -> TestTree
responseDeleteSuiteDefinition =
  res
    "DeleteSuiteDefinitionResponse"
    "fixture/DeleteSuiteDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSuiteDefinition)

responseUpdateSuiteDefinition :: UpdateSuiteDefinitionResponse -> TestTree
responseUpdateSuiteDefinition =
  res
    "UpdateSuiteDefinitionResponse"
    "fixture/UpdateSuiteDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSuiteDefinition)

responseCreateSuiteDefinition :: CreateSuiteDefinitionResponse -> TestTree
responseCreateSuiteDefinition =
  res
    "CreateSuiteDefinitionResponse"
    "fixture/CreateSuiteDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSuiteDefinition)

responseStopSuiteRun :: StopSuiteRunResponse -> TestTree
responseStopSuiteRun =
  res
    "StopSuiteRunResponse"
    "fixture/StopSuiteRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopSuiteRun)

responseGetSuiteDefinition :: GetSuiteDefinitionResponse -> TestTree
responseGetSuiteDefinition =
  res
    "GetSuiteDefinitionResponse"
    "fixture/GetSuiteDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSuiteDefinition)

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

responseListSuiteRuns :: ListSuiteRunsResponse -> TestTree
responseListSuiteRuns =
  res
    "ListSuiteRunsResponse"
    "fixture/ListSuiteRunsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSuiteRuns)

responseGetSuiteRun :: GetSuiteRunResponse -> TestTree
responseGetSuiteRun =
  res
    "GetSuiteRunResponse"
    "fixture/GetSuiteRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSuiteRun)
