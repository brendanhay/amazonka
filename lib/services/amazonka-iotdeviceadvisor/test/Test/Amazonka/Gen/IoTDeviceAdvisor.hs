{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.IoTDeviceAdvisor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.IoTDeviceAdvisor where

import Amazonka.IoTDeviceAdvisor
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.IoTDeviceAdvisor.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateSuiteDefinition $
--             newCreateSuiteDefinition
--
--         , requestDeleteSuiteDefinition $
--             newDeleteSuiteDefinition
--
--         , requestGetEndpoint $
--             newGetEndpoint
--
--         , requestGetSuiteDefinition $
--             newGetSuiteDefinition
--
--         , requestGetSuiteRun $
--             newGetSuiteRun
--
--         , requestGetSuiteRunReport $
--             newGetSuiteRunReport
--
--         , requestListSuiteDefinitions $
--             newListSuiteDefinitions
--
--         , requestListSuiteRuns $
--             newListSuiteRuns
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStartSuiteRun $
--             newStartSuiteRun
--
--         , requestStopSuiteRun $
--             newStopSuiteRun
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateSuiteDefinition $
--             newUpdateSuiteDefinition
--
--           ]

--     , testGroup "response"
--         [ responseCreateSuiteDefinition $
--             newCreateSuiteDefinitionResponse
--
--         , responseDeleteSuiteDefinition $
--             newDeleteSuiteDefinitionResponse
--
--         , responseGetEndpoint $
--             newGetEndpointResponse
--
--         , responseGetSuiteDefinition $
--             newGetSuiteDefinitionResponse
--
--         , responseGetSuiteRun $
--             newGetSuiteRunResponse
--
--         , responseGetSuiteRunReport $
--             newGetSuiteRunReportResponse
--
--         , responseListSuiteDefinitions $
--             newListSuiteDefinitionsResponse
--
--         , responseListSuiteRuns $
--             newListSuiteRunsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStartSuiteRun $
--             newStartSuiteRunResponse
--
--         , responseStopSuiteRun $
--             newStopSuiteRunResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateSuiteDefinition $
--             newUpdateSuiteDefinitionResponse
--
--           ]
--     ]

-- Requests

requestCreateSuiteDefinition :: CreateSuiteDefinition -> TestTree
requestCreateSuiteDefinition =
  req
    "CreateSuiteDefinition"
    "fixture/CreateSuiteDefinition.yaml"

requestDeleteSuiteDefinition :: DeleteSuiteDefinition -> TestTree
requestDeleteSuiteDefinition =
  req
    "DeleteSuiteDefinition"
    "fixture/DeleteSuiteDefinition.yaml"

requestGetEndpoint :: GetEndpoint -> TestTree
requestGetEndpoint =
  req
    "GetEndpoint"
    "fixture/GetEndpoint.yaml"

requestGetSuiteDefinition :: GetSuiteDefinition -> TestTree
requestGetSuiteDefinition =
  req
    "GetSuiteDefinition"
    "fixture/GetSuiteDefinition.yaml"

requestGetSuiteRun :: GetSuiteRun -> TestTree
requestGetSuiteRun =
  req
    "GetSuiteRun"
    "fixture/GetSuiteRun.yaml"

requestGetSuiteRunReport :: GetSuiteRunReport -> TestTree
requestGetSuiteRunReport =
  req
    "GetSuiteRunReport"
    "fixture/GetSuiteRunReport.yaml"

requestListSuiteDefinitions :: ListSuiteDefinitions -> TestTree
requestListSuiteDefinitions =
  req
    "ListSuiteDefinitions"
    "fixture/ListSuiteDefinitions.yaml"

requestListSuiteRuns :: ListSuiteRuns -> TestTree
requestListSuiteRuns =
  req
    "ListSuiteRuns"
    "fixture/ListSuiteRuns.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStartSuiteRun :: StartSuiteRun -> TestTree
requestStartSuiteRun =
  req
    "StartSuiteRun"
    "fixture/StartSuiteRun.yaml"

requestStopSuiteRun :: StopSuiteRun -> TestTree
requestStopSuiteRun =
  req
    "StopSuiteRun"
    "fixture/StopSuiteRun.yaml"

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

requestUpdateSuiteDefinition :: UpdateSuiteDefinition -> TestTree
requestUpdateSuiteDefinition =
  req
    "UpdateSuiteDefinition"
    "fixture/UpdateSuiteDefinition.yaml"

-- Responses

responseCreateSuiteDefinition :: CreateSuiteDefinitionResponse -> TestTree
responseCreateSuiteDefinition =
  res
    "CreateSuiteDefinitionResponse"
    "fixture/CreateSuiteDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSuiteDefinition)

responseDeleteSuiteDefinition :: DeleteSuiteDefinitionResponse -> TestTree
responseDeleteSuiteDefinition =
  res
    "DeleteSuiteDefinitionResponse"
    "fixture/DeleteSuiteDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSuiteDefinition)

responseGetEndpoint :: GetEndpointResponse -> TestTree
responseGetEndpoint =
  res
    "GetEndpointResponse"
    "fixture/GetEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEndpoint)

responseGetSuiteDefinition :: GetSuiteDefinitionResponse -> TestTree
responseGetSuiteDefinition =
  res
    "GetSuiteDefinitionResponse"
    "fixture/GetSuiteDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSuiteDefinition)

responseGetSuiteRun :: GetSuiteRunResponse -> TestTree
responseGetSuiteRun =
  res
    "GetSuiteRunResponse"
    "fixture/GetSuiteRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSuiteRun)

responseGetSuiteRunReport :: GetSuiteRunReportResponse -> TestTree
responseGetSuiteRunReport =
  res
    "GetSuiteRunReportResponse"
    "fixture/GetSuiteRunReportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSuiteRunReport)

responseListSuiteDefinitions :: ListSuiteDefinitionsResponse -> TestTree
responseListSuiteDefinitions =
  res
    "ListSuiteDefinitionsResponse"
    "fixture/ListSuiteDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSuiteDefinitions)

responseListSuiteRuns :: ListSuiteRunsResponse -> TestTree
responseListSuiteRuns =
  res
    "ListSuiteRunsResponse"
    "fixture/ListSuiteRunsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSuiteRuns)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseStartSuiteRun :: StartSuiteRunResponse -> TestTree
responseStartSuiteRun =
  res
    "StartSuiteRunResponse"
    "fixture/StartSuiteRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartSuiteRun)

responseStopSuiteRun :: StopSuiteRunResponse -> TestTree
responseStopSuiteRun =
  res
    "StopSuiteRunResponse"
    "fixture/StopSuiteRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopSuiteRun)

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

responseUpdateSuiteDefinition :: UpdateSuiteDefinitionResponse -> TestTree
responseUpdateSuiteDefinition =
  res
    "UpdateSuiteDefinitionResponse"
    "fixture/UpdateSuiteDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSuiteDefinition)
