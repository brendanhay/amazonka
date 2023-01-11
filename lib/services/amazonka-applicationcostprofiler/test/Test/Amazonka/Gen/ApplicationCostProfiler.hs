{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ApplicationCostProfiler
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ApplicationCostProfiler where

import Amazonka.ApplicationCostProfiler
import qualified Data.Proxy as Proxy
import Test.Amazonka.ApplicationCostProfiler.Internal
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
--         [ requestDeleteReportDefinition $
--             newDeleteReportDefinition
--
--         , requestGetReportDefinition $
--             newGetReportDefinition
--
--         , requestImportApplicationUsage $
--             newImportApplicationUsage
--
--         , requestListReportDefinitions $
--             newListReportDefinitions
--
--         , requestPutReportDefinition $
--             newPutReportDefinition
--
--         , requestUpdateReportDefinition $
--             newUpdateReportDefinition
--
--           ]

--     , testGroup "response"
--         [ responseDeleteReportDefinition $
--             newDeleteReportDefinitionResponse
--
--         , responseGetReportDefinition $
--             newGetReportDefinitionResponse
--
--         , responseImportApplicationUsage $
--             newImportApplicationUsageResponse
--
--         , responseListReportDefinitions $
--             newListReportDefinitionsResponse
--
--         , responsePutReportDefinition $
--             newPutReportDefinitionResponse
--
--         , responseUpdateReportDefinition $
--             newUpdateReportDefinitionResponse
--
--           ]
--     ]

-- Requests

requestDeleteReportDefinition :: DeleteReportDefinition -> TestTree
requestDeleteReportDefinition =
  req
    "DeleteReportDefinition"
    "fixture/DeleteReportDefinition.yaml"

requestGetReportDefinition :: GetReportDefinition -> TestTree
requestGetReportDefinition =
  req
    "GetReportDefinition"
    "fixture/GetReportDefinition.yaml"

requestImportApplicationUsage :: ImportApplicationUsage -> TestTree
requestImportApplicationUsage =
  req
    "ImportApplicationUsage"
    "fixture/ImportApplicationUsage.yaml"

requestListReportDefinitions :: ListReportDefinitions -> TestTree
requestListReportDefinitions =
  req
    "ListReportDefinitions"
    "fixture/ListReportDefinitions.yaml"

requestPutReportDefinition :: PutReportDefinition -> TestTree
requestPutReportDefinition =
  req
    "PutReportDefinition"
    "fixture/PutReportDefinition.yaml"

requestUpdateReportDefinition :: UpdateReportDefinition -> TestTree
requestUpdateReportDefinition =
  req
    "UpdateReportDefinition"
    "fixture/UpdateReportDefinition.yaml"

-- Responses

responseDeleteReportDefinition :: DeleteReportDefinitionResponse -> TestTree
responseDeleteReportDefinition =
  res
    "DeleteReportDefinitionResponse"
    "fixture/DeleteReportDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReportDefinition)

responseGetReportDefinition :: GetReportDefinitionResponse -> TestTree
responseGetReportDefinition =
  res
    "GetReportDefinitionResponse"
    "fixture/GetReportDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReportDefinition)

responseImportApplicationUsage :: ImportApplicationUsageResponse -> TestTree
responseImportApplicationUsage =
  res
    "ImportApplicationUsageResponse"
    "fixture/ImportApplicationUsageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportApplicationUsage)

responseListReportDefinitions :: ListReportDefinitionsResponse -> TestTree
responseListReportDefinitions =
  res
    "ListReportDefinitionsResponse"
    "fixture/ListReportDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReportDefinitions)

responsePutReportDefinition :: PutReportDefinitionResponse -> TestTree
responsePutReportDefinition =
  res
    "PutReportDefinitionResponse"
    "fixture/PutReportDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutReportDefinition)

responseUpdateReportDefinition :: UpdateReportDefinitionResponse -> TestTree
responseUpdateReportDefinition =
  res
    "UpdateReportDefinitionResponse"
    "fixture/UpdateReportDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateReportDefinition)
