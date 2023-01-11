{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CostAndUsageReport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CostAndUsageReport where

import Amazonka.CostAndUsageReport
import qualified Data.Proxy as Proxy
import Test.Amazonka.CostAndUsageReport.Internal
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
--         , requestDescribeReportDefinitions $
--             newDescribeReportDefinitions
--
--         , requestModifyReportDefinition $
--             newModifyReportDefinition
--
--         , requestPutReportDefinition $
--             newPutReportDefinition
--
--           ]

--     , testGroup "response"
--         [ responseDeleteReportDefinition $
--             newDeleteReportDefinitionResponse
--
--         , responseDescribeReportDefinitions $
--             newDescribeReportDefinitionsResponse
--
--         , responseModifyReportDefinition $
--             newModifyReportDefinitionResponse
--
--         , responsePutReportDefinition $
--             newPutReportDefinitionResponse
--
--           ]
--     ]

-- Requests

requestDeleteReportDefinition :: DeleteReportDefinition -> TestTree
requestDeleteReportDefinition =
  req
    "DeleteReportDefinition"
    "fixture/DeleteReportDefinition.yaml"

requestDescribeReportDefinitions :: DescribeReportDefinitions -> TestTree
requestDescribeReportDefinitions =
  req
    "DescribeReportDefinitions"
    "fixture/DescribeReportDefinitions.yaml"

requestModifyReportDefinition :: ModifyReportDefinition -> TestTree
requestModifyReportDefinition =
  req
    "ModifyReportDefinition"
    "fixture/ModifyReportDefinition.yaml"

requestPutReportDefinition :: PutReportDefinition -> TestTree
requestPutReportDefinition =
  req
    "PutReportDefinition"
    "fixture/PutReportDefinition.yaml"

-- Responses

responseDeleteReportDefinition :: DeleteReportDefinitionResponse -> TestTree
responseDeleteReportDefinition =
  res
    "DeleteReportDefinitionResponse"
    "fixture/DeleteReportDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReportDefinition)

responseDescribeReportDefinitions :: DescribeReportDefinitionsResponse -> TestTree
responseDescribeReportDefinitions =
  res
    "DescribeReportDefinitionsResponse"
    "fixture/DescribeReportDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReportDefinitions)

responseModifyReportDefinition :: ModifyReportDefinitionResponse -> TestTree
responseModifyReportDefinition =
  res
    "ModifyReportDefinitionResponse"
    "fixture/ModifyReportDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyReportDefinition)

responsePutReportDefinition :: PutReportDefinitionResponse -> TestTree
responsePutReportDefinition =
  res
    "PutReportDefinitionResponse"
    "fixture/PutReportDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutReportDefinition)
