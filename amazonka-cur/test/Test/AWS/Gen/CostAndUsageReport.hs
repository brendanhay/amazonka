{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CostAndUsageReport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.CostAndUsageReport where

import Data.Proxy
import Network.AWS.CostAndUsageReport
import Test.AWS.CostAndUsageReport.Internal
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
--         [ requestModifyReportDefinition $
--             newModifyReportDefinition
--
--         , requestDeleteReportDefinition $
--             newDeleteReportDefinition
--
--         , requestDescribeReportDefinitions $
--             newDescribeReportDefinitions
--
--         , requestPutReportDefinition $
--             newPutReportDefinition
--
--           ]

--     , testGroup "response"
--         [ responseModifyReportDefinition $
--             newModifyReportDefinitionResponse
--
--         , responseDeleteReportDefinition $
--             newDeleteReportDefinitionResponse
--
--         , responseDescribeReportDefinitions $
--             newDescribeReportDefinitionsResponse
--
--         , responsePutReportDefinition $
--             newPutReportDefinitionResponse
--
--           ]
--     ]

-- Requests

requestModifyReportDefinition :: ModifyReportDefinition -> TestTree
requestModifyReportDefinition =
  req
    "ModifyReportDefinition"
    "fixture/ModifyReportDefinition.yaml"

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

requestPutReportDefinition :: PutReportDefinition -> TestTree
requestPutReportDefinition =
  req
    "PutReportDefinition"
    "fixture/PutReportDefinition.yaml"

-- Responses

responseModifyReportDefinition :: ModifyReportDefinitionResponse -> TestTree
responseModifyReportDefinition =
  res
    "ModifyReportDefinitionResponse"
    "fixture/ModifyReportDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyReportDefinition)

responseDeleteReportDefinition :: DeleteReportDefinitionResponse -> TestTree
responseDeleteReportDefinition =
  res
    "DeleteReportDefinitionResponse"
    "fixture/DeleteReportDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteReportDefinition)

responseDescribeReportDefinitions :: DescribeReportDefinitionsResponse -> TestTree
responseDescribeReportDefinitions =
  res
    "DescribeReportDefinitionsResponse"
    "fixture/DescribeReportDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReportDefinitions)

responsePutReportDefinition :: PutReportDefinitionResponse -> TestTree
responsePutReportDefinition =
  res
    "PutReportDefinitionResponse"
    "fixture/PutReportDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy PutReportDefinition)
