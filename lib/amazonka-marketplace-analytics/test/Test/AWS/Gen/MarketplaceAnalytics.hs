{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MarketplaceAnalytics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.MarketplaceAnalytics where

import Data.Proxy
import Network.AWS.MarketplaceAnalytics
import Test.AWS.Fixture
import Test.AWS.MarketplaceAnalytics.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestStartSupportDataExport $
--             mkStartSupportDataExport
--
--         , requestGenerateDataSet $
--             mkGenerateDataSet
--
--           ]

--     , testGroup "response"
--         [ responseStartSupportDataExport $
--             mkStartSupportDataExportResponse
--
--         , responseGenerateDataSet $
--             mkGenerateDataSetResponse
--
--           ]
--     ]

-- Requests

requestStartSupportDataExport :: StartSupportDataExport -> TestTree
requestStartSupportDataExport =
  req
    "StartSupportDataExport"
    "fixture/StartSupportDataExport.yaml"

requestGenerateDataSet :: GenerateDataSet -> TestTree
requestGenerateDataSet =
  req
    "GenerateDataSet"
    "fixture/GenerateDataSet.yaml"

-- Responses

responseStartSupportDataExport :: StartSupportDataExportResponse -> TestTree
responseStartSupportDataExport =
  res
    "StartSupportDataExportResponse"
    "fixture/StartSupportDataExportResponse.proto"
    marketplaceAnalyticsService
    (Proxy :: Proxy StartSupportDataExport)

responseGenerateDataSet :: GenerateDataSetResponse -> TestTree
responseGenerateDataSet =
  res
    "GenerateDataSetResponse"
    "fixture/GenerateDataSetResponse.proto"
    marketplaceAnalyticsService
    (Proxy :: Proxy GenerateDataSet)
