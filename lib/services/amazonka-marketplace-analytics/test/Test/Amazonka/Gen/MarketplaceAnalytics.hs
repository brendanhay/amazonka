{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MarketplaceAnalytics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.MarketplaceAnalytics where

import Amazonka.MarketplaceAnalytics
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.MarketplaceAnalytics.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGenerateDataSet $
--             newGenerateDataSet
--
--         , requestStartSupportDataExport $
--             newStartSupportDataExport
--
--           ]

--     , testGroup "response"
--         [ responseGenerateDataSet $
--             newGenerateDataSetResponse
--
--         , responseStartSupportDataExport $
--             newStartSupportDataExportResponse
--
--           ]
--     ]

-- Requests

requestGenerateDataSet :: GenerateDataSet -> TestTree
requestGenerateDataSet =
  req
    "GenerateDataSet"
    "fixture/GenerateDataSet.yaml"

requestStartSupportDataExport :: StartSupportDataExport -> TestTree
requestStartSupportDataExport =
  req
    "StartSupportDataExport"
    "fixture/StartSupportDataExport.yaml"

-- Responses

responseGenerateDataSet :: GenerateDataSetResponse -> TestTree
responseGenerateDataSet =
  res
    "GenerateDataSetResponse"
    "fixture/GenerateDataSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateDataSet)

responseStartSupportDataExport :: StartSupportDataExportResponse -> TestTree
responseStartSupportDataExport =
  res
    "StartSupportDataExportResponse"
    "fixture/StartSupportDataExportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartSupportDataExport)
