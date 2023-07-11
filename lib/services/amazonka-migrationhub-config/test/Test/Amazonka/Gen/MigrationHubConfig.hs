{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MigrationHubConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.MigrationHubConfig where

import Amazonka.MigrationHubConfig
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.MigrationHubConfig.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateHomeRegionControl $
--             newCreateHomeRegionControl
--
--         , requestDescribeHomeRegionControls $
--             newDescribeHomeRegionControls
--
--         , requestGetHomeRegion $
--             newGetHomeRegion
--
--           ]

--     , testGroup "response"
--         [ responseCreateHomeRegionControl $
--             newCreateHomeRegionControlResponse
--
--         , responseDescribeHomeRegionControls $
--             newDescribeHomeRegionControlsResponse
--
--         , responseGetHomeRegion $
--             newGetHomeRegionResponse
--
--           ]
--     ]

-- Requests

requestCreateHomeRegionControl :: CreateHomeRegionControl -> TestTree
requestCreateHomeRegionControl =
  req
    "CreateHomeRegionControl"
    "fixture/CreateHomeRegionControl.yaml"

requestDescribeHomeRegionControls :: DescribeHomeRegionControls -> TestTree
requestDescribeHomeRegionControls =
  req
    "DescribeHomeRegionControls"
    "fixture/DescribeHomeRegionControls.yaml"

requestGetHomeRegion :: GetHomeRegion -> TestTree
requestGetHomeRegion =
  req
    "GetHomeRegion"
    "fixture/GetHomeRegion.yaml"

-- Responses

responseCreateHomeRegionControl :: CreateHomeRegionControlResponse -> TestTree
responseCreateHomeRegionControl =
  res
    "CreateHomeRegionControlResponse"
    "fixture/CreateHomeRegionControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHomeRegionControl)

responseDescribeHomeRegionControls :: DescribeHomeRegionControlsResponse -> TestTree
responseDescribeHomeRegionControls =
  res
    "DescribeHomeRegionControlsResponse"
    "fixture/DescribeHomeRegionControlsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHomeRegionControls)

responseGetHomeRegion :: GetHomeRegionResponse -> TestTree
responseGetHomeRegion =
  res
    "GetHomeRegionResponse"
    "fixture/GetHomeRegionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHomeRegion)
