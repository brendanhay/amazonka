{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MigrationHubConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--         [ requestGetHomeRegion $
--             newGetHomeRegion
--
--         , requestCreateHomeRegionControl $
--             newCreateHomeRegionControl
--
--         , requestDescribeHomeRegionControls $
--             newDescribeHomeRegionControls
--
--           ]

--     , testGroup "response"
--         [ responseGetHomeRegion $
--             newGetHomeRegionResponse
--
--         , responseCreateHomeRegionControl $
--             newCreateHomeRegionControlResponse
--
--         , responseDescribeHomeRegionControls $
--             newDescribeHomeRegionControlsResponse
--
--           ]
--     ]

-- Requests

requestGetHomeRegion :: GetHomeRegion -> TestTree
requestGetHomeRegion =
  req
    "GetHomeRegion"
    "fixture/GetHomeRegion.yaml"

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

-- Responses

responseGetHomeRegion :: GetHomeRegionResponse -> TestTree
responseGetHomeRegion =
  res
    "GetHomeRegionResponse"
    "fixture/GetHomeRegionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHomeRegion)

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
