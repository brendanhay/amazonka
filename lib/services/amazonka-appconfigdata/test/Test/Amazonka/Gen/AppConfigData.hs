{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.AppConfigData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.AppConfigData where

import Amazonka.AppConfigData
import qualified Data.Proxy as Proxy
import Test.Amazonka.AppConfigData.Internal
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
--         [ requestGetLatestConfiguration $
--             newGetLatestConfiguration
--
--         , requestStartConfigurationSession $
--             newStartConfigurationSession
--
--           ]

--     , testGroup "response"
--         [ responseGetLatestConfiguration $
--             newGetLatestConfigurationResponse
--
--         , responseStartConfigurationSession $
--             newStartConfigurationSessionResponse
--
--           ]
--     ]

-- Requests

requestGetLatestConfiguration :: GetLatestConfiguration -> TestTree
requestGetLatestConfiguration =
  req
    "GetLatestConfiguration"
    "fixture/GetLatestConfiguration.yaml"

requestStartConfigurationSession :: StartConfigurationSession -> TestTree
requestStartConfigurationSession =
  req
    "StartConfigurationSession"
    "fixture/StartConfigurationSession.yaml"

-- Responses

responseGetLatestConfiguration :: GetLatestConfigurationResponse -> TestTree
responseGetLatestConfiguration =
  res
    "GetLatestConfigurationResponse"
    "fixture/GetLatestConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLatestConfiguration)

responseStartConfigurationSession :: StartConfigurationSessionResponse -> TestTree
responseStartConfigurationSession =
  res
    "StartConfigurationSessionResponse"
    "fixture/StartConfigurationSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartConfigurationSession)
