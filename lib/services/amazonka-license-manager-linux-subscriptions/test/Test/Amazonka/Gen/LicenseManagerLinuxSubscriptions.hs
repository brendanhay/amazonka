{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.LicenseManagerLinuxSubscriptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.LicenseManagerLinuxSubscriptions where

import Amazonka.LicenseManagerLinuxSubscriptions
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.LicenseManagerLinuxSubscriptions.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetServiceSettings $
--             newGetServiceSettings
--
--         , requestListLinuxSubscriptionInstances $
--             newListLinuxSubscriptionInstances
--
--         , requestListLinuxSubscriptions $
--             newListLinuxSubscriptions
--
--         , requestUpdateServiceSettings $
--             newUpdateServiceSettings
--
--           ]

--     , testGroup "response"
--         [ responseGetServiceSettings $
--             newGetServiceSettingsResponse
--
--         , responseListLinuxSubscriptionInstances $
--             newListLinuxSubscriptionInstancesResponse
--
--         , responseListLinuxSubscriptions $
--             newListLinuxSubscriptionsResponse
--
--         , responseUpdateServiceSettings $
--             newUpdateServiceSettingsResponse
--
--           ]
--     ]

-- Requests

requestGetServiceSettings :: GetServiceSettings -> TestTree
requestGetServiceSettings =
  req
    "GetServiceSettings"
    "fixture/GetServiceSettings.yaml"

requestListLinuxSubscriptionInstances :: ListLinuxSubscriptionInstances -> TestTree
requestListLinuxSubscriptionInstances =
  req
    "ListLinuxSubscriptionInstances"
    "fixture/ListLinuxSubscriptionInstances.yaml"

requestListLinuxSubscriptions :: ListLinuxSubscriptions -> TestTree
requestListLinuxSubscriptions =
  req
    "ListLinuxSubscriptions"
    "fixture/ListLinuxSubscriptions.yaml"

requestUpdateServiceSettings :: UpdateServiceSettings -> TestTree
requestUpdateServiceSettings =
  req
    "UpdateServiceSettings"
    "fixture/UpdateServiceSettings.yaml"

-- Responses

responseGetServiceSettings :: GetServiceSettingsResponse -> TestTree
responseGetServiceSettings =
  res
    "GetServiceSettingsResponse"
    "fixture/GetServiceSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServiceSettings)

responseListLinuxSubscriptionInstances :: ListLinuxSubscriptionInstancesResponse -> TestTree
responseListLinuxSubscriptionInstances =
  res
    "ListLinuxSubscriptionInstancesResponse"
    "fixture/ListLinuxSubscriptionInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLinuxSubscriptionInstances)

responseListLinuxSubscriptions :: ListLinuxSubscriptionsResponse -> TestTree
responseListLinuxSubscriptions =
  res
    "ListLinuxSubscriptionsResponse"
    "fixture/ListLinuxSubscriptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLinuxSubscriptions)

responseUpdateServiceSettings :: UpdateServiceSettingsResponse -> TestTree
responseUpdateServiceSettings =
  res
    "UpdateServiceSettingsResponse"
    "fixture/UpdateServiceSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServiceSettings)
