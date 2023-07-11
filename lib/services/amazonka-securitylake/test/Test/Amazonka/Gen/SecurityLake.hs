{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SecurityLake
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.SecurityLake where

import Amazonka.SecurityLake
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.SecurityLake.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateAwsLogSource $
--             newCreateAwsLogSource
--
--         , requestCreateCustomLogSource $
--             newCreateCustomLogSource
--
--         , requestCreateDatalake $
--             newCreateDatalake
--
--         , requestCreateDatalakeAutoEnable $
--             newCreateDatalakeAutoEnable
--
--         , requestCreateDatalakeDelegatedAdmin $
--             newCreateDatalakeDelegatedAdmin
--
--         , requestCreateDatalakeExceptionsSubscription $
--             newCreateDatalakeExceptionsSubscription
--
--         , requestCreateSubscriber $
--             newCreateSubscriber
--
--         , requestCreateSubscriptionNotificationConfiguration $
--             newCreateSubscriptionNotificationConfiguration
--
--         , requestDeleteAwsLogSource $
--             newDeleteAwsLogSource
--
--         , requestDeleteCustomLogSource $
--             newDeleteCustomLogSource
--
--         , requestDeleteDatalake $
--             newDeleteDatalake
--
--         , requestDeleteDatalakeAutoEnable $
--             newDeleteDatalakeAutoEnable
--
--         , requestDeleteDatalakeDelegatedAdmin $
--             newDeleteDatalakeDelegatedAdmin
--
--         , requestDeleteDatalakeExceptionsSubscription $
--             newDeleteDatalakeExceptionsSubscription
--
--         , requestDeleteSubscriber $
--             newDeleteSubscriber
--
--         , requestDeleteSubscriptionNotificationConfiguration $
--             newDeleteSubscriptionNotificationConfiguration
--
--         , requestGetDatalake $
--             newGetDatalake
--
--         , requestGetDatalakeAutoEnable $
--             newGetDatalakeAutoEnable
--
--         , requestGetDatalakeExceptionsExpiry $
--             newGetDatalakeExceptionsExpiry
--
--         , requestGetDatalakeExceptionsSubscription $
--             newGetDatalakeExceptionsSubscription
--
--         , requestGetDatalakeStatus $
--             newGetDatalakeStatus
--
--         , requestGetSubscriber $
--             newGetSubscriber
--
--         , requestListDatalakeExceptions $
--             newListDatalakeExceptions
--
--         , requestListLogSources $
--             newListLogSources
--
--         , requestListSubscribers $
--             newListSubscribers
--
--         , requestUpdateDatalake $
--             newUpdateDatalake
--
--         , requestUpdateDatalakeExceptionsExpiry $
--             newUpdateDatalakeExceptionsExpiry
--
--         , requestUpdateDatalakeExceptionsSubscription $
--             newUpdateDatalakeExceptionsSubscription
--
--         , requestUpdateSubscriber $
--             newUpdateSubscriber
--
--         , requestUpdateSubscriptionNotificationConfiguration $
--             newUpdateSubscriptionNotificationConfiguration
--
--           ]

--     , testGroup "response"
--         [ responseCreateAwsLogSource $
--             newCreateAwsLogSourceResponse
--
--         , responseCreateCustomLogSource $
--             newCreateCustomLogSourceResponse
--
--         , responseCreateDatalake $
--             newCreateDatalakeResponse
--
--         , responseCreateDatalakeAutoEnable $
--             newCreateDatalakeAutoEnableResponse
--
--         , responseCreateDatalakeDelegatedAdmin $
--             newCreateDatalakeDelegatedAdminResponse
--
--         , responseCreateDatalakeExceptionsSubscription $
--             newCreateDatalakeExceptionsSubscriptionResponse
--
--         , responseCreateSubscriber $
--             newCreateSubscriberResponse
--
--         , responseCreateSubscriptionNotificationConfiguration $
--             newCreateSubscriptionNotificationConfigurationResponse
--
--         , responseDeleteAwsLogSource $
--             newDeleteAwsLogSourceResponse
--
--         , responseDeleteCustomLogSource $
--             newDeleteCustomLogSourceResponse
--
--         , responseDeleteDatalake $
--             newDeleteDatalakeResponse
--
--         , responseDeleteDatalakeAutoEnable $
--             newDeleteDatalakeAutoEnableResponse
--
--         , responseDeleteDatalakeDelegatedAdmin $
--             newDeleteDatalakeDelegatedAdminResponse
--
--         , responseDeleteDatalakeExceptionsSubscription $
--             newDeleteDatalakeExceptionsSubscriptionResponse
--
--         , responseDeleteSubscriber $
--             newDeleteSubscriberResponse
--
--         , responseDeleteSubscriptionNotificationConfiguration $
--             newDeleteSubscriptionNotificationConfigurationResponse
--
--         , responseGetDatalake $
--             newGetDatalakeResponse
--
--         , responseGetDatalakeAutoEnable $
--             newGetDatalakeAutoEnableResponse
--
--         , responseGetDatalakeExceptionsExpiry $
--             newGetDatalakeExceptionsExpiryResponse
--
--         , responseGetDatalakeExceptionsSubscription $
--             newGetDatalakeExceptionsSubscriptionResponse
--
--         , responseGetDatalakeStatus $
--             newGetDatalakeStatusResponse
--
--         , responseGetSubscriber $
--             newGetSubscriberResponse
--
--         , responseListDatalakeExceptions $
--             newListDatalakeExceptionsResponse
--
--         , responseListLogSources $
--             newListLogSourcesResponse
--
--         , responseListSubscribers $
--             newListSubscribersResponse
--
--         , responseUpdateDatalake $
--             newUpdateDatalakeResponse
--
--         , responseUpdateDatalakeExceptionsExpiry $
--             newUpdateDatalakeExceptionsExpiryResponse
--
--         , responseUpdateDatalakeExceptionsSubscription $
--             newUpdateDatalakeExceptionsSubscriptionResponse
--
--         , responseUpdateSubscriber $
--             newUpdateSubscriberResponse
--
--         , responseUpdateSubscriptionNotificationConfiguration $
--             newUpdateSubscriptionNotificationConfigurationResponse
--
--           ]
--     ]

-- Requests

requestCreateAwsLogSource :: CreateAwsLogSource -> TestTree
requestCreateAwsLogSource =
  req
    "CreateAwsLogSource"
    "fixture/CreateAwsLogSource.yaml"

requestCreateCustomLogSource :: CreateCustomLogSource -> TestTree
requestCreateCustomLogSource =
  req
    "CreateCustomLogSource"
    "fixture/CreateCustomLogSource.yaml"

requestCreateDatalake :: CreateDatalake -> TestTree
requestCreateDatalake =
  req
    "CreateDatalake"
    "fixture/CreateDatalake.yaml"

requestCreateDatalakeAutoEnable :: CreateDatalakeAutoEnable -> TestTree
requestCreateDatalakeAutoEnable =
  req
    "CreateDatalakeAutoEnable"
    "fixture/CreateDatalakeAutoEnable.yaml"

requestCreateDatalakeDelegatedAdmin :: CreateDatalakeDelegatedAdmin -> TestTree
requestCreateDatalakeDelegatedAdmin =
  req
    "CreateDatalakeDelegatedAdmin"
    "fixture/CreateDatalakeDelegatedAdmin.yaml"

requestCreateDatalakeExceptionsSubscription :: CreateDatalakeExceptionsSubscription -> TestTree
requestCreateDatalakeExceptionsSubscription =
  req
    "CreateDatalakeExceptionsSubscription"
    "fixture/CreateDatalakeExceptionsSubscription.yaml"

requestCreateSubscriber :: CreateSubscriber -> TestTree
requestCreateSubscriber =
  req
    "CreateSubscriber"
    "fixture/CreateSubscriber.yaml"

requestCreateSubscriptionNotificationConfiguration :: CreateSubscriptionNotificationConfiguration -> TestTree
requestCreateSubscriptionNotificationConfiguration =
  req
    "CreateSubscriptionNotificationConfiguration"
    "fixture/CreateSubscriptionNotificationConfiguration.yaml"

requestDeleteAwsLogSource :: DeleteAwsLogSource -> TestTree
requestDeleteAwsLogSource =
  req
    "DeleteAwsLogSource"
    "fixture/DeleteAwsLogSource.yaml"

requestDeleteCustomLogSource :: DeleteCustomLogSource -> TestTree
requestDeleteCustomLogSource =
  req
    "DeleteCustomLogSource"
    "fixture/DeleteCustomLogSource.yaml"

requestDeleteDatalake :: DeleteDatalake -> TestTree
requestDeleteDatalake =
  req
    "DeleteDatalake"
    "fixture/DeleteDatalake.yaml"

requestDeleteDatalakeAutoEnable :: DeleteDatalakeAutoEnable -> TestTree
requestDeleteDatalakeAutoEnable =
  req
    "DeleteDatalakeAutoEnable"
    "fixture/DeleteDatalakeAutoEnable.yaml"

requestDeleteDatalakeDelegatedAdmin :: DeleteDatalakeDelegatedAdmin -> TestTree
requestDeleteDatalakeDelegatedAdmin =
  req
    "DeleteDatalakeDelegatedAdmin"
    "fixture/DeleteDatalakeDelegatedAdmin.yaml"

requestDeleteDatalakeExceptionsSubscription :: DeleteDatalakeExceptionsSubscription -> TestTree
requestDeleteDatalakeExceptionsSubscription =
  req
    "DeleteDatalakeExceptionsSubscription"
    "fixture/DeleteDatalakeExceptionsSubscription.yaml"

requestDeleteSubscriber :: DeleteSubscriber -> TestTree
requestDeleteSubscriber =
  req
    "DeleteSubscriber"
    "fixture/DeleteSubscriber.yaml"

requestDeleteSubscriptionNotificationConfiguration :: DeleteSubscriptionNotificationConfiguration -> TestTree
requestDeleteSubscriptionNotificationConfiguration =
  req
    "DeleteSubscriptionNotificationConfiguration"
    "fixture/DeleteSubscriptionNotificationConfiguration.yaml"

requestGetDatalake :: GetDatalake -> TestTree
requestGetDatalake =
  req
    "GetDatalake"
    "fixture/GetDatalake.yaml"

requestGetDatalakeAutoEnable :: GetDatalakeAutoEnable -> TestTree
requestGetDatalakeAutoEnable =
  req
    "GetDatalakeAutoEnable"
    "fixture/GetDatalakeAutoEnable.yaml"

requestGetDatalakeExceptionsExpiry :: GetDatalakeExceptionsExpiry -> TestTree
requestGetDatalakeExceptionsExpiry =
  req
    "GetDatalakeExceptionsExpiry"
    "fixture/GetDatalakeExceptionsExpiry.yaml"

requestGetDatalakeExceptionsSubscription :: GetDatalakeExceptionsSubscription -> TestTree
requestGetDatalakeExceptionsSubscription =
  req
    "GetDatalakeExceptionsSubscription"
    "fixture/GetDatalakeExceptionsSubscription.yaml"

requestGetDatalakeStatus :: GetDatalakeStatus -> TestTree
requestGetDatalakeStatus =
  req
    "GetDatalakeStatus"
    "fixture/GetDatalakeStatus.yaml"

requestGetSubscriber :: GetSubscriber -> TestTree
requestGetSubscriber =
  req
    "GetSubscriber"
    "fixture/GetSubscriber.yaml"

requestListDatalakeExceptions :: ListDatalakeExceptions -> TestTree
requestListDatalakeExceptions =
  req
    "ListDatalakeExceptions"
    "fixture/ListDatalakeExceptions.yaml"

requestListLogSources :: ListLogSources -> TestTree
requestListLogSources =
  req
    "ListLogSources"
    "fixture/ListLogSources.yaml"

requestListSubscribers :: ListSubscribers -> TestTree
requestListSubscribers =
  req
    "ListSubscribers"
    "fixture/ListSubscribers.yaml"

requestUpdateDatalake :: UpdateDatalake -> TestTree
requestUpdateDatalake =
  req
    "UpdateDatalake"
    "fixture/UpdateDatalake.yaml"

requestUpdateDatalakeExceptionsExpiry :: UpdateDatalakeExceptionsExpiry -> TestTree
requestUpdateDatalakeExceptionsExpiry =
  req
    "UpdateDatalakeExceptionsExpiry"
    "fixture/UpdateDatalakeExceptionsExpiry.yaml"

requestUpdateDatalakeExceptionsSubscription :: UpdateDatalakeExceptionsSubscription -> TestTree
requestUpdateDatalakeExceptionsSubscription =
  req
    "UpdateDatalakeExceptionsSubscription"
    "fixture/UpdateDatalakeExceptionsSubscription.yaml"

requestUpdateSubscriber :: UpdateSubscriber -> TestTree
requestUpdateSubscriber =
  req
    "UpdateSubscriber"
    "fixture/UpdateSubscriber.yaml"

requestUpdateSubscriptionNotificationConfiguration :: UpdateSubscriptionNotificationConfiguration -> TestTree
requestUpdateSubscriptionNotificationConfiguration =
  req
    "UpdateSubscriptionNotificationConfiguration"
    "fixture/UpdateSubscriptionNotificationConfiguration.yaml"

-- Responses

responseCreateAwsLogSource :: CreateAwsLogSourceResponse -> TestTree
responseCreateAwsLogSource =
  res
    "CreateAwsLogSourceResponse"
    "fixture/CreateAwsLogSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAwsLogSource)

responseCreateCustomLogSource :: CreateCustomLogSourceResponse -> TestTree
responseCreateCustomLogSource =
  res
    "CreateCustomLogSourceResponse"
    "fixture/CreateCustomLogSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCustomLogSource)

responseCreateDatalake :: CreateDatalakeResponse -> TestTree
responseCreateDatalake =
  res
    "CreateDatalakeResponse"
    "fixture/CreateDatalakeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDatalake)

responseCreateDatalakeAutoEnable :: CreateDatalakeAutoEnableResponse -> TestTree
responseCreateDatalakeAutoEnable =
  res
    "CreateDatalakeAutoEnableResponse"
    "fixture/CreateDatalakeAutoEnableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDatalakeAutoEnable)

responseCreateDatalakeDelegatedAdmin :: CreateDatalakeDelegatedAdminResponse -> TestTree
responseCreateDatalakeDelegatedAdmin =
  res
    "CreateDatalakeDelegatedAdminResponse"
    "fixture/CreateDatalakeDelegatedAdminResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDatalakeDelegatedAdmin)

responseCreateDatalakeExceptionsSubscription :: CreateDatalakeExceptionsSubscriptionResponse -> TestTree
responseCreateDatalakeExceptionsSubscription =
  res
    "CreateDatalakeExceptionsSubscriptionResponse"
    "fixture/CreateDatalakeExceptionsSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDatalakeExceptionsSubscription)

responseCreateSubscriber :: CreateSubscriberResponse -> TestTree
responseCreateSubscriber =
  res
    "CreateSubscriberResponse"
    "fixture/CreateSubscriberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSubscriber)

responseCreateSubscriptionNotificationConfiguration :: CreateSubscriptionNotificationConfigurationResponse -> TestTree
responseCreateSubscriptionNotificationConfiguration =
  res
    "CreateSubscriptionNotificationConfigurationResponse"
    "fixture/CreateSubscriptionNotificationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSubscriptionNotificationConfiguration)

responseDeleteAwsLogSource :: DeleteAwsLogSourceResponse -> TestTree
responseDeleteAwsLogSource =
  res
    "DeleteAwsLogSourceResponse"
    "fixture/DeleteAwsLogSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAwsLogSource)

responseDeleteCustomLogSource :: DeleteCustomLogSourceResponse -> TestTree
responseDeleteCustomLogSource =
  res
    "DeleteCustomLogSourceResponse"
    "fixture/DeleteCustomLogSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCustomLogSource)

responseDeleteDatalake :: DeleteDatalakeResponse -> TestTree
responseDeleteDatalake =
  res
    "DeleteDatalakeResponse"
    "fixture/DeleteDatalakeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDatalake)

responseDeleteDatalakeAutoEnable :: DeleteDatalakeAutoEnableResponse -> TestTree
responseDeleteDatalakeAutoEnable =
  res
    "DeleteDatalakeAutoEnableResponse"
    "fixture/DeleteDatalakeAutoEnableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDatalakeAutoEnable)

responseDeleteDatalakeDelegatedAdmin :: DeleteDatalakeDelegatedAdminResponse -> TestTree
responseDeleteDatalakeDelegatedAdmin =
  res
    "DeleteDatalakeDelegatedAdminResponse"
    "fixture/DeleteDatalakeDelegatedAdminResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDatalakeDelegatedAdmin)

responseDeleteDatalakeExceptionsSubscription :: DeleteDatalakeExceptionsSubscriptionResponse -> TestTree
responseDeleteDatalakeExceptionsSubscription =
  res
    "DeleteDatalakeExceptionsSubscriptionResponse"
    "fixture/DeleteDatalakeExceptionsSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDatalakeExceptionsSubscription)

responseDeleteSubscriber :: DeleteSubscriberResponse -> TestTree
responseDeleteSubscriber =
  res
    "DeleteSubscriberResponse"
    "fixture/DeleteSubscriberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSubscriber)

responseDeleteSubscriptionNotificationConfiguration :: DeleteSubscriptionNotificationConfigurationResponse -> TestTree
responseDeleteSubscriptionNotificationConfiguration =
  res
    "DeleteSubscriptionNotificationConfigurationResponse"
    "fixture/DeleteSubscriptionNotificationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSubscriptionNotificationConfiguration)

responseGetDatalake :: GetDatalakeResponse -> TestTree
responseGetDatalake =
  res
    "GetDatalakeResponse"
    "fixture/GetDatalakeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDatalake)

responseGetDatalakeAutoEnable :: GetDatalakeAutoEnableResponse -> TestTree
responseGetDatalakeAutoEnable =
  res
    "GetDatalakeAutoEnableResponse"
    "fixture/GetDatalakeAutoEnableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDatalakeAutoEnable)

responseGetDatalakeExceptionsExpiry :: GetDatalakeExceptionsExpiryResponse -> TestTree
responseGetDatalakeExceptionsExpiry =
  res
    "GetDatalakeExceptionsExpiryResponse"
    "fixture/GetDatalakeExceptionsExpiryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDatalakeExceptionsExpiry)

responseGetDatalakeExceptionsSubscription :: GetDatalakeExceptionsSubscriptionResponse -> TestTree
responseGetDatalakeExceptionsSubscription =
  res
    "GetDatalakeExceptionsSubscriptionResponse"
    "fixture/GetDatalakeExceptionsSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDatalakeExceptionsSubscription)

responseGetDatalakeStatus :: GetDatalakeStatusResponse -> TestTree
responseGetDatalakeStatus =
  res
    "GetDatalakeStatusResponse"
    "fixture/GetDatalakeStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDatalakeStatus)

responseGetSubscriber :: GetSubscriberResponse -> TestTree
responseGetSubscriber =
  res
    "GetSubscriberResponse"
    "fixture/GetSubscriberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSubscriber)

responseListDatalakeExceptions :: ListDatalakeExceptionsResponse -> TestTree
responseListDatalakeExceptions =
  res
    "ListDatalakeExceptionsResponse"
    "fixture/ListDatalakeExceptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatalakeExceptions)

responseListLogSources :: ListLogSourcesResponse -> TestTree
responseListLogSources =
  res
    "ListLogSourcesResponse"
    "fixture/ListLogSourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLogSources)

responseListSubscribers :: ListSubscribersResponse -> TestTree
responseListSubscribers =
  res
    "ListSubscribersResponse"
    "fixture/ListSubscribersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSubscribers)

responseUpdateDatalake :: UpdateDatalakeResponse -> TestTree
responseUpdateDatalake =
  res
    "UpdateDatalakeResponse"
    "fixture/UpdateDatalakeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDatalake)

responseUpdateDatalakeExceptionsExpiry :: UpdateDatalakeExceptionsExpiryResponse -> TestTree
responseUpdateDatalakeExceptionsExpiry =
  res
    "UpdateDatalakeExceptionsExpiryResponse"
    "fixture/UpdateDatalakeExceptionsExpiryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDatalakeExceptionsExpiry)

responseUpdateDatalakeExceptionsSubscription :: UpdateDatalakeExceptionsSubscriptionResponse -> TestTree
responseUpdateDatalakeExceptionsSubscription =
  res
    "UpdateDatalakeExceptionsSubscriptionResponse"
    "fixture/UpdateDatalakeExceptionsSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDatalakeExceptionsSubscription)

responseUpdateSubscriber :: UpdateSubscriberResponse -> TestTree
responseUpdateSubscriber =
  res
    "UpdateSubscriberResponse"
    "fixture/UpdateSubscriberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSubscriber)

responseUpdateSubscriptionNotificationConfiguration :: UpdateSubscriptionNotificationConfigurationResponse -> TestTree
responseUpdateSubscriptionNotificationConfiguration =
  res
    "UpdateSubscriptionNotificationConfigurationResponse"
    "fixture/UpdateSubscriptionNotificationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSubscriptionNotificationConfiguration)
