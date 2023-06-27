{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SecurityLake
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--         , requestCreateDataLake $
--             newCreateDataLake
--
--         , requestCreateDataLakeExceptionSubscription $
--             newCreateDataLakeExceptionSubscription
--
--         , requestCreateDataLakeOrganizationConfiguration $
--             newCreateDataLakeOrganizationConfiguration
--
--         , requestCreateSubscriber $
--             newCreateSubscriber
--
--         , requestCreateSubscriberNotification $
--             newCreateSubscriberNotification
--
--         , requestDeleteAwsLogSource $
--             newDeleteAwsLogSource
--
--         , requestDeleteCustomLogSource $
--             newDeleteCustomLogSource
--
--         , requestDeleteDataLake $
--             newDeleteDataLake
--
--         , requestDeleteDataLakeExceptionSubscription $
--             newDeleteDataLakeExceptionSubscription
--
--         , requestDeleteDataLakeOrganizationConfiguration $
--             newDeleteDataLakeOrganizationConfiguration
--
--         , requestDeleteSubscriber $
--             newDeleteSubscriber
--
--         , requestDeleteSubscriberNotification $
--             newDeleteSubscriberNotification
--
--         , requestDeregisterDataLakeDelegatedAdministrator $
--             newDeregisterDataLakeDelegatedAdministrator
--
--         , requestGetDataLakeExceptionSubscription $
--             newGetDataLakeExceptionSubscription
--
--         , requestGetDataLakeOrganizationConfiguration $
--             newGetDataLakeOrganizationConfiguration
--
--         , requestGetDataLakeSources $
--             newGetDataLakeSources
--
--         , requestGetSubscriber $
--             newGetSubscriber
--
--         , requestListDataLakeExceptions $
--             newListDataLakeExceptions
--
--         , requestListDataLakes $
--             newListDataLakes
--
--         , requestListLogSources $
--             newListLogSources
--
--         , requestListSubscribers $
--             newListSubscribers
--
--         , requestRegisterDataLakeDelegatedAdministrator $
--             newRegisterDataLakeDelegatedAdministrator
--
--         , requestUpdateDataLake $
--             newUpdateDataLake
--
--         , requestUpdateDataLakeExceptionSubscription $
--             newUpdateDataLakeExceptionSubscription
--
--         , requestUpdateSubscriber $
--             newUpdateSubscriber
--
--         , requestUpdateSubscriberNotification $
--             newUpdateSubscriberNotification
--
--           ]

--     , testGroup "response"
--         [ responseCreateAwsLogSource $
--             newCreateAwsLogSourceResponse
--
--         , responseCreateCustomLogSource $
--             newCreateCustomLogSourceResponse
--
--         , responseCreateDataLake $
--             newCreateDataLakeResponse
--
--         , responseCreateDataLakeExceptionSubscription $
--             newCreateDataLakeExceptionSubscriptionResponse
--
--         , responseCreateDataLakeOrganizationConfiguration $
--             newCreateDataLakeOrganizationConfigurationResponse
--
--         , responseCreateSubscriber $
--             newCreateSubscriberResponse
--
--         , responseCreateSubscriberNotification $
--             newCreateSubscriberNotificationResponse
--
--         , responseDeleteAwsLogSource $
--             newDeleteAwsLogSourceResponse
--
--         , responseDeleteCustomLogSource $
--             newDeleteCustomLogSourceResponse
--
--         , responseDeleteDataLake $
--             newDeleteDataLakeResponse
--
--         , responseDeleteDataLakeExceptionSubscription $
--             newDeleteDataLakeExceptionSubscriptionResponse
--
--         , responseDeleteDataLakeOrganizationConfiguration $
--             newDeleteDataLakeOrganizationConfigurationResponse
--
--         , responseDeleteSubscriber $
--             newDeleteSubscriberResponse
--
--         , responseDeleteSubscriberNotification $
--             newDeleteSubscriberNotificationResponse
--
--         , responseDeregisterDataLakeDelegatedAdministrator $
--             newDeregisterDataLakeDelegatedAdministratorResponse
--
--         , responseGetDataLakeExceptionSubscription $
--             newGetDataLakeExceptionSubscriptionResponse
--
--         , responseGetDataLakeOrganizationConfiguration $
--             newGetDataLakeOrganizationConfigurationResponse
--
--         , responseGetDataLakeSources $
--             newGetDataLakeSourcesResponse
--
--         , responseGetSubscriber $
--             newGetSubscriberResponse
--
--         , responseListDataLakeExceptions $
--             newListDataLakeExceptionsResponse
--
--         , responseListDataLakes $
--             newListDataLakesResponse
--
--         , responseListLogSources $
--             newListLogSourcesResponse
--
--         , responseListSubscribers $
--             newListSubscribersResponse
--
--         , responseRegisterDataLakeDelegatedAdministrator $
--             newRegisterDataLakeDelegatedAdministratorResponse
--
--         , responseUpdateDataLake $
--             newUpdateDataLakeResponse
--
--         , responseUpdateDataLakeExceptionSubscription $
--             newUpdateDataLakeExceptionSubscriptionResponse
--
--         , responseUpdateSubscriber $
--             newUpdateSubscriberResponse
--
--         , responseUpdateSubscriberNotification $
--             newUpdateSubscriberNotificationResponse
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

requestCreateDataLake :: CreateDataLake -> TestTree
requestCreateDataLake =
  req
    "CreateDataLake"
    "fixture/CreateDataLake.yaml"

requestCreateDataLakeExceptionSubscription :: CreateDataLakeExceptionSubscription -> TestTree
requestCreateDataLakeExceptionSubscription =
  req
    "CreateDataLakeExceptionSubscription"
    "fixture/CreateDataLakeExceptionSubscription.yaml"

requestCreateDataLakeOrganizationConfiguration :: CreateDataLakeOrganizationConfiguration -> TestTree
requestCreateDataLakeOrganizationConfiguration =
  req
    "CreateDataLakeOrganizationConfiguration"
    "fixture/CreateDataLakeOrganizationConfiguration.yaml"

requestCreateSubscriber :: CreateSubscriber -> TestTree
requestCreateSubscriber =
  req
    "CreateSubscriber"
    "fixture/CreateSubscriber.yaml"

requestCreateSubscriberNotification :: CreateSubscriberNotification -> TestTree
requestCreateSubscriberNotification =
  req
    "CreateSubscriberNotification"
    "fixture/CreateSubscriberNotification.yaml"

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

requestDeleteDataLake :: DeleteDataLake -> TestTree
requestDeleteDataLake =
  req
    "DeleteDataLake"
    "fixture/DeleteDataLake.yaml"

requestDeleteDataLakeExceptionSubscription :: DeleteDataLakeExceptionSubscription -> TestTree
requestDeleteDataLakeExceptionSubscription =
  req
    "DeleteDataLakeExceptionSubscription"
    "fixture/DeleteDataLakeExceptionSubscription.yaml"

requestDeleteDataLakeOrganizationConfiguration :: DeleteDataLakeOrganizationConfiguration -> TestTree
requestDeleteDataLakeOrganizationConfiguration =
  req
    "DeleteDataLakeOrganizationConfiguration"
    "fixture/DeleteDataLakeOrganizationConfiguration.yaml"

requestDeleteSubscriber :: DeleteSubscriber -> TestTree
requestDeleteSubscriber =
  req
    "DeleteSubscriber"
    "fixture/DeleteSubscriber.yaml"

requestDeleteSubscriberNotification :: DeleteSubscriberNotification -> TestTree
requestDeleteSubscriberNotification =
  req
    "DeleteSubscriberNotification"
    "fixture/DeleteSubscriberNotification.yaml"

requestDeregisterDataLakeDelegatedAdministrator :: DeregisterDataLakeDelegatedAdministrator -> TestTree
requestDeregisterDataLakeDelegatedAdministrator =
  req
    "DeregisterDataLakeDelegatedAdministrator"
    "fixture/DeregisterDataLakeDelegatedAdministrator.yaml"

requestGetDataLakeExceptionSubscription :: GetDataLakeExceptionSubscription -> TestTree
requestGetDataLakeExceptionSubscription =
  req
    "GetDataLakeExceptionSubscription"
    "fixture/GetDataLakeExceptionSubscription.yaml"

requestGetDataLakeOrganizationConfiguration :: GetDataLakeOrganizationConfiguration -> TestTree
requestGetDataLakeOrganizationConfiguration =
  req
    "GetDataLakeOrganizationConfiguration"
    "fixture/GetDataLakeOrganizationConfiguration.yaml"

requestGetDataLakeSources :: GetDataLakeSources -> TestTree
requestGetDataLakeSources =
  req
    "GetDataLakeSources"
    "fixture/GetDataLakeSources.yaml"

requestGetSubscriber :: GetSubscriber -> TestTree
requestGetSubscriber =
  req
    "GetSubscriber"
    "fixture/GetSubscriber.yaml"

requestListDataLakeExceptions :: ListDataLakeExceptions -> TestTree
requestListDataLakeExceptions =
  req
    "ListDataLakeExceptions"
    "fixture/ListDataLakeExceptions.yaml"

requestListDataLakes :: ListDataLakes -> TestTree
requestListDataLakes =
  req
    "ListDataLakes"
    "fixture/ListDataLakes.yaml"

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

requestRegisterDataLakeDelegatedAdministrator :: RegisterDataLakeDelegatedAdministrator -> TestTree
requestRegisterDataLakeDelegatedAdministrator =
  req
    "RegisterDataLakeDelegatedAdministrator"
    "fixture/RegisterDataLakeDelegatedAdministrator.yaml"

requestUpdateDataLake :: UpdateDataLake -> TestTree
requestUpdateDataLake =
  req
    "UpdateDataLake"
    "fixture/UpdateDataLake.yaml"

requestUpdateDataLakeExceptionSubscription :: UpdateDataLakeExceptionSubscription -> TestTree
requestUpdateDataLakeExceptionSubscription =
  req
    "UpdateDataLakeExceptionSubscription"
    "fixture/UpdateDataLakeExceptionSubscription.yaml"

requestUpdateSubscriber :: UpdateSubscriber -> TestTree
requestUpdateSubscriber =
  req
    "UpdateSubscriber"
    "fixture/UpdateSubscriber.yaml"

requestUpdateSubscriberNotification :: UpdateSubscriberNotification -> TestTree
requestUpdateSubscriberNotification =
  req
    "UpdateSubscriberNotification"
    "fixture/UpdateSubscriberNotification.yaml"

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

responseCreateDataLake :: CreateDataLakeResponse -> TestTree
responseCreateDataLake =
  res
    "CreateDataLakeResponse"
    "fixture/CreateDataLakeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataLake)

responseCreateDataLakeExceptionSubscription :: CreateDataLakeExceptionSubscriptionResponse -> TestTree
responseCreateDataLakeExceptionSubscription =
  res
    "CreateDataLakeExceptionSubscriptionResponse"
    "fixture/CreateDataLakeExceptionSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataLakeExceptionSubscription)

responseCreateDataLakeOrganizationConfiguration :: CreateDataLakeOrganizationConfigurationResponse -> TestTree
responseCreateDataLakeOrganizationConfiguration =
  res
    "CreateDataLakeOrganizationConfigurationResponse"
    "fixture/CreateDataLakeOrganizationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataLakeOrganizationConfiguration)

responseCreateSubscriber :: CreateSubscriberResponse -> TestTree
responseCreateSubscriber =
  res
    "CreateSubscriberResponse"
    "fixture/CreateSubscriberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSubscriber)

responseCreateSubscriberNotification :: CreateSubscriberNotificationResponse -> TestTree
responseCreateSubscriberNotification =
  res
    "CreateSubscriberNotificationResponse"
    "fixture/CreateSubscriberNotificationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSubscriberNotification)

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

responseDeleteDataLake :: DeleteDataLakeResponse -> TestTree
responseDeleteDataLake =
  res
    "DeleteDataLakeResponse"
    "fixture/DeleteDataLakeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataLake)

responseDeleteDataLakeExceptionSubscription :: DeleteDataLakeExceptionSubscriptionResponse -> TestTree
responseDeleteDataLakeExceptionSubscription =
  res
    "DeleteDataLakeExceptionSubscriptionResponse"
    "fixture/DeleteDataLakeExceptionSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataLakeExceptionSubscription)

responseDeleteDataLakeOrganizationConfiguration :: DeleteDataLakeOrganizationConfigurationResponse -> TestTree
responseDeleteDataLakeOrganizationConfiguration =
  res
    "DeleteDataLakeOrganizationConfigurationResponse"
    "fixture/DeleteDataLakeOrganizationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataLakeOrganizationConfiguration)

responseDeleteSubscriber :: DeleteSubscriberResponse -> TestTree
responseDeleteSubscriber =
  res
    "DeleteSubscriberResponse"
    "fixture/DeleteSubscriberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSubscriber)

responseDeleteSubscriberNotification :: DeleteSubscriberNotificationResponse -> TestTree
responseDeleteSubscriberNotification =
  res
    "DeleteSubscriberNotificationResponse"
    "fixture/DeleteSubscriberNotificationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSubscriberNotification)

responseDeregisterDataLakeDelegatedAdministrator :: DeregisterDataLakeDelegatedAdministratorResponse -> TestTree
responseDeregisterDataLakeDelegatedAdministrator =
  res
    "DeregisterDataLakeDelegatedAdministratorResponse"
    "fixture/DeregisterDataLakeDelegatedAdministratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterDataLakeDelegatedAdministrator)

responseGetDataLakeExceptionSubscription :: GetDataLakeExceptionSubscriptionResponse -> TestTree
responseGetDataLakeExceptionSubscription =
  res
    "GetDataLakeExceptionSubscriptionResponse"
    "fixture/GetDataLakeExceptionSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataLakeExceptionSubscription)

responseGetDataLakeOrganizationConfiguration :: GetDataLakeOrganizationConfigurationResponse -> TestTree
responseGetDataLakeOrganizationConfiguration =
  res
    "GetDataLakeOrganizationConfigurationResponse"
    "fixture/GetDataLakeOrganizationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataLakeOrganizationConfiguration)

responseGetDataLakeSources :: GetDataLakeSourcesResponse -> TestTree
responseGetDataLakeSources =
  res
    "GetDataLakeSourcesResponse"
    "fixture/GetDataLakeSourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataLakeSources)

responseGetSubscriber :: GetSubscriberResponse -> TestTree
responseGetSubscriber =
  res
    "GetSubscriberResponse"
    "fixture/GetSubscriberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSubscriber)

responseListDataLakeExceptions :: ListDataLakeExceptionsResponse -> TestTree
responseListDataLakeExceptions =
  res
    "ListDataLakeExceptionsResponse"
    "fixture/ListDataLakeExceptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataLakeExceptions)

responseListDataLakes :: ListDataLakesResponse -> TestTree
responseListDataLakes =
  res
    "ListDataLakesResponse"
    "fixture/ListDataLakesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataLakes)

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

responseRegisterDataLakeDelegatedAdministrator :: RegisterDataLakeDelegatedAdministratorResponse -> TestTree
responseRegisterDataLakeDelegatedAdministrator =
  res
    "RegisterDataLakeDelegatedAdministratorResponse"
    "fixture/RegisterDataLakeDelegatedAdministratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterDataLakeDelegatedAdministrator)

responseUpdateDataLake :: UpdateDataLakeResponse -> TestTree
responseUpdateDataLake =
  res
    "UpdateDataLakeResponse"
    "fixture/UpdateDataLakeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataLake)

responseUpdateDataLakeExceptionSubscription :: UpdateDataLakeExceptionSubscriptionResponse -> TestTree
responseUpdateDataLakeExceptionSubscription =
  res
    "UpdateDataLakeExceptionSubscriptionResponse"
    "fixture/UpdateDataLakeExceptionSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataLakeExceptionSubscription)

responseUpdateSubscriber :: UpdateSubscriberResponse -> TestTree
responseUpdateSubscriber =
  res
    "UpdateSubscriberResponse"
    "fixture/UpdateSubscriberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSubscriber)

responseUpdateSubscriberNotification :: UpdateSubscriberNotificationResponse -> TestTree
responseUpdateSubscriberNotification =
  res
    "UpdateSubscriberNotificationResponse"
    "fixture/UpdateSubscriberNotificationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSubscriberNotification)
