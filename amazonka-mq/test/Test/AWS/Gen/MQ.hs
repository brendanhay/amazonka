{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MQ
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.MQ where

import Data.Proxy
import Network.AWS.MQ
import Test.AWS.Fixture
import Test.AWS.MQ.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateBroker $
--             newCreateBroker
--
--         , requestDescribeBrokerInstanceOptions $
--             newDescribeBrokerInstanceOptions
--
--         , requestUpdateConfiguration $
--             newUpdateConfiguration
--
--         , requestListConfigurations $
--             newListConfigurations
--
--         , requestDescribeBroker $
--             newDescribeBroker
--
--         , requestDescribeBrokerEngineTypes $
--             newDescribeBrokerEngineTypes
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestListBrokers $
--             newListBrokers
--
--         , requestUpdateBroker $
--             newUpdateBroker
--
--         , requestDeleteBroker $
--             newDeleteBroker
--
--         , requestRebootBroker $
--             newRebootBroker
--
--         , requestListConfigurationRevisions $
--             newListConfigurationRevisions
--
--         , requestCreateConfiguration $
--             newCreateConfiguration
--
--         , requestDescribeUser $
--             newDescribeUser
--
--         , requestDescribeConfigurationRevision $
--             newDescribeConfigurationRevision
--
--         , requestListTags $
--             newListTags
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestListUsers $
--             newListUsers
--
--         , requestUpdateUser $
--             newUpdateUser
--
--         , requestDescribeConfiguration $
--             newDescribeConfiguration
--
--         , requestCreateTags $
--             newCreateTags
--
--           ]

--     , testGroup "response"
--         [ responseCreateBroker $
--             newCreateBrokerResponse
--
--         , responseDescribeBrokerInstanceOptions $
--             newDescribeBrokerInstanceOptionsResponse
--
--         , responseUpdateConfiguration $
--             newUpdateConfigurationResponse
--
--         , responseListConfigurations $
--             newListConfigurationsResponse
--
--         , responseDescribeBroker $
--             newDescribeBrokerResponse
--
--         , responseDescribeBrokerEngineTypes $
--             newDescribeBrokerEngineTypesResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseListBrokers $
--             newListBrokersResponse
--
--         , responseUpdateBroker $
--             newUpdateBrokerResponse
--
--         , responseDeleteBroker $
--             newDeleteBrokerResponse
--
--         , responseRebootBroker $
--             newRebootBrokerResponse
--
--         , responseListConfigurationRevisions $
--             newListConfigurationRevisionsResponse
--
--         , responseCreateConfiguration $
--             newCreateConfigurationResponse
--
--         , responseDescribeUser $
--             newDescribeUserResponse
--
--         , responseDescribeConfigurationRevision $
--             newDescribeConfigurationRevisionResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseListUsers $
--             newListUsersResponse
--
--         , responseUpdateUser $
--             newUpdateUserResponse
--
--         , responseDescribeConfiguration $
--             newDescribeConfigurationResponse
--
--         , responseCreateTags $
--             newCreateTagsResponse
--
--           ]
--     ]

-- Requests

requestCreateBroker :: CreateBroker -> TestTree
requestCreateBroker =
  req
    "CreateBroker"
    "fixture/CreateBroker.yaml"

requestDescribeBrokerInstanceOptions :: DescribeBrokerInstanceOptions -> TestTree
requestDescribeBrokerInstanceOptions =
  req
    "DescribeBrokerInstanceOptions"
    "fixture/DescribeBrokerInstanceOptions.yaml"

requestUpdateConfiguration :: UpdateConfiguration -> TestTree
requestUpdateConfiguration =
  req
    "UpdateConfiguration"
    "fixture/UpdateConfiguration.yaml"

requestListConfigurations :: ListConfigurations -> TestTree
requestListConfigurations =
  req
    "ListConfigurations"
    "fixture/ListConfigurations.yaml"

requestDescribeBroker :: DescribeBroker -> TestTree
requestDescribeBroker =
  req
    "DescribeBroker"
    "fixture/DescribeBroker.yaml"

requestDescribeBrokerEngineTypes :: DescribeBrokerEngineTypes -> TestTree
requestDescribeBrokerEngineTypes =
  req
    "DescribeBrokerEngineTypes"
    "fixture/DescribeBrokerEngineTypes.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags =
  req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestListBrokers :: ListBrokers -> TestTree
requestListBrokers =
  req
    "ListBrokers"
    "fixture/ListBrokers.yaml"

requestUpdateBroker :: UpdateBroker -> TestTree
requestUpdateBroker =
  req
    "UpdateBroker"
    "fixture/UpdateBroker.yaml"

requestDeleteBroker :: DeleteBroker -> TestTree
requestDeleteBroker =
  req
    "DeleteBroker"
    "fixture/DeleteBroker.yaml"

requestRebootBroker :: RebootBroker -> TestTree
requestRebootBroker =
  req
    "RebootBroker"
    "fixture/RebootBroker.yaml"

requestListConfigurationRevisions :: ListConfigurationRevisions -> TestTree
requestListConfigurationRevisions =
  req
    "ListConfigurationRevisions"
    "fixture/ListConfigurationRevisions.yaml"

requestCreateConfiguration :: CreateConfiguration -> TestTree
requestCreateConfiguration =
  req
    "CreateConfiguration"
    "fixture/CreateConfiguration.yaml"

requestDescribeUser :: DescribeUser -> TestTree
requestDescribeUser =
  req
    "DescribeUser"
    "fixture/DescribeUser.yaml"

requestDescribeConfigurationRevision :: DescribeConfigurationRevision -> TestTree
requestDescribeConfigurationRevision =
  req
    "DescribeConfigurationRevision"
    "fixture/DescribeConfigurationRevision.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers =
  req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestUpdateUser :: UpdateUser -> TestTree
requestUpdateUser =
  req
    "UpdateUser"
    "fixture/UpdateUser.yaml"

requestDescribeConfiguration :: DescribeConfiguration -> TestTree
requestDescribeConfiguration =
  req
    "DescribeConfiguration"
    "fixture/DescribeConfiguration.yaml"

requestCreateTags :: CreateTags -> TestTree
requestCreateTags =
  req
    "CreateTags"
    "fixture/CreateTags.yaml"

-- Responses

responseCreateBroker :: CreateBrokerResponse -> TestTree
responseCreateBroker =
  res
    "CreateBrokerResponse"
    "fixture/CreateBrokerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBroker)

responseDescribeBrokerInstanceOptions :: DescribeBrokerInstanceOptionsResponse -> TestTree
responseDescribeBrokerInstanceOptions =
  res
    "DescribeBrokerInstanceOptionsResponse"
    "fixture/DescribeBrokerInstanceOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBrokerInstanceOptions)

responseUpdateConfiguration :: UpdateConfigurationResponse -> TestTree
responseUpdateConfiguration =
  res
    "UpdateConfigurationResponse"
    "fixture/UpdateConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateConfiguration)

responseListConfigurations :: ListConfigurationsResponse -> TestTree
responseListConfigurations =
  res
    "ListConfigurationsResponse"
    "fixture/ListConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListConfigurations)

responseDescribeBroker :: DescribeBrokerResponse -> TestTree
responseDescribeBroker =
  res
    "DescribeBrokerResponse"
    "fixture/DescribeBrokerResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBroker)

responseDescribeBrokerEngineTypes :: DescribeBrokerEngineTypesResponse -> TestTree
responseDescribeBrokerEngineTypes =
  res
    "DescribeBrokerEngineTypesResponse"
    "fixture/DescribeBrokerEngineTypesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBrokerEngineTypes)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTags)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUser)

responseListBrokers :: ListBrokersResponse -> TestTree
responseListBrokers =
  res
    "ListBrokersResponse"
    "fixture/ListBrokersResponse.proto"
    defaultService
    (Proxy :: Proxy ListBrokers)

responseUpdateBroker :: UpdateBrokerResponse -> TestTree
responseUpdateBroker =
  res
    "UpdateBrokerResponse"
    "fixture/UpdateBrokerResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateBroker)

responseDeleteBroker :: DeleteBrokerResponse -> TestTree
responseDeleteBroker =
  res
    "DeleteBrokerResponse"
    "fixture/DeleteBrokerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBroker)

responseRebootBroker :: RebootBrokerResponse -> TestTree
responseRebootBroker =
  res
    "RebootBrokerResponse"
    "fixture/RebootBrokerResponse.proto"
    defaultService
    (Proxy :: Proxy RebootBroker)

responseListConfigurationRevisions :: ListConfigurationRevisionsResponse -> TestTree
responseListConfigurationRevisions =
  res
    "ListConfigurationRevisionsResponse"
    "fixture/ListConfigurationRevisionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListConfigurationRevisions)

responseCreateConfiguration :: CreateConfigurationResponse -> TestTree
responseCreateConfiguration =
  res
    "CreateConfigurationResponse"
    "fixture/CreateConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConfiguration)

responseDescribeUser :: DescribeUserResponse -> TestTree
responseDescribeUser =
  res
    "DescribeUserResponse"
    "fixture/DescribeUserResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUser)

responseDescribeConfigurationRevision :: DescribeConfigurationRevisionResponse -> TestTree
responseDescribeConfigurationRevision =
  res
    "DescribeConfigurationRevisionResponse"
    "fixture/DescribeConfigurationRevisionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConfigurationRevision)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTags)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUser)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    defaultService
    (Proxy :: Proxy ListUsers)

responseUpdateUser :: UpdateUserResponse -> TestTree
responseUpdateUser =
  res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUser)

responseDescribeConfiguration :: DescribeConfigurationResponse -> TestTree
responseDescribeConfiguration =
  res
    "DescribeConfigurationResponse"
    "fixture/DescribeConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConfiguration)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags =
  res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTags)
