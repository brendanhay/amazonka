{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MQ
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.MQ where

import Amazonka.MQ
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.MQ.Internal
import Test.Amazonka.Prelude
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
--         , requestCreateConfiguration $
--             newCreateConfiguration
--
--         , requestCreateTags $
--             newCreateTags
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestDeleteBroker $
--             newDeleteBroker
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestDescribeBroker $
--             newDescribeBroker
--
--         , requestDescribeBrokerEngineTypes $
--             newDescribeBrokerEngineTypes
--
--         , requestDescribeBrokerInstanceOptions $
--             newDescribeBrokerInstanceOptions
--
--         , requestDescribeConfiguration $
--             newDescribeConfiguration
--
--         , requestDescribeConfigurationRevision $
--             newDescribeConfigurationRevision
--
--         , requestDescribeUser $
--             newDescribeUser
--
--         , requestListBrokers $
--             newListBrokers
--
--         , requestListConfigurationRevisions $
--             newListConfigurationRevisions
--
--         , requestListConfigurations $
--             newListConfigurations
--
--         , requestListTags $
--             newListTags
--
--         , requestListUsers $
--             newListUsers
--
--         , requestPromote $
--             newPromote
--
--         , requestRebootBroker $
--             newRebootBroker
--
--         , requestUpdateBroker $
--             newUpdateBroker
--
--         , requestUpdateConfiguration $
--             newUpdateConfiguration
--
--         , requestUpdateUser $
--             newUpdateUser
--
--           ]

--     , testGroup "response"
--         [ responseCreateBroker $
--             newCreateBrokerResponse
--
--         , responseCreateConfiguration $
--             newCreateConfigurationResponse
--
--         , responseCreateTags $
--             newCreateTagsResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseDeleteBroker $
--             newDeleteBrokerResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseDescribeBroker $
--             newDescribeBrokerResponse
--
--         , responseDescribeBrokerEngineTypes $
--             newDescribeBrokerEngineTypesResponse
--
--         , responseDescribeBrokerInstanceOptions $
--             newDescribeBrokerInstanceOptionsResponse
--
--         , responseDescribeConfiguration $
--             newDescribeConfigurationResponse
--
--         , responseDescribeConfigurationRevision $
--             newDescribeConfigurationRevisionResponse
--
--         , responseDescribeUser $
--             newDescribeUserResponse
--
--         , responseListBrokers $
--             newListBrokersResponse
--
--         , responseListConfigurationRevisions $
--             newListConfigurationRevisionsResponse
--
--         , responseListConfigurations $
--             newListConfigurationsResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responseListUsers $
--             newListUsersResponse
--
--         , responsePromote $
--             newPromoteResponse
--
--         , responseRebootBroker $
--             newRebootBrokerResponse
--
--         , responseUpdateBroker $
--             newUpdateBrokerResponse
--
--         , responseUpdateConfiguration $
--             newUpdateConfigurationResponse
--
--         , responseUpdateUser $
--             newUpdateUserResponse
--
--           ]
--     ]

-- Requests

requestCreateBroker :: CreateBroker -> TestTree
requestCreateBroker =
  req
    "CreateBroker"
    "fixture/CreateBroker.yaml"

requestCreateConfiguration :: CreateConfiguration -> TestTree
requestCreateConfiguration =
  req
    "CreateConfiguration"
    "fixture/CreateConfiguration.yaml"

requestCreateTags :: CreateTags -> TestTree
requestCreateTags =
  req
    "CreateTags"
    "fixture/CreateTags.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestDeleteBroker :: DeleteBroker -> TestTree
requestDeleteBroker =
  req
    "DeleteBroker"
    "fixture/DeleteBroker.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags =
  req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

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

requestDescribeBrokerInstanceOptions :: DescribeBrokerInstanceOptions -> TestTree
requestDescribeBrokerInstanceOptions =
  req
    "DescribeBrokerInstanceOptions"
    "fixture/DescribeBrokerInstanceOptions.yaml"

requestDescribeConfiguration :: DescribeConfiguration -> TestTree
requestDescribeConfiguration =
  req
    "DescribeConfiguration"
    "fixture/DescribeConfiguration.yaml"

requestDescribeConfigurationRevision :: DescribeConfigurationRevision -> TestTree
requestDescribeConfigurationRevision =
  req
    "DescribeConfigurationRevision"
    "fixture/DescribeConfigurationRevision.yaml"

requestDescribeUser :: DescribeUser -> TestTree
requestDescribeUser =
  req
    "DescribeUser"
    "fixture/DescribeUser.yaml"

requestListBrokers :: ListBrokers -> TestTree
requestListBrokers =
  req
    "ListBrokers"
    "fixture/ListBrokers.yaml"

requestListConfigurationRevisions :: ListConfigurationRevisions -> TestTree
requestListConfigurationRevisions =
  req
    "ListConfigurationRevisions"
    "fixture/ListConfigurationRevisions.yaml"

requestListConfigurations :: ListConfigurations -> TestTree
requestListConfigurations =
  req
    "ListConfigurations"
    "fixture/ListConfigurations.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers =
  req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestPromote :: Promote -> TestTree
requestPromote =
  req
    "Promote"
    "fixture/Promote.yaml"

requestRebootBroker :: RebootBroker -> TestTree
requestRebootBroker =
  req
    "RebootBroker"
    "fixture/RebootBroker.yaml"

requestUpdateBroker :: UpdateBroker -> TestTree
requestUpdateBroker =
  req
    "UpdateBroker"
    "fixture/UpdateBroker.yaml"

requestUpdateConfiguration :: UpdateConfiguration -> TestTree
requestUpdateConfiguration =
  req
    "UpdateConfiguration"
    "fixture/UpdateConfiguration.yaml"

requestUpdateUser :: UpdateUser -> TestTree
requestUpdateUser =
  req
    "UpdateUser"
    "fixture/UpdateUser.yaml"

-- Responses

responseCreateBroker :: CreateBrokerResponse -> TestTree
responseCreateBroker =
  res
    "CreateBrokerResponse"
    "fixture/CreateBrokerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBroker)

responseCreateConfiguration :: CreateConfigurationResponse -> TestTree
responseCreateConfiguration =
  res
    "CreateConfigurationResponse"
    "fixture/CreateConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConfiguration)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags =
  res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTags)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUser)

responseDeleteBroker :: DeleteBrokerResponse -> TestTree
responseDeleteBroker =
  res
    "DeleteBrokerResponse"
    "fixture/DeleteBrokerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBroker)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTags)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUser)

responseDescribeBroker :: DescribeBrokerResponse -> TestTree
responseDescribeBroker =
  res
    "DescribeBrokerResponse"
    "fixture/DescribeBrokerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBroker)

responseDescribeBrokerEngineTypes :: DescribeBrokerEngineTypesResponse -> TestTree
responseDescribeBrokerEngineTypes =
  res
    "DescribeBrokerEngineTypesResponse"
    "fixture/DescribeBrokerEngineTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBrokerEngineTypes)

responseDescribeBrokerInstanceOptions :: DescribeBrokerInstanceOptionsResponse -> TestTree
responseDescribeBrokerInstanceOptions =
  res
    "DescribeBrokerInstanceOptionsResponse"
    "fixture/DescribeBrokerInstanceOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBrokerInstanceOptions)

responseDescribeConfiguration :: DescribeConfigurationResponse -> TestTree
responseDescribeConfiguration =
  res
    "DescribeConfigurationResponse"
    "fixture/DescribeConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConfiguration)

responseDescribeConfigurationRevision :: DescribeConfigurationRevisionResponse -> TestTree
responseDescribeConfigurationRevision =
  res
    "DescribeConfigurationRevisionResponse"
    "fixture/DescribeConfigurationRevisionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConfigurationRevision)

responseDescribeUser :: DescribeUserResponse -> TestTree
responseDescribeUser =
  res
    "DescribeUserResponse"
    "fixture/DescribeUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUser)

responseListBrokers :: ListBrokersResponse -> TestTree
responseListBrokers =
  res
    "ListBrokersResponse"
    "fixture/ListBrokersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBrokers)

responseListConfigurationRevisions :: ListConfigurationRevisionsResponse -> TestTree
responseListConfigurationRevisions =
  res
    "ListConfigurationRevisionsResponse"
    "fixture/ListConfigurationRevisionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConfigurationRevisions)

responseListConfigurations :: ListConfigurationsResponse -> TestTree
responseListConfigurations =
  res
    "ListConfigurationsResponse"
    "fixture/ListConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConfigurations)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTags)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUsers)

responsePromote :: PromoteResponse -> TestTree
responsePromote =
  res
    "PromoteResponse"
    "fixture/PromoteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Promote)

responseRebootBroker :: RebootBrokerResponse -> TestTree
responseRebootBroker =
  res
    "RebootBrokerResponse"
    "fixture/RebootBrokerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RebootBroker)

responseUpdateBroker :: UpdateBrokerResponse -> TestTree
responseUpdateBroker =
  res
    "UpdateBrokerResponse"
    "fixture/UpdateBrokerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBroker)

responseUpdateConfiguration :: UpdateConfigurationResponse -> TestTree
responseUpdateConfiguration =
  res
    "UpdateConfigurationResponse"
    "fixture/UpdateConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConfiguration)

responseUpdateUser :: UpdateUserResponse -> TestTree
responseUpdateUser =
  res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUser)
