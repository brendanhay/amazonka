{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MQ
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.MQ where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.MQ
import Test.AWS.MQ.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateConfiguration $
--             mkCreateConfiguration
--
--         , requestCreateBroker $
--             mkCreateBroker
--
--         , requestDeleteBroker $
--             mkDeleteBroker
--
--         , requestUpdateBroker $
--             mkUpdateBroker
--
--         , requestRebootBroker $
--             mkRebootBroker
--
--         , requestListConfigurationRevisions $
--             mkListConfigurationRevisions
--
--         , requestCreateTags $
--             mkCreateTags
--
--         , requestListUsers $
--             mkListUsers
--
--         , requestDeleteTags $
--             mkDeleteTags
--
--         , requestListConfigurations $
--             mkListConfigurations
--
--         , requestDescribeUser $
--             mkDescribeUser
--
--         , requestDescribeBrokerInstanceOptions $
--             mkDescribeBrokerInstanceOptions
--
--         , requestListBrokers $
--             mkListBrokers
--
--         , requestCreateUser $
--             mkCreateUser
--
--         , requestDescribeConfiguration $
--             mkDescribeConfiguration
--
--         , requestUpdateUser $
--             mkUpdateUser
--
--         , requestDeleteUser $
--             mkDeleteUser
--
--         , requestListTags $
--             mkListTags
--
--         , requestDescribeBrokerEngineTypes $
--             mkDescribeBrokerEngineTypes
--
--         , requestDescribeConfigurationRevision $
--             mkDescribeConfigurationRevision
--
--         , requestDescribeBroker $
--             mkDescribeBroker
--
--         , requestUpdateConfiguration $
--             mkUpdateConfiguration
--
--           ]

--     , testGroup "response"
--         [ responseCreateConfiguration $
--             mkCreateConfigurationResponse
--
--         , responseCreateBroker $
--             mkCreateBrokerResponse
--
--         , responseDeleteBroker $
--             mkDeleteBrokerResponse
--
--         , responseUpdateBroker $
--             mkUpdateBrokerResponse
--
--         , responseRebootBroker $
--             mkRebootBrokerResponse
--
--         , responseListConfigurationRevisions $
--             mkListConfigurationRevisionsResponse
--
--         , responseCreateTags $
--             mkCreateTagsResponse
--
--         , responseListUsers $
--             mkListUsersResponse
--
--         , responseDeleteTags $
--             mkDeleteTagsResponse
--
--         , responseListConfigurations $
--             mkListConfigurationsResponse
--
--         , responseDescribeUser $
--             mkDescribeUserResponse
--
--         , responseDescribeBrokerInstanceOptions $
--             mkDescribeBrokerInstanceOptionsResponse
--
--         , responseListBrokers $
--             mkListBrokersResponse
--
--         , responseCreateUser $
--             mkCreateUserResponse
--
--         , responseDescribeConfiguration $
--             mkDescribeConfigurationResponse
--
--         , responseUpdateUser $
--             mkUpdateUserResponse
--
--         , responseDeleteUser $
--             mkDeleteUserResponse
--
--         , responseListTags $
--             mkListTagsResponse
--
--         , responseDescribeBrokerEngineTypes $
--             mkDescribeBrokerEngineTypesResponse
--
--         , responseDescribeConfigurationRevision $
--             mkDescribeConfigurationRevisionResponse
--
--         , responseDescribeBroker $
--             mkDescribeBrokerResponse
--
--         , responseUpdateConfiguration $
--             mkUpdateConfigurationResponse
--
--           ]
--     ]

-- Requests

requestCreateConfiguration :: CreateConfiguration -> TestTree
requestCreateConfiguration = req
    "CreateConfiguration"
    "fixture/CreateConfiguration.yaml"

requestCreateBroker :: CreateBroker -> TestTree
requestCreateBroker = req
    "CreateBroker"
    "fixture/CreateBroker.yaml"

requestDeleteBroker :: DeleteBroker -> TestTree
requestDeleteBroker = req
    "DeleteBroker"
    "fixture/DeleteBroker.yaml"

requestUpdateBroker :: UpdateBroker -> TestTree
requestUpdateBroker = req
    "UpdateBroker"
    "fixture/UpdateBroker.yaml"

requestRebootBroker :: RebootBroker -> TestTree
requestRebootBroker = req
    "RebootBroker"
    "fixture/RebootBroker.yaml"

requestListConfigurationRevisions :: ListConfigurationRevisions -> TestTree
requestListConfigurationRevisions = req
    "ListConfigurationRevisions"
    "fixture/ListConfigurationRevisions.yaml"

requestCreateTags :: CreateTags -> TestTree
requestCreateTags = req
    "CreateTags"
    "fixture/CreateTags.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers = req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags = req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestListConfigurations :: ListConfigurations -> TestTree
requestListConfigurations = req
    "ListConfigurations"
    "fixture/ListConfigurations.yaml"

requestDescribeUser :: DescribeUser -> TestTree
requestDescribeUser = req
    "DescribeUser"
    "fixture/DescribeUser.yaml"

requestDescribeBrokerInstanceOptions :: DescribeBrokerInstanceOptions -> TestTree
requestDescribeBrokerInstanceOptions = req
    "DescribeBrokerInstanceOptions"
    "fixture/DescribeBrokerInstanceOptions.yaml"

requestListBrokers :: ListBrokers -> TestTree
requestListBrokers = req
    "ListBrokers"
    "fixture/ListBrokers.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser = req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestDescribeConfiguration :: DescribeConfiguration -> TestTree
requestDescribeConfiguration = req
    "DescribeConfiguration"
    "fixture/DescribeConfiguration.yaml"

requestUpdateUser :: UpdateUser -> TestTree
requestUpdateUser = req
    "UpdateUser"
    "fixture/UpdateUser.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser = req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestListTags :: ListTags -> TestTree
requestListTags = req
    "ListTags"
    "fixture/ListTags.yaml"

requestDescribeBrokerEngineTypes :: DescribeBrokerEngineTypes -> TestTree
requestDescribeBrokerEngineTypes = req
    "DescribeBrokerEngineTypes"
    "fixture/DescribeBrokerEngineTypes.yaml"

requestDescribeConfigurationRevision :: DescribeConfigurationRevision -> TestTree
requestDescribeConfigurationRevision = req
    "DescribeConfigurationRevision"
    "fixture/DescribeConfigurationRevision.yaml"

requestDescribeBroker :: DescribeBroker -> TestTree
requestDescribeBroker = req
    "DescribeBroker"
    "fixture/DescribeBroker.yaml"

requestUpdateConfiguration :: UpdateConfiguration -> TestTree
requestUpdateConfiguration = req
    "UpdateConfiguration"
    "fixture/UpdateConfiguration.yaml"

-- Responses

responseCreateConfiguration :: CreateConfigurationResponse -> TestTree
responseCreateConfiguration = res
    "CreateConfigurationResponse"
    "fixture/CreateConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateConfiguration)

responseCreateBroker :: CreateBrokerResponse -> TestTree
responseCreateBroker = res
    "CreateBrokerResponse"
    "fixture/CreateBrokerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateBroker)

responseDeleteBroker :: DeleteBrokerResponse -> TestTree
responseDeleteBroker = res
    "DeleteBrokerResponse"
    "fixture/DeleteBrokerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBroker)

responseUpdateBroker :: UpdateBrokerResponse -> TestTree
responseUpdateBroker = res
    "UpdateBrokerResponse"
    "fixture/UpdateBrokerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateBroker)

responseRebootBroker :: RebootBrokerResponse -> TestTree
responseRebootBroker = res
    "RebootBrokerResponse"
    "fixture/RebootBrokerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RebootBroker)

responseListConfigurationRevisions :: ListConfigurationRevisionsResponse -> TestTree
responseListConfigurationRevisions = res
    "ListConfigurationRevisionsResponse"
    "fixture/ListConfigurationRevisionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListConfigurationRevisions)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags = res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTags)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers = res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListUsers)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTags)

responseListConfigurations :: ListConfigurationsResponse -> TestTree
responseListConfigurations = res
    "ListConfigurationsResponse"
    "fixture/ListConfigurationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListConfigurations)

responseDescribeUser :: DescribeUserResponse -> TestTree
responseDescribeUser = res
    "DescribeUserResponse"
    "fixture/DescribeUserResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeUser)

responseDescribeBrokerInstanceOptions :: DescribeBrokerInstanceOptionsResponse -> TestTree
responseDescribeBrokerInstanceOptions = res
    "DescribeBrokerInstanceOptionsResponse"
    "fixture/DescribeBrokerInstanceOptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeBrokerInstanceOptions)

responseListBrokers :: ListBrokersResponse -> TestTree
responseListBrokers = res
    "ListBrokersResponse"
    "fixture/ListBrokersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListBrokers)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser = res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateUser)

responseDescribeConfiguration :: DescribeConfigurationResponse -> TestTree
responseDescribeConfiguration = res
    "DescribeConfigurationResponse"
    "fixture/DescribeConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeConfiguration)

responseUpdateUser :: UpdateUserResponse -> TestTree
responseUpdateUser = res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateUser)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser = res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteUser)

responseListTags :: ListTagsResponse -> TestTree
responseListTags = res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTags)

responseDescribeBrokerEngineTypes :: DescribeBrokerEngineTypesResponse -> TestTree
responseDescribeBrokerEngineTypes = res
    "DescribeBrokerEngineTypesResponse"
    "fixture/DescribeBrokerEngineTypesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeBrokerEngineTypes)

responseDescribeConfigurationRevision :: DescribeConfigurationRevisionResponse -> TestTree
responseDescribeConfigurationRevision = res
    "DescribeConfigurationRevisionResponse"
    "fixture/DescribeConfigurationRevisionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeConfigurationRevision)

responseDescribeBroker :: DescribeBrokerResponse -> TestTree
responseDescribeBroker = res
    "DescribeBrokerResponse"
    "fixture/DescribeBrokerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeBroker)

responseUpdateConfiguration :: UpdateConfigurationResponse -> TestTree
responseUpdateConfiguration = res
    "UpdateConfigurationResponse"
    "fixture/UpdateConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateConfiguration)
