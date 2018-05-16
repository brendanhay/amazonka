{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MQ
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--         [ requestCreateConfiguration $
--             createConfiguration
--
--         , requestCreateBroker $
--             createBroker
--
--         , requestDeleteBroker $
--             deleteBroker
--
--         , requestUpdateBroker $
--             updateBroker
--
--         , requestRebootBroker $
--             rebootBroker
--
--         , requestListConfigurationRevisions $
--             listConfigurationRevisions
--
--         , requestListUsers $
--             listUsers
--
--         , requestListConfigurations $
--             listConfigurations
--
--         , requestDescribeUser $
--             describeUser
--
--         , requestListBrokers $
--             listBrokers
--
--         , requestCreateUser $
--             createUser
--
--         , requestDescribeConfiguration $
--             describeConfiguration
--
--         , requestUpdateUser $
--             updateUser
--
--         , requestDeleteUser $
--             deleteUser
--
--         , requestDescribeConfigurationRevision $
--             describeConfigurationRevision
--
--         , requestDescribeBroker $
--             describeBroker
--
--         , requestUpdateConfiguration $
--             updateConfiguration
--
--           ]

--     , testGroup "response"
--         [ responseCreateConfiguration $
--             createConfigurationResponse
--
--         , responseCreateBroker $
--             createBrokerResponse
--
--         , responseDeleteBroker $
--             deleteBrokerResponse
--
--         , responseUpdateBroker $
--             updateBrokerResponse
--
--         , responseRebootBroker $
--             rebootBrokerResponse
--
--         , responseListConfigurationRevisions $
--             listConfigurationRevisionsResponse
--
--         , responseListUsers $
--             listUsersResponse
--
--         , responseListConfigurations $
--             listConfigurationsResponse
--
--         , responseDescribeUser $
--             describeUserResponse
--
--         , responseListBrokers $
--             listBrokersResponse
--
--         , responseCreateUser $
--             createUserResponse
--
--         , responseDescribeConfiguration $
--             describeConfigurationResponse
--
--         , responseUpdateUser $
--             updateUserResponse
--
--         , responseDeleteUser $
--             deleteUserResponse
--
--         , responseDescribeConfigurationRevision $
--             describeConfigurationRevisionResponse
--
--         , responseDescribeBroker $
--             describeBrokerResponse
--
--         , responseUpdateConfiguration $
--             updateConfigurationResponse
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

requestListUsers :: ListUsers -> TestTree
requestListUsers = req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestListConfigurations :: ListConfigurations -> TestTree
requestListConfigurations = req
    "ListConfigurations"
    "fixture/ListConfigurations.yaml"

requestDescribeUser :: DescribeUser -> TestTree
requestDescribeUser = req
    "DescribeUser"
    "fixture/DescribeUser.yaml"

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
    mq
    (Proxy :: Proxy CreateConfiguration)

responseCreateBroker :: CreateBrokerResponse -> TestTree
responseCreateBroker = res
    "CreateBrokerResponse"
    "fixture/CreateBrokerResponse.proto"
    mq
    (Proxy :: Proxy CreateBroker)

responseDeleteBroker :: DeleteBrokerResponse -> TestTree
responseDeleteBroker = res
    "DeleteBrokerResponse"
    "fixture/DeleteBrokerResponse.proto"
    mq
    (Proxy :: Proxy DeleteBroker)

responseUpdateBroker :: UpdateBrokerResponse -> TestTree
responseUpdateBroker = res
    "UpdateBrokerResponse"
    "fixture/UpdateBrokerResponse.proto"
    mq
    (Proxy :: Proxy UpdateBroker)

responseRebootBroker :: RebootBrokerResponse -> TestTree
responseRebootBroker = res
    "RebootBrokerResponse"
    "fixture/RebootBrokerResponse.proto"
    mq
    (Proxy :: Proxy RebootBroker)

responseListConfigurationRevisions :: ListConfigurationRevisionsResponse -> TestTree
responseListConfigurationRevisions = res
    "ListConfigurationRevisionsResponse"
    "fixture/ListConfigurationRevisionsResponse.proto"
    mq
    (Proxy :: Proxy ListConfigurationRevisions)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers = res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    mq
    (Proxy :: Proxy ListUsers)

responseListConfigurations :: ListConfigurationsResponse -> TestTree
responseListConfigurations = res
    "ListConfigurationsResponse"
    "fixture/ListConfigurationsResponse.proto"
    mq
    (Proxy :: Proxy ListConfigurations)

responseDescribeUser :: DescribeUserResponse -> TestTree
responseDescribeUser = res
    "DescribeUserResponse"
    "fixture/DescribeUserResponse.proto"
    mq
    (Proxy :: Proxy DescribeUser)

responseListBrokers :: ListBrokersResponse -> TestTree
responseListBrokers = res
    "ListBrokersResponse"
    "fixture/ListBrokersResponse.proto"
    mq
    (Proxy :: Proxy ListBrokers)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser = res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    mq
    (Proxy :: Proxy CreateUser)

responseDescribeConfiguration :: DescribeConfigurationResponse -> TestTree
responseDescribeConfiguration = res
    "DescribeConfigurationResponse"
    "fixture/DescribeConfigurationResponse.proto"
    mq
    (Proxy :: Proxy DescribeConfiguration)

responseUpdateUser :: UpdateUserResponse -> TestTree
responseUpdateUser = res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    mq
    (Proxy :: Proxy UpdateUser)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser = res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    mq
    (Proxy :: Proxy DeleteUser)

responseDescribeConfigurationRevision :: DescribeConfigurationRevisionResponse -> TestTree
responseDescribeConfigurationRevision = res
    "DescribeConfigurationRevisionResponse"
    "fixture/DescribeConfigurationRevisionResponse.proto"
    mq
    (Proxy :: Proxy DescribeConfigurationRevision)

responseDescribeBroker :: DescribeBrokerResponse -> TestTree
responseDescribeBroker = res
    "DescribeBrokerResponse"
    "fixture/DescribeBrokerResponse.proto"
    mq
    (Proxy :: Proxy DescribeBroker)

responseUpdateConfiguration :: UpdateConfigurationResponse -> TestTree
responseUpdateConfiguration = res
    "UpdateConfigurationResponse"
    "fixture/UpdateConfigurationResponse.proto"
    mq
    (Proxy :: Proxy UpdateConfiguration)
