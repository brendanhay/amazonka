{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ChimeSDKIdentity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ChimeSDKIdentity where

import Amazonka.ChimeSDKIdentity
import qualified Data.Proxy as Proxy
import Test.Amazonka.ChimeSDKIdentity.Internal
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
--         [ requestCreateAppInstance $
--             newCreateAppInstance
--
--         , requestCreateAppInstanceAdmin $
--             newCreateAppInstanceAdmin
--
--         , requestCreateAppInstanceUser $
--             newCreateAppInstanceUser
--
--         , requestDeleteAppInstance $
--             newDeleteAppInstance
--
--         , requestDeleteAppInstanceAdmin $
--             newDeleteAppInstanceAdmin
--
--         , requestDeleteAppInstanceUser $
--             newDeleteAppInstanceUser
--
--         , requestDescribeAppInstance $
--             newDescribeAppInstance
--
--         , requestDescribeAppInstanceAdmin $
--             newDescribeAppInstanceAdmin
--
--         , requestDescribeAppInstanceUser $
--             newDescribeAppInstanceUser
--
--         , requestGetAppInstanceRetentionSettings $
--             newGetAppInstanceRetentionSettings
--
--         , requestListAppInstanceAdmins $
--             newListAppInstanceAdmins
--
--         , requestListAppInstanceUsers $
--             newListAppInstanceUsers
--
--         , requestListAppInstances $
--             newListAppInstances
--
--         , requestPutAppInstanceRetentionSettings $
--             newPutAppInstanceRetentionSettings
--
--         , requestUpdateAppInstance $
--             newUpdateAppInstance
--
--         , requestUpdateAppInstanceUser $
--             newUpdateAppInstanceUser
--
--           ]

--     , testGroup "response"
--         [ responseCreateAppInstance $
--             newCreateAppInstanceResponse
--
--         , responseCreateAppInstanceAdmin $
--             newCreateAppInstanceAdminResponse
--
--         , responseCreateAppInstanceUser $
--             newCreateAppInstanceUserResponse
--
--         , responseDeleteAppInstance $
--             newDeleteAppInstanceResponse
--
--         , responseDeleteAppInstanceAdmin $
--             newDeleteAppInstanceAdminResponse
--
--         , responseDeleteAppInstanceUser $
--             newDeleteAppInstanceUserResponse
--
--         , responseDescribeAppInstance $
--             newDescribeAppInstanceResponse
--
--         , responseDescribeAppInstanceAdmin $
--             newDescribeAppInstanceAdminResponse
--
--         , responseDescribeAppInstanceUser $
--             newDescribeAppInstanceUserResponse
--
--         , responseGetAppInstanceRetentionSettings $
--             newGetAppInstanceRetentionSettingsResponse
--
--         , responseListAppInstanceAdmins $
--             newListAppInstanceAdminsResponse
--
--         , responseListAppInstanceUsers $
--             newListAppInstanceUsersResponse
--
--         , responseListAppInstances $
--             newListAppInstancesResponse
--
--         , responsePutAppInstanceRetentionSettings $
--             newPutAppInstanceRetentionSettingsResponse
--
--         , responseUpdateAppInstance $
--             newUpdateAppInstanceResponse
--
--         , responseUpdateAppInstanceUser $
--             newUpdateAppInstanceUserResponse
--
--           ]
--     ]

-- Requests

requestCreateAppInstance :: CreateAppInstance -> TestTree
requestCreateAppInstance =
  req
    "CreateAppInstance"
    "fixture/CreateAppInstance.yaml"

requestCreateAppInstanceAdmin :: CreateAppInstanceAdmin -> TestTree
requestCreateAppInstanceAdmin =
  req
    "CreateAppInstanceAdmin"
    "fixture/CreateAppInstanceAdmin.yaml"

requestCreateAppInstanceUser :: CreateAppInstanceUser -> TestTree
requestCreateAppInstanceUser =
  req
    "CreateAppInstanceUser"
    "fixture/CreateAppInstanceUser.yaml"

requestDeleteAppInstance :: DeleteAppInstance -> TestTree
requestDeleteAppInstance =
  req
    "DeleteAppInstance"
    "fixture/DeleteAppInstance.yaml"

requestDeleteAppInstanceAdmin :: DeleteAppInstanceAdmin -> TestTree
requestDeleteAppInstanceAdmin =
  req
    "DeleteAppInstanceAdmin"
    "fixture/DeleteAppInstanceAdmin.yaml"

requestDeleteAppInstanceUser :: DeleteAppInstanceUser -> TestTree
requestDeleteAppInstanceUser =
  req
    "DeleteAppInstanceUser"
    "fixture/DeleteAppInstanceUser.yaml"

requestDescribeAppInstance :: DescribeAppInstance -> TestTree
requestDescribeAppInstance =
  req
    "DescribeAppInstance"
    "fixture/DescribeAppInstance.yaml"

requestDescribeAppInstanceAdmin :: DescribeAppInstanceAdmin -> TestTree
requestDescribeAppInstanceAdmin =
  req
    "DescribeAppInstanceAdmin"
    "fixture/DescribeAppInstanceAdmin.yaml"

requestDescribeAppInstanceUser :: DescribeAppInstanceUser -> TestTree
requestDescribeAppInstanceUser =
  req
    "DescribeAppInstanceUser"
    "fixture/DescribeAppInstanceUser.yaml"

requestGetAppInstanceRetentionSettings :: GetAppInstanceRetentionSettings -> TestTree
requestGetAppInstanceRetentionSettings =
  req
    "GetAppInstanceRetentionSettings"
    "fixture/GetAppInstanceRetentionSettings.yaml"

requestListAppInstanceAdmins :: ListAppInstanceAdmins -> TestTree
requestListAppInstanceAdmins =
  req
    "ListAppInstanceAdmins"
    "fixture/ListAppInstanceAdmins.yaml"

requestListAppInstanceUsers :: ListAppInstanceUsers -> TestTree
requestListAppInstanceUsers =
  req
    "ListAppInstanceUsers"
    "fixture/ListAppInstanceUsers.yaml"

requestListAppInstances :: ListAppInstances -> TestTree
requestListAppInstances =
  req
    "ListAppInstances"
    "fixture/ListAppInstances.yaml"

requestPutAppInstanceRetentionSettings :: PutAppInstanceRetentionSettings -> TestTree
requestPutAppInstanceRetentionSettings =
  req
    "PutAppInstanceRetentionSettings"
    "fixture/PutAppInstanceRetentionSettings.yaml"

requestUpdateAppInstance :: UpdateAppInstance -> TestTree
requestUpdateAppInstance =
  req
    "UpdateAppInstance"
    "fixture/UpdateAppInstance.yaml"

requestUpdateAppInstanceUser :: UpdateAppInstanceUser -> TestTree
requestUpdateAppInstanceUser =
  req
    "UpdateAppInstanceUser"
    "fixture/UpdateAppInstanceUser.yaml"

-- Responses

responseCreateAppInstance :: CreateAppInstanceResponse -> TestTree
responseCreateAppInstance =
  res
    "CreateAppInstanceResponse"
    "fixture/CreateAppInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAppInstance)

responseCreateAppInstanceAdmin :: CreateAppInstanceAdminResponse -> TestTree
responseCreateAppInstanceAdmin =
  res
    "CreateAppInstanceAdminResponse"
    "fixture/CreateAppInstanceAdminResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAppInstanceAdmin)

responseCreateAppInstanceUser :: CreateAppInstanceUserResponse -> TestTree
responseCreateAppInstanceUser =
  res
    "CreateAppInstanceUserResponse"
    "fixture/CreateAppInstanceUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAppInstanceUser)

responseDeleteAppInstance :: DeleteAppInstanceResponse -> TestTree
responseDeleteAppInstance =
  res
    "DeleteAppInstanceResponse"
    "fixture/DeleteAppInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAppInstance)

responseDeleteAppInstanceAdmin :: DeleteAppInstanceAdminResponse -> TestTree
responseDeleteAppInstanceAdmin =
  res
    "DeleteAppInstanceAdminResponse"
    "fixture/DeleteAppInstanceAdminResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAppInstanceAdmin)

responseDeleteAppInstanceUser :: DeleteAppInstanceUserResponse -> TestTree
responseDeleteAppInstanceUser =
  res
    "DeleteAppInstanceUserResponse"
    "fixture/DeleteAppInstanceUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAppInstanceUser)

responseDescribeAppInstance :: DescribeAppInstanceResponse -> TestTree
responseDescribeAppInstance =
  res
    "DescribeAppInstanceResponse"
    "fixture/DescribeAppInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAppInstance)

responseDescribeAppInstanceAdmin :: DescribeAppInstanceAdminResponse -> TestTree
responseDescribeAppInstanceAdmin =
  res
    "DescribeAppInstanceAdminResponse"
    "fixture/DescribeAppInstanceAdminResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAppInstanceAdmin)

responseDescribeAppInstanceUser :: DescribeAppInstanceUserResponse -> TestTree
responseDescribeAppInstanceUser =
  res
    "DescribeAppInstanceUserResponse"
    "fixture/DescribeAppInstanceUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAppInstanceUser)

responseGetAppInstanceRetentionSettings :: GetAppInstanceRetentionSettingsResponse -> TestTree
responseGetAppInstanceRetentionSettings =
  res
    "GetAppInstanceRetentionSettingsResponse"
    "fixture/GetAppInstanceRetentionSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAppInstanceRetentionSettings)

responseListAppInstanceAdmins :: ListAppInstanceAdminsResponse -> TestTree
responseListAppInstanceAdmins =
  res
    "ListAppInstanceAdminsResponse"
    "fixture/ListAppInstanceAdminsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAppInstanceAdmins)

responseListAppInstanceUsers :: ListAppInstanceUsersResponse -> TestTree
responseListAppInstanceUsers =
  res
    "ListAppInstanceUsersResponse"
    "fixture/ListAppInstanceUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAppInstanceUsers)

responseListAppInstances :: ListAppInstancesResponse -> TestTree
responseListAppInstances =
  res
    "ListAppInstancesResponse"
    "fixture/ListAppInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAppInstances)

responsePutAppInstanceRetentionSettings :: PutAppInstanceRetentionSettingsResponse -> TestTree
responsePutAppInstanceRetentionSettings =
  res
    "PutAppInstanceRetentionSettingsResponse"
    "fixture/PutAppInstanceRetentionSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAppInstanceRetentionSettings)

responseUpdateAppInstance :: UpdateAppInstanceResponse -> TestTree
responseUpdateAppInstance =
  res
    "UpdateAppInstanceResponse"
    "fixture/UpdateAppInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAppInstance)

responseUpdateAppInstanceUser :: UpdateAppInstanceUserResponse -> TestTree
responseUpdateAppInstanceUser =
  res
    "UpdateAppInstanceUserResponse"
    "fixture/UpdateAppInstanceUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAppInstanceUser)
