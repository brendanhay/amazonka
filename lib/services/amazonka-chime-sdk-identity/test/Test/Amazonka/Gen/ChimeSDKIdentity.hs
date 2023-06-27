{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ChimeSDKIdentity
-- Copyright   : (c) 2013-2023 Brendan Hay
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
--         , requestCreateAppInstanceBot $
--             newCreateAppInstanceBot
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
--         , requestDeleteAppInstanceBot $
--             newDeleteAppInstanceBot
--
--         , requestDeleteAppInstanceUser $
--             newDeleteAppInstanceUser
--
--         , requestDeregisterAppInstanceUserEndpoint $
--             newDeregisterAppInstanceUserEndpoint
--
--         , requestDescribeAppInstance $
--             newDescribeAppInstance
--
--         , requestDescribeAppInstanceAdmin $
--             newDescribeAppInstanceAdmin
--
--         , requestDescribeAppInstanceBot $
--             newDescribeAppInstanceBot
--
--         , requestDescribeAppInstanceUser $
--             newDescribeAppInstanceUser
--
--         , requestDescribeAppInstanceUserEndpoint $
--             newDescribeAppInstanceUserEndpoint
--
--         , requestGetAppInstanceRetentionSettings $
--             newGetAppInstanceRetentionSettings
--
--         , requestListAppInstanceAdmins $
--             newListAppInstanceAdmins
--
--         , requestListAppInstanceBots $
--             newListAppInstanceBots
--
--         , requestListAppInstanceUserEndpoints $
--             newListAppInstanceUserEndpoints
--
--         , requestListAppInstanceUsers $
--             newListAppInstanceUsers
--
--         , requestListAppInstances $
--             newListAppInstances
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutAppInstanceRetentionSettings $
--             newPutAppInstanceRetentionSettings
--
--         , requestPutAppInstanceUserExpirationSettings $
--             newPutAppInstanceUserExpirationSettings
--
--         , requestRegisterAppInstanceUserEndpoint $
--             newRegisterAppInstanceUserEndpoint
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAppInstance $
--             newUpdateAppInstance
--
--         , requestUpdateAppInstanceBot $
--             newUpdateAppInstanceBot
--
--         , requestUpdateAppInstanceUser $
--             newUpdateAppInstanceUser
--
--         , requestUpdateAppInstanceUserEndpoint $
--             newUpdateAppInstanceUserEndpoint
--
--           ]

--     , testGroup "response"
--         [ responseCreateAppInstance $
--             newCreateAppInstanceResponse
--
--         , responseCreateAppInstanceAdmin $
--             newCreateAppInstanceAdminResponse
--
--         , responseCreateAppInstanceBot $
--             newCreateAppInstanceBotResponse
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
--         , responseDeleteAppInstanceBot $
--             newDeleteAppInstanceBotResponse
--
--         , responseDeleteAppInstanceUser $
--             newDeleteAppInstanceUserResponse
--
--         , responseDeregisterAppInstanceUserEndpoint $
--             newDeregisterAppInstanceUserEndpointResponse
--
--         , responseDescribeAppInstance $
--             newDescribeAppInstanceResponse
--
--         , responseDescribeAppInstanceAdmin $
--             newDescribeAppInstanceAdminResponse
--
--         , responseDescribeAppInstanceBot $
--             newDescribeAppInstanceBotResponse
--
--         , responseDescribeAppInstanceUser $
--             newDescribeAppInstanceUserResponse
--
--         , responseDescribeAppInstanceUserEndpoint $
--             newDescribeAppInstanceUserEndpointResponse
--
--         , responseGetAppInstanceRetentionSettings $
--             newGetAppInstanceRetentionSettingsResponse
--
--         , responseListAppInstanceAdmins $
--             newListAppInstanceAdminsResponse
--
--         , responseListAppInstanceBots $
--             newListAppInstanceBotsResponse
--
--         , responseListAppInstanceUserEndpoints $
--             newListAppInstanceUserEndpointsResponse
--
--         , responseListAppInstanceUsers $
--             newListAppInstanceUsersResponse
--
--         , responseListAppInstances $
--             newListAppInstancesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutAppInstanceRetentionSettings $
--             newPutAppInstanceRetentionSettingsResponse
--
--         , responsePutAppInstanceUserExpirationSettings $
--             newPutAppInstanceUserExpirationSettingsResponse
--
--         , responseRegisterAppInstanceUserEndpoint $
--             newRegisterAppInstanceUserEndpointResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAppInstance $
--             newUpdateAppInstanceResponse
--
--         , responseUpdateAppInstanceBot $
--             newUpdateAppInstanceBotResponse
--
--         , responseUpdateAppInstanceUser $
--             newUpdateAppInstanceUserResponse
--
--         , responseUpdateAppInstanceUserEndpoint $
--             newUpdateAppInstanceUserEndpointResponse
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

requestCreateAppInstanceBot :: CreateAppInstanceBot -> TestTree
requestCreateAppInstanceBot =
  req
    "CreateAppInstanceBot"
    "fixture/CreateAppInstanceBot.yaml"

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

requestDeleteAppInstanceBot :: DeleteAppInstanceBot -> TestTree
requestDeleteAppInstanceBot =
  req
    "DeleteAppInstanceBot"
    "fixture/DeleteAppInstanceBot.yaml"

requestDeleteAppInstanceUser :: DeleteAppInstanceUser -> TestTree
requestDeleteAppInstanceUser =
  req
    "DeleteAppInstanceUser"
    "fixture/DeleteAppInstanceUser.yaml"

requestDeregisterAppInstanceUserEndpoint :: DeregisterAppInstanceUserEndpoint -> TestTree
requestDeregisterAppInstanceUserEndpoint =
  req
    "DeregisterAppInstanceUserEndpoint"
    "fixture/DeregisterAppInstanceUserEndpoint.yaml"

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

requestDescribeAppInstanceBot :: DescribeAppInstanceBot -> TestTree
requestDescribeAppInstanceBot =
  req
    "DescribeAppInstanceBot"
    "fixture/DescribeAppInstanceBot.yaml"

requestDescribeAppInstanceUser :: DescribeAppInstanceUser -> TestTree
requestDescribeAppInstanceUser =
  req
    "DescribeAppInstanceUser"
    "fixture/DescribeAppInstanceUser.yaml"

requestDescribeAppInstanceUserEndpoint :: DescribeAppInstanceUserEndpoint -> TestTree
requestDescribeAppInstanceUserEndpoint =
  req
    "DescribeAppInstanceUserEndpoint"
    "fixture/DescribeAppInstanceUserEndpoint.yaml"

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

requestListAppInstanceBots :: ListAppInstanceBots -> TestTree
requestListAppInstanceBots =
  req
    "ListAppInstanceBots"
    "fixture/ListAppInstanceBots.yaml"

requestListAppInstanceUserEndpoints :: ListAppInstanceUserEndpoints -> TestTree
requestListAppInstanceUserEndpoints =
  req
    "ListAppInstanceUserEndpoints"
    "fixture/ListAppInstanceUserEndpoints.yaml"

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

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutAppInstanceRetentionSettings :: PutAppInstanceRetentionSettings -> TestTree
requestPutAppInstanceRetentionSettings =
  req
    "PutAppInstanceRetentionSettings"
    "fixture/PutAppInstanceRetentionSettings.yaml"

requestPutAppInstanceUserExpirationSettings :: PutAppInstanceUserExpirationSettings -> TestTree
requestPutAppInstanceUserExpirationSettings =
  req
    "PutAppInstanceUserExpirationSettings"
    "fixture/PutAppInstanceUserExpirationSettings.yaml"

requestRegisterAppInstanceUserEndpoint :: RegisterAppInstanceUserEndpoint -> TestTree
requestRegisterAppInstanceUserEndpoint =
  req
    "RegisterAppInstanceUserEndpoint"
    "fixture/RegisterAppInstanceUserEndpoint.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateAppInstance :: UpdateAppInstance -> TestTree
requestUpdateAppInstance =
  req
    "UpdateAppInstance"
    "fixture/UpdateAppInstance.yaml"

requestUpdateAppInstanceBot :: UpdateAppInstanceBot -> TestTree
requestUpdateAppInstanceBot =
  req
    "UpdateAppInstanceBot"
    "fixture/UpdateAppInstanceBot.yaml"

requestUpdateAppInstanceUser :: UpdateAppInstanceUser -> TestTree
requestUpdateAppInstanceUser =
  req
    "UpdateAppInstanceUser"
    "fixture/UpdateAppInstanceUser.yaml"

requestUpdateAppInstanceUserEndpoint :: UpdateAppInstanceUserEndpoint -> TestTree
requestUpdateAppInstanceUserEndpoint =
  req
    "UpdateAppInstanceUserEndpoint"
    "fixture/UpdateAppInstanceUserEndpoint.yaml"

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

responseCreateAppInstanceBot :: CreateAppInstanceBotResponse -> TestTree
responseCreateAppInstanceBot =
  res
    "CreateAppInstanceBotResponse"
    "fixture/CreateAppInstanceBotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAppInstanceBot)

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

responseDeleteAppInstanceBot :: DeleteAppInstanceBotResponse -> TestTree
responseDeleteAppInstanceBot =
  res
    "DeleteAppInstanceBotResponse"
    "fixture/DeleteAppInstanceBotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAppInstanceBot)

responseDeleteAppInstanceUser :: DeleteAppInstanceUserResponse -> TestTree
responseDeleteAppInstanceUser =
  res
    "DeleteAppInstanceUserResponse"
    "fixture/DeleteAppInstanceUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAppInstanceUser)

responseDeregisterAppInstanceUserEndpoint :: DeregisterAppInstanceUserEndpointResponse -> TestTree
responseDeregisterAppInstanceUserEndpoint =
  res
    "DeregisterAppInstanceUserEndpointResponse"
    "fixture/DeregisterAppInstanceUserEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterAppInstanceUserEndpoint)

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

responseDescribeAppInstanceBot :: DescribeAppInstanceBotResponse -> TestTree
responseDescribeAppInstanceBot =
  res
    "DescribeAppInstanceBotResponse"
    "fixture/DescribeAppInstanceBotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAppInstanceBot)

responseDescribeAppInstanceUser :: DescribeAppInstanceUserResponse -> TestTree
responseDescribeAppInstanceUser =
  res
    "DescribeAppInstanceUserResponse"
    "fixture/DescribeAppInstanceUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAppInstanceUser)

responseDescribeAppInstanceUserEndpoint :: DescribeAppInstanceUserEndpointResponse -> TestTree
responseDescribeAppInstanceUserEndpoint =
  res
    "DescribeAppInstanceUserEndpointResponse"
    "fixture/DescribeAppInstanceUserEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAppInstanceUserEndpoint)

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

responseListAppInstanceBots :: ListAppInstanceBotsResponse -> TestTree
responseListAppInstanceBots =
  res
    "ListAppInstanceBotsResponse"
    "fixture/ListAppInstanceBotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAppInstanceBots)

responseListAppInstanceUserEndpoints :: ListAppInstanceUserEndpointsResponse -> TestTree
responseListAppInstanceUserEndpoints =
  res
    "ListAppInstanceUserEndpointsResponse"
    "fixture/ListAppInstanceUserEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAppInstanceUserEndpoints)

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

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutAppInstanceRetentionSettings :: PutAppInstanceRetentionSettingsResponse -> TestTree
responsePutAppInstanceRetentionSettings =
  res
    "PutAppInstanceRetentionSettingsResponse"
    "fixture/PutAppInstanceRetentionSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAppInstanceRetentionSettings)

responsePutAppInstanceUserExpirationSettings :: PutAppInstanceUserExpirationSettingsResponse -> TestTree
responsePutAppInstanceUserExpirationSettings =
  res
    "PutAppInstanceUserExpirationSettingsResponse"
    "fixture/PutAppInstanceUserExpirationSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAppInstanceUserExpirationSettings)

responseRegisterAppInstanceUserEndpoint :: RegisterAppInstanceUserEndpointResponse -> TestTree
responseRegisterAppInstanceUserEndpoint =
  res
    "RegisterAppInstanceUserEndpointResponse"
    "fixture/RegisterAppInstanceUserEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterAppInstanceUserEndpoint)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateAppInstance :: UpdateAppInstanceResponse -> TestTree
responseUpdateAppInstance =
  res
    "UpdateAppInstanceResponse"
    "fixture/UpdateAppInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAppInstance)

responseUpdateAppInstanceBot :: UpdateAppInstanceBotResponse -> TestTree
responseUpdateAppInstanceBot =
  res
    "UpdateAppInstanceBotResponse"
    "fixture/UpdateAppInstanceBotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAppInstanceBot)

responseUpdateAppInstanceUser :: UpdateAppInstanceUserResponse -> TestTree
responseUpdateAppInstanceUser =
  res
    "UpdateAppInstanceUserResponse"
    "fixture/UpdateAppInstanceUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAppInstanceUser)

responseUpdateAppInstanceUserEndpoint :: UpdateAppInstanceUserEndpointResponse -> TestTree
responseUpdateAppInstanceUserEndpoint =
  res
    "UpdateAppInstanceUserEndpointResponse"
    "fixture/UpdateAppInstanceUserEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAppInstanceUserEndpoint)
