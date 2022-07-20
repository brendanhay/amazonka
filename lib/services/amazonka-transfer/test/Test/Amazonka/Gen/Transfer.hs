{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Transfer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Transfer where

import Amazonka.Transfer
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.Transfer.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateAccess $
--             newCreateAccess
--
--         , requestCreateServer $
--             newCreateServer
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestCreateWorkflow $
--             newCreateWorkflow
--
--         , requestDeleteAccess $
--             newDeleteAccess
--
--         , requestDeleteServer $
--             newDeleteServer
--
--         , requestDeleteSshPublicKey $
--             newDeleteSshPublicKey
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestDeleteWorkflow $
--             newDeleteWorkflow
--
--         , requestDescribeAccess $
--             newDescribeAccess
--
--         , requestDescribeExecution $
--             newDescribeExecution
--
--         , requestDescribeSecurityPolicy $
--             newDescribeSecurityPolicy
--
--         , requestDescribeServer $
--             newDescribeServer
--
--         , requestDescribeUser $
--             newDescribeUser
--
--         , requestDescribeWorkflow $
--             newDescribeWorkflow
--
--         , requestImportSshPublicKey $
--             newImportSshPublicKey
--
--         , requestListAccesses $
--             newListAccesses
--
--         , requestListExecutions $
--             newListExecutions
--
--         , requestListSecurityPolicies $
--             newListSecurityPolicies
--
--         , requestListServers $
--             newListServers
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListUsers $
--             newListUsers
--
--         , requestListWorkflows $
--             newListWorkflows
--
--         , requestSendWorkflowStepState $
--             newSendWorkflowStepState
--
--         , requestStartServer $
--             newStartServer
--
--         , requestStopServer $
--             newStopServer
--
--         , requestTagResource $
--             newTagResource
--
--         , requestTestIdentityProvider $
--             newTestIdentityProvider
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAccess $
--             newUpdateAccess
--
--         , requestUpdateServer $
--             newUpdateServer
--
--         , requestUpdateUser $
--             newUpdateUser
--
--           ]

--     , testGroup "response"
--         [ responseCreateAccess $
--             newCreateAccessResponse
--
--         , responseCreateServer $
--             newCreateServerResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseCreateWorkflow $
--             newCreateWorkflowResponse
--
--         , responseDeleteAccess $
--             newDeleteAccessResponse
--
--         , responseDeleteServer $
--             newDeleteServerResponse
--
--         , responseDeleteSshPublicKey $
--             newDeleteSshPublicKeyResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseDeleteWorkflow $
--             newDeleteWorkflowResponse
--
--         , responseDescribeAccess $
--             newDescribeAccessResponse
--
--         , responseDescribeExecution $
--             newDescribeExecutionResponse
--
--         , responseDescribeSecurityPolicy $
--             newDescribeSecurityPolicyResponse
--
--         , responseDescribeServer $
--             newDescribeServerResponse
--
--         , responseDescribeUser $
--             newDescribeUserResponse
--
--         , responseDescribeWorkflow $
--             newDescribeWorkflowResponse
--
--         , responseImportSshPublicKey $
--             newImportSshPublicKeyResponse
--
--         , responseListAccesses $
--             newListAccessesResponse
--
--         , responseListExecutions $
--             newListExecutionsResponse
--
--         , responseListSecurityPolicies $
--             newListSecurityPoliciesResponse
--
--         , responseListServers $
--             newListServersResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListUsers $
--             newListUsersResponse
--
--         , responseListWorkflows $
--             newListWorkflowsResponse
--
--         , responseSendWorkflowStepState $
--             newSendWorkflowStepStateResponse
--
--         , responseStartServer $
--             newStartServerResponse
--
--         , responseStopServer $
--             newStopServerResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseTestIdentityProvider $
--             newTestIdentityProviderResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAccess $
--             newUpdateAccessResponse
--
--         , responseUpdateServer $
--             newUpdateServerResponse
--
--         , responseUpdateUser $
--             newUpdateUserResponse
--
--           ]
--     ]

-- Requests

requestCreateAccess :: CreateAccess -> TestTree
requestCreateAccess =
  req
    "CreateAccess"
    "fixture/CreateAccess.yaml"

requestCreateServer :: CreateServer -> TestTree
requestCreateServer =
  req
    "CreateServer"
    "fixture/CreateServer.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestCreateWorkflow :: CreateWorkflow -> TestTree
requestCreateWorkflow =
  req
    "CreateWorkflow"
    "fixture/CreateWorkflow.yaml"

requestDeleteAccess :: DeleteAccess -> TestTree
requestDeleteAccess =
  req
    "DeleteAccess"
    "fixture/DeleteAccess.yaml"

requestDeleteServer :: DeleteServer -> TestTree
requestDeleteServer =
  req
    "DeleteServer"
    "fixture/DeleteServer.yaml"

requestDeleteSshPublicKey :: DeleteSshPublicKey -> TestTree
requestDeleteSshPublicKey =
  req
    "DeleteSshPublicKey"
    "fixture/DeleteSshPublicKey.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestDeleteWorkflow :: DeleteWorkflow -> TestTree
requestDeleteWorkflow =
  req
    "DeleteWorkflow"
    "fixture/DeleteWorkflow.yaml"

requestDescribeAccess :: DescribeAccess -> TestTree
requestDescribeAccess =
  req
    "DescribeAccess"
    "fixture/DescribeAccess.yaml"

requestDescribeExecution :: DescribeExecution -> TestTree
requestDescribeExecution =
  req
    "DescribeExecution"
    "fixture/DescribeExecution.yaml"

requestDescribeSecurityPolicy :: DescribeSecurityPolicy -> TestTree
requestDescribeSecurityPolicy =
  req
    "DescribeSecurityPolicy"
    "fixture/DescribeSecurityPolicy.yaml"

requestDescribeServer :: DescribeServer -> TestTree
requestDescribeServer =
  req
    "DescribeServer"
    "fixture/DescribeServer.yaml"

requestDescribeUser :: DescribeUser -> TestTree
requestDescribeUser =
  req
    "DescribeUser"
    "fixture/DescribeUser.yaml"

requestDescribeWorkflow :: DescribeWorkflow -> TestTree
requestDescribeWorkflow =
  req
    "DescribeWorkflow"
    "fixture/DescribeWorkflow.yaml"

requestImportSshPublicKey :: ImportSshPublicKey -> TestTree
requestImportSshPublicKey =
  req
    "ImportSshPublicKey"
    "fixture/ImportSshPublicKey.yaml"

requestListAccesses :: ListAccesses -> TestTree
requestListAccesses =
  req
    "ListAccesses"
    "fixture/ListAccesses.yaml"

requestListExecutions :: ListExecutions -> TestTree
requestListExecutions =
  req
    "ListExecutions"
    "fixture/ListExecutions.yaml"

requestListSecurityPolicies :: ListSecurityPolicies -> TestTree
requestListSecurityPolicies =
  req
    "ListSecurityPolicies"
    "fixture/ListSecurityPolicies.yaml"

requestListServers :: ListServers -> TestTree
requestListServers =
  req
    "ListServers"
    "fixture/ListServers.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers =
  req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestListWorkflows :: ListWorkflows -> TestTree
requestListWorkflows =
  req
    "ListWorkflows"
    "fixture/ListWorkflows.yaml"

requestSendWorkflowStepState :: SendWorkflowStepState -> TestTree
requestSendWorkflowStepState =
  req
    "SendWorkflowStepState"
    "fixture/SendWorkflowStepState.yaml"

requestStartServer :: StartServer -> TestTree
requestStartServer =
  req
    "StartServer"
    "fixture/StartServer.yaml"

requestStopServer :: StopServer -> TestTree
requestStopServer =
  req
    "StopServer"
    "fixture/StopServer.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestTestIdentityProvider :: TestIdentityProvider -> TestTree
requestTestIdentityProvider =
  req
    "TestIdentityProvider"
    "fixture/TestIdentityProvider.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateAccess :: UpdateAccess -> TestTree
requestUpdateAccess =
  req
    "UpdateAccess"
    "fixture/UpdateAccess.yaml"

requestUpdateServer :: UpdateServer -> TestTree
requestUpdateServer =
  req
    "UpdateServer"
    "fixture/UpdateServer.yaml"

requestUpdateUser :: UpdateUser -> TestTree
requestUpdateUser =
  req
    "UpdateUser"
    "fixture/UpdateUser.yaml"

-- Responses

responseCreateAccess :: CreateAccessResponse -> TestTree
responseCreateAccess =
  res
    "CreateAccessResponse"
    "fixture/CreateAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAccess)

responseCreateServer :: CreateServerResponse -> TestTree
responseCreateServer =
  res
    "CreateServerResponse"
    "fixture/CreateServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateServer)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUser)

responseCreateWorkflow :: CreateWorkflowResponse -> TestTree
responseCreateWorkflow =
  res
    "CreateWorkflowResponse"
    "fixture/CreateWorkflowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkflow)

responseDeleteAccess :: DeleteAccessResponse -> TestTree
responseDeleteAccess =
  res
    "DeleteAccessResponse"
    "fixture/DeleteAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccess)

responseDeleteServer :: DeleteServerResponse -> TestTree
responseDeleteServer =
  res
    "DeleteServerResponse"
    "fixture/DeleteServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteServer)

responseDeleteSshPublicKey :: DeleteSshPublicKeyResponse -> TestTree
responseDeleteSshPublicKey =
  res
    "DeleteSshPublicKeyResponse"
    "fixture/DeleteSshPublicKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSshPublicKey)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUser)

responseDeleteWorkflow :: DeleteWorkflowResponse -> TestTree
responseDeleteWorkflow =
  res
    "DeleteWorkflowResponse"
    "fixture/DeleteWorkflowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkflow)

responseDescribeAccess :: DescribeAccessResponse -> TestTree
responseDescribeAccess =
  res
    "DescribeAccessResponse"
    "fixture/DescribeAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccess)

responseDescribeExecution :: DescribeExecutionResponse -> TestTree
responseDescribeExecution =
  res
    "DescribeExecutionResponse"
    "fixture/DescribeExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeExecution)

responseDescribeSecurityPolicy :: DescribeSecurityPolicyResponse -> TestTree
responseDescribeSecurityPolicy =
  res
    "DescribeSecurityPolicyResponse"
    "fixture/DescribeSecurityPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSecurityPolicy)

responseDescribeServer :: DescribeServerResponse -> TestTree
responseDescribeServer =
  res
    "DescribeServerResponse"
    "fixture/DescribeServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeServer)

responseDescribeUser :: DescribeUserResponse -> TestTree
responseDescribeUser =
  res
    "DescribeUserResponse"
    "fixture/DescribeUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUser)

responseDescribeWorkflow :: DescribeWorkflowResponse -> TestTree
responseDescribeWorkflow =
  res
    "DescribeWorkflowResponse"
    "fixture/DescribeWorkflowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkflow)

responseImportSshPublicKey :: ImportSshPublicKeyResponse -> TestTree
responseImportSshPublicKey =
  res
    "ImportSshPublicKeyResponse"
    "fixture/ImportSshPublicKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportSshPublicKey)

responseListAccesses :: ListAccessesResponse -> TestTree
responseListAccesses =
  res
    "ListAccessesResponse"
    "fixture/ListAccessesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccesses)

responseListExecutions :: ListExecutionsResponse -> TestTree
responseListExecutions =
  res
    "ListExecutionsResponse"
    "fixture/ListExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExecutions)

responseListSecurityPolicies :: ListSecurityPoliciesResponse -> TestTree
responseListSecurityPolicies =
  res
    "ListSecurityPoliciesResponse"
    "fixture/ListSecurityPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSecurityPolicies)

responseListServers :: ListServersResponse -> TestTree
responseListServers =
  res
    "ListServersResponse"
    "fixture/ListServersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServers)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUsers)

responseListWorkflows :: ListWorkflowsResponse -> TestTree
responseListWorkflows =
  res
    "ListWorkflowsResponse"
    "fixture/ListWorkflowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkflows)

responseSendWorkflowStepState :: SendWorkflowStepStateResponse -> TestTree
responseSendWorkflowStepState =
  res
    "SendWorkflowStepStateResponse"
    "fixture/SendWorkflowStepStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendWorkflowStepState)

responseStartServer :: StartServerResponse -> TestTree
responseStartServer =
  res
    "StartServerResponse"
    "fixture/StartServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartServer)

responseStopServer :: StopServerResponse -> TestTree
responseStopServer =
  res
    "StopServerResponse"
    "fixture/StopServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopServer)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseTestIdentityProvider :: TestIdentityProviderResponse -> TestTree
responseTestIdentityProvider =
  res
    "TestIdentityProviderResponse"
    "fixture/TestIdentityProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestIdentityProvider)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateAccess :: UpdateAccessResponse -> TestTree
responseUpdateAccess =
  res
    "UpdateAccessResponse"
    "fixture/UpdateAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAccess)

responseUpdateServer :: UpdateServerResponse -> TestTree
responseUpdateServer =
  res
    "UpdateServerResponse"
    "fixture/UpdateServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServer)

responseUpdateUser :: UpdateUserResponse -> TestTree
responseUpdateUser =
  res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUser)
