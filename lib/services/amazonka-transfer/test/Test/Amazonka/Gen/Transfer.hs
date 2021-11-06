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
--         [ requestUpdateServer $
--             newUpdateServer
--
--         , requestDeleteServer $
--             newDeleteServer
--
--         , requestCreateWorkflow $
--             newCreateWorkflow
--
--         , requestDeleteSshPublicKey $
--             newDeleteSshPublicKey
--
--         , requestListSecurityPolicies $
--             newListSecurityPolicies
--
--         , requestDeleteWorkflow $
--             newDeleteWorkflow
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestSendWorkflowStepState $
--             newSendWorkflowStepState
--
--         , requestStopServer $
--             newStopServer
--
--         , requestListUsers $
--             newListUsers
--
--         , requestDescribeServer $
--             newDescribeServer
--
--         , requestDescribeSecurityPolicy $
--             newDescribeSecurityPolicy
--
--         , requestImportSshPublicKey $
--             newImportSshPublicKey
--
--         , requestListExecutions $
--             newListExecutions
--
--         , requestCreateServer $
--             newCreateServer
--
--         , requestTestIdentityProvider $
--             newTestIdentityProvider
--
--         , requestListServers $
--             newListServers
--
--         , requestDescribeUser $
--             newDescribeUser
--
--         , requestDescribeExecution $
--             newDescribeExecution
--
--         , requestListWorkflows $
--             newListWorkflows
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestStartServer $
--             newStartServer
--
--         , requestUpdateAccess $
--             newUpdateAccess
--
--         , requestDeleteAccess $
--             newDeleteAccess
--
--         , requestCreateAccess $
--             newCreateAccess
--
--         , requestListAccesses $
--             newListAccesses
--
--         , requestUpdateUser $
--             newUpdateUser
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeWorkflow $
--             newDescribeWorkflow
--
--         , requestDescribeAccess $
--             newDescribeAccess
--
--           ]

--     , testGroup "response"
--         [ responseUpdateServer $
--             newUpdateServerResponse
--
--         , responseDeleteServer $
--             newDeleteServerResponse
--
--         , responseCreateWorkflow $
--             newCreateWorkflowResponse
--
--         , responseDeleteSshPublicKey $
--             newDeleteSshPublicKeyResponse
--
--         , responseListSecurityPolicies $
--             newListSecurityPoliciesResponse
--
--         , responseDeleteWorkflow $
--             newDeleteWorkflowResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseSendWorkflowStepState $
--             newSendWorkflowStepStateResponse
--
--         , responseStopServer $
--             newStopServerResponse
--
--         , responseListUsers $
--             newListUsersResponse
--
--         , responseDescribeServer $
--             newDescribeServerResponse
--
--         , responseDescribeSecurityPolicy $
--             newDescribeSecurityPolicyResponse
--
--         , responseImportSshPublicKey $
--             newImportSshPublicKeyResponse
--
--         , responseListExecutions $
--             newListExecutionsResponse
--
--         , responseCreateServer $
--             newCreateServerResponse
--
--         , responseTestIdentityProvider $
--             newTestIdentityProviderResponse
--
--         , responseListServers $
--             newListServersResponse
--
--         , responseDescribeUser $
--             newDescribeUserResponse
--
--         , responseDescribeExecution $
--             newDescribeExecutionResponse
--
--         , responseListWorkflows $
--             newListWorkflowsResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseStartServer $
--             newStartServerResponse
--
--         , responseUpdateAccess $
--             newUpdateAccessResponse
--
--         , responseDeleteAccess $
--             newDeleteAccessResponse
--
--         , responseCreateAccess $
--             newCreateAccessResponse
--
--         , responseListAccesses $
--             newListAccessesResponse
--
--         , responseUpdateUser $
--             newUpdateUserResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeWorkflow $
--             newDescribeWorkflowResponse
--
--         , responseDescribeAccess $
--             newDescribeAccessResponse
--
--           ]
--     ]

-- Requests

requestUpdateServer :: UpdateServer -> TestTree
requestUpdateServer =
  req
    "UpdateServer"
    "fixture/UpdateServer.yaml"

requestDeleteServer :: DeleteServer -> TestTree
requestDeleteServer =
  req
    "DeleteServer"
    "fixture/DeleteServer.yaml"

requestCreateWorkflow :: CreateWorkflow -> TestTree
requestCreateWorkflow =
  req
    "CreateWorkflow"
    "fixture/CreateWorkflow.yaml"

requestDeleteSshPublicKey :: DeleteSshPublicKey -> TestTree
requestDeleteSshPublicKey =
  req
    "DeleteSshPublicKey"
    "fixture/DeleteSshPublicKey.yaml"

requestListSecurityPolicies :: ListSecurityPolicies -> TestTree
requestListSecurityPolicies =
  req
    "ListSecurityPolicies"
    "fixture/ListSecurityPolicies.yaml"

requestDeleteWorkflow :: DeleteWorkflow -> TestTree
requestDeleteWorkflow =
  req
    "DeleteWorkflow"
    "fixture/DeleteWorkflow.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestSendWorkflowStepState :: SendWorkflowStepState -> TestTree
requestSendWorkflowStepState =
  req
    "SendWorkflowStepState"
    "fixture/SendWorkflowStepState.yaml"

requestStopServer :: StopServer -> TestTree
requestStopServer =
  req
    "StopServer"
    "fixture/StopServer.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers =
  req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestDescribeServer :: DescribeServer -> TestTree
requestDescribeServer =
  req
    "DescribeServer"
    "fixture/DescribeServer.yaml"

requestDescribeSecurityPolicy :: DescribeSecurityPolicy -> TestTree
requestDescribeSecurityPolicy =
  req
    "DescribeSecurityPolicy"
    "fixture/DescribeSecurityPolicy.yaml"

requestImportSshPublicKey :: ImportSshPublicKey -> TestTree
requestImportSshPublicKey =
  req
    "ImportSshPublicKey"
    "fixture/ImportSshPublicKey.yaml"

requestListExecutions :: ListExecutions -> TestTree
requestListExecutions =
  req
    "ListExecutions"
    "fixture/ListExecutions.yaml"

requestCreateServer :: CreateServer -> TestTree
requestCreateServer =
  req
    "CreateServer"
    "fixture/CreateServer.yaml"

requestTestIdentityProvider :: TestIdentityProvider -> TestTree
requestTestIdentityProvider =
  req
    "TestIdentityProvider"
    "fixture/TestIdentityProvider.yaml"

requestListServers :: ListServers -> TestTree
requestListServers =
  req
    "ListServers"
    "fixture/ListServers.yaml"

requestDescribeUser :: DescribeUser -> TestTree
requestDescribeUser =
  req
    "DescribeUser"
    "fixture/DescribeUser.yaml"

requestDescribeExecution :: DescribeExecution -> TestTree
requestDescribeExecution =
  req
    "DescribeExecution"
    "fixture/DescribeExecution.yaml"

requestListWorkflows :: ListWorkflows -> TestTree
requestListWorkflows =
  req
    "ListWorkflows"
    "fixture/ListWorkflows.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestStartServer :: StartServer -> TestTree
requestStartServer =
  req
    "StartServer"
    "fixture/StartServer.yaml"

requestUpdateAccess :: UpdateAccess -> TestTree
requestUpdateAccess =
  req
    "UpdateAccess"
    "fixture/UpdateAccess.yaml"

requestDeleteAccess :: DeleteAccess -> TestTree
requestDeleteAccess =
  req
    "DeleteAccess"
    "fixture/DeleteAccess.yaml"

requestCreateAccess :: CreateAccess -> TestTree
requestCreateAccess =
  req
    "CreateAccess"
    "fixture/CreateAccess.yaml"

requestListAccesses :: ListAccesses -> TestTree
requestListAccesses =
  req
    "ListAccesses"
    "fixture/ListAccesses.yaml"

requestUpdateUser :: UpdateUser -> TestTree
requestUpdateUser =
  req
    "UpdateUser"
    "fixture/UpdateUser.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

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

requestDescribeWorkflow :: DescribeWorkflow -> TestTree
requestDescribeWorkflow =
  req
    "DescribeWorkflow"
    "fixture/DescribeWorkflow.yaml"

requestDescribeAccess :: DescribeAccess -> TestTree
requestDescribeAccess =
  req
    "DescribeAccess"
    "fixture/DescribeAccess.yaml"

-- Responses

responseUpdateServer :: UpdateServerResponse -> TestTree
responseUpdateServer =
  res
    "UpdateServerResponse"
    "fixture/UpdateServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServer)

responseDeleteServer :: DeleteServerResponse -> TestTree
responseDeleteServer =
  res
    "DeleteServerResponse"
    "fixture/DeleteServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteServer)

responseCreateWorkflow :: CreateWorkflowResponse -> TestTree
responseCreateWorkflow =
  res
    "CreateWorkflowResponse"
    "fixture/CreateWorkflowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkflow)

responseDeleteSshPublicKey :: DeleteSshPublicKeyResponse -> TestTree
responseDeleteSshPublicKey =
  res
    "DeleteSshPublicKeyResponse"
    "fixture/DeleteSshPublicKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSshPublicKey)

responseListSecurityPolicies :: ListSecurityPoliciesResponse -> TestTree
responseListSecurityPolicies =
  res
    "ListSecurityPoliciesResponse"
    "fixture/ListSecurityPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSecurityPolicies)

responseDeleteWorkflow :: DeleteWorkflowResponse -> TestTree
responseDeleteWorkflow =
  res
    "DeleteWorkflowResponse"
    "fixture/DeleteWorkflowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkflow)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseSendWorkflowStepState :: SendWorkflowStepStateResponse -> TestTree
responseSendWorkflowStepState =
  res
    "SendWorkflowStepStateResponse"
    "fixture/SendWorkflowStepStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendWorkflowStepState)

responseStopServer :: StopServerResponse -> TestTree
responseStopServer =
  res
    "StopServerResponse"
    "fixture/StopServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopServer)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUsers)

responseDescribeServer :: DescribeServerResponse -> TestTree
responseDescribeServer =
  res
    "DescribeServerResponse"
    "fixture/DescribeServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeServer)

responseDescribeSecurityPolicy :: DescribeSecurityPolicyResponse -> TestTree
responseDescribeSecurityPolicy =
  res
    "DescribeSecurityPolicyResponse"
    "fixture/DescribeSecurityPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSecurityPolicy)

responseImportSshPublicKey :: ImportSshPublicKeyResponse -> TestTree
responseImportSshPublicKey =
  res
    "ImportSshPublicKeyResponse"
    "fixture/ImportSshPublicKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportSshPublicKey)

responseListExecutions :: ListExecutionsResponse -> TestTree
responseListExecutions =
  res
    "ListExecutionsResponse"
    "fixture/ListExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExecutions)

responseCreateServer :: CreateServerResponse -> TestTree
responseCreateServer =
  res
    "CreateServerResponse"
    "fixture/CreateServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateServer)

responseTestIdentityProvider :: TestIdentityProviderResponse -> TestTree
responseTestIdentityProvider =
  res
    "TestIdentityProviderResponse"
    "fixture/TestIdentityProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestIdentityProvider)

responseListServers :: ListServersResponse -> TestTree
responseListServers =
  res
    "ListServersResponse"
    "fixture/ListServersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServers)

responseDescribeUser :: DescribeUserResponse -> TestTree
responseDescribeUser =
  res
    "DescribeUserResponse"
    "fixture/DescribeUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUser)

responseDescribeExecution :: DescribeExecutionResponse -> TestTree
responseDescribeExecution =
  res
    "DescribeExecutionResponse"
    "fixture/DescribeExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeExecution)

responseListWorkflows :: ListWorkflowsResponse -> TestTree
responseListWorkflows =
  res
    "ListWorkflowsResponse"
    "fixture/ListWorkflowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkflows)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUser)

responseStartServer :: StartServerResponse -> TestTree
responseStartServer =
  res
    "StartServerResponse"
    "fixture/StartServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartServer)

responseUpdateAccess :: UpdateAccessResponse -> TestTree
responseUpdateAccess =
  res
    "UpdateAccessResponse"
    "fixture/UpdateAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAccess)

responseDeleteAccess :: DeleteAccessResponse -> TestTree
responseDeleteAccess =
  res
    "DeleteAccessResponse"
    "fixture/DeleteAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccess)

responseCreateAccess :: CreateAccessResponse -> TestTree
responseCreateAccess =
  res
    "CreateAccessResponse"
    "fixture/CreateAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAccess)

responseListAccesses :: ListAccessesResponse -> TestTree
responseListAccesses =
  res
    "ListAccessesResponse"
    "fixture/ListAccessesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccesses)

responseUpdateUser :: UpdateUserResponse -> TestTree
responseUpdateUser =
  res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUser)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUser)

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

responseDescribeWorkflow :: DescribeWorkflowResponse -> TestTree
responseDescribeWorkflow =
  res
    "DescribeWorkflowResponse"
    "fixture/DescribeWorkflowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkflow)

responseDescribeAccess :: DescribeAccessResponse -> TestTree
responseDescribeAccess =
  res
    "DescribeAccessResponse"
    "fixture/DescribeAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccess)
