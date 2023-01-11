{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Transfer
-- Copyright   : (c) 2013-2023 Brendan Hay
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
--         , requestCreateAgreement $
--             newCreateAgreement
--
--         , requestCreateConnector $
--             newCreateConnector
--
--         , requestCreateProfile $
--             newCreateProfile
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
--         , requestDeleteAgreement $
--             newDeleteAgreement
--
--         , requestDeleteCertificate $
--             newDeleteCertificate
--
--         , requestDeleteConnector $
--             newDeleteConnector
--
--         , requestDeleteHostKey $
--             newDeleteHostKey
--
--         , requestDeleteProfile $
--             newDeleteProfile
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
--         , requestDescribeAgreement $
--             newDescribeAgreement
--
--         , requestDescribeCertificate $
--             newDescribeCertificate
--
--         , requestDescribeConnector $
--             newDescribeConnector
--
--         , requestDescribeExecution $
--             newDescribeExecution
--
--         , requestDescribeHostKey $
--             newDescribeHostKey
--
--         , requestDescribeProfile $
--             newDescribeProfile
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
--         , requestImportCertificate $
--             newImportCertificate
--
--         , requestImportHostKey $
--             newImportHostKey
--
--         , requestImportSshPublicKey $
--             newImportSshPublicKey
--
--         , requestListAccesses $
--             newListAccesses
--
--         , requestListAgreements $
--             newListAgreements
--
--         , requestListCertificates $
--             newListCertificates
--
--         , requestListConnectors $
--             newListConnectors
--
--         , requestListExecutions $
--             newListExecutions
--
--         , requestListHostKeys $
--             newListHostKeys
--
--         , requestListProfiles $
--             newListProfiles
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
--         , requestStartFileTransfer $
--             newStartFileTransfer
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
--         , requestUpdateAgreement $
--             newUpdateAgreement
--
--         , requestUpdateCertificate $
--             newUpdateCertificate
--
--         , requestUpdateConnector $
--             newUpdateConnector
--
--         , requestUpdateHostKey $
--             newUpdateHostKey
--
--         , requestUpdateProfile $
--             newUpdateProfile
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
--         , responseCreateAgreement $
--             newCreateAgreementResponse
--
--         , responseCreateConnector $
--             newCreateConnectorResponse
--
--         , responseCreateProfile $
--             newCreateProfileResponse
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
--         , responseDeleteAgreement $
--             newDeleteAgreementResponse
--
--         , responseDeleteCertificate $
--             newDeleteCertificateResponse
--
--         , responseDeleteConnector $
--             newDeleteConnectorResponse
--
--         , responseDeleteHostKey $
--             newDeleteHostKeyResponse
--
--         , responseDeleteProfile $
--             newDeleteProfileResponse
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
--         , responseDescribeAgreement $
--             newDescribeAgreementResponse
--
--         , responseDescribeCertificate $
--             newDescribeCertificateResponse
--
--         , responseDescribeConnector $
--             newDescribeConnectorResponse
--
--         , responseDescribeExecution $
--             newDescribeExecutionResponse
--
--         , responseDescribeHostKey $
--             newDescribeHostKeyResponse
--
--         , responseDescribeProfile $
--             newDescribeProfileResponse
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
--         , responseImportCertificate $
--             newImportCertificateResponse
--
--         , responseImportHostKey $
--             newImportHostKeyResponse
--
--         , responseImportSshPublicKey $
--             newImportSshPublicKeyResponse
--
--         , responseListAccesses $
--             newListAccessesResponse
--
--         , responseListAgreements $
--             newListAgreementsResponse
--
--         , responseListCertificates $
--             newListCertificatesResponse
--
--         , responseListConnectors $
--             newListConnectorsResponse
--
--         , responseListExecutions $
--             newListExecutionsResponse
--
--         , responseListHostKeys $
--             newListHostKeysResponse
--
--         , responseListProfiles $
--             newListProfilesResponse
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
--         , responseStartFileTransfer $
--             newStartFileTransferResponse
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
--         , responseUpdateAgreement $
--             newUpdateAgreementResponse
--
--         , responseUpdateCertificate $
--             newUpdateCertificateResponse
--
--         , responseUpdateConnector $
--             newUpdateConnectorResponse
--
--         , responseUpdateHostKey $
--             newUpdateHostKeyResponse
--
--         , responseUpdateProfile $
--             newUpdateProfileResponse
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

requestCreateAgreement :: CreateAgreement -> TestTree
requestCreateAgreement =
  req
    "CreateAgreement"
    "fixture/CreateAgreement.yaml"

requestCreateConnector :: CreateConnector -> TestTree
requestCreateConnector =
  req
    "CreateConnector"
    "fixture/CreateConnector.yaml"

requestCreateProfile :: CreateProfile -> TestTree
requestCreateProfile =
  req
    "CreateProfile"
    "fixture/CreateProfile.yaml"

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

requestDeleteAgreement :: DeleteAgreement -> TestTree
requestDeleteAgreement =
  req
    "DeleteAgreement"
    "fixture/DeleteAgreement.yaml"

requestDeleteCertificate :: DeleteCertificate -> TestTree
requestDeleteCertificate =
  req
    "DeleteCertificate"
    "fixture/DeleteCertificate.yaml"

requestDeleteConnector :: DeleteConnector -> TestTree
requestDeleteConnector =
  req
    "DeleteConnector"
    "fixture/DeleteConnector.yaml"

requestDeleteHostKey :: DeleteHostKey -> TestTree
requestDeleteHostKey =
  req
    "DeleteHostKey"
    "fixture/DeleteHostKey.yaml"

requestDeleteProfile :: DeleteProfile -> TestTree
requestDeleteProfile =
  req
    "DeleteProfile"
    "fixture/DeleteProfile.yaml"

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

requestDescribeAgreement :: DescribeAgreement -> TestTree
requestDescribeAgreement =
  req
    "DescribeAgreement"
    "fixture/DescribeAgreement.yaml"

requestDescribeCertificate :: DescribeCertificate -> TestTree
requestDescribeCertificate =
  req
    "DescribeCertificate"
    "fixture/DescribeCertificate.yaml"

requestDescribeConnector :: DescribeConnector -> TestTree
requestDescribeConnector =
  req
    "DescribeConnector"
    "fixture/DescribeConnector.yaml"

requestDescribeExecution :: DescribeExecution -> TestTree
requestDescribeExecution =
  req
    "DescribeExecution"
    "fixture/DescribeExecution.yaml"

requestDescribeHostKey :: DescribeHostKey -> TestTree
requestDescribeHostKey =
  req
    "DescribeHostKey"
    "fixture/DescribeHostKey.yaml"

requestDescribeProfile :: DescribeProfile -> TestTree
requestDescribeProfile =
  req
    "DescribeProfile"
    "fixture/DescribeProfile.yaml"

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

requestImportCertificate :: ImportCertificate -> TestTree
requestImportCertificate =
  req
    "ImportCertificate"
    "fixture/ImportCertificate.yaml"

requestImportHostKey :: ImportHostKey -> TestTree
requestImportHostKey =
  req
    "ImportHostKey"
    "fixture/ImportHostKey.yaml"

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

requestListAgreements :: ListAgreements -> TestTree
requestListAgreements =
  req
    "ListAgreements"
    "fixture/ListAgreements.yaml"

requestListCertificates :: ListCertificates -> TestTree
requestListCertificates =
  req
    "ListCertificates"
    "fixture/ListCertificates.yaml"

requestListConnectors :: ListConnectors -> TestTree
requestListConnectors =
  req
    "ListConnectors"
    "fixture/ListConnectors.yaml"

requestListExecutions :: ListExecutions -> TestTree
requestListExecutions =
  req
    "ListExecutions"
    "fixture/ListExecutions.yaml"

requestListHostKeys :: ListHostKeys -> TestTree
requestListHostKeys =
  req
    "ListHostKeys"
    "fixture/ListHostKeys.yaml"

requestListProfiles :: ListProfiles -> TestTree
requestListProfiles =
  req
    "ListProfiles"
    "fixture/ListProfiles.yaml"

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

requestStartFileTransfer :: StartFileTransfer -> TestTree
requestStartFileTransfer =
  req
    "StartFileTransfer"
    "fixture/StartFileTransfer.yaml"

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

requestUpdateAgreement :: UpdateAgreement -> TestTree
requestUpdateAgreement =
  req
    "UpdateAgreement"
    "fixture/UpdateAgreement.yaml"

requestUpdateCertificate :: UpdateCertificate -> TestTree
requestUpdateCertificate =
  req
    "UpdateCertificate"
    "fixture/UpdateCertificate.yaml"

requestUpdateConnector :: UpdateConnector -> TestTree
requestUpdateConnector =
  req
    "UpdateConnector"
    "fixture/UpdateConnector.yaml"

requestUpdateHostKey :: UpdateHostKey -> TestTree
requestUpdateHostKey =
  req
    "UpdateHostKey"
    "fixture/UpdateHostKey.yaml"

requestUpdateProfile :: UpdateProfile -> TestTree
requestUpdateProfile =
  req
    "UpdateProfile"
    "fixture/UpdateProfile.yaml"

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

responseCreateAgreement :: CreateAgreementResponse -> TestTree
responseCreateAgreement =
  res
    "CreateAgreementResponse"
    "fixture/CreateAgreementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAgreement)

responseCreateConnector :: CreateConnectorResponse -> TestTree
responseCreateConnector =
  res
    "CreateConnectorResponse"
    "fixture/CreateConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConnector)

responseCreateProfile :: CreateProfileResponse -> TestTree
responseCreateProfile =
  res
    "CreateProfileResponse"
    "fixture/CreateProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProfile)

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

responseDeleteAgreement :: DeleteAgreementResponse -> TestTree
responseDeleteAgreement =
  res
    "DeleteAgreementResponse"
    "fixture/DeleteAgreementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAgreement)

responseDeleteCertificate :: DeleteCertificateResponse -> TestTree
responseDeleteCertificate =
  res
    "DeleteCertificateResponse"
    "fixture/DeleteCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCertificate)

responseDeleteConnector :: DeleteConnectorResponse -> TestTree
responseDeleteConnector =
  res
    "DeleteConnectorResponse"
    "fixture/DeleteConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConnector)

responseDeleteHostKey :: DeleteHostKeyResponse -> TestTree
responseDeleteHostKey =
  res
    "DeleteHostKeyResponse"
    "fixture/DeleteHostKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHostKey)

responseDeleteProfile :: DeleteProfileResponse -> TestTree
responseDeleteProfile =
  res
    "DeleteProfileResponse"
    "fixture/DeleteProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProfile)

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

responseDescribeAgreement :: DescribeAgreementResponse -> TestTree
responseDescribeAgreement =
  res
    "DescribeAgreementResponse"
    "fixture/DescribeAgreementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAgreement)

responseDescribeCertificate :: DescribeCertificateResponse -> TestTree
responseDescribeCertificate =
  res
    "DescribeCertificateResponse"
    "fixture/DescribeCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCertificate)

responseDescribeConnector :: DescribeConnectorResponse -> TestTree
responseDescribeConnector =
  res
    "DescribeConnectorResponse"
    "fixture/DescribeConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConnector)

responseDescribeExecution :: DescribeExecutionResponse -> TestTree
responseDescribeExecution =
  res
    "DescribeExecutionResponse"
    "fixture/DescribeExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeExecution)

responseDescribeHostKey :: DescribeHostKeyResponse -> TestTree
responseDescribeHostKey =
  res
    "DescribeHostKeyResponse"
    "fixture/DescribeHostKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHostKey)

responseDescribeProfile :: DescribeProfileResponse -> TestTree
responseDescribeProfile =
  res
    "DescribeProfileResponse"
    "fixture/DescribeProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProfile)

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

responseImportCertificate :: ImportCertificateResponse -> TestTree
responseImportCertificate =
  res
    "ImportCertificateResponse"
    "fixture/ImportCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportCertificate)

responseImportHostKey :: ImportHostKeyResponse -> TestTree
responseImportHostKey =
  res
    "ImportHostKeyResponse"
    "fixture/ImportHostKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportHostKey)

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

responseListAgreements :: ListAgreementsResponse -> TestTree
responseListAgreements =
  res
    "ListAgreementsResponse"
    "fixture/ListAgreementsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAgreements)

responseListCertificates :: ListCertificatesResponse -> TestTree
responseListCertificates =
  res
    "ListCertificatesResponse"
    "fixture/ListCertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCertificates)

responseListConnectors :: ListConnectorsResponse -> TestTree
responseListConnectors =
  res
    "ListConnectorsResponse"
    "fixture/ListConnectorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConnectors)

responseListExecutions :: ListExecutionsResponse -> TestTree
responseListExecutions =
  res
    "ListExecutionsResponse"
    "fixture/ListExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExecutions)

responseListHostKeys :: ListHostKeysResponse -> TestTree
responseListHostKeys =
  res
    "ListHostKeysResponse"
    "fixture/ListHostKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHostKeys)

responseListProfiles :: ListProfilesResponse -> TestTree
responseListProfiles =
  res
    "ListProfilesResponse"
    "fixture/ListProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProfiles)

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

responseStartFileTransfer :: StartFileTransferResponse -> TestTree
responseStartFileTransfer =
  res
    "StartFileTransferResponse"
    "fixture/StartFileTransferResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartFileTransfer)

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

responseUpdateAgreement :: UpdateAgreementResponse -> TestTree
responseUpdateAgreement =
  res
    "UpdateAgreementResponse"
    "fixture/UpdateAgreementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAgreement)

responseUpdateCertificate :: UpdateCertificateResponse -> TestTree
responseUpdateCertificate =
  res
    "UpdateCertificateResponse"
    "fixture/UpdateCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCertificate)

responseUpdateConnector :: UpdateConnectorResponse -> TestTree
responseUpdateConnector =
  res
    "UpdateConnectorResponse"
    "fixture/UpdateConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConnector)

responseUpdateHostKey :: UpdateHostKeyResponse -> TestTree
responseUpdateHostKey =
  res
    "UpdateHostKeyResponse"
    "fixture/UpdateHostKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateHostKey)

responseUpdateProfile :: UpdateProfileResponse -> TestTree
responseUpdateProfile =
  res
    "UpdateProfileResponse"
    "fixture/UpdateProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProfile)

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
