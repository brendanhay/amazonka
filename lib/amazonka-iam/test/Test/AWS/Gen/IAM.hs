{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.IAM
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.IAM where

import Data.Proxy
import Network.AWS.IAM
import Test.AWS.Fixture
import Test.AWS.IAM.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetContextKeysForPrincipalPolicy $
--             getContextKeysForPrincipalPolicy
--
--         , requestListPolicies $
--             listPolicies
--
--         , requestCreatePolicy $
--             createPolicy
--
--         , requestListInstanceProfilesForRole $
--             listInstanceProfilesForRole
--
--         , requestAttachGroupPolicy $
--             attachGroupPolicy
--
--         , requestCreateAccessKey $
--             createAccessKey
--
--         , requestListSSHPublicKeys $
--             listSSHPublicKeys
--
--         , requestListOpenIdConnectProviders $
--             listOpenIdConnectProviders
--
--         , requestCreateVirtualMFADevice $
--             createVirtualMFADevice
--
--         , requestDeleteAccountPasswordPolicy $
--             deleteAccountPasswordPolicy
--
--         , requestUpdateAccountPasswordPolicy $
--             updateAccountPasswordPolicy
--
--         , requestAttachRolePolicy $
--             attachRolePolicy
--
--         , requestUpdateSSHPublicKey $
--             updateSSHPublicKey
--
--         , requestDeleteSSHPublicKey $
--             deleteSSHPublicKey
--
--         , requestGetUserPolicy $
--             getUserPolicy
--
--         , requestUpdateServiceSpecificCredential $
--             updateServiceSpecificCredential
--
--         , requestDeleteServiceSpecificCredential $
--             deleteServiceSpecificCredential
--
--         , requestListAttachedRolePolicies $
--             listAttachedRolePolicies
--
--         , requestGetRole $
--             getRole
--
--         , requestDeactivateMFADevice $
--             deactivateMFADevice
--
--         , requestCreateOpenIdConnectProvider $
--             createOpenIdConnectProvider
--
--         , requestDeleteVirtualMFADevice $
--             deleteVirtualMFADevice
--
--         , requestListRoles $
--             listRoles
--
--         , requestListUserPolicies $
--             listUserPolicies
--
--         , requestUploadSSHPublicKey $
--             uploadSSHPublicKey
--
--         , requestSimulateCustomPolicy $
--             simulateCustomPolicy
--
--         , requestUpdateRole $
--             updateRole
--
--         , requestDeleteRole $
--             deleteRole
--
--         , requestListUsers $
--             listUsers
--
--         , requestUpdateOpenIdConnectProviderThumbprint $
--             updateOpenIdConnectProviderThumbprint
--
--         , requestPutUserPolicy $
--             putUserPolicy
--
--         , requestGetSSHPublicKey $
--             getSSHPublicKey
--
--         , requestDetachGroupPolicy $
--             detachGroupPolicy
--
--         , requestGetOpenIdConnectProvider $
--             getOpenIdConnectProvider
--
--         , requestDeleteUserPolicy $
--             deleteUserPolicy
--
--         , requestCreateRole $
--             createRole
--
--         , requestResetServiceSpecificCredential $
--             resetServiceSpecificCredential
--
--         , requestGetCredentialReport $
--             getCredentialReport
--
--         , requestGetAccountSummary $
--             getAccountSummary
--
--         , requestListGroupPolicies $
--             listGroupPolicies
--
--         , requestDeletePolicyVersion $
--             deletePolicyVersion
--
--         , requestDeleteInstanceProfile $
--             deleteInstanceProfile
--
--         , requestDetachRolePolicy $
--             detachRolePolicy
--
--         , requestRemoveRoleFromInstanceProfile $
--             removeRoleFromInstanceProfile
--
--         , requestCreatePolicyVersion $
--             createPolicyVersion
--
--         , requestCreateInstanceProfile $
--             createInstanceProfile
--
--         , requestCreateSAMLProvider $
--             createSAMLProvider
--
--         , requestGetAccountAuthorizationDetails $
--             getAccountAuthorizationDetails
--
--         , requestGetServiceLinkedRoleDeletionStatus $
--             getServiceLinkedRoleDeletionStatus
--
--         , requestDeleteAccountAlias $
--             deleteAccountAlias
--
--         , requestDetachUserPolicy $
--             detachUserPolicy
--
--         , requestRemoveUserFromGroup $
--             removeUserFromGroup
--
--         , requestDeleteGroupPolicy $
--             deleteGroupPolicy
--
--         , requestPutGroupPolicy $
--             putGroupPolicy
--
--         , requestGetLoginProfile $
--             getLoginProfile
--
--         , requestGetGroupPolicy $
--             getGroupPolicy
--
--         , requestChangePassword $
--             changePassword
--
--         , requestListServerCertificates $
--             listServerCertificates
--
--         , requestDeleteServiceLinkedRole $
--             deleteServiceLinkedRole
--
--         , requestDeletePolicy $
--             deletePolicy
--
--         , requestUpdateAssumeRolePolicy $
--             updateAssumeRolePolicy
--
--         , requestGetInstanceProfile $
--             getInstanceProfile
--
--         , requestCreateLoginProfile $
--             createLoginProfile
--
--         , requestGetSAMLProvider $
--             getSAMLProvider
--
--         , requestAddRoleToInstanceProfile $
--             addRoleToInstanceProfile
--
--         , requestListGroupsForUser $
--             listGroupsForUser
--
--         , requestListEntitiesForPolicy $
--             listEntitiesForPolicy
--
--         , requestAddUserToGroup $
--             addUserToGroup
--
--         , requestSimulatePrincipalPolicy $
--             simulatePrincipalPolicy
--
--         , requestGetPolicyVersion $
--             getPolicyVersion
--
--         , requestCreateServiceLinkedRole $
--             createServiceLinkedRole
--
--         , requestListServiceSpecificCredentials $
--             listServiceSpecificCredentials
--
--         , requestDeleteOpenIdConnectProvider $
--             deleteOpenIdConnectProvider
--
--         , requestGetUser $
--             getUser
--
--         , requestListSigningCertificates $
--             listSigningCertificates
--
--         , requestDeleteSigningCertificate $
--             deleteSigningCertificate
--
--         , requestUpdateSigningCertificate $
--             updateSigningCertificate
--
--         , requestListAttachedUserPolicies $
--             listAttachedUserPolicies
--
--         , requestRemoveClientIdFromOpenIdConnectProvider $
--             removeClientIdFromOpenIdConnectProvider
--
--         , requestAttachUserPolicy $
--             attachUserPolicy
--
--         , requestCreateServiceSpecificCredential $
--             createServiceSpecificCredential
--
--         , requestListVirtualMFADevices $
--             listVirtualMFADevices
--
--         , requestResyncMFADevice $
--             resyncMFADevice
--
--         , requestDeleteAccessKey $
--             deleteAccessKey
--
--         , requestUpdateAccessKey $
--             updateAccessKey
--
--         , requestListAccessKeys $
--             listAccessKeys
--
--         , requestGetRolePolicy $
--             getRolePolicy
--
--         , requestCreateUser $
--             createUser
--
--         , requestPutRolePolicy $
--             putRolePolicy
--
--         , requestGetContextKeysForCustomPolicy $
--             getContextKeysForCustomPolicy
--
--         , requestUploadSigningCertificate $
--             uploadSigningCertificate
--
--         , requestDeleteRolePolicy $
--             deleteRolePolicy
--
--         , requestGetAccountPasswordPolicy $
--             getAccountPasswordPolicy
--
--         , requestGetAccessKeyLastUsed $
--             getAccessKeyLastUsed
--
--         , requestUpdateUser $
--             updateUser
--
--         , requestDeleteUser $
--             deleteUser
--
--         , requestAddClientIdToOpenIdConnectProvider $
--             addClientIdToOpenIdConnectProvider
--
--         , requestListRolePolicies $
--             listRolePolicies
--
--         , requestCreateAccountAlias $
--             createAccountAlias
--
--         , requestListInstanceProfiles $
--             listInstanceProfiles
--
--         , requestEnableMFADevice $
--             enableMFADevice
--
--         , requestListAccountAliases $
--             listAccountAliases
--
--         , requestDeleteSAMLProvider $
--             deleteSAMLProvider
--
--         , requestUpdateSAMLProvider $
--             updateSAMLProvider
--
--         , requestCreateGroup $
--             createGroup
--
--         , requestListMFADevices $
--             listMFADevices
--
--         , requestUploadServerCertificate $
--             uploadServerCertificate
--
--         , requestSetDefaultPolicyVersion $
--             setDefaultPolicyVersion
--
--         , requestListPolicyVersions $
--             listPolicyVersions
--
--         , requestUpdateRoleDescription $
--             updateRoleDescription
--
--         , requestListSAMLProviders $
--             listSAMLProviders
--
--         , requestGetServerCertificate $
--             getServerCertificate
--
--         , requestDeleteGroup $
--             deleteGroup
--
--         , requestUpdateGroup $
--             updateGroup
--
--         , requestListGroups $
--             listGroups
--
--         , requestGenerateCredentialReport $
--             generateCredentialReport
--
--         , requestGetPolicy $
--             getPolicy
--
--         , requestUpdateLoginProfile $
--             updateLoginProfile
--
--         , requestDeleteLoginProfile $
--             deleteLoginProfile
--
--         , requestGetGroup $
--             getGroup
--
--         , requestDeleteServerCertificate $
--             deleteServerCertificate
--
--         , requestUpdateServerCertificate $
--             updateServerCertificate
--
--         , requestListAttachedGroupPolicies $
--             listAttachedGroupPolicies
--
--           ]

--     , testGroup "response"
--         [ responseGetContextKeysForPrincipalPolicy $
--             getContextKeysForPolicyResponse
--
--         , responseListPolicies $
--             listPoliciesResponse
--
--         , responseCreatePolicy $
--             createPolicyResponse
--
--         , responseListInstanceProfilesForRole $
--             listInstanceProfilesForRoleResponse
--
--         , responseAttachGroupPolicy $
--             attachGroupPolicyResponse
--
--         , responseCreateAccessKey $
--             createAccessKeyResponse
--
--         , responseListSSHPublicKeys $
--             listSSHPublicKeysResponse
--
--         , responseListOpenIdConnectProviders $
--             listOpenIdConnectProvidersResponse
--
--         , responseCreateVirtualMFADevice $
--             createVirtualMFADeviceResponse
--
--         , responseDeleteAccountPasswordPolicy $
--             deleteAccountPasswordPolicyResponse
--
--         , responseUpdateAccountPasswordPolicy $
--             updateAccountPasswordPolicyResponse
--
--         , responseAttachRolePolicy $
--             attachRolePolicyResponse
--
--         , responseUpdateSSHPublicKey $
--             updateSSHPublicKeyResponse
--
--         , responseDeleteSSHPublicKey $
--             deleteSSHPublicKeyResponse
--
--         , responseGetUserPolicy $
--             getUserPolicyResponse
--
--         , responseUpdateServiceSpecificCredential $
--             updateServiceSpecificCredentialResponse
--
--         , responseDeleteServiceSpecificCredential $
--             deleteServiceSpecificCredentialResponse
--
--         , responseListAttachedRolePolicies $
--             listAttachedRolePoliciesResponse
--
--         , responseGetRole $
--             getRoleResponse
--
--         , responseDeactivateMFADevice $
--             deactivateMFADeviceResponse
--
--         , responseCreateOpenIdConnectProvider $
--             createOpenIdConnectProviderResponse
--
--         , responseDeleteVirtualMFADevice $
--             deleteVirtualMFADeviceResponse
--
--         , responseListRoles $
--             listRolesResponse
--
--         , responseListUserPolicies $
--             listUserPoliciesResponse
--
--         , responseUploadSSHPublicKey $
--             uploadSSHPublicKeyResponse
--
--         , responseSimulateCustomPolicy $
--             simulatePolicyResponse
--
--         , responseUpdateRole $
--             updateRoleResponse
--
--         , responseDeleteRole $
--             deleteRoleResponse
--
--         , responseListUsers $
--             listUsersResponse
--
--         , responseUpdateOpenIdConnectProviderThumbprint $
--             updateOpenIdConnectProviderThumbprintResponse
--
--         , responsePutUserPolicy $
--             putUserPolicyResponse
--
--         , responseGetSSHPublicKey $
--             getSSHPublicKeyResponse
--
--         , responseDetachGroupPolicy $
--             detachGroupPolicyResponse
--
--         , responseGetOpenIdConnectProvider $
--             getOpenIdConnectProviderResponse
--
--         , responseDeleteUserPolicy $
--             deleteUserPolicyResponse
--
--         , responseCreateRole $
--             createRoleResponse
--
--         , responseResetServiceSpecificCredential $
--             resetServiceSpecificCredentialResponse
--
--         , responseGetCredentialReport $
--             getCredentialReportResponse
--
--         , responseGetAccountSummary $
--             getAccountSummaryResponse
--
--         , responseListGroupPolicies $
--             listGroupPoliciesResponse
--
--         , responseDeletePolicyVersion $
--             deletePolicyVersionResponse
--
--         , responseDeleteInstanceProfile $
--             deleteInstanceProfileResponse
--
--         , responseDetachRolePolicy $
--             detachRolePolicyResponse
--
--         , responseRemoveRoleFromInstanceProfile $
--             removeRoleFromInstanceProfileResponse
--
--         , responseCreatePolicyVersion $
--             createPolicyVersionResponse
--
--         , responseCreateInstanceProfile $
--             createInstanceProfileResponse
--
--         , responseCreateSAMLProvider $
--             createSAMLProviderResponse
--
--         , responseGetAccountAuthorizationDetails $
--             getAccountAuthorizationDetailsResponse
--
--         , responseGetServiceLinkedRoleDeletionStatus $
--             getServiceLinkedRoleDeletionStatusResponse
--
--         , responseDeleteAccountAlias $
--             deleteAccountAliasResponse
--
--         , responseDetachUserPolicy $
--             detachUserPolicyResponse
--
--         , responseRemoveUserFromGroup $
--             removeUserFromGroupResponse
--
--         , responseDeleteGroupPolicy $
--             deleteGroupPolicyResponse
--
--         , responsePutGroupPolicy $
--             putGroupPolicyResponse
--
--         , responseGetLoginProfile $
--             getLoginProfileResponse
--
--         , responseGetGroupPolicy $
--             getGroupPolicyResponse
--
--         , responseChangePassword $
--             changePasswordResponse
--
--         , responseListServerCertificates $
--             listServerCertificatesResponse
--
--         , responseDeleteServiceLinkedRole $
--             deleteServiceLinkedRoleResponse
--
--         , responseDeletePolicy $
--             deletePolicyResponse
--
--         , responseUpdateAssumeRolePolicy $
--             updateAssumeRolePolicyResponse
--
--         , responseGetInstanceProfile $
--             getInstanceProfileResponse
--
--         , responseCreateLoginProfile $
--             createLoginProfileResponse
--
--         , responseGetSAMLProvider $
--             getSAMLProviderResponse
--
--         , responseAddRoleToInstanceProfile $
--             addRoleToInstanceProfileResponse
--
--         , responseListGroupsForUser $
--             listGroupsForUserResponse
--
--         , responseListEntitiesForPolicy $
--             listEntitiesForPolicyResponse
--
--         , responseAddUserToGroup $
--             addUserToGroupResponse
--
--         , responseSimulatePrincipalPolicy $
--             simulatePolicyResponse
--
--         , responseGetPolicyVersion $
--             getPolicyVersionResponse
--
--         , responseCreateServiceLinkedRole $
--             createServiceLinkedRoleResponse
--
--         , responseListServiceSpecificCredentials $
--             listServiceSpecificCredentialsResponse
--
--         , responseDeleteOpenIdConnectProvider $
--             deleteOpenIdConnectProviderResponse
--
--         , responseGetUser $
--             getUserResponse
--
--         , responseListSigningCertificates $
--             listSigningCertificatesResponse
--
--         , responseDeleteSigningCertificate $
--             deleteSigningCertificateResponse
--
--         , responseUpdateSigningCertificate $
--             updateSigningCertificateResponse
--
--         , responseListAttachedUserPolicies $
--             listAttachedUserPoliciesResponse
--
--         , responseRemoveClientIdFromOpenIdConnectProvider $
--             removeClientIdFromOpenIdConnectProviderResponse
--
--         , responseAttachUserPolicy $
--             attachUserPolicyResponse
--
--         , responseCreateServiceSpecificCredential $
--             createServiceSpecificCredentialResponse
--
--         , responseListVirtualMFADevices $
--             listVirtualMFADevicesResponse
--
--         , responseResyncMFADevice $
--             resyncMFADeviceResponse
--
--         , responseDeleteAccessKey $
--             deleteAccessKeyResponse
--
--         , responseUpdateAccessKey $
--             updateAccessKeyResponse
--
--         , responseListAccessKeys $
--             listAccessKeysResponse
--
--         , responseGetRolePolicy $
--             getRolePolicyResponse
--
--         , responseCreateUser $
--             createUserResponse
--
--         , responsePutRolePolicy $
--             putRolePolicyResponse
--
--         , responseGetContextKeysForCustomPolicy $
--             getContextKeysForPolicyResponse
--
--         , responseUploadSigningCertificate $
--             uploadSigningCertificateResponse
--
--         , responseDeleteRolePolicy $
--             deleteRolePolicyResponse
--
--         , responseGetAccountPasswordPolicy $
--             getAccountPasswordPolicyResponse
--
--         , responseGetAccessKeyLastUsed $
--             getAccessKeyLastUsedResponse
--
--         , responseUpdateUser $
--             updateUserResponse
--
--         , responseDeleteUser $
--             deleteUserResponse
--
--         , responseAddClientIdToOpenIdConnectProvider $
--             addClientIdToOpenIdConnectProviderResponse
--
--         , responseListRolePolicies $
--             listRolePoliciesResponse
--
--         , responseCreateAccountAlias $
--             createAccountAliasResponse
--
--         , responseListInstanceProfiles $
--             listInstanceProfilesResponse
--
--         , responseEnableMFADevice $
--             enableMFADeviceResponse
--
--         , responseListAccountAliases $
--             listAccountAliasesResponse
--
--         , responseDeleteSAMLProvider $
--             deleteSAMLProviderResponse
--
--         , responseUpdateSAMLProvider $
--             updateSAMLProviderResponse
--
--         , responseCreateGroup $
--             createGroupResponse
--
--         , responseListMFADevices $
--             listMFADevicesResponse
--
--         , responseUploadServerCertificate $
--             uploadServerCertificateResponse
--
--         , responseSetDefaultPolicyVersion $
--             setDefaultPolicyVersionResponse
--
--         , responseListPolicyVersions $
--             listPolicyVersionsResponse
--
--         , responseUpdateRoleDescription $
--             updateRoleDescriptionResponse
--
--         , responseListSAMLProviders $
--             listSAMLProvidersResponse
--
--         , responseGetServerCertificate $
--             getServerCertificateResponse
--
--         , responseDeleteGroup $
--             deleteGroupResponse
--
--         , responseUpdateGroup $
--             updateGroupResponse
--
--         , responseListGroups $
--             listGroupsResponse
--
--         , responseGenerateCredentialReport $
--             generateCredentialReportResponse
--
--         , responseGetPolicy $
--             getPolicyResponse
--
--         , responseUpdateLoginProfile $
--             updateLoginProfileResponse
--
--         , responseDeleteLoginProfile $
--             deleteLoginProfileResponse
--
--         , responseGetGroup $
--             getGroupResponse
--
--         , responseDeleteServerCertificate $
--             deleteServerCertificateResponse
--
--         , responseUpdateServerCertificate $
--             updateServerCertificateResponse
--
--         , responseListAttachedGroupPolicies $
--             listAttachedGroupPoliciesResponse
--
--           ]
--     ]

-- Requests

requestGetContextKeysForPrincipalPolicy :: GetContextKeysForPrincipalPolicy -> TestTree
requestGetContextKeysForPrincipalPolicy = req
    "GetContextKeysForPrincipalPolicy"
    "fixture/GetContextKeysForPrincipalPolicy.yaml"

requestListPolicies :: ListPolicies -> TestTree
requestListPolicies = req
    "ListPolicies"
    "fixture/ListPolicies.yaml"

requestCreatePolicy :: CreatePolicy -> TestTree
requestCreatePolicy = req
    "CreatePolicy"
    "fixture/CreatePolicy.yaml"

requestListInstanceProfilesForRole :: ListInstanceProfilesForRole -> TestTree
requestListInstanceProfilesForRole = req
    "ListInstanceProfilesForRole"
    "fixture/ListInstanceProfilesForRole.yaml"

requestAttachGroupPolicy :: AttachGroupPolicy -> TestTree
requestAttachGroupPolicy = req
    "AttachGroupPolicy"
    "fixture/AttachGroupPolicy.yaml"

requestCreateAccessKey :: CreateAccessKey -> TestTree
requestCreateAccessKey = req
    "CreateAccessKey"
    "fixture/CreateAccessKey.yaml"

requestListSSHPublicKeys :: ListSSHPublicKeys -> TestTree
requestListSSHPublicKeys = req
    "ListSSHPublicKeys"
    "fixture/ListSSHPublicKeys.yaml"

requestListOpenIdConnectProviders :: ListOpenIdConnectProviders -> TestTree
requestListOpenIdConnectProviders = req
    "ListOpenIdConnectProviders"
    "fixture/ListOpenIdConnectProviders.yaml"

requestCreateVirtualMFADevice :: CreateVirtualMFADevice -> TestTree
requestCreateVirtualMFADevice = req
    "CreateVirtualMFADevice"
    "fixture/CreateVirtualMFADevice.yaml"

requestDeleteAccountPasswordPolicy :: DeleteAccountPasswordPolicy -> TestTree
requestDeleteAccountPasswordPolicy = req
    "DeleteAccountPasswordPolicy"
    "fixture/DeleteAccountPasswordPolicy.yaml"

requestUpdateAccountPasswordPolicy :: UpdateAccountPasswordPolicy -> TestTree
requestUpdateAccountPasswordPolicy = req
    "UpdateAccountPasswordPolicy"
    "fixture/UpdateAccountPasswordPolicy.yaml"

requestAttachRolePolicy :: AttachRolePolicy -> TestTree
requestAttachRolePolicy = req
    "AttachRolePolicy"
    "fixture/AttachRolePolicy.yaml"

requestUpdateSSHPublicKey :: UpdateSSHPublicKey -> TestTree
requestUpdateSSHPublicKey = req
    "UpdateSSHPublicKey"
    "fixture/UpdateSSHPublicKey.yaml"

requestDeleteSSHPublicKey :: DeleteSSHPublicKey -> TestTree
requestDeleteSSHPublicKey = req
    "DeleteSSHPublicKey"
    "fixture/DeleteSSHPublicKey.yaml"

requestGetUserPolicy :: GetUserPolicy -> TestTree
requestGetUserPolicy = req
    "GetUserPolicy"
    "fixture/GetUserPolicy.yaml"

requestUpdateServiceSpecificCredential :: UpdateServiceSpecificCredential -> TestTree
requestUpdateServiceSpecificCredential = req
    "UpdateServiceSpecificCredential"
    "fixture/UpdateServiceSpecificCredential.yaml"

requestDeleteServiceSpecificCredential :: DeleteServiceSpecificCredential -> TestTree
requestDeleteServiceSpecificCredential = req
    "DeleteServiceSpecificCredential"
    "fixture/DeleteServiceSpecificCredential.yaml"

requestListAttachedRolePolicies :: ListAttachedRolePolicies -> TestTree
requestListAttachedRolePolicies = req
    "ListAttachedRolePolicies"
    "fixture/ListAttachedRolePolicies.yaml"

requestGetRole :: GetRole -> TestTree
requestGetRole = req
    "GetRole"
    "fixture/GetRole.yaml"

requestDeactivateMFADevice :: DeactivateMFADevice -> TestTree
requestDeactivateMFADevice = req
    "DeactivateMFADevice"
    "fixture/DeactivateMFADevice.yaml"

requestCreateOpenIdConnectProvider :: CreateOpenIdConnectProvider -> TestTree
requestCreateOpenIdConnectProvider = req
    "CreateOpenIdConnectProvider"
    "fixture/CreateOpenIdConnectProvider.yaml"

requestDeleteVirtualMFADevice :: DeleteVirtualMFADevice -> TestTree
requestDeleteVirtualMFADevice = req
    "DeleteVirtualMFADevice"
    "fixture/DeleteVirtualMFADevice.yaml"

requestListRoles :: ListRoles -> TestTree
requestListRoles = req
    "ListRoles"
    "fixture/ListRoles.yaml"

requestListUserPolicies :: ListUserPolicies -> TestTree
requestListUserPolicies = req
    "ListUserPolicies"
    "fixture/ListUserPolicies.yaml"

requestUploadSSHPublicKey :: UploadSSHPublicKey -> TestTree
requestUploadSSHPublicKey = req
    "UploadSSHPublicKey"
    "fixture/UploadSSHPublicKey.yaml"

requestSimulateCustomPolicy :: SimulateCustomPolicy -> TestTree
requestSimulateCustomPolicy = req
    "SimulateCustomPolicy"
    "fixture/SimulateCustomPolicy.yaml"

requestUpdateRole :: UpdateRole -> TestTree
requestUpdateRole = req
    "UpdateRole"
    "fixture/UpdateRole.yaml"

requestDeleteRole :: DeleteRole -> TestTree
requestDeleteRole = req
    "DeleteRole"
    "fixture/DeleteRole.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers = req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestUpdateOpenIdConnectProviderThumbprint :: UpdateOpenIdConnectProviderThumbprint -> TestTree
requestUpdateOpenIdConnectProviderThumbprint = req
    "UpdateOpenIdConnectProviderThumbprint"
    "fixture/UpdateOpenIdConnectProviderThumbprint.yaml"

requestPutUserPolicy :: PutUserPolicy -> TestTree
requestPutUserPolicy = req
    "PutUserPolicy"
    "fixture/PutUserPolicy.yaml"

requestGetSSHPublicKey :: GetSSHPublicKey -> TestTree
requestGetSSHPublicKey = req
    "GetSSHPublicKey"
    "fixture/GetSSHPublicKey.yaml"

requestDetachGroupPolicy :: DetachGroupPolicy -> TestTree
requestDetachGroupPolicy = req
    "DetachGroupPolicy"
    "fixture/DetachGroupPolicy.yaml"

requestGetOpenIdConnectProvider :: GetOpenIdConnectProvider -> TestTree
requestGetOpenIdConnectProvider = req
    "GetOpenIdConnectProvider"
    "fixture/GetOpenIdConnectProvider.yaml"

requestDeleteUserPolicy :: DeleteUserPolicy -> TestTree
requestDeleteUserPolicy = req
    "DeleteUserPolicy"
    "fixture/DeleteUserPolicy.yaml"

requestCreateRole :: CreateRole -> TestTree
requestCreateRole = req
    "CreateRole"
    "fixture/CreateRole.yaml"

requestResetServiceSpecificCredential :: ResetServiceSpecificCredential -> TestTree
requestResetServiceSpecificCredential = req
    "ResetServiceSpecificCredential"
    "fixture/ResetServiceSpecificCredential.yaml"

requestGetCredentialReport :: GetCredentialReport -> TestTree
requestGetCredentialReport = req
    "GetCredentialReport"
    "fixture/GetCredentialReport.yaml"

requestGetAccountSummary :: GetAccountSummary -> TestTree
requestGetAccountSummary = req
    "GetAccountSummary"
    "fixture/GetAccountSummary.yaml"

requestListGroupPolicies :: ListGroupPolicies -> TestTree
requestListGroupPolicies = req
    "ListGroupPolicies"
    "fixture/ListGroupPolicies.yaml"

requestDeletePolicyVersion :: DeletePolicyVersion -> TestTree
requestDeletePolicyVersion = req
    "DeletePolicyVersion"
    "fixture/DeletePolicyVersion.yaml"

requestDeleteInstanceProfile :: DeleteInstanceProfile -> TestTree
requestDeleteInstanceProfile = req
    "DeleteInstanceProfile"
    "fixture/DeleteInstanceProfile.yaml"

requestDetachRolePolicy :: DetachRolePolicy -> TestTree
requestDetachRolePolicy = req
    "DetachRolePolicy"
    "fixture/DetachRolePolicy.yaml"

requestRemoveRoleFromInstanceProfile :: RemoveRoleFromInstanceProfile -> TestTree
requestRemoveRoleFromInstanceProfile = req
    "RemoveRoleFromInstanceProfile"
    "fixture/RemoveRoleFromInstanceProfile.yaml"

requestCreatePolicyVersion :: CreatePolicyVersion -> TestTree
requestCreatePolicyVersion = req
    "CreatePolicyVersion"
    "fixture/CreatePolicyVersion.yaml"

requestCreateInstanceProfile :: CreateInstanceProfile -> TestTree
requestCreateInstanceProfile = req
    "CreateInstanceProfile"
    "fixture/CreateInstanceProfile.yaml"

requestCreateSAMLProvider :: CreateSAMLProvider -> TestTree
requestCreateSAMLProvider = req
    "CreateSAMLProvider"
    "fixture/CreateSAMLProvider.yaml"

requestGetAccountAuthorizationDetails :: GetAccountAuthorizationDetails -> TestTree
requestGetAccountAuthorizationDetails = req
    "GetAccountAuthorizationDetails"
    "fixture/GetAccountAuthorizationDetails.yaml"

requestGetServiceLinkedRoleDeletionStatus :: GetServiceLinkedRoleDeletionStatus -> TestTree
requestGetServiceLinkedRoleDeletionStatus = req
    "GetServiceLinkedRoleDeletionStatus"
    "fixture/GetServiceLinkedRoleDeletionStatus.yaml"

requestDeleteAccountAlias :: DeleteAccountAlias -> TestTree
requestDeleteAccountAlias = req
    "DeleteAccountAlias"
    "fixture/DeleteAccountAlias.yaml"

requestDetachUserPolicy :: DetachUserPolicy -> TestTree
requestDetachUserPolicy = req
    "DetachUserPolicy"
    "fixture/DetachUserPolicy.yaml"

requestRemoveUserFromGroup :: RemoveUserFromGroup -> TestTree
requestRemoveUserFromGroup = req
    "RemoveUserFromGroup"
    "fixture/RemoveUserFromGroup.yaml"

requestDeleteGroupPolicy :: DeleteGroupPolicy -> TestTree
requestDeleteGroupPolicy = req
    "DeleteGroupPolicy"
    "fixture/DeleteGroupPolicy.yaml"

requestPutGroupPolicy :: PutGroupPolicy -> TestTree
requestPutGroupPolicy = req
    "PutGroupPolicy"
    "fixture/PutGroupPolicy.yaml"

requestGetLoginProfile :: GetLoginProfile -> TestTree
requestGetLoginProfile = req
    "GetLoginProfile"
    "fixture/GetLoginProfile.yaml"

requestGetGroupPolicy :: GetGroupPolicy -> TestTree
requestGetGroupPolicy = req
    "GetGroupPolicy"
    "fixture/GetGroupPolicy.yaml"

requestChangePassword :: ChangePassword -> TestTree
requestChangePassword = req
    "ChangePassword"
    "fixture/ChangePassword.yaml"

requestListServerCertificates :: ListServerCertificates -> TestTree
requestListServerCertificates = req
    "ListServerCertificates"
    "fixture/ListServerCertificates.yaml"

requestDeleteServiceLinkedRole :: DeleteServiceLinkedRole -> TestTree
requestDeleteServiceLinkedRole = req
    "DeleteServiceLinkedRole"
    "fixture/DeleteServiceLinkedRole.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy = req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

requestUpdateAssumeRolePolicy :: UpdateAssumeRolePolicy -> TestTree
requestUpdateAssumeRolePolicy = req
    "UpdateAssumeRolePolicy"
    "fixture/UpdateAssumeRolePolicy.yaml"

requestGetInstanceProfile :: GetInstanceProfile -> TestTree
requestGetInstanceProfile = req
    "GetInstanceProfile"
    "fixture/GetInstanceProfile.yaml"

requestCreateLoginProfile :: CreateLoginProfile -> TestTree
requestCreateLoginProfile = req
    "CreateLoginProfile"
    "fixture/CreateLoginProfile.yaml"

requestGetSAMLProvider :: GetSAMLProvider -> TestTree
requestGetSAMLProvider = req
    "GetSAMLProvider"
    "fixture/GetSAMLProvider.yaml"

requestAddRoleToInstanceProfile :: AddRoleToInstanceProfile -> TestTree
requestAddRoleToInstanceProfile = req
    "AddRoleToInstanceProfile"
    "fixture/AddRoleToInstanceProfile.yaml"

requestListGroupsForUser :: ListGroupsForUser -> TestTree
requestListGroupsForUser = req
    "ListGroupsForUser"
    "fixture/ListGroupsForUser.yaml"

requestListEntitiesForPolicy :: ListEntitiesForPolicy -> TestTree
requestListEntitiesForPolicy = req
    "ListEntitiesForPolicy"
    "fixture/ListEntitiesForPolicy.yaml"

requestAddUserToGroup :: AddUserToGroup -> TestTree
requestAddUserToGroup = req
    "AddUserToGroup"
    "fixture/AddUserToGroup.yaml"

requestSimulatePrincipalPolicy :: SimulatePrincipalPolicy -> TestTree
requestSimulatePrincipalPolicy = req
    "SimulatePrincipalPolicy"
    "fixture/SimulatePrincipalPolicy.yaml"

requestGetPolicyVersion :: GetPolicyVersion -> TestTree
requestGetPolicyVersion = req
    "GetPolicyVersion"
    "fixture/GetPolicyVersion.yaml"

requestCreateServiceLinkedRole :: CreateServiceLinkedRole -> TestTree
requestCreateServiceLinkedRole = req
    "CreateServiceLinkedRole"
    "fixture/CreateServiceLinkedRole.yaml"

requestListServiceSpecificCredentials :: ListServiceSpecificCredentials -> TestTree
requestListServiceSpecificCredentials = req
    "ListServiceSpecificCredentials"
    "fixture/ListServiceSpecificCredentials.yaml"

requestDeleteOpenIdConnectProvider :: DeleteOpenIdConnectProvider -> TestTree
requestDeleteOpenIdConnectProvider = req
    "DeleteOpenIdConnectProvider"
    "fixture/DeleteOpenIdConnectProvider.yaml"

requestGetUser :: GetUser -> TestTree
requestGetUser = req
    "GetUser"
    "fixture/GetUser.yaml"

requestListSigningCertificates :: ListSigningCertificates -> TestTree
requestListSigningCertificates = req
    "ListSigningCertificates"
    "fixture/ListSigningCertificates.yaml"

requestDeleteSigningCertificate :: DeleteSigningCertificate -> TestTree
requestDeleteSigningCertificate = req
    "DeleteSigningCertificate"
    "fixture/DeleteSigningCertificate.yaml"

requestUpdateSigningCertificate :: UpdateSigningCertificate -> TestTree
requestUpdateSigningCertificate = req
    "UpdateSigningCertificate"
    "fixture/UpdateSigningCertificate.yaml"

requestListAttachedUserPolicies :: ListAttachedUserPolicies -> TestTree
requestListAttachedUserPolicies = req
    "ListAttachedUserPolicies"
    "fixture/ListAttachedUserPolicies.yaml"

requestRemoveClientIdFromOpenIdConnectProvider :: RemoveClientIdFromOpenIdConnectProvider -> TestTree
requestRemoveClientIdFromOpenIdConnectProvider = req
    "RemoveClientIdFromOpenIdConnectProvider"
    "fixture/RemoveClientIdFromOpenIdConnectProvider.yaml"

requestAttachUserPolicy :: AttachUserPolicy -> TestTree
requestAttachUserPolicy = req
    "AttachUserPolicy"
    "fixture/AttachUserPolicy.yaml"

requestCreateServiceSpecificCredential :: CreateServiceSpecificCredential -> TestTree
requestCreateServiceSpecificCredential = req
    "CreateServiceSpecificCredential"
    "fixture/CreateServiceSpecificCredential.yaml"

requestListVirtualMFADevices :: ListVirtualMFADevices -> TestTree
requestListVirtualMFADevices = req
    "ListVirtualMFADevices"
    "fixture/ListVirtualMFADevices.yaml"

requestResyncMFADevice :: ResyncMFADevice -> TestTree
requestResyncMFADevice = req
    "ResyncMFADevice"
    "fixture/ResyncMFADevice.yaml"

requestDeleteAccessKey :: DeleteAccessKey -> TestTree
requestDeleteAccessKey = req
    "DeleteAccessKey"
    "fixture/DeleteAccessKey.yaml"

requestUpdateAccessKey :: UpdateAccessKey -> TestTree
requestUpdateAccessKey = req
    "UpdateAccessKey"
    "fixture/UpdateAccessKey.yaml"

requestListAccessKeys :: ListAccessKeys -> TestTree
requestListAccessKeys = req
    "ListAccessKeys"
    "fixture/ListAccessKeys.yaml"

requestGetRolePolicy :: GetRolePolicy -> TestTree
requestGetRolePolicy = req
    "GetRolePolicy"
    "fixture/GetRolePolicy.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser = req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestPutRolePolicy :: PutRolePolicy -> TestTree
requestPutRolePolicy = req
    "PutRolePolicy"
    "fixture/PutRolePolicy.yaml"

requestGetContextKeysForCustomPolicy :: GetContextKeysForCustomPolicy -> TestTree
requestGetContextKeysForCustomPolicy = req
    "GetContextKeysForCustomPolicy"
    "fixture/GetContextKeysForCustomPolicy.yaml"

requestUploadSigningCertificate :: UploadSigningCertificate -> TestTree
requestUploadSigningCertificate = req
    "UploadSigningCertificate"
    "fixture/UploadSigningCertificate.yaml"

requestDeleteRolePolicy :: DeleteRolePolicy -> TestTree
requestDeleteRolePolicy = req
    "DeleteRolePolicy"
    "fixture/DeleteRolePolicy.yaml"

requestGetAccountPasswordPolicy :: GetAccountPasswordPolicy -> TestTree
requestGetAccountPasswordPolicy = req
    "GetAccountPasswordPolicy"
    "fixture/GetAccountPasswordPolicy.yaml"

requestGetAccessKeyLastUsed :: GetAccessKeyLastUsed -> TestTree
requestGetAccessKeyLastUsed = req
    "GetAccessKeyLastUsed"
    "fixture/GetAccessKeyLastUsed.yaml"

requestUpdateUser :: UpdateUser -> TestTree
requestUpdateUser = req
    "UpdateUser"
    "fixture/UpdateUser.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser = req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestAddClientIdToOpenIdConnectProvider :: AddClientIdToOpenIdConnectProvider -> TestTree
requestAddClientIdToOpenIdConnectProvider = req
    "AddClientIdToOpenIdConnectProvider"
    "fixture/AddClientIdToOpenIdConnectProvider.yaml"

requestListRolePolicies :: ListRolePolicies -> TestTree
requestListRolePolicies = req
    "ListRolePolicies"
    "fixture/ListRolePolicies.yaml"

requestCreateAccountAlias :: CreateAccountAlias -> TestTree
requestCreateAccountAlias = req
    "CreateAccountAlias"
    "fixture/CreateAccountAlias.yaml"

requestListInstanceProfiles :: ListInstanceProfiles -> TestTree
requestListInstanceProfiles = req
    "ListInstanceProfiles"
    "fixture/ListInstanceProfiles.yaml"

requestEnableMFADevice :: EnableMFADevice -> TestTree
requestEnableMFADevice = req
    "EnableMFADevice"
    "fixture/EnableMFADevice.yaml"

requestListAccountAliases :: ListAccountAliases -> TestTree
requestListAccountAliases = req
    "ListAccountAliases"
    "fixture/ListAccountAliases.yaml"

requestDeleteSAMLProvider :: DeleteSAMLProvider -> TestTree
requestDeleteSAMLProvider = req
    "DeleteSAMLProvider"
    "fixture/DeleteSAMLProvider.yaml"

requestUpdateSAMLProvider :: UpdateSAMLProvider -> TestTree
requestUpdateSAMLProvider = req
    "UpdateSAMLProvider"
    "fixture/UpdateSAMLProvider.yaml"

requestCreateGroup :: CreateGroup -> TestTree
requestCreateGroup = req
    "CreateGroup"
    "fixture/CreateGroup.yaml"

requestListMFADevices :: ListMFADevices -> TestTree
requestListMFADevices = req
    "ListMFADevices"
    "fixture/ListMFADevices.yaml"

requestUploadServerCertificate :: UploadServerCertificate -> TestTree
requestUploadServerCertificate = req
    "UploadServerCertificate"
    "fixture/UploadServerCertificate.yaml"

requestSetDefaultPolicyVersion :: SetDefaultPolicyVersion -> TestTree
requestSetDefaultPolicyVersion = req
    "SetDefaultPolicyVersion"
    "fixture/SetDefaultPolicyVersion.yaml"

requestListPolicyVersions :: ListPolicyVersions -> TestTree
requestListPolicyVersions = req
    "ListPolicyVersions"
    "fixture/ListPolicyVersions.yaml"

requestUpdateRoleDescription :: UpdateRoleDescription -> TestTree
requestUpdateRoleDescription = req
    "UpdateRoleDescription"
    "fixture/UpdateRoleDescription.yaml"

requestListSAMLProviders :: ListSAMLProviders -> TestTree
requestListSAMLProviders = req
    "ListSAMLProviders"
    "fixture/ListSAMLProviders.yaml"

requestGetServerCertificate :: GetServerCertificate -> TestTree
requestGetServerCertificate = req
    "GetServerCertificate"
    "fixture/GetServerCertificate.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup = req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

requestUpdateGroup :: UpdateGroup -> TestTree
requestUpdateGroup = req
    "UpdateGroup"
    "fixture/UpdateGroup.yaml"

requestListGroups :: ListGroups -> TestTree
requestListGroups = req
    "ListGroups"
    "fixture/ListGroups.yaml"

requestGenerateCredentialReport :: GenerateCredentialReport -> TestTree
requestGenerateCredentialReport = req
    "GenerateCredentialReport"
    "fixture/GenerateCredentialReport.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy = req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

requestUpdateLoginProfile :: UpdateLoginProfile -> TestTree
requestUpdateLoginProfile = req
    "UpdateLoginProfile"
    "fixture/UpdateLoginProfile.yaml"

requestDeleteLoginProfile :: DeleteLoginProfile -> TestTree
requestDeleteLoginProfile = req
    "DeleteLoginProfile"
    "fixture/DeleteLoginProfile.yaml"

requestGetGroup :: GetGroup -> TestTree
requestGetGroup = req
    "GetGroup"
    "fixture/GetGroup.yaml"

requestDeleteServerCertificate :: DeleteServerCertificate -> TestTree
requestDeleteServerCertificate = req
    "DeleteServerCertificate"
    "fixture/DeleteServerCertificate.yaml"

requestUpdateServerCertificate :: UpdateServerCertificate -> TestTree
requestUpdateServerCertificate = req
    "UpdateServerCertificate"
    "fixture/UpdateServerCertificate.yaml"

requestListAttachedGroupPolicies :: ListAttachedGroupPolicies -> TestTree
requestListAttachedGroupPolicies = req
    "ListAttachedGroupPolicies"
    "fixture/ListAttachedGroupPolicies.yaml"

-- Responses

responseGetContextKeysForPrincipalPolicy :: GetContextKeysForPolicyResponse -> TestTree
responseGetContextKeysForPrincipalPolicy = res
    "GetContextKeysForPrincipalPolicyResponse"
    "fixture/GetContextKeysForPrincipalPolicyResponse.proto"
    iam
    (Proxy :: Proxy GetContextKeysForPrincipalPolicy)

responseListPolicies :: ListPoliciesResponse -> TestTree
responseListPolicies = res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    iam
    (Proxy :: Proxy ListPolicies)

responseCreatePolicy :: CreatePolicyResponse -> TestTree
responseCreatePolicy = res
    "CreatePolicyResponse"
    "fixture/CreatePolicyResponse.proto"
    iam
    (Proxy :: Proxy CreatePolicy)

responseListInstanceProfilesForRole :: ListInstanceProfilesForRoleResponse -> TestTree
responseListInstanceProfilesForRole = res
    "ListInstanceProfilesForRoleResponse"
    "fixture/ListInstanceProfilesForRoleResponse.proto"
    iam
    (Proxy :: Proxy ListInstanceProfilesForRole)

responseAttachGroupPolicy :: AttachGroupPolicyResponse -> TestTree
responseAttachGroupPolicy = res
    "AttachGroupPolicyResponse"
    "fixture/AttachGroupPolicyResponse.proto"
    iam
    (Proxy :: Proxy AttachGroupPolicy)

responseCreateAccessKey :: CreateAccessKeyResponse -> TestTree
responseCreateAccessKey = res
    "CreateAccessKeyResponse"
    "fixture/CreateAccessKeyResponse.proto"
    iam
    (Proxy :: Proxy CreateAccessKey)

responseListSSHPublicKeys :: ListSSHPublicKeysResponse -> TestTree
responseListSSHPublicKeys = res
    "ListSSHPublicKeysResponse"
    "fixture/ListSSHPublicKeysResponse.proto"
    iam
    (Proxy :: Proxy ListSSHPublicKeys)

responseListOpenIdConnectProviders :: ListOpenIdConnectProvidersResponse -> TestTree
responseListOpenIdConnectProviders = res
    "ListOpenIdConnectProvidersResponse"
    "fixture/ListOpenIdConnectProvidersResponse.proto"
    iam
    (Proxy :: Proxy ListOpenIdConnectProviders)

responseCreateVirtualMFADevice :: CreateVirtualMFADeviceResponse -> TestTree
responseCreateVirtualMFADevice = res
    "CreateVirtualMFADeviceResponse"
    "fixture/CreateVirtualMFADeviceResponse.proto"
    iam
    (Proxy :: Proxy CreateVirtualMFADevice)

responseDeleteAccountPasswordPolicy :: DeleteAccountPasswordPolicyResponse -> TestTree
responseDeleteAccountPasswordPolicy = res
    "DeleteAccountPasswordPolicyResponse"
    "fixture/DeleteAccountPasswordPolicyResponse.proto"
    iam
    (Proxy :: Proxy DeleteAccountPasswordPolicy)

responseUpdateAccountPasswordPolicy :: UpdateAccountPasswordPolicyResponse -> TestTree
responseUpdateAccountPasswordPolicy = res
    "UpdateAccountPasswordPolicyResponse"
    "fixture/UpdateAccountPasswordPolicyResponse.proto"
    iam
    (Proxy :: Proxy UpdateAccountPasswordPolicy)

responseAttachRolePolicy :: AttachRolePolicyResponse -> TestTree
responseAttachRolePolicy = res
    "AttachRolePolicyResponse"
    "fixture/AttachRolePolicyResponse.proto"
    iam
    (Proxy :: Proxy AttachRolePolicy)

responseUpdateSSHPublicKey :: UpdateSSHPublicKeyResponse -> TestTree
responseUpdateSSHPublicKey = res
    "UpdateSSHPublicKeyResponse"
    "fixture/UpdateSSHPublicKeyResponse.proto"
    iam
    (Proxy :: Proxy UpdateSSHPublicKey)

responseDeleteSSHPublicKey :: DeleteSSHPublicKeyResponse -> TestTree
responseDeleteSSHPublicKey = res
    "DeleteSSHPublicKeyResponse"
    "fixture/DeleteSSHPublicKeyResponse.proto"
    iam
    (Proxy :: Proxy DeleteSSHPublicKey)

responseGetUserPolicy :: GetUserPolicyResponse -> TestTree
responseGetUserPolicy = res
    "GetUserPolicyResponse"
    "fixture/GetUserPolicyResponse.proto"
    iam
    (Proxy :: Proxy GetUserPolicy)

responseUpdateServiceSpecificCredential :: UpdateServiceSpecificCredentialResponse -> TestTree
responseUpdateServiceSpecificCredential = res
    "UpdateServiceSpecificCredentialResponse"
    "fixture/UpdateServiceSpecificCredentialResponse.proto"
    iam
    (Proxy :: Proxy UpdateServiceSpecificCredential)

responseDeleteServiceSpecificCredential :: DeleteServiceSpecificCredentialResponse -> TestTree
responseDeleteServiceSpecificCredential = res
    "DeleteServiceSpecificCredentialResponse"
    "fixture/DeleteServiceSpecificCredentialResponse.proto"
    iam
    (Proxy :: Proxy DeleteServiceSpecificCredential)

responseListAttachedRolePolicies :: ListAttachedRolePoliciesResponse -> TestTree
responseListAttachedRolePolicies = res
    "ListAttachedRolePoliciesResponse"
    "fixture/ListAttachedRolePoliciesResponse.proto"
    iam
    (Proxy :: Proxy ListAttachedRolePolicies)

responseGetRole :: GetRoleResponse -> TestTree
responseGetRole = res
    "GetRoleResponse"
    "fixture/GetRoleResponse.proto"
    iam
    (Proxy :: Proxy GetRole)

responseDeactivateMFADevice :: DeactivateMFADeviceResponse -> TestTree
responseDeactivateMFADevice = res
    "DeactivateMFADeviceResponse"
    "fixture/DeactivateMFADeviceResponse.proto"
    iam
    (Proxy :: Proxy DeactivateMFADevice)

responseCreateOpenIdConnectProvider :: CreateOpenIdConnectProviderResponse -> TestTree
responseCreateOpenIdConnectProvider = res
    "CreateOpenIdConnectProviderResponse"
    "fixture/CreateOpenIdConnectProviderResponse.proto"
    iam
    (Proxy :: Proxy CreateOpenIdConnectProvider)

responseDeleteVirtualMFADevice :: DeleteVirtualMFADeviceResponse -> TestTree
responseDeleteVirtualMFADevice = res
    "DeleteVirtualMFADeviceResponse"
    "fixture/DeleteVirtualMFADeviceResponse.proto"
    iam
    (Proxy :: Proxy DeleteVirtualMFADevice)

responseListRoles :: ListRolesResponse -> TestTree
responseListRoles = res
    "ListRolesResponse"
    "fixture/ListRolesResponse.proto"
    iam
    (Proxy :: Proxy ListRoles)

responseListUserPolicies :: ListUserPoliciesResponse -> TestTree
responseListUserPolicies = res
    "ListUserPoliciesResponse"
    "fixture/ListUserPoliciesResponse.proto"
    iam
    (Proxy :: Proxy ListUserPolicies)

responseUploadSSHPublicKey :: UploadSSHPublicKeyResponse -> TestTree
responseUploadSSHPublicKey = res
    "UploadSSHPublicKeyResponse"
    "fixture/UploadSSHPublicKeyResponse.proto"
    iam
    (Proxy :: Proxy UploadSSHPublicKey)

responseSimulateCustomPolicy :: SimulatePolicyResponse -> TestTree
responseSimulateCustomPolicy = res
    "SimulateCustomPolicyResponse"
    "fixture/SimulateCustomPolicyResponse.proto"
    iam
    (Proxy :: Proxy SimulateCustomPolicy)

responseUpdateRole :: UpdateRoleResponse -> TestTree
responseUpdateRole = res
    "UpdateRoleResponse"
    "fixture/UpdateRoleResponse.proto"
    iam
    (Proxy :: Proxy UpdateRole)

responseDeleteRole :: DeleteRoleResponse -> TestTree
responseDeleteRole = res
    "DeleteRoleResponse"
    "fixture/DeleteRoleResponse.proto"
    iam
    (Proxy :: Proxy DeleteRole)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers = res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    iam
    (Proxy :: Proxy ListUsers)

responseUpdateOpenIdConnectProviderThumbprint :: UpdateOpenIdConnectProviderThumbprintResponse -> TestTree
responseUpdateOpenIdConnectProviderThumbprint = res
    "UpdateOpenIdConnectProviderThumbprintResponse"
    "fixture/UpdateOpenIdConnectProviderThumbprintResponse.proto"
    iam
    (Proxy :: Proxy UpdateOpenIdConnectProviderThumbprint)

responsePutUserPolicy :: PutUserPolicyResponse -> TestTree
responsePutUserPolicy = res
    "PutUserPolicyResponse"
    "fixture/PutUserPolicyResponse.proto"
    iam
    (Proxy :: Proxy PutUserPolicy)

responseGetSSHPublicKey :: GetSSHPublicKeyResponse -> TestTree
responseGetSSHPublicKey = res
    "GetSSHPublicKeyResponse"
    "fixture/GetSSHPublicKeyResponse.proto"
    iam
    (Proxy :: Proxy GetSSHPublicKey)

responseDetachGroupPolicy :: DetachGroupPolicyResponse -> TestTree
responseDetachGroupPolicy = res
    "DetachGroupPolicyResponse"
    "fixture/DetachGroupPolicyResponse.proto"
    iam
    (Proxy :: Proxy DetachGroupPolicy)

responseGetOpenIdConnectProvider :: GetOpenIdConnectProviderResponse -> TestTree
responseGetOpenIdConnectProvider = res
    "GetOpenIdConnectProviderResponse"
    "fixture/GetOpenIdConnectProviderResponse.proto"
    iam
    (Proxy :: Proxy GetOpenIdConnectProvider)

responseDeleteUserPolicy :: DeleteUserPolicyResponse -> TestTree
responseDeleteUserPolicy = res
    "DeleteUserPolicyResponse"
    "fixture/DeleteUserPolicyResponse.proto"
    iam
    (Proxy :: Proxy DeleteUserPolicy)

responseCreateRole :: CreateRoleResponse -> TestTree
responseCreateRole = res
    "CreateRoleResponse"
    "fixture/CreateRoleResponse.proto"
    iam
    (Proxy :: Proxy CreateRole)

responseResetServiceSpecificCredential :: ResetServiceSpecificCredentialResponse -> TestTree
responseResetServiceSpecificCredential = res
    "ResetServiceSpecificCredentialResponse"
    "fixture/ResetServiceSpecificCredentialResponse.proto"
    iam
    (Proxy :: Proxy ResetServiceSpecificCredential)

responseGetCredentialReport :: GetCredentialReportResponse -> TestTree
responseGetCredentialReport = res
    "GetCredentialReportResponse"
    "fixture/GetCredentialReportResponse.proto"
    iam
    (Proxy :: Proxy GetCredentialReport)

responseGetAccountSummary :: GetAccountSummaryResponse -> TestTree
responseGetAccountSummary = res
    "GetAccountSummaryResponse"
    "fixture/GetAccountSummaryResponse.proto"
    iam
    (Proxy :: Proxy GetAccountSummary)

responseListGroupPolicies :: ListGroupPoliciesResponse -> TestTree
responseListGroupPolicies = res
    "ListGroupPoliciesResponse"
    "fixture/ListGroupPoliciesResponse.proto"
    iam
    (Proxy :: Proxy ListGroupPolicies)

responseDeletePolicyVersion :: DeletePolicyVersionResponse -> TestTree
responseDeletePolicyVersion = res
    "DeletePolicyVersionResponse"
    "fixture/DeletePolicyVersionResponse.proto"
    iam
    (Proxy :: Proxy DeletePolicyVersion)

responseDeleteInstanceProfile :: DeleteInstanceProfileResponse -> TestTree
responseDeleteInstanceProfile = res
    "DeleteInstanceProfileResponse"
    "fixture/DeleteInstanceProfileResponse.proto"
    iam
    (Proxy :: Proxy DeleteInstanceProfile)

responseDetachRolePolicy :: DetachRolePolicyResponse -> TestTree
responseDetachRolePolicy = res
    "DetachRolePolicyResponse"
    "fixture/DetachRolePolicyResponse.proto"
    iam
    (Proxy :: Proxy DetachRolePolicy)

responseRemoveRoleFromInstanceProfile :: RemoveRoleFromInstanceProfileResponse -> TestTree
responseRemoveRoleFromInstanceProfile = res
    "RemoveRoleFromInstanceProfileResponse"
    "fixture/RemoveRoleFromInstanceProfileResponse.proto"
    iam
    (Proxy :: Proxy RemoveRoleFromInstanceProfile)

responseCreatePolicyVersion :: CreatePolicyVersionResponse -> TestTree
responseCreatePolicyVersion = res
    "CreatePolicyVersionResponse"
    "fixture/CreatePolicyVersionResponse.proto"
    iam
    (Proxy :: Proxy CreatePolicyVersion)

responseCreateInstanceProfile :: CreateInstanceProfileResponse -> TestTree
responseCreateInstanceProfile = res
    "CreateInstanceProfileResponse"
    "fixture/CreateInstanceProfileResponse.proto"
    iam
    (Proxy :: Proxy CreateInstanceProfile)

responseCreateSAMLProvider :: CreateSAMLProviderResponse -> TestTree
responseCreateSAMLProvider = res
    "CreateSAMLProviderResponse"
    "fixture/CreateSAMLProviderResponse.proto"
    iam
    (Proxy :: Proxy CreateSAMLProvider)

responseGetAccountAuthorizationDetails :: GetAccountAuthorizationDetailsResponse -> TestTree
responseGetAccountAuthorizationDetails = res
    "GetAccountAuthorizationDetailsResponse"
    "fixture/GetAccountAuthorizationDetailsResponse.proto"
    iam
    (Proxy :: Proxy GetAccountAuthorizationDetails)

responseGetServiceLinkedRoleDeletionStatus :: GetServiceLinkedRoleDeletionStatusResponse -> TestTree
responseGetServiceLinkedRoleDeletionStatus = res
    "GetServiceLinkedRoleDeletionStatusResponse"
    "fixture/GetServiceLinkedRoleDeletionStatusResponse.proto"
    iam
    (Proxy :: Proxy GetServiceLinkedRoleDeletionStatus)

responseDeleteAccountAlias :: DeleteAccountAliasResponse -> TestTree
responseDeleteAccountAlias = res
    "DeleteAccountAliasResponse"
    "fixture/DeleteAccountAliasResponse.proto"
    iam
    (Proxy :: Proxy DeleteAccountAlias)

responseDetachUserPolicy :: DetachUserPolicyResponse -> TestTree
responseDetachUserPolicy = res
    "DetachUserPolicyResponse"
    "fixture/DetachUserPolicyResponse.proto"
    iam
    (Proxy :: Proxy DetachUserPolicy)

responseRemoveUserFromGroup :: RemoveUserFromGroupResponse -> TestTree
responseRemoveUserFromGroup = res
    "RemoveUserFromGroupResponse"
    "fixture/RemoveUserFromGroupResponse.proto"
    iam
    (Proxy :: Proxy RemoveUserFromGroup)

responseDeleteGroupPolicy :: DeleteGroupPolicyResponse -> TestTree
responseDeleteGroupPolicy = res
    "DeleteGroupPolicyResponse"
    "fixture/DeleteGroupPolicyResponse.proto"
    iam
    (Proxy :: Proxy DeleteGroupPolicy)

responsePutGroupPolicy :: PutGroupPolicyResponse -> TestTree
responsePutGroupPolicy = res
    "PutGroupPolicyResponse"
    "fixture/PutGroupPolicyResponse.proto"
    iam
    (Proxy :: Proxy PutGroupPolicy)

responseGetLoginProfile :: GetLoginProfileResponse -> TestTree
responseGetLoginProfile = res
    "GetLoginProfileResponse"
    "fixture/GetLoginProfileResponse.proto"
    iam
    (Proxy :: Proxy GetLoginProfile)

responseGetGroupPolicy :: GetGroupPolicyResponse -> TestTree
responseGetGroupPolicy = res
    "GetGroupPolicyResponse"
    "fixture/GetGroupPolicyResponse.proto"
    iam
    (Proxy :: Proxy GetGroupPolicy)

responseChangePassword :: ChangePasswordResponse -> TestTree
responseChangePassword = res
    "ChangePasswordResponse"
    "fixture/ChangePasswordResponse.proto"
    iam
    (Proxy :: Proxy ChangePassword)

responseListServerCertificates :: ListServerCertificatesResponse -> TestTree
responseListServerCertificates = res
    "ListServerCertificatesResponse"
    "fixture/ListServerCertificatesResponse.proto"
    iam
    (Proxy :: Proxy ListServerCertificates)

responseDeleteServiceLinkedRole :: DeleteServiceLinkedRoleResponse -> TestTree
responseDeleteServiceLinkedRole = res
    "DeleteServiceLinkedRoleResponse"
    "fixture/DeleteServiceLinkedRoleResponse.proto"
    iam
    (Proxy :: Proxy DeleteServiceLinkedRole)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy = res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    iam
    (Proxy :: Proxy DeletePolicy)

responseUpdateAssumeRolePolicy :: UpdateAssumeRolePolicyResponse -> TestTree
responseUpdateAssumeRolePolicy = res
    "UpdateAssumeRolePolicyResponse"
    "fixture/UpdateAssumeRolePolicyResponse.proto"
    iam
    (Proxy :: Proxy UpdateAssumeRolePolicy)

responseGetInstanceProfile :: GetInstanceProfileResponse -> TestTree
responseGetInstanceProfile = res
    "GetInstanceProfileResponse"
    "fixture/GetInstanceProfileResponse.proto"
    iam
    (Proxy :: Proxy GetInstanceProfile)

responseCreateLoginProfile :: CreateLoginProfileResponse -> TestTree
responseCreateLoginProfile = res
    "CreateLoginProfileResponse"
    "fixture/CreateLoginProfileResponse.proto"
    iam
    (Proxy :: Proxy CreateLoginProfile)

responseGetSAMLProvider :: GetSAMLProviderResponse -> TestTree
responseGetSAMLProvider = res
    "GetSAMLProviderResponse"
    "fixture/GetSAMLProviderResponse.proto"
    iam
    (Proxy :: Proxy GetSAMLProvider)

responseAddRoleToInstanceProfile :: AddRoleToInstanceProfileResponse -> TestTree
responseAddRoleToInstanceProfile = res
    "AddRoleToInstanceProfileResponse"
    "fixture/AddRoleToInstanceProfileResponse.proto"
    iam
    (Proxy :: Proxy AddRoleToInstanceProfile)

responseListGroupsForUser :: ListGroupsForUserResponse -> TestTree
responseListGroupsForUser = res
    "ListGroupsForUserResponse"
    "fixture/ListGroupsForUserResponse.proto"
    iam
    (Proxy :: Proxy ListGroupsForUser)

responseListEntitiesForPolicy :: ListEntitiesForPolicyResponse -> TestTree
responseListEntitiesForPolicy = res
    "ListEntitiesForPolicyResponse"
    "fixture/ListEntitiesForPolicyResponse.proto"
    iam
    (Proxy :: Proxy ListEntitiesForPolicy)

responseAddUserToGroup :: AddUserToGroupResponse -> TestTree
responseAddUserToGroup = res
    "AddUserToGroupResponse"
    "fixture/AddUserToGroupResponse.proto"
    iam
    (Proxy :: Proxy AddUserToGroup)

responseSimulatePrincipalPolicy :: SimulatePolicyResponse -> TestTree
responseSimulatePrincipalPolicy = res
    "SimulatePrincipalPolicyResponse"
    "fixture/SimulatePrincipalPolicyResponse.proto"
    iam
    (Proxy :: Proxy SimulatePrincipalPolicy)

responseGetPolicyVersion :: GetPolicyVersionResponse -> TestTree
responseGetPolicyVersion = res
    "GetPolicyVersionResponse"
    "fixture/GetPolicyVersionResponse.proto"
    iam
    (Proxy :: Proxy GetPolicyVersion)

responseCreateServiceLinkedRole :: CreateServiceLinkedRoleResponse -> TestTree
responseCreateServiceLinkedRole = res
    "CreateServiceLinkedRoleResponse"
    "fixture/CreateServiceLinkedRoleResponse.proto"
    iam
    (Proxy :: Proxy CreateServiceLinkedRole)

responseListServiceSpecificCredentials :: ListServiceSpecificCredentialsResponse -> TestTree
responseListServiceSpecificCredentials = res
    "ListServiceSpecificCredentialsResponse"
    "fixture/ListServiceSpecificCredentialsResponse.proto"
    iam
    (Proxy :: Proxy ListServiceSpecificCredentials)

responseDeleteOpenIdConnectProvider :: DeleteOpenIdConnectProviderResponse -> TestTree
responseDeleteOpenIdConnectProvider = res
    "DeleteOpenIdConnectProviderResponse"
    "fixture/DeleteOpenIdConnectProviderResponse.proto"
    iam
    (Proxy :: Proxy DeleteOpenIdConnectProvider)

responseGetUser :: GetUserResponse -> TestTree
responseGetUser = res
    "GetUserResponse"
    "fixture/GetUserResponse.proto"
    iam
    (Proxy :: Proxy GetUser)

responseListSigningCertificates :: ListSigningCertificatesResponse -> TestTree
responseListSigningCertificates = res
    "ListSigningCertificatesResponse"
    "fixture/ListSigningCertificatesResponse.proto"
    iam
    (Proxy :: Proxy ListSigningCertificates)

responseDeleteSigningCertificate :: DeleteSigningCertificateResponse -> TestTree
responseDeleteSigningCertificate = res
    "DeleteSigningCertificateResponse"
    "fixture/DeleteSigningCertificateResponse.proto"
    iam
    (Proxy :: Proxy DeleteSigningCertificate)

responseUpdateSigningCertificate :: UpdateSigningCertificateResponse -> TestTree
responseUpdateSigningCertificate = res
    "UpdateSigningCertificateResponse"
    "fixture/UpdateSigningCertificateResponse.proto"
    iam
    (Proxy :: Proxy UpdateSigningCertificate)

responseListAttachedUserPolicies :: ListAttachedUserPoliciesResponse -> TestTree
responseListAttachedUserPolicies = res
    "ListAttachedUserPoliciesResponse"
    "fixture/ListAttachedUserPoliciesResponse.proto"
    iam
    (Proxy :: Proxy ListAttachedUserPolicies)

responseRemoveClientIdFromOpenIdConnectProvider :: RemoveClientIdFromOpenIdConnectProviderResponse -> TestTree
responseRemoveClientIdFromOpenIdConnectProvider = res
    "RemoveClientIdFromOpenIdConnectProviderResponse"
    "fixture/RemoveClientIdFromOpenIdConnectProviderResponse.proto"
    iam
    (Proxy :: Proxy RemoveClientIdFromOpenIdConnectProvider)

responseAttachUserPolicy :: AttachUserPolicyResponse -> TestTree
responseAttachUserPolicy = res
    "AttachUserPolicyResponse"
    "fixture/AttachUserPolicyResponse.proto"
    iam
    (Proxy :: Proxy AttachUserPolicy)

responseCreateServiceSpecificCredential :: CreateServiceSpecificCredentialResponse -> TestTree
responseCreateServiceSpecificCredential = res
    "CreateServiceSpecificCredentialResponse"
    "fixture/CreateServiceSpecificCredentialResponse.proto"
    iam
    (Proxy :: Proxy CreateServiceSpecificCredential)

responseListVirtualMFADevices :: ListVirtualMFADevicesResponse -> TestTree
responseListVirtualMFADevices = res
    "ListVirtualMFADevicesResponse"
    "fixture/ListVirtualMFADevicesResponse.proto"
    iam
    (Proxy :: Proxy ListVirtualMFADevices)

responseResyncMFADevice :: ResyncMFADeviceResponse -> TestTree
responseResyncMFADevice = res
    "ResyncMFADeviceResponse"
    "fixture/ResyncMFADeviceResponse.proto"
    iam
    (Proxy :: Proxy ResyncMFADevice)

responseDeleteAccessKey :: DeleteAccessKeyResponse -> TestTree
responseDeleteAccessKey = res
    "DeleteAccessKeyResponse"
    "fixture/DeleteAccessKeyResponse.proto"
    iam
    (Proxy :: Proxy DeleteAccessKey)

responseUpdateAccessKey :: UpdateAccessKeyResponse -> TestTree
responseUpdateAccessKey = res
    "UpdateAccessKeyResponse"
    "fixture/UpdateAccessKeyResponse.proto"
    iam
    (Proxy :: Proxy UpdateAccessKey)

responseListAccessKeys :: ListAccessKeysResponse -> TestTree
responseListAccessKeys = res
    "ListAccessKeysResponse"
    "fixture/ListAccessKeysResponse.proto"
    iam
    (Proxy :: Proxy ListAccessKeys)

responseGetRolePolicy :: GetRolePolicyResponse -> TestTree
responseGetRolePolicy = res
    "GetRolePolicyResponse"
    "fixture/GetRolePolicyResponse.proto"
    iam
    (Proxy :: Proxy GetRolePolicy)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser = res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    iam
    (Proxy :: Proxy CreateUser)

responsePutRolePolicy :: PutRolePolicyResponse -> TestTree
responsePutRolePolicy = res
    "PutRolePolicyResponse"
    "fixture/PutRolePolicyResponse.proto"
    iam
    (Proxy :: Proxy PutRolePolicy)

responseGetContextKeysForCustomPolicy :: GetContextKeysForPolicyResponse -> TestTree
responseGetContextKeysForCustomPolicy = res
    "GetContextKeysForCustomPolicyResponse"
    "fixture/GetContextKeysForCustomPolicyResponse.proto"
    iam
    (Proxy :: Proxy GetContextKeysForCustomPolicy)

responseUploadSigningCertificate :: UploadSigningCertificateResponse -> TestTree
responseUploadSigningCertificate = res
    "UploadSigningCertificateResponse"
    "fixture/UploadSigningCertificateResponse.proto"
    iam
    (Proxy :: Proxy UploadSigningCertificate)

responseDeleteRolePolicy :: DeleteRolePolicyResponse -> TestTree
responseDeleteRolePolicy = res
    "DeleteRolePolicyResponse"
    "fixture/DeleteRolePolicyResponse.proto"
    iam
    (Proxy :: Proxy DeleteRolePolicy)

responseGetAccountPasswordPolicy :: GetAccountPasswordPolicyResponse -> TestTree
responseGetAccountPasswordPolicy = res
    "GetAccountPasswordPolicyResponse"
    "fixture/GetAccountPasswordPolicyResponse.proto"
    iam
    (Proxy :: Proxy GetAccountPasswordPolicy)

responseGetAccessKeyLastUsed :: GetAccessKeyLastUsedResponse -> TestTree
responseGetAccessKeyLastUsed = res
    "GetAccessKeyLastUsedResponse"
    "fixture/GetAccessKeyLastUsedResponse.proto"
    iam
    (Proxy :: Proxy GetAccessKeyLastUsed)

responseUpdateUser :: UpdateUserResponse -> TestTree
responseUpdateUser = res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    iam
    (Proxy :: Proxy UpdateUser)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser = res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    iam
    (Proxy :: Proxy DeleteUser)

responseAddClientIdToOpenIdConnectProvider :: AddClientIdToOpenIdConnectProviderResponse -> TestTree
responseAddClientIdToOpenIdConnectProvider = res
    "AddClientIdToOpenIdConnectProviderResponse"
    "fixture/AddClientIdToOpenIdConnectProviderResponse.proto"
    iam
    (Proxy :: Proxy AddClientIdToOpenIdConnectProvider)

responseListRolePolicies :: ListRolePoliciesResponse -> TestTree
responseListRolePolicies = res
    "ListRolePoliciesResponse"
    "fixture/ListRolePoliciesResponse.proto"
    iam
    (Proxy :: Proxy ListRolePolicies)

responseCreateAccountAlias :: CreateAccountAliasResponse -> TestTree
responseCreateAccountAlias = res
    "CreateAccountAliasResponse"
    "fixture/CreateAccountAliasResponse.proto"
    iam
    (Proxy :: Proxy CreateAccountAlias)

responseListInstanceProfiles :: ListInstanceProfilesResponse -> TestTree
responseListInstanceProfiles = res
    "ListInstanceProfilesResponse"
    "fixture/ListInstanceProfilesResponse.proto"
    iam
    (Proxy :: Proxy ListInstanceProfiles)

responseEnableMFADevice :: EnableMFADeviceResponse -> TestTree
responseEnableMFADevice = res
    "EnableMFADeviceResponse"
    "fixture/EnableMFADeviceResponse.proto"
    iam
    (Proxy :: Proxy EnableMFADevice)

responseListAccountAliases :: ListAccountAliasesResponse -> TestTree
responseListAccountAliases = res
    "ListAccountAliasesResponse"
    "fixture/ListAccountAliasesResponse.proto"
    iam
    (Proxy :: Proxy ListAccountAliases)

responseDeleteSAMLProvider :: DeleteSAMLProviderResponse -> TestTree
responseDeleteSAMLProvider = res
    "DeleteSAMLProviderResponse"
    "fixture/DeleteSAMLProviderResponse.proto"
    iam
    (Proxy :: Proxy DeleteSAMLProvider)

responseUpdateSAMLProvider :: UpdateSAMLProviderResponse -> TestTree
responseUpdateSAMLProvider = res
    "UpdateSAMLProviderResponse"
    "fixture/UpdateSAMLProviderResponse.proto"
    iam
    (Proxy :: Proxy UpdateSAMLProvider)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup = res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    iam
    (Proxy :: Proxy CreateGroup)

responseListMFADevices :: ListMFADevicesResponse -> TestTree
responseListMFADevices = res
    "ListMFADevicesResponse"
    "fixture/ListMFADevicesResponse.proto"
    iam
    (Proxy :: Proxy ListMFADevices)

responseUploadServerCertificate :: UploadServerCertificateResponse -> TestTree
responseUploadServerCertificate = res
    "UploadServerCertificateResponse"
    "fixture/UploadServerCertificateResponse.proto"
    iam
    (Proxy :: Proxy UploadServerCertificate)

responseSetDefaultPolicyVersion :: SetDefaultPolicyVersionResponse -> TestTree
responseSetDefaultPolicyVersion = res
    "SetDefaultPolicyVersionResponse"
    "fixture/SetDefaultPolicyVersionResponse.proto"
    iam
    (Proxy :: Proxy SetDefaultPolicyVersion)

responseListPolicyVersions :: ListPolicyVersionsResponse -> TestTree
responseListPolicyVersions = res
    "ListPolicyVersionsResponse"
    "fixture/ListPolicyVersionsResponse.proto"
    iam
    (Proxy :: Proxy ListPolicyVersions)

responseUpdateRoleDescription :: UpdateRoleDescriptionResponse -> TestTree
responseUpdateRoleDescription = res
    "UpdateRoleDescriptionResponse"
    "fixture/UpdateRoleDescriptionResponse.proto"
    iam
    (Proxy :: Proxy UpdateRoleDescription)

responseListSAMLProviders :: ListSAMLProvidersResponse -> TestTree
responseListSAMLProviders = res
    "ListSAMLProvidersResponse"
    "fixture/ListSAMLProvidersResponse.proto"
    iam
    (Proxy :: Proxy ListSAMLProviders)

responseGetServerCertificate :: GetServerCertificateResponse -> TestTree
responseGetServerCertificate = res
    "GetServerCertificateResponse"
    "fixture/GetServerCertificateResponse.proto"
    iam
    (Proxy :: Proxy GetServerCertificate)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup = res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    iam
    (Proxy :: Proxy DeleteGroup)

responseUpdateGroup :: UpdateGroupResponse -> TestTree
responseUpdateGroup = res
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse.proto"
    iam
    (Proxy :: Proxy UpdateGroup)

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups = res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    iam
    (Proxy :: Proxy ListGroups)

responseGenerateCredentialReport :: GenerateCredentialReportResponse -> TestTree
responseGenerateCredentialReport = res
    "GenerateCredentialReportResponse"
    "fixture/GenerateCredentialReportResponse.proto"
    iam
    (Proxy :: Proxy GenerateCredentialReport)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy = res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    iam
    (Proxy :: Proxy GetPolicy)

responseUpdateLoginProfile :: UpdateLoginProfileResponse -> TestTree
responseUpdateLoginProfile = res
    "UpdateLoginProfileResponse"
    "fixture/UpdateLoginProfileResponse.proto"
    iam
    (Proxy :: Proxy UpdateLoginProfile)

responseDeleteLoginProfile :: DeleteLoginProfileResponse -> TestTree
responseDeleteLoginProfile = res
    "DeleteLoginProfileResponse"
    "fixture/DeleteLoginProfileResponse.proto"
    iam
    (Proxy :: Proxy DeleteLoginProfile)

responseGetGroup :: GetGroupResponse -> TestTree
responseGetGroup = res
    "GetGroupResponse"
    "fixture/GetGroupResponse.proto"
    iam
    (Proxy :: Proxy GetGroup)

responseDeleteServerCertificate :: DeleteServerCertificateResponse -> TestTree
responseDeleteServerCertificate = res
    "DeleteServerCertificateResponse"
    "fixture/DeleteServerCertificateResponse.proto"
    iam
    (Proxy :: Proxy DeleteServerCertificate)

responseUpdateServerCertificate :: UpdateServerCertificateResponse -> TestTree
responseUpdateServerCertificate = res
    "UpdateServerCertificateResponse"
    "fixture/UpdateServerCertificateResponse.proto"
    iam
    (Proxy :: Proxy UpdateServerCertificate)

responseListAttachedGroupPolicies :: ListAttachedGroupPoliciesResponse -> TestTree
responseListAttachedGroupPolicies = res
    "ListAttachedGroupPoliciesResponse"
    "fixture/ListAttachedGroupPoliciesResponse.proto"
    iam
    (Proxy :: Proxy ListAttachedGroupPolicies)
