{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.IAM
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.IAM where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.IAM
import Test.AWS.IAM.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testGetContextKeysForPrincipalPolicy $
--             getContextKeysForPrincipalPolicy
--
--         , testListPolicies $
--             listPolicies
--
--         , testCreatePolicy $
--             createPolicy
--
--         , testListInstanceProfilesForRole $
--             listInstanceProfilesForRole
--
--         , testAttachGroupPolicy $
--             attachGroupPolicy
--
--         , testCreateAccessKey $
--             createAccessKey
--
--         , testListSSHPublicKeys $
--             listSSHPublicKeys
--
--         , testListOpenIdConnectProviders $
--             listOpenIdConnectProviders
--
--         , testCreateVirtualMFADevice $
--             createVirtualMFADevice
--
--         , testDeleteAccountPasswordPolicy $
--             deleteAccountPasswordPolicy
--
--         , testUpdateAccountPasswordPolicy $
--             updateAccountPasswordPolicy
--
--         , testAttachRolePolicy $
--             attachRolePolicy
--
--         , testUpdateSSHPublicKey $
--             updateSSHPublicKey
--
--         , testDeleteSSHPublicKey $
--             deleteSSHPublicKey
--
--         , testGetUserPolicy $
--             getUserPolicy
--
--         , testListAttachedRolePolicies $
--             listAttachedRolePolicies
--
--         , testGetRole $
--             getRole
--
--         , testDeactivateMFADevice $
--             deactivateMFADevice
--
--         , testCreateOpenIdConnectProvider $
--             createOpenIdConnectProvider
--
--         , testDeleteVirtualMFADevice $
--             deleteVirtualMFADevice
--
--         , testListRoles $
--             listRoles
--
--         , testListUserPolicies $
--             listUserPolicies
--
--         , testUploadSSHPublicKey $
--             uploadSSHPublicKey
--
--         , testSimulateCustomPolicy $
--             simulateCustomPolicy
--
--         , testDeleteRole $
--             deleteRole
--
--         , testListUsers $
--             listUsers
--
--         , testUpdateOpenIdConnectProviderThumbprint $
--             updateOpenIdConnectProviderThumbprint
--
--         , testPutUserPolicy $
--             putUserPolicy
--
--         , testGetSSHPublicKey $
--             getSSHPublicKey
--
--         , testDetachGroupPolicy $
--             detachGroupPolicy
--
--         , testGetOpenIdConnectProvider $
--             getOpenIdConnectProvider
--
--         , testDeleteUserPolicy $
--             deleteUserPolicy
--
--         , testCreateRole $
--             createRole
--
--         , testGetCredentialReport $
--             getCredentialReport
--
--         , testGetAccountSummary $
--             getAccountSummary
--
--         , testListGroupPolicies $
--             listGroupPolicies
--
--         , testDeletePolicyVersion $
--             deletePolicyVersion
--
--         , testDeleteInstanceProfile $
--             deleteInstanceProfile
--
--         , testDetachRolePolicy $
--             detachRolePolicy
--
--         , testRemoveRoleFromInstanceProfile $
--             removeRoleFromInstanceProfile
--
--         , testCreatePolicyVersion $
--             createPolicyVersion
--
--         , testCreateInstanceProfile $
--             createInstanceProfile
--
--         , testCreateSAMLProvider $
--             createSAMLProvider
--
--         , testGetAccountAuthorizationDetails $
--             getAccountAuthorizationDetails
--
--         , testDeleteAccountAlias $
--             deleteAccountAlias
--
--         , testDetachUserPolicy $
--             detachUserPolicy
--
--         , testRemoveUserFromGroup $
--             removeUserFromGroup
--
--         , testDeleteGroupPolicy $
--             deleteGroupPolicy
--
--         , testPutGroupPolicy $
--             putGroupPolicy
--
--         , testGetLoginProfile $
--             getLoginProfile
--
--         , testGetGroupPolicy $
--             getGroupPolicy
--
--         , testChangePassword $
--             changePassword
--
--         , testListServerCertificates $
--             listServerCertificates
--
--         , testDeletePolicy $
--             deletePolicy
--
--         , testUpdateAssumeRolePolicy $
--             updateAssumeRolePolicy
--
--         , testGetInstanceProfile $
--             getInstanceProfile
--
--         , testCreateLoginProfile $
--             createLoginProfile
--
--         , testGetSAMLProvider $
--             getSAMLProvider
--
--         , testAddRoleToInstanceProfile $
--             addRoleToInstanceProfile
--
--         , testListGroupsForUser $
--             listGroupsForUser
--
--         , testListEntitiesForPolicy $
--             listEntitiesForPolicy
--
--         , testAddUserToGroup $
--             addUserToGroup
--
--         , testSimulatePrincipalPolicy $
--             simulatePrincipalPolicy
--
--         , testGetPolicyVersion $
--             getPolicyVersion
--
--         , testDeleteOpenIdConnectProvider $
--             deleteOpenIdConnectProvider
--
--         , testGetUser $
--             getUser
--
--         , testListSigningCertificates $
--             listSigningCertificates
--
--         , testDeleteSigningCertificate $
--             deleteSigningCertificate
--
--         , testUpdateSigningCertificate $
--             updateSigningCertificate
--
--         , testListAttachedUserPolicies $
--             listAttachedUserPolicies
--
--         , testRemoveClientIdFromOpenIdConnectProvider $
--             removeClientIdFromOpenIdConnectProvider
--
--         , testAttachUserPolicy $
--             attachUserPolicy
--
--         , testListVirtualMFADevices $
--             listVirtualMFADevices
--
--         , testResyncMFADevice $
--             resyncMFADevice
--
--         , testDeleteAccessKey $
--             deleteAccessKey
--
--         , testUpdateAccessKey $
--             updateAccessKey
--
--         , testListAccessKeys $
--             listAccessKeys
--
--         , testGetRolePolicy $
--             getRolePolicy
--
--         , testCreateUser $
--             createUser
--
--         , testPutRolePolicy $
--             putRolePolicy
--
--         , testGetContextKeysForCustomPolicy $
--             getContextKeysForCustomPolicy
--
--         , testUploadSigningCertificate $
--             uploadSigningCertificate
--
--         , testDeleteRolePolicy $
--             deleteRolePolicy
--
--         , testGetAccountPasswordPolicy $
--             getAccountPasswordPolicy
--
--         , testGetAccessKeyLastUsed $
--             getAccessKeyLastUsed
--
--         , testUpdateUser $
--             updateUser
--
--         , testDeleteUser $
--             deleteUser
--
--         , testAddClientIdToOpenIdConnectProvider $
--             addClientIdToOpenIdConnectProvider
--
--         , testListRolePolicies $
--             listRolePolicies
--
--         , testCreateAccountAlias $
--             createAccountAlias
--
--         , testListInstanceProfiles $
--             listInstanceProfiles
--
--         , testEnableMFADevice $
--             enableMFADevice
--
--         , testListAccountAliases $
--             listAccountAliases
--
--         , testDeleteSAMLProvider $
--             deleteSAMLProvider
--
--         , testUpdateSAMLProvider $
--             updateSAMLProvider
--
--         , testCreateGroup $
--             createGroup
--
--         , testListMFADevices $
--             listMFADevices
--
--         , testUploadServerCertificate $
--             uploadServerCertificate
--
--         , testSetDefaultPolicyVersion $
--             setDefaultPolicyVersion
--
--         , testListPolicyVersions $
--             listPolicyVersions
--
--         , testListSAMLProviders $
--             listSAMLProviders
--
--         , testGetServerCertificate $
--             getServerCertificate
--
--         , testDeleteGroup $
--             deleteGroup
--
--         , testUpdateGroup $
--             updateGroup
--
--         , testListGroups $
--             listGroups
--
--         , testGenerateCredentialReport $
--             generateCredentialReport
--
--         , testGetPolicy $
--             getPolicy
--
--         , testUpdateLoginProfile $
--             updateLoginProfile
--
--         , testDeleteLoginProfile $
--             deleteLoginProfile
--
--         , testGetGroup $
--             getGroup
--
--         , testDeleteServerCertificate $
--             deleteServerCertificate
--
--         , testUpdateServerCertificate $
--             updateServerCertificate
--
--         , testListAttachedGroupPolicies $
--             listAttachedGroupPolicies
--
--           ]

--     , testGroup "response"
--         [ testGetContextKeysForPrincipalPolicyResponse $
--             getContextKeysForPolicyResponse
--
--         , testListPoliciesResponse $
--             listPoliciesResponse
--
--         , testCreatePolicyResponse $
--             createPolicyResponse
--
--         , testListInstanceProfilesForRoleResponse $
--             listInstanceProfilesForRoleResponse
--
--         , testAttachGroupPolicyResponse $
--             attachGroupPolicyResponse
--
--         , testCreateAccessKeyResponse $
--             createAccessKeyResponse
--
--         , testListSSHPublicKeysResponse $
--             listSSHPublicKeysResponse
--
--         , testListOpenIdConnectProvidersResponse $
--             listOpenIdConnectProvidersResponse
--
--         , testCreateVirtualMFADeviceResponse $
--             createVirtualMFADeviceResponse
--
--         , testDeleteAccountPasswordPolicyResponse $
--             deleteAccountPasswordPolicyResponse
--
--         , testUpdateAccountPasswordPolicyResponse $
--             updateAccountPasswordPolicyResponse
--
--         , testAttachRolePolicyResponse $
--             attachRolePolicyResponse
--
--         , testUpdateSSHPublicKeyResponse $
--             updateSSHPublicKeyResponse
--
--         , testDeleteSSHPublicKeyResponse $
--             deleteSSHPublicKeyResponse
--
--         , testGetUserPolicyResponse $
--             getUserPolicyResponse
--
--         , testListAttachedRolePoliciesResponse $
--             listAttachedRolePoliciesResponse
--
--         , testGetRoleResponse $
--             getRoleResponse
--
--         , testDeactivateMFADeviceResponse $
--             deactivateMFADeviceResponse
--
--         , testCreateOpenIdConnectProviderResponse $
--             createOpenIdConnectProviderResponse
--
--         , testDeleteVirtualMFADeviceResponse $
--             deleteVirtualMFADeviceResponse
--
--         , testListRolesResponse $
--             listRolesResponse
--
--         , testListUserPoliciesResponse $
--             listUserPoliciesResponse
--
--         , testUploadSSHPublicKeyResponse $
--             uploadSSHPublicKeyResponse
--
--         , testSimulateCustomPolicyResponse $
--             simulatePolicyResponse
--
--         , testDeleteRoleResponse $
--             deleteRoleResponse
--
--         , testListUsersResponse $
--             listUsersResponse
--
--         , testUpdateOpenIdConnectProviderThumbprintResponse $
--             updateOpenIdConnectProviderThumbprintResponse
--
--         , testPutUserPolicyResponse $
--             putUserPolicyResponse
--
--         , testGetSSHPublicKeyResponse $
--             getSSHPublicKeyResponse
--
--         , testDetachGroupPolicyResponse $
--             detachGroupPolicyResponse
--
--         , testGetOpenIdConnectProviderResponse $
--             getOpenIdConnectProviderResponse
--
--         , testDeleteUserPolicyResponse $
--             deleteUserPolicyResponse
--
--         , testCreateRoleResponse $
--             createRoleResponse
--
--         , testGetCredentialReportResponse $
--             getCredentialReportResponse
--
--         , testGetAccountSummaryResponse $
--             getAccountSummaryResponse
--
--         , testListGroupPoliciesResponse $
--             listGroupPoliciesResponse
--
--         , testDeletePolicyVersionResponse $
--             deletePolicyVersionResponse
--
--         , testDeleteInstanceProfileResponse $
--             deleteInstanceProfileResponse
--
--         , testDetachRolePolicyResponse $
--             detachRolePolicyResponse
--
--         , testRemoveRoleFromInstanceProfileResponse $
--             removeRoleFromInstanceProfileResponse
--
--         , testCreatePolicyVersionResponse $
--             createPolicyVersionResponse
--
--         , testCreateInstanceProfileResponse $
--             createInstanceProfileResponse
--
--         , testCreateSAMLProviderResponse $
--             createSAMLProviderResponse
--
--         , testGetAccountAuthorizationDetailsResponse $
--             getAccountAuthorizationDetailsResponse
--
--         , testDeleteAccountAliasResponse $
--             deleteAccountAliasResponse
--
--         , testDetachUserPolicyResponse $
--             detachUserPolicyResponse
--
--         , testRemoveUserFromGroupResponse $
--             removeUserFromGroupResponse
--
--         , testDeleteGroupPolicyResponse $
--             deleteGroupPolicyResponse
--
--         , testPutGroupPolicyResponse $
--             putGroupPolicyResponse
--
--         , testGetLoginProfileResponse $
--             getLoginProfileResponse
--
--         , testGetGroupPolicyResponse $
--             getGroupPolicyResponse
--
--         , testChangePasswordResponse $
--             changePasswordResponse
--
--         , testListServerCertificatesResponse $
--             listServerCertificatesResponse
--
--         , testDeletePolicyResponse $
--             deletePolicyResponse
--
--         , testUpdateAssumeRolePolicyResponse $
--             updateAssumeRolePolicyResponse
--
--         , testGetInstanceProfileResponse $
--             getInstanceProfileResponse
--
--         , testCreateLoginProfileResponse $
--             createLoginProfileResponse
--
--         , testGetSAMLProviderResponse $
--             getSAMLProviderResponse
--
--         , testAddRoleToInstanceProfileResponse $
--             addRoleToInstanceProfileResponse
--
--         , testListGroupsForUserResponse $
--             listGroupsForUserResponse
--
--         , testListEntitiesForPolicyResponse $
--             listEntitiesForPolicyResponse
--
--         , testAddUserToGroupResponse $
--             addUserToGroupResponse
--
--         , testSimulatePrincipalPolicyResponse $
--             simulatePolicyResponse
--
--         , testGetPolicyVersionResponse $
--             getPolicyVersionResponse
--
--         , testDeleteOpenIdConnectProviderResponse $
--             deleteOpenIdConnectProviderResponse
--
--         , testGetUserResponse $
--             getUserResponse
--
--         , testListSigningCertificatesResponse $
--             listSigningCertificatesResponse
--
--         , testDeleteSigningCertificateResponse $
--             deleteSigningCertificateResponse
--
--         , testUpdateSigningCertificateResponse $
--             updateSigningCertificateResponse
--
--         , testListAttachedUserPoliciesResponse $
--             listAttachedUserPoliciesResponse
--
--         , testRemoveClientIdFromOpenIdConnectProviderResponse $
--             removeClientIdFromOpenIdConnectProviderResponse
--
--         , testAttachUserPolicyResponse $
--             attachUserPolicyResponse
--
--         , testListVirtualMFADevicesResponse $
--             listVirtualMFADevicesResponse
--
--         , testResyncMFADeviceResponse $
--             resyncMFADeviceResponse
--
--         , testDeleteAccessKeyResponse $
--             deleteAccessKeyResponse
--
--         , testUpdateAccessKeyResponse $
--             updateAccessKeyResponse
--
--         , testListAccessKeysResponse $
--             listAccessKeysResponse
--
--         , testGetRolePolicyResponse $
--             getRolePolicyResponse
--
--         , testCreateUserResponse $
--             createUserResponse
--
--         , testPutRolePolicyResponse $
--             putRolePolicyResponse
--
--         , testGetContextKeysForCustomPolicyResponse $
--             getContextKeysForPolicyResponse
--
--         , testUploadSigningCertificateResponse $
--             uploadSigningCertificateResponse
--
--         , testDeleteRolePolicyResponse $
--             deleteRolePolicyResponse
--
--         , testGetAccountPasswordPolicyResponse $
--             getAccountPasswordPolicyResponse
--
--         , testGetAccessKeyLastUsedResponse $
--             getAccessKeyLastUsedResponse
--
--         , testUpdateUserResponse $
--             updateUserResponse
--
--         , testDeleteUserResponse $
--             deleteUserResponse
--
--         , testAddClientIdToOpenIdConnectProviderResponse $
--             addClientIdToOpenIdConnectProviderResponse
--
--         , testListRolePoliciesResponse $
--             listRolePoliciesResponse
--
--         , testCreateAccountAliasResponse $
--             createAccountAliasResponse
--
--         , testListInstanceProfilesResponse $
--             listInstanceProfilesResponse
--
--         , testEnableMFADeviceResponse $
--             enableMFADeviceResponse
--
--         , testListAccountAliasesResponse $
--             listAccountAliasesResponse
--
--         , testDeleteSAMLProviderResponse $
--             deleteSAMLProviderResponse
--
--         , testUpdateSAMLProviderResponse $
--             updateSAMLProviderResponse
--
--         , testCreateGroupResponse $
--             createGroupResponse
--
--         , testListMFADevicesResponse $
--             listMFADevicesResponse
--
--         , testUploadServerCertificateResponse $
--             uploadServerCertificateResponse
--
--         , testSetDefaultPolicyVersionResponse $
--             setDefaultPolicyVersionResponse
--
--         , testListPolicyVersionsResponse $
--             listPolicyVersionsResponse
--
--         , testListSAMLProvidersResponse $
--             listSAMLProvidersResponse
--
--         , testGetServerCertificateResponse $
--             getServerCertificateResponse
--
--         , testDeleteGroupResponse $
--             deleteGroupResponse
--
--         , testUpdateGroupResponse $
--             updateGroupResponse
--
--         , testListGroupsResponse $
--             listGroupsResponse
--
--         , testGenerateCredentialReportResponse $
--             generateCredentialReportResponse
--
--         , testGetPolicyResponse $
--             getPolicyResponse
--
--         , testUpdateLoginProfileResponse $
--             updateLoginProfileResponse
--
--         , testDeleteLoginProfileResponse $
--             deleteLoginProfileResponse
--
--         , testGetGroupResponse $
--             getGroupResponse
--
--         , testDeleteServerCertificateResponse $
--             deleteServerCertificateResponse
--
--         , testUpdateServerCertificateResponse $
--             updateServerCertificateResponse
--
--         , testListAttachedGroupPoliciesResponse $
--             listAttachedGroupPoliciesResponse
--
--           ]
--     ]

-- Requests

testGetContextKeysForPrincipalPolicy :: GetContextKeysForPrincipalPolicy -> TestTree
testGetContextKeysForPrincipalPolicy = req
    "GetContextKeysForPrincipalPolicy"
    "fixture/GetContextKeysForPrincipalPolicy.yaml"

testListPolicies :: ListPolicies -> TestTree
testListPolicies = req
    "ListPolicies"
    "fixture/ListPolicies.yaml"

testCreatePolicy :: CreatePolicy -> TestTree
testCreatePolicy = req
    "CreatePolicy"
    "fixture/CreatePolicy.yaml"

testListInstanceProfilesForRole :: ListInstanceProfilesForRole -> TestTree
testListInstanceProfilesForRole = req
    "ListInstanceProfilesForRole"
    "fixture/ListInstanceProfilesForRole.yaml"

testAttachGroupPolicy :: AttachGroupPolicy -> TestTree
testAttachGroupPolicy = req
    "AttachGroupPolicy"
    "fixture/AttachGroupPolicy.yaml"

testCreateAccessKey :: CreateAccessKey -> TestTree
testCreateAccessKey = req
    "CreateAccessKey"
    "fixture/CreateAccessKey.yaml"

testListSSHPublicKeys :: ListSSHPublicKeys -> TestTree
testListSSHPublicKeys = req
    "ListSSHPublicKeys"
    "fixture/ListSSHPublicKeys.yaml"

testListOpenIdConnectProviders :: ListOpenIdConnectProviders -> TestTree
testListOpenIdConnectProviders = req
    "ListOpenIdConnectProviders"
    "fixture/ListOpenIdConnectProviders.yaml"

testCreateVirtualMFADevice :: CreateVirtualMFADevice -> TestTree
testCreateVirtualMFADevice = req
    "CreateVirtualMFADevice"
    "fixture/CreateVirtualMFADevice.yaml"

testDeleteAccountPasswordPolicy :: DeleteAccountPasswordPolicy -> TestTree
testDeleteAccountPasswordPolicy = req
    "DeleteAccountPasswordPolicy"
    "fixture/DeleteAccountPasswordPolicy.yaml"

testUpdateAccountPasswordPolicy :: UpdateAccountPasswordPolicy -> TestTree
testUpdateAccountPasswordPolicy = req
    "UpdateAccountPasswordPolicy"
    "fixture/UpdateAccountPasswordPolicy.yaml"

testAttachRolePolicy :: AttachRolePolicy -> TestTree
testAttachRolePolicy = req
    "AttachRolePolicy"
    "fixture/AttachRolePolicy.yaml"

testUpdateSSHPublicKey :: UpdateSSHPublicKey -> TestTree
testUpdateSSHPublicKey = req
    "UpdateSSHPublicKey"
    "fixture/UpdateSSHPublicKey.yaml"

testDeleteSSHPublicKey :: DeleteSSHPublicKey -> TestTree
testDeleteSSHPublicKey = req
    "DeleteSSHPublicKey"
    "fixture/DeleteSSHPublicKey.yaml"

testGetUserPolicy :: GetUserPolicy -> TestTree
testGetUserPolicy = req
    "GetUserPolicy"
    "fixture/GetUserPolicy.yaml"

testListAttachedRolePolicies :: ListAttachedRolePolicies -> TestTree
testListAttachedRolePolicies = req
    "ListAttachedRolePolicies"
    "fixture/ListAttachedRolePolicies.yaml"

testGetRole :: GetRole -> TestTree
testGetRole = req
    "GetRole"
    "fixture/GetRole.yaml"

testDeactivateMFADevice :: DeactivateMFADevice -> TestTree
testDeactivateMFADevice = req
    "DeactivateMFADevice"
    "fixture/DeactivateMFADevice.yaml"

testCreateOpenIdConnectProvider :: CreateOpenIdConnectProvider -> TestTree
testCreateOpenIdConnectProvider = req
    "CreateOpenIdConnectProvider"
    "fixture/CreateOpenIdConnectProvider.yaml"

testDeleteVirtualMFADevice :: DeleteVirtualMFADevice -> TestTree
testDeleteVirtualMFADevice = req
    "DeleteVirtualMFADevice"
    "fixture/DeleteVirtualMFADevice.yaml"

testListRoles :: ListRoles -> TestTree
testListRoles = req
    "ListRoles"
    "fixture/ListRoles.yaml"

testListUserPolicies :: ListUserPolicies -> TestTree
testListUserPolicies = req
    "ListUserPolicies"
    "fixture/ListUserPolicies.yaml"

testUploadSSHPublicKey :: UploadSSHPublicKey -> TestTree
testUploadSSHPublicKey = req
    "UploadSSHPublicKey"
    "fixture/UploadSSHPublicKey.yaml"

testSimulateCustomPolicy :: SimulateCustomPolicy -> TestTree
testSimulateCustomPolicy = req
    "SimulateCustomPolicy"
    "fixture/SimulateCustomPolicy.yaml"

testDeleteRole :: DeleteRole -> TestTree
testDeleteRole = req
    "DeleteRole"
    "fixture/DeleteRole.yaml"

testListUsers :: ListUsers -> TestTree
testListUsers = req
    "ListUsers"
    "fixture/ListUsers.yaml"

testUpdateOpenIdConnectProviderThumbprint :: UpdateOpenIdConnectProviderThumbprint -> TestTree
testUpdateOpenIdConnectProviderThumbprint = req
    "UpdateOpenIdConnectProviderThumbprint"
    "fixture/UpdateOpenIdConnectProviderThumbprint.yaml"

testPutUserPolicy :: PutUserPolicy -> TestTree
testPutUserPolicy = req
    "PutUserPolicy"
    "fixture/PutUserPolicy.yaml"

testGetSSHPublicKey :: GetSSHPublicKey -> TestTree
testGetSSHPublicKey = req
    "GetSSHPublicKey"
    "fixture/GetSSHPublicKey.yaml"

testDetachGroupPolicy :: DetachGroupPolicy -> TestTree
testDetachGroupPolicy = req
    "DetachGroupPolicy"
    "fixture/DetachGroupPolicy.yaml"

testGetOpenIdConnectProvider :: GetOpenIdConnectProvider -> TestTree
testGetOpenIdConnectProvider = req
    "GetOpenIdConnectProvider"
    "fixture/GetOpenIdConnectProvider.yaml"

testDeleteUserPolicy :: DeleteUserPolicy -> TestTree
testDeleteUserPolicy = req
    "DeleteUserPolicy"
    "fixture/DeleteUserPolicy.yaml"

testCreateRole :: CreateRole -> TestTree
testCreateRole = req
    "CreateRole"
    "fixture/CreateRole.yaml"

testGetCredentialReport :: GetCredentialReport -> TestTree
testGetCredentialReport = req
    "GetCredentialReport"
    "fixture/GetCredentialReport.yaml"

testGetAccountSummary :: GetAccountSummary -> TestTree
testGetAccountSummary = req
    "GetAccountSummary"
    "fixture/GetAccountSummary.yaml"

testListGroupPolicies :: ListGroupPolicies -> TestTree
testListGroupPolicies = req
    "ListGroupPolicies"
    "fixture/ListGroupPolicies.yaml"

testDeletePolicyVersion :: DeletePolicyVersion -> TestTree
testDeletePolicyVersion = req
    "DeletePolicyVersion"
    "fixture/DeletePolicyVersion.yaml"

testDeleteInstanceProfile :: DeleteInstanceProfile -> TestTree
testDeleteInstanceProfile = req
    "DeleteInstanceProfile"
    "fixture/DeleteInstanceProfile.yaml"

testDetachRolePolicy :: DetachRolePolicy -> TestTree
testDetachRolePolicy = req
    "DetachRolePolicy"
    "fixture/DetachRolePolicy.yaml"

testRemoveRoleFromInstanceProfile :: RemoveRoleFromInstanceProfile -> TestTree
testRemoveRoleFromInstanceProfile = req
    "RemoveRoleFromInstanceProfile"
    "fixture/RemoveRoleFromInstanceProfile.yaml"

testCreatePolicyVersion :: CreatePolicyVersion -> TestTree
testCreatePolicyVersion = req
    "CreatePolicyVersion"
    "fixture/CreatePolicyVersion.yaml"

testCreateInstanceProfile :: CreateInstanceProfile -> TestTree
testCreateInstanceProfile = req
    "CreateInstanceProfile"
    "fixture/CreateInstanceProfile.yaml"

testCreateSAMLProvider :: CreateSAMLProvider -> TestTree
testCreateSAMLProvider = req
    "CreateSAMLProvider"
    "fixture/CreateSAMLProvider.yaml"

testGetAccountAuthorizationDetails :: GetAccountAuthorizationDetails -> TestTree
testGetAccountAuthorizationDetails = req
    "GetAccountAuthorizationDetails"
    "fixture/GetAccountAuthorizationDetails.yaml"

testDeleteAccountAlias :: DeleteAccountAlias -> TestTree
testDeleteAccountAlias = req
    "DeleteAccountAlias"
    "fixture/DeleteAccountAlias.yaml"

testDetachUserPolicy :: DetachUserPolicy -> TestTree
testDetachUserPolicy = req
    "DetachUserPolicy"
    "fixture/DetachUserPolicy.yaml"

testRemoveUserFromGroup :: RemoveUserFromGroup -> TestTree
testRemoveUserFromGroup = req
    "RemoveUserFromGroup"
    "fixture/RemoveUserFromGroup.yaml"

testDeleteGroupPolicy :: DeleteGroupPolicy -> TestTree
testDeleteGroupPolicy = req
    "DeleteGroupPolicy"
    "fixture/DeleteGroupPolicy.yaml"

testPutGroupPolicy :: PutGroupPolicy -> TestTree
testPutGroupPolicy = req
    "PutGroupPolicy"
    "fixture/PutGroupPolicy.yaml"

testGetLoginProfile :: GetLoginProfile -> TestTree
testGetLoginProfile = req
    "GetLoginProfile"
    "fixture/GetLoginProfile.yaml"

testGetGroupPolicy :: GetGroupPolicy -> TestTree
testGetGroupPolicy = req
    "GetGroupPolicy"
    "fixture/GetGroupPolicy.yaml"

testChangePassword :: ChangePassword -> TestTree
testChangePassword = req
    "ChangePassword"
    "fixture/ChangePassword.yaml"

testListServerCertificates :: ListServerCertificates -> TestTree
testListServerCertificates = req
    "ListServerCertificates"
    "fixture/ListServerCertificates.yaml"

testDeletePolicy :: DeletePolicy -> TestTree
testDeletePolicy = req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

testUpdateAssumeRolePolicy :: UpdateAssumeRolePolicy -> TestTree
testUpdateAssumeRolePolicy = req
    "UpdateAssumeRolePolicy"
    "fixture/UpdateAssumeRolePolicy.yaml"

testGetInstanceProfile :: GetInstanceProfile -> TestTree
testGetInstanceProfile = req
    "GetInstanceProfile"
    "fixture/GetInstanceProfile.yaml"

testCreateLoginProfile :: CreateLoginProfile -> TestTree
testCreateLoginProfile = req
    "CreateLoginProfile"
    "fixture/CreateLoginProfile.yaml"

testGetSAMLProvider :: GetSAMLProvider -> TestTree
testGetSAMLProvider = req
    "GetSAMLProvider"
    "fixture/GetSAMLProvider.yaml"

testAddRoleToInstanceProfile :: AddRoleToInstanceProfile -> TestTree
testAddRoleToInstanceProfile = req
    "AddRoleToInstanceProfile"
    "fixture/AddRoleToInstanceProfile.yaml"

testListGroupsForUser :: ListGroupsForUser -> TestTree
testListGroupsForUser = req
    "ListGroupsForUser"
    "fixture/ListGroupsForUser.yaml"

testListEntitiesForPolicy :: ListEntitiesForPolicy -> TestTree
testListEntitiesForPolicy = req
    "ListEntitiesForPolicy"
    "fixture/ListEntitiesForPolicy.yaml"

testAddUserToGroup :: AddUserToGroup -> TestTree
testAddUserToGroup = req
    "AddUserToGroup"
    "fixture/AddUserToGroup.yaml"

testSimulatePrincipalPolicy :: SimulatePrincipalPolicy -> TestTree
testSimulatePrincipalPolicy = req
    "SimulatePrincipalPolicy"
    "fixture/SimulatePrincipalPolicy.yaml"

testGetPolicyVersion :: GetPolicyVersion -> TestTree
testGetPolicyVersion = req
    "GetPolicyVersion"
    "fixture/GetPolicyVersion.yaml"

testDeleteOpenIdConnectProvider :: DeleteOpenIdConnectProvider -> TestTree
testDeleteOpenIdConnectProvider = req
    "DeleteOpenIdConnectProvider"
    "fixture/DeleteOpenIdConnectProvider.yaml"

testGetUser :: GetUser -> TestTree
testGetUser = req
    "GetUser"
    "fixture/GetUser.yaml"

testListSigningCertificates :: ListSigningCertificates -> TestTree
testListSigningCertificates = req
    "ListSigningCertificates"
    "fixture/ListSigningCertificates.yaml"

testDeleteSigningCertificate :: DeleteSigningCertificate -> TestTree
testDeleteSigningCertificate = req
    "DeleteSigningCertificate"
    "fixture/DeleteSigningCertificate.yaml"

testUpdateSigningCertificate :: UpdateSigningCertificate -> TestTree
testUpdateSigningCertificate = req
    "UpdateSigningCertificate"
    "fixture/UpdateSigningCertificate.yaml"

testListAttachedUserPolicies :: ListAttachedUserPolicies -> TestTree
testListAttachedUserPolicies = req
    "ListAttachedUserPolicies"
    "fixture/ListAttachedUserPolicies.yaml"

testRemoveClientIdFromOpenIdConnectProvider :: RemoveClientIdFromOpenIdConnectProvider -> TestTree
testRemoveClientIdFromOpenIdConnectProvider = req
    "RemoveClientIdFromOpenIdConnectProvider"
    "fixture/RemoveClientIdFromOpenIdConnectProvider.yaml"

testAttachUserPolicy :: AttachUserPolicy -> TestTree
testAttachUserPolicy = req
    "AttachUserPolicy"
    "fixture/AttachUserPolicy.yaml"

testListVirtualMFADevices :: ListVirtualMFADevices -> TestTree
testListVirtualMFADevices = req
    "ListVirtualMFADevices"
    "fixture/ListVirtualMFADevices.yaml"

testResyncMFADevice :: ResyncMFADevice -> TestTree
testResyncMFADevice = req
    "ResyncMFADevice"
    "fixture/ResyncMFADevice.yaml"

testDeleteAccessKey :: DeleteAccessKey -> TestTree
testDeleteAccessKey = req
    "DeleteAccessKey"
    "fixture/DeleteAccessKey.yaml"

testUpdateAccessKey :: UpdateAccessKey -> TestTree
testUpdateAccessKey = req
    "UpdateAccessKey"
    "fixture/UpdateAccessKey.yaml"

testListAccessKeys :: ListAccessKeys -> TestTree
testListAccessKeys = req
    "ListAccessKeys"
    "fixture/ListAccessKeys.yaml"

testGetRolePolicy :: GetRolePolicy -> TestTree
testGetRolePolicy = req
    "GetRolePolicy"
    "fixture/GetRolePolicy.yaml"

testCreateUser :: CreateUser -> TestTree
testCreateUser = req
    "CreateUser"
    "fixture/CreateUser.yaml"

testPutRolePolicy :: PutRolePolicy -> TestTree
testPutRolePolicy = req
    "PutRolePolicy"
    "fixture/PutRolePolicy.yaml"

testGetContextKeysForCustomPolicy :: GetContextKeysForCustomPolicy -> TestTree
testGetContextKeysForCustomPolicy = req
    "GetContextKeysForCustomPolicy"
    "fixture/GetContextKeysForCustomPolicy.yaml"

testUploadSigningCertificate :: UploadSigningCertificate -> TestTree
testUploadSigningCertificate = req
    "UploadSigningCertificate"
    "fixture/UploadSigningCertificate.yaml"

testDeleteRolePolicy :: DeleteRolePolicy -> TestTree
testDeleteRolePolicy = req
    "DeleteRolePolicy"
    "fixture/DeleteRolePolicy.yaml"

testGetAccountPasswordPolicy :: GetAccountPasswordPolicy -> TestTree
testGetAccountPasswordPolicy = req
    "GetAccountPasswordPolicy"
    "fixture/GetAccountPasswordPolicy.yaml"

testGetAccessKeyLastUsed :: GetAccessKeyLastUsed -> TestTree
testGetAccessKeyLastUsed = req
    "GetAccessKeyLastUsed"
    "fixture/GetAccessKeyLastUsed.yaml"

testUpdateUser :: UpdateUser -> TestTree
testUpdateUser = req
    "UpdateUser"
    "fixture/UpdateUser.yaml"

testDeleteUser :: DeleteUser -> TestTree
testDeleteUser = req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

testAddClientIdToOpenIdConnectProvider :: AddClientIdToOpenIdConnectProvider -> TestTree
testAddClientIdToOpenIdConnectProvider = req
    "AddClientIdToOpenIdConnectProvider"
    "fixture/AddClientIdToOpenIdConnectProvider.yaml"

testListRolePolicies :: ListRolePolicies -> TestTree
testListRolePolicies = req
    "ListRolePolicies"
    "fixture/ListRolePolicies.yaml"

testCreateAccountAlias :: CreateAccountAlias -> TestTree
testCreateAccountAlias = req
    "CreateAccountAlias"
    "fixture/CreateAccountAlias.yaml"

testListInstanceProfiles :: ListInstanceProfiles -> TestTree
testListInstanceProfiles = req
    "ListInstanceProfiles"
    "fixture/ListInstanceProfiles.yaml"

testEnableMFADevice :: EnableMFADevice -> TestTree
testEnableMFADevice = req
    "EnableMFADevice"
    "fixture/EnableMFADevice.yaml"

testListAccountAliases :: ListAccountAliases -> TestTree
testListAccountAliases = req
    "ListAccountAliases"
    "fixture/ListAccountAliases.yaml"

testDeleteSAMLProvider :: DeleteSAMLProvider -> TestTree
testDeleteSAMLProvider = req
    "DeleteSAMLProvider"
    "fixture/DeleteSAMLProvider.yaml"

testUpdateSAMLProvider :: UpdateSAMLProvider -> TestTree
testUpdateSAMLProvider = req
    "UpdateSAMLProvider"
    "fixture/UpdateSAMLProvider.yaml"

testCreateGroup :: CreateGroup -> TestTree
testCreateGroup = req
    "CreateGroup"
    "fixture/CreateGroup.yaml"

testListMFADevices :: ListMFADevices -> TestTree
testListMFADevices = req
    "ListMFADevices"
    "fixture/ListMFADevices.yaml"

testUploadServerCertificate :: UploadServerCertificate -> TestTree
testUploadServerCertificate = req
    "UploadServerCertificate"
    "fixture/UploadServerCertificate.yaml"

testSetDefaultPolicyVersion :: SetDefaultPolicyVersion -> TestTree
testSetDefaultPolicyVersion = req
    "SetDefaultPolicyVersion"
    "fixture/SetDefaultPolicyVersion.yaml"

testListPolicyVersions :: ListPolicyVersions -> TestTree
testListPolicyVersions = req
    "ListPolicyVersions"
    "fixture/ListPolicyVersions.yaml"

testListSAMLProviders :: ListSAMLProviders -> TestTree
testListSAMLProviders = req
    "ListSAMLProviders"
    "fixture/ListSAMLProviders.yaml"

testGetServerCertificate :: GetServerCertificate -> TestTree
testGetServerCertificate = req
    "GetServerCertificate"
    "fixture/GetServerCertificate.yaml"

testDeleteGroup :: DeleteGroup -> TestTree
testDeleteGroup = req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

testUpdateGroup :: UpdateGroup -> TestTree
testUpdateGroup = req
    "UpdateGroup"
    "fixture/UpdateGroup.yaml"

testListGroups :: ListGroups -> TestTree
testListGroups = req
    "ListGroups"
    "fixture/ListGroups.yaml"

testGenerateCredentialReport :: GenerateCredentialReport -> TestTree
testGenerateCredentialReport = req
    "GenerateCredentialReport"
    "fixture/GenerateCredentialReport.yaml"

testGetPolicy :: GetPolicy -> TestTree
testGetPolicy = req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

testUpdateLoginProfile :: UpdateLoginProfile -> TestTree
testUpdateLoginProfile = req
    "UpdateLoginProfile"
    "fixture/UpdateLoginProfile.yaml"

testDeleteLoginProfile :: DeleteLoginProfile -> TestTree
testDeleteLoginProfile = req
    "DeleteLoginProfile"
    "fixture/DeleteLoginProfile.yaml"

testGetGroup :: GetGroup -> TestTree
testGetGroup = req
    "GetGroup"
    "fixture/GetGroup.yaml"

testDeleteServerCertificate :: DeleteServerCertificate -> TestTree
testDeleteServerCertificate = req
    "DeleteServerCertificate"
    "fixture/DeleteServerCertificate.yaml"

testUpdateServerCertificate :: UpdateServerCertificate -> TestTree
testUpdateServerCertificate = req
    "UpdateServerCertificate"
    "fixture/UpdateServerCertificate.yaml"

testListAttachedGroupPolicies :: ListAttachedGroupPolicies -> TestTree
testListAttachedGroupPolicies = req
    "ListAttachedGroupPolicies"
    "fixture/ListAttachedGroupPolicies.yaml"

-- Responses

testGetContextKeysForPrincipalPolicyResponse :: GetContextKeysForPolicyResponse -> TestTree
testGetContextKeysForPrincipalPolicyResponse = res
    "GetContextKeysForPrincipalPolicyResponse"
    "fixture/GetContextKeysForPrincipalPolicyResponse.proto"
    iAM
    (Proxy :: Proxy GetContextKeysForPrincipalPolicy)

testListPoliciesResponse :: ListPoliciesResponse -> TestTree
testListPoliciesResponse = res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    iAM
    (Proxy :: Proxy ListPolicies)

testCreatePolicyResponse :: CreatePolicyResponse -> TestTree
testCreatePolicyResponse = res
    "CreatePolicyResponse"
    "fixture/CreatePolicyResponse.proto"
    iAM
    (Proxy :: Proxy CreatePolicy)

testListInstanceProfilesForRoleResponse :: ListInstanceProfilesForRoleResponse -> TestTree
testListInstanceProfilesForRoleResponse = res
    "ListInstanceProfilesForRoleResponse"
    "fixture/ListInstanceProfilesForRoleResponse.proto"
    iAM
    (Proxy :: Proxy ListInstanceProfilesForRole)

testAttachGroupPolicyResponse :: AttachGroupPolicyResponse -> TestTree
testAttachGroupPolicyResponse = res
    "AttachGroupPolicyResponse"
    "fixture/AttachGroupPolicyResponse.proto"
    iAM
    (Proxy :: Proxy AttachGroupPolicy)

testCreateAccessKeyResponse :: CreateAccessKeyResponse -> TestTree
testCreateAccessKeyResponse = res
    "CreateAccessKeyResponse"
    "fixture/CreateAccessKeyResponse.proto"
    iAM
    (Proxy :: Proxy CreateAccessKey)

testListSSHPublicKeysResponse :: ListSSHPublicKeysResponse -> TestTree
testListSSHPublicKeysResponse = res
    "ListSSHPublicKeysResponse"
    "fixture/ListSSHPublicKeysResponse.proto"
    iAM
    (Proxy :: Proxy ListSSHPublicKeys)

testListOpenIdConnectProvidersResponse :: ListOpenIdConnectProvidersResponse -> TestTree
testListOpenIdConnectProvidersResponse = res
    "ListOpenIdConnectProvidersResponse"
    "fixture/ListOpenIdConnectProvidersResponse.proto"
    iAM
    (Proxy :: Proxy ListOpenIdConnectProviders)

testCreateVirtualMFADeviceResponse :: CreateVirtualMFADeviceResponse -> TestTree
testCreateVirtualMFADeviceResponse = res
    "CreateVirtualMFADeviceResponse"
    "fixture/CreateVirtualMFADeviceResponse.proto"
    iAM
    (Proxy :: Proxy CreateVirtualMFADevice)

testDeleteAccountPasswordPolicyResponse :: DeleteAccountPasswordPolicyResponse -> TestTree
testDeleteAccountPasswordPolicyResponse = res
    "DeleteAccountPasswordPolicyResponse"
    "fixture/DeleteAccountPasswordPolicyResponse.proto"
    iAM
    (Proxy :: Proxy DeleteAccountPasswordPolicy)

testUpdateAccountPasswordPolicyResponse :: UpdateAccountPasswordPolicyResponse -> TestTree
testUpdateAccountPasswordPolicyResponse = res
    "UpdateAccountPasswordPolicyResponse"
    "fixture/UpdateAccountPasswordPolicyResponse.proto"
    iAM
    (Proxy :: Proxy UpdateAccountPasswordPolicy)

testAttachRolePolicyResponse :: AttachRolePolicyResponse -> TestTree
testAttachRolePolicyResponse = res
    "AttachRolePolicyResponse"
    "fixture/AttachRolePolicyResponse.proto"
    iAM
    (Proxy :: Proxy AttachRolePolicy)

testUpdateSSHPublicKeyResponse :: UpdateSSHPublicKeyResponse -> TestTree
testUpdateSSHPublicKeyResponse = res
    "UpdateSSHPublicKeyResponse"
    "fixture/UpdateSSHPublicKeyResponse.proto"
    iAM
    (Proxy :: Proxy UpdateSSHPublicKey)

testDeleteSSHPublicKeyResponse :: DeleteSSHPublicKeyResponse -> TestTree
testDeleteSSHPublicKeyResponse = res
    "DeleteSSHPublicKeyResponse"
    "fixture/DeleteSSHPublicKeyResponse.proto"
    iAM
    (Proxy :: Proxy DeleteSSHPublicKey)

testGetUserPolicyResponse :: GetUserPolicyResponse -> TestTree
testGetUserPolicyResponse = res
    "GetUserPolicyResponse"
    "fixture/GetUserPolicyResponse.proto"
    iAM
    (Proxy :: Proxy GetUserPolicy)

testListAttachedRolePoliciesResponse :: ListAttachedRolePoliciesResponse -> TestTree
testListAttachedRolePoliciesResponse = res
    "ListAttachedRolePoliciesResponse"
    "fixture/ListAttachedRolePoliciesResponse.proto"
    iAM
    (Proxy :: Proxy ListAttachedRolePolicies)

testGetRoleResponse :: GetRoleResponse -> TestTree
testGetRoleResponse = res
    "GetRoleResponse"
    "fixture/GetRoleResponse.proto"
    iAM
    (Proxy :: Proxy GetRole)

testDeactivateMFADeviceResponse :: DeactivateMFADeviceResponse -> TestTree
testDeactivateMFADeviceResponse = res
    "DeactivateMFADeviceResponse"
    "fixture/DeactivateMFADeviceResponse.proto"
    iAM
    (Proxy :: Proxy DeactivateMFADevice)

testCreateOpenIdConnectProviderResponse :: CreateOpenIdConnectProviderResponse -> TestTree
testCreateOpenIdConnectProviderResponse = res
    "CreateOpenIdConnectProviderResponse"
    "fixture/CreateOpenIdConnectProviderResponse.proto"
    iAM
    (Proxy :: Proxy CreateOpenIdConnectProvider)

testDeleteVirtualMFADeviceResponse :: DeleteVirtualMFADeviceResponse -> TestTree
testDeleteVirtualMFADeviceResponse = res
    "DeleteVirtualMFADeviceResponse"
    "fixture/DeleteVirtualMFADeviceResponse.proto"
    iAM
    (Proxy :: Proxy DeleteVirtualMFADevice)

testListRolesResponse :: ListRolesResponse -> TestTree
testListRolesResponse = res
    "ListRolesResponse"
    "fixture/ListRolesResponse.proto"
    iAM
    (Proxy :: Proxy ListRoles)

testListUserPoliciesResponse :: ListUserPoliciesResponse -> TestTree
testListUserPoliciesResponse = res
    "ListUserPoliciesResponse"
    "fixture/ListUserPoliciesResponse.proto"
    iAM
    (Proxy :: Proxy ListUserPolicies)

testUploadSSHPublicKeyResponse :: UploadSSHPublicKeyResponse -> TestTree
testUploadSSHPublicKeyResponse = res
    "UploadSSHPublicKeyResponse"
    "fixture/UploadSSHPublicKeyResponse.proto"
    iAM
    (Proxy :: Proxy UploadSSHPublicKey)

testSimulateCustomPolicyResponse :: SimulatePolicyResponse -> TestTree
testSimulateCustomPolicyResponse = res
    "SimulateCustomPolicyResponse"
    "fixture/SimulateCustomPolicyResponse.proto"
    iAM
    (Proxy :: Proxy SimulateCustomPolicy)

testDeleteRoleResponse :: DeleteRoleResponse -> TestTree
testDeleteRoleResponse = res
    "DeleteRoleResponse"
    "fixture/DeleteRoleResponse.proto"
    iAM
    (Proxy :: Proxy DeleteRole)

testListUsersResponse :: ListUsersResponse -> TestTree
testListUsersResponse = res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    iAM
    (Proxy :: Proxy ListUsers)

testUpdateOpenIdConnectProviderThumbprintResponse :: UpdateOpenIdConnectProviderThumbprintResponse -> TestTree
testUpdateOpenIdConnectProviderThumbprintResponse = res
    "UpdateOpenIdConnectProviderThumbprintResponse"
    "fixture/UpdateOpenIdConnectProviderThumbprintResponse.proto"
    iAM
    (Proxy :: Proxy UpdateOpenIdConnectProviderThumbprint)

testPutUserPolicyResponse :: PutUserPolicyResponse -> TestTree
testPutUserPolicyResponse = res
    "PutUserPolicyResponse"
    "fixture/PutUserPolicyResponse.proto"
    iAM
    (Proxy :: Proxy PutUserPolicy)

testGetSSHPublicKeyResponse :: GetSSHPublicKeyResponse -> TestTree
testGetSSHPublicKeyResponse = res
    "GetSSHPublicKeyResponse"
    "fixture/GetSSHPublicKeyResponse.proto"
    iAM
    (Proxy :: Proxy GetSSHPublicKey)

testDetachGroupPolicyResponse :: DetachGroupPolicyResponse -> TestTree
testDetachGroupPolicyResponse = res
    "DetachGroupPolicyResponse"
    "fixture/DetachGroupPolicyResponse.proto"
    iAM
    (Proxy :: Proxy DetachGroupPolicy)

testGetOpenIdConnectProviderResponse :: GetOpenIdConnectProviderResponse -> TestTree
testGetOpenIdConnectProviderResponse = res
    "GetOpenIdConnectProviderResponse"
    "fixture/GetOpenIdConnectProviderResponse.proto"
    iAM
    (Proxy :: Proxy GetOpenIdConnectProvider)

testDeleteUserPolicyResponse :: DeleteUserPolicyResponse -> TestTree
testDeleteUserPolicyResponse = res
    "DeleteUserPolicyResponse"
    "fixture/DeleteUserPolicyResponse.proto"
    iAM
    (Proxy :: Proxy DeleteUserPolicy)

testCreateRoleResponse :: CreateRoleResponse -> TestTree
testCreateRoleResponse = res
    "CreateRoleResponse"
    "fixture/CreateRoleResponse.proto"
    iAM
    (Proxy :: Proxy CreateRole)

testGetCredentialReportResponse :: GetCredentialReportResponse -> TestTree
testGetCredentialReportResponse = res
    "GetCredentialReportResponse"
    "fixture/GetCredentialReportResponse.proto"
    iAM
    (Proxy :: Proxy GetCredentialReport)

testGetAccountSummaryResponse :: GetAccountSummaryResponse -> TestTree
testGetAccountSummaryResponse = res
    "GetAccountSummaryResponse"
    "fixture/GetAccountSummaryResponse.proto"
    iAM
    (Proxy :: Proxy GetAccountSummary)

testListGroupPoliciesResponse :: ListGroupPoliciesResponse -> TestTree
testListGroupPoliciesResponse = res
    "ListGroupPoliciesResponse"
    "fixture/ListGroupPoliciesResponse.proto"
    iAM
    (Proxy :: Proxy ListGroupPolicies)

testDeletePolicyVersionResponse :: DeletePolicyVersionResponse -> TestTree
testDeletePolicyVersionResponse = res
    "DeletePolicyVersionResponse"
    "fixture/DeletePolicyVersionResponse.proto"
    iAM
    (Proxy :: Proxy DeletePolicyVersion)

testDeleteInstanceProfileResponse :: DeleteInstanceProfileResponse -> TestTree
testDeleteInstanceProfileResponse = res
    "DeleteInstanceProfileResponse"
    "fixture/DeleteInstanceProfileResponse.proto"
    iAM
    (Proxy :: Proxy DeleteInstanceProfile)

testDetachRolePolicyResponse :: DetachRolePolicyResponse -> TestTree
testDetachRolePolicyResponse = res
    "DetachRolePolicyResponse"
    "fixture/DetachRolePolicyResponse.proto"
    iAM
    (Proxy :: Proxy DetachRolePolicy)

testRemoveRoleFromInstanceProfileResponse :: RemoveRoleFromInstanceProfileResponse -> TestTree
testRemoveRoleFromInstanceProfileResponse = res
    "RemoveRoleFromInstanceProfileResponse"
    "fixture/RemoveRoleFromInstanceProfileResponse.proto"
    iAM
    (Proxy :: Proxy RemoveRoleFromInstanceProfile)

testCreatePolicyVersionResponse :: CreatePolicyVersionResponse -> TestTree
testCreatePolicyVersionResponse = res
    "CreatePolicyVersionResponse"
    "fixture/CreatePolicyVersionResponse.proto"
    iAM
    (Proxy :: Proxy CreatePolicyVersion)

testCreateInstanceProfileResponse :: CreateInstanceProfileResponse -> TestTree
testCreateInstanceProfileResponse = res
    "CreateInstanceProfileResponse"
    "fixture/CreateInstanceProfileResponse.proto"
    iAM
    (Proxy :: Proxy CreateInstanceProfile)

testCreateSAMLProviderResponse :: CreateSAMLProviderResponse -> TestTree
testCreateSAMLProviderResponse = res
    "CreateSAMLProviderResponse"
    "fixture/CreateSAMLProviderResponse.proto"
    iAM
    (Proxy :: Proxy CreateSAMLProvider)

testGetAccountAuthorizationDetailsResponse :: GetAccountAuthorizationDetailsResponse -> TestTree
testGetAccountAuthorizationDetailsResponse = res
    "GetAccountAuthorizationDetailsResponse"
    "fixture/GetAccountAuthorizationDetailsResponse.proto"
    iAM
    (Proxy :: Proxy GetAccountAuthorizationDetails)

testDeleteAccountAliasResponse :: DeleteAccountAliasResponse -> TestTree
testDeleteAccountAliasResponse = res
    "DeleteAccountAliasResponse"
    "fixture/DeleteAccountAliasResponse.proto"
    iAM
    (Proxy :: Proxy DeleteAccountAlias)

testDetachUserPolicyResponse :: DetachUserPolicyResponse -> TestTree
testDetachUserPolicyResponse = res
    "DetachUserPolicyResponse"
    "fixture/DetachUserPolicyResponse.proto"
    iAM
    (Proxy :: Proxy DetachUserPolicy)

testRemoveUserFromGroupResponse :: RemoveUserFromGroupResponse -> TestTree
testRemoveUserFromGroupResponse = res
    "RemoveUserFromGroupResponse"
    "fixture/RemoveUserFromGroupResponse.proto"
    iAM
    (Proxy :: Proxy RemoveUserFromGroup)

testDeleteGroupPolicyResponse :: DeleteGroupPolicyResponse -> TestTree
testDeleteGroupPolicyResponse = res
    "DeleteGroupPolicyResponse"
    "fixture/DeleteGroupPolicyResponse.proto"
    iAM
    (Proxy :: Proxy DeleteGroupPolicy)

testPutGroupPolicyResponse :: PutGroupPolicyResponse -> TestTree
testPutGroupPolicyResponse = res
    "PutGroupPolicyResponse"
    "fixture/PutGroupPolicyResponse.proto"
    iAM
    (Proxy :: Proxy PutGroupPolicy)

testGetLoginProfileResponse :: GetLoginProfileResponse -> TestTree
testGetLoginProfileResponse = res
    "GetLoginProfileResponse"
    "fixture/GetLoginProfileResponse.proto"
    iAM
    (Proxy :: Proxy GetLoginProfile)

testGetGroupPolicyResponse :: GetGroupPolicyResponse -> TestTree
testGetGroupPolicyResponse = res
    "GetGroupPolicyResponse"
    "fixture/GetGroupPolicyResponse.proto"
    iAM
    (Proxy :: Proxy GetGroupPolicy)

testChangePasswordResponse :: ChangePasswordResponse -> TestTree
testChangePasswordResponse = res
    "ChangePasswordResponse"
    "fixture/ChangePasswordResponse.proto"
    iAM
    (Proxy :: Proxy ChangePassword)

testListServerCertificatesResponse :: ListServerCertificatesResponse -> TestTree
testListServerCertificatesResponse = res
    "ListServerCertificatesResponse"
    "fixture/ListServerCertificatesResponse.proto"
    iAM
    (Proxy :: Proxy ListServerCertificates)

testDeletePolicyResponse :: DeletePolicyResponse -> TestTree
testDeletePolicyResponse = res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    iAM
    (Proxy :: Proxy DeletePolicy)

testUpdateAssumeRolePolicyResponse :: UpdateAssumeRolePolicyResponse -> TestTree
testUpdateAssumeRolePolicyResponse = res
    "UpdateAssumeRolePolicyResponse"
    "fixture/UpdateAssumeRolePolicyResponse.proto"
    iAM
    (Proxy :: Proxy UpdateAssumeRolePolicy)

testGetInstanceProfileResponse :: GetInstanceProfileResponse -> TestTree
testGetInstanceProfileResponse = res
    "GetInstanceProfileResponse"
    "fixture/GetInstanceProfileResponse.proto"
    iAM
    (Proxy :: Proxy GetInstanceProfile)

testCreateLoginProfileResponse :: CreateLoginProfileResponse -> TestTree
testCreateLoginProfileResponse = res
    "CreateLoginProfileResponse"
    "fixture/CreateLoginProfileResponse.proto"
    iAM
    (Proxy :: Proxy CreateLoginProfile)

testGetSAMLProviderResponse :: GetSAMLProviderResponse -> TestTree
testGetSAMLProviderResponse = res
    "GetSAMLProviderResponse"
    "fixture/GetSAMLProviderResponse.proto"
    iAM
    (Proxy :: Proxy GetSAMLProvider)

testAddRoleToInstanceProfileResponse :: AddRoleToInstanceProfileResponse -> TestTree
testAddRoleToInstanceProfileResponse = res
    "AddRoleToInstanceProfileResponse"
    "fixture/AddRoleToInstanceProfileResponse.proto"
    iAM
    (Proxy :: Proxy AddRoleToInstanceProfile)

testListGroupsForUserResponse :: ListGroupsForUserResponse -> TestTree
testListGroupsForUserResponse = res
    "ListGroupsForUserResponse"
    "fixture/ListGroupsForUserResponse.proto"
    iAM
    (Proxy :: Proxy ListGroupsForUser)

testListEntitiesForPolicyResponse :: ListEntitiesForPolicyResponse -> TestTree
testListEntitiesForPolicyResponse = res
    "ListEntitiesForPolicyResponse"
    "fixture/ListEntitiesForPolicyResponse.proto"
    iAM
    (Proxy :: Proxy ListEntitiesForPolicy)

testAddUserToGroupResponse :: AddUserToGroupResponse -> TestTree
testAddUserToGroupResponse = res
    "AddUserToGroupResponse"
    "fixture/AddUserToGroupResponse.proto"
    iAM
    (Proxy :: Proxy AddUserToGroup)

testSimulatePrincipalPolicyResponse :: SimulatePolicyResponse -> TestTree
testSimulatePrincipalPolicyResponse = res
    "SimulatePrincipalPolicyResponse"
    "fixture/SimulatePrincipalPolicyResponse.proto"
    iAM
    (Proxy :: Proxy SimulatePrincipalPolicy)

testGetPolicyVersionResponse :: GetPolicyVersionResponse -> TestTree
testGetPolicyVersionResponse = res
    "GetPolicyVersionResponse"
    "fixture/GetPolicyVersionResponse.proto"
    iAM
    (Proxy :: Proxy GetPolicyVersion)

testDeleteOpenIdConnectProviderResponse :: DeleteOpenIdConnectProviderResponse -> TestTree
testDeleteOpenIdConnectProviderResponse = res
    "DeleteOpenIdConnectProviderResponse"
    "fixture/DeleteOpenIdConnectProviderResponse.proto"
    iAM
    (Proxy :: Proxy DeleteOpenIdConnectProvider)

testGetUserResponse :: GetUserResponse -> TestTree
testGetUserResponse = res
    "GetUserResponse"
    "fixture/GetUserResponse.proto"
    iAM
    (Proxy :: Proxy GetUser)

testListSigningCertificatesResponse :: ListSigningCertificatesResponse -> TestTree
testListSigningCertificatesResponse = res
    "ListSigningCertificatesResponse"
    "fixture/ListSigningCertificatesResponse.proto"
    iAM
    (Proxy :: Proxy ListSigningCertificates)

testDeleteSigningCertificateResponse :: DeleteSigningCertificateResponse -> TestTree
testDeleteSigningCertificateResponse = res
    "DeleteSigningCertificateResponse"
    "fixture/DeleteSigningCertificateResponse.proto"
    iAM
    (Proxy :: Proxy DeleteSigningCertificate)

testUpdateSigningCertificateResponse :: UpdateSigningCertificateResponse -> TestTree
testUpdateSigningCertificateResponse = res
    "UpdateSigningCertificateResponse"
    "fixture/UpdateSigningCertificateResponse.proto"
    iAM
    (Proxy :: Proxy UpdateSigningCertificate)

testListAttachedUserPoliciesResponse :: ListAttachedUserPoliciesResponse -> TestTree
testListAttachedUserPoliciesResponse = res
    "ListAttachedUserPoliciesResponse"
    "fixture/ListAttachedUserPoliciesResponse.proto"
    iAM
    (Proxy :: Proxy ListAttachedUserPolicies)

testRemoveClientIdFromOpenIdConnectProviderResponse :: RemoveClientIdFromOpenIdConnectProviderResponse -> TestTree
testRemoveClientIdFromOpenIdConnectProviderResponse = res
    "RemoveClientIdFromOpenIdConnectProviderResponse"
    "fixture/RemoveClientIdFromOpenIdConnectProviderResponse.proto"
    iAM
    (Proxy :: Proxy RemoveClientIdFromOpenIdConnectProvider)

testAttachUserPolicyResponse :: AttachUserPolicyResponse -> TestTree
testAttachUserPolicyResponse = res
    "AttachUserPolicyResponse"
    "fixture/AttachUserPolicyResponse.proto"
    iAM
    (Proxy :: Proxy AttachUserPolicy)

testListVirtualMFADevicesResponse :: ListVirtualMFADevicesResponse -> TestTree
testListVirtualMFADevicesResponse = res
    "ListVirtualMFADevicesResponse"
    "fixture/ListVirtualMFADevicesResponse.proto"
    iAM
    (Proxy :: Proxy ListVirtualMFADevices)

testResyncMFADeviceResponse :: ResyncMFADeviceResponse -> TestTree
testResyncMFADeviceResponse = res
    "ResyncMFADeviceResponse"
    "fixture/ResyncMFADeviceResponse.proto"
    iAM
    (Proxy :: Proxy ResyncMFADevice)

testDeleteAccessKeyResponse :: DeleteAccessKeyResponse -> TestTree
testDeleteAccessKeyResponse = res
    "DeleteAccessKeyResponse"
    "fixture/DeleteAccessKeyResponse.proto"
    iAM
    (Proxy :: Proxy DeleteAccessKey)

testUpdateAccessKeyResponse :: UpdateAccessKeyResponse -> TestTree
testUpdateAccessKeyResponse = res
    "UpdateAccessKeyResponse"
    "fixture/UpdateAccessKeyResponse.proto"
    iAM
    (Proxy :: Proxy UpdateAccessKey)

testListAccessKeysResponse :: ListAccessKeysResponse -> TestTree
testListAccessKeysResponse = res
    "ListAccessKeysResponse"
    "fixture/ListAccessKeysResponse.proto"
    iAM
    (Proxy :: Proxy ListAccessKeys)

testGetRolePolicyResponse :: GetRolePolicyResponse -> TestTree
testGetRolePolicyResponse = res
    "GetRolePolicyResponse"
    "fixture/GetRolePolicyResponse.proto"
    iAM
    (Proxy :: Proxy GetRolePolicy)

testCreateUserResponse :: CreateUserResponse -> TestTree
testCreateUserResponse = res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    iAM
    (Proxy :: Proxy CreateUser)

testPutRolePolicyResponse :: PutRolePolicyResponse -> TestTree
testPutRolePolicyResponse = res
    "PutRolePolicyResponse"
    "fixture/PutRolePolicyResponse.proto"
    iAM
    (Proxy :: Proxy PutRolePolicy)

testGetContextKeysForCustomPolicyResponse :: GetContextKeysForPolicyResponse -> TestTree
testGetContextKeysForCustomPolicyResponse = res
    "GetContextKeysForCustomPolicyResponse"
    "fixture/GetContextKeysForCustomPolicyResponse.proto"
    iAM
    (Proxy :: Proxy GetContextKeysForCustomPolicy)

testUploadSigningCertificateResponse :: UploadSigningCertificateResponse -> TestTree
testUploadSigningCertificateResponse = res
    "UploadSigningCertificateResponse"
    "fixture/UploadSigningCertificateResponse.proto"
    iAM
    (Proxy :: Proxy UploadSigningCertificate)

testDeleteRolePolicyResponse :: DeleteRolePolicyResponse -> TestTree
testDeleteRolePolicyResponse = res
    "DeleteRolePolicyResponse"
    "fixture/DeleteRolePolicyResponse.proto"
    iAM
    (Proxy :: Proxy DeleteRolePolicy)

testGetAccountPasswordPolicyResponse :: GetAccountPasswordPolicyResponse -> TestTree
testGetAccountPasswordPolicyResponse = res
    "GetAccountPasswordPolicyResponse"
    "fixture/GetAccountPasswordPolicyResponse.proto"
    iAM
    (Proxy :: Proxy GetAccountPasswordPolicy)

testGetAccessKeyLastUsedResponse :: GetAccessKeyLastUsedResponse -> TestTree
testGetAccessKeyLastUsedResponse = res
    "GetAccessKeyLastUsedResponse"
    "fixture/GetAccessKeyLastUsedResponse.proto"
    iAM
    (Proxy :: Proxy GetAccessKeyLastUsed)

testUpdateUserResponse :: UpdateUserResponse -> TestTree
testUpdateUserResponse = res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    iAM
    (Proxy :: Proxy UpdateUser)

testDeleteUserResponse :: DeleteUserResponse -> TestTree
testDeleteUserResponse = res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    iAM
    (Proxy :: Proxy DeleteUser)

testAddClientIdToOpenIdConnectProviderResponse :: AddClientIdToOpenIdConnectProviderResponse -> TestTree
testAddClientIdToOpenIdConnectProviderResponse = res
    "AddClientIdToOpenIdConnectProviderResponse"
    "fixture/AddClientIdToOpenIdConnectProviderResponse.proto"
    iAM
    (Proxy :: Proxy AddClientIdToOpenIdConnectProvider)

testListRolePoliciesResponse :: ListRolePoliciesResponse -> TestTree
testListRolePoliciesResponse = res
    "ListRolePoliciesResponse"
    "fixture/ListRolePoliciesResponse.proto"
    iAM
    (Proxy :: Proxy ListRolePolicies)

testCreateAccountAliasResponse :: CreateAccountAliasResponse -> TestTree
testCreateAccountAliasResponse = res
    "CreateAccountAliasResponse"
    "fixture/CreateAccountAliasResponse.proto"
    iAM
    (Proxy :: Proxy CreateAccountAlias)

testListInstanceProfilesResponse :: ListInstanceProfilesResponse -> TestTree
testListInstanceProfilesResponse = res
    "ListInstanceProfilesResponse"
    "fixture/ListInstanceProfilesResponse.proto"
    iAM
    (Proxy :: Proxy ListInstanceProfiles)

testEnableMFADeviceResponse :: EnableMFADeviceResponse -> TestTree
testEnableMFADeviceResponse = res
    "EnableMFADeviceResponse"
    "fixture/EnableMFADeviceResponse.proto"
    iAM
    (Proxy :: Proxy EnableMFADevice)

testListAccountAliasesResponse :: ListAccountAliasesResponse -> TestTree
testListAccountAliasesResponse = res
    "ListAccountAliasesResponse"
    "fixture/ListAccountAliasesResponse.proto"
    iAM
    (Proxy :: Proxy ListAccountAliases)

testDeleteSAMLProviderResponse :: DeleteSAMLProviderResponse -> TestTree
testDeleteSAMLProviderResponse = res
    "DeleteSAMLProviderResponse"
    "fixture/DeleteSAMLProviderResponse.proto"
    iAM
    (Proxy :: Proxy DeleteSAMLProvider)

testUpdateSAMLProviderResponse :: UpdateSAMLProviderResponse -> TestTree
testUpdateSAMLProviderResponse = res
    "UpdateSAMLProviderResponse"
    "fixture/UpdateSAMLProviderResponse.proto"
    iAM
    (Proxy :: Proxy UpdateSAMLProvider)

testCreateGroupResponse :: CreateGroupResponse -> TestTree
testCreateGroupResponse = res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    iAM
    (Proxy :: Proxy CreateGroup)

testListMFADevicesResponse :: ListMFADevicesResponse -> TestTree
testListMFADevicesResponse = res
    "ListMFADevicesResponse"
    "fixture/ListMFADevicesResponse.proto"
    iAM
    (Proxy :: Proxy ListMFADevices)

testUploadServerCertificateResponse :: UploadServerCertificateResponse -> TestTree
testUploadServerCertificateResponse = res
    "UploadServerCertificateResponse"
    "fixture/UploadServerCertificateResponse.proto"
    iAM
    (Proxy :: Proxy UploadServerCertificate)

testSetDefaultPolicyVersionResponse :: SetDefaultPolicyVersionResponse -> TestTree
testSetDefaultPolicyVersionResponse = res
    "SetDefaultPolicyVersionResponse"
    "fixture/SetDefaultPolicyVersionResponse.proto"
    iAM
    (Proxy :: Proxy SetDefaultPolicyVersion)

testListPolicyVersionsResponse :: ListPolicyVersionsResponse -> TestTree
testListPolicyVersionsResponse = res
    "ListPolicyVersionsResponse"
    "fixture/ListPolicyVersionsResponse.proto"
    iAM
    (Proxy :: Proxy ListPolicyVersions)

testListSAMLProvidersResponse :: ListSAMLProvidersResponse -> TestTree
testListSAMLProvidersResponse = res
    "ListSAMLProvidersResponse"
    "fixture/ListSAMLProvidersResponse.proto"
    iAM
    (Proxy :: Proxy ListSAMLProviders)

testGetServerCertificateResponse :: GetServerCertificateResponse -> TestTree
testGetServerCertificateResponse = res
    "GetServerCertificateResponse"
    "fixture/GetServerCertificateResponse.proto"
    iAM
    (Proxy :: Proxy GetServerCertificate)

testDeleteGroupResponse :: DeleteGroupResponse -> TestTree
testDeleteGroupResponse = res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    iAM
    (Proxy :: Proxy DeleteGroup)

testUpdateGroupResponse :: UpdateGroupResponse -> TestTree
testUpdateGroupResponse = res
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse.proto"
    iAM
    (Proxy :: Proxy UpdateGroup)

testListGroupsResponse :: ListGroupsResponse -> TestTree
testListGroupsResponse = res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    iAM
    (Proxy :: Proxy ListGroups)

testGenerateCredentialReportResponse :: GenerateCredentialReportResponse -> TestTree
testGenerateCredentialReportResponse = res
    "GenerateCredentialReportResponse"
    "fixture/GenerateCredentialReportResponse.proto"
    iAM
    (Proxy :: Proxy GenerateCredentialReport)

testGetPolicyResponse :: GetPolicyResponse -> TestTree
testGetPolicyResponse = res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    iAM
    (Proxy :: Proxy GetPolicy)

testUpdateLoginProfileResponse :: UpdateLoginProfileResponse -> TestTree
testUpdateLoginProfileResponse = res
    "UpdateLoginProfileResponse"
    "fixture/UpdateLoginProfileResponse.proto"
    iAM
    (Proxy :: Proxy UpdateLoginProfile)

testDeleteLoginProfileResponse :: DeleteLoginProfileResponse -> TestTree
testDeleteLoginProfileResponse = res
    "DeleteLoginProfileResponse"
    "fixture/DeleteLoginProfileResponse.proto"
    iAM
    (Proxy :: Proxy DeleteLoginProfile)

testGetGroupResponse :: GetGroupResponse -> TestTree
testGetGroupResponse = res
    "GetGroupResponse"
    "fixture/GetGroupResponse.proto"
    iAM
    (Proxy :: Proxy GetGroup)

testDeleteServerCertificateResponse :: DeleteServerCertificateResponse -> TestTree
testDeleteServerCertificateResponse = res
    "DeleteServerCertificateResponse"
    "fixture/DeleteServerCertificateResponse.proto"
    iAM
    (Proxy :: Proxy DeleteServerCertificate)

testUpdateServerCertificateResponse :: UpdateServerCertificateResponse -> TestTree
testUpdateServerCertificateResponse = res
    "UpdateServerCertificateResponse"
    "fixture/UpdateServerCertificateResponse.proto"
    iAM
    (Proxy :: Proxy UpdateServerCertificate)

testListAttachedGroupPoliciesResponse :: ListAttachedGroupPoliciesResponse -> TestTree
testListAttachedGroupPoliciesResponse = res
    "ListAttachedGroupPoliciesResponse"
    "fixture/ListAttachedGroupPoliciesResponse.proto"
    iAM
    (Proxy :: Proxy ListAttachedGroupPolicies)
