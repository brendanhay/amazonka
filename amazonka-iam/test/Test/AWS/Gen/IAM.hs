{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.IAM
-- Copyright   : (c) 2013-2015 Brendan Hay
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
--         [ testAttachGroupPolicy $
--             attachGroupPolicy
--
--         , testListInstanceProfilesForRole $
--             listInstanceProfilesForRole
--
--         , testCreatePolicy $
--             createPolicy
--
--         , testListPolicies $
--             listPolicies
--
--         , testAttachRolePolicy $
--             attachRolePolicy
--
--         , testListSSHPublicKeys $
--             listSSHPublicKeys
--
--         , testDeleteSSHPublicKey $
--             deleteSSHPublicKey
--
--         , testUpdateSSHPublicKey $
--             updateSSHPublicKey
--
--         , testListOpenIdConnectProviders $
--             listOpenIdConnectProviders
--
--         , testDeleteAccountPasswordPolicy $
--             deleteAccountPasswordPolicy
--
--         , testUpdateAccountPasswordPolicy $
--             updateAccountPasswordPolicy
--
--         , testCreateAccessKey $
--             createAccessKey
--
--         , testGetUserPolicy $
--             getUserPolicy
--
--         , testCreateVirtualMFADevice $
--             createVirtualMFADevice
--
--         , testCreateOpenIdConnectProvider $
--             createOpenIdConnectProvider
--
--         , testListAttachedRolePolicies $
--             listAttachedRolePolicies
--
--         , testDeleteVirtualMFADevice $
--             deleteVirtualMFADevice
--
--         , testGetRole $
--             getRole
--
--         , testDeactivateMFADevice $
--             deactivateMFADevice
--
--         , testListRoles $
--             listRoles
--
--         , testDeleteRole $
--             deleteRole
--
--         , testListUserPolicies $
--             listUserPolicies
--
--         , testUploadSSHPublicKey $
--             uploadSSHPublicKey
--
--         , testListUsers $
--             listUsers
--
--         , testUpdateOpenIdConnectProviderThumbprint $
--             updateOpenIdConnectProviderThumbprint
--
--         , testGetSSHPublicKey $
--             getSSHPublicKey
--
--         , testPutUserPolicy $
--             putUserPolicy
--
--         , testCreateRole $
--             createRole
--
--         , testDeleteUserPolicy $
--             deleteUserPolicy
--
--         , testGetOpenIdConnectProvider $
--             getOpenIdConnectProvider
--
--         , testDetachGroupPolicy $
--             detachGroupPolicy
--
--         , testGetCredentialReport $
--             getCredentialReport
--
--         , testDeletePolicyVersion $
--             deletePolicyVersion
--
--         , testDetachRolePolicy $
--             detachRolePolicy
--
--         , testDeleteInstanceProfile $
--             deleteInstanceProfile
--
--         , testListGroupPolicies $
--             listGroupPolicies
--
--         , testGetAccountSummary $
--             getAccountSummary
--
--         , testCreateInstanceProfile $
--             createInstanceProfile
--
--         , testPutGroupPolicy $
--             putGroupPolicy
--
--         , testDeleteGroupPolicy $
--             deleteGroupPolicy
--
--         , testGetAccountAuthorizationDetails $
--             getAccountAuthorizationDetails
--
--         , testDeleteAccountAlias $
--             deleteAccountAlias
--
--         , testRemoveRoleFromInstanceProfile $
--             removeRoleFromInstanceProfile
--
--         , testGetLoginProfile $
--             getLoginProfile
--
--         , testRemoveUserFromGroup $
--             removeUserFromGroup
--
--         , testDetachUserPolicy $
--             detachUserPolicy
--
--         , testCreateSAMLProvider $
--             createSAMLProvider
--
--         , testCreatePolicyVersion $
--             createPolicyVersion
--
--         , testGetGroupPolicy $
--             getGroupPolicy
--
--         , testDeletePolicy $
--             deletePolicy
--
--         , testListServerCertificates $
--             listServerCertificates
--
--         , testUpdateAssumeRolePolicy $
--             updateAssumeRolePolicy
--
--         , testChangePassword $
--             changePassword
--
--         , testListGroupsForUser $
--             listGroupsForUser
--
--         , testGetPolicyVersion $
--             getPolicyVersion
--
--         , testCreateLoginProfile $
--             createLoginProfile
--
--         , testGetInstanceProfile $
--             getInstanceProfile
--
--         , testListEntitiesForPolicy $
--             listEntitiesForPolicy
--
--         , testGetSAMLProvider $
--             getSAMLProvider
--
--         , testAddRoleToInstanceProfile $
--             addRoleToInstanceProfile
--
--         , testAddUserToGroup $
--             addUserToGroup
--
--         , testDeleteOpenIdConnectProvider $
--             deleteOpenIdConnectProvider
--
--         , testGetUser $
--             getUser
--
--         , testListAttachedUserPolicies $
--             listAttachedUserPolicies
--
--         , testDeleteSigningCertificate $
--             deleteSigningCertificate
--
--         , testUpdateSigningCertificate $
--             updateSigningCertificate
--
--         , testListSigningCertificates $
--             listSigningCertificates
--
--         , testRemoveClientIdFromOpenIdConnectProvider $
--             removeClientIdFromOpenIdConnectProvider
--
--         , testListAccessKeys $
--             listAccessKeys
--
--         , testListVirtualMFADevices $
--             listVirtualMFADevices
--
--         , testDeleteAccessKey $
--             deleteAccessKey
--
--         , testUpdateAccessKey $
--             updateAccessKey
--
--         , testGetRolePolicy $
--             getRolePolicy
--
--         , testAttachUserPolicy $
--             attachUserPolicy
--
--         , testResyncMFADevice $
--             resyncMFADevice
--
--         , testCreateUser $
--             createUser
--
--         , testUploadSigningCertificate $
--             uploadSigningCertificate
--
--         , testPutRolePolicy $
--             putRolePolicy
--
--         , testDeleteRolePolicy $
--             deleteRolePolicy
--
--         , testUpdateUser $
--             updateUser
--
--         , testDeleteUser $
--             deleteUser
--
--         , testListRolePolicies $
--             listRolePolicies
--
--         , testAddClientIdToOpenIdConnectProvider $
--             addClientIdToOpenIdConnectProvider
--
--         , testGetAccessKeyLastUsed $
--             getAccessKeyLastUsed
--
--         , testGetAccountPasswordPolicy $
--             getAccountPasswordPolicy
--
--         , testListAccountAliases $
--             listAccountAliases
--
--         , testCreateAccountAlias $
--             createAccountAlias
--
--         , testUploadServerCertificate $
--             uploadServerCertificate
--
--         , testListMFADevices $
--             listMFADevices
--
--         , testEnableMFADevice $
--             enableMFADevice
--
--         , testListPolicyVersions $
--             listPolicyVersions
--
--         , testListSAMLProviders $
--             listSAMLProviders
--
--         , testUpdateSAMLProvider $
--             updateSAMLProvider
--
--         , testDeleteSAMLProvider $
--             deleteSAMLProvider
--
--         , testCreateGroup $
--             createGroup
--
--         , testSetDefaultPolicyVersion $
--             setDefaultPolicyVersion
--
--         , testListInstanceProfiles $
--             listInstanceProfiles
--
--         , testListGroups $
--             listGroups
--
--         , testDeleteGroup $
--             deleteGroup
--
--         , testUpdateGroup $
--             updateGroup
--
--         , testGetServerCertificate $
--             getServerCertificate
--
--         , testGetPolicy $
--             getPolicy
--
--         , testGenerateCredentialReport $
--             generateCredentialReport
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
--         , testDeleteLoginProfile $
--             deleteLoginProfile
--
--         , testUpdateLoginProfile $
--             updateLoginProfile
--
--         , testListAttachedGroupPolicies $
--             listAttachedGroupPolicies
--
--           ]

--     , testGroup "response"
--         [ testAttachGroupPolicyResponse $
--             attachGroupPolicyResponse
--
--         , testListInstanceProfilesForRoleResponse $
--             listInstanceProfilesForRoleResponse
--
--         , testCreatePolicyResponse $
--             createPolicyResponse
--
--         , testListPoliciesResponse $
--             listPoliciesResponse
--
--         , testAttachRolePolicyResponse $
--             attachRolePolicyResponse
--
--         , testListSSHPublicKeysResponse $
--             listSSHPublicKeysResponse
--
--         , testDeleteSSHPublicKeyResponse $
--             deleteSSHPublicKeyResponse
--
--         , testUpdateSSHPublicKeyResponse $
--             updateSSHPublicKeyResponse
--
--         , testListOpenIdConnectProvidersResponse $
--             listOpenIdConnectProvidersResponse
--
--         , testDeleteAccountPasswordPolicyResponse $
--             deleteAccountPasswordPolicyResponse
--
--         , testUpdateAccountPasswordPolicyResponse $
--             updateAccountPasswordPolicyResponse
--
--         , testCreateAccessKeyResponse $
--             createAccessKeyResponse
--
--         , testGetUserPolicyResponse $
--             getUserPolicyResponse
--
--         , testCreateVirtualMFADeviceResponse $
--             createVirtualMFADeviceResponse
--
--         , testCreateOpenIdConnectProviderResponse $
--             createOpenIdConnectProviderResponse
--
--         , testListAttachedRolePoliciesResponse $
--             listAttachedRolePoliciesResponse
--
--         , testDeleteVirtualMFADeviceResponse $
--             deleteVirtualMFADeviceResponse
--
--         , testGetRoleResponse $
--             getRoleResponse
--
--         , testDeactivateMFADeviceResponse $
--             deactivateMFADeviceResponse
--
--         , testListRolesResponse $
--             listRolesResponse
--
--         , testDeleteRoleResponse $
--             deleteRoleResponse
--
--         , testListUserPoliciesResponse $
--             listUserPoliciesResponse
--
--         , testUploadSSHPublicKeyResponse $
--             uploadSSHPublicKeyResponse
--
--         , testListUsersResponse $
--             listUsersResponse
--
--         , testUpdateOpenIdConnectProviderThumbprintResponse $
--             updateOpenIdConnectProviderThumbprintResponse
--
--         , testGetSSHPublicKeyResponse $
--             getSSHPublicKeyResponse
--
--         , testPutUserPolicyResponse $
--             putUserPolicyResponse
--
--         , testCreateRoleResponse $
--             createRoleResponse
--
--         , testDeleteUserPolicyResponse $
--             deleteUserPolicyResponse
--
--         , testGetOpenIdConnectProviderResponse $
--             getOpenIdConnectProviderResponse
--
--         , testDetachGroupPolicyResponse $
--             detachGroupPolicyResponse
--
--         , testGetCredentialReportResponse $
--             getCredentialReportResponse
--
--         , testDeletePolicyVersionResponse $
--             deletePolicyVersionResponse
--
--         , testDetachRolePolicyResponse $
--             detachRolePolicyResponse
--
--         , testDeleteInstanceProfileResponse $
--             deleteInstanceProfileResponse
--
--         , testListGroupPoliciesResponse $
--             listGroupPoliciesResponse
--
--         , testGetAccountSummaryResponse $
--             getAccountSummaryResponse
--
--         , testCreateInstanceProfileResponse $
--             createInstanceProfileResponse
--
--         , testPutGroupPolicyResponse $
--             putGroupPolicyResponse
--
--         , testDeleteGroupPolicyResponse $
--             deleteGroupPolicyResponse
--
--         , testGetAccountAuthorizationDetailsResponse $
--             getAccountAuthorizationDetailsResponse
--
--         , testDeleteAccountAliasResponse $
--             deleteAccountAliasResponse
--
--         , testRemoveRoleFromInstanceProfileResponse $
--             removeRoleFromInstanceProfileResponse
--
--         , testGetLoginProfileResponse $
--             getLoginProfileResponse
--
--         , testRemoveUserFromGroupResponse $
--             removeUserFromGroupResponse
--
--         , testDetachUserPolicyResponse $
--             detachUserPolicyResponse
--
--         , testCreateSAMLProviderResponse $
--             createSAMLProviderResponse
--
--         , testCreatePolicyVersionResponse $
--             createPolicyVersionResponse
--
--         , testGetGroupPolicyResponse $
--             getGroupPolicyResponse
--
--         , testDeletePolicyResponse $
--             deletePolicyResponse
--
--         , testListServerCertificatesResponse $
--             listServerCertificatesResponse
--
--         , testUpdateAssumeRolePolicyResponse $
--             updateAssumeRolePolicyResponse
--
--         , testChangePasswordResponse $
--             changePasswordResponse
--
--         , testListGroupsForUserResponse $
--             listGroupsForUserResponse
--
--         , testGetPolicyVersionResponse $
--             getPolicyVersionResponse
--
--         , testCreateLoginProfileResponse $
--             createLoginProfileResponse
--
--         , testGetInstanceProfileResponse $
--             getInstanceProfileResponse
--
--         , testListEntitiesForPolicyResponse $
--             listEntitiesForPolicyResponse
--
--         , testGetSAMLProviderResponse $
--             getSAMLProviderResponse
--
--         , testAddRoleToInstanceProfileResponse $
--             addRoleToInstanceProfileResponse
--
--         , testAddUserToGroupResponse $
--             addUserToGroupResponse
--
--         , testDeleteOpenIdConnectProviderResponse $
--             deleteOpenIdConnectProviderResponse
--
--         , testGetUserResponse $
--             getUserResponse
--
--         , testListAttachedUserPoliciesResponse $
--             listAttachedUserPoliciesResponse
--
--         , testDeleteSigningCertificateResponse $
--             deleteSigningCertificateResponse
--
--         , testUpdateSigningCertificateResponse $
--             updateSigningCertificateResponse
--
--         , testListSigningCertificatesResponse $
--             listSigningCertificatesResponse
--
--         , testRemoveClientIdFromOpenIdConnectProviderResponse $
--             removeClientIdFromOpenIdConnectProviderResponse
--
--         , testListAccessKeysResponse $
--             listAccessKeysResponse
--
--         , testListVirtualMFADevicesResponse $
--             listVirtualMFADevicesResponse
--
--         , testDeleteAccessKeyResponse $
--             deleteAccessKeyResponse
--
--         , testUpdateAccessKeyResponse $
--             updateAccessKeyResponse
--
--         , testGetRolePolicyResponse $
--             getRolePolicyResponse
--
--         , testAttachUserPolicyResponse $
--             attachUserPolicyResponse
--
--         , testResyncMFADeviceResponse $
--             resyncMFADeviceResponse
--
--         , testCreateUserResponse $
--             createUserResponse
--
--         , testUploadSigningCertificateResponse $
--             uploadSigningCertificateResponse
--
--         , testPutRolePolicyResponse $
--             putRolePolicyResponse
--
--         , testDeleteRolePolicyResponse $
--             deleteRolePolicyResponse
--
--         , testUpdateUserResponse $
--             updateUserResponse
--
--         , testDeleteUserResponse $
--             deleteUserResponse
--
--         , testListRolePoliciesResponse $
--             listRolePoliciesResponse
--
--         , testAddClientIdToOpenIdConnectProviderResponse $
--             addClientIdToOpenIdConnectProviderResponse
--
--         , testGetAccessKeyLastUsedResponse $
--             getAccessKeyLastUsedResponse
--
--         , testGetAccountPasswordPolicyResponse $
--             getAccountPasswordPolicyResponse
--
--         , testListAccountAliasesResponse $
--             listAccountAliasesResponse
--
--         , testCreateAccountAliasResponse $
--             createAccountAliasResponse
--
--         , testUploadServerCertificateResponse $
--             uploadServerCertificateResponse
--
--         , testListMFADevicesResponse $
--             listMFADevicesResponse
--
--         , testEnableMFADeviceResponse $
--             enableMFADeviceResponse
--
--         , testListPolicyVersionsResponse $
--             listPolicyVersionsResponse
--
--         , testListSAMLProvidersResponse $
--             listSAMLProvidersResponse
--
--         , testUpdateSAMLProviderResponse $
--             updateSAMLProviderResponse
--
--         , testDeleteSAMLProviderResponse $
--             deleteSAMLProviderResponse
--
--         , testCreateGroupResponse $
--             createGroupResponse
--
--         , testSetDefaultPolicyVersionResponse $
--             setDefaultPolicyVersionResponse
--
--         , testListInstanceProfilesResponse $
--             listInstanceProfilesResponse
--
--         , testListGroupsResponse $
--             listGroupsResponse
--
--         , testDeleteGroupResponse $
--             deleteGroupResponse
--
--         , testUpdateGroupResponse $
--             updateGroupResponse
--
--         , testGetServerCertificateResponse $
--             getServerCertificateResponse
--
--         , testGetPolicyResponse $
--             getPolicyResponse
--
--         , testGenerateCredentialReportResponse $
--             generateCredentialReportResponse
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
--         , testDeleteLoginProfileResponse $
--             deleteLoginProfileResponse
--
--         , testUpdateLoginProfileResponse $
--             updateLoginProfileResponse
--
--         , testListAttachedGroupPoliciesResponse $
--             listAttachedGroupPoliciesResponse
--
--           ]
--     ]

-- Requests

testAttachGroupPolicy :: AttachGroupPolicy -> TestTree
testAttachGroupPolicy = req
    "AttachGroupPolicy"
    "fixture/AttachGroupPolicy.yaml"

testListInstanceProfilesForRole :: ListInstanceProfilesForRole -> TestTree
testListInstanceProfilesForRole = req
    "ListInstanceProfilesForRole"
    "fixture/ListInstanceProfilesForRole.yaml"

testCreatePolicy :: CreatePolicy -> TestTree
testCreatePolicy = req
    "CreatePolicy"
    "fixture/CreatePolicy.yaml"

testListPolicies :: ListPolicies -> TestTree
testListPolicies = req
    "ListPolicies"
    "fixture/ListPolicies.yaml"

testAttachRolePolicy :: AttachRolePolicy -> TestTree
testAttachRolePolicy = req
    "AttachRolePolicy"
    "fixture/AttachRolePolicy.yaml"

testListSSHPublicKeys :: ListSSHPublicKeys -> TestTree
testListSSHPublicKeys = req
    "ListSSHPublicKeys"
    "fixture/ListSSHPublicKeys.yaml"

testDeleteSSHPublicKey :: DeleteSSHPublicKey -> TestTree
testDeleteSSHPublicKey = req
    "DeleteSSHPublicKey"
    "fixture/DeleteSSHPublicKey.yaml"

testUpdateSSHPublicKey :: UpdateSSHPublicKey -> TestTree
testUpdateSSHPublicKey = req
    "UpdateSSHPublicKey"
    "fixture/UpdateSSHPublicKey.yaml"

testListOpenIdConnectProviders :: ListOpenIdConnectProviders -> TestTree
testListOpenIdConnectProviders = req
    "ListOpenIdConnectProviders"
    "fixture/ListOpenIdConnectProviders.yaml"

testDeleteAccountPasswordPolicy :: DeleteAccountPasswordPolicy -> TestTree
testDeleteAccountPasswordPolicy = req
    "DeleteAccountPasswordPolicy"
    "fixture/DeleteAccountPasswordPolicy.yaml"

testUpdateAccountPasswordPolicy :: UpdateAccountPasswordPolicy -> TestTree
testUpdateAccountPasswordPolicy = req
    "UpdateAccountPasswordPolicy"
    "fixture/UpdateAccountPasswordPolicy.yaml"

testCreateAccessKey :: CreateAccessKey -> TestTree
testCreateAccessKey = req
    "CreateAccessKey"
    "fixture/CreateAccessKey.yaml"

testGetUserPolicy :: GetUserPolicy -> TestTree
testGetUserPolicy = req
    "GetUserPolicy"
    "fixture/GetUserPolicy.yaml"

testCreateVirtualMFADevice :: CreateVirtualMFADevice -> TestTree
testCreateVirtualMFADevice = req
    "CreateVirtualMFADevice"
    "fixture/CreateVirtualMFADevice.yaml"

testCreateOpenIdConnectProvider :: CreateOpenIdConnectProvider -> TestTree
testCreateOpenIdConnectProvider = req
    "CreateOpenIdConnectProvider"
    "fixture/CreateOpenIdConnectProvider.yaml"

testListAttachedRolePolicies :: ListAttachedRolePolicies -> TestTree
testListAttachedRolePolicies = req
    "ListAttachedRolePolicies"
    "fixture/ListAttachedRolePolicies.yaml"

testDeleteVirtualMFADevice :: DeleteVirtualMFADevice -> TestTree
testDeleteVirtualMFADevice = req
    "DeleteVirtualMFADevice"
    "fixture/DeleteVirtualMFADevice.yaml"

testGetRole :: GetRole -> TestTree
testGetRole = req
    "GetRole"
    "fixture/GetRole.yaml"

testDeactivateMFADevice :: DeactivateMFADevice -> TestTree
testDeactivateMFADevice = req
    "DeactivateMFADevice"
    "fixture/DeactivateMFADevice.yaml"

testListRoles :: ListRoles -> TestTree
testListRoles = req
    "ListRoles"
    "fixture/ListRoles.yaml"

testDeleteRole :: DeleteRole -> TestTree
testDeleteRole = req
    "DeleteRole"
    "fixture/DeleteRole.yaml"

testListUserPolicies :: ListUserPolicies -> TestTree
testListUserPolicies = req
    "ListUserPolicies"
    "fixture/ListUserPolicies.yaml"

testUploadSSHPublicKey :: UploadSSHPublicKey -> TestTree
testUploadSSHPublicKey = req
    "UploadSSHPublicKey"
    "fixture/UploadSSHPublicKey.yaml"

testListUsers :: ListUsers -> TestTree
testListUsers = req
    "ListUsers"
    "fixture/ListUsers.yaml"

testUpdateOpenIdConnectProviderThumbprint :: UpdateOpenIdConnectProviderThumbprint -> TestTree
testUpdateOpenIdConnectProviderThumbprint = req
    "UpdateOpenIdConnectProviderThumbprint"
    "fixture/UpdateOpenIdConnectProviderThumbprint.yaml"

testGetSSHPublicKey :: GetSSHPublicKey -> TestTree
testGetSSHPublicKey = req
    "GetSSHPublicKey"
    "fixture/GetSSHPublicKey.yaml"

testPutUserPolicy :: PutUserPolicy -> TestTree
testPutUserPolicy = req
    "PutUserPolicy"
    "fixture/PutUserPolicy.yaml"

testCreateRole :: CreateRole -> TestTree
testCreateRole = req
    "CreateRole"
    "fixture/CreateRole.yaml"

testDeleteUserPolicy :: DeleteUserPolicy -> TestTree
testDeleteUserPolicy = req
    "DeleteUserPolicy"
    "fixture/DeleteUserPolicy.yaml"

testGetOpenIdConnectProvider :: GetOpenIdConnectProvider -> TestTree
testGetOpenIdConnectProvider = req
    "GetOpenIdConnectProvider"
    "fixture/GetOpenIdConnectProvider.yaml"

testDetachGroupPolicy :: DetachGroupPolicy -> TestTree
testDetachGroupPolicy = req
    "DetachGroupPolicy"
    "fixture/DetachGroupPolicy.yaml"

testGetCredentialReport :: GetCredentialReport -> TestTree
testGetCredentialReport = req
    "GetCredentialReport"
    "fixture/GetCredentialReport.yaml"

testDeletePolicyVersion :: DeletePolicyVersion -> TestTree
testDeletePolicyVersion = req
    "DeletePolicyVersion"
    "fixture/DeletePolicyVersion.yaml"

testDetachRolePolicy :: DetachRolePolicy -> TestTree
testDetachRolePolicy = req
    "DetachRolePolicy"
    "fixture/DetachRolePolicy.yaml"

testDeleteInstanceProfile :: DeleteInstanceProfile -> TestTree
testDeleteInstanceProfile = req
    "DeleteInstanceProfile"
    "fixture/DeleteInstanceProfile.yaml"

testListGroupPolicies :: ListGroupPolicies -> TestTree
testListGroupPolicies = req
    "ListGroupPolicies"
    "fixture/ListGroupPolicies.yaml"

testGetAccountSummary :: GetAccountSummary -> TestTree
testGetAccountSummary = req
    "GetAccountSummary"
    "fixture/GetAccountSummary.yaml"

testCreateInstanceProfile :: CreateInstanceProfile -> TestTree
testCreateInstanceProfile = req
    "CreateInstanceProfile"
    "fixture/CreateInstanceProfile.yaml"

testPutGroupPolicy :: PutGroupPolicy -> TestTree
testPutGroupPolicy = req
    "PutGroupPolicy"
    "fixture/PutGroupPolicy.yaml"

testDeleteGroupPolicy :: DeleteGroupPolicy -> TestTree
testDeleteGroupPolicy = req
    "DeleteGroupPolicy"
    "fixture/DeleteGroupPolicy.yaml"

testGetAccountAuthorizationDetails :: GetAccountAuthorizationDetails -> TestTree
testGetAccountAuthorizationDetails = req
    "GetAccountAuthorizationDetails"
    "fixture/GetAccountAuthorizationDetails.yaml"

testDeleteAccountAlias :: DeleteAccountAlias -> TestTree
testDeleteAccountAlias = req
    "DeleteAccountAlias"
    "fixture/DeleteAccountAlias.yaml"

testRemoveRoleFromInstanceProfile :: RemoveRoleFromInstanceProfile -> TestTree
testRemoveRoleFromInstanceProfile = req
    "RemoveRoleFromInstanceProfile"
    "fixture/RemoveRoleFromInstanceProfile.yaml"

testGetLoginProfile :: GetLoginProfile -> TestTree
testGetLoginProfile = req
    "GetLoginProfile"
    "fixture/GetLoginProfile.yaml"

testRemoveUserFromGroup :: RemoveUserFromGroup -> TestTree
testRemoveUserFromGroup = req
    "RemoveUserFromGroup"
    "fixture/RemoveUserFromGroup.yaml"

testDetachUserPolicy :: DetachUserPolicy -> TestTree
testDetachUserPolicy = req
    "DetachUserPolicy"
    "fixture/DetachUserPolicy.yaml"

testCreateSAMLProvider :: CreateSAMLProvider -> TestTree
testCreateSAMLProvider = req
    "CreateSAMLProvider"
    "fixture/CreateSAMLProvider.yaml"

testCreatePolicyVersion :: CreatePolicyVersion -> TestTree
testCreatePolicyVersion = req
    "CreatePolicyVersion"
    "fixture/CreatePolicyVersion.yaml"

testGetGroupPolicy :: GetGroupPolicy -> TestTree
testGetGroupPolicy = req
    "GetGroupPolicy"
    "fixture/GetGroupPolicy.yaml"

testDeletePolicy :: DeletePolicy -> TestTree
testDeletePolicy = req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

testListServerCertificates :: ListServerCertificates -> TestTree
testListServerCertificates = req
    "ListServerCertificates"
    "fixture/ListServerCertificates.yaml"

testUpdateAssumeRolePolicy :: UpdateAssumeRolePolicy -> TestTree
testUpdateAssumeRolePolicy = req
    "UpdateAssumeRolePolicy"
    "fixture/UpdateAssumeRolePolicy.yaml"

testChangePassword :: ChangePassword -> TestTree
testChangePassword = req
    "ChangePassword"
    "fixture/ChangePassword.yaml"

testListGroupsForUser :: ListGroupsForUser -> TestTree
testListGroupsForUser = req
    "ListGroupsForUser"
    "fixture/ListGroupsForUser.yaml"

testGetPolicyVersion :: GetPolicyVersion -> TestTree
testGetPolicyVersion = req
    "GetPolicyVersion"
    "fixture/GetPolicyVersion.yaml"

testCreateLoginProfile :: CreateLoginProfile -> TestTree
testCreateLoginProfile = req
    "CreateLoginProfile"
    "fixture/CreateLoginProfile.yaml"

testGetInstanceProfile :: GetInstanceProfile -> TestTree
testGetInstanceProfile = req
    "GetInstanceProfile"
    "fixture/GetInstanceProfile.yaml"

testListEntitiesForPolicy :: ListEntitiesForPolicy -> TestTree
testListEntitiesForPolicy = req
    "ListEntitiesForPolicy"
    "fixture/ListEntitiesForPolicy.yaml"

testGetSAMLProvider :: GetSAMLProvider -> TestTree
testGetSAMLProvider = req
    "GetSAMLProvider"
    "fixture/GetSAMLProvider.yaml"

testAddRoleToInstanceProfile :: AddRoleToInstanceProfile -> TestTree
testAddRoleToInstanceProfile = req
    "AddRoleToInstanceProfile"
    "fixture/AddRoleToInstanceProfile.yaml"

testAddUserToGroup :: AddUserToGroup -> TestTree
testAddUserToGroup = req
    "AddUserToGroup"
    "fixture/AddUserToGroup.yaml"

testDeleteOpenIdConnectProvider :: DeleteOpenIdConnectProvider -> TestTree
testDeleteOpenIdConnectProvider = req
    "DeleteOpenIdConnectProvider"
    "fixture/DeleteOpenIdConnectProvider.yaml"

testGetUser :: GetUser -> TestTree
testGetUser = req
    "GetUser"
    "fixture/GetUser.yaml"

testListAttachedUserPolicies :: ListAttachedUserPolicies -> TestTree
testListAttachedUserPolicies = req
    "ListAttachedUserPolicies"
    "fixture/ListAttachedUserPolicies.yaml"

testDeleteSigningCertificate :: DeleteSigningCertificate -> TestTree
testDeleteSigningCertificate = req
    "DeleteSigningCertificate"
    "fixture/DeleteSigningCertificate.yaml"

testUpdateSigningCertificate :: UpdateSigningCertificate -> TestTree
testUpdateSigningCertificate = req
    "UpdateSigningCertificate"
    "fixture/UpdateSigningCertificate.yaml"

testListSigningCertificates :: ListSigningCertificates -> TestTree
testListSigningCertificates = req
    "ListSigningCertificates"
    "fixture/ListSigningCertificates.yaml"

testRemoveClientIdFromOpenIdConnectProvider :: RemoveClientIdFromOpenIdConnectProvider -> TestTree
testRemoveClientIdFromOpenIdConnectProvider = req
    "RemoveClientIdFromOpenIdConnectProvider"
    "fixture/RemoveClientIdFromOpenIdConnectProvider.yaml"

testListAccessKeys :: ListAccessKeys -> TestTree
testListAccessKeys = req
    "ListAccessKeys"
    "fixture/ListAccessKeys.yaml"

testListVirtualMFADevices :: ListVirtualMFADevices -> TestTree
testListVirtualMFADevices = req
    "ListVirtualMFADevices"
    "fixture/ListVirtualMFADevices.yaml"

testDeleteAccessKey :: DeleteAccessKey -> TestTree
testDeleteAccessKey = req
    "DeleteAccessKey"
    "fixture/DeleteAccessKey.yaml"

testUpdateAccessKey :: UpdateAccessKey -> TestTree
testUpdateAccessKey = req
    "UpdateAccessKey"
    "fixture/UpdateAccessKey.yaml"

testGetRolePolicy :: GetRolePolicy -> TestTree
testGetRolePolicy = req
    "GetRolePolicy"
    "fixture/GetRolePolicy.yaml"

testAttachUserPolicy :: AttachUserPolicy -> TestTree
testAttachUserPolicy = req
    "AttachUserPolicy"
    "fixture/AttachUserPolicy.yaml"

testResyncMFADevice :: ResyncMFADevice -> TestTree
testResyncMFADevice = req
    "ResyncMFADevice"
    "fixture/ResyncMFADevice.yaml"

testCreateUser :: CreateUser -> TestTree
testCreateUser = req
    "CreateUser"
    "fixture/CreateUser.yaml"

testUploadSigningCertificate :: UploadSigningCertificate -> TestTree
testUploadSigningCertificate = req
    "UploadSigningCertificate"
    "fixture/UploadSigningCertificate.yaml"

testPutRolePolicy :: PutRolePolicy -> TestTree
testPutRolePolicy = req
    "PutRolePolicy"
    "fixture/PutRolePolicy.yaml"

testDeleteRolePolicy :: DeleteRolePolicy -> TestTree
testDeleteRolePolicy = req
    "DeleteRolePolicy"
    "fixture/DeleteRolePolicy.yaml"

testUpdateUser :: UpdateUser -> TestTree
testUpdateUser = req
    "UpdateUser"
    "fixture/UpdateUser.yaml"

testDeleteUser :: DeleteUser -> TestTree
testDeleteUser = req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

testListRolePolicies :: ListRolePolicies -> TestTree
testListRolePolicies = req
    "ListRolePolicies"
    "fixture/ListRolePolicies.yaml"

testAddClientIdToOpenIdConnectProvider :: AddClientIdToOpenIdConnectProvider -> TestTree
testAddClientIdToOpenIdConnectProvider = req
    "AddClientIdToOpenIdConnectProvider"
    "fixture/AddClientIdToOpenIdConnectProvider.yaml"

testGetAccessKeyLastUsed :: GetAccessKeyLastUsed -> TestTree
testGetAccessKeyLastUsed = req
    "GetAccessKeyLastUsed"
    "fixture/GetAccessKeyLastUsed.yaml"

testGetAccountPasswordPolicy :: GetAccountPasswordPolicy -> TestTree
testGetAccountPasswordPolicy = req
    "GetAccountPasswordPolicy"
    "fixture/GetAccountPasswordPolicy.yaml"

testListAccountAliases :: ListAccountAliases -> TestTree
testListAccountAliases = req
    "ListAccountAliases"
    "fixture/ListAccountAliases.yaml"

testCreateAccountAlias :: CreateAccountAlias -> TestTree
testCreateAccountAlias = req
    "CreateAccountAlias"
    "fixture/CreateAccountAlias.yaml"

testUploadServerCertificate :: UploadServerCertificate -> TestTree
testUploadServerCertificate = req
    "UploadServerCertificate"
    "fixture/UploadServerCertificate.yaml"

testListMFADevices :: ListMFADevices -> TestTree
testListMFADevices = req
    "ListMFADevices"
    "fixture/ListMFADevices.yaml"

testEnableMFADevice :: EnableMFADevice -> TestTree
testEnableMFADevice = req
    "EnableMFADevice"
    "fixture/EnableMFADevice.yaml"

testListPolicyVersions :: ListPolicyVersions -> TestTree
testListPolicyVersions = req
    "ListPolicyVersions"
    "fixture/ListPolicyVersions.yaml"

testListSAMLProviders :: ListSAMLProviders -> TestTree
testListSAMLProviders = req
    "ListSAMLProviders"
    "fixture/ListSAMLProviders.yaml"

testUpdateSAMLProvider :: UpdateSAMLProvider -> TestTree
testUpdateSAMLProvider = req
    "UpdateSAMLProvider"
    "fixture/UpdateSAMLProvider.yaml"

testDeleteSAMLProvider :: DeleteSAMLProvider -> TestTree
testDeleteSAMLProvider = req
    "DeleteSAMLProvider"
    "fixture/DeleteSAMLProvider.yaml"

testCreateGroup :: CreateGroup -> TestTree
testCreateGroup = req
    "CreateGroup"
    "fixture/CreateGroup.yaml"

testSetDefaultPolicyVersion :: SetDefaultPolicyVersion -> TestTree
testSetDefaultPolicyVersion = req
    "SetDefaultPolicyVersion"
    "fixture/SetDefaultPolicyVersion.yaml"

testListInstanceProfiles :: ListInstanceProfiles -> TestTree
testListInstanceProfiles = req
    "ListInstanceProfiles"
    "fixture/ListInstanceProfiles.yaml"

testListGroups :: ListGroups -> TestTree
testListGroups = req
    "ListGroups"
    "fixture/ListGroups.yaml"

testDeleteGroup :: DeleteGroup -> TestTree
testDeleteGroup = req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

testUpdateGroup :: UpdateGroup -> TestTree
testUpdateGroup = req
    "UpdateGroup"
    "fixture/UpdateGroup.yaml"

testGetServerCertificate :: GetServerCertificate -> TestTree
testGetServerCertificate = req
    "GetServerCertificate"
    "fixture/GetServerCertificate.yaml"

testGetPolicy :: GetPolicy -> TestTree
testGetPolicy = req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

testGenerateCredentialReport :: GenerateCredentialReport -> TestTree
testGenerateCredentialReport = req
    "GenerateCredentialReport"
    "fixture/GenerateCredentialReport.yaml"

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

testDeleteLoginProfile :: DeleteLoginProfile -> TestTree
testDeleteLoginProfile = req
    "DeleteLoginProfile"
    "fixture/DeleteLoginProfile.yaml"

testUpdateLoginProfile :: UpdateLoginProfile -> TestTree
testUpdateLoginProfile = req
    "UpdateLoginProfile"
    "fixture/UpdateLoginProfile.yaml"

testListAttachedGroupPolicies :: ListAttachedGroupPolicies -> TestTree
testListAttachedGroupPolicies = req
    "ListAttachedGroupPolicies"
    "fixture/ListAttachedGroupPolicies.yaml"

-- Responses

testAttachGroupPolicyResponse :: AttachGroupPolicyResponse -> TestTree
testAttachGroupPolicyResponse = res
    "AttachGroupPolicyResponse"
    "fixture/AttachGroupPolicyResponse.proto"
    iAM
    (Proxy :: Proxy AttachGroupPolicy)

testListInstanceProfilesForRoleResponse :: ListInstanceProfilesForRoleResponse -> TestTree
testListInstanceProfilesForRoleResponse = res
    "ListInstanceProfilesForRoleResponse"
    "fixture/ListInstanceProfilesForRoleResponse.proto"
    iAM
    (Proxy :: Proxy ListInstanceProfilesForRole)

testCreatePolicyResponse :: CreatePolicyResponse -> TestTree
testCreatePolicyResponse = res
    "CreatePolicyResponse"
    "fixture/CreatePolicyResponse.proto"
    iAM
    (Proxy :: Proxy CreatePolicy)

testListPoliciesResponse :: ListPoliciesResponse -> TestTree
testListPoliciesResponse = res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    iAM
    (Proxy :: Proxy ListPolicies)

testAttachRolePolicyResponse :: AttachRolePolicyResponse -> TestTree
testAttachRolePolicyResponse = res
    "AttachRolePolicyResponse"
    "fixture/AttachRolePolicyResponse.proto"
    iAM
    (Proxy :: Proxy AttachRolePolicy)

testListSSHPublicKeysResponse :: ListSSHPublicKeysResponse -> TestTree
testListSSHPublicKeysResponse = res
    "ListSSHPublicKeysResponse"
    "fixture/ListSSHPublicKeysResponse.proto"
    iAM
    (Proxy :: Proxy ListSSHPublicKeys)

testDeleteSSHPublicKeyResponse :: DeleteSSHPublicKeyResponse -> TestTree
testDeleteSSHPublicKeyResponse = res
    "DeleteSSHPublicKeyResponse"
    "fixture/DeleteSSHPublicKeyResponse.proto"
    iAM
    (Proxy :: Proxy DeleteSSHPublicKey)

testUpdateSSHPublicKeyResponse :: UpdateSSHPublicKeyResponse -> TestTree
testUpdateSSHPublicKeyResponse = res
    "UpdateSSHPublicKeyResponse"
    "fixture/UpdateSSHPublicKeyResponse.proto"
    iAM
    (Proxy :: Proxy UpdateSSHPublicKey)

testListOpenIdConnectProvidersResponse :: ListOpenIdConnectProvidersResponse -> TestTree
testListOpenIdConnectProvidersResponse = res
    "ListOpenIdConnectProvidersResponse"
    "fixture/ListOpenIdConnectProvidersResponse.proto"
    iAM
    (Proxy :: Proxy ListOpenIdConnectProviders)

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

testCreateAccessKeyResponse :: CreateAccessKeyResponse -> TestTree
testCreateAccessKeyResponse = res
    "CreateAccessKeyResponse"
    "fixture/CreateAccessKeyResponse.proto"
    iAM
    (Proxy :: Proxy CreateAccessKey)

testGetUserPolicyResponse :: GetUserPolicyResponse -> TestTree
testGetUserPolicyResponse = res
    "GetUserPolicyResponse"
    "fixture/GetUserPolicyResponse.proto"
    iAM
    (Proxy :: Proxy GetUserPolicy)

testCreateVirtualMFADeviceResponse :: CreateVirtualMFADeviceResponse -> TestTree
testCreateVirtualMFADeviceResponse = res
    "CreateVirtualMFADeviceResponse"
    "fixture/CreateVirtualMFADeviceResponse.proto"
    iAM
    (Proxy :: Proxy CreateVirtualMFADevice)

testCreateOpenIdConnectProviderResponse :: CreateOpenIdConnectProviderResponse -> TestTree
testCreateOpenIdConnectProviderResponse = res
    "CreateOpenIdConnectProviderResponse"
    "fixture/CreateOpenIdConnectProviderResponse.proto"
    iAM
    (Proxy :: Proxy CreateOpenIdConnectProvider)

testListAttachedRolePoliciesResponse :: ListAttachedRolePoliciesResponse -> TestTree
testListAttachedRolePoliciesResponse = res
    "ListAttachedRolePoliciesResponse"
    "fixture/ListAttachedRolePoliciesResponse.proto"
    iAM
    (Proxy :: Proxy ListAttachedRolePolicies)

testDeleteVirtualMFADeviceResponse :: DeleteVirtualMFADeviceResponse -> TestTree
testDeleteVirtualMFADeviceResponse = res
    "DeleteVirtualMFADeviceResponse"
    "fixture/DeleteVirtualMFADeviceResponse.proto"
    iAM
    (Proxy :: Proxy DeleteVirtualMFADevice)

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

testListRolesResponse :: ListRolesResponse -> TestTree
testListRolesResponse = res
    "ListRolesResponse"
    "fixture/ListRolesResponse.proto"
    iAM
    (Proxy :: Proxy ListRoles)

testDeleteRoleResponse :: DeleteRoleResponse -> TestTree
testDeleteRoleResponse = res
    "DeleteRoleResponse"
    "fixture/DeleteRoleResponse.proto"
    iAM
    (Proxy :: Proxy DeleteRole)

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

testGetSSHPublicKeyResponse :: GetSSHPublicKeyResponse -> TestTree
testGetSSHPublicKeyResponse = res
    "GetSSHPublicKeyResponse"
    "fixture/GetSSHPublicKeyResponse.proto"
    iAM
    (Proxy :: Proxy GetSSHPublicKey)

testPutUserPolicyResponse :: PutUserPolicyResponse -> TestTree
testPutUserPolicyResponse = res
    "PutUserPolicyResponse"
    "fixture/PutUserPolicyResponse.proto"
    iAM
    (Proxy :: Proxy PutUserPolicy)

testCreateRoleResponse :: CreateRoleResponse -> TestTree
testCreateRoleResponse = res
    "CreateRoleResponse"
    "fixture/CreateRoleResponse.proto"
    iAM
    (Proxy :: Proxy CreateRole)

testDeleteUserPolicyResponse :: DeleteUserPolicyResponse -> TestTree
testDeleteUserPolicyResponse = res
    "DeleteUserPolicyResponse"
    "fixture/DeleteUserPolicyResponse.proto"
    iAM
    (Proxy :: Proxy DeleteUserPolicy)

testGetOpenIdConnectProviderResponse :: GetOpenIdConnectProviderResponse -> TestTree
testGetOpenIdConnectProviderResponse = res
    "GetOpenIdConnectProviderResponse"
    "fixture/GetOpenIdConnectProviderResponse.proto"
    iAM
    (Proxy :: Proxy GetOpenIdConnectProvider)

testDetachGroupPolicyResponse :: DetachGroupPolicyResponse -> TestTree
testDetachGroupPolicyResponse = res
    "DetachGroupPolicyResponse"
    "fixture/DetachGroupPolicyResponse.proto"
    iAM
    (Proxy :: Proxy DetachGroupPolicy)

testGetCredentialReportResponse :: GetCredentialReportResponse -> TestTree
testGetCredentialReportResponse = res
    "GetCredentialReportResponse"
    "fixture/GetCredentialReportResponse.proto"
    iAM
    (Proxy :: Proxy GetCredentialReport)

testDeletePolicyVersionResponse :: DeletePolicyVersionResponse -> TestTree
testDeletePolicyVersionResponse = res
    "DeletePolicyVersionResponse"
    "fixture/DeletePolicyVersionResponse.proto"
    iAM
    (Proxy :: Proxy DeletePolicyVersion)

testDetachRolePolicyResponse :: DetachRolePolicyResponse -> TestTree
testDetachRolePolicyResponse = res
    "DetachRolePolicyResponse"
    "fixture/DetachRolePolicyResponse.proto"
    iAM
    (Proxy :: Proxy DetachRolePolicy)

testDeleteInstanceProfileResponse :: DeleteInstanceProfileResponse -> TestTree
testDeleteInstanceProfileResponse = res
    "DeleteInstanceProfileResponse"
    "fixture/DeleteInstanceProfileResponse.proto"
    iAM
    (Proxy :: Proxy DeleteInstanceProfile)

testListGroupPoliciesResponse :: ListGroupPoliciesResponse -> TestTree
testListGroupPoliciesResponse = res
    "ListGroupPoliciesResponse"
    "fixture/ListGroupPoliciesResponse.proto"
    iAM
    (Proxy :: Proxy ListGroupPolicies)

testGetAccountSummaryResponse :: GetAccountSummaryResponse -> TestTree
testGetAccountSummaryResponse = res
    "GetAccountSummaryResponse"
    "fixture/GetAccountSummaryResponse.proto"
    iAM
    (Proxy :: Proxy GetAccountSummary)

testCreateInstanceProfileResponse :: CreateInstanceProfileResponse -> TestTree
testCreateInstanceProfileResponse = res
    "CreateInstanceProfileResponse"
    "fixture/CreateInstanceProfileResponse.proto"
    iAM
    (Proxy :: Proxy CreateInstanceProfile)

testPutGroupPolicyResponse :: PutGroupPolicyResponse -> TestTree
testPutGroupPolicyResponse = res
    "PutGroupPolicyResponse"
    "fixture/PutGroupPolicyResponse.proto"
    iAM
    (Proxy :: Proxy PutGroupPolicy)

testDeleteGroupPolicyResponse :: DeleteGroupPolicyResponse -> TestTree
testDeleteGroupPolicyResponse = res
    "DeleteGroupPolicyResponse"
    "fixture/DeleteGroupPolicyResponse.proto"
    iAM
    (Proxy :: Proxy DeleteGroupPolicy)

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

testRemoveRoleFromInstanceProfileResponse :: RemoveRoleFromInstanceProfileResponse -> TestTree
testRemoveRoleFromInstanceProfileResponse = res
    "RemoveRoleFromInstanceProfileResponse"
    "fixture/RemoveRoleFromInstanceProfileResponse.proto"
    iAM
    (Proxy :: Proxy RemoveRoleFromInstanceProfile)

testGetLoginProfileResponse :: GetLoginProfileResponse -> TestTree
testGetLoginProfileResponse = res
    "GetLoginProfileResponse"
    "fixture/GetLoginProfileResponse.proto"
    iAM
    (Proxy :: Proxy GetLoginProfile)

testRemoveUserFromGroupResponse :: RemoveUserFromGroupResponse -> TestTree
testRemoveUserFromGroupResponse = res
    "RemoveUserFromGroupResponse"
    "fixture/RemoveUserFromGroupResponse.proto"
    iAM
    (Proxy :: Proxy RemoveUserFromGroup)

testDetachUserPolicyResponse :: DetachUserPolicyResponse -> TestTree
testDetachUserPolicyResponse = res
    "DetachUserPolicyResponse"
    "fixture/DetachUserPolicyResponse.proto"
    iAM
    (Proxy :: Proxy DetachUserPolicy)

testCreateSAMLProviderResponse :: CreateSAMLProviderResponse -> TestTree
testCreateSAMLProviderResponse = res
    "CreateSAMLProviderResponse"
    "fixture/CreateSAMLProviderResponse.proto"
    iAM
    (Proxy :: Proxy CreateSAMLProvider)

testCreatePolicyVersionResponse :: CreatePolicyVersionResponse -> TestTree
testCreatePolicyVersionResponse = res
    "CreatePolicyVersionResponse"
    "fixture/CreatePolicyVersionResponse.proto"
    iAM
    (Proxy :: Proxy CreatePolicyVersion)

testGetGroupPolicyResponse :: GetGroupPolicyResponse -> TestTree
testGetGroupPolicyResponse = res
    "GetGroupPolicyResponse"
    "fixture/GetGroupPolicyResponse.proto"
    iAM
    (Proxy :: Proxy GetGroupPolicy)

testDeletePolicyResponse :: DeletePolicyResponse -> TestTree
testDeletePolicyResponse = res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    iAM
    (Proxy :: Proxy DeletePolicy)

testListServerCertificatesResponse :: ListServerCertificatesResponse -> TestTree
testListServerCertificatesResponse = res
    "ListServerCertificatesResponse"
    "fixture/ListServerCertificatesResponse.proto"
    iAM
    (Proxy :: Proxy ListServerCertificates)

testUpdateAssumeRolePolicyResponse :: UpdateAssumeRolePolicyResponse -> TestTree
testUpdateAssumeRolePolicyResponse = res
    "UpdateAssumeRolePolicyResponse"
    "fixture/UpdateAssumeRolePolicyResponse.proto"
    iAM
    (Proxy :: Proxy UpdateAssumeRolePolicy)

testChangePasswordResponse :: ChangePasswordResponse -> TestTree
testChangePasswordResponse = res
    "ChangePasswordResponse"
    "fixture/ChangePasswordResponse.proto"
    iAM
    (Proxy :: Proxy ChangePassword)

testListGroupsForUserResponse :: ListGroupsForUserResponse -> TestTree
testListGroupsForUserResponse = res
    "ListGroupsForUserResponse"
    "fixture/ListGroupsForUserResponse.proto"
    iAM
    (Proxy :: Proxy ListGroupsForUser)

testGetPolicyVersionResponse :: GetPolicyVersionResponse -> TestTree
testGetPolicyVersionResponse = res
    "GetPolicyVersionResponse"
    "fixture/GetPolicyVersionResponse.proto"
    iAM
    (Proxy :: Proxy GetPolicyVersion)

testCreateLoginProfileResponse :: CreateLoginProfileResponse -> TestTree
testCreateLoginProfileResponse = res
    "CreateLoginProfileResponse"
    "fixture/CreateLoginProfileResponse.proto"
    iAM
    (Proxy :: Proxy CreateLoginProfile)

testGetInstanceProfileResponse :: GetInstanceProfileResponse -> TestTree
testGetInstanceProfileResponse = res
    "GetInstanceProfileResponse"
    "fixture/GetInstanceProfileResponse.proto"
    iAM
    (Proxy :: Proxy GetInstanceProfile)

testListEntitiesForPolicyResponse :: ListEntitiesForPolicyResponse -> TestTree
testListEntitiesForPolicyResponse = res
    "ListEntitiesForPolicyResponse"
    "fixture/ListEntitiesForPolicyResponse.proto"
    iAM
    (Proxy :: Proxy ListEntitiesForPolicy)

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

testAddUserToGroupResponse :: AddUserToGroupResponse -> TestTree
testAddUserToGroupResponse = res
    "AddUserToGroupResponse"
    "fixture/AddUserToGroupResponse.proto"
    iAM
    (Proxy :: Proxy AddUserToGroup)

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

testListAttachedUserPoliciesResponse :: ListAttachedUserPoliciesResponse -> TestTree
testListAttachedUserPoliciesResponse = res
    "ListAttachedUserPoliciesResponse"
    "fixture/ListAttachedUserPoliciesResponse.proto"
    iAM
    (Proxy :: Proxy ListAttachedUserPolicies)

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

testListSigningCertificatesResponse :: ListSigningCertificatesResponse -> TestTree
testListSigningCertificatesResponse = res
    "ListSigningCertificatesResponse"
    "fixture/ListSigningCertificatesResponse.proto"
    iAM
    (Proxy :: Proxy ListSigningCertificates)

testRemoveClientIdFromOpenIdConnectProviderResponse :: RemoveClientIdFromOpenIdConnectProviderResponse -> TestTree
testRemoveClientIdFromOpenIdConnectProviderResponse = res
    "RemoveClientIdFromOpenIdConnectProviderResponse"
    "fixture/RemoveClientIdFromOpenIdConnectProviderResponse.proto"
    iAM
    (Proxy :: Proxy RemoveClientIdFromOpenIdConnectProvider)

testListAccessKeysResponse :: ListAccessKeysResponse -> TestTree
testListAccessKeysResponse = res
    "ListAccessKeysResponse"
    "fixture/ListAccessKeysResponse.proto"
    iAM
    (Proxy :: Proxy ListAccessKeys)

testListVirtualMFADevicesResponse :: ListVirtualMFADevicesResponse -> TestTree
testListVirtualMFADevicesResponse = res
    "ListVirtualMFADevicesResponse"
    "fixture/ListVirtualMFADevicesResponse.proto"
    iAM
    (Proxy :: Proxy ListVirtualMFADevices)

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

testGetRolePolicyResponse :: GetRolePolicyResponse -> TestTree
testGetRolePolicyResponse = res
    "GetRolePolicyResponse"
    "fixture/GetRolePolicyResponse.proto"
    iAM
    (Proxy :: Proxy GetRolePolicy)

testAttachUserPolicyResponse :: AttachUserPolicyResponse -> TestTree
testAttachUserPolicyResponse = res
    "AttachUserPolicyResponse"
    "fixture/AttachUserPolicyResponse.proto"
    iAM
    (Proxy :: Proxy AttachUserPolicy)

testResyncMFADeviceResponse :: ResyncMFADeviceResponse -> TestTree
testResyncMFADeviceResponse = res
    "ResyncMFADeviceResponse"
    "fixture/ResyncMFADeviceResponse.proto"
    iAM
    (Proxy :: Proxy ResyncMFADevice)

testCreateUserResponse :: CreateUserResponse -> TestTree
testCreateUserResponse = res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    iAM
    (Proxy :: Proxy CreateUser)

testUploadSigningCertificateResponse :: UploadSigningCertificateResponse -> TestTree
testUploadSigningCertificateResponse = res
    "UploadSigningCertificateResponse"
    "fixture/UploadSigningCertificateResponse.proto"
    iAM
    (Proxy :: Proxy UploadSigningCertificate)

testPutRolePolicyResponse :: PutRolePolicyResponse -> TestTree
testPutRolePolicyResponse = res
    "PutRolePolicyResponse"
    "fixture/PutRolePolicyResponse.proto"
    iAM
    (Proxy :: Proxy PutRolePolicy)

testDeleteRolePolicyResponse :: DeleteRolePolicyResponse -> TestTree
testDeleteRolePolicyResponse = res
    "DeleteRolePolicyResponse"
    "fixture/DeleteRolePolicyResponse.proto"
    iAM
    (Proxy :: Proxy DeleteRolePolicy)

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

testListRolePoliciesResponse :: ListRolePoliciesResponse -> TestTree
testListRolePoliciesResponse = res
    "ListRolePoliciesResponse"
    "fixture/ListRolePoliciesResponse.proto"
    iAM
    (Proxy :: Proxy ListRolePolicies)

testAddClientIdToOpenIdConnectProviderResponse :: AddClientIdToOpenIdConnectProviderResponse -> TestTree
testAddClientIdToOpenIdConnectProviderResponse = res
    "AddClientIdToOpenIdConnectProviderResponse"
    "fixture/AddClientIdToOpenIdConnectProviderResponse.proto"
    iAM
    (Proxy :: Proxy AddClientIdToOpenIdConnectProvider)

testGetAccessKeyLastUsedResponse :: GetAccessKeyLastUsedResponse -> TestTree
testGetAccessKeyLastUsedResponse = res
    "GetAccessKeyLastUsedResponse"
    "fixture/GetAccessKeyLastUsedResponse.proto"
    iAM
    (Proxy :: Proxy GetAccessKeyLastUsed)

testGetAccountPasswordPolicyResponse :: GetAccountPasswordPolicyResponse -> TestTree
testGetAccountPasswordPolicyResponse = res
    "GetAccountPasswordPolicyResponse"
    "fixture/GetAccountPasswordPolicyResponse.proto"
    iAM
    (Proxy :: Proxy GetAccountPasswordPolicy)

testListAccountAliasesResponse :: ListAccountAliasesResponse -> TestTree
testListAccountAliasesResponse = res
    "ListAccountAliasesResponse"
    "fixture/ListAccountAliasesResponse.proto"
    iAM
    (Proxy :: Proxy ListAccountAliases)

testCreateAccountAliasResponse :: CreateAccountAliasResponse -> TestTree
testCreateAccountAliasResponse = res
    "CreateAccountAliasResponse"
    "fixture/CreateAccountAliasResponse.proto"
    iAM
    (Proxy :: Proxy CreateAccountAlias)

testUploadServerCertificateResponse :: UploadServerCertificateResponse -> TestTree
testUploadServerCertificateResponse = res
    "UploadServerCertificateResponse"
    "fixture/UploadServerCertificateResponse.proto"
    iAM
    (Proxy :: Proxy UploadServerCertificate)

testListMFADevicesResponse :: ListMFADevicesResponse -> TestTree
testListMFADevicesResponse = res
    "ListMFADevicesResponse"
    "fixture/ListMFADevicesResponse.proto"
    iAM
    (Proxy :: Proxy ListMFADevices)

testEnableMFADeviceResponse :: EnableMFADeviceResponse -> TestTree
testEnableMFADeviceResponse = res
    "EnableMFADeviceResponse"
    "fixture/EnableMFADeviceResponse.proto"
    iAM
    (Proxy :: Proxy EnableMFADevice)

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

testUpdateSAMLProviderResponse :: UpdateSAMLProviderResponse -> TestTree
testUpdateSAMLProviderResponse = res
    "UpdateSAMLProviderResponse"
    "fixture/UpdateSAMLProviderResponse.proto"
    iAM
    (Proxy :: Proxy UpdateSAMLProvider)

testDeleteSAMLProviderResponse :: DeleteSAMLProviderResponse -> TestTree
testDeleteSAMLProviderResponse = res
    "DeleteSAMLProviderResponse"
    "fixture/DeleteSAMLProviderResponse.proto"
    iAM
    (Proxy :: Proxy DeleteSAMLProvider)

testCreateGroupResponse :: CreateGroupResponse -> TestTree
testCreateGroupResponse = res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    iAM
    (Proxy :: Proxy CreateGroup)

testSetDefaultPolicyVersionResponse :: SetDefaultPolicyVersionResponse -> TestTree
testSetDefaultPolicyVersionResponse = res
    "SetDefaultPolicyVersionResponse"
    "fixture/SetDefaultPolicyVersionResponse.proto"
    iAM
    (Proxy :: Proxy SetDefaultPolicyVersion)

testListInstanceProfilesResponse :: ListInstanceProfilesResponse -> TestTree
testListInstanceProfilesResponse = res
    "ListInstanceProfilesResponse"
    "fixture/ListInstanceProfilesResponse.proto"
    iAM
    (Proxy :: Proxy ListInstanceProfiles)

testListGroupsResponse :: ListGroupsResponse -> TestTree
testListGroupsResponse = res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    iAM
    (Proxy :: Proxy ListGroups)

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

testGetServerCertificateResponse :: GetServerCertificateResponse -> TestTree
testGetServerCertificateResponse = res
    "GetServerCertificateResponse"
    "fixture/GetServerCertificateResponse.proto"
    iAM
    (Proxy :: Proxy GetServerCertificate)

testGetPolicyResponse :: GetPolicyResponse -> TestTree
testGetPolicyResponse = res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    iAM
    (Proxy :: Proxy GetPolicy)

testGenerateCredentialReportResponse :: GenerateCredentialReportResponse -> TestTree
testGenerateCredentialReportResponse = res
    "GenerateCredentialReportResponse"
    "fixture/GenerateCredentialReportResponse.proto"
    iAM
    (Proxy :: Proxy GenerateCredentialReport)

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

testDeleteLoginProfileResponse :: DeleteLoginProfileResponse -> TestTree
testDeleteLoginProfileResponse = res
    "DeleteLoginProfileResponse"
    "fixture/DeleteLoginProfileResponse.proto"
    iAM
    (Proxy :: Proxy DeleteLoginProfile)

testUpdateLoginProfileResponse :: UpdateLoginProfileResponse -> TestTree
testUpdateLoginProfileResponse = res
    "UpdateLoginProfileResponse"
    "fixture/UpdateLoginProfileResponse.proto"
    iAM
    (Proxy :: Proxy UpdateLoginProfile)

testListAttachedGroupPoliciesResponse :: ListAttachedGroupPoliciesResponse -> TestTree
testListAttachedGroupPoliciesResponse = res
    "ListAttachedGroupPoliciesResponse"
    "fixture/ListAttachedGroupPoliciesResponse.proto"
    iAM
    (Proxy :: Proxy ListAttachedGroupPolicies)
