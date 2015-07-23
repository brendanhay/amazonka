{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.IAM
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
    "fixture/AttachGroupPolicy"

testListInstanceProfilesForRole :: ListInstanceProfilesForRole -> TestTree
testListInstanceProfilesForRole = req
    "ListInstanceProfilesForRole"
    "fixture/ListInstanceProfilesForRole"

testCreatePolicy :: CreatePolicy -> TestTree
testCreatePolicy = req
    "CreatePolicy"
    "fixture/CreatePolicy"

testListPolicies :: ListPolicies -> TestTree
testListPolicies = req
    "ListPolicies"
    "fixture/ListPolicies"

testAttachRolePolicy :: AttachRolePolicy -> TestTree
testAttachRolePolicy = req
    "AttachRolePolicy"
    "fixture/AttachRolePolicy"

testListSSHPublicKeys :: ListSSHPublicKeys -> TestTree
testListSSHPublicKeys = req
    "ListSSHPublicKeys"
    "fixture/ListSSHPublicKeys"

testDeleteSSHPublicKey :: DeleteSSHPublicKey -> TestTree
testDeleteSSHPublicKey = req
    "DeleteSSHPublicKey"
    "fixture/DeleteSSHPublicKey"

testUpdateSSHPublicKey :: UpdateSSHPublicKey -> TestTree
testUpdateSSHPublicKey = req
    "UpdateSSHPublicKey"
    "fixture/UpdateSSHPublicKey"

testListOpenIdConnectProviders :: ListOpenIdConnectProviders -> TestTree
testListOpenIdConnectProviders = req
    "ListOpenIdConnectProviders"
    "fixture/ListOpenIdConnectProviders"

testDeleteAccountPasswordPolicy :: DeleteAccountPasswordPolicy -> TestTree
testDeleteAccountPasswordPolicy = req
    "DeleteAccountPasswordPolicy"
    "fixture/DeleteAccountPasswordPolicy"

testUpdateAccountPasswordPolicy :: UpdateAccountPasswordPolicy -> TestTree
testUpdateAccountPasswordPolicy = req
    "UpdateAccountPasswordPolicy"
    "fixture/UpdateAccountPasswordPolicy"

testCreateAccessKey :: CreateAccessKey -> TestTree
testCreateAccessKey = req
    "CreateAccessKey"
    "fixture/CreateAccessKey"

testGetUserPolicy :: GetUserPolicy -> TestTree
testGetUserPolicy = req
    "GetUserPolicy"
    "fixture/GetUserPolicy"

testCreateVirtualMFADevice :: CreateVirtualMFADevice -> TestTree
testCreateVirtualMFADevice = req
    "CreateVirtualMFADevice"
    "fixture/CreateVirtualMFADevice"

testCreateOpenIdConnectProvider :: CreateOpenIdConnectProvider -> TestTree
testCreateOpenIdConnectProvider = req
    "CreateOpenIdConnectProvider"
    "fixture/CreateOpenIdConnectProvider"

testListAttachedRolePolicies :: ListAttachedRolePolicies -> TestTree
testListAttachedRolePolicies = req
    "ListAttachedRolePolicies"
    "fixture/ListAttachedRolePolicies"

testDeleteVirtualMFADevice :: DeleteVirtualMFADevice -> TestTree
testDeleteVirtualMFADevice = req
    "DeleteVirtualMFADevice"
    "fixture/DeleteVirtualMFADevice"

testGetRole :: GetRole -> TestTree
testGetRole = req
    "GetRole"
    "fixture/GetRole"

testDeactivateMFADevice :: DeactivateMFADevice -> TestTree
testDeactivateMFADevice = req
    "DeactivateMFADevice"
    "fixture/DeactivateMFADevice"

testListRoles :: ListRoles -> TestTree
testListRoles = req
    "ListRoles"
    "fixture/ListRoles"

testDeleteRole :: DeleteRole -> TestTree
testDeleteRole = req
    "DeleteRole"
    "fixture/DeleteRole"

testListUserPolicies :: ListUserPolicies -> TestTree
testListUserPolicies = req
    "ListUserPolicies"
    "fixture/ListUserPolicies"

testUploadSSHPublicKey :: UploadSSHPublicKey -> TestTree
testUploadSSHPublicKey = req
    "UploadSSHPublicKey"
    "fixture/UploadSSHPublicKey"

testListUsers :: ListUsers -> TestTree
testListUsers = req
    "ListUsers"
    "fixture/ListUsers"

testUpdateOpenIdConnectProviderThumbprint :: UpdateOpenIdConnectProviderThumbprint -> TestTree
testUpdateOpenIdConnectProviderThumbprint = req
    "UpdateOpenIdConnectProviderThumbprint"
    "fixture/UpdateOpenIdConnectProviderThumbprint"

testGetSSHPublicKey :: GetSSHPublicKey -> TestTree
testGetSSHPublicKey = req
    "GetSSHPublicKey"
    "fixture/GetSSHPublicKey"

testPutUserPolicy :: PutUserPolicy -> TestTree
testPutUserPolicy = req
    "PutUserPolicy"
    "fixture/PutUserPolicy"

testCreateRole :: CreateRole -> TestTree
testCreateRole = req
    "CreateRole"
    "fixture/CreateRole"

testDeleteUserPolicy :: DeleteUserPolicy -> TestTree
testDeleteUserPolicy = req
    "DeleteUserPolicy"
    "fixture/DeleteUserPolicy"

testGetOpenIdConnectProvider :: GetOpenIdConnectProvider -> TestTree
testGetOpenIdConnectProvider = req
    "GetOpenIdConnectProvider"
    "fixture/GetOpenIdConnectProvider"

testDetachGroupPolicy :: DetachGroupPolicy -> TestTree
testDetachGroupPolicy = req
    "DetachGroupPolicy"
    "fixture/DetachGroupPolicy"

testGetCredentialReport :: GetCredentialReport -> TestTree
testGetCredentialReport = req
    "GetCredentialReport"
    "fixture/GetCredentialReport"

testDeletePolicyVersion :: DeletePolicyVersion -> TestTree
testDeletePolicyVersion = req
    "DeletePolicyVersion"
    "fixture/DeletePolicyVersion"

testDetachRolePolicy :: DetachRolePolicy -> TestTree
testDetachRolePolicy = req
    "DetachRolePolicy"
    "fixture/DetachRolePolicy"

testDeleteInstanceProfile :: DeleteInstanceProfile -> TestTree
testDeleteInstanceProfile = req
    "DeleteInstanceProfile"
    "fixture/DeleteInstanceProfile"

testListGroupPolicies :: ListGroupPolicies -> TestTree
testListGroupPolicies = req
    "ListGroupPolicies"
    "fixture/ListGroupPolicies"

testGetAccountSummary :: GetAccountSummary -> TestTree
testGetAccountSummary = req
    "GetAccountSummary"
    "fixture/GetAccountSummary"

testCreateInstanceProfile :: CreateInstanceProfile -> TestTree
testCreateInstanceProfile = req
    "CreateInstanceProfile"
    "fixture/CreateInstanceProfile"

testPutGroupPolicy :: PutGroupPolicy -> TestTree
testPutGroupPolicy = req
    "PutGroupPolicy"
    "fixture/PutGroupPolicy"

testDeleteGroupPolicy :: DeleteGroupPolicy -> TestTree
testDeleteGroupPolicy = req
    "DeleteGroupPolicy"
    "fixture/DeleteGroupPolicy"

testGetAccountAuthorizationDetails :: GetAccountAuthorizationDetails -> TestTree
testGetAccountAuthorizationDetails = req
    "GetAccountAuthorizationDetails"
    "fixture/GetAccountAuthorizationDetails"

testDeleteAccountAlias :: DeleteAccountAlias -> TestTree
testDeleteAccountAlias = req
    "DeleteAccountAlias"
    "fixture/DeleteAccountAlias"

testRemoveRoleFromInstanceProfile :: RemoveRoleFromInstanceProfile -> TestTree
testRemoveRoleFromInstanceProfile = req
    "RemoveRoleFromInstanceProfile"
    "fixture/RemoveRoleFromInstanceProfile"

testGetLoginProfile :: GetLoginProfile -> TestTree
testGetLoginProfile = req
    "GetLoginProfile"
    "fixture/GetLoginProfile"

testRemoveUserFromGroup :: RemoveUserFromGroup -> TestTree
testRemoveUserFromGroup = req
    "RemoveUserFromGroup"
    "fixture/RemoveUserFromGroup"

testDetachUserPolicy :: DetachUserPolicy -> TestTree
testDetachUserPolicy = req
    "DetachUserPolicy"
    "fixture/DetachUserPolicy"

testCreateSAMLProvider :: CreateSAMLProvider -> TestTree
testCreateSAMLProvider = req
    "CreateSAMLProvider"
    "fixture/CreateSAMLProvider"

testCreatePolicyVersion :: CreatePolicyVersion -> TestTree
testCreatePolicyVersion = req
    "CreatePolicyVersion"
    "fixture/CreatePolicyVersion"

testGetGroupPolicy :: GetGroupPolicy -> TestTree
testGetGroupPolicy = req
    "GetGroupPolicy"
    "fixture/GetGroupPolicy"

testDeletePolicy :: DeletePolicy -> TestTree
testDeletePolicy = req
    "DeletePolicy"
    "fixture/DeletePolicy"

testListServerCertificates :: ListServerCertificates -> TestTree
testListServerCertificates = req
    "ListServerCertificates"
    "fixture/ListServerCertificates"

testUpdateAssumeRolePolicy :: UpdateAssumeRolePolicy -> TestTree
testUpdateAssumeRolePolicy = req
    "UpdateAssumeRolePolicy"
    "fixture/UpdateAssumeRolePolicy"

testChangePassword :: ChangePassword -> TestTree
testChangePassword = req
    "ChangePassword"
    "fixture/ChangePassword"

testListGroupsForUser :: ListGroupsForUser -> TestTree
testListGroupsForUser = req
    "ListGroupsForUser"
    "fixture/ListGroupsForUser"

testGetPolicyVersion :: GetPolicyVersion -> TestTree
testGetPolicyVersion = req
    "GetPolicyVersion"
    "fixture/GetPolicyVersion"

testCreateLoginProfile :: CreateLoginProfile -> TestTree
testCreateLoginProfile = req
    "CreateLoginProfile"
    "fixture/CreateLoginProfile"

testGetInstanceProfile :: GetInstanceProfile -> TestTree
testGetInstanceProfile = req
    "GetInstanceProfile"
    "fixture/GetInstanceProfile"

testListEntitiesForPolicy :: ListEntitiesForPolicy -> TestTree
testListEntitiesForPolicy = req
    "ListEntitiesForPolicy"
    "fixture/ListEntitiesForPolicy"

testGetSAMLProvider :: GetSAMLProvider -> TestTree
testGetSAMLProvider = req
    "GetSAMLProvider"
    "fixture/GetSAMLProvider"

testAddRoleToInstanceProfile :: AddRoleToInstanceProfile -> TestTree
testAddRoleToInstanceProfile = req
    "AddRoleToInstanceProfile"
    "fixture/AddRoleToInstanceProfile"

testAddUserToGroup :: AddUserToGroup -> TestTree
testAddUserToGroup = req
    "AddUserToGroup"
    "fixture/AddUserToGroup"

testDeleteOpenIdConnectProvider :: DeleteOpenIdConnectProvider -> TestTree
testDeleteOpenIdConnectProvider = req
    "DeleteOpenIdConnectProvider"
    "fixture/DeleteOpenIdConnectProvider"

testGetUser :: GetUser -> TestTree
testGetUser = req
    "GetUser"
    "fixture/GetUser"

testListAttachedUserPolicies :: ListAttachedUserPolicies -> TestTree
testListAttachedUserPolicies = req
    "ListAttachedUserPolicies"
    "fixture/ListAttachedUserPolicies"

testDeleteSigningCertificate :: DeleteSigningCertificate -> TestTree
testDeleteSigningCertificate = req
    "DeleteSigningCertificate"
    "fixture/DeleteSigningCertificate"

testUpdateSigningCertificate :: UpdateSigningCertificate -> TestTree
testUpdateSigningCertificate = req
    "UpdateSigningCertificate"
    "fixture/UpdateSigningCertificate"

testListSigningCertificates :: ListSigningCertificates -> TestTree
testListSigningCertificates = req
    "ListSigningCertificates"
    "fixture/ListSigningCertificates"

testRemoveClientIdFromOpenIdConnectProvider :: RemoveClientIdFromOpenIdConnectProvider -> TestTree
testRemoveClientIdFromOpenIdConnectProvider = req
    "RemoveClientIdFromOpenIdConnectProvider"
    "fixture/RemoveClientIdFromOpenIdConnectProvider"

testListAccessKeys :: ListAccessKeys -> TestTree
testListAccessKeys = req
    "ListAccessKeys"
    "fixture/ListAccessKeys"

testListVirtualMFADevices :: ListVirtualMFADevices -> TestTree
testListVirtualMFADevices = req
    "ListVirtualMFADevices"
    "fixture/ListVirtualMFADevices"

testDeleteAccessKey :: DeleteAccessKey -> TestTree
testDeleteAccessKey = req
    "DeleteAccessKey"
    "fixture/DeleteAccessKey"

testUpdateAccessKey :: UpdateAccessKey -> TestTree
testUpdateAccessKey = req
    "UpdateAccessKey"
    "fixture/UpdateAccessKey"

testGetRolePolicy :: GetRolePolicy -> TestTree
testGetRolePolicy = req
    "GetRolePolicy"
    "fixture/GetRolePolicy"

testAttachUserPolicy :: AttachUserPolicy -> TestTree
testAttachUserPolicy = req
    "AttachUserPolicy"
    "fixture/AttachUserPolicy"

testResyncMFADevice :: ResyncMFADevice -> TestTree
testResyncMFADevice = req
    "ResyncMFADevice"
    "fixture/ResyncMFADevice"

testCreateUser :: CreateUser -> TestTree
testCreateUser = req
    "CreateUser"
    "fixture/CreateUser"

testUploadSigningCertificate :: UploadSigningCertificate -> TestTree
testUploadSigningCertificate = req
    "UploadSigningCertificate"
    "fixture/UploadSigningCertificate"

testPutRolePolicy :: PutRolePolicy -> TestTree
testPutRolePolicy = req
    "PutRolePolicy"
    "fixture/PutRolePolicy"

testDeleteRolePolicy :: DeleteRolePolicy -> TestTree
testDeleteRolePolicy = req
    "DeleteRolePolicy"
    "fixture/DeleteRolePolicy"

testUpdateUser :: UpdateUser -> TestTree
testUpdateUser = req
    "UpdateUser"
    "fixture/UpdateUser"

testDeleteUser :: DeleteUser -> TestTree
testDeleteUser = req
    "DeleteUser"
    "fixture/DeleteUser"

testListRolePolicies :: ListRolePolicies -> TestTree
testListRolePolicies = req
    "ListRolePolicies"
    "fixture/ListRolePolicies"

testAddClientIdToOpenIdConnectProvider :: AddClientIdToOpenIdConnectProvider -> TestTree
testAddClientIdToOpenIdConnectProvider = req
    "AddClientIdToOpenIdConnectProvider"
    "fixture/AddClientIdToOpenIdConnectProvider"

testGetAccessKeyLastUsed :: GetAccessKeyLastUsed -> TestTree
testGetAccessKeyLastUsed = req
    "GetAccessKeyLastUsed"
    "fixture/GetAccessKeyLastUsed"

testGetAccountPasswordPolicy :: GetAccountPasswordPolicy -> TestTree
testGetAccountPasswordPolicy = req
    "GetAccountPasswordPolicy"
    "fixture/GetAccountPasswordPolicy"

testListAccountAliases :: ListAccountAliases -> TestTree
testListAccountAliases = req
    "ListAccountAliases"
    "fixture/ListAccountAliases"

testCreateAccountAlias :: CreateAccountAlias -> TestTree
testCreateAccountAlias = req
    "CreateAccountAlias"
    "fixture/CreateAccountAlias"

testUploadServerCertificate :: UploadServerCertificate -> TestTree
testUploadServerCertificate = req
    "UploadServerCertificate"
    "fixture/UploadServerCertificate"

testListMFADevices :: ListMFADevices -> TestTree
testListMFADevices = req
    "ListMFADevices"
    "fixture/ListMFADevices"

testEnableMFADevice :: EnableMFADevice -> TestTree
testEnableMFADevice = req
    "EnableMFADevice"
    "fixture/EnableMFADevice"

testListPolicyVersions :: ListPolicyVersions -> TestTree
testListPolicyVersions = req
    "ListPolicyVersions"
    "fixture/ListPolicyVersions"

testListSAMLProviders :: ListSAMLProviders -> TestTree
testListSAMLProviders = req
    "ListSAMLProviders"
    "fixture/ListSAMLProviders"

testUpdateSAMLProvider :: UpdateSAMLProvider -> TestTree
testUpdateSAMLProvider = req
    "UpdateSAMLProvider"
    "fixture/UpdateSAMLProvider"

testDeleteSAMLProvider :: DeleteSAMLProvider -> TestTree
testDeleteSAMLProvider = req
    "DeleteSAMLProvider"
    "fixture/DeleteSAMLProvider"

testCreateGroup :: CreateGroup -> TestTree
testCreateGroup = req
    "CreateGroup"
    "fixture/CreateGroup"

testSetDefaultPolicyVersion :: SetDefaultPolicyVersion -> TestTree
testSetDefaultPolicyVersion = req
    "SetDefaultPolicyVersion"
    "fixture/SetDefaultPolicyVersion"

testListInstanceProfiles :: ListInstanceProfiles -> TestTree
testListInstanceProfiles = req
    "ListInstanceProfiles"
    "fixture/ListInstanceProfiles"

testListGroups :: ListGroups -> TestTree
testListGroups = req
    "ListGroups"
    "fixture/ListGroups"

testDeleteGroup :: DeleteGroup -> TestTree
testDeleteGroup = req
    "DeleteGroup"
    "fixture/DeleteGroup"

testUpdateGroup :: UpdateGroup -> TestTree
testUpdateGroup = req
    "UpdateGroup"
    "fixture/UpdateGroup"

testGetServerCertificate :: GetServerCertificate -> TestTree
testGetServerCertificate = req
    "GetServerCertificate"
    "fixture/GetServerCertificate"

testGetPolicy :: GetPolicy -> TestTree
testGetPolicy = req
    "GetPolicy"
    "fixture/GetPolicy"

testGenerateCredentialReport :: GenerateCredentialReport -> TestTree
testGenerateCredentialReport = req
    "GenerateCredentialReport"
    "fixture/GenerateCredentialReport"

testGetGroup :: GetGroup -> TestTree
testGetGroup = req
    "GetGroup"
    "fixture/GetGroup"

testDeleteServerCertificate :: DeleteServerCertificate -> TestTree
testDeleteServerCertificate = req
    "DeleteServerCertificate"
    "fixture/DeleteServerCertificate"

testUpdateServerCertificate :: UpdateServerCertificate -> TestTree
testUpdateServerCertificate = req
    "UpdateServerCertificate"
    "fixture/UpdateServerCertificate"

testDeleteLoginProfile :: DeleteLoginProfile -> TestTree
testDeleteLoginProfile = req
    "DeleteLoginProfile"
    "fixture/DeleteLoginProfile"

testUpdateLoginProfile :: UpdateLoginProfile -> TestTree
testUpdateLoginProfile = req
    "UpdateLoginProfile"
    "fixture/UpdateLoginProfile"

testListAttachedGroupPolicies :: ListAttachedGroupPolicies -> TestTree
testListAttachedGroupPolicies = req
    "ListAttachedGroupPolicies"
    "fixture/ListAttachedGroupPolicies"

-- Responses

testAttachGroupPolicyResponse :: AttachGroupPolicyResponse -> TestTree
testAttachGroupPolicyResponse = res
    "AttachGroupPolicyResponse"
    "fixture/AttachGroupPolicyResponse"
    (Proxy :: Proxy AttachGroupPolicy)

testListInstanceProfilesForRoleResponse :: ListInstanceProfilesForRoleResponse -> TestTree
testListInstanceProfilesForRoleResponse = res
    "ListInstanceProfilesForRoleResponse"
    "fixture/ListInstanceProfilesForRoleResponse"
    (Proxy :: Proxy ListInstanceProfilesForRole)

testCreatePolicyResponse :: CreatePolicyResponse -> TestTree
testCreatePolicyResponse = res
    "CreatePolicyResponse"
    "fixture/CreatePolicyResponse"
    (Proxy :: Proxy CreatePolicy)

testListPoliciesResponse :: ListPoliciesResponse -> TestTree
testListPoliciesResponse = res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse"
    (Proxy :: Proxy ListPolicies)

testAttachRolePolicyResponse :: AttachRolePolicyResponse -> TestTree
testAttachRolePolicyResponse = res
    "AttachRolePolicyResponse"
    "fixture/AttachRolePolicyResponse"
    (Proxy :: Proxy AttachRolePolicy)

testListSSHPublicKeysResponse :: ListSSHPublicKeysResponse -> TestTree
testListSSHPublicKeysResponse = res
    "ListSSHPublicKeysResponse"
    "fixture/ListSSHPublicKeysResponse"
    (Proxy :: Proxy ListSSHPublicKeys)

testDeleteSSHPublicKeyResponse :: DeleteSSHPublicKeyResponse -> TestTree
testDeleteSSHPublicKeyResponse = res
    "DeleteSSHPublicKeyResponse"
    "fixture/DeleteSSHPublicKeyResponse"
    (Proxy :: Proxy DeleteSSHPublicKey)

testUpdateSSHPublicKeyResponse :: UpdateSSHPublicKeyResponse -> TestTree
testUpdateSSHPublicKeyResponse = res
    "UpdateSSHPublicKeyResponse"
    "fixture/UpdateSSHPublicKeyResponse"
    (Proxy :: Proxy UpdateSSHPublicKey)

testListOpenIdConnectProvidersResponse :: ListOpenIdConnectProvidersResponse -> TestTree
testListOpenIdConnectProvidersResponse = res
    "ListOpenIdConnectProvidersResponse"
    "fixture/ListOpenIdConnectProvidersResponse"
    (Proxy :: Proxy ListOpenIdConnectProviders)

testDeleteAccountPasswordPolicyResponse :: DeleteAccountPasswordPolicyResponse -> TestTree
testDeleteAccountPasswordPolicyResponse = res
    "DeleteAccountPasswordPolicyResponse"
    "fixture/DeleteAccountPasswordPolicyResponse"
    (Proxy :: Proxy DeleteAccountPasswordPolicy)

testUpdateAccountPasswordPolicyResponse :: UpdateAccountPasswordPolicyResponse -> TestTree
testUpdateAccountPasswordPolicyResponse = res
    "UpdateAccountPasswordPolicyResponse"
    "fixture/UpdateAccountPasswordPolicyResponse"
    (Proxy :: Proxy UpdateAccountPasswordPolicy)

testCreateAccessKeyResponse :: CreateAccessKeyResponse -> TestTree
testCreateAccessKeyResponse = res
    "CreateAccessKeyResponse"
    "fixture/CreateAccessKeyResponse"
    (Proxy :: Proxy CreateAccessKey)

testGetUserPolicyResponse :: GetUserPolicyResponse -> TestTree
testGetUserPolicyResponse = res
    "GetUserPolicyResponse"
    "fixture/GetUserPolicyResponse"
    (Proxy :: Proxy GetUserPolicy)

testCreateVirtualMFADeviceResponse :: CreateVirtualMFADeviceResponse -> TestTree
testCreateVirtualMFADeviceResponse = res
    "CreateVirtualMFADeviceResponse"
    "fixture/CreateVirtualMFADeviceResponse"
    (Proxy :: Proxy CreateVirtualMFADevice)

testCreateOpenIdConnectProviderResponse :: CreateOpenIdConnectProviderResponse -> TestTree
testCreateOpenIdConnectProviderResponse = res
    "CreateOpenIdConnectProviderResponse"
    "fixture/CreateOpenIdConnectProviderResponse"
    (Proxy :: Proxy CreateOpenIdConnectProvider)

testListAttachedRolePoliciesResponse :: ListAttachedRolePoliciesResponse -> TestTree
testListAttachedRolePoliciesResponse = res
    "ListAttachedRolePoliciesResponse"
    "fixture/ListAttachedRolePoliciesResponse"
    (Proxy :: Proxy ListAttachedRolePolicies)

testDeleteVirtualMFADeviceResponse :: DeleteVirtualMFADeviceResponse -> TestTree
testDeleteVirtualMFADeviceResponse = res
    "DeleteVirtualMFADeviceResponse"
    "fixture/DeleteVirtualMFADeviceResponse"
    (Proxy :: Proxy DeleteVirtualMFADevice)

testGetRoleResponse :: GetRoleResponse -> TestTree
testGetRoleResponse = res
    "GetRoleResponse"
    "fixture/GetRoleResponse"
    (Proxy :: Proxy GetRole)

testDeactivateMFADeviceResponse :: DeactivateMFADeviceResponse -> TestTree
testDeactivateMFADeviceResponse = res
    "DeactivateMFADeviceResponse"
    "fixture/DeactivateMFADeviceResponse"
    (Proxy :: Proxy DeactivateMFADevice)

testListRolesResponse :: ListRolesResponse -> TestTree
testListRolesResponse = res
    "ListRolesResponse"
    "fixture/ListRolesResponse"
    (Proxy :: Proxy ListRoles)

testDeleteRoleResponse :: DeleteRoleResponse -> TestTree
testDeleteRoleResponse = res
    "DeleteRoleResponse"
    "fixture/DeleteRoleResponse"
    (Proxy :: Proxy DeleteRole)

testListUserPoliciesResponse :: ListUserPoliciesResponse -> TestTree
testListUserPoliciesResponse = res
    "ListUserPoliciesResponse"
    "fixture/ListUserPoliciesResponse"
    (Proxy :: Proxy ListUserPolicies)

testUploadSSHPublicKeyResponse :: UploadSSHPublicKeyResponse -> TestTree
testUploadSSHPublicKeyResponse = res
    "UploadSSHPublicKeyResponse"
    "fixture/UploadSSHPublicKeyResponse"
    (Proxy :: Proxy UploadSSHPublicKey)

testListUsersResponse :: ListUsersResponse -> TestTree
testListUsersResponse = res
    "ListUsersResponse"
    "fixture/ListUsersResponse"
    (Proxy :: Proxy ListUsers)

testUpdateOpenIdConnectProviderThumbprintResponse :: UpdateOpenIdConnectProviderThumbprintResponse -> TestTree
testUpdateOpenIdConnectProviderThumbprintResponse = res
    "UpdateOpenIdConnectProviderThumbprintResponse"
    "fixture/UpdateOpenIdConnectProviderThumbprintResponse"
    (Proxy :: Proxy UpdateOpenIdConnectProviderThumbprint)

testGetSSHPublicKeyResponse :: GetSSHPublicKeyResponse -> TestTree
testGetSSHPublicKeyResponse = res
    "GetSSHPublicKeyResponse"
    "fixture/GetSSHPublicKeyResponse"
    (Proxy :: Proxy GetSSHPublicKey)

testPutUserPolicyResponse :: PutUserPolicyResponse -> TestTree
testPutUserPolicyResponse = res
    "PutUserPolicyResponse"
    "fixture/PutUserPolicyResponse"
    (Proxy :: Proxy PutUserPolicy)

testCreateRoleResponse :: CreateRoleResponse -> TestTree
testCreateRoleResponse = res
    "CreateRoleResponse"
    "fixture/CreateRoleResponse"
    (Proxy :: Proxy CreateRole)

testDeleteUserPolicyResponse :: DeleteUserPolicyResponse -> TestTree
testDeleteUserPolicyResponse = res
    "DeleteUserPolicyResponse"
    "fixture/DeleteUserPolicyResponse"
    (Proxy :: Proxy DeleteUserPolicy)

testGetOpenIdConnectProviderResponse :: GetOpenIdConnectProviderResponse -> TestTree
testGetOpenIdConnectProviderResponse = res
    "GetOpenIdConnectProviderResponse"
    "fixture/GetOpenIdConnectProviderResponse"
    (Proxy :: Proxy GetOpenIdConnectProvider)

testDetachGroupPolicyResponse :: DetachGroupPolicyResponse -> TestTree
testDetachGroupPolicyResponse = res
    "DetachGroupPolicyResponse"
    "fixture/DetachGroupPolicyResponse"
    (Proxy :: Proxy DetachGroupPolicy)

testGetCredentialReportResponse :: GetCredentialReportResponse -> TestTree
testGetCredentialReportResponse = res
    "GetCredentialReportResponse"
    "fixture/GetCredentialReportResponse"
    (Proxy :: Proxy GetCredentialReport)

testDeletePolicyVersionResponse :: DeletePolicyVersionResponse -> TestTree
testDeletePolicyVersionResponse = res
    "DeletePolicyVersionResponse"
    "fixture/DeletePolicyVersionResponse"
    (Proxy :: Proxy DeletePolicyVersion)

testDetachRolePolicyResponse :: DetachRolePolicyResponse -> TestTree
testDetachRolePolicyResponse = res
    "DetachRolePolicyResponse"
    "fixture/DetachRolePolicyResponse"
    (Proxy :: Proxy DetachRolePolicy)

testDeleteInstanceProfileResponse :: DeleteInstanceProfileResponse -> TestTree
testDeleteInstanceProfileResponse = res
    "DeleteInstanceProfileResponse"
    "fixture/DeleteInstanceProfileResponse"
    (Proxy :: Proxy DeleteInstanceProfile)

testListGroupPoliciesResponse :: ListGroupPoliciesResponse -> TestTree
testListGroupPoliciesResponse = res
    "ListGroupPoliciesResponse"
    "fixture/ListGroupPoliciesResponse"
    (Proxy :: Proxy ListGroupPolicies)

testGetAccountSummaryResponse :: GetAccountSummaryResponse -> TestTree
testGetAccountSummaryResponse = res
    "GetAccountSummaryResponse"
    "fixture/GetAccountSummaryResponse"
    (Proxy :: Proxy GetAccountSummary)

testCreateInstanceProfileResponse :: CreateInstanceProfileResponse -> TestTree
testCreateInstanceProfileResponse = res
    "CreateInstanceProfileResponse"
    "fixture/CreateInstanceProfileResponse"
    (Proxy :: Proxy CreateInstanceProfile)

testPutGroupPolicyResponse :: PutGroupPolicyResponse -> TestTree
testPutGroupPolicyResponse = res
    "PutGroupPolicyResponse"
    "fixture/PutGroupPolicyResponse"
    (Proxy :: Proxy PutGroupPolicy)

testDeleteGroupPolicyResponse :: DeleteGroupPolicyResponse -> TestTree
testDeleteGroupPolicyResponse = res
    "DeleteGroupPolicyResponse"
    "fixture/DeleteGroupPolicyResponse"
    (Proxy :: Proxy DeleteGroupPolicy)

testGetAccountAuthorizationDetailsResponse :: GetAccountAuthorizationDetailsResponse -> TestTree
testGetAccountAuthorizationDetailsResponse = res
    "GetAccountAuthorizationDetailsResponse"
    "fixture/GetAccountAuthorizationDetailsResponse"
    (Proxy :: Proxy GetAccountAuthorizationDetails)

testDeleteAccountAliasResponse :: DeleteAccountAliasResponse -> TestTree
testDeleteAccountAliasResponse = res
    "DeleteAccountAliasResponse"
    "fixture/DeleteAccountAliasResponse"
    (Proxy :: Proxy DeleteAccountAlias)

testRemoveRoleFromInstanceProfileResponse :: RemoveRoleFromInstanceProfileResponse -> TestTree
testRemoveRoleFromInstanceProfileResponse = res
    "RemoveRoleFromInstanceProfileResponse"
    "fixture/RemoveRoleFromInstanceProfileResponse"
    (Proxy :: Proxy RemoveRoleFromInstanceProfile)

testGetLoginProfileResponse :: GetLoginProfileResponse -> TestTree
testGetLoginProfileResponse = res
    "GetLoginProfileResponse"
    "fixture/GetLoginProfileResponse"
    (Proxy :: Proxy GetLoginProfile)

testRemoveUserFromGroupResponse :: RemoveUserFromGroupResponse -> TestTree
testRemoveUserFromGroupResponse = res
    "RemoveUserFromGroupResponse"
    "fixture/RemoveUserFromGroupResponse"
    (Proxy :: Proxy RemoveUserFromGroup)

testDetachUserPolicyResponse :: DetachUserPolicyResponse -> TestTree
testDetachUserPolicyResponse = res
    "DetachUserPolicyResponse"
    "fixture/DetachUserPolicyResponse"
    (Proxy :: Proxy DetachUserPolicy)

testCreateSAMLProviderResponse :: CreateSAMLProviderResponse -> TestTree
testCreateSAMLProviderResponse = res
    "CreateSAMLProviderResponse"
    "fixture/CreateSAMLProviderResponse"
    (Proxy :: Proxy CreateSAMLProvider)

testCreatePolicyVersionResponse :: CreatePolicyVersionResponse -> TestTree
testCreatePolicyVersionResponse = res
    "CreatePolicyVersionResponse"
    "fixture/CreatePolicyVersionResponse"
    (Proxy :: Proxy CreatePolicyVersion)

testGetGroupPolicyResponse :: GetGroupPolicyResponse -> TestTree
testGetGroupPolicyResponse = res
    "GetGroupPolicyResponse"
    "fixture/GetGroupPolicyResponse"
    (Proxy :: Proxy GetGroupPolicy)

testDeletePolicyResponse :: DeletePolicyResponse -> TestTree
testDeletePolicyResponse = res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse"
    (Proxy :: Proxy DeletePolicy)

testListServerCertificatesResponse :: ListServerCertificatesResponse -> TestTree
testListServerCertificatesResponse = res
    "ListServerCertificatesResponse"
    "fixture/ListServerCertificatesResponse"
    (Proxy :: Proxy ListServerCertificates)

testUpdateAssumeRolePolicyResponse :: UpdateAssumeRolePolicyResponse -> TestTree
testUpdateAssumeRolePolicyResponse = res
    "UpdateAssumeRolePolicyResponse"
    "fixture/UpdateAssumeRolePolicyResponse"
    (Proxy :: Proxy UpdateAssumeRolePolicy)

testChangePasswordResponse :: ChangePasswordResponse -> TestTree
testChangePasswordResponse = res
    "ChangePasswordResponse"
    "fixture/ChangePasswordResponse"
    (Proxy :: Proxy ChangePassword)

testListGroupsForUserResponse :: ListGroupsForUserResponse -> TestTree
testListGroupsForUserResponse = res
    "ListGroupsForUserResponse"
    "fixture/ListGroupsForUserResponse"
    (Proxy :: Proxy ListGroupsForUser)

testGetPolicyVersionResponse :: GetPolicyVersionResponse -> TestTree
testGetPolicyVersionResponse = res
    "GetPolicyVersionResponse"
    "fixture/GetPolicyVersionResponse"
    (Proxy :: Proxy GetPolicyVersion)

testCreateLoginProfileResponse :: CreateLoginProfileResponse -> TestTree
testCreateLoginProfileResponse = res
    "CreateLoginProfileResponse"
    "fixture/CreateLoginProfileResponse"
    (Proxy :: Proxy CreateLoginProfile)

testGetInstanceProfileResponse :: GetInstanceProfileResponse -> TestTree
testGetInstanceProfileResponse = res
    "GetInstanceProfileResponse"
    "fixture/GetInstanceProfileResponse"
    (Proxy :: Proxy GetInstanceProfile)

testListEntitiesForPolicyResponse :: ListEntitiesForPolicyResponse -> TestTree
testListEntitiesForPolicyResponse = res
    "ListEntitiesForPolicyResponse"
    "fixture/ListEntitiesForPolicyResponse"
    (Proxy :: Proxy ListEntitiesForPolicy)

testGetSAMLProviderResponse :: GetSAMLProviderResponse -> TestTree
testGetSAMLProviderResponse = res
    "GetSAMLProviderResponse"
    "fixture/GetSAMLProviderResponse"
    (Proxy :: Proxy GetSAMLProvider)

testAddRoleToInstanceProfileResponse :: AddRoleToInstanceProfileResponse -> TestTree
testAddRoleToInstanceProfileResponse = res
    "AddRoleToInstanceProfileResponse"
    "fixture/AddRoleToInstanceProfileResponse"
    (Proxy :: Proxy AddRoleToInstanceProfile)

testAddUserToGroupResponse :: AddUserToGroupResponse -> TestTree
testAddUserToGroupResponse = res
    "AddUserToGroupResponse"
    "fixture/AddUserToGroupResponse"
    (Proxy :: Proxy AddUserToGroup)

testDeleteOpenIdConnectProviderResponse :: DeleteOpenIdConnectProviderResponse -> TestTree
testDeleteOpenIdConnectProviderResponse = res
    "DeleteOpenIdConnectProviderResponse"
    "fixture/DeleteOpenIdConnectProviderResponse"
    (Proxy :: Proxy DeleteOpenIdConnectProvider)

testGetUserResponse :: GetUserResponse -> TestTree
testGetUserResponse = res
    "GetUserResponse"
    "fixture/GetUserResponse"
    (Proxy :: Proxy GetUser)

testListAttachedUserPoliciesResponse :: ListAttachedUserPoliciesResponse -> TestTree
testListAttachedUserPoliciesResponse = res
    "ListAttachedUserPoliciesResponse"
    "fixture/ListAttachedUserPoliciesResponse"
    (Proxy :: Proxy ListAttachedUserPolicies)

testDeleteSigningCertificateResponse :: DeleteSigningCertificateResponse -> TestTree
testDeleteSigningCertificateResponse = res
    "DeleteSigningCertificateResponse"
    "fixture/DeleteSigningCertificateResponse"
    (Proxy :: Proxy DeleteSigningCertificate)

testUpdateSigningCertificateResponse :: UpdateSigningCertificateResponse -> TestTree
testUpdateSigningCertificateResponse = res
    "UpdateSigningCertificateResponse"
    "fixture/UpdateSigningCertificateResponse"
    (Proxy :: Proxy UpdateSigningCertificate)

testListSigningCertificatesResponse :: ListSigningCertificatesResponse -> TestTree
testListSigningCertificatesResponse = res
    "ListSigningCertificatesResponse"
    "fixture/ListSigningCertificatesResponse"
    (Proxy :: Proxy ListSigningCertificates)

testRemoveClientIdFromOpenIdConnectProviderResponse :: RemoveClientIdFromOpenIdConnectProviderResponse -> TestTree
testRemoveClientIdFromOpenIdConnectProviderResponse = res
    "RemoveClientIdFromOpenIdConnectProviderResponse"
    "fixture/RemoveClientIdFromOpenIdConnectProviderResponse"
    (Proxy :: Proxy RemoveClientIdFromOpenIdConnectProvider)

testListAccessKeysResponse :: ListAccessKeysResponse -> TestTree
testListAccessKeysResponse = res
    "ListAccessKeysResponse"
    "fixture/ListAccessKeysResponse"
    (Proxy :: Proxy ListAccessKeys)

testListVirtualMFADevicesResponse :: ListVirtualMFADevicesResponse -> TestTree
testListVirtualMFADevicesResponse = res
    "ListVirtualMFADevicesResponse"
    "fixture/ListVirtualMFADevicesResponse"
    (Proxy :: Proxy ListVirtualMFADevices)

testDeleteAccessKeyResponse :: DeleteAccessKeyResponse -> TestTree
testDeleteAccessKeyResponse = res
    "DeleteAccessKeyResponse"
    "fixture/DeleteAccessKeyResponse"
    (Proxy :: Proxy DeleteAccessKey)

testUpdateAccessKeyResponse :: UpdateAccessKeyResponse -> TestTree
testUpdateAccessKeyResponse = res
    "UpdateAccessKeyResponse"
    "fixture/UpdateAccessKeyResponse"
    (Proxy :: Proxy UpdateAccessKey)

testGetRolePolicyResponse :: GetRolePolicyResponse -> TestTree
testGetRolePolicyResponse = res
    "GetRolePolicyResponse"
    "fixture/GetRolePolicyResponse"
    (Proxy :: Proxy GetRolePolicy)

testAttachUserPolicyResponse :: AttachUserPolicyResponse -> TestTree
testAttachUserPolicyResponse = res
    "AttachUserPolicyResponse"
    "fixture/AttachUserPolicyResponse"
    (Proxy :: Proxy AttachUserPolicy)

testResyncMFADeviceResponse :: ResyncMFADeviceResponse -> TestTree
testResyncMFADeviceResponse = res
    "ResyncMFADeviceResponse"
    "fixture/ResyncMFADeviceResponse"
    (Proxy :: Proxy ResyncMFADevice)

testCreateUserResponse :: CreateUserResponse -> TestTree
testCreateUserResponse = res
    "CreateUserResponse"
    "fixture/CreateUserResponse"
    (Proxy :: Proxy CreateUser)

testUploadSigningCertificateResponse :: UploadSigningCertificateResponse -> TestTree
testUploadSigningCertificateResponse = res
    "UploadSigningCertificateResponse"
    "fixture/UploadSigningCertificateResponse"
    (Proxy :: Proxy UploadSigningCertificate)

testPutRolePolicyResponse :: PutRolePolicyResponse -> TestTree
testPutRolePolicyResponse = res
    "PutRolePolicyResponse"
    "fixture/PutRolePolicyResponse"
    (Proxy :: Proxy PutRolePolicy)

testDeleteRolePolicyResponse :: DeleteRolePolicyResponse -> TestTree
testDeleteRolePolicyResponse = res
    "DeleteRolePolicyResponse"
    "fixture/DeleteRolePolicyResponse"
    (Proxy :: Proxy DeleteRolePolicy)

testUpdateUserResponse :: UpdateUserResponse -> TestTree
testUpdateUserResponse = res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse"
    (Proxy :: Proxy UpdateUser)

testDeleteUserResponse :: DeleteUserResponse -> TestTree
testDeleteUserResponse = res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse"
    (Proxy :: Proxy DeleteUser)

testListRolePoliciesResponse :: ListRolePoliciesResponse -> TestTree
testListRolePoliciesResponse = res
    "ListRolePoliciesResponse"
    "fixture/ListRolePoliciesResponse"
    (Proxy :: Proxy ListRolePolicies)

testAddClientIdToOpenIdConnectProviderResponse :: AddClientIdToOpenIdConnectProviderResponse -> TestTree
testAddClientIdToOpenIdConnectProviderResponse = res
    "AddClientIdToOpenIdConnectProviderResponse"
    "fixture/AddClientIdToOpenIdConnectProviderResponse"
    (Proxy :: Proxy AddClientIdToOpenIdConnectProvider)

testGetAccessKeyLastUsedResponse :: GetAccessKeyLastUsedResponse -> TestTree
testGetAccessKeyLastUsedResponse = res
    "GetAccessKeyLastUsedResponse"
    "fixture/GetAccessKeyLastUsedResponse"
    (Proxy :: Proxy GetAccessKeyLastUsed)

testGetAccountPasswordPolicyResponse :: GetAccountPasswordPolicyResponse -> TestTree
testGetAccountPasswordPolicyResponse = res
    "GetAccountPasswordPolicyResponse"
    "fixture/GetAccountPasswordPolicyResponse"
    (Proxy :: Proxy GetAccountPasswordPolicy)

testListAccountAliasesResponse :: ListAccountAliasesResponse -> TestTree
testListAccountAliasesResponse = res
    "ListAccountAliasesResponse"
    "fixture/ListAccountAliasesResponse"
    (Proxy :: Proxy ListAccountAliases)

testCreateAccountAliasResponse :: CreateAccountAliasResponse -> TestTree
testCreateAccountAliasResponse = res
    "CreateAccountAliasResponse"
    "fixture/CreateAccountAliasResponse"
    (Proxy :: Proxy CreateAccountAlias)

testUploadServerCertificateResponse :: UploadServerCertificateResponse -> TestTree
testUploadServerCertificateResponse = res
    "UploadServerCertificateResponse"
    "fixture/UploadServerCertificateResponse"
    (Proxy :: Proxy UploadServerCertificate)

testListMFADevicesResponse :: ListMFADevicesResponse -> TestTree
testListMFADevicesResponse = res
    "ListMFADevicesResponse"
    "fixture/ListMFADevicesResponse"
    (Proxy :: Proxy ListMFADevices)

testEnableMFADeviceResponse :: EnableMFADeviceResponse -> TestTree
testEnableMFADeviceResponse = res
    "EnableMFADeviceResponse"
    "fixture/EnableMFADeviceResponse"
    (Proxy :: Proxy EnableMFADevice)

testListPolicyVersionsResponse :: ListPolicyVersionsResponse -> TestTree
testListPolicyVersionsResponse = res
    "ListPolicyVersionsResponse"
    "fixture/ListPolicyVersionsResponse"
    (Proxy :: Proxy ListPolicyVersions)

testListSAMLProvidersResponse :: ListSAMLProvidersResponse -> TestTree
testListSAMLProvidersResponse = res
    "ListSAMLProvidersResponse"
    "fixture/ListSAMLProvidersResponse"
    (Proxy :: Proxy ListSAMLProviders)

testUpdateSAMLProviderResponse :: UpdateSAMLProviderResponse -> TestTree
testUpdateSAMLProviderResponse = res
    "UpdateSAMLProviderResponse"
    "fixture/UpdateSAMLProviderResponse"
    (Proxy :: Proxy UpdateSAMLProvider)

testDeleteSAMLProviderResponse :: DeleteSAMLProviderResponse -> TestTree
testDeleteSAMLProviderResponse = res
    "DeleteSAMLProviderResponse"
    "fixture/DeleteSAMLProviderResponse"
    (Proxy :: Proxy DeleteSAMLProvider)

testCreateGroupResponse :: CreateGroupResponse -> TestTree
testCreateGroupResponse = res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse"
    (Proxy :: Proxy CreateGroup)

testSetDefaultPolicyVersionResponse :: SetDefaultPolicyVersionResponse -> TestTree
testSetDefaultPolicyVersionResponse = res
    "SetDefaultPolicyVersionResponse"
    "fixture/SetDefaultPolicyVersionResponse"
    (Proxy :: Proxy SetDefaultPolicyVersion)

testListInstanceProfilesResponse :: ListInstanceProfilesResponse -> TestTree
testListInstanceProfilesResponse = res
    "ListInstanceProfilesResponse"
    "fixture/ListInstanceProfilesResponse"
    (Proxy :: Proxy ListInstanceProfiles)

testListGroupsResponse :: ListGroupsResponse -> TestTree
testListGroupsResponse = res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse"
    (Proxy :: Proxy ListGroups)

testDeleteGroupResponse :: DeleteGroupResponse -> TestTree
testDeleteGroupResponse = res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse"
    (Proxy :: Proxy DeleteGroup)

testUpdateGroupResponse :: UpdateGroupResponse -> TestTree
testUpdateGroupResponse = res
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse"
    (Proxy :: Proxy UpdateGroup)

testGetServerCertificateResponse :: GetServerCertificateResponse -> TestTree
testGetServerCertificateResponse = res
    "GetServerCertificateResponse"
    "fixture/GetServerCertificateResponse"
    (Proxy :: Proxy GetServerCertificate)

testGetPolicyResponse :: GetPolicyResponse -> TestTree
testGetPolicyResponse = res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse"
    (Proxy :: Proxy GetPolicy)

testGenerateCredentialReportResponse :: GenerateCredentialReportResponse -> TestTree
testGenerateCredentialReportResponse = res
    "GenerateCredentialReportResponse"
    "fixture/GenerateCredentialReportResponse"
    (Proxy :: Proxy GenerateCredentialReport)

testGetGroupResponse :: GetGroupResponse -> TestTree
testGetGroupResponse = res
    "GetGroupResponse"
    "fixture/GetGroupResponse"
    (Proxy :: Proxy GetGroup)

testDeleteServerCertificateResponse :: DeleteServerCertificateResponse -> TestTree
testDeleteServerCertificateResponse = res
    "DeleteServerCertificateResponse"
    "fixture/DeleteServerCertificateResponse"
    (Proxy :: Proxy DeleteServerCertificate)

testUpdateServerCertificateResponse :: UpdateServerCertificateResponse -> TestTree
testUpdateServerCertificateResponse = res
    "UpdateServerCertificateResponse"
    "fixture/UpdateServerCertificateResponse"
    (Proxy :: Proxy UpdateServerCertificate)

testDeleteLoginProfileResponse :: DeleteLoginProfileResponse -> TestTree
testDeleteLoginProfileResponse = res
    "DeleteLoginProfileResponse"
    "fixture/DeleteLoginProfileResponse"
    (Proxy :: Proxy DeleteLoginProfile)

testUpdateLoginProfileResponse :: UpdateLoginProfileResponse -> TestTree
testUpdateLoginProfileResponse = res
    "UpdateLoginProfileResponse"
    "fixture/UpdateLoginProfileResponse"
    (Proxy :: Proxy UpdateLoginProfile)

testListAttachedGroupPoliciesResponse :: ListAttachedGroupPoliciesResponse -> TestTree
testListAttachedGroupPoliciesResponse = res
    "ListAttachedGroupPoliciesResponse"
    "fixture/ListAttachedGroupPoliciesResponse"
    (Proxy :: Proxy ListAttachedGroupPolicies)
