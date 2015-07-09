{-# OPTIONS_GHC -fno-warn-orphans #-}

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
--         , testListOpenIDConnectProviders $
--             listOpenIDConnectProviders
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
--         , testCreateOpenIDConnectProvider $
--             createOpenIDConnectProvider
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
--         , testListUsers $
--             listUsers
--
--         , testUpdateOpenIDConnectProviderThumbprint $
--             updateOpenIDConnectProviderThumbprint
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
--         , testGetOpenIDConnectProvider $
--             getOpenIDConnectProvider
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
--         , testDeleteOpenIDConnectProvider $
--             deleteOpenIDConnectProvider
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
--         , testRemoveClientIDFromOpenIDConnectProvider $
--             removeClientIDFromOpenIDConnectProvider
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
--         , testAddClientIDToOpenIDConnectProvider $
--             addClientIDToOpenIDConnectProvider
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
--         , testListOpenIDConnectProvidersResponse $
--             listOpenIDConnectProvidersResponse
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
--         , testCreateOpenIDConnectProviderResponse $
--             createOpenIDConnectProviderResponse
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
--         , testListUsersResponse $
--             listUsersResponse
--
--         , testUpdateOpenIDConnectProviderThumbprintResponse $
--             updateOpenIDConnectProviderThumbprintResponse
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
--         , testGetOpenIDConnectProviderResponse $
--             getOpenIDConnectProviderResponse
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
--         , testDeleteOpenIDConnectProviderResponse $
--             deleteOpenIDConnectProviderResponse
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
--         , testRemoveClientIDFromOpenIDConnectProviderResponse $
--             removeClientIDFromOpenIDConnectProviderResponse
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
--         , testAddClientIDToOpenIDConnectProviderResponse $
--             addClientIDToOpenIDConnectProviderResponse
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
testAttachGroupPolicy = undefined

testListInstanceProfilesForRole :: ListInstanceProfilesForRole -> TestTree
testListInstanceProfilesForRole = undefined

testCreatePolicy :: CreatePolicy -> TestTree
testCreatePolicy = undefined

testListPolicies :: ListPolicies -> TestTree
testListPolicies = undefined

testAttachRolePolicy :: AttachRolePolicy -> TestTree
testAttachRolePolicy = undefined

testListOpenIDConnectProviders :: ListOpenIDConnectProviders -> TestTree
testListOpenIDConnectProviders = undefined

testDeleteAccountPasswordPolicy :: DeleteAccountPasswordPolicy -> TestTree
testDeleteAccountPasswordPolicy = undefined

testUpdateAccountPasswordPolicy :: UpdateAccountPasswordPolicy -> TestTree
testUpdateAccountPasswordPolicy = undefined

testCreateAccessKey :: CreateAccessKey -> TestTree
testCreateAccessKey = undefined

testGetUserPolicy :: GetUserPolicy -> TestTree
testGetUserPolicy = undefined

testCreateVirtualMFADevice :: CreateVirtualMFADevice -> TestTree
testCreateVirtualMFADevice = undefined

testCreateOpenIDConnectProvider :: CreateOpenIDConnectProvider -> TestTree
testCreateOpenIDConnectProvider = undefined

testListAttachedRolePolicies :: ListAttachedRolePolicies -> TestTree
testListAttachedRolePolicies = undefined

testDeleteVirtualMFADevice :: DeleteVirtualMFADevice -> TestTree
testDeleteVirtualMFADevice = undefined

testGetRole :: GetRole -> TestTree
testGetRole = undefined

testDeactivateMFADevice :: DeactivateMFADevice -> TestTree
testDeactivateMFADevice = undefined

testListRoles :: ListRoles -> TestTree
testListRoles = undefined

testDeleteRole :: DeleteRole -> TestTree
testDeleteRole = undefined

testListUserPolicies :: ListUserPolicies -> TestTree
testListUserPolicies = undefined

testListUsers :: ListUsers -> TestTree
testListUsers = undefined

testUpdateOpenIDConnectProviderThumbprint :: UpdateOpenIDConnectProviderThumbprint -> TestTree
testUpdateOpenIDConnectProviderThumbprint = undefined

testPutUserPolicy :: PutUserPolicy -> TestTree
testPutUserPolicy = undefined

testCreateRole :: CreateRole -> TestTree
testCreateRole = undefined

testDeleteUserPolicy :: DeleteUserPolicy -> TestTree
testDeleteUserPolicy = undefined

testGetOpenIDConnectProvider :: GetOpenIDConnectProvider -> TestTree
testGetOpenIDConnectProvider = undefined

testDetachGroupPolicy :: DetachGroupPolicy -> TestTree
testDetachGroupPolicy = undefined

testGetCredentialReport :: GetCredentialReport -> TestTree
testGetCredentialReport = undefined

testDeletePolicyVersion :: DeletePolicyVersion -> TestTree
testDeletePolicyVersion = undefined

testDetachRolePolicy :: DetachRolePolicy -> TestTree
testDetachRolePolicy = undefined

testDeleteInstanceProfile :: DeleteInstanceProfile -> TestTree
testDeleteInstanceProfile = undefined

testListGroupPolicies :: ListGroupPolicies -> TestTree
testListGroupPolicies = undefined

testGetAccountSummary :: GetAccountSummary -> TestTree
testGetAccountSummary = undefined

testCreateInstanceProfile :: CreateInstanceProfile -> TestTree
testCreateInstanceProfile = undefined

testPutGroupPolicy :: PutGroupPolicy -> TestTree
testPutGroupPolicy = undefined

testDeleteGroupPolicy :: DeleteGroupPolicy -> TestTree
testDeleteGroupPolicy = undefined

testGetAccountAuthorizationDetails :: GetAccountAuthorizationDetails -> TestTree
testGetAccountAuthorizationDetails = undefined

testDeleteAccountAlias :: DeleteAccountAlias -> TestTree
testDeleteAccountAlias = undefined

testRemoveRoleFromInstanceProfile :: RemoveRoleFromInstanceProfile -> TestTree
testRemoveRoleFromInstanceProfile = undefined

testGetLoginProfile :: GetLoginProfile -> TestTree
testGetLoginProfile = undefined

testRemoveUserFromGroup :: RemoveUserFromGroup -> TestTree
testRemoveUserFromGroup = undefined

testDetachUserPolicy :: DetachUserPolicy -> TestTree
testDetachUserPolicy = undefined

testCreateSAMLProvider :: CreateSAMLProvider -> TestTree
testCreateSAMLProvider = undefined

testCreatePolicyVersion :: CreatePolicyVersion -> TestTree
testCreatePolicyVersion = undefined

testGetGroupPolicy :: GetGroupPolicy -> TestTree
testGetGroupPolicy = undefined

testDeletePolicy :: DeletePolicy -> TestTree
testDeletePolicy = undefined

testListServerCertificates :: ListServerCertificates -> TestTree
testListServerCertificates = undefined

testUpdateAssumeRolePolicy :: UpdateAssumeRolePolicy -> TestTree
testUpdateAssumeRolePolicy = undefined

testChangePassword :: ChangePassword -> TestTree
testChangePassword = undefined

testListGroupsForUser :: ListGroupsForUser -> TestTree
testListGroupsForUser = undefined

testGetPolicyVersion :: GetPolicyVersion -> TestTree
testGetPolicyVersion = undefined

testCreateLoginProfile :: CreateLoginProfile -> TestTree
testCreateLoginProfile = undefined

testGetInstanceProfile :: GetInstanceProfile -> TestTree
testGetInstanceProfile = undefined

testListEntitiesForPolicy :: ListEntitiesForPolicy -> TestTree
testListEntitiesForPolicy = undefined

testGetSAMLProvider :: GetSAMLProvider -> TestTree
testGetSAMLProvider = undefined

testAddRoleToInstanceProfile :: AddRoleToInstanceProfile -> TestTree
testAddRoleToInstanceProfile = undefined

testAddUserToGroup :: AddUserToGroup -> TestTree
testAddUserToGroup = undefined

testDeleteOpenIDConnectProvider :: DeleteOpenIDConnectProvider -> TestTree
testDeleteOpenIDConnectProvider = undefined

testGetUser :: GetUser -> TestTree
testGetUser = undefined

testListAttachedUserPolicies :: ListAttachedUserPolicies -> TestTree
testListAttachedUserPolicies = undefined

testDeleteSigningCertificate :: DeleteSigningCertificate -> TestTree
testDeleteSigningCertificate = undefined

testUpdateSigningCertificate :: UpdateSigningCertificate -> TestTree
testUpdateSigningCertificate = undefined

testListSigningCertificates :: ListSigningCertificates -> TestTree
testListSigningCertificates = undefined

testRemoveClientIDFromOpenIDConnectProvider :: RemoveClientIDFromOpenIDConnectProvider -> TestTree
testRemoveClientIDFromOpenIDConnectProvider = undefined

testListAccessKeys :: ListAccessKeys -> TestTree
testListAccessKeys = undefined

testListVirtualMFADevices :: ListVirtualMFADevices -> TestTree
testListVirtualMFADevices = undefined

testDeleteAccessKey :: DeleteAccessKey -> TestTree
testDeleteAccessKey = undefined

testUpdateAccessKey :: UpdateAccessKey -> TestTree
testUpdateAccessKey = undefined

testGetRolePolicy :: GetRolePolicy -> TestTree
testGetRolePolicy = undefined

testAttachUserPolicy :: AttachUserPolicy -> TestTree
testAttachUserPolicy = undefined

testResyncMFADevice :: ResyncMFADevice -> TestTree
testResyncMFADevice = undefined

testCreateUser :: CreateUser -> TestTree
testCreateUser = undefined

testUploadSigningCertificate :: UploadSigningCertificate -> TestTree
testUploadSigningCertificate = undefined

testPutRolePolicy :: PutRolePolicy -> TestTree
testPutRolePolicy = undefined

testDeleteRolePolicy :: DeleteRolePolicy -> TestTree
testDeleteRolePolicy = undefined

testUpdateUser :: UpdateUser -> TestTree
testUpdateUser = undefined

testDeleteUser :: DeleteUser -> TestTree
testDeleteUser = undefined

testListRolePolicies :: ListRolePolicies -> TestTree
testListRolePolicies = undefined

testAddClientIDToOpenIDConnectProvider :: AddClientIDToOpenIDConnectProvider -> TestTree
testAddClientIDToOpenIDConnectProvider = undefined

testGetAccessKeyLastUsed :: GetAccessKeyLastUsed -> TestTree
testGetAccessKeyLastUsed = undefined

testGetAccountPasswordPolicy :: GetAccountPasswordPolicy -> TestTree
testGetAccountPasswordPolicy = undefined

testListAccountAliases :: ListAccountAliases -> TestTree
testListAccountAliases = undefined

testCreateAccountAlias :: CreateAccountAlias -> TestTree
testCreateAccountAlias = undefined

testUploadServerCertificate :: UploadServerCertificate -> TestTree
testUploadServerCertificate = undefined

testListMFADevices :: ListMFADevices -> TestTree
testListMFADevices = undefined

testEnableMFADevice :: EnableMFADevice -> TestTree
testEnableMFADevice = undefined

testListPolicyVersions :: ListPolicyVersions -> TestTree
testListPolicyVersions = undefined

testListSAMLProviders :: ListSAMLProviders -> TestTree
testListSAMLProviders = undefined

testUpdateSAMLProvider :: UpdateSAMLProvider -> TestTree
testUpdateSAMLProvider = undefined

testDeleteSAMLProvider :: DeleteSAMLProvider -> TestTree
testDeleteSAMLProvider = undefined

testCreateGroup :: CreateGroup -> TestTree
testCreateGroup = undefined

testSetDefaultPolicyVersion :: SetDefaultPolicyVersion -> TestTree
testSetDefaultPolicyVersion = undefined

testListInstanceProfiles :: ListInstanceProfiles -> TestTree
testListInstanceProfiles = undefined

testListGroups :: ListGroups -> TestTree
testListGroups = undefined

testDeleteGroup :: DeleteGroup -> TestTree
testDeleteGroup = undefined

testUpdateGroup :: UpdateGroup -> TestTree
testUpdateGroup = undefined

testGetServerCertificate :: GetServerCertificate -> TestTree
testGetServerCertificate = undefined

testGetPolicy :: GetPolicy -> TestTree
testGetPolicy = undefined

testGenerateCredentialReport :: GenerateCredentialReport -> TestTree
testGenerateCredentialReport = undefined

testGetGroup :: GetGroup -> TestTree
testGetGroup = undefined

testDeleteServerCertificate :: DeleteServerCertificate -> TestTree
testDeleteServerCertificate = undefined

testUpdateServerCertificate :: UpdateServerCertificate -> TestTree
testUpdateServerCertificate = undefined

testDeleteLoginProfile :: DeleteLoginProfile -> TestTree
testDeleteLoginProfile = undefined

testUpdateLoginProfile :: UpdateLoginProfile -> TestTree
testUpdateLoginProfile = undefined

testListAttachedGroupPolicies :: ListAttachedGroupPolicies -> TestTree
testListAttachedGroupPolicies = undefined

-- Responses

testAttachGroupPolicyResponse :: AttachGroupPolicyResponse -> TestTree
testAttachGroupPolicyResponse = resp
    "AttachGroupPolicyResponse"
    "fixture/AttachGroupPolicyResponse"
    (Proxy :: Proxy AttachGroupPolicy)

testListInstanceProfilesForRoleResponse :: ListInstanceProfilesForRoleResponse -> TestTree
testListInstanceProfilesForRoleResponse = resp
    "ListInstanceProfilesForRoleResponse"
    "fixture/ListInstanceProfilesForRoleResponse"
    (Proxy :: Proxy ListInstanceProfilesForRole)

testCreatePolicyResponse :: CreatePolicyResponse -> TestTree
testCreatePolicyResponse = resp
    "CreatePolicyResponse"
    "fixture/CreatePolicyResponse"
    (Proxy :: Proxy CreatePolicy)

testListPoliciesResponse :: ListPoliciesResponse -> TestTree
testListPoliciesResponse = resp
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse"
    (Proxy :: Proxy ListPolicies)

testAttachRolePolicyResponse :: AttachRolePolicyResponse -> TestTree
testAttachRolePolicyResponse = resp
    "AttachRolePolicyResponse"
    "fixture/AttachRolePolicyResponse"
    (Proxy :: Proxy AttachRolePolicy)

testListOpenIDConnectProvidersResponse :: ListOpenIDConnectProvidersResponse -> TestTree
testListOpenIDConnectProvidersResponse = resp
    "ListOpenIDConnectProvidersResponse"
    "fixture/ListOpenIDConnectProvidersResponse"
    (Proxy :: Proxy ListOpenIDConnectProviders)

testDeleteAccountPasswordPolicyResponse :: DeleteAccountPasswordPolicyResponse -> TestTree
testDeleteAccountPasswordPolicyResponse = resp
    "DeleteAccountPasswordPolicyResponse"
    "fixture/DeleteAccountPasswordPolicyResponse"
    (Proxy :: Proxy DeleteAccountPasswordPolicy)

testUpdateAccountPasswordPolicyResponse :: UpdateAccountPasswordPolicyResponse -> TestTree
testUpdateAccountPasswordPolicyResponse = resp
    "UpdateAccountPasswordPolicyResponse"
    "fixture/UpdateAccountPasswordPolicyResponse"
    (Proxy :: Proxy UpdateAccountPasswordPolicy)

testCreateAccessKeyResponse :: CreateAccessKeyResponse -> TestTree
testCreateAccessKeyResponse = resp
    "CreateAccessKeyResponse"
    "fixture/CreateAccessKeyResponse"
    (Proxy :: Proxy CreateAccessKey)

testGetUserPolicyResponse :: GetUserPolicyResponse -> TestTree
testGetUserPolicyResponse = resp
    "GetUserPolicyResponse"
    "fixture/GetUserPolicyResponse"
    (Proxy :: Proxy GetUserPolicy)

testCreateVirtualMFADeviceResponse :: CreateVirtualMFADeviceResponse -> TestTree
testCreateVirtualMFADeviceResponse = resp
    "CreateVirtualMFADeviceResponse"
    "fixture/CreateVirtualMFADeviceResponse"
    (Proxy :: Proxy CreateVirtualMFADevice)

testCreateOpenIDConnectProviderResponse :: CreateOpenIDConnectProviderResponse -> TestTree
testCreateOpenIDConnectProviderResponse = resp
    "CreateOpenIDConnectProviderResponse"
    "fixture/CreateOpenIDConnectProviderResponse"
    (Proxy :: Proxy CreateOpenIDConnectProvider)

testListAttachedRolePoliciesResponse :: ListAttachedRolePoliciesResponse -> TestTree
testListAttachedRolePoliciesResponse = resp
    "ListAttachedRolePoliciesResponse"
    "fixture/ListAttachedRolePoliciesResponse"
    (Proxy :: Proxy ListAttachedRolePolicies)

testDeleteVirtualMFADeviceResponse :: DeleteVirtualMFADeviceResponse -> TestTree
testDeleteVirtualMFADeviceResponse = resp
    "DeleteVirtualMFADeviceResponse"
    "fixture/DeleteVirtualMFADeviceResponse"
    (Proxy :: Proxy DeleteVirtualMFADevice)

testGetRoleResponse :: GetRoleResponse -> TestTree
testGetRoleResponse = resp
    "GetRoleResponse"
    "fixture/GetRoleResponse"
    (Proxy :: Proxy GetRole)

testDeactivateMFADeviceResponse :: DeactivateMFADeviceResponse -> TestTree
testDeactivateMFADeviceResponse = resp
    "DeactivateMFADeviceResponse"
    "fixture/DeactivateMFADeviceResponse"
    (Proxy :: Proxy DeactivateMFADevice)

testListRolesResponse :: ListRolesResponse -> TestTree
testListRolesResponse = resp
    "ListRolesResponse"
    "fixture/ListRolesResponse"
    (Proxy :: Proxy ListRoles)

testDeleteRoleResponse :: DeleteRoleResponse -> TestTree
testDeleteRoleResponse = resp
    "DeleteRoleResponse"
    "fixture/DeleteRoleResponse"
    (Proxy :: Proxy DeleteRole)

testListUserPoliciesResponse :: ListUserPoliciesResponse -> TestTree
testListUserPoliciesResponse = resp
    "ListUserPoliciesResponse"
    "fixture/ListUserPoliciesResponse"
    (Proxy :: Proxy ListUserPolicies)

testListUsersResponse :: ListUsersResponse -> TestTree
testListUsersResponse = resp
    "ListUsersResponse"
    "fixture/ListUsersResponse"
    (Proxy :: Proxy ListUsers)

testUpdateOpenIDConnectProviderThumbprintResponse :: UpdateOpenIDConnectProviderThumbprintResponse -> TestTree
testUpdateOpenIDConnectProviderThumbprintResponse = resp
    "UpdateOpenIDConnectProviderThumbprintResponse"
    "fixture/UpdateOpenIDConnectProviderThumbprintResponse"
    (Proxy :: Proxy UpdateOpenIDConnectProviderThumbprint)

testPutUserPolicyResponse :: PutUserPolicyResponse -> TestTree
testPutUserPolicyResponse = resp
    "PutUserPolicyResponse"
    "fixture/PutUserPolicyResponse"
    (Proxy :: Proxy PutUserPolicy)

testCreateRoleResponse :: CreateRoleResponse -> TestTree
testCreateRoleResponse = resp
    "CreateRoleResponse"
    "fixture/CreateRoleResponse"
    (Proxy :: Proxy CreateRole)

testDeleteUserPolicyResponse :: DeleteUserPolicyResponse -> TestTree
testDeleteUserPolicyResponse = resp
    "DeleteUserPolicyResponse"
    "fixture/DeleteUserPolicyResponse"
    (Proxy :: Proxy DeleteUserPolicy)

testGetOpenIDConnectProviderResponse :: GetOpenIDConnectProviderResponse -> TestTree
testGetOpenIDConnectProviderResponse = resp
    "GetOpenIDConnectProviderResponse"
    "fixture/GetOpenIDConnectProviderResponse"
    (Proxy :: Proxy GetOpenIDConnectProvider)

testDetachGroupPolicyResponse :: DetachGroupPolicyResponse -> TestTree
testDetachGroupPolicyResponse = resp
    "DetachGroupPolicyResponse"
    "fixture/DetachGroupPolicyResponse"
    (Proxy :: Proxy DetachGroupPolicy)

testGetCredentialReportResponse :: GetCredentialReportResponse -> TestTree
testGetCredentialReportResponse = resp
    "GetCredentialReportResponse"
    "fixture/GetCredentialReportResponse"
    (Proxy :: Proxy GetCredentialReport)

testDeletePolicyVersionResponse :: DeletePolicyVersionResponse -> TestTree
testDeletePolicyVersionResponse = resp
    "DeletePolicyVersionResponse"
    "fixture/DeletePolicyVersionResponse"
    (Proxy :: Proxy DeletePolicyVersion)

testDetachRolePolicyResponse :: DetachRolePolicyResponse -> TestTree
testDetachRolePolicyResponse = resp
    "DetachRolePolicyResponse"
    "fixture/DetachRolePolicyResponse"
    (Proxy :: Proxy DetachRolePolicy)

testDeleteInstanceProfileResponse :: DeleteInstanceProfileResponse -> TestTree
testDeleteInstanceProfileResponse = resp
    "DeleteInstanceProfileResponse"
    "fixture/DeleteInstanceProfileResponse"
    (Proxy :: Proxy DeleteInstanceProfile)

testListGroupPoliciesResponse :: ListGroupPoliciesResponse -> TestTree
testListGroupPoliciesResponse = resp
    "ListGroupPoliciesResponse"
    "fixture/ListGroupPoliciesResponse"
    (Proxy :: Proxy ListGroupPolicies)

testGetAccountSummaryResponse :: GetAccountSummaryResponse -> TestTree
testGetAccountSummaryResponse = resp
    "GetAccountSummaryResponse"
    "fixture/GetAccountSummaryResponse"
    (Proxy :: Proxy GetAccountSummary)

testCreateInstanceProfileResponse :: CreateInstanceProfileResponse -> TestTree
testCreateInstanceProfileResponse = resp
    "CreateInstanceProfileResponse"
    "fixture/CreateInstanceProfileResponse"
    (Proxy :: Proxy CreateInstanceProfile)

testPutGroupPolicyResponse :: PutGroupPolicyResponse -> TestTree
testPutGroupPolicyResponse = resp
    "PutGroupPolicyResponse"
    "fixture/PutGroupPolicyResponse"
    (Proxy :: Proxy PutGroupPolicy)

testDeleteGroupPolicyResponse :: DeleteGroupPolicyResponse -> TestTree
testDeleteGroupPolicyResponse = resp
    "DeleteGroupPolicyResponse"
    "fixture/DeleteGroupPolicyResponse"
    (Proxy :: Proxy DeleteGroupPolicy)

testGetAccountAuthorizationDetailsResponse :: GetAccountAuthorizationDetailsResponse -> TestTree
testGetAccountAuthorizationDetailsResponse = resp
    "GetAccountAuthorizationDetailsResponse"
    "fixture/GetAccountAuthorizationDetailsResponse"
    (Proxy :: Proxy GetAccountAuthorizationDetails)

testDeleteAccountAliasResponse :: DeleteAccountAliasResponse -> TestTree
testDeleteAccountAliasResponse = resp
    "DeleteAccountAliasResponse"
    "fixture/DeleteAccountAliasResponse"
    (Proxy :: Proxy DeleteAccountAlias)

testRemoveRoleFromInstanceProfileResponse :: RemoveRoleFromInstanceProfileResponse -> TestTree
testRemoveRoleFromInstanceProfileResponse = resp
    "RemoveRoleFromInstanceProfileResponse"
    "fixture/RemoveRoleFromInstanceProfileResponse"
    (Proxy :: Proxy RemoveRoleFromInstanceProfile)

testGetLoginProfileResponse :: GetLoginProfileResponse -> TestTree
testGetLoginProfileResponse = resp
    "GetLoginProfileResponse"
    "fixture/GetLoginProfileResponse"
    (Proxy :: Proxy GetLoginProfile)

testRemoveUserFromGroupResponse :: RemoveUserFromGroupResponse -> TestTree
testRemoveUserFromGroupResponse = resp
    "RemoveUserFromGroupResponse"
    "fixture/RemoveUserFromGroupResponse"
    (Proxy :: Proxy RemoveUserFromGroup)

testDetachUserPolicyResponse :: DetachUserPolicyResponse -> TestTree
testDetachUserPolicyResponse = resp
    "DetachUserPolicyResponse"
    "fixture/DetachUserPolicyResponse"
    (Proxy :: Proxy DetachUserPolicy)

testCreateSAMLProviderResponse :: CreateSAMLProviderResponse -> TestTree
testCreateSAMLProviderResponse = resp
    "CreateSAMLProviderResponse"
    "fixture/CreateSAMLProviderResponse"
    (Proxy :: Proxy CreateSAMLProvider)

testCreatePolicyVersionResponse :: CreatePolicyVersionResponse -> TestTree
testCreatePolicyVersionResponse = resp
    "CreatePolicyVersionResponse"
    "fixture/CreatePolicyVersionResponse"
    (Proxy :: Proxy CreatePolicyVersion)

testGetGroupPolicyResponse :: GetGroupPolicyResponse -> TestTree
testGetGroupPolicyResponse = resp
    "GetGroupPolicyResponse"
    "fixture/GetGroupPolicyResponse"
    (Proxy :: Proxy GetGroupPolicy)

testDeletePolicyResponse :: DeletePolicyResponse -> TestTree
testDeletePolicyResponse = resp
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse"
    (Proxy :: Proxy DeletePolicy)

testListServerCertificatesResponse :: ListServerCertificatesResponse -> TestTree
testListServerCertificatesResponse = resp
    "ListServerCertificatesResponse"
    "fixture/ListServerCertificatesResponse"
    (Proxy :: Proxy ListServerCertificates)

testUpdateAssumeRolePolicyResponse :: UpdateAssumeRolePolicyResponse -> TestTree
testUpdateAssumeRolePolicyResponse = resp
    "UpdateAssumeRolePolicyResponse"
    "fixture/UpdateAssumeRolePolicyResponse"
    (Proxy :: Proxy UpdateAssumeRolePolicy)

testChangePasswordResponse :: ChangePasswordResponse -> TestTree
testChangePasswordResponse = resp
    "ChangePasswordResponse"
    "fixture/ChangePasswordResponse"
    (Proxy :: Proxy ChangePassword)

testListGroupsForUserResponse :: ListGroupsForUserResponse -> TestTree
testListGroupsForUserResponse = resp
    "ListGroupsForUserResponse"
    "fixture/ListGroupsForUserResponse"
    (Proxy :: Proxy ListGroupsForUser)

testGetPolicyVersionResponse :: GetPolicyVersionResponse -> TestTree
testGetPolicyVersionResponse = resp
    "GetPolicyVersionResponse"
    "fixture/GetPolicyVersionResponse"
    (Proxy :: Proxy GetPolicyVersion)

testCreateLoginProfileResponse :: CreateLoginProfileResponse -> TestTree
testCreateLoginProfileResponse = resp
    "CreateLoginProfileResponse"
    "fixture/CreateLoginProfileResponse"
    (Proxy :: Proxy CreateLoginProfile)

testGetInstanceProfileResponse :: GetInstanceProfileResponse -> TestTree
testGetInstanceProfileResponse = resp
    "GetInstanceProfileResponse"
    "fixture/GetInstanceProfileResponse"
    (Proxy :: Proxy GetInstanceProfile)

testListEntitiesForPolicyResponse :: ListEntitiesForPolicyResponse -> TestTree
testListEntitiesForPolicyResponse = resp
    "ListEntitiesForPolicyResponse"
    "fixture/ListEntitiesForPolicyResponse"
    (Proxy :: Proxy ListEntitiesForPolicy)

testGetSAMLProviderResponse :: GetSAMLProviderResponse -> TestTree
testGetSAMLProviderResponse = resp
    "GetSAMLProviderResponse"
    "fixture/GetSAMLProviderResponse"
    (Proxy :: Proxy GetSAMLProvider)

testAddRoleToInstanceProfileResponse :: AddRoleToInstanceProfileResponse -> TestTree
testAddRoleToInstanceProfileResponse = resp
    "AddRoleToInstanceProfileResponse"
    "fixture/AddRoleToInstanceProfileResponse"
    (Proxy :: Proxy AddRoleToInstanceProfile)

testAddUserToGroupResponse :: AddUserToGroupResponse -> TestTree
testAddUserToGroupResponse = resp
    "AddUserToGroupResponse"
    "fixture/AddUserToGroupResponse"
    (Proxy :: Proxy AddUserToGroup)

testDeleteOpenIDConnectProviderResponse :: DeleteOpenIDConnectProviderResponse -> TestTree
testDeleteOpenIDConnectProviderResponse = resp
    "DeleteOpenIDConnectProviderResponse"
    "fixture/DeleteOpenIDConnectProviderResponse"
    (Proxy :: Proxy DeleteOpenIDConnectProvider)

testGetUserResponse :: GetUserResponse -> TestTree
testGetUserResponse = resp
    "GetUserResponse"
    "fixture/GetUserResponse"
    (Proxy :: Proxy GetUser)

testListAttachedUserPoliciesResponse :: ListAttachedUserPoliciesResponse -> TestTree
testListAttachedUserPoliciesResponse = resp
    "ListAttachedUserPoliciesResponse"
    "fixture/ListAttachedUserPoliciesResponse"
    (Proxy :: Proxy ListAttachedUserPolicies)

testDeleteSigningCertificateResponse :: DeleteSigningCertificateResponse -> TestTree
testDeleteSigningCertificateResponse = resp
    "DeleteSigningCertificateResponse"
    "fixture/DeleteSigningCertificateResponse"
    (Proxy :: Proxy DeleteSigningCertificate)

testUpdateSigningCertificateResponse :: UpdateSigningCertificateResponse -> TestTree
testUpdateSigningCertificateResponse = resp
    "UpdateSigningCertificateResponse"
    "fixture/UpdateSigningCertificateResponse"
    (Proxy :: Proxy UpdateSigningCertificate)

testListSigningCertificatesResponse :: ListSigningCertificatesResponse -> TestTree
testListSigningCertificatesResponse = resp
    "ListSigningCertificatesResponse"
    "fixture/ListSigningCertificatesResponse"
    (Proxy :: Proxy ListSigningCertificates)

testRemoveClientIDFromOpenIDConnectProviderResponse :: RemoveClientIDFromOpenIDConnectProviderResponse -> TestTree
testRemoveClientIDFromOpenIDConnectProviderResponse = resp
    "RemoveClientIDFromOpenIDConnectProviderResponse"
    "fixture/RemoveClientIDFromOpenIDConnectProviderResponse"
    (Proxy :: Proxy RemoveClientIDFromOpenIDConnectProvider)

testListAccessKeysResponse :: ListAccessKeysResponse -> TestTree
testListAccessKeysResponse = resp
    "ListAccessKeysResponse"
    "fixture/ListAccessKeysResponse"
    (Proxy :: Proxy ListAccessKeys)

testListVirtualMFADevicesResponse :: ListVirtualMFADevicesResponse -> TestTree
testListVirtualMFADevicesResponse = resp
    "ListVirtualMFADevicesResponse"
    "fixture/ListVirtualMFADevicesResponse"
    (Proxy :: Proxy ListVirtualMFADevices)

testDeleteAccessKeyResponse :: DeleteAccessKeyResponse -> TestTree
testDeleteAccessKeyResponse = resp
    "DeleteAccessKeyResponse"
    "fixture/DeleteAccessKeyResponse"
    (Proxy :: Proxy DeleteAccessKey)

testUpdateAccessKeyResponse :: UpdateAccessKeyResponse -> TestTree
testUpdateAccessKeyResponse = resp
    "UpdateAccessKeyResponse"
    "fixture/UpdateAccessKeyResponse"
    (Proxy :: Proxy UpdateAccessKey)

testGetRolePolicyResponse :: GetRolePolicyResponse -> TestTree
testGetRolePolicyResponse = resp
    "GetRolePolicyResponse"
    "fixture/GetRolePolicyResponse"
    (Proxy :: Proxy GetRolePolicy)

testAttachUserPolicyResponse :: AttachUserPolicyResponse -> TestTree
testAttachUserPolicyResponse = resp
    "AttachUserPolicyResponse"
    "fixture/AttachUserPolicyResponse"
    (Proxy :: Proxy AttachUserPolicy)

testResyncMFADeviceResponse :: ResyncMFADeviceResponse -> TestTree
testResyncMFADeviceResponse = resp
    "ResyncMFADeviceResponse"
    "fixture/ResyncMFADeviceResponse"
    (Proxy :: Proxy ResyncMFADevice)

testCreateUserResponse :: CreateUserResponse -> TestTree
testCreateUserResponse = resp
    "CreateUserResponse"
    "fixture/CreateUserResponse"
    (Proxy :: Proxy CreateUser)

testUploadSigningCertificateResponse :: UploadSigningCertificateResponse -> TestTree
testUploadSigningCertificateResponse = resp
    "UploadSigningCertificateResponse"
    "fixture/UploadSigningCertificateResponse"
    (Proxy :: Proxy UploadSigningCertificate)

testPutRolePolicyResponse :: PutRolePolicyResponse -> TestTree
testPutRolePolicyResponse = resp
    "PutRolePolicyResponse"
    "fixture/PutRolePolicyResponse"
    (Proxy :: Proxy PutRolePolicy)

testDeleteRolePolicyResponse :: DeleteRolePolicyResponse -> TestTree
testDeleteRolePolicyResponse = resp
    "DeleteRolePolicyResponse"
    "fixture/DeleteRolePolicyResponse"
    (Proxy :: Proxy DeleteRolePolicy)

testUpdateUserResponse :: UpdateUserResponse -> TestTree
testUpdateUserResponse = resp
    "UpdateUserResponse"
    "fixture/UpdateUserResponse"
    (Proxy :: Proxy UpdateUser)

testDeleteUserResponse :: DeleteUserResponse -> TestTree
testDeleteUserResponse = resp
    "DeleteUserResponse"
    "fixture/DeleteUserResponse"
    (Proxy :: Proxy DeleteUser)

testListRolePoliciesResponse :: ListRolePoliciesResponse -> TestTree
testListRolePoliciesResponse = resp
    "ListRolePoliciesResponse"
    "fixture/ListRolePoliciesResponse"
    (Proxy :: Proxy ListRolePolicies)

testAddClientIDToOpenIDConnectProviderResponse :: AddClientIDToOpenIDConnectProviderResponse -> TestTree
testAddClientIDToOpenIDConnectProviderResponse = resp
    "AddClientIDToOpenIDConnectProviderResponse"
    "fixture/AddClientIDToOpenIDConnectProviderResponse"
    (Proxy :: Proxy AddClientIDToOpenIDConnectProvider)

testGetAccessKeyLastUsedResponse :: GetAccessKeyLastUsedResponse -> TestTree
testGetAccessKeyLastUsedResponse = resp
    "GetAccessKeyLastUsedResponse"
    "fixture/GetAccessKeyLastUsedResponse"
    (Proxy :: Proxy GetAccessKeyLastUsed)

testGetAccountPasswordPolicyResponse :: GetAccountPasswordPolicyResponse -> TestTree
testGetAccountPasswordPolicyResponse = resp
    "GetAccountPasswordPolicyResponse"
    "fixture/GetAccountPasswordPolicyResponse"
    (Proxy :: Proxy GetAccountPasswordPolicy)

testListAccountAliasesResponse :: ListAccountAliasesResponse -> TestTree
testListAccountAliasesResponse = resp
    "ListAccountAliasesResponse"
    "fixture/ListAccountAliasesResponse"
    (Proxy :: Proxy ListAccountAliases)

testCreateAccountAliasResponse :: CreateAccountAliasResponse -> TestTree
testCreateAccountAliasResponse = resp
    "CreateAccountAliasResponse"
    "fixture/CreateAccountAliasResponse"
    (Proxy :: Proxy CreateAccountAlias)

testUploadServerCertificateResponse :: UploadServerCertificateResponse -> TestTree
testUploadServerCertificateResponse = resp
    "UploadServerCertificateResponse"
    "fixture/UploadServerCertificateResponse"
    (Proxy :: Proxy UploadServerCertificate)

testListMFADevicesResponse :: ListMFADevicesResponse -> TestTree
testListMFADevicesResponse = resp
    "ListMFADevicesResponse"
    "fixture/ListMFADevicesResponse"
    (Proxy :: Proxy ListMFADevices)

testEnableMFADeviceResponse :: EnableMFADeviceResponse -> TestTree
testEnableMFADeviceResponse = resp
    "EnableMFADeviceResponse"
    "fixture/EnableMFADeviceResponse"
    (Proxy :: Proxy EnableMFADevice)

testListPolicyVersionsResponse :: ListPolicyVersionsResponse -> TestTree
testListPolicyVersionsResponse = resp
    "ListPolicyVersionsResponse"
    "fixture/ListPolicyVersionsResponse"
    (Proxy :: Proxy ListPolicyVersions)

testListSAMLProvidersResponse :: ListSAMLProvidersResponse -> TestTree
testListSAMLProvidersResponse = resp
    "ListSAMLProvidersResponse"
    "fixture/ListSAMLProvidersResponse"
    (Proxy :: Proxy ListSAMLProviders)

testUpdateSAMLProviderResponse :: UpdateSAMLProviderResponse -> TestTree
testUpdateSAMLProviderResponse = resp
    "UpdateSAMLProviderResponse"
    "fixture/UpdateSAMLProviderResponse"
    (Proxy :: Proxy UpdateSAMLProvider)

testDeleteSAMLProviderResponse :: DeleteSAMLProviderResponse -> TestTree
testDeleteSAMLProviderResponse = resp
    "DeleteSAMLProviderResponse"
    "fixture/DeleteSAMLProviderResponse"
    (Proxy :: Proxy DeleteSAMLProvider)

testCreateGroupResponse :: CreateGroupResponse -> TestTree
testCreateGroupResponse = resp
    "CreateGroupResponse"
    "fixture/CreateGroupResponse"
    (Proxy :: Proxy CreateGroup)

testSetDefaultPolicyVersionResponse :: SetDefaultPolicyVersionResponse -> TestTree
testSetDefaultPolicyVersionResponse = resp
    "SetDefaultPolicyVersionResponse"
    "fixture/SetDefaultPolicyVersionResponse"
    (Proxy :: Proxy SetDefaultPolicyVersion)

testListInstanceProfilesResponse :: ListInstanceProfilesResponse -> TestTree
testListInstanceProfilesResponse = resp
    "ListInstanceProfilesResponse"
    "fixture/ListInstanceProfilesResponse"
    (Proxy :: Proxy ListInstanceProfiles)

testListGroupsResponse :: ListGroupsResponse -> TestTree
testListGroupsResponse = resp
    "ListGroupsResponse"
    "fixture/ListGroupsResponse"
    (Proxy :: Proxy ListGroups)

testDeleteGroupResponse :: DeleteGroupResponse -> TestTree
testDeleteGroupResponse = resp
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse"
    (Proxy :: Proxy DeleteGroup)

testUpdateGroupResponse :: UpdateGroupResponse -> TestTree
testUpdateGroupResponse = resp
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse"
    (Proxy :: Proxy UpdateGroup)

testGetServerCertificateResponse :: GetServerCertificateResponse -> TestTree
testGetServerCertificateResponse = resp
    "GetServerCertificateResponse"
    "fixture/GetServerCertificateResponse"
    (Proxy :: Proxy GetServerCertificate)

testGetPolicyResponse :: GetPolicyResponse -> TestTree
testGetPolicyResponse = resp
    "GetPolicyResponse"
    "fixture/GetPolicyResponse"
    (Proxy :: Proxy GetPolicy)

testGenerateCredentialReportResponse :: GenerateCredentialReportResponse -> TestTree
testGenerateCredentialReportResponse = resp
    "GenerateCredentialReportResponse"
    "fixture/GenerateCredentialReportResponse"
    (Proxy :: Proxy GenerateCredentialReport)

testGetGroupResponse :: GetGroupResponse -> TestTree
testGetGroupResponse = resp
    "GetGroupResponse"
    "fixture/GetGroupResponse"
    (Proxy :: Proxy GetGroup)

testDeleteServerCertificateResponse :: DeleteServerCertificateResponse -> TestTree
testDeleteServerCertificateResponse = resp
    "DeleteServerCertificateResponse"
    "fixture/DeleteServerCertificateResponse"
    (Proxy :: Proxy DeleteServerCertificate)

testUpdateServerCertificateResponse :: UpdateServerCertificateResponse -> TestTree
testUpdateServerCertificateResponse = resp
    "UpdateServerCertificateResponse"
    "fixture/UpdateServerCertificateResponse"
    (Proxy :: Proxy UpdateServerCertificate)

testDeleteLoginProfileResponse :: DeleteLoginProfileResponse -> TestTree
testDeleteLoginProfileResponse = resp
    "DeleteLoginProfileResponse"
    "fixture/DeleteLoginProfileResponse"
    (Proxy :: Proxy DeleteLoginProfile)

testUpdateLoginProfileResponse :: UpdateLoginProfileResponse -> TestTree
testUpdateLoginProfileResponse = resp
    "UpdateLoginProfileResponse"
    "fixture/UpdateLoginProfileResponse"
    (Proxy :: Proxy UpdateLoginProfile)

testListAttachedGroupPoliciesResponse :: ListAttachedGroupPoliciesResponse -> TestTree
testListAttachedGroupPoliciesResponse = resp
    "ListAttachedGroupPoliciesResponse"
    "fixture/ListAttachedGroupPoliciesResponse"
    (Proxy :: Proxy ListAttachedGroupPolicies)

instance Out AccessKey
instance Out AccessKeyLastUsed
instance Out AccessKeyMetadata
instance Out AddClientIDToOpenIDConnectProvider
instance Out AddClientIDToOpenIDConnectProviderResponse
instance Out AddRoleToInstanceProfile
instance Out AddRoleToInstanceProfileResponse
instance Out AddUserToGroup
instance Out AddUserToGroupResponse
instance Out AssignmentStatusType
instance Out AttachGroupPolicy
instance Out AttachGroupPolicyResponse
instance Out AttachRolePolicy
instance Out AttachRolePolicyResponse
instance Out AttachUserPolicy
instance Out AttachUserPolicyResponse
instance Out AttachedPolicy
instance Out ChangePassword
instance Out ChangePasswordResponse
instance Out CreateAccessKey
instance Out CreateAccessKeyResponse
instance Out CreateAccountAlias
instance Out CreateAccountAliasResponse
instance Out CreateGroup
instance Out CreateGroupResponse
instance Out CreateInstanceProfile
instance Out CreateInstanceProfileResponse
instance Out CreateLoginProfile
instance Out CreateLoginProfileResponse
instance Out CreateOpenIDConnectProvider
instance Out CreateOpenIDConnectProviderResponse
instance Out CreatePolicy
instance Out CreatePolicyResponse
instance Out CreatePolicyVersion
instance Out CreatePolicyVersionResponse
instance Out CreateRole
instance Out CreateRoleResponse
instance Out CreateSAMLProvider
instance Out CreateSAMLProviderResponse
instance Out CreateUser
instance Out CreateUserResponse
instance Out CreateVirtualMFADevice
instance Out CreateVirtualMFADeviceResponse
instance Out DeactivateMFADevice
instance Out DeactivateMFADeviceResponse
instance Out DeleteAccessKey
instance Out DeleteAccessKeyResponse
instance Out DeleteAccountAlias
instance Out DeleteAccountAliasResponse
instance Out DeleteAccountPasswordPolicy
instance Out DeleteAccountPasswordPolicyResponse
instance Out DeleteGroup
instance Out DeleteGroupPolicy
instance Out DeleteGroupPolicyResponse
instance Out DeleteGroupResponse
instance Out DeleteInstanceProfile
instance Out DeleteInstanceProfileResponse
instance Out DeleteLoginProfile
instance Out DeleteLoginProfileResponse
instance Out DeleteOpenIDConnectProvider
instance Out DeleteOpenIDConnectProviderResponse
instance Out DeletePolicy
instance Out DeletePolicyResponse
instance Out DeletePolicyVersion
instance Out DeletePolicyVersionResponse
instance Out DeleteRole
instance Out DeleteRolePolicy
instance Out DeleteRolePolicyResponse
instance Out DeleteRoleResponse
instance Out DeleteSAMLProvider
instance Out DeleteSAMLProviderResponse
instance Out DeleteServerCertificate
instance Out DeleteServerCertificateResponse
instance Out DeleteSigningCertificate
instance Out DeleteSigningCertificateResponse
instance Out DeleteUser
instance Out DeleteUserPolicy
instance Out DeleteUserPolicyResponse
instance Out DeleteUserResponse
instance Out DeleteVirtualMFADevice
instance Out DeleteVirtualMFADeviceResponse
instance Out DetachGroupPolicy
instance Out DetachGroupPolicyResponse
instance Out DetachRolePolicy
instance Out DetachRolePolicyResponse
instance Out DetachUserPolicy
instance Out DetachUserPolicyResponse
instance Out EnableMFADevice
instance Out EnableMFADeviceResponse
instance Out EntityType
instance Out GenerateCredentialReport
instance Out GenerateCredentialReportResponse
instance Out GetAccessKeyLastUsed
instance Out GetAccessKeyLastUsedResponse
instance Out GetAccountAuthorizationDetails
instance Out GetAccountAuthorizationDetailsResponse
instance Out GetAccountPasswordPolicy
instance Out GetAccountPasswordPolicyResponse
instance Out GetAccountSummary
instance Out GetAccountSummaryResponse
instance Out GetCredentialReport
instance Out GetCredentialReportResponse
instance Out GetGroup
instance Out GetGroupPolicy
instance Out GetGroupPolicyResponse
instance Out GetGroupResponse
instance Out GetInstanceProfile
instance Out GetInstanceProfileResponse
instance Out GetLoginProfile
instance Out GetLoginProfileResponse
instance Out GetOpenIDConnectProvider
instance Out GetOpenIDConnectProviderResponse
instance Out GetPolicy
instance Out GetPolicyResponse
instance Out GetPolicyVersion
instance Out GetPolicyVersionResponse
instance Out GetRole
instance Out GetRolePolicy
instance Out GetRolePolicyResponse
instance Out GetRoleResponse
instance Out GetSAMLProvider
instance Out GetSAMLProviderResponse
instance Out GetServerCertificate
instance Out GetServerCertificateResponse
instance Out GetUser
instance Out GetUserPolicy
instance Out GetUserPolicyResponse
instance Out GetUserResponse
instance Out Group
instance Out GroupDetail
instance Out InstanceProfile
instance Out ListAccessKeys
instance Out ListAccessKeysResponse
instance Out ListAccountAliases
instance Out ListAccountAliasesResponse
instance Out ListAttachedGroupPolicies
instance Out ListAttachedGroupPoliciesResponse
instance Out ListAttachedRolePolicies
instance Out ListAttachedRolePoliciesResponse
instance Out ListAttachedUserPolicies
instance Out ListAttachedUserPoliciesResponse
instance Out ListEntitiesForPolicy
instance Out ListEntitiesForPolicyResponse
instance Out ListGroupPolicies
instance Out ListGroupPoliciesResponse
instance Out ListGroups
instance Out ListGroupsForUser
instance Out ListGroupsForUserResponse
instance Out ListGroupsResponse
instance Out ListInstanceProfiles
instance Out ListInstanceProfilesForRole
instance Out ListInstanceProfilesForRoleResponse
instance Out ListInstanceProfilesResponse
instance Out ListMFADevices
instance Out ListMFADevicesResponse
instance Out ListOpenIDConnectProviders
instance Out ListOpenIDConnectProvidersResponse
instance Out ListPolicies
instance Out ListPoliciesResponse
instance Out ListPolicyVersions
instance Out ListPolicyVersionsResponse
instance Out ListRolePolicies
instance Out ListRolePoliciesResponse
instance Out ListRoles
instance Out ListRolesResponse
instance Out ListSAMLProviders
instance Out ListSAMLProvidersResponse
instance Out ListServerCertificates
instance Out ListServerCertificatesResponse
instance Out ListSigningCertificates
instance Out ListSigningCertificatesResponse
instance Out ListUserPolicies
instance Out ListUserPoliciesResponse
instance Out ListUsers
instance Out ListUsersResponse
instance Out ListVirtualMFADevices
instance Out ListVirtualMFADevicesResponse
instance Out LoginProfile
instance Out MFADevice
instance Out ManagedPolicyDetail
instance Out OpenIDConnectProviderListEntry
instance Out PasswordPolicy
instance Out Policy
instance Out PolicyDetail
instance Out PolicyGroup
instance Out PolicyRole
instance Out PolicyScopeType
instance Out PolicyUser
instance Out PolicyVersion
instance Out PutGroupPolicy
instance Out PutGroupPolicyResponse
instance Out PutRolePolicy
instance Out PutRolePolicyResponse
instance Out PutUserPolicy
instance Out PutUserPolicyResponse
instance Out RemoveClientIDFromOpenIDConnectProvider
instance Out RemoveClientIDFromOpenIDConnectProviderResponse
instance Out RemoveRoleFromInstanceProfile
instance Out RemoveRoleFromInstanceProfileResponse
instance Out RemoveUserFromGroup
instance Out RemoveUserFromGroupResponse
instance Out ReportFormatType
instance Out ReportStateType
instance Out ResyncMFADevice
instance Out ResyncMFADeviceResponse
instance Out Role
instance Out RoleDetail
instance Out SAMLProviderListEntry
instance Out ServerCertificate
instance Out ServerCertificateMetadata
instance Out SetDefaultPolicyVersion
instance Out SetDefaultPolicyVersionResponse
instance Out SigningCertificate
instance Out StatusType
instance Out SummaryKeyType
instance Out UpdateAccessKey
instance Out UpdateAccessKeyResponse
instance Out UpdateAccountPasswordPolicy
instance Out UpdateAccountPasswordPolicyResponse
instance Out UpdateAssumeRolePolicy
instance Out UpdateAssumeRolePolicyResponse
instance Out UpdateGroup
instance Out UpdateGroupResponse
instance Out UpdateLoginProfile
instance Out UpdateLoginProfileResponse
instance Out UpdateOpenIDConnectProviderThumbprint
instance Out UpdateOpenIDConnectProviderThumbprintResponse
instance Out UpdateSAMLProvider
instance Out UpdateSAMLProviderResponse
instance Out UpdateServerCertificate
instance Out UpdateServerCertificateResponse
instance Out UpdateSigningCertificate
instance Out UpdateSigningCertificateResponse
instance Out UpdateUser
instance Out UpdateUserResponse
instance Out UploadServerCertificate
instance Out UploadServerCertificateResponse
instance Out UploadSigningCertificate
instance Out UploadSigningCertificateResponse
instance Out User
instance Out UserDetail
instance Out VirtualMFADevice
