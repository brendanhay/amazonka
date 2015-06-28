-- Module      : Test.AWS.Gen.IAM
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.IAM where

import           Data.Proxy
import           Network.AWS.IAM
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ attachGroupPolicyTest $
--             attachGroupPolicy
--
--         , listInstanceProfilesForRoleTest $
--             listInstanceProfilesForRole
--
--         , createPolicyTest $
--             createPolicy
--
--         , listPoliciesTest $
--             listPolicies
--
--         , attachRolePolicyTest $
--             attachRolePolicy
--
--         , listOpenIDConnectProvidersTest $
--             listOpenIDConnectProviders
--
--         , deleteAccountPasswordPolicyTest $
--             deleteAccountPasswordPolicy
--
--         , updateAccountPasswordPolicyTest $
--             updateAccountPasswordPolicy
--
--         , createAccessKeyTest $
--             createAccessKey
--
--         , getUserPolicyTest $
--             getUserPolicy
--
--         , createVirtualMFADeviceTest $
--             createVirtualMFADevice
--
--         , createOpenIDConnectProviderTest $
--             createOpenIDConnectProvider
--
--         , listAttachedRolePoliciesTest $
--             listAttachedRolePolicies
--
--         , deleteVirtualMFADeviceTest $
--             deleteVirtualMFADevice
--
--         , getRoleTest $
--             getRole
--
--         , deactivateMFADeviceTest $
--             deactivateMFADevice
--
--         , listRolesTest $
--             listRoles
--
--         , deleteRoleTest $
--             deleteRole
--
--         , listUserPoliciesTest $
--             listUserPolicies
--
--         , listUsersTest $
--             listUsers
--
--         , updateOpenIDConnectProviderThumbprintTest $
--             updateOpenIDConnectProviderThumbprint
--
--         , putUserPolicyTest $
--             putUserPolicy
--
--         , createRoleTest $
--             createRole
--
--         , deleteUserPolicyTest $
--             deleteUserPolicy
--
--         , getOpenIDConnectProviderTest $
--             getOpenIDConnectProvider
--
--         , detachGroupPolicyTest $
--             detachGroupPolicy
--
--         , getCredentialReportTest $
--             getCredentialReport
--
--         , deletePolicyVersionTest $
--             deletePolicyVersion
--
--         , detachRolePolicyTest $
--             detachRolePolicy
--
--         , deleteInstanceProfileTest $
--             deleteInstanceProfile
--
--         , listGroupPoliciesTest $
--             listGroupPolicies
--
--         , getAccountSummaryTest $
--             getAccountSummary
--
--         , createInstanceProfileTest $
--             createInstanceProfile
--
--         , putGroupPolicyTest $
--             putGroupPolicy
--
--         , deleteGroupPolicyTest $
--             deleteGroupPolicy
--
--         , getAccountAuthorizationDetailsTest $
--             getAccountAuthorizationDetails
--
--         , deleteAccountAliasTest $
--             deleteAccountAlias
--
--         , removeRoleFromInstanceProfileTest $
--             removeRoleFromInstanceProfile
--
--         , getLoginProfileTest $
--             getLoginProfile
--
--         , removeUserFromGroupTest $
--             removeUserFromGroup
--
--         , detachUserPolicyTest $
--             detachUserPolicy
--
--         , createSAMLProviderTest $
--             createSAMLProvider
--
--         , createPolicyVersionTest $
--             createPolicyVersion
--
--         , getGroupPolicyTest $
--             getGroupPolicy
--
--         , deletePolicyTest $
--             deletePolicy
--
--         , listServerCertificatesTest $
--             listServerCertificates
--
--         , updateAssumeRolePolicyTest $
--             updateAssumeRolePolicy
--
--         , changePasswordTest $
--             changePassword
--
--         , listGroupsForUserTest $
--             listGroupsForUser
--
--         , getPolicyVersionTest $
--             getPolicyVersion
--
--         , createLoginProfileTest $
--             createLoginProfile
--
--         , getInstanceProfileTest $
--             getInstanceProfile
--
--         , listEntitiesForPolicyTest $
--             listEntitiesForPolicy
--
--         , getSAMLProviderTest $
--             getSAMLProvider
--
--         , addRoleToInstanceProfileTest $
--             addRoleToInstanceProfile
--
--         , addUserToGroupTest $
--             addUserToGroup
--
--         , deleteOpenIDConnectProviderTest $
--             deleteOpenIDConnectProvider
--
--         , getUserTest $
--             getUser
--
--         , listAttachedUserPoliciesTest $
--             listAttachedUserPolicies
--
--         , deleteSigningCertificateTest $
--             deleteSigningCertificate
--
--         , updateSigningCertificateTest $
--             updateSigningCertificate
--
--         , listSigningCertificatesTest $
--             listSigningCertificates
--
--         , removeClientIDFromOpenIDConnectProviderTest $
--             removeClientIDFromOpenIDConnectProvider
--
--         , listAccessKeysTest $
--             listAccessKeys
--
--         , listVirtualMFADevicesTest $
--             listVirtualMFADevices
--
--         , deleteAccessKeyTest $
--             deleteAccessKey
--
--         , updateAccessKeyTest $
--             updateAccessKey
--
--         , getRolePolicyTest $
--             getRolePolicy
--
--         , attachUserPolicyTest $
--             attachUserPolicy
--
--         , resyncMFADeviceTest $
--             resyncMFADevice
--
--         , createUserTest $
--             createUser
--
--         , uploadSigningCertificateTest $
--             uploadSigningCertificate
--
--         , putRolePolicyTest $
--             putRolePolicy
--
--         , deleteRolePolicyTest $
--             deleteRolePolicy
--
--         , updateUserTest $
--             updateUser
--
--         , deleteUserTest $
--             deleteUser
--
--         , listRolePoliciesTest $
--             listRolePolicies
--
--         , addClientIDToOpenIDConnectProviderTest $
--             addClientIDToOpenIDConnectProvider
--
--         , getAccessKeyLastUsedTest $
--             getAccessKeyLastUsed
--
--         , getAccountPasswordPolicyTest $
--             getAccountPasswordPolicy
--
--         , listAccountAliasesTest $
--             listAccountAliases
--
--         , createAccountAliasTest $
--             createAccountAlias
--
--         , uploadServerCertificateTest $
--             uploadServerCertificate
--
--         , listMFADevicesTest $
--             listMFADevices
--
--         , enableMFADeviceTest $
--             enableMFADevice
--
--         , listPolicyVersionsTest $
--             listPolicyVersions
--
--         , listSAMLProvidersTest $
--             listSAMLProviders
--
--         , updateSAMLProviderTest $
--             updateSAMLProvider
--
--         , deleteSAMLProviderTest $
--             deleteSAMLProvider
--
--         , createGroupTest $
--             createGroup
--
--         , setDefaultPolicyVersionTest $
--             setDefaultPolicyVersion
--
--         , listInstanceProfilesTest $
--             listInstanceProfiles
--
--         , listGroupsTest $
--             listGroups
--
--         , deleteGroupTest $
--             deleteGroup
--
--         , updateGroupTest $
--             updateGroup
--
--         , getServerCertificateTest $
--             getServerCertificate
--
--         , getPolicyTest $
--             getPolicy
--
--         , generateCredentialReportTest $
--             generateCredentialReport
--
--         , getGroupTest $
--             getGroup
--
--         , deleteServerCertificateTest $
--             deleteServerCertificate
--
--         , updateServerCertificateTest $
--             updateServerCertificate
--
--         , deleteLoginProfileTest $
--             deleteLoginProfile
--
--         , updateLoginProfileTest $
--             updateLoginProfile
--
--         , listAttachedGroupPoliciesTest $
--             listAttachedGroupPolicies
--
--           ]

--     , testGroup "response"
--         [ attachGroupPolicyResponseTest $
--             attachGroupPolicyResponse
--
--         , listInstanceProfilesForRoleResponseTest $
--             listInstanceProfilesForRoleResponse
--
--         , createPolicyResponseTest $
--             createPolicyResponse
--
--         , listPoliciesResponseTest $
--             listPoliciesResponse
--
--         , attachRolePolicyResponseTest $
--             attachRolePolicyResponse
--
--         , listOpenIDConnectProvidersResponseTest $
--             listOpenIDConnectProvidersResponse
--
--         , deleteAccountPasswordPolicyResponseTest $
--             deleteAccountPasswordPolicyResponse
--
--         , updateAccountPasswordPolicyResponseTest $
--             updateAccountPasswordPolicyResponse
--
--         , createAccessKeyResponseTest $
--             createAccessKeyResponse
--
--         , getUserPolicyResponseTest $
--             getUserPolicyResponse
--
--         , createVirtualMFADeviceResponseTest $
--             createVirtualMFADeviceResponse
--
--         , createOpenIDConnectProviderResponseTest $
--             createOpenIDConnectProviderResponse
--
--         , listAttachedRolePoliciesResponseTest $
--             listAttachedRolePoliciesResponse
--
--         , deleteVirtualMFADeviceResponseTest $
--             deleteVirtualMFADeviceResponse
--
--         , getRoleResponseTest $
--             getRoleResponse
--
--         , deactivateMFADeviceResponseTest $
--             deactivateMFADeviceResponse
--
--         , listRolesResponseTest $
--             listRolesResponse
--
--         , deleteRoleResponseTest $
--             deleteRoleResponse
--
--         , listUserPoliciesResponseTest $
--             listUserPoliciesResponse
--
--         , listUsersResponseTest $
--             listUsersResponse
--
--         , updateOpenIDConnectProviderThumbprintResponseTest $
--             updateOpenIDConnectProviderThumbprintResponse
--
--         , putUserPolicyResponseTest $
--             putUserPolicyResponse
--
--         , createRoleResponseTest $
--             createRoleResponse
--
--         , deleteUserPolicyResponseTest $
--             deleteUserPolicyResponse
--
--         , getOpenIDConnectProviderResponseTest $
--             getOpenIDConnectProviderResponse
--
--         , detachGroupPolicyResponseTest $
--             detachGroupPolicyResponse
--
--         , getCredentialReportResponseTest $
--             getCredentialReportResponse
--
--         , deletePolicyVersionResponseTest $
--             deletePolicyVersionResponse
--
--         , detachRolePolicyResponseTest $
--             detachRolePolicyResponse
--
--         , deleteInstanceProfileResponseTest $
--             deleteInstanceProfileResponse
--
--         , listGroupPoliciesResponseTest $
--             listGroupPoliciesResponse
--
--         , getAccountSummaryResponseTest $
--             getAccountSummaryResponse
--
--         , createInstanceProfileResponseTest $
--             createInstanceProfileResponse
--
--         , putGroupPolicyResponseTest $
--             putGroupPolicyResponse
--
--         , deleteGroupPolicyResponseTest $
--             deleteGroupPolicyResponse
--
--         , getAccountAuthorizationDetailsResponseTest $
--             getAccountAuthorizationDetailsResponse
--
--         , deleteAccountAliasResponseTest $
--             deleteAccountAliasResponse
--
--         , removeRoleFromInstanceProfileResponseTest $
--             removeRoleFromInstanceProfileResponse
--
--         , getLoginProfileResponseTest $
--             getLoginProfileResponse
--
--         , removeUserFromGroupResponseTest $
--             removeUserFromGroupResponse
--
--         , detachUserPolicyResponseTest $
--             detachUserPolicyResponse
--
--         , createSAMLProviderResponseTest $
--             createSAMLProviderResponse
--
--         , createPolicyVersionResponseTest $
--             createPolicyVersionResponse
--
--         , getGroupPolicyResponseTest $
--             getGroupPolicyResponse
--
--         , deletePolicyResponseTest $
--             deletePolicyResponse
--
--         , listServerCertificatesResponseTest $
--             listServerCertificatesResponse
--
--         , updateAssumeRolePolicyResponseTest $
--             updateAssumeRolePolicyResponse
--
--         , changePasswordResponseTest $
--             changePasswordResponse
--
--         , listGroupsForUserResponseTest $
--             listGroupsForUserResponse
--
--         , getPolicyVersionResponseTest $
--             getPolicyVersionResponse
--
--         , createLoginProfileResponseTest $
--             createLoginProfileResponse
--
--         , getInstanceProfileResponseTest $
--             getInstanceProfileResponse
--
--         , listEntitiesForPolicyResponseTest $
--             listEntitiesForPolicyResponse
--
--         , getSAMLProviderResponseTest $
--             getSAMLProviderResponse
--
--         , addRoleToInstanceProfileResponseTest $
--             addRoleToInstanceProfileResponse
--
--         , addUserToGroupResponseTest $
--             addUserToGroupResponse
--
--         , deleteOpenIDConnectProviderResponseTest $
--             deleteOpenIDConnectProviderResponse
--
--         , getUserResponseTest $
--             getUserResponse
--
--         , listAttachedUserPoliciesResponseTest $
--             listAttachedUserPoliciesResponse
--
--         , deleteSigningCertificateResponseTest $
--             deleteSigningCertificateResponse
--
--         , updateSigningCertificateResponseTest $
--             updateSigningCertificateResponse
--
--         , listSigningCertificatesResponseTest $
--             listSigningCertificatesResponse
--
--         , removeClientIDFromOpenIDConnectProviderResponseTest $
--             removeClientIDFromOpenIDConnectProviderResponse
--
--         , listAccessKeysResponseTest $
--             listAccessKeysResponse
--
--         , listVirtualMFADevicesResponseTest $
--             listVirtualMFADevicesResponse
--
--         , deleteAccessKeyResponseTest $
--             deleteAccessKeyResponse
--
--         , updateAccessKeyResponseTest $
--             updateAccessKeyResponse
--
--         , getRolePolicyResponseTest $
--             getRolePolicyResponse
--
--         , attachUserPolicyResponseTest $
--             attachUserPolicyResponse
--
--         , resyncMFADeviceResponseTest $
--             resyncMFADeviceResponse
--
--         , createUserResponseTest $
--             createUserResponse
--
--         , uploadSigningCertificateResponseTest $
--             uploadSigningCertificateResponse
--
--         , putRolePolicyResponseTest $
--             putRolePolicyResponse
--
--         , deleteRolePolicyResponseTest $
--             deleteRolePolicyResponse
--
--         , updateUserResponseTest $
--             updateUserResponse
--
--         , deleteUserResponseTest $
--             deleteUserResponse
--
--         , listRolePoliciesResponseTest $
--             listRolePoliciesResponse
--
--         , addClientIDToOpenIDConnectProviderResponseTest $
--             addClientIDToOpenIDConnectProviderResponse
--
--         , getAccessKeyLastUsedResponseTest $
--             getAccessKeyLastUsedResponse
--
--         , getAccountPasswordPolicyResponseTest $
--             getAccountPasswordPolicyResponse
--
--         , listAccountAliasesResponseTest $
--             listAccountAliasesResponse
--
--         , createAccountAliasResponseTest $
--             createAccountAliasResponse
--
--         , uploadServerCertificateResponseTest $
--             uploadServerCertificateResponse
--
--         , listMFADevicesResponseTest $
--             listMFADevicesResponse
--
--         , enableMFADeviceResponseTest $
--             enableMFADeviceResponse
--
--         , listPolicyVersionsResponseTest $
--             listPolicyVersionsResponse
--
--         , listSAMLProvidersResponseTest $
--             listSAMLProvidersResponse
--
--         , updateSAMLProviderResponseTest $
--             updateSAMLProviderResponse
--
--         , deleteSAMLProviderResponseTest $
--             deleteSAMLProviderResponse
--
--         , createGroupResponseTest $
--             createGroupResponse
--
--         , setDefaultPolicyVersionResponseTest $
--             setDefaultPolicyVersionResponse
--
--         , listInstanceProfilesResponseTest $
--             listInstanceProfilesResponse
--
--         , listGroupsResponseTest $
--             listGroupsResponse
--
--         , deleteGroupResponseTest $
--             deleteGroupResponse
--
--         , updateGroupResponseTest $
--             updateGroupResponse
--
--         , getServerCertificateResponseTest $
--             getServerCertificateResponse
--
--         , getPolicyResponseTest $
--             getPolicyResponse
--
--         , generateCredentialReportResponseTest $
--             generateCredentialReportResponse
--
--         , getGroupResponseTest $
--             getGroupResponse
--
--         , deleteServerCertificateResponseTest $
--             deleteServerCertificateResponse
--
--         , updateServerCertificateResponseTest $
--             updateServerCertificateResponse
--
--         , deleteLoginProfileResponseTest $
--             deleteLoginProfileResponse
--
--         , updateLoginProfileResponseTest $
--             updateLoginProfileResponse
--
--         , listAttachedGroupPoliciesResponseTest $
--             listAttachedGroupPoliciesResponse
--
--           ]
--     ]

-- Requests

attachGroupPolicyTest :: AttachGroupPolicy -> TestTree
attachGroupPolicyTest = undefined

listInstanceProfilesForRoleTest :: ListInstanceProfilesForRole -> TestTree
listInstanceProfilesForRoleTest = undefined

createPolicyTest :: CreatePolicy -> TestTree
createPolicyTest = undefined

listPoliciesTest :: ListPolicies -> TestTree
listPoliciesTest = undefined

attachRolePolicyTest :: AttachRolePolicy -> TestTree
attachRolePolicyTest = undefined

listOpenIDConnectProvidersTest :: ListOpenIDConnectProviders -> TestTree
listOpenIDConnectProvidersTest = undefined

deleteAccountPasswordPolicyTest :: DeleteAccountPasswordPolicy -> TestTree
deleteAccountPasswordPolicyTest = undefined

updateAccountPasswordPolicyTest :: UpdateAccountPasswordPolicy -> TestTree
updateAccountPasswordPolicyTest = undefined

createAccessKeyTest :: CreateAccessKey -> TestTree
createAccessKeyTest = undefined

getUserPolicyTest :: GetUserPolicy -> TestTree
getUserPolicyTest = undefined

createVirtualMFADeviceTest :: CreateVirtualMFADevice -> TestTree
createVirtualMFADeviceTest = undefined

createOpenIDConnectProviderTest :: CreateOpenIDConnectProvider -> TestTree
createOpenIDConnectProviderTest = undefined

listAttachedRolePoliciesTest :: ListAttachedRolePolicies -> TestTree
listAttachedRolePoliciesTest = undefined

deleteVirtualMFADeviceTest :: DeleteVirtualMFADevice -> TestTree
deleteVirtualMFADeviceTest = undefined

getRoleTest :: GetRole -> TestTree
getRoleTest = undefined

deactivateMFADeviceTest :: DeactivateMFADevice -> TestTree
deactivateMFADeviceTest = undefined

listRolesTest :: ListRoles -> TestTree
listRolesTest = undefined

deleteRoleTest :: DeleteRole -> TestTree
deleteRoleTest = undefined

listUserPoliciesTest :: ListUserPolicies -> TestTree
listUserPoliciesTest = undefined

listUsersTest :: ListUsers -> TestTree
listUsersTest = undefined

updateOpenIDConnectProviderThumbprintTest :: UpdateOpenIDConnectProviderThumbprint -> TestTree
updateOpenIDConnectProviderThumbprintTest = undefined

putUserPolicyTest :: PutUserPolicy -> TestTree
putUserPolicyTest = undefined

createRoleTest :: CreateRole -> TestTree
createRoleTest = undefined

deleteUserPolicyTest :: DeleteUserPolicy -> TestTree
deleteUserPolicyTest = undefined

getOpenIDConnectProviderTest :: GetOpenIDConnectProvider -> TestTree
getOpenIDConnectProviderTest = undefined

detachGroupPolicyTest :: DetachGroupPolicy -> TestTree
detachGroupPolicyTest = undefined

getCredentialReportTest :: GetCredentialReport -> TestTree
getCredentialReportTest = undefined

deletePolicyVersionTest :: DeletePolicyVersion -> TestTree
deletePolicyVersionTest = undefined

detachRolePolicyTest :: DetachRolePolicy -> TestTree
detachRolePolicyTest = undefined

deleteInstanceProfileTest :: DeleteInstanceProfile -> TestTree
deleteInstanceProfileTest = undefined

listGroupPoliciesTest :: ListGroupPolicies -> TestTree
listGroupPoliciesTest = undefined

getAccountSummaryTest :: GetAccountSummary -> TestTree
getAccountSummaryTest = undefined

createInstanceProfileTest :: CreateInstanceProfile -> TestTree
createInstanceProfileTest = undefined

putGroupPolicyTest :: PutGroupPolicy -> TestTree
putGroupPolicyTest = undefined

deleteGroupPolicyTest :: DeleteGroupPolicy -> TestTree
deleteGroupPolicyTest = undefined

getAccountAuthorizationDetailsTest :: GetAccountAuthorizationDetails -> TestTree
getAccountAuthorizationDetailsTest = undefined

deleteAccountAliasTest :: DeleteAccountAlias -> TestTree
deleteAccountAliasTest = undefined

removeRoleFromInstanceProfileTest :: RemoveRoleFromInstanceProfile -> TestTree
removeRoleFromInstanceProfileTest = undefined

getLoginProfileTest :: GetLoginProfile -> TestTree
getLoginProfileTest = undefined

removeUserFromGroupTest :: RemoveUserFromGroup -> TestTree
removeUserFromGroupTest = undefined

detachUserPolicyTest :: DetachUserPolicy -> TestTree
detachUserPolicyTest = undefined

createSAMLProviderTest :: CreateSAMLProvider -> TestTree
createSAMLProviderTest = undefined

createPolicyVersionTest :: CreatePolicyVersion -> TestTree
createPolicyVersionTest = undefined

getGroupPolicyTest :: GetGroupPolicy -> TestTree
getGroupPolicyTest = undefined

deletePolicyTest :: DeletePolicy -> TestTree
deletePolicyTest = undefined

listServerCertificatesTest :: ListServerCertificates -> TestTree
listServerCertificatesTest = undefined

updateAssumeRolePolicyTest :: UpdateAssumeRolePolicy -> TestTree
updateAssumeRolePolicyTest = undefined

changePasswordTest :: ChangePassword -> TestTree
changePasswordTest = undefined

listGroupsForUserTest :: ListGroupsForUser -> TestTree
listGroupsForUserTest = undefined

getPolicyVersionTest :: GetPolicyVersion -> TestTree
getPolicyVersionTest = undefined

createLoginProfileTest :: CreateLoginProfile -> TestTree
createLoginProfileTest = undefined

getInstanceProfileTest :: GetInstanceProfile -> TestTree
getInstanceProfileTest = undefined

listEntitiesForPolicyTest :: ListEntitiesForPolicy -> TestTree
listEntitiesForPolicyTest = undefined

getSAMLProviderTest :: GetSAMLProvider -> TestTree
getSAMLProviderTest = undefined

addRoleToInstanceProfileTest :: AddRoleToInstanceProfile -> TestTree
addRoleToInstanceProfileTest = undefined

addUserToGroupTest :: AddUserToGroup -> TestTree
addUserToGroupTest = undefined

deleteOpenIDConnectProviderTest :: DeleteOpenIDConnectProvider -> TestTree
deleteOpenIDConnectProviderTest = undefined

getUserTest :: GetUser -> TestTree
getUserTest = undefined

listAttachedUserPoliciesTest :: ListAttachedUserPolicies -> TestTree
listAttachedUserPoliciesTest = undefined

deleteSigningCertificateTest :: DeleteSigningCertificate -> TestTree
deleteSigningCertificateTest = undefined

updateSigningCertificateTest :: UpdateSigningCertificate -> TestTree
updateSigningCertificateTest = undefined

listSigningCertificatesTest :: ListSigningCertificates -> TestTree
listSigningCertificatesTest = undefined

removeClientIDFromOpenIDConnectProviderTest :: RemoveClientIDFromOpenIDConnectProvider -> TestTree
removeClientIDFromOpenIDConnectProviderTest = undefined

listAccessKeysTest :: ListAccessKeys -> TestTree
listAccessKeysTest = undefined

listVirtualMFADevicesTest :: ListVirtualMFADevices -> TestTree
listVirtualMFADevicesTest = undefined

deleteAccessKeyTest :: DeleteAccessKey -> TestTree
deleteAccessKeyTest = undefined

updateAccessKeyTest :: UpdateAccessKey -> TestTree
updateAccessKeyTest = undefined

getRolePolicyTest :: GetRolePolicy -> TestTree
getRolePolicyTest = undefined

attachUserPolicyTest :: AttachUserPolicy -> TestTree
attachUserPolicyTest = undefined

resyncMFADeviceTest :: ResyncMFADevice -> TestTree
resyncMFADeviceTest = undefined

createUserTest :: CreateUser -> TestTree
createUserTest = undefined

uploadSigningCertificateTest :: UploadSigningCertificate -> TestTree
uploadSigningCertificateTest = undefined

putRolePolicyTest :: PutRolePolicy -> TestTree
putRolePolicyTest = undefined

deleteRolePolicyTest :: DeleteRolePolicy -> TestTree
deleteRolePolicyTest = undefined

updateUserTest :: UpdateUser -> TestTree
updateUserTest = undefined

deleteUserTest :: DeleteUser -> TestTree
deleteUserTest = undefined

listRolePoliciesTest :: ListRolePolicies -> TestTree
listRolePoliciesTest = undefined

addClientIDToOpenIDConnectProviderTest :: AddClientIDToOpenIDConnectProvider -> TestTree
addClientIDToOpenIDConnectProviderTest = undefined

getAccessKeyLastUsedTest :: GetAccessKeyLastUsed -> TestTree
getAccessKeyLastUsedTest = undefined

getAccountPasswordPolicyTest :: GetAccountPasswordPolicy -> TestTree
getAccountPasswordPolicyTest = undefined

listAccountAliasesTest :: ListAccountAliases -> TestTree
listAccountAliasesTest = undefined

createAccountAliasTest :: CreateAccountAlias -> TestTree
createAccountAliasTest = undefined

uploadServerCertificateTest :: UploadServerCertificate -> TestTree
uploadServerCertificateTest = undefined

listMFADevicesTest :: ListMFADevices -> TestTree
listMFADevicesTest = undefined

enableMFADeviceTest :: EnableMFADevice -> TestTree
enableMFADeviceTest = undefined

listPolicyVersionsTest :: ListPolicyVersions -> TestTree
listPolicyVersionsTest = undefined

listSAMLProvidersTest :: ListSAMLProviders -> TestTree
listSAMLProvidersTest = undefined

updateSAMLProviderTest :: UpdateSAMLProvider -> TestTree
updateSAMLProviderTest = undefined

deleteSAMLProviderTest :: DeleteSAMLProvider -> TestTree
deleteSAMLProviderTest = undefined

createGroupTest :: CreateGroup -> TestTree
createGroupTest = undefined

setDefaultPolicyVersionTest :: SetDefaultPolicyVersion -> TestTree
setDefaultPolicyVersionTest = undefined

listInstanceProfilesTest :: ListInstanceProfiles -> TestTree
listInstanceProfilesTest = undefined

listGroupsTest :: ListGroups -> TestTree
listGroupsTest = undefined

deleteGroupTest :: DeleteGroup -> TestTree
deleteGroupTest = undefined

updateGroupTest :: UpdateGroup -> TestTree
updateGroupTest = undefined

getServerCertificateTest :: GetServerCertificate -> TestTree
getServerCertificateTest = undefined

getPolicyTest :: GetPolicy -> TestTree
getPolicyTest = undefined

generateCredentialReportTest :: GenerateCredentialReport -> TestTree
generateCredentialReportTest = undefined

getGroupTest :: GetGroup -> TestTree
getGroupTest = undefined

deleteServerCertificateTest :: DeleteServerCertificate -> TestTree
deleteServerCertificateTest = undefined

updateServerCertificateTest :: UpdateServerCertificate -> TestTree
updateServerCertificateTest = undefined

deleteLoginProfileTest :: DeleteLoginProfile -> TestTree
deleteLoginProfileTest = undefined

updateLoginProfileTest :: UpdateLoginProfile -> TestTree
updateLoginProfileTest = undefined

listAttachedGroupPoliciesTest :: ListAttachedGroupPolicies -> TestTree
listAttachedGroupPoliciesTest = undefined

-- Responses

attachGroupPolicyResponseTest :: AttachGroupPolicyResponse -> TestTree
attachGroupPolicyResponseTest = resp
    "AttachGroupPolicyResponse"
    "fixture/IAM/AttachGroupPolicyResponse"
    (Proxy :: Proxy AttachGroupPolicy)

listInstanceProfilesForRoleResponseTest :: ListInstanceProfilesForRoleResponse -> TestTree
listInstanceProfilesForRoleResponseTest = resp
    "ListInstanceProfilesForRoleResponse"
    "fixture/IAM/ListInstanceProfilesForRoleResponse"
    (Proxy :: Proxy ListInstanceProfilesForRole)

createPolicyResponseTest :: CreatePolicyResponse -> TestTree
createPolicyResponseTest = resp
    "CreatePolicyResponse"
    "fixture/IAM/CreatePolicyResponse"
    (Proxy :: Proxy CreatePolicy)

listPoliciesResponseTest :: ListPoliciesResponse -> TestTree
listPoliciesResponseTest = resp
    "ListPoliciesResponse"
    "fixture/IAM/ListPoliciesResponse"
    (Proxy :: Proxy ListPolicies)

attachRolePolicyResponseTest :: AttachRolePolicyResponse -> TestTree
attachRolePolicyResponseTest = resp
    "AttachRolePolicyResponse"
    "fixture/IAM/AttachRolePolicyResponse"
    (Proxy :: Proxy AttachRolePolicy)

listOpenIDConnectProvidersResponseTest :: ListOpenIDConnectProvidersResponse -> TestTree
listOpenIDConnectProvidersResponseTest = resp
    "ListOpenIDConnectProvidersResponse"
    "fixture/IAM/ListOpenIDConnectProvidersResponse"
    (Proxy :: Proxy ListOpenIDConnectProviders)

deleteAccountPasswordPolicyResponseTest :: DeleteAccountPasswordPolicyResponse -> TestTree
deleteAccountPasswordPolicyResponseTest = resp
    "DeleteAccountPasswordPolicyResponse"
    "fixture/IAM/DeleteAccountPasswordPolicyResponse"
    (Proxy :: Proxy DeleteAccountPasswordPolicy)

updateAccountPasswordPolicyResponseTest :: UpdateAccountPasswordPolicyResponse -> TestTree
updateAccountPasswordPolicyResponseTest = resp
    "UpdateAccountPasswordPolicyResponse"
    "fixture/IAM/UpdateAccountPasswordPolicyResponse"
    (Proxy :: Proxy UpdateAccountPasswordPolicy)

createAccessKeyResponseTest :: CreateAccessKeyResponse -> TestTree
createAccessKeyResponseTest = resp
    "CreateAccessKeyResponse"
    "fixture/IAM/CreateAccessKeyResponse"
    (Proxy :: Proxy CreateAccessKey)

getUserPolicyResponseTest :: GetUserPolicyResponse -> TestTree
getUserPolicyResponseTest = resp
    "GetUserPolicyResponse"
    "fixture/IAM/GetUserPolicyResponse"
    (Proxy :: Proxy GetUserPolicy)

createVirtualMFADeviceResponseTest :: CreateVirtualMFADeviceResponse -> TestTree
createVirtualMFADeviceResponseTest = resp
    "CreateVirtualMFADeviceResponse"
    "fixture/IAM/CreateVirtualMFADeviceResponse"
    (Proxy :: Proxy CreateVirtualMFADevice)

createOpenIDConnectProviderResponseTest :: CreateOpenIDConnectProviderResponse -> TestTree
createOpenIDConnectProviderResponseTest = resp
    "CreateOpenIDConnectProviderResponse"
    "fixture/IAM/CreateOpenIDConnectProviderResponse"
    (Proxy :: Proxy CreateOpenIDConnectProvider)

listAttachedRolePoliciesResponseTest :: ListAttachedRolePoliciesResponse -> TestTree
listAttachedRolePoliciesResponseTest = resp
    "ListAttachedRolePoliciesResponse"
    "fixture/IAM/ListAttachedRolePoliciesResponse"
    (Proxy :: Proxy ListAttachedRolePolicies)

deleteVirtualMFADeviceResponseTest :: DeleteVirtualMFADeviceResponse -> TestTree
deleteVirtualMFADeviceResponseTest = resp
    "DeleteVirtualMFADeviceResponse"
    "fixture/IAM/DeleteVirtualMFADeviceResponse"
    (Proxy :: Proxy DeleteVirtualMFADevice)

getRoleResponseTest :: GetRoleResponse -> TestTree
getRoleResponseTest = resp
    "GetRoleResponse"
    "fixture/IAM/GetRoleResponse"
    (Proxy :: Proxy GetRole)

deactivateMFADeviceResponseTest :: DeactivateMFADeviceResponse -> TestTree
deactivateMFADeviceResponseTest = resp
    "DeactivateMFADeviceResponse"
    "fixture/IAM/DeactivateMFADeviceResponse"
    (Proxy :: Proxy DeactivateMFADevice)

listRolesResponseTest :: ListRolesResponse -> TestTree
listRolesResponseTest = resp
    "ListRolesResponse"
    "fixture/IAM/ListRolesResponse"
    (Proxy :: Proxy ListRoles)

deleteRoleResponseTest :: DeleteRoleResponse -> TestTree
deleteRoleResponseTest = resp
    "DeleteRoleResponse"
    "fixture/IAM/DeleteRoleResponse"
    (Proxy :: Proxy DeleteRole)

listUserPoliciesResponseTest :: ListUserPoliciesResponse -> TestTree
listUserPoliciesResponseTest = resp
    "ListUserPoliciesResponse"
    "fixture/IAM/ListUserPoliciesResponse"
    (Proxy :: Proxy ListUserPolicies)

listUsersResponseTest :: ListUsersResponse -> TestTree
listUsersResponseTest = resp
    "ListUsersResponse"
    "fixture/IAM/ListUsersResponse"
    (Proxy :: Proxy ListUsers)

updateOpenIDConnectProviderThumbprintResponseTest :: UpdateOpenIDConnectProviderThumbprintResponse -> TestTree
updateOpenIDConnectProviderThumbprintResponseTest = resp
    "UpdateOpenIDConnectProviderThumbprintResponse"
    "fixture/IAM/UpdateOpenIDConnectProviderThumbprintResponse"
    (Proxy :: Proxy UpdateOpenIDConnectProviderThumbprint)

putUserPolicyResponseTest :: PutUserPolicyResponse -> TestTree
putUserPolicyResponseTest = resp
    "PutUserPolicyResponse"
    "fixture/IAM/PutUserPolicyResponse"
    (Proxy :: Proxy PutUserPolicy)

createRoleResponseTest :: CreateRoleResponse -> TestTree
createRoleResponseTest = resp
    "CreateRoleResponse"
    "fixture/IAM/CreateRoleResponse"
    (Proxy :: Proxy CreateRole)

deleteUserPolicyResponseTest :: DeleteUserPolicyResponse -> TestTree
deleteUserPolicyResponseTest = resp
    "DeleteUserPolicyResponse"
    "fixture/IAM/DeleteUserPolicyResponse"
    (Proxy :: Proxy DeleteUserPolicy)

getOpenIDConnectProviderResponseTest :: GetOpenIDConnectProviderResponse -> TestTree
getOpenIDConnectProviderResponseTest = resp
    "GetOpenIDConnectProviderResponse"
    "fixture/IAM/GetOpenIDConnectProviderResponse"
    (Proxy :: Proxy GetOpenIDConnectProvider)

detachGroupPolicyResponseTest :: DetachGroupPolicyResponse -> TestTree
detachGroupPolicyResponseTest = resp
    "DetachGroupPolicyResponse"
    "fixture/IAM/DetachGroupPolicyResponse"
    (Proxy :: Proxy DetachGroupPolicy)

getCredentialReportResponseTest :: GetCredentialReportResponse -> TestTree
getCredentialReportResponseTest = resp
    "GetCredentialReportResponse"
    "fixture/IAM/GetCredentialReportResponse"
    (Proxy :: Proxy GetCredentialReport)

deletePolicyVersionResponseTest :: DeletePolicyVersionResponse -> TestTree
deletePolicyVersionResponseTest = resp
    "DeletePolicyVersionResponse"
    "fixture/IAM/DeletePolicyVersionResponse"
    (Proxy :: Proxy DeletePolicyVersion)

detachRolePolicyResponseTest :: DetachRolePolicyResponse -> TestTree
detachRolePolicyResponseTest = resp
    "DetachRolePolicyResponse"
    "fixture/IAM/DetachRolePolicyResponse"
    (Proxy :: Proxy DetachRolePolicy)

deleteInstanceProfileResponseTest :: DeleteInstanceProfileResponse -> TestTree
deleteInstanceProfileResponseTest = resp
    "DeleteInstanceProfileResponse"
    "fixture/IAM/DeleteInstanceProfileResponse"
    (Proxy :: Proxy DeleteInstanceProfile)

listGroupPoliciesResponseTest :: ListGroupPoliciesResponse -> TestTree
listGroupPoliciesResponseTest = resp
    "ListGroupPoliciesResponse"
    "fixture/IAM/ListGroupPoliciesResponse"
    (Proxy :: Proxy ListGroupPolicies)

getAccountSummaryResponseTest :: GetAccountSummaryResponse -> TestTree
getAccountSummaryResponseTest = resp
    "GetAccountSummaryResponse"
    "fixture/IAM/GetAccountSummaryResponse"
    (Proxy :: Proxy GetAccountSummary)

createInstanceProfileResponseTest :: CreateInstanceProfileResponse -> TestTree
createInstanceProfileResponseTest = resp
    "CreateInstanceProfileResponse"
    "fixture/IAM/CreateInstanceProfileResponse"
    (Proxy :: Proxy CreateInstanceProfile)

putGroupPolicyResponseTest :: PutGroupPolicyResponse -> TestTree
putGroupPolicyResponseTest = resp
    "PutGroupPolicyResponse"
    "fixture/IAM/PutGroupPolicyResponse"
    (Proxy :: Proxy PutGroupPolicy)

deleteGroupPolicyResponseTest :: DeleteGroupPolicyResponse -> TestTree
deleteGroupPolicyResponseTest = resp
    "DeleteGroupPolicyResponse"
    "fixture/IAM/DeleteGroupPolicyResponse"
    (Proxy :: Proxy DeleteGroupPolicy)

getAccountAuthorizationDetailsResponseTest :: GetAccountAuthorizationDetailsResponse -> TestTree
getAccountAuthorizationDetailsResponseTest = resp
    "GetAccountAuthorizationDetailsResponse"
    "fixture/IAM/GetAccountAuthorizationDetailsResponse"
    (Proxy :: Proxy GetAccountAuthorizationDetails)

deleteAccountAliasResponseTest :: DeleteAccountAliasResponse -> TestTree
deleteAccountAliasResponseTest = resp
    "DeleteAccountAliasResponse"
    "fixture/IAM/DeleteAccountAliasResponse"
    (Proxy :: Proxy DeleteAccountAlias)

removeRoleFromInstanceProfileResponseTest :: RemoveRoleFromInstanceProfileResponse -> TestTree
removeRoleFromInstanceProfileResponseTest = resp
    "RemoveRoleFromInstanceProfileResponse"
    "fixture/IAM/RemoveRoleFromInstanceProfileResponse"
    (Proxy :: Proxy RemoveRoleFromInstanceProfile)

getLoginProfileResponseTest :: GetLoginProfileResponse -> TestTree
getLoginProfileResponseTest = resp
    "GetLoginProfileResponse"
    "fixture/IAM/GetLoginProfileResponse"
    (Proxy :: Proxy GetLoginProfile)

removeUserFromGroupResponseTest :: RemoveUserFromGroupResponse -> TestTree
removeUserFromGroupResponseTest = resp
    "RemoveUserFromGroupResponse"
    "fixture/IAM/RemoveUserFromGroupResponse"
    (Proxy :: Proxy RemoveUserFromGroup)

detachUserPolicyResponseTest :: DetachUserPolicyResponse -> TestTree
detachUserPolicyResponseTest = resp
    "DetachUserPolicyResponse"
    "fixture/IAM/DetachUserPolicyResponse"
    (Proxy :: Proxy DetachUserPolicy)

createSAMLProviderResponseTest :: CreateSAMLProviderResponse -> TestTree
createSAMLProviderResponseTest = resp
    "CreateSAMLProviderResponse"
    "fixture/IAM/CreateSAMLProviderResponse"
    (Proxy :: Proxy CreateSAMLProvider)

createPolicyVersionResponseTest :: CreatePolicyVersionResponse -> TestTree
createPolicyVersionResponseTest = resp
    "CreatePolicyVersionResponse"
    "fixture/IAM/CreatePolicyVersionResponse"
    (Proxy :: Proxy CreatePolicyVersion)

getGroupPolicyResponseTest :: GetGroupPolicyResponse -> TestTree
getGroupPolicyResponseTest = resp
    "GetGroupPolicyResponse"
    "fixture/IAM/GetGroupPolicyResponse"
    (Proxy :: Proxy GetGroupPolicy)

deletePolicyResponseTest :: DeletePolicyResponse -> TestTree
deletePolicyResponseTest = resp
    "DeletePolicyResponse"
    "fixture/IAM/DeletePolicyResponse"
    (Proxy :: Proxy DeletePolicy)

listServerCertificatesResponseTest :: ListServerCertificatesResponse -> TestTree
listServerCertificatesResponseTest = resp
    "ListServerCertificatesResponse"
    "fixture/IAM/ListServerCertificatesResponse"
    (Proxy :: Proxy ListServerCertificates)

updateAssumeRolePolicyResponseTest :: UpdateAssumeRolePolicyResponse -> TestTree
updateAssumeRolePolicyResponseTest = resp
    "UpdateAssumeRolePolicyResponse"
    "fixture/IAM/UpdateAssumeRolePolicyResponse"
    (Proxy :: Proxy UpdateAssumeRolePolicy)

changePasswordResponseTest :: ChangePasswordResponse -> TestTree
changePasswordResponseTest = resp
    "ChangePasswordResponse"
    "fixture/IAM/ChangePasswordResponse"
    (Proxy :: Proxy ChangePassword)

listGroupsForUserResponseTest :: ListGroupsForUserResponse -> TestTree
listGroupsForUserResponseTest = resp
    "ListGroupsForUserResponse"
    "fixture/IAM/ListGroupsForUserResponse"
    (Proxy :: Proxy ListGroupsForUser)

getPolicyVersionResponseTest :: GetPolicyVersionResponse -> TestTree
getPolicyVersionResponseTest = resp
    "GetPolicyVersionResponse"
    "fixture/IAM/GetPolicyVersionResponse"
    (Proxy :: Proxy GetPolicyVersion)

createLoginProfileResponseTest :: CreateLoginProfileResponse -> TestTree
createLoginProfileResponseTest = resp
    "CreateLoginProfileResponse"
    "fixture/IAM/CreateLoginProfileResponse"
    (Proxy :: Proxy CreateLoginProfile)

getInstanceProfileResponseTest :: GetInstanceProfileResponse -> TestTree
getInstanceProfileResponseTest = resp
    "GetInstanceProfileResponse"
    "fixture/IAM/GetInstanceProfileResponse"
    (Proxy :: Proxy GetInstanceProfile)

listEntitiesForPolicyResponseTest :: ListEntitiesForPolicyResponse -> TestTree
listEntitiesForPolicyResponseTest = resp
    "ListEntitiesForPolicyResponse"
    "fixture/IAM/ListEntitiesForPolicyResponse"
    (Proxy :: Proxy ListEntitiesForPolicy)

getSAMLProviderResponseTest :: GetSAMLProviderResponse -> TestTree
getSAMLProviderResponseTest = resp
    "GetSAMLProviderResponse"
    "fixture/IAM/GetSAMLProviderResponse"
    (Proxy :: Proxy GetSAMLProvider)

addRoleToInstanceProfileResponseTest :: AddRoleToInstanceProfileResponse -> TestTree
addRoleToInstanceProfileResponseTest = resp
    "AddRoleToInstanceProfileResponse"
    "fixture/IAM/AddRoleToInstanceProfileResponse"
    (Proxy :: Proxy AddRoleToInstanceProfile)

addUserToGroupResponseTest :: AddUserToGroupResponse -> TestTree
addUserToGroupResponseTest = resp
    "AddUserToGroupResponse"
    "fixture/IAM/AddUserToGroupResponse"
    (Proxy :: Proxy AddUserToGroup)

deleteOpenIDConnectProviderResponseTest :: DeleteOpenIDConnectProviderResponse -> TestTree
deleteOpenIDConnectProviderResponseTest = resp
    "DeleteOpenIDConnectProviderResponse"
    "fixture/IAM/DeleteOpenIDConnectProviderResponse"
    (Proxy :: Proxy DeleteOpenIDConnectProvider)

getUserResponseTest :: GetUserResponse -> TestTree
getUserResponseTest = resp
    "GetUserResponse"
    "fixture/IAM/GetUserResponse"
    (Proxy :: Proxy GetUser)

listAttachedUserPoliciesResponseTest :: ListAttachedUserPoliciesResponse -> TestTree
listAttachedUserPoliciesResponseTest = resp
    "ListAttachedUserPoliciesResponse"
    "fixture/IAM/ListAttachedUserPoliciesResponse"
    (Proxy :: Proxy ListAttachedUserPolicies)

deleteSigningCertificateResponseTest :: DeleteSigningCertificateResponse -> TestTree
deleteSigningCertificateResponseTest = resp
    "DeleteSigningCertificateResponse"
    "fixture/IAM/DeleteSigningCertificateResponse"
    (Proxy :: Proxy DeleteSigningCertificate)

updateSigningCertificateResponseTest :: UpdateSigningCertificateResponse -> TestTree
updateSigningCertificateResponseTest = resp
    "UpdateSigningCertificateResponse"
    "fixture/IAM/UpdateSigningCertificateResponse"
    (Proxy :: Proxy UpdateSigningCertificate)

listSigningCertificatesResponseTest :: ListSigningCertificatesResponse -> TestTree
listSigningCertificatesResponseTest = resp
    "ListSigningCertificatesResponse"
    "fixture/IAM/ListSigningCertificatesResponse"
    (Proxy :: Proxy ListSigningCertificates)

removeClientIDFromOpenIDConnectProviderResponseTest :: RemoveClientIDFromOpenIDConnectProviderResponse -> TestTree
removeClientIDFromOpenIDConnectProviderResponseTest = resp
    "RemoveClientIDFromOpenIDConnectProviderResponse"
    "fixture/IAM/RemoveClientIDFromOpenIDConnectProviderResponse"
    (Proxy :: Proxy RemoveClientIDFromOpenIDConnectProvider)

listAccessKeysResponseTest :: ListAccessKeysResponse -> TestTree
listAccessKeysResponseTest = resp
    "ListAccessKeysResponse"
    "fixture/IAM/ListAccessKeysResponse"
    (Proxy :: Proxy ListAccessKeys)

listVirtualMFADevicesResponseTest :: ListVirtualMFADevicesResponse -> TestTree
listVirtualMFADevicesResponseTest = resp
    "ListVirtualMFADevicesResponse"
    "fixture/IAM/ListVirtualMFADevicesResponse"
    (Proxy :: Proxy ListVirtualMFADevices)

deleteAccessKeyResponseTest :: DeleteAccessKeyResponse -> TestTree
deleteAccessKeyResponseTest = resp
    "DeleteAccessKeyResponse"
    "fixture/IAM/DeleteAccessKeyResponse"
    (Proxy :: Proxy DeleteAccessKey)

updateAccessKeyResponseTest :: UpdateAccessKeyResponse -> TestTree
updateAccessKeyResponseTest = resp
    "UpdateAccessKeyResponse"
    "fixture/IAM/UpdateAccessKeyResponse"
    (Proxy :: Proxy UpdateAccessKey)

getRolePolicyResponseTest :: GetRolePolicyResponse -> TestTree
getRolePolicyResponseTest = resp
    "GetRolePolicyResponse"
    "fixture/IAM/GetRolePolicyResponse"
    (Proxy :: Proxy GetRolePolicy)

attachUserPolicyResponseTest :: AttachUserPolicyResponse -> TestTree
attachUserPolicyResponseTest = resp
    "AttachUserPolicyResponse"
    "fixture/IAM/AttachUserPolicyResponse"
    (Proxy :: Proxy AttachUserPolicy)

resyncMFADeviceResponseTest :: ResyncMFADeviceResponse -> TestTree
resyncMFADeviceResponseTest = resp
    "ResyncMFADeviceResponse"
    "fixture/IAM/ResyncMFADeviceResponse"
    (Proxy :: Proxy ResyncMFADevice)

createUserResponseTest :: CreateUserResponse -> TestTree
createUserResponseTest = resp
    "CreateUserResponse"
    "fixture/IAM/CreateUserResponse"
    (Proxy :: Proxy CreateUser)

uploadSigningCertificateResponseTest :: UploadSigningCertificateResponse -> TestTree
uploadSigningCertificateResponseTest = resp
    "UploadSigningCertificateResponse"
    "fixture/IAM/UploadSigningCertificateResponse"
    (Proxy :: Proxy UploadSigningCertificate)

putRolePolicyResponseTest :: PutRolePolicyResponse -> TestTree
putRolePolicyResponseTest = resp
    "PutRolePolicyResponse"
    "fixture/IAM/PutRolePolicyResponse"
    (Proxy :: Proxy PutRolePolicy)

deleteRolePolicyResponseTest :: DeleteRolePolicyResponse -> TestTree
deleteRolePolicyResponseTest = resp
    "DeleteRolePolicyResponse"
    "fixture/IAM/DeleteRolePolicyResponse"
    (Proxy :: Proxy DeleteRolePolicy)

updateUserResponseTest :: UpdateUserResponse -> TestTree
updateUserResponseTest = resp
    "UpdateUserResponse"
    "fixture/IAM/UpdateUserResponse"
    (Proxy :: Proxy UpdateUser)

deleteUserResponseTest :: DeleteUserResponse -> TestTree
deleteUserResponseTest = resp
    "DeleteUserResponse"
    "fixture/IAM/DeleteUserResponse"
    (Proxy :: Proxy DeleteUser)

listRolePoliciesResponseTest :: ListRolePoliciesResponse -> TestTree
listRolePoliciesResponseTest = resp
    "ListRolePoliciesResponse"
    "fixture/IAM/ListRolePoliciesResponse"
    (Proxy :: Proxy ListRolePolicies)

addClientIDToOpenIDConnectProviderResponseTest :: AddClientIDToOpenIDConnectProviderResponse -> TestTree
addClientIDToOpenIDConnectProviderResponseTest = resp
    "AddClientIDToOpenIDConnectProviderResponse"
    "fixture/IAM/AddClientIDToOpenIDConnectProviderResponse"
    (Proxy :: Proxy AddClientIDToOpenIDConnectProvider)

getAccessKeyLastUsedResponseTest :: GetAccessKeyLastUsedResponse -> TestTree
getAccessKeyLastUsedResponseTest = resp
    "GetAccessKeyLastUsedResponse"
    "fixture/IAM/GetAccessKeyLastUsedResponse"
    (Proxy :: Proxy GetAccessKeyLastUsed)

getAccountPasswordPolicyResponseTest :: GetAccountPasswordPolicyResponse -> TestTree
getAccountPasswordPolicyResponseTest = resp
    "GetAccountPasswordPolicyResponse"
    "fixture/IAM/GetAccountPasswordPolicyResponse"
    (Proxy :: Proxy GetAccountPasswordPolicy)

listAccountAliasesResponseTest :: ListAccountAliasesResponse -> TestTree
listAccountAliasesResponseTest = resp
    "ListAccountAliasesResponse"
    "fixture/IAM/ListAccountAliasesResponse"
    (Proxy :: Proxy ListAccountAliases)

createAccountAliasResponseTest :: CreateAccountAliasResponse -> TestTree
createAccountAliasResponseTest = resp
    "CreateAccountAliasResponse"
    "fixture/IAM/CreateAccountAliasResponse"
    (Proxy :: Proxy CreateAccountAlias)

uploadServerCertificateResponseTest :: UploadServerCertificateResponse -> TestTree
uploadServerCertificateResponseTest = resp
    "UploadServerCertificateResponse"
    "fixture/IAM/UploadServerCertificateResponse"
    (Proxy :: Proxy UploadServerCertificate)

listMFADevicesResponseTest :: ListMFADevicesResponse -> TestTree
listMFADevicesResponseTest = resp
    "ListMFADevicesResponse"
    "fixture/IAM/ListMFADevicesResponse"
    (Proxy :: Proxy ListMFADevices)

enableMFADeviceResponseTest :: EnableMFADeviceResponse -> TestTree
enableMFADeviceResponseTest = resp
    "EnableMFADeviceResponse"
    "fixture/IAM/EnableMFADeviceResponse"
    (Proxy :: Proxy EnableMFADevice)

listPolicyVersionsResponseTest :: ListPolicyVersionsResponse -> TestTree
listPolicyVersionsResponseTest = resp
    "ListPolicyVersionsResponse"
    "fixture/IAM/ListPolicyVersionsResponse"
    (Proxy :: Proxy ListPolicyVersions)

listSAMLProvidersResponseTest :: ListSAMLProvidersResponse -> TestTree
listSAMLProvidersResponseTest = resp
    "ListSAMLProvidersResponse"
    "fixture/IAM/ListSAMLProvidersResponse"
    (Proxy :: Proxy ListSAMLProviders)

updateSAMLProviderResponseTest :: UpdateSAMLProviderResponse -> TestTree
updateSAMLProviderResponseTest = resp
    "UpdateSAMLProviderResponse"
    "fixture/IAM/UpdateSAMLProviderResponse"
    (Proxy :: Proxy UpdateSAMLProvider)

deleteSAMLProviderResponseTest :: DeleteSAMLProviderResponse -> TestTree
deleteSAMLProviderResponseTest = resp
    "DeleteSAMLProviderResponse"
    "fixture/IAM/DeleteSAMLProviderResponse"
    (Proxy :: Proxy DeleteSAMLProvider)

createGroupResponseTest :: CreateGroupResponse -> TestTree
createGroupResponseTest = resp
    "CreateGroupResponse"
    "fixture/IAM/CreateGroupResponse"
    (Proxy :: Proxy CreateGroup)

setDefaultPolicyVersionResponseTest :: SetDefaultPolicyVersionResponse -> TestTree
setDefaultPolicyVersionResponseTest = resp
    "SetDefaultPolicyVersionResponse"
    "fixture/IAM/SetDefaultPolicyVersionResponse"
    (Proxy :: Proxy SetDefaultPolicyVersion)

listInstanceProfilesResponseTest :: ListInstanceProfilesResponse -> TestTree
listInstanceProfilesResponseTest = resp
    "ListInstanceProfilesResponse"
    "fixture/IAM/ListInstanceProfilesResponse"
    (Proxy :: Proxy ListInstanceProfiles)

listGroupsResponseTest :: ListGroupsResponse -> TestTree
listGroupsResponseTest = resp
    "ListGroupsResponse"
    "fixture/IAM/ListGroupsResponse"
    (Proxy :: Proxy ListGroups)

deleteGroupResponseTest :: DeleteGroupResponse -> TestTree
deleteGroupResponseTest = resp
    "DeleteGroupResponse"
    "fixture/IAM/DeleteGroupResponse"
    (Proxy :: Proxy DeleteGroup)

updateGroupResponseTest :: UpdateGroupResponse -> TestTree
updateGroupResponseTest = resp
    "UpdateGroupResponse"
    "fixture/IAM/UpdateGroupResponse"
    (Proxy :: Proxy UpdateGroup)

getServerCertificateResponseTest :: GetServerCertificateResponse -> TestTree
getServerCertificateResponseTest = resp
    "GetServerCertificateResponse"
    "fixture/IAM/GetServerCertificateResponse"
    (Proxy :: Proxy GetServerCertificate)

getPolicyResponseTest :: GetPolicyResponse -> TestTree
getPolicyResponseTest = resp
    "GetPolicyResponse"
    "fixture/IAM/GetPolicyResponse"
    (Proxy :: Proxy GetPolicy)

generateCredentialReportResponseTest :: GenerateCredentialReportResponse -> TestTree
generateCredentialReportResponseTest = resp
    "GenerateCredentialReportResponse"
    "fixture/IAM/GenerateCredentialReportResponse"
    (Proxy :: Proxy GenerateCredentialReport)

getGroupResponseTest :: GetGroupResponse -> TestTree
getGroupResponseTest = resp
    "GetGroupResponse"
    "fixture/IAM/GetGroupResponse"
    (Proxy :: Proxy GetGroup)

deleteServerCertificateResponseTest :: DeleteServerCertificateResponse -> TestTree
deleteServerCertificateResponseTest = resp
    "DeleteServerCertificateResponse"
    "fixture/IAM/DeleteServerCertificateResponse"
    (Proxy :: Proxy DeleteServerCertificate)

updateServerCertificateResponseTest :: UpdateServerCertificateResponse -> TestTree
updateServerCertificateResponseTest = resp
    "UpdateServerCertificateResponse"
    "fixture/IAM/UpdateServerCertificateResponse"
    (Proxy :: Proxy UpdateServerCertificate)

deleteLoginProfileResponseTest :: DeleteLoginProfileResponse -> TestTree
deleteLoginProfileResponseTest = resp
    "DeleteLoginProfileResponse"
    "fixture/IAM/DeleteLoginProfileResponse"
    (Proxy :: Proxy DeleteLoginProfile)

updateLoginProfileResponseTest :: UpdateLoginProfileResponse -> TestTree
updateLoginProfileResponseTest = resp
    "UpdateLoginProfileResponse"
    "fixture/IAM/UpdateLoginProfileResponse"
    (Proxy :: Proxy UpdateLoginProfile)

listAttachedGroupPoliciesResponseTest :: ListAttachedGroupPoliciesResponse -> TestTree
listAttachedGroupPoliciesResponseTest = resp
    "ListAttachedGroupPoliciesResponse"
    "fixture/IAM/ListAttachedGroupPoliciesResponse"
    (Proxy :: Proxy ListAttachedGroupPolicies)
