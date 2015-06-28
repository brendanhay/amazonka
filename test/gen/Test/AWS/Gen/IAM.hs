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
    "AttachGroupPolicy"
    "fixture/IAM/AttachGroupPolicyResponse"
    (Proxy :: Proxy AttachGroupPolicy)

listInstanceProfilesForRoleResponseTest :: ListInstanceProfilesForRoleResponse -> TestTree
listInstanceProfilesForRoleResponseTest = resp
    "ListInstanceProfilesForRole"
    "fixture/IAM/ListInstanceProfilesForRoleResponse"
    (Proxy :: Proxy ListInstanceProfilesForRole)

createPolicyResponseTest :: CreatePolicyResponse -> TestTree
createPolicyResponseTest = resp
    "CreatePolicy"
    "fixture/IAM/CreatePolicyResponse"
    (Proxy :: Proxy CreatePolicy)

listPoliciesResponseTest :: ListPoliciesResponse -> TestTree
listPoliciesResponseTest = resp
    "ListPolicies"
    "fixture/IAM/ListPoliciesResponse"
    (Proxy :: Proxy ListPolicies)

attachRolePolicyResponseTest :: AttachRolePolicyResponse -> TestTree
attachRolePolicyResponseTest = resp
    "AttachRolePolicy"
    "fixture/IAM/AttachRolePolicyResponse"
    (Proxy :: Proxy AttachRolePolicy)

listOpenIDConnectProvidersResponseTest :: ListOpenIDConnectProvidersResponse -> TestTree
listOpenIDConnectProvidersResponseTest = resp
    "ListOpenIDConnectProviders"
    "fixture/IAM/ListOpenIDConnectProvidersResponse"
    (Proxy :: Proxy ListOpenIDConnectProviders)

deleteAccountPasswordPolicyResponseTest :: DeleteAccountPasswordPolicyResponse -> TestTree
deleteAccountPasswordPolicyResponseTest = resp
    "DeleteAccountPasswordPolicy"
    "fixture/IAM/DeleteAccountPasswordPolicyResponse"
    (Proxy :: Proxy DeleteAccountPasswordPolicy)

updateAccountPasswordPolicyResponseTest :: UpdateAccountPasswordPolicyResponse -> TestTree
updateAccountPasswordPolicyResponseTest = resp
    "UpdateAccountPasswordPolicy"
    "fixture/IAM/UpdateAccountPasswordPolicyResponse"
    (Proxy :: Proxy UpdateAccountPasswordPolicy)

createAccessKeyResponseTest :: CreateAccessKeyResponse -> TestTree
createAccessKeyResponseTest = resp
    "CreateAccessKey"
    "fixture/IAM/CreateAccessKeyResponse"
    (Proxy :: Proxy CreateAccessKey)

getUserPolicyResponseTest :: GetUserPolicyResponse -> TestTree
getUserPolicyResponseTest = resp
    "GetUserPolicy"
    "fixture/IAM/GetUserPolicyResponse"
    (Proxy :: Proxy GetUserPolicy)

createVirtualMFADeviceResponseTest :: CreateVirtualMFADeviceResponse -> TestTree
createVirtualMFADeviceResponseTest = resp
    "CreateVirtualMFADevice"
    "fixture/IAM/CreateVirtualMFADeviceResponse"
    (Proxy :: Proxy CreateVirtualMFADevice)

createOpenIDConnectProviderResponseTest :: CreateOpenIDConnectProviderResponse -> TestTree
createOpenIDConnectProviderResponseTest = resp
    "CreateOpenIDConnectProvider"
    "fixture/IAM/CreateOpenIDConnectProviderResponse"
    (Proxy :: Proxy CreateOpenIDConnectProvider)

listAttachedRolePoliciesResponseTest :: ListAttachedRolePoliciesResponse -> TestTree
listAttachedRolePoliciesResponseTest = resp
    "ListAttachedRolePolicies"
    "fixture/IAM/ListAttachedRolePoliciesResponse"
    (Proxy :: Proxy ListAttachedRolePolicies)

deleteVirtualMFADeviceResponseTest :: DeleteVirtualMFADeviceResponse -> TestTree
deleteVirtualMFADeviceResponseTest = resp
    "DeleteVirtualMFADevice"
    "fixture/IAM/DeleteVirtualMFADeviceResponse"
    (Proxy :: Proxy DeleteVirtualMFADevice)

getRoleResponseTest :: GetRoleResponse -> TestTree
getRoleResponseTest = resp
    "GetRole"
    "fixture/IAM/GetRoleResponse"
    (Proxy :: Proxy GetRole)

deactivateMFADeviceResponseTest :: DeactivateMFADeviceResponse -> TestTree
deactivateMFADeviceResponseTest = resp
    "DeactivateMFADevice"
    "fixture/IAM/DeactivateMFADeviceResponse"
    (Proxy :: Proxy DeactivateMFADevice)

listRolesResponseTest :: ListRolesResponse -> TestTree
listRolesResponseTest = resp
    "ListRoles"
    "fixture/IAM/ListRolesResponse"
    (Proxy :: Proxy ListRoles)

deleteRoleResponseTest :: DeleteRoleResponse -> TestTree
deleteRoleResponseTest = resp
    "DeleteRole"
    "fixture/IAM/DeleteRoleResponse"
    (Proxy :: Proxy DeleteRole)

listUserPoliciesResponseTest :: ListUserPoliciesResponse -> TestTree
listUserPoliciesResponseTest = resp
    "ListUserPolicies"
    "fixture/IAM/ListUserPoliciesResponse"
    (Proxy :: Proxy ListUserPolicies)

listUsersResponseTest :: ListUsersResponse -> TestTree
listUsersResponseTest = resp
    "ListUsers"
    "fixture/IAM/ListUsersResponse"
    (Proxy :: Proxy ListUsers)

updateOpenIDConnectProviderThumbprintResponseTest :: UpdateOpenIDConnectProviderThumbprintResponse -> TestTree
updateOpenIDConnectProviderThumbprintResponseTest = resp
    "UpdateOpenIDConnectProviderThumbprint"
    "fixture/IAM/UpdateOpenIDConnectProviderThumbprintResponse"
    (Proxy :: Proxy UpdateOpenIDConnectProviderThumbprint)

putUserPolicyResponseTest :: PutUserPolicyResponse -> TestTree
putUserPolicyResponseTest = resp
    "PutUserPolicy"
    "fixture/IAM/PutUserPolicyResponse"
    (Proxy :: Proxy PutUserPolicy)

createRoleResponseTest :: CreateRoleResponse -> TestTree
createRoleResponseTest = resp
    "CreateRole"
    "fixture/IAM/CreateRoleResponse"
    (Proxy :: Proxy CreateRole)

deleteUserPolicyResponseTest :: DeleteUserPolicyResponse -> TestTree
deleteUserPolicyResponseTest = resp
    "DeleteUserPolicy"
    "fixture/IAM/DeleteUserPolicyResponse"
    (Proxy :: Proxy DeleteUserPolicy)

getOpenIDConnectProviderResponseTest :: GetOpenIDConnectProviderResponse -> TestTree
getOpenIDConnectProviderResponseTest = resp
    "GetOpenIDConnectProvider"
    "fixture/IAM/GetOpenIDConnectProviderResponse"
    (Proxy :: Proxy GetOpenIDConnectProvider)

detachGroupPolicyResponseTest :: DetachGroupPolicyResponse -> TestTree
detachGroupPolicyResponseTest = resp
    "DetachGroupPolicy"
    "fixture/IAM/DetachGroupPolicyResponse"
    (Proxy :: Proxy DetachGroupPolicy)

getCredentialReportResponseTest :: GetCredentialReportResponse -> TestTree
getCredentialReportResponseTest = resp
    "GetCredentialReport"
    "fixture/IAM/GetCredentialReportResponse"
    (Proxy :: Proxy GetCredentialReport)

deletePolicyVersionResponseTest :: DeletePolicyVersionResponse -> TestTree
deletePolicyVersionResponseTest = resp
    "DeletePolicyVersion"
    "fixture/IAM/DeletePolicyVersionResponse"
    (Proxy :: Proxy DeletePolicyVersion)

detachRolePolicyResponseTest :: DetachRolePolicyResponse -> TestTree
detachRolePolicyResponseTest = resp
    "DetachRolePolicy"
    "fixture/IAM/DetachRolePolicyResponse"
    (Proxy :: Proxy DetachRolePolicy)

deleteInstanceProfileResponseTest :: DeleteInstanceProfileResponse -> TestTree
deleteInstanceProfileResponseTest = resp
    "DeleteInstanceProfile"
    "fixture/IAM/DeleteInstanceProfileResponse"
    (Proxy :: Proxy DeleteInstanceProfile)

listGroupPoliciesResponseTest :: ListGroupPoliciesResponse -> TestTree
listGroupPoliciesResponseTest = resp
    "ListGroupPolicies"
    "fixture/IAM/ListGroupPoliciesResponse"
    (Proxy :: Proxy ListGroupPolicies)

getAccountSummaryResponseTest :: GetAccountSummaryResponse -> TestTree
getAccountSummaryResponseTest = resp
    "GetAccountSummary"
    "fixture/IAM/GetAccountSummaryResponse"
    (Proxy :: Proxy GetAccountSummary)

createInstanceProfileResponseTest :: CreateInstanceProfileResponse -> TestTree
createInstanceProfileResponseTest = resp
    "CreateInstanceProfile"
    "fixture/IAM/CreateInstanceProfileResponse"
    (Proxy :: Proxy CreateInstanceProfile)

putGroupPolicyResponseTest :: PutGroupPolicyResponse -> TestTree
putGroupPolicyResponseTest = resp
    "PutGroupPolicy"
    "fixture/IAM/PutGroupPolicyResponse"
    (Proxy :: Proxy PutGroupPolicy)

deleteGroupPolicyResponseTest :: DeleteGroupPolicyResponse -> TestTree
deleteGroupPolicyResponseTest = resp
    "DeleteGroupPolicy"
    "fixture/IAM/DeleteGroupPolicyResponse"
    (Proxy :: Proxy DeleteGroupPolicy)

getAccountAuthorizationDetailsResponseTest :: GetAccountAuthorizationDetailsResponse -> TestTree
getAccountAuthorizationDetailsResponseTest = resp
    "GetAccountAuthorizationDetails"
    "fixture/IAM/GetAccountAuthorizationDetailsResponse"
    (Proxy :: Proxy GetAccountAuthorizationDetails)

deleteAccountAliasResponseTest :: DeleteAccountAliasResponse -> TestTree
deleteAccountAliasResponseTest = resp
    "DeleteAccountAlias"
    "fixture/IAM/DeleteAccountAliasResponse"
    (Proxy :: Proxy DeleteAccountAlias)

removeRoleFromInstanceProfileResponseTest :: RemoveRoleFromInstanceProfileResponse -> TestTree
removeRoleFromInstanceProfileResponseTest = resp
    "RemoveRoleFromInstanceProfile"
    "fixture/IAM/RemoveRoleFromInstanceProfileResponse"
    (Proxy :: Proxy RemoveRoleFromInstanceProfile)

getLoginProfileResponseTest :: GetLoginProfileResponse -> TestTree
getLoginProfileResponseTest = resp
    "GetLoginProfile"
    "fixture/IAM/GetLoginProfileResponse"
    (Proxy :: Proxy GetLoginProfile)

removeUserFromGroupResponseTest :: RemoveUserFromGroupResponse -> TestTree
removeUserFromGroupResponseTest = resp
    "RemoveUserFromGroup"
    "fixture/IAM/RemoveUserFromGroupResponse"
    (Proxy :: Proxy RemoveUserFromGroup)

detachUserPolicyResponseTest :: DetachUserPolicyResponse -> TestTree
detachUserPolicyResponseTest = resp
    "DetachUserPolicy"
    "fixture/IAM/DetachUserPolicyResponse"
    (Proxy :: Proxy DetachUserPolicy)

createSAMLProviderResponseTest :: CreateSAMLProviderResponse -> TestTree
createSAMLProviderResponseTest = resp
    "CreateSAMLProvider"
    "fixture/IAM/CreateSAMLProviderResponse"
    (Proxy :: Proxy CreateSAMLProvider)

createPolicyVersionResponseTest :: CreatePolicyVersionResponse -> TestTree
createPolicyVersionResponseTest = resp
    "CreatePolicyVersion"
    "fixture/IAM/CreatePolicyVersionResponse"
    (Proxy :: Proxy CreatePolicyVersion)

getGroupPolicyResponseTest :: GetGroupPolicyResponse -> TestTree
getGroupPolicyResponseTest = resp
    "GetGroupPolicy"
    "fixture/IAM/GetGroupPolicyResponse"
    (Proxy :: Proxy GetGroupPolicy)

deletePolicyResponseTest :: DeletePolicyResponse -> TestTree
deletePolicyResponseTest = resp
    "DeletePolicy"
    "fixture/IAM/DeletePolicyResponse"
    (Proxy :: Proxy DeletePolicy)

listServerCertificatesResponseTest :: ListServerCertificatesResponse -> TestTree
listServerCertificatesResponseTest = resp
    "ListServerCertificates"
    "fixture/IAM/ListServerCertificatesResponse"
    (Proxy :: Proxy ListServerCertificates)

updateAssumeRolePolicyResponseTest :: UpdateAssumeRolePolicyResponse -> TestTree
updateAssumeRolePolicyResponseTest = resp
    "UpdateAssumeRolePolicy"
    "fixture/IAM/UpdateAssumeRolePolicyResponse"
    (Proxy :: Proxy UpdateAssumeRolePolicy)

changePasswordResponseTest :: ChangePasswordResponse -> TestTree
changePasswordResponseTest = resp
    "ChangePassword"
    "fixture/IAM/ChangePasswordResponse"
    (Proxy :: Proxy ChangePassword)

listGroupsForUserResponseTest :: ListGroupsForUserResponse -> TestTree
listGroupsForUserResponseTest = resp
    "ListGroupsForUser"
    "fixture/IAM/ListGroupsForUserResponse"
    (Proxy :: Proxy ListGroupsForUser)

getPolicyVersionResponseTest :: GetPolicyVersionResponse -> TestTree
getPolicyVersionResponseTest = resp
    "GetPolicyVersion"
    "fixture/IAM/GetPolicyVersionResponse"
    (Proxy :: Proxy GetPolicyVersion)

createLoginProfileResponseTest :: CreateLoginProfileResponse -> TestTree
createLoginProfileResponseTest = resp
    "CreateLoginProfile"
    "fixture/IAM/CreateLoginProfileResponse"
    (Proxy :: Proxy CreateLoginProfile)

getInstanceProfileResponseTest :: GetInstanceProfileResponse -> TestTree
getInstanceProfileResponseTest = resp
    "GetInstanceProfile"
    "fixture/IAM/GetInstanceProfileResponse"
    (Proxy :: Proxy GetInstanceProfile)

listEntitiesForPolicyResponseTest :: ListEntitiesForPolicyResponse -> TestTree
listEntitiesForPolicyResponseTest = resp
    "ListEntitiesForPolicy"
    "fixture/IAM/ListEntitiesForPolicyResponse"
    (Proxy :: Proxy ListEntitiesForPolicy)

getSAMLProviderResponseTest :: GetSAMLProviderResponse -> TestTree
getSAMLProviderResponseTest = resp
    "GetSAMLProvider"
    "fixture/IAM/GetSAMLProviderResponse"
    (Proxy :: Proxy GetSAMLProvider)

addRoleToInstanceProfileResponseTest :: AddRoleToInstanceProfileResponse -> TestTree
addRoleToInstanceProfileResponseTest = resp
    "AddRoleToInstanceProfile"
    "fixture/IAM/AddRoleToInstanceProfileResponse"
    (Proxy :: Proxy AddRoleToInstanceProfile)

addUserToGroupResponseTest :: AddUserToGroupResponse -> TestTree
addUserToGroupResponseTest = resp
    "AddUserToGroup"
    "fixture/IAM/AddUserToGroupResponse"
    (Proxy :: Proxy AddUserToGroup)

deleteOpenIDConnectProviderResponseTest :: DeleteOpenIDConnectProviderResponse -> TestTree
deleteOpenIDConnectProviderResponseTest = resp
    "DeleteOpenIDConnectProvider"
    "fixture/IAM/DeleteOpenIDConnectProviderResponse"
    (Proxy :: Proxy DeleteOpenIDConnectProvider)

getUserResponseTest :: GetUserResponse -> TestTree
getUserResponseTest = resp
    "GetUser"
    "fixture/IAM/GetUserResponse"
    (Proxy :: Proxy GetUser)

listAttachedUserPoliciesResponseTest :: ListAttachedUserPoliciesResponse -> TestTree
listAttachedUserPoliciesResponseTest = resp
    "ListAttachedUserPolicies"
    "fixture/IAM/ListAttachedUserPoliciesResponse"
    (Proxy :: Proxy ListAttachedUserPolicies)

deleteSigningCertificateResponseTest :: DeleteSigningCertificateResponse -> TestTree
deleteSigningCertificateResponseTest = resp
    "DeleteSigningCertificate"
    "fixture/IAM/DeleteSigningCertificateResponse"
    (Proxy :: Proxy DeleteSigningCertificate)

updateSigningCertificateResponseTest :: UpdateSigningCertificateResponse -> TestTree
updateSigningCertificateResponseTest = resp
    "UpdateSigningCertificate"
    "fixture/IAM/UpdateSigningCertificateResponse"
    (Proxy :: Proxy UpdateSigningCertificate)

listSigningCertificatesResponseTest :: ListSigningCertificatesResponse -> TestTree
listSigningCertificatesResponseTest = resp
    "ListSigningCertificates"
    "fixture/IAM/ListSigningCertificatesResponse"
    (Proxy :: Proxy ListSigningCertificates)

removeClientIDFromOpenIDConnectProviderResponseTest :: RemoveClientIDFromOpenIDConnectProviderResponse -> TestTree
removeClientIDFromOpenIDConnectProviderResponseTest = resp
    "RemoveClientIDFromOpenIDConnectProvider"
    "fixture/IAM/RemoveClientIDFromOpenIDConnectProviderResponse"
    (Proxy :: Proxy RemoveClientIDFromOpenIDConnectProvider)

listAccessKeysResponseTest :: ListAccessKeysResponse -> TestTree
listAccessKeysResponseTest = resp
    "ListAccessKeys"
    "fixture/IAM/ListAccessKeysResponse"
    (Proxy :: Proxy ListAccessKeys)

listVirtualMFADevicesResponseTest :: ListVirtualMFADevicesResponse -> TestTree
listVirtualMFADevicesResponseTest = resp
    "ListVirtualMFADevices"
    "fixture/IAM/ListVirtualMFADevicesResponse"
    (Proxy :: Proxy ListVirtualMFADevices)

deleteAccessKeyResponseTest :: DeleteAccessKeyResponse -> TestTree
deleteAccessKeyResponseTest = resp
    "DeleteAccessKey"
    "fixture/IAM/DeleteAccessKeyResponse"
    (Proxy :: Proxy DeleteAccessKey)

updateAccessKeyResponseTest :: UpdateAccessKeyResponse -> TestTree
updateAccessKeyResponseTest = resp
    "UpdateAccessKey"
    "fixture/IAM/UpdateAccessKeyResponse"
    (Proxy :: Proxy UpdateAccessKey)

getRolePolicyResponseTest :: GetRolePolicyResponse -> TestTree
getRolePolicyResponseTest = resp
    "GetRolePolicy"
    "fixture/IAM/GetRolePolicyResponse"
    (Proxy :: Proxy GetRolePolicy)

attachUserPolicyResponseTest :: AttachUserPolicyResponse -> TestTree
attachUserPolicyResponseTest = resp
    "AttachUserPolicy"
    "fixture/IAM/AttachUserPolicyResponse"
    (Proxy :: Proxy AttachUserPolicy)

resyncMFADeviceResponseTest :: ResyncMFADeviceResponse -> TestTree
resyncMFADeviceResponseTest = resp
    "ResyncMFADevice"
    "fixture/IAM/ResyncMFADeviceResponse"
    (Proxy :: Proxy ResyncMFADevice)

createUserResponseTest :: CreateUserResponse -> TestTree
createUserResponseTest = resp
    "CreateUser"
    "fixture/IAM/CreateUserResponse"
    (Proxy :: Proxy CreateUser)

uploadSigningCertificateResponseTest :: UploadSigningCertificateResponse -> TestTree
uploadSigningCertificateResponseTest = resp
    "UploadSigningCertificate"
    "fixture/IAM/UploadSigningCertificateResponse"
    (Proxy :: Proxy UploadSigningCertificate)

putRolePolicyResponseTest :: PutRolePolicyResponse -> TestTree
putRolePolicyResponseTest = resp
    "PutRolePolicy"
    "fixture/IAM/PutRolePolicyResponse"
    (Proxy :: Proxy PutRolePolicy)

deleteRolePolicyResponseTest :: DeleteRolePolicyResponse -> TestTree
deleteRolePolicyResponseTest = resp
    "DeleteRolePolicy"
    "fixture/IAM/DeleteRolePolicyResponse"
    (Proxy :: Proxy DeleteRolePolicy)

updateUserResponseTest :: UpdateUserResponse -> TestTree
updateUserResponseTest = resp
    "UpdateUser"
    "fixture/IAM/UpdateUserResponse"
    (Proxy :: Proxy UpdateUser)

deleteUserResponseTest :: DeleteUserResponse -> TestTree
deleteUserResponseTest = resp
    "DeleteUser"
    "fixture/IAM/DeleteUserResponse"
    (Proxy :: Proxy DeleteUser)

listRolePoliciesResponseTest :: ListRolePoliciesResponse -> TestTree
listRolePoliciesResponseTest = resp
    "ListRolePolicies"
    "fixture/IAM/ListRolePoliciesResponse"
    (Proxy :: Proxy ListRolePolicies)

addClientIDToOpenIDConnectProviderResponseTest :: AddClientIDToOpenIDConnectProviderResponse -> TestTree
addClientIDToOpenIDConnectProviderResponseTest = resp
    "AddClientIDToOpenIDConnectProvider"
    "fixture/IAM/AddClientIDToOpenIDConnectProviderResponse"
    (Proxy :: Proxy AddClientIDToOpenIDConnectProvider)

getAccessKeyLastUsedResponseTest :: GetAccessKeyLastUsedResponse -> TestTree
getAccessKeyLastUsedResponseTest = resp
    "GetAccessKeyLastUsed"
    "fixture/IAM/GetAccessKeyLastUsedResponse"
    (Proxy :: Proxy GetAccessKeyLastUsed)

getAccountPasswordPolicyResponseTest :: GetAccountPasswordPolicyResponse -> TestTree
getAccountPasswordPolicyResponseTest = resp
    "GetAccountPasswordPolicy"
    "fixture/IAM/GetAccountPasswordPolicyResponse"
    (Proxy :: Proxy GetAccountPasswordPolicy)

listAccountAliasesResponseTest :: ListAccountAliasesResponse -> TestTree
listAccountAliasesResponseTest = resp
    "ListAccountAliases"
    "fixture/IAM/ListAccountAliasesResponse"
    (Proxy :: Proxy ListAccountAliases)

createAccountAliasResponseTest :: CreateAccountAliasResponse -> TestTree
createAccountAliasResponseTest = resp
    "CreateAccountAlias"
    "fixture/IAM/CreateAccountAliasResponse"
    (Proxy :: Proxy CreateAccountAlias)

uploadServerCertificateResponseTest :: UploadServerCertificateResponse -> TestTree
uploadServerCertificateResponseTest = resp
    "UploadServerCertificate"
    "fixture/IAM/UploadServerCertificateResponse"
    (Proxy :: Proxy UploadServerCertificate)

listMFADevicesResponseTest :: ListMFADevicesResponse -> TestTree
listMFADevicesResponseTest = resp
    "ListMFADevices"
    "fixture/IAM/ListMFADevicesResponse"
    (Proxy :: Proxy ListMFADevices)

enableMFADeviceResponseTest :: EnableMFADeviceResponse -> TestTree
enableMFADeviceResponseTest = resp
    "EnableMFADevice"
    "fixture/IAM/EnableMFADeviceResponse"
    (Proxy :: Proxy EnableMFADevice)

listPolicyVersionsResponseTest :: ListPolicyVersionsResponse -> TestTree
listPolicyVersionsResponseTest = resp
    "ListPolicyVersions"
    "fixture/IAM/ListPolicyVersionsResponse"
    (Proxy :: Proxy ListPolicyVersions)

listSAMLProvidersResponseTest :: ListSAMLProvidersResponse -> TestTree
listSAMLProvidersResponseTest = resp
    "ListSAMLProviders"
    "fixture/IAM/ListSAMLProvidersResponse"
    (Proxy :: Proxy ListSAMLProviders)

updateSAMLProviderResponseTest :: UpdateSAMLProviderResponse -> TestTree
updateSAMLProviderResponseTest = resp
    "UpdateSAMLProvider"
    "fixture/IAM/UpdateSAMLProviderResponse"
    (Proxy :: Proxy UpdateSAMLProvider)

deleteSAMLProviderResponseTest :: DeleteSAMLProviderResponse -> TestTree
deleteSAMLProviderResponseTest = resp
    "DeleteSAMLProvider"
    "fixture/IAM/DeleteSAMLProviderResponse"
    (Proxy :: Proxy DeleteSAMLProvider)

createGroupResponseTest :: CreateGroupResponse -> TestTree
createGroupResponseTest = resp
    "CreateGroup"
    "fixture/IAM/CreateGroupResponse"
    (Proxy :: Proxy CreateGroup)

setDefaultPolicyVersionResponseTest :: SetDefaultPolicyVersionResponse -> TestTree
setDefaultPolicyVersionResponseTest = resp
    "SetDefaultPolicyVersion"
    "fixture/IAM/SetDefaultPolicyVersionResponse"
    (Proxy :: Proxy SetDefaultPolicyVersion)

listInstanceProfilesResponseTest :: ListInstanceProfilesResponse -> TestTree
listInstanceProfilesResponseTest = resp
    "ListInstanceProfiles"
    "fixture/IAM/ListInstanceProfilesResponse"
    (Proxy :: Proxy ListInstanceProfiles)

listGroupsResponseTest :: ListGroupsResponse -> TestTree
listGroupsResponseTest = resp
    "ListGroups"
    "fixture/IAM/ListGroupsResponse"
    (Proxy :: Proxy ListGroups)

deleteGroupResponseTest :: DeleteGroupResponse -> TestTree
deleteGroupResponseTest = resp
    "DeleteGroup"
    "fixture/IAM/DeleteGroupResponse"
    (Proxy :: Proxy DeleteGroup)

updateGroupResponseTest :: UpdateGroupResponse -> TestTree
updateGroupResponseTest = resp
    "UpdateGroup"
    "fixture/IAM/UpdateGroupResponse"
    (Proxy :: Proxy UpdateGroup)

getServerCertificateResponseTest :: GetServerCertificateResponse -> TestTree
getServerCertificateResponseTest = resp
    "GetServerCertificate"
    "fixture/IAM/GetServerCertificateResponse"
    (Proxy :: Proxy GetServerCertificate)

getPolicyResponseTest :: GetPolicyResponse -> TestTree
getPolicyResponseTest = resp
    "GetPolicy"
    "fixture/IAM/GetPolicyResponse"
    (Proxy :: Proxy GetPolicy)

generateCredentialReportResponseTest :: GenerateCredentialReportResponse -> TestTree
generateCredentialReportResponseTest = resp
    "GenerateCredentialReport"
    "fixture/IAM/GenerateCredentialReportResponse"
    (Proxy :: Proxy GenerateCredentialReport)

getGroupResponseTest :: GetGroupResponse -> TestTree
getGroupResponseTest = resp
    "GetGroup"
    "fixture/IAM/GetGroupResponse"
    (Proxy :: Proxy GetGroup)

deleteServerCertificateResponseTest :: DeleteServerCertificateResponse -> TestTree
deleteServerCertificateResponseTest = resp
    "DeleteServerCertificate"
    "fixture/IAM/DeleteServerCertificateResponse"
    (Proxy :: Proxy DeleteServerCertificate)

updateServerCertificateResponseTest :: UpdateServerCertificateResponse -> TestTree
updateServerCertificateResponseTest = resp
    "UpdateServerCertificate"
    "fixture/IAM/UpdateServerCertificateResponse"
    (Proxy :: Proxy UpdateServerCertificate)

deleteLoginProfileResponseTest :: DeleteLoginProfileResponse -> TestTree
deleteLoginProfileResponseTest = resp
    "DeleteLoginProfile"
    "fixture/IAM/DeleteLoginProfileResponse"
    (Proxy :: Proxy DeleteLoginProfile)

updateLoginProfileResponseTest :: UpdateLoginProfileResponse -> TestTree
updateLoginProfileResponseTest = resp
    "UpdateLoginProfile"
    "fixture/IAM/UpdateLoginProfileResponse"
    (Proxy :: Proxy UpdateLoginProfile)

listAttachedGroupPoliciesResponseTest :: ListAttachedGroupPoliciesResponse -> TestTree
listAttachedGroupPoliciesResponseTest = resp
    "ListAttachedGroupPolicies"
    "fixture/IAM/ListAttachedGroupPoliciesResponse"
    (Proxy :: Proxy ListAttachedGroupPolicies)
