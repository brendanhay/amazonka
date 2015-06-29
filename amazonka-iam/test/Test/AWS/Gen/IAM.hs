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

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.IAM

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ addClientIDToOpenIDConnectProviderTest $
--             addClientIDToOpenIDConnectProvider
--
--         , addRoleToInstanceProfileTest $
--             addRoleToInstanceProfile
--
--         , addUserToGroupTest $
--             addUserToGroup
--
--         , attachGroupPolicyTest $
--             attachGroupPolicy
--
--         , attachRolePolicyTest $
--             attachRolePolicy
--
--         , attachUserPolicyTest $
--             attachUserPolicy
--
--         , changePasswordTest $
--             changePassword
--
--         , createAccessKeyTest $
--             createAccessKey
--
--         , createAccountAliasTest $
--             createAccountAlias
--
--         , createGroupTest $
--             createGroup
--
--         , createInstanceProfileTest $
--             createInstanceProfile
--
--         , createLoginProfileTest $
--             createLoginProfile
--
--         , createOpenIDConnectProviderTest $
--             createOpenIDConnectProvider
--
--         , createPolicyTest $
--             createPolicy
--
--         , createPolicyVersionTest $
--             createPolicyVersion
--
--         , createRoleTest $
--             createRole
--
--         , createSAMLProviderTest $
--             createSAMLProvider
--
--         , createUserTest $
--             createUser
--
--         , createVirtualMFADeviceTest $
--             createVirtualMFADevice
--
--         , deactivateMFADeviceTest $
--             deactivateMFADevice
--
--         , deleteAccessKeyTest $
--             deleteAccessKey
--
--         , deleteAccountAliasTest $
--             deleteAccountAlias
--
--         , deleteAccountPasswordPolicyTest $
--             deleteAccountPasswordPolicy
--
--         , deleteGroupTest $
--             deleteGroup
--
--         , deleteGroupPolicyTest $
--             deleteGroupPolicy
--
--         , deleteInstanceProfileTest $
--             deleteInstanceProfile
--
--         , deleteLoginProfileTest $
--             deleteLoginProfile
--
--         , deleteOpenIDConnectProviderTest $
--             deleteOpenIDConnectProvider
--
--         , deletePolicyTest $
--             deletePolicy
--
--         , deletePolicyVersionTest $
--             deletePolicyVersion
--
--         , deleteRoleTest $
--             deleteRole
--
--         , deleteRolePolicyTest $
--             deleteRolePolicy
--
--         , deleteSAMLProviderTest $
--             deleteSAMLProvider
--
--         , deleteServerCertificateTest $
--             deleteServerCertificate
--
--         , deleteSigningCertificateTest $
--             deleteSigningCertificate
--
--         , deleteUserTest $
--             deleteUser
--
--         , deleteUserPolicyTest $
--             deleteUserPolicy
--
--         , deleteVirtualMFADeviceTest $
--             deleteVirtualMFADevice
--
--         , detachGroupPolicyTest $
--             detachGroupPolicy
--
--         , detachRolePolicyTest $
--             detachRolePolicy
--
--         , detachUserPolicyTest $
--             detachUserPolicy
--
--         , enableMFADeviceTest $
--             enableMFADevice
--
--         , generateCredentialReportTest $
--             generateCredentialReport
--
--         , getAccessKeyLastUsedTest $
--             getAccessKeyLastUsed
--
--         , getAccountAuthorizationDetailsTest $
--             getAccountAuthorizationDetails
--
--         , getAccountPasswordPolicyTest $
--             getAccountPasswordPolicy
--
--         , getAccountSummaryTest $
--             getAccountSummary
--
--         , getCredentialReportTest $
--             getCredentialReport
--
--         , getGroupTest $
--             getGroup
--
--         , getGroupPolicyTest $
--             getGroupPolicy
--
--         , getInstanceProfileTest $
--             getInstanceProfile
--
--         , getLoginProfileTest $
--             getLoginProfile
--
--         , getOpenIDConnectProviderTest $
--             getOpenIDConnectProvider
--
--         , getPolicyTest $
--             getPolicy
--
--         , getPolicyVersionTest $
--             getPolicyVersion
--
--         , getRoleTest $
--             getRole
--
--         , getRolePolicyTest $
--             getRolePolicy
--
--         , getSAMLProviderTest $
--             getSAMLProvider
--
--         , getServerCertificateTest $
--             getServerCertificate
--
--         , getUserTest $
--             getUser
--
--         , getUserPolicyTest $
--             getUserPolicy
--
--         , listAccessKeysTest $
--             listAccessKeys
--
--         , listAccountAliasesTest $
--             listAccountAliases
--
--         , listAttachedGroupPoliciesTest $
--             listAttachedGroupPolicies
--
--         , listAttachedRolePoliciesTest $
--             listAttachedRolePolicies
--
--         , listAttachedUserPoliciesTest $
--             listAttachedUserPolicies
--
--         , listEntitiesForPolicyTest $
--             listEntitiesForPolicy
--
--         , listGroupPoliciesTest $
--             listGroupPolicies
--
--         , listGroupsTest $
--             listGroups
--
--         , listGroupsForUserTest $
--             listGroupsForUser
--
--         , listInstanceProfilesTest $
--             listInstanceProfiles
--
--         , listInstanceProfilesForRoleTest $
--             listInstanceProfilesForRole
--
--         , listMFADevicesTest $
--             listMFADevices
--
--         , listOpenIDConnectProvidersTest $
--             listOpenIDConnectProviders
--
--         , listPoliciesTest $
--             listPolicies
--
--         , listPolicyVersionsTest $
--             listPolicyVersions
--
--         , listRolePoliciesTest $
--             listRolePolicies
--
--         , listRolesTest $
--             listRoles
--
--         , listSAMLProvidersTest $
--             listSAMLProviders
--
--         , listServerCertificatesTest $
--             listServerCertificates
--
--         , listSigningCertificatesTest $
--             listSigningCertificates
--
--         , listUserPoliciesTest $
--             listUserPolicies
--
--         , listUsersTest $
--             listUsers
--
--         , listVirtualMFADevicesTest $
--             listVirtualMFADevices
--
--         , putGroupPolicyTest $
--             putGroupPolicy
--
--         , putRolePolicyTest $
--             putRolePolicy
--
--         , putUserPolicyTest $
--             putUserPolicy
--
--         , removeClientIDFromOpenIDConnectProviderTest $
--             removeClientIDFromOpenIDConnectProvider
--
--         , removeRoleFromInstanceProfileTest $
--             removeRoleFromInstanceProfile
--
--         , removeUserFromGroupTest $
--             removeUserFromGroup
--
--         , resyncMFADeviceTest $
--             resyncMFADevice
--
--         , setDefaultPolicyVersionTest $
--             setDefaultPolicyVersion
--
--         , updateAccessKeyTest $
--             updateAccessKey
--
--         , updateAccountPasswordPolicyTest $
--             updateAccountPasswordPolicy
--
--         , updateAssumeRolePolicyTest $
--             updateAssumeRolePolicy
--
--         , updateGroupTest $
--             updateGroup
--
--         , updateLoginProfileTest $
--             updateLoginProfile
--
--         , updateOpenIDConnectProviderThumbprintTest $
--             updateOpenIDConnectProviderThumbprint
--
--         , updateSAMLProviderTest $
--             updateSAMLProvider
--
--         , updateServerCertificateTest $
--             updateServerCertificate
--
--         , updateSigningCertificateTest $
--             updateSigningCertificate
--
--         , updateUserTest $
--             updateUser
--
--         , uploadServerCertificateTest $
--             uploadServerCertificate
--
--         , uploadSigningCertificateTest $
--             uploadSigningCertificate
--
--           ]

--     , testGroup "response"
--         [ addClientIDToOpenIDConnectProviderResponseTest $
--             addClientIDToOpenIDConnectProviderResponse
--
--         , addRoleToInstanceProfileResponseTest $
--             addRoleToInstanceProfileResponse
--
--         , addUserToGroupResponseTest $
--             addUserToGroupResponse
--
--         , attachGroupPolicyResponseTest $
--             attachGroupPolicyResponse
--
--         , attachRolePolicyResponseTest $
--             attachRolePolicyResponse
--
--         , attachUserPolicyResponseTest $
--             attachUserPolicyResponse
--
--         , changePasswordResponseTest $
--             changePasswordResponse
--
--         , createAccessKeyResponseTest $
--             createAccessKeyResponse
--
--         , createAccountAliasResponseTest $
--             createAccountAliasResponse
--
--         , createGroupResponseTest $
--             createGroupResponse
--
--         , createInstanceProfileResponseTest $
--             createInstanceProfileResponse
--
--         , createLoginProfileResponseTest $
--             createLoginProfileResponse
--
--         , createOpenIDConnectProviderResponseTest $
--             createOpenIDConnectProviderResponse
--
--         , createPolicyResponseTest $
--             createPolicyResponse
--
--         , createPolicyVersionResponseTest $
--             createPolicyVersionResponse
--
--         , createRoleResponseTest $
--             createRoleResponse
--
--         , createSAMLProviderResponseTest $
--             createSAMLProviderResponse
--
--         , createUserResponseTest $
--             createUserResponse
--
--         , createVirtualMFADeviceResponseTest $
--             createVirtualMFADeviceResponse
--
--         , deactivateMFADeviceResponseTest $
--             deactivateMFADeviceResponse
--
--         , deleteAccessKeyResponseTest $
--             deleteAccessKeyResponse
--
--         , deleteAccountAliasResponseTest $
--             deleteAccountAliasResponse
--
--         , deleteAccountPasswordPolicyResponseTest $
--             deleteAccountPasswordPolicyResponse
--
--         , deleteGroupResponseTest $
--             deleteGroupResponse
--
--         , deleteGroupPolicyResponseTest $
--             deleteGroupPolicyResponse
--
--         , deleteInstanceProfileResponseTest $
--             deleteInstanceProfileResponse
--
--         , deleteLoginProfileResponseTest $
--             deleteLoginProfileResponse
--
--         , deleteOpenIDConnectProviderResponseTest $
--             deleteOpenIDConnectProviderResponse
--
--         , deletePolicyResponseTest $
--             deletePolicyResponse
--
--         , deletePolicyVersionResponseTest $
--             deletePolicyVersionResponse
--
--         , deleteRoleResponseTest $
--             deleteRoleResponse
--
--         , deleteRolePolicyResponseTest $
--             deleteRolePolicyResponse
--
--         , deleteSAMLProviderResponseTest $
--             deleteSAMLProviderResponse
--
--         , deleteServerCertificateResponseTest $
--             deleteServerCertificateResponse
--
--         , deleteSigningCertificateResponseTest $
--             deleteSigningCertificateResponse
--
--         , deleteUserResponseTest $
--             deleteUserResponse
--
--         , deleteUserPolicyResponseTest $
--             deleteUserPolicyResponse
--
--         , deleteVirtualMFADeviceResponseTest $
--             deleteVirtualMFADeviceResponse
--
--         , detachGroupPolicyResponseTest $
--             detachGroupPolicyResponse
--
--         , detachRolePolicyResponseTest $
--             detachRolePolicyResponse
--
--         , detachUserPolicyResponseTest $
--             detachUserPolicyResponse
--
--         , enableMFADeviceResponseTest $
--             enableMFADeviceResponse
--
--         , generateCredentialReportResponseTest $
--             generateCredentialReportResponse
--
--         , getAccessKeyLastUsedResponseTest $
--             getAccessKeyLastUsedResponse
--
--         , getAccountAuthorizationDetailsResponseTest $
--             getAccountAuthorizationDetailsResponse
--
--         , getAccountPasswordPolicyResponseTest $
--             getAccountPasswordPolicyResponse
--
--         , getAccountSummaryResponseTest $
--             getAccountSummaryResponse
--
--         , getCredentialReportResponseTest $
--             getCredentialReportResponse
--
--         , getGroupResponseTest $
--             getGroupResponse
--
--         , getGroupPolicyResponseTest $
--             getGroupPolicyResponse
--
--         , getInstanceProfileResponseTest $
--             getInstanceProfileResponse
--
--         , getLoginProfileResponseTest $
--             getLoginProfileResponse
--
--         , getOpenIDConnectProviderResponseTest $
--             getOpenIDConnectProviderResponse
--
--         , getPolicyResponseTest $
--             getPolicyResponse
--
--         , getPolicyVersionResponseTest $
--             getPolicyVersionResponse
--
--         , getRoleResponseTest $
--             getRoleResponse
--
--         , getRolePolicyResponseTest $
--             getRolePolicyResponse
--
--         , getSAMLProviderResponseTest $
--             getSAMLProviderResponse
--
--         , getServerCertificateResponseTest $
--             getServerCertificateResponse
--
--         , getUserResponseTest $
--             getUserResponse
--
--         , getUserPolicyResponseTest $
--             getUserPolicyResponse
--
--         , listAccessKeysResponseTest $
--             listAccessKeysResponse
--
--         , listAccountAliasesResponseTest $
--             listAccountAliasesResponse
--
--         , listAttachedGroupPoliciesResponseTest $
--             listAttachedGroupPoliciesResponse
--
--         , listAttachedRolePoliciesResponseTest $
--             listAttachedRolePoliciesResponse
--
--         , listAttachedUserPoliciesResponseTest $
--             listAttachedUserPoliciesResponse
--
--         , listEntitiesForPolicyResponseTest $
--             listEntitiesForPolicyResponse
--
--         , listGroupPoliciesResponseTest $
--             listGroupPoliciesResponse
--
--         , listGroupsResponseTest $
--             listGroupsResponse
--
--         , listGroupsForUserResponseTest $
--             listGroupsForUserResponse
--
--         , listInstanceProfilesResponseTest $
--             listInstanceProfilesResponse
--
--         , listInstanceProfilesForRoleResponseTest $
--             listInstanceProfilesForRoleResponse
--
--         , listMFADevicesResponseTest $
--             listMFADevicesResponse
--
--         , listOpenIDConnectProvidersResponseTest $
--             listOpenIDConnectProvidersResponse
--
--         , listPoliciesResponseTest $
--             listPoliciesResponse
--
--         , listPolicyVersionsResponseTest $
--             listPolicyVersionsResponse
--
--         , listRolePoliciesResponseTest $
--             listRolePoliciesResponse
--
--         , listRolesResponseTest $
--             listRolesResponse
--
--         , listSAMLProvidersResponseTest $
--             listSAMLProvidersResponse
--
--         , listServerCertificatesResponseTest $
--             listServerCertificatesResponse
--
--         , listSigningCertificatesResponseTest $
--             listSigningCertificatesResponse
--
--         , listUserPoliciesResponseTest $
--             listUserPoliciesResponse
--
--         , listUsersResponseTest $
--             listUsersResponse
--
--         , listVirtualMFADevicesResponseTest $
--             listVirtualMFADevicesResponse
--
--         , putGroupPolicyResponseTest $
--             putGroupPolicyResponse
--
--         , putRolePolicyResponseTest $
--             putRolePolicyResponse
--
--         , putUserPolicyResponseTest $
--             putUserPolicyResponse
--
--         , removeClientIDFromOpenIDConnectProviderResponseTest $
--             removeClientIDFromOpenIDConnectProviderResponse
--
--         , removeRoleFromInstanceProfileResponseTest $
--             removeRoleFromInstanceProfileResponse
--
--         , removeUserFromGroupResponseTest $
--             removeUserFromGroupResponse
--
--         , resyncMFADeviceResponseTest $
--             resyncMFADeviceResponse
--
--         , setDefaultPolicyVersionResponseTest $
--             setDefaultPolicyVersionResponse
--
--         , updateAccessKeyResponseTest $
--             updateAccessKeyResponse
--
--         , updateAccountPasswordPolicyResponseTest $
--             updateAccountPasswordPolicyResponse
--
--         , updateAssumeRolePolicyResponseTest $
--             updateAssumeRolePolicyResponse
--
--         , updateGroupResponseTest $
--             updateGroupResponse
--
--         , updateLoginProfileResponseTest $
--             updateLoginProfileResponse
--
--         , updateOpenIDConnectProviderThumbprintResponseTest $
--             updateOpenIDConnectProviderThumbprintResponse
--
--         , updateSAMLProviderResponseTest $
--             updateSAMLProviderResponse
--
--         , updateServerCertificateResponseTest $
--             updateServerCertificateResponse
--
--         , updateSigningCertificateResponseTest $
--             updateSigningCertificateResponse
--
--         , updateUserResponseTest $
--             updateUserResponse
--
--         , uploadServerCertificateResponseTest $
--             uploadServerCertificateResponse
--
--         , uploadSigningCertificateResponseTest $
--             uploadSigningCertificateResponse
--
--           ]
--     ]

-- Requests

addClientIDToOpenIDConnectProviderTest :: AddClientIDToOpenIDConnectProvider -> TestTree
addClientIDToOpenIDConnectProviderTest = undefined

addRoleToInstanceProfileTest :: AddRoleToInstanceProfile -> TestTree
addRoleToInstanceProfileTest = undefined

addUserToGroupTest :: AddUserToGroup -> TestTree
addUserToGroupTest = undefined

attachGroupPolicyTest :: AttachGroupPolicy -> TestTree
attachGroupPolicyTest = undefined

attachRolePolicyTest :: AttachRolePolicy -> TestTree
attachRolePolicyTest = undefined

attachUserPolicyTest :: AttachUserPolicy -> TestTree
attachUserPolicyTest = undefined

changePasswordTest :: ChangePassword -> TestTree
changePasswordTest = undefined

createAccessKeyTest :: CreateAccessKey -> TestTree
createAccessKeyTest = undefined

createAccountAliasTest :: CreateAccountAlias -> TestTree
createAccountAliasTest = undefined

createGroupTest :: CreateGroup -> TestTree
createGroupTest = undefined

createInstanceProfileTest :: CreateInstanceProfile -> TestTree
createInstanceProfileTest = undefined

createLoginProfileTest :: CreateLoginProfile -> TestTree
createLoginProfileTest = undefined

createOpenIDConnectProviderTest :: CreateOpenIDConnectProvider -> TestTree
createOpenIDConnectProviderTest = undefined

createPolicyTest :: CreatePolicy -> TestTree
createPolicyTest = undefined

createPolicyVersionTest :: CreatePolicyVersion -> TestTree
createPolicyVersionTest = undefined

createRoleTest :: CreateRole -> TestTree
createRoleTest = undefined

createSAMLProviderTest :: CreateSAMLProvider -> TestTree
createSAMLProviderTest = undefined

createUserTest :: CreateUser -> TestTree
createUserTest = undefined

createVirtualMFADeviceTest :: CreateVirtualMFADevice -> TestTree
createVirtualMFADeviceTest = undefined

deactivateMFADeviceTest :: DeactivateMFADevice -> TestTree
deactivateMFADeviceTest = undefined

deleteAccessKeyTest :: DeleteAccessKey -> TestTree
deleteAccessKeyTest = undefined

deleteAccountAliasTest :: DeleteAccountAlias -> TestTree
deleteAccountAliasTest = undefined

deleteAccountPasswordPolicyTest :: DeleteAccountPasswordPolicy -> TestTree
deleteAccountPasswordPolicyTest = undefined

deleteGroupTest :: DeleteGroup -> TestTree
deleteGroupTest = undefined

deleteGroupPolicyTest :: DeleteGroupPolicy -> TestTree
deleteGroupPolicyTest = undefined

deleteInstanceProfileTest :: DeleteInstanceProfile -> TestTree
deleteInstanceProfileTest = undefined

deleteLoginProfileTest :: DeleteLoginProfile -> TestTree
deleteLoginProfileTest = undefined

deleteOpenIDConnectProviderTest :: DeleteOpenIDConnectProvider -> TestTree
deleteOpenIDConnectProviderTest = undefined

deletePolicyTest :: DeletePolicy -> TestTree
deletePolicyTest = undefined

deletePolicyVersionTest :: DeletePolicyVersion -> TestTree
deletePolicyVersionTest = undefined

deleteRoleTest :: DeleteRole -> TestTree
deleteRoleTest = undefined

deleteRolePolicyTest :: DeleteRolePolicy -> TestTree
deleteRolePolicyTest = undefined

deleteSAMLProviderTest :: DeleteSAMLProvider -> TestTree
deleteSAMLProviderTest = undefined

deleteServerCertificateTest :: DeleteServerCertificate -> TestTree
deleteServerCertificateTest = undefined

deleteSigningCertificateTest :: DeleteSigningCertificate -> TestTree
deleteSigningCertificateTest = undefined

deleteUserTest :: DeleteUser -> TestTree
deleteUserTest = undefined

deleteUserPolicyTest :: DeleteUserPolicy -> TestTree
deleteUserPolicyTest = undefined

deleteVirtualMFADeviceTest :: DeleteVirtualMFADevice -> TestTree
deleteVirtualMFADeviceTest = undefined

detachGroupPolicyTest :: DetachGroupPolicy -> TestTree
detachGroupPolicyTest = undefined

detachRolePolicyTest :: DetachRolePolicy -> TestTree
detachRolePolicyTest = undefined

detachUserPolicyTest :: DetachUserPolicy -> TestTree
detachUserPolicyTest = undefined

enableMFADeviceTest :: EnableMFADevice -> TestTree
enableMFADeviceTest = undefined

generateCredentialReportTest :: GenerateCredentialReport -> TestTree
generateCredentialReportTest = undefined

getAccessKeyLastUsedTest :: GetAccessKeyLastUsed -> TestTree
getAccessKeyLastUsedTest = undefined

getAccountAuthorizationDetailsTest :: GetAccountAuthorizationDetails -> TestTree
getAccountAuthorizationDetailsTest = undefined

getAccountPasswordPolicyTest :: GetAccountPasswordPolicy -> TestTree
getAccountPasswordPolicyTest = undefined

getAccountSummaryTest :: GetAccountSummary -> TestTree
getAccountSummaryTest = undefined

getCredentialReportTest :: GetCredentialReport -> TestTree
getCredentialReportTest = undefined

getGroupTest :: GetGroup -> TestTree
getGroupTest = undefined

getGroupPolicyTest :: GetGroupPolicy -> TestTree
getGroupPolicyTest = undefined

getInstanceProfileTest :: GetInstanceProfile -> TestTree
getInstanceProfileTest = undefined

getLoginProfileTest :: GetLoginProfile -> TestTree
getLoginProfileTest = undefined

getOpenIDConnectProviderTest :: GetOpenIDConnectProvider -> TestTree
getOpenIDConnectProviderTest = undefined

getPolicyTest :: GetPolicy -> TestTree
getPolicyTest = undefined

getPolicyVersionTest :: GetPolicyVersion -> TestTree
getPolicyVersionTest = undefined

getRoleTest :: GetRole -> TestTree
getRoleTest = undefined

getRolePolicyTest :: GetRolePolicy -> TestTree
getRolePolicyTest = undefined

getSAMLProviderTest :: GetSAMLProvider -> TestTree
getSAMLProviderTest = undefined

getServerCertificateTest :: GetServerCertificate -> TestTree
getServerCertificateTest = undefined

getUserTest :: GetUser -> TestTree
getUserTest = undefined

getUserPolicyTest :: GetUserPolicy -> TestTree
getUserPolicyTest = undefined

listAccessKeysTest :: ListAccessKeys -> TestTree
listAccessKeysTest = undefined

listAccountAliasesTest :: ListAccountAliases -> TestTree
listAccountAliasesTest = undefined

listAttachedGroupPoliciesTest :: ListAttachedGroupPolicies -> TestTree
listAttachedGroupPoliciesTest = undefined

listAttachedRolePoliciesTest :: ListAttachedRolePolicies -> TestTree
listAttachedRolePoliciesTest = undefined

listAttachedUserPoliciesTest :: ListAttachedUserPolicies -> TestTree
listAttachedUserPoliciesTest = undefined

listEntitiesForPolicyTest :: ListEntitiesForPolicy -> TestTree
listEntitiesForPolicyTest = undefined

listGroupPoliciesTest :: ListGroupPolicies -> TestTree
listGroupPoliciesTest = undefined

listGroupsTest :: ListGroups -> TestTree
listGroupsTest = undefined

listGroupsForUserTest :: ListGroupsForUser -> TestTree
listGroupsForUserTest = undefined

listInstanceProfilesTest :: ListInstanceProfiles -> TestTree
listInstanceProfilesTest = undefined

listInstanceProfilesForRoleTest :: ListInstanceProfilesForRole -> TestTree
listInstanceProfilesForRoleTest = undefined

listMFADevicesTest :: ListMFADevices -> TestTree
listMFADevicesTest = undefined

listOpenIDConnectProvidersTest :: ListOpenIDConnectProviders -> TestTree
listOpenIDConnectProvidersTest = undefined

listPoliciesTest :: ListPolicies -> TestTree
listPoliciesTest = undefined

listPolicyVersionsTest :: ListPolicyVersions -> TestTree
listPolicyVersionsTest = undefined

listRolePoliciesTest :: ListRolePolicies -> TestTree
listRolePoliciesTest = undefined

listRolesTest :: ListRoles -> TestTree
listRolesTest = undefined

listSAMLProvidersTest :: ListSAMLProviders -> TestTree
listSAMLProvidersTest = undefined

listServerCertificatesTest :: ListServerCertificates -> TestTree
listServerCertificatesTest = undefined

listSigningCertificatesTest :: ListSigningCertificates -> TestTree
listSigningCertificatesTest = undefined

listUserPoliciesTest :: ListUserPolicies -> TestTree
listUserPoliciesTest = undefined

listUsersTest :: ListUsers -> TestTree
listUsersTest = undefined

listVirtualMFADevicesTest :: ListVirtualMFADevices -> TestTree
listVirtualMFADevicesTest = undefined

putGroupPolicyTest :: PutGroupPolicy -> TestTree
putGroupPolicyTest = undefined

putRolePolicyTest :: PutRolePolicy -> TestTree
putRolePolicyTest = undefined

putUserPolicyTest :: PutUserPolicy -> TestTree
putUserPolicyTest = undefined

removeClientIDFromOpenIDConnectProviderTest :: RemoveClientIDFromOpenIDConnectProvider -> TestTree
removeClientIDFromOpenIDConnectProviderTest = undefined

removeRoleFromInstanceProfileTest :: RemoveRoleFromInstanceProfile -> TestTree
removeRoleFromInstanceProfileTest = undefined

removeUserFromGroupTest :: RemoveUserFromGroup -> TestTree
removeUserFromGroupTest = undefined

resyncMFADeviceTest :: ResyncMFADevice -> TestTree
resyncMFADeviceTest = undefined

setDefaultPolicyVersionTest :: SetDefaultPolicyVersion -> TestTree
setDefaultPolicyVersionTest = undefined

updateAccessKeyTest :: UpdateAccessKey -> TestTree
updateAccessKeyTest = undefined

updateAccountPasswordPolicyTest :: UpdateAccountPasswordPolicy -> TestTree
updateAccountPasswordPolicyTest = undefined

updateAssumeRolePolicyTest :: UpdateAssumeRolePolicy -> TestTree
updateAssumeRolePolicyTest = undefined

updateGroupTest :: UpdateGroup -> TestTree
updateGroupTest = undefined

updateLoginProfileTest :: UpdateLoginProfile -> TestTree
updateLoginProfileTest = undefined

updateOpenIDConnectProviderThumbprintTest :: UpdateOpenIDConnectProviderThumbprint -> TestTree
updateOpenIDConnectProviderThumbprintTest = undefined

updateSAMLProviderTest :: UpdateSAMLProvider -> TestTree
updateSAMLProviderTest = undefined

updateServerCertificateTest :: UpdateServerCertificate -> TestTree
updateServerCertificateTest = undefined

updateSigningCertificateTest :: UpdateSigningCertificate -> TestTree
updateSigningCertificateTest = undefined

updateUserTest :: UpdateUser -> TestTree
updateUserTest = undefined

uploadServerCertificateTest :: UploadServerCertificate -> TestTree
uploadServerCertificateTest = undefined

uploadSigningCertificateTest :: UploadSigningCertificate -> TestTree
uploadSigningCertificateTest = undefined

-- Responses

addClientIDToOpenIDConnectProviderResponseTest :: AddClientIDToOpenIDConnectProviderResponse -> TestTree
addClientIDToOpenIDConnectProviderResponseTest = resp
    "addClientIDToOpenIDConnectProviderResponse"
    "fixture/AddClientIDToOpenIDConnectProviderResponse"
    (Proxy :: Proxy AddClientIDToOpenIDConnectProvider)

addRoleToInstanceProfileResponseTest :: AddRoleToInstanceProfileResponse -> TestTree
addRoleToInstanceProfileResponseTest = resp
    "addRoleToInstanceProfileResponse"
    "fixture/AddRoleToInstanceProfileResponse"
    (Proxy :: Proxy AddRoleToInstanceProfile)

addUserToGroupResponseTest :: AddUserToGroupResponse -> TestTree
addUserToGroupResponseTest = resp
    "addUserToGroupResponse"
    "fixture/AddUserToGroupResponse"
    (Proxy :: Proxy AddUserToGroup)

attachGroupPolicyResponseTest :: AttachGroupPolicyResponse -> TestTree
attachGroupPolicyResponseTest = resp
    "attachGroupPolicyResponse"
    "fixture/AttachGroupPolicyResponse"
    (Proxy :: Proxy AttachGroupPolicy)

attachRolePolicyResponseTest :: AttachRolePolicyResponse -> TestTree
attachRolePolicyResponseTest = resp
    "attachRolePolicyResponse"
    "fixture/AttachRolePolicyResponse"
    (Proxy :: Proxy AttachRolePolicy)

attachUserPolicyResponseTest :: AttachUserPolicyResponse -> TestTree
attachUserPolicyResponseTest = resp
    "attachUserPolicyResponse"
    "fixture/AttachUserPolicyResponse"
    (Proxy :: Proxy AttachUserPolicy)

changePasswordResponseTest :: ChangePasswordResponse -> TestTree
changePasswordResponseTest = resp
    "changePasswordResponse"
    "fixture/ChangePasswordResponse"
    (Proxy :: Proxy ChangePassword)

createAccessKeyResponseTest :: CreateAccessKeyResponse -> TestTree
createAccessKeyResponseTest = resp
    "createAccessKeyResponse"
    "fixture/CreateAccessKeyResponse"
    (Proxy :: Proxy CreateAccessKey)

createAccountAliasResponseTest :: CreateAccountAliasResponse -> TestTree
createAccountAliasResponseTest = resp
    "createAccountAliasResponse"
    "fixture/CreateAccountAliasResponse"
    (Proxy :: Proxy CreateAccountAlias)

createGroupResponseTest :: CreateGroupResponse -> TestTree
createGroupResponseTest = resp
    "createGroupResponse"
    "fixture/CreateGroupResponse"
    (Proxy :: Proxy CreateGroup)

createInstanceProfileResponseTest :: CreateInstanceProfileResponse -> TestTree
createInstanceProfileResponseTest = resp
    "createInstanceProfileResponse"
    "fixture/CreateInstanceProfileResponse"
    (Proxy :: Proxy CreateInstanceProfile)

createLoginProfileResponseTest :: CreateLoginProfileResponse -> TestTree
createLoginProfileResponseTest = resp
    "createLoginProfileResponse"
    "fixture/CreateLoginProfileResponse"
    (Proxy :: Proxy CreateLoginProfile)

createOpenIDConnectProviderResponseTest :: CreateOpenIDConnectProviderResponse -> TestTree
createOpenIDConnectProviderResponseTest = resp
    "createOpenIDConnectProviderResponse"
    "fixture/CreateOpenIDConnectProviderResponse"
    (Proxy :: Proxy CreateOpenIDConnectProvider)

createPolicyResponseTest :: CreatePolicyResponse -> TestTree
createPolicyResponseTest = resp
    "createPolicyResponse"
    "fixture/CreatePolicyResponse"
    (Proxy :: Proxy CreatePolicy)

createPolicyVersionResponseTest :: CreatePolicyVersionResponse -> TestTree
createPolicyVersionResponseTest = resp
    "createPolicyVersionResponse"
    "fixture/CreatePolicyVersionResponse"
    (Proxy :: Proxy CreatePolicyVersion)

createRoleResponseTest :: CreateRoleResponse -> TestTree
createRoleResponseTest = resp
    "createRoleResponse"
    "fixture/CreateRoleResponse"
    (Proxy :: Proxy CreateRole)

createSAMLProviderResponseTest :: CreateSAMLProviderResponse -> TestTree
createSAMLProviderResponseTest = resp
    "createSAMLProviderResponse"
    "fixture/CreateSAMLProviderResponse"
    (Proxy :: Proxy CreateSAMLProvider)

createUserResponseTest :: CreateUserResponse -> TestTree
createUserResponseTest = resp
    "createUserResponse"
    "fixture/CreateUserResponse"
    (Proxy :: Proxy CreateUser)

createVirtualMFADeviceResponseTest :: CreateVirtualMFADeviceResponse -> TestTree
createVirtualMFADeviceResponseTest = resp
    "createVirtualMFADeviceResponse"
    "fixture/CreateVirtualMFADeviceResponse"
    (Proxy :: Proxy CreateVirtualMFADevice)

deactivateMFADeviceResponseTest :: DeactivateMFADeviceResponse -> TestTree
deactivateMFADeviceResponseTest = resp
    "deactivateMFADeviceResponse"
    "fixture/DeactivateMFADeviceResponse"
    (Proxy :: Proxy DeactivateMFADevice)

deleteAccessKeyResponseTest :: DeleteAccessKeyResponse -> TestTree
deleteAccessKeyResponseTest = resp
    "deleteAccessKeyResponse"
    "fixture/DeleteAccessKeyResponse"
    (Proxy :: Proxy DeleteAccessKey)

deleteAccountAliasResponseTest :: DeleteAccountAliasResponse -> TestTree
deleteAccountAliasResponseTest = resp
    "deleteAccountAliasResponse"
    "fixture/DeleteAccountAliasResponse"
    (Proxy :: Proxy DeleteAccountAlias)

deleteAccountPasswordPolicyResponseTest :: DeleteAccountPasswordPolicyResponse -> TestTree
deleteAccountPasswordPolicyResponseTest = resp
    "deleteAccountPasswordPolicyResponse"
    "fixture/DeleteAccountPasswordPolicyResponse"
    (Proxy :: Proxy DeleteAccountPasswordPolicy)

deleteGroupResponseTest :: DeleteGroupResponse -> TestTree
deleteGroupResponseTest = resp
    "deleteGroupResponse"
    "fixture/DeleteGroupResponse"
    (Proxy :: Proxy DeleteGroup)

deleteGroupPolicyResponseTest :: DeleteGroupPolicyResponse -> TestTree
deleteGroupPolicyResponseTest = resp
    "deleteGroupPolicyResponse"
    "fixture/DeleteGroupPolicyResponse"
    (Proxy :: Proxy DeleteGroupPolicy)

deleteInstanceProfileResponseTest :: DeleteInstanceProfileResponse -> TestTree
deleteInstanceProfileResponseTest = resp
    "deleteInstanceProfileResponse"
    "fixture/DeleteInstanceProfileResponse"
    (Proxy :: Proxy DeleteInstanceProfile)

deleteLoginProfileResponseTest :: DeleteLoginProfileResponse -> TestTree
deleteLoginProfileResponseTest = resp
    "deleteLoginProfileResponse"
    "fixture/DeleteLoginProfileResponse"
    (Proxy :: Proxy DeleteLoginProfile)

deleteOpenIDConnectProviderResponseTest :: DeleteOpenIDConnectProviderResponse -> TestTree
deleteOpenIDConnectProviderResponseTest = resp
    "deleteOpenIDConnectProviderResponse"
    "fixture/DeleteOpenIDConnectProviderResponse"
    (Proxy :: Proxy DeleteOpenIDConnectProvider)

deletePolicyResponseTest :: DeletePolicyResponse -> TestTree
deletePolicyResponseTest = resp
    "deletePolicyResponse"
    "fixture/DeletePolicyResponse"
    (Proxy :: Proxy DeletePolicy)

deletePolicyVersionResponseTest :: DeletePolicyVersionResponse -> TestTree
deletePolicyVersionResponseTest = resp
    "deletePolicyVersionResponse"
    "fixture/DeletePolicyVersionResponse"
    (Proxy :: Proxy DeletePolicyVersion)

deleteRoleResponseTest :: DeleteRoleResponse -> TestTree
deleteRoleResponseTest = resp
    "deleteRoleResponse"
    "fixture/DeleteRoleResponse"
    (Proxy :: Proxy DeleteRole)

deleteRolePolicyResponseTest :: DeleteRolePolicyResponse -> TestTree
deleteRolePolicyResponseTest = resp
    "deleteRolePolicyResponse"
    "fixture/DeleteRolePolicyResponse"
    (Proxy :: Proxy DeleteRolePolicy)

deleteSAMLProviderResponseTest :: DeleteSAMLProviderResponse -> TestTree
deleteSAMLProviderResponseTest = resp
    "deleteSAMLProviderResponse"
    "fixture/DeleteSAMLProviderResponse"
    (Proxy :: Proxy DeleteSAMLProvider)

deleteServerCertificateResponseTest :: DeleteServerCertificateResponse -> TestTree
deleteServerCertificateResponseTest = resp
    "deleteServerCertificateResponse"
    "fixture/DeleteServerCertificateResponse"
    (Proxy :: Proxy DeleteServerCertificate)

deleteSigningCertificateResponseTest :: DeleteSigningCertificateResponse -> TestTree
deleteSigningCertificateResponseTest = resp
    "deleteSigningCertificateResponse"
    "fixture/DeleteSigningCertificateResponse"
    (Proxy :: Proxy DeleteSigningCertificate)

deleteUserResponseTest :: DeleteUserResponse -> TestTree
deleteUserResponseTest = resp
    "deleteUserResponse"
    "fixture/DeleteUserResponse"
    (Proxy :: Proxy DeleteUser)

deleteUserPolicyResponseTest :: DeleteUserPolicyResponse -> TestTree
deleteUserPolicyResponseTest = resp
    "deleteUserPolicyResponse"
    "fixture/DeleteUserPolicyResponse"
    (Proxy :: Proxy DeleteUserPolicy)

deleteVirtualMFADeviceResponseTest :: DeleteVirtualMFADeviceResponse -> TestTree
deleteVirtualMFADeviceResponseTest = resp
    "deleteVirtualMFADeviceResponse"
    "fixture/DeleteVirtualMFADeviceResponse"
    (Proxy :: Proxy DeleteVirtualMFADevice)

detachGroupPolicyResponseTest :: DetachGroupPolicyResponse -> TestTree
detachGroupPolicyResponseTest = resp
    "detachGroupPolicyResponse"
    "fixture/DetachGroupPolicyResponse"
    (Proxy :: Proxy DetachGroupPolicy)

detachRolePolicyResponseTest :: DetachRolePolicyResponse -> TestTree
detachRolePolicyResponseTest = resp
    "detachRolePolicyResponse"
    "fixture/DetachRolePolicyResponse"
    (Proxy :: Proxy DetachRolePolicy)

detachUserPolicyResponseTest :: DetachUserPolicyResponse -> TestTree
detachUserPolicyResponseTest = resp
    "detachUserPolicyResponse"
    "fixture/DetachUserPolicyResponse"
    (Proxy :: Proxy DetachUserPolicy)

enableMFADeviceResponseTest :: EnableMFADeviceResponse -> TestTree
enableMFADeviceResponseTest = resp
    "enableMFADeviceResponse"
    "fixture/EnableMFADeviceResponse"
    (Proxy :: Proxy EnableMFADevice)

generateCredentialReportResponseTest :: GenerateCredentialReportResponse -> TestTree
generateCredentialReportResponseTest = resp
    "generateCredentialReportResponse"
    "fixture/GenerateCredentialReportResponse"
    (Proxy :: Proxy GenerateCredentialReport)

getAccessKeyLastUsedResponseTest :: GetAccessKeyLastUsedResponse -> TestTree
getAccessKeyLastUsedResponseTest = resp
    "getAccessKeyLastUsedResponse"
    "fixture/GetAccessKeyLastUsedResponse"
    (Proxy :: Proxy GetAccessKeyLastUsed)

getAccountAuthorizationDetailsResponseTest :: GetAccountAuthorizationDetailsResponse -> TestTree
getAccountAuthorizationDetailsResponseTest = resp
    "getAccountAuthorizationDetailsResponse"
    "fixture/GetAccountAuthorizationDetailsResponse"
    (Proxy :: Proxy GetAccountAuthorizationDetails)

getAccountPasswordPolicyResponseTest :: GetAccountPasswordPolicyResponse -> TestTree
getAccountPasswordPolicyResponseTest = resp
    "getAccountPasswordPolicyResponse"
    "fixture/GetAccountPasswordPolicyResponse"
    (Proxy :: Proxy GetAccountPasswordPolicy)

getAccountSummaryResponseTest :: GetAccountSummaryResponse -> TestTree
getAccountSummaryResponseTest = resp
    "getAccountSummaryResponse"
    "fixture/GetAccountSummaryResponse"
    (Proxy :: Proxy GetAccountSummary)

getCredentialReportResponseTest :: GetCredentialReportResponse -> TestTree
getCredentialReportResponseTest = resp
    "getCredentialReportResponse"
    "fixture/GetCredentialReportResponse"
    (Proxy :: Proxy GetCredentialReport)

getGroupResponseTest :: GetGroupResponse -> TestTree
getGroupResponseTest = resp
    "getGroupResponse"
    "fixture/GetGroupResponse"
    (Proxy :: Proxy GetGroup)

getGroupPolicyResponseTest :: GetGroupPolicyResponse -> TestTree
getGroupPolicyResponseTest = resp
    "getGroupPolicyResponse"
    "fixture/GetGroupPolicyResponse"
    (Proxy :: Proxy GetGroupPolicy)

getInstanceProfileResponseTest :: GetInstanceProfileResponse -> TestTree
getInstanceProfileResponseTest = resp
    "getInstanceProfileResponse"
    "fixture/GetInstanceProfileResponse"
    (Proxy :: Proxy GetInstanceProfile)

getLoginProfileResponseTest :: GetLoginProfileResponse -> TestTree
getLoginProfileResponseTest = resp
    "getLoginProfileResponse"
    "fixture/GetLoginProfileResponse"
    (Proxy :: Proxy GetLoginProfile)

getOpenIDConnectProviderResponseTest :: GetOpenIDConnectProviderResponse -> TestTree
getOpenIDConnectProviderResponseTest = resp
    "getOpenIDConnectProviderResponse"
    "fixture/GetOpenIDConnectProviderResponse"
    (Proxy :: Proxy GetOpenIDConnectProvider)

getPolicyResponseTest :: GetPolicyResponse -> TestTree
getPolicyResponseTest = resp
    "getPolicyResponse"
    "fixture/GetPolicyResponse"
    (Proxy :: Proxy GetPolicy)

getPolicyVersionResponseTest :: GetPolicyVersionResponse -> TestTree
getPolicyVersionResponseTest = resp
    "getPolicyVersionResponse"
    "fixture/GetPolicyVersionResponse"
    (Proxy :: Proxy GetPolicyVersion)

getRoleResponseTest :: GetRoleResponse -> TestTree
getRoleResponseTest = resp
    "getRoleResponse"
    "fixture/GetRoleResponse"
    (Proxy :: Proxy GetRole)

getRolePolicyResponseTest :: GetRolePolicyResponse -> TestTree
getRolePolicyResponseTest = resp
    "getRolePolicyResponse"
    "fixture/GetRolePolicyResponse"
    (Proxy :: Proxy GetRolePolicy)

getSAMLProviderResponseTest :: GetSAMLProviderResponse -> TestTree
getSAMLProviderResponseTest = resp
    "getSAMLProviderResponse"
    "fixture/GetSAMLProviderResponse"
    (Proxy :: Proxy GetSAMLProvider)

getServerCertificateResponseTest :: GetServerCertificateResponse -> TestTree
getServerCertificateResponseTest = resp
    "getServerCertificateResponse"
    "fixture/GetServerCertificateResponse"
    (Proxy :: Proxy GetServerCertificate)

getUserResponseTest :: GetUserResponse -> TestTree
getUserResponseTest = resp
    "getUserResponse"
    "fixture/GetUserResponse"
    (Proxy :: Proxy GetUser)

getUserPolicyResponseTest :: GetUserPolicyResponse -> TestTree
getUserPolicyResponseTest = resp
    "getUserPolicyResponse"
    "fixture/GetUserPolicyResponse"
    (Proxy :: Proxy GetUserPolicy)

listAccessKeysResponseTest :: ListAccessKeysResponse -> TestTree
listAccessKeysResponseTest = resp
    "listAccessKeysResponse"
    "fixture/ListAccessKeysResponse"
    (Proxy :: Proxy ListAccessKeys)

listAccountAliasesResponseTest :: ListAccountAliasesResponse -> TestTree
listAccountAliasesResponseTest = resp
    "listAccountAliasesResponse"
    "fixture/ListAccountAliasesResponse"
    (Proxy :: Proxy ListAccountAliases)

listAttachedGroupPoliciesResponseTest :: ListAttachedGroupPoliciesResponse -> TestTree
listAttachedGroupPoliciesResponseTest = resp
    "listAttachedGroupPoliciesResponse"
    "fixture/ListAttachedGroupPoliciesResponse"
    (Proxy :: Proxy ListAttachedGroupPolicies)

listAttachedRolePoliciesResponseTest :: ListAttachedRolePoliciesResponse -> TestTree
listAttachedRolePoliciesResponseTest = resp
    "listAttachedRolePoliciesResponse"
    "fixture/ListAttachedRolePoliciesResponse"
    (Proxy :: Proxy ListAttachedRolePolicies)

listAttachedUserPoliciesResponseTest :: ListAttachedUserPoliciesResponse -> TestTree
listAttachedUserPoliciesResponseTest = resp
    "listAttachedUserPoliciesResponse"
    "fixture/ListAttachedUserPoliciesResponse"
    (Proxy :: Proxy ListAttachedUserPolicies)

listEntitiesForPolicyResponseTest :: ListEntitiesForPolicyResponse -> TestTree
listEntitiesForPolicyResponseTest = resp
    "listEntitiesForPolicyResponse"
    "fixture/ListEntitiesForPolicyResponse"
    (Proxy :: Proxy ListEntitiesForPolicy)

listGroupPoliciesResponseTest :: ListGroupPoliciesResponse -> TestTree
listGroupPoliciesResponseTest = resp
    "listGroupPoliciesResponse"
    "fixture/ListGroupPoliciesResponse"
    (Proxy :: Proxy ListGroupPolicies)

listGroupsResponseTest :: ListGroupsResponse -> TestTree
listGroupsResponseTest = resp
    "listGroupsResponse"
    "fixture/ListGroupsResponse"
    (Proxy :: Proxy ListGroups)

listGroupsForUserResponseTest :: ListGroupsForUserResponse -> TestTree
listGroupsForUserResponseTest = resp
    "listGroupsForUserResponse"
    "fixture/ListGroupsForUserResponse"
    (Proxy :: Proxy ListGroupsForUser)

listInstanceProfilesResponseTest :: ListInstanceProfilesResponse -> TestTree
listInstanceProfilesResponseTest = resp
    "listInstanceProfilesResponse"
    "fixture/ListInstanceProfilesResponse"
    (Proxy :: Proxy ListInstanceProfiles)

listInstanceProfilesForRoleResponseTest :: ListInstanceProfilesForRoleResponse -> TestTree
listInstanceProfilesForRoleResponseTest = resp
    "listInstanceProfilesForRoleResponse"
    "fixture/ListInstanceProfilesForRoleResponse"
    (Proxy :: Proxy ListInstanceProfilesForRole)

listMFADevicesResponseTest :: ListMFADevicesResponse -> TestTree
listMFADevicesResponseTest = resp
    "listMFADevicesResponse"
    "fixture/ListMFADevicesResponse"
    (Proxy :: Proxy ListMFADevices)

listOpenIDConnectProvidersResponseTest :: ListOpenIDConnectProvidersResponse -> TestTree
listOpenIDConnectProvidersResponseTest = resp
    "listOpenIDConnectProvidersResponse"
    "fixture/ListOpenIDConnectProvidersResponse"
    (Proxy :: Proxy ListOpenIDConnectProviders)

listPoliciesResponseTest :: ListPoliciesResponse -> TestTree
listPoliciesResponseTest = resp
    "listPoliciesResponse"
    "fixture/ListPoliciesResponse"
    (Proxy :: Proxy ListPolicies)

listPolicyVersionsResponseTest :: ListPolicyVersionsResponse -> TestTree
listPolicyVersionsResponseTest = resp
    "listPolicyVersionsResponse"
    "fixture/ListPolicyVersionsResponse"
    (Proxy :: Proxy ListPolicyVersions)

listRolePoliciesResponseTest :: ListRolePoliciesResponse -> TestTree
listRolePoliciesResponseTest = resp
    "listRolePoliciesResponse"
    "fixture/ListRolePoliciesResponse"
    (Proxy :: Proxy ListRolePolicies)

listRolesResponseTest :: ListRolesResponse -> TestTree
listRolesResponseTest = resp
    "listRolesResponse"
    "fixture/ListRolesResponse"
    (Proxy :: Proxy ListRoles)

listSAMLProvidersResponseTest :: ListSAMLProvidersResponse -> TestTree
listSAMLProvidersResponseTest = resp
    "listSAMLProvidersResponse"
    "fixture/ListSAMLProvidersResponse"
    (Proxy :: Proxy ListSAMLProviders)

listServerCertificatesResponseTest :: ListServerCertificatesResponse -> TestTree
listServerCertificatesResponseTest = resp
    "listServerCertificatesResponse"
    "fixture/ListServerCertificatesResponse"
    (Proxy :: Proxy ListServerCertificates)

listSigningCertificatesResponseTest :: ListSigningCertificatesResponse -> TestTree
listSigningCertificatesResponseTest = resp
    "listSigningCertificatesResponse"
    "fixture/ListSigningCertificatesResponse"
    (Proxy :: Proxy ListSigningCertificates)

listUserPoliciesResponseTest :: ListUserPoliciesResponse -> TestTree
listUserPoliciesResponseTest = resp
    "listUserPoliciesResponse"
    "fixture/ListUserPoliciesResponse"
    (Proxy :: Proxy ListUserPolicies)

listUsersResponseTest :: ListUsersResponse -> TestTree
listUsersResponseTest = resp
    "listUsersResponse"
    "fixture/ListUsersResponse"
    (Proxy :: Proxy ListUsers)

listVirtualMFADevicesResponseTest :: ListVirtualMFADevicesResponse -> TestTree
listVirtualMFADevicesResponseTest = resp
    "listVirtualMFADevicesResponse"
    "fixture/ListVirtualMFADevicesResponse"
    (Proxy :: Proxy ListVirtualMFADevices)

putGroupPolicyResponseTest :: PutGroupPolicyResponse -> TestTree
putGroupPolicyResponseTest = resp
    "putGroupPolicyResponse"
    "fixture/PutGroupPolicyResponse"
    (Proxy :: Proxy PutGroupPolicy)

putRolePolicyResponseTest :: PutRolePolicyResponse -> TestTree
putRolePolicyResponseTest = resp
    "putRolePolicyResponse"
    "fixture/PutRolePolicyResponse"
    (Proxy :: Proxy PutRolePolicy)

putUserPolicyResponseTest :: PutUserPolicyResponse -> TestTree
putUserPolicyResponseTest = resp
    "putUserPolicyResponse"
    "fixture/PutUserPolicyResponse"
    (Proxy :: Proxy PutUserPolicy)

removeClientIDFromOpenIDConnectProviderResponseTest :: RemoveClientIDFromOpenIDConnectProviderResponse -> TestTree
removeClientIDFromOpenIDConnectProviderResponseTest = resp
    "removeClientIDFromOpenIDConnectProviderResponse"
    "fixture/RemoveClientIDFromOpenIDConnectProviderResponse"
    (Proxy :: Proxy RemoveClientIDFromOpenIDConnectProvider)

removeRoleFromInstanceProfileResponseTest :: RemoveRoleFromInstanceProfileResponse -> TestTree
removeRoleFromInstanceProfileResponseTest = resp
    "removeRoleFromInstanceProfileResponse"
    "fixture/RemoveRoleFromInstanceProfileResponse"
    (Proxy :: Proxy RemoveRoleFromInstanceProfile)

removeUserFromGroupResponseTest :: RemoveUserFromGroupResponse -> TestTree
removeUserFromGroupResponseTest = resp
    "removeUserFromGroupResponse"
    "fixture/RemoveUserFromGroupResponse"
    (Proxy :: Proxy RemoveUserFromGroup)

resyncMFADeviceResponseTest :: ResyncMFADeviceResponse -> TestTree
resyncMFADeviceResponseTest = resp
    "resyncMFADeviceResponse"
    "fixture/ResyncMFADeviceResponse"
    (Proxy :: Proxy ResyncMFADevice)

setDefaultPolicyVersionResponseTest :: SetDefaultPolicyVersionResponse -> TestTree
setDefaultPolicyVersionResponseTest = resp
    "setDefaultPolicyVersionResponse"
    "fixture/SetDefaultPolicyVersionResponse"
    (Proxy :: Proxy SetDefaultPolicyVersion)

updateAccessKeyResponseTest :: UpdateAccessKeyResponse -> TestTree
updateAccessKeyResponseTest = resp
    "updateAccessKeyResponse"
    "fixture/UpdateAccessKeyResponse"
    (Proxy :: Proxy UpdateAccessKey)

updateAccountPasswordPolicyResponseTest :: UpdateAccountPasswordPolicyResponse -> TestTree
updateAccountPasswordPolicyResponseTest = resp
    "updateAccountPasswordPolicyResponse"
    "fixture/UpdateAccountPasswordPolicyResponse"
    (Proxy :: Proxy UpdateAccountPasswordPolicy)

updateAssumeRolePolicyResponseTest :: UpdateAssumeRolePolicyResponse -> TestTree
updateAssumeRolePolicyResponseTest = resp
    "updateAssumeRolePolicyResponse"
    "fixture/UpdateAssumeRolePolicyResponse"
    (Proxy :: Proxy UpdateAssumeRolePolicy)

updateGroupResponseTest :: UpdateGroupResponse -> TestTree
updateGroupResponseTest = resp
    "updateGroupResponse"
    "fixture/UpdateGroupResponse"
    (Proxy :: Proxy UpdateGroup)

updateLoginProfileResponseTest :: UpdateLoginProfileResponse -> TestTree
updateLoginProfileResponseTest = resp
    "updateLoginProfileResponse"
    "fixture/UpdateLoginProfileResponse"
    (Proxy :: Proxy UpdateLoginProfile)

updateOpenIDConnectProviderThumbprintResponseTest :: UpdateOpenIDConnectProviderThumbprintResponse -> TestTree
updateOpenIDConnectProviderThumbprintResponseTest = resp
    "updateOpenIDConnectProviderThumbprintResponse"
    "fixture/UpdateOpenIDConnectProviderThumbprintResponse"
    (Proxy :: Proxy UpdateOpenIDConnectProviderThumbprint)

updateSAMLProviderResponseTest :: UpdateSAMLProviderResponse -> TestTree
updateSAMLProviderResponseTest = resp
    "updateSAMLProviderResponse"
    "fixture/UpdateSAMLProviderResponse"
    (Proxy :: Proxy UpdateSAMLProvider)

updateServerCertificateResponseTest :: UpdateServerCertificateResponse -> TestTree
updateServerCertificateResponseTest = resp
    "updateServerCertificateResponse"
    "fixture/UpdateServerCertificateResponse"
    (Proxy :: Proxy UpdateServerCertificate)

updateSigningCertificateResponseTest :: UpdateSigningCertificateResponse -> TestTree
updateSigningCertificateResponseTest = resp
    "updateSigningCertificateResponse"
    "fixture/UpdateSigningCertificateResponse"
    (Proxy :: Proxy UpdateSigningCertificate)

updateUserResponseTest :: UpdateUserResponse -> TestTree
updateUserResponseTest = resp
    "updateUserResponse"
    "fixture/UpdateUserResponse"
    (Proxy :: Proxy UpdateUser)

uploadServerCertificateResponseTest :: UploadServerCertificateResponse -> TestTree
uploadServerCertificateResponseTest = resp
    "uploadServerCertificateResponse"
    "fixture/UploadServerCertificateResponse"
    (Proxy :: Proxy UploadServerCertificate)

uploadSigningCertificateResponseTest :: UploadSigningCertificateResponse -> TestTree
uploadSigningCertificateResponseTest = resp
    "uploadSigningCertificateResponse"
    "fixture/UploadSigningCertificateResponse"
    (Proxy :: Proxy UploadSigningCertificate)
