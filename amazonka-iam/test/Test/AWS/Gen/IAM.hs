{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.IAM
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--         [ requestCreateVirtualMFADevice $
--             newCreateVirtualMFADevice
--
--         , requestAttachRolePolicy $
--             newAttachRolePolicy
--
--         , requestDeleteSSHPublicKey $
--             newDeleteSSHPublicKey
--
--         , requestGetUser $
--             newGetUser
--
--         , requestUpdateSSHPublicKey $
--             newUpdateSSHPublicKey
--
--         , requestUntagOpenIDConnectProvider $
--             newUntagOpenIDConnectProvider
--
--         , requestListSigningCertificates $
--             newListSigningCertificates
--
--         , requestDeleteOpenIDConnectProvider $
--             newDeleteOpenIDConnectProvider
--
--         , requestListRoleTags $
--             newListRoleTags
--
--         , requestListOpenIDConnectProviders $
--             newListOpenIDConnectProviders
--
--         , requestCreatePolicy $
--             newCreatePolicy
--
--         , requestGetSAMLProvider $
--             newGetSAMLProvider
--
--         , requestGetContextKeysForPrincipalPolicy $
--             newGetContextKeysForPrincipalPolicy
--
--         , requestListEntitiesForPolicy $
--             newListEntitiesForPolicy
--
--         , requestListGroupsForUser $
--             newListGroupsForUser
--
--         , requestSimulatePrincipalPolicy $
--             newSimulatePrincipalPolicy
--
--         , requestListPolicies $
--             newListPolicies
--
--         , requestCreateServiceLinkedRole $
--             newCreateServiceLinkedRole
--
--         , requestUntagPolicy $
--             newUntagPolicy
--
--         , requestDeletePolicy $
--             newDeletePolicy
--
--         , requestDeleteServerCertificate $
--             newDeleteServerCertificate
--
--         , requestListAttachedGroupPolicies $
--             newListAttachedGroupPolicies
--
--         , requestChangePassword $
--             newChangePassword
--
--         , requestListMFADeviceTags $
--             newListMFADeviceTags
--
--         , requestUntagServerCertificate $
--             newUntagServerCertificate
--
--         , requestUpdateAssumeRolePolicy $
--             newUpdateAssumeRolePolicy
--
--         , requestGetGroupPolicy $
--             newGetGroupPolicy
--
--         , requestUpdateServerCertificate $
--             newUpdateServerCertificate
--
--         , requestListServerCertificates $
--             newListServerCertificates
--
--         , requestListInstanceProfileTags $
--             newListInstanceProfileTags
--
--         , requestDeleteGroupPolicy $
--             newDeleteGroupPolicy
--
--         , requestCreateInstanceProfile $
--             newCreateInstanceProfile
--
--         , requestListGroups $
--             newListGroups
--
--         , requestGetLoginProfile $
--             newGetLoginProfile
--
--         , requestTagRole $
--             newTagRole
--
--         , requestRemoveRoleFromInstanceProfile $
--             newRemoveRoleFromInstanceProfile
--
--         , requestGenerateCredentialReport $
--             newGenerateCredentialReport
--
--         , requestCreatePolicyVersion $
--             newCreatePolicyVersion
--
--         , requestGetServerCertificate $
--             newGetServerCertificate
--
--         , requestRemoveUserFromGroup $
--             newRemoveUserFromGroup
--
--         , requestSetDefaultPolicyVersion $
--             newSetDefaultPolicyVersion
--
--         , requestResetServiceSpecificCredential $
--             newResetServiceSpecificCredential
--
--         , requestGenerateServiceLastAccessedDetails $
--             newGenerateServiceLastAccessedDetails
--
--         , requestListPoliciesGrantingServiceAccess $
--             newListPoliciesGrantingServiceAccess
--
--         , requestUpdateRoleDescription $
--             newUpdateRoleDescription
--
--         , requestUploadServerCertificate $
--             newUploadServerCertificate
--
--         , requestDetachRolePolicy $
--             newDetachRolePolicy
--
--         , requestEnableMFADevice $
--             newEnableMFADevice
--
--         , requestListSAMLProviders $
--             newListSAMLProviders
--
--         , requestListPolicyTags $
--             newListPolicyTags
--
--         , requestCreateGroup $
--             newCreateGroup
--
--         , requestTagMFADevice $
--             newTagMFADevice
--
--         , requestTagInstanceProfile $
--             newTagInstanceProfile
--
--         , requestGetOpenIDConnectProvider $
--             newGetOpenIDConnectProvider
--
--         , requestCreateRole $
--             newCreateRole
--
--         , requestPutUserPermissionsBoundary $
--             newPutUserPermissionsBoundary
--
--         , requestDeleteUserPolicy $
--             newDeleteUserPolicy
--
--         , requestDeleteRolePermissionsBoundary $
--             newDeleteRolePermissionsBoundary
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestListOpenIDConnectProviderTags $
--             newListOpenIDConnectProviderTags
--
--         , requestListRoles $
--             newListRoles
--
--         , requestUploadSigningCertificate $
--             newUploadSigningCertificate
--
--         , requestDeleteRolePolicy $
--             newDeleteRolePolicy
--
--         , requestListAttachedRolePolicies $
--             newListAttachedRolePolicies
--
--         , requestGetRolePolicy $
--             newGetRolePolicy
--
--         , requestDeleteAccessKey $
--             newDeleteAccessKey
--
--         , requestListVirtualMFADevices $
--             newListVirtualMFADevices
--
--         , requestTagPolicy $
--             newTagPolicy
--
--         , requestRemoveClientIDFromOpenIDConnectProvider $
--             newRemoveClientIDFromOpenIDConnectProvider
--
--         , requestDeleteVirtualMFADevice $
--             newDeleteVirtualMFADevice
--
--         , requestUpdateAccessKey $
--             newUpdateAccessKey
--
--         , requestCreateServiceSpecificCredential $
--             newCreateServiceSpecificCredential
--
--         , requestResyncMFADevice $
--             newResyncMFADevice
--
--         , requestUpdateServiceSpecificCredential $
--             newUpdateServiceSpecificCredential
--
--         , requestGetUserPolicy $
--             newGetUserPolicy
--
--         , requestUpdateAccountPasswordPolicy $
--             newUpdateAccountPasswordPolicy
--
--         , requestListServiceSpecificCredentials $
--             newListServiceSpecificCredentials
--
--         , requestDeleteSigningCertificate $
--             newDeleteSigningCertificate
--
--         , requestListAttachedUserPolicies $
--             newListAttachedUserPolicies
--
--         , requestUpdateSigningCertificate $
--             newUpdateSigningCertificate
--
--         , requestListSSHPublicKeys $
--             newListSSHPublicKeys
--
--         , requestDeleteServiceSpecificCredential $
--             newDeleteServiceSpecificCredential
--
--         , requestCreateAccessKey $
--             newCreateAccessKey
--
--         , requestDeleteAccountPasswordPolicy $
--             newDeleteAccountPasswordPolicy
--
--         , requestGetOrganizationsAccessReport $
--             newGetOrganizationsAccessReport
--
--         , requestListInstanceProfilesForRole $
--             newListInstanceProfilesForRole
--
--         , requestGetPolicyVersion $
--             newGetPolicyVersion
--
--         , requestCreateLoginProfile $
--             newCreateLoginProfile
--
--         , requestAddRoleToInstanceProfile $
--             newAddRoleToInstanceProfile
--
--         , requestGetInstanceProfile $
--             newGetInstanceProfile
--
--         , requestTagOpenIDConnectProvider $
--             newTagOpenIDConnectProvider
--
--         , requestAddUserToGroup $
--             newAddUserToGroup
--
--         , requestAttachGroupPolicy $
--             newAttachGroupPolicy
--
--         , requestUpdateLoginProfile $
--             newUpdateLoginProfile
--
--         , requestListSAMLProviderTags $
--             newListSAMLProviderTags
--
--         , requestGetGroup $
--             newGetGroup
--
--         , requestDeleteLoginProfile $
--             newDeleteLoginProfile
--
--         , requestDeleteServiceLinkedRole $
--             newDeleteServiceLinkedRole
--
--         , requestGenerateOrganizationsAccessReport $
--             newGenerateOrganizationsAccessReport
--
--         , requestGetServiceLastAccessedDetailsWithEntities $
--             newGetServiceLastAccessedDetailsWithEntities
--
--         , requestPutGroupPolicy $
--             newPutGroupPolicy
--
--         , requestGetServiceLastAccessedDetails $
--             newGetServiceLastAccessedDetails
--
--         , requestDeleteAccountAlias $
--             newDeleteAccountAlias
--
--         , requestCreateSAMLProvider $
--             newCreateSAMLProvider
--
--         , requestGetPolicy $
--             newGetPolicy
--
--         , requestDetachUserPolicy $
--             newDetachUserPolicy
--
--         , requestUpdateGroup $
--             newUpdateGroup
--
--         , requestDeleteGroup $
--             newDeleteGroup
--
--         , requestGetServiceLinkedRoleDeletionStatus $
--             newGetServiceLinkedRoleDeletionStatus
--
--         , requestGetAccountAuthorizationDetails $
--             newGetAccountAuthorizationDetails
--
--         , requestListGroupPolicies $
--             newListGroupPolicies
--
--         , requestDeletePolicyVersion $
--             newDeletePolicyVersion
--
--         , requestDeleteSAMLProvider $
--             newDeleteSAMLProvider
--
--         , requestTagUser $
--             newTagUser
--
--         , requestListInstanceProfiles $
--             newListInstanceProfiles
--
--         , requestGetCredentialReport $
--             newGetCredentialReport
--
--         , requestListMFADevices $
--             newListMFADevices
--
--         , requestUpdateSAMLProvider $
--             newUpdateSAMLProvider
--
--         , requestUntagInstanceProfile $
--             newUntagInstanceProfile
--
--         , requestCreateAccountAlias $
--             newCreateAccountAlias
--
--         , requestUntagMFADevice $
--             newUntagMFADevice
--
--         , requestUntagSAMLProvider $
--             newUntagSAMLProvider
--
--         , requestListAccountAliases $
--             newListAccountAliases
--
--         , requestListPolicyVersions $
--             newListPolicyVersions
--
--         , requestDeleteInstanceProfile $
--             newDeleteInstanceProfile
--
--         , requestGetAccountSummary $
--             newGetAccountSummary
--
--         , requestListServerCertificateTags $
--             newListServerCertificateTags
--
--         , requestGetSSHPublicKey $
--             newGetSSHPublicKey
--
--         , requestUpdateOpenIDConnectProviderThumbprint $
--             newUpdateOpenIDConnectProviderThumbprint
--
--         , requestGetAccessKeyLastUsed $
--             newGetAccessKeyLastUsed
--
--         , requestTagSAMLProvider $
--             newTagSAMLProvider
--
--         , requestGetAccountPasswordPolicy $
--             newGetAccountPasswordPolicy
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestListUsers $
--             newListUsers
--
--         , requestUpdateUser $
--             newUpdateUser
--
--         , requestListRolePolicies $
--             newListRolePolicies
--
--         , requestAddClientIDToOpenIDConnectProvider $
--             newAddClientIDToOpenIDConnectProvider
--
--         , requestDeleteUserPermissionsBoundary $
--             newDeleteUserPermissionsBoundary
--
--         , requestPutUserPolicy $
--             newPutUserPolicy
--
--         , requestDetachGroupPolicy $
--             newDetachGroupPolicy
--
--         , requestUntagUser $
--             newUntagUser
--
--         , requestGetContextKeysForCustomPolicy $
--             newGetContextKeysForCustomPolicy
--
--         , requestPutRolePermissionsBoundary $
--             newPutRolePermissionsBoundary
--
--         , requestUntagRole $
--             newUntagRole
--
--         , requestSimulateCustomPolicy $
--             newSimulateCustomPolicy
--
--         , requestUploadSSHPublicKey $
--             newUploadSSHPublicKey
--
--         , requestDeleteRole $
--             newDeleteRole
--
--         , requestListUserPolicies $
--             newListUserPolicies
--
--         , requestPutRolePolicy $
--             newPutRolePolicy
--
--         , requestUpdateRole $
--             newUpdateRole
--
--         , requestSetSecurityTokenServicePreferences $
--             newSetSecurityTokenServicePreferences
--
--         , requestAttachUserPolicy $
--             newAttachUserPolicy
--
--         , requestTagServerCertificate $
--             newTagServerCertificate
--
--         , requestListAccessKeys $
--             newListAccessKeys
--
--         , requestCreateOpenIDConnectProvider $
--             newCreateOpenIDConnectProvider
--
--         , requestDeactivateMFADevice $
--             newDeactivateMFADevice
--
--         , requestListUserTags $
--             newListUserTags
--
--         , requestGetRole $
--             newGetRole
--
--           ]

--     , testGroup "response"
--         [ responseCreateVirtualMFADevice $
--             newCreateVirtualMFADeviceResponse
--
--         , responseAttachRolePolicy $
--             newAttachRolePolicyResponse
--
--         , responseDeleteSSHPublicKey $
--             newDeleteSSHPublicKeyResponse
--
--         , responseGetUser $
--             newGetUserResponse
--
--         , responseUpdateSSHPublicKey $
--             newUpdateSSHPublicKeyResponse
--
--         , responseUntagOpenIDConnectProvider $
--             newUntagOpenIDConnectProviderResponse
--
--         , responseListSigningCertificates $
--             newListSigningCertificatesResponse
--
--         , responseDeleteOpenIDConnectProvider $
--             newDeleteOpenIDConnectProviderResponse
--
--         , responseListRoleTags $
--             newListRoleTagsResponse
--
--         , responseListOpenIDConnectProviders $
--             newListOpenIDConnectProvidersResponse
--
--         , responseCreatePolicy $
--             newCreatePolicyResponse
--
--         , responseGetSAMLProvider $
--             newGetSAMLProviderResponse
--
--         , responseGetContextKeysForPrincipalPolicy $
--             newGetContextKeysForPolicyResponse
--
--         , responseListEntitiesForPolicy $
--             newListEntitiesForPolicyResponse
--
--         , responseListGroupsForUser $
--             newListGroupsForUserResponse
--
--         , responseSimulatePrincipalPolicy $
--             newSimulatePolicyResponse
--
--         , responseListPolicies $
--             newListPoliciesResponse
--
--         , responseCreateServiceLinkedRole $
--             newCreateServiceLinkedRoleResponse
--
--         , responseUntagPolicy $
--             newUntagPolicyResponse
--
--         , responseDeletePolicy $
--             newDeletePolicyResponse
--
--         , responseDeleteServerCertificate $
--             newDeleteServerCertificateResponse
--
--         , responseListAttachedGroupPolicies $
--             newListAttachedGroupPoliciesResponse
--
--         , responseChangePassword $
--             newChangePasswordResponse
--
--         , responseListMFADeviceTags $
--             newListMFADeviceTagsResponse
--
--         , responseUntagServerCertificate $
--             newUntagServerCertificateResponse
--
--         , responseUpdateAssumeRolePolicy $
--             newUpdateAssumeRolePolicyResponse
--
--         , responseGetGroupPolicy $
--             newGetGroupPolicyResponse
--
--         , responseUpdateServerCertificate $
--             newUpdateServerCertificateResponse
--
--         , responseListServerCertificates $
--             newListServerCertificatesResponse
--
--         , responseListInstanceProfileTags $
--             newListInstanceProfileTagsResponse
--
--         , responseDeleteGroupPolicy $
--             newDeleteGroupPolicyResponse
--
--         , responseCreateInstanceProfile $
--             newCreateInstanceProfileResponse
--
--         , responseListGroups $
--             newListGroupsResponse
--
--         , responseGetLoginProfile $
--             newGetLoginProfileResponse
--
--         , responseTagRole $
--             newTagRoleResponse
--
--         , responseRemoveRoleFromInstanceProfile $
--             newRemoveRoleFromInstanceProfileResponse
--
--         , responseGenerateCredentialReport $
--             newGenerateCredentialReportResponse
--
--         , responseCreatePolicyVersion $
--             newCreatePolicyVersionResponse
--
--         , responseGetServerCertificate $
--             newGetServerCertificateResponse
--
--         , responseRemoveUserFromGroup $
--             newRemoveUserFromGroupResponse
--
--         , responseSetDefaultPolicyVersion $
--             newSetDefaultPolicyVersionResponse
--
--         , responseResetServiceSpecificCredential $
--             newResetServiceSpecificCredentialResponse
--
--         , responseGenerateServiceLastAccessedDetails $
--             newGenerateServiceLastAccessedDetailsResponse
--
--         , responseListPoliciesGrantingServiceAccess $
--             newListPoliciesGrantingServiceAccessResponse
--
--         , responseUpdateRoleDescription $
--             newUpdateRoleDescriptionResponse
--
--         , responseUploadServerCertificate $
--             newUploadServerCertificateResponse
--
--         , responseDetachRolePolicy $
--             newDetachRolePolicyResponse
--
--         , responseEnableMFADevice $
--             newEnableMFADeviceResponse
--
--         , responseListSAMLProviders $
--             newListSAMLProvidersResponse
--
--         , responseListPolicyTags $
--             newListPolicyTagsResponse
--
--         , responseCreateGroup $
--             newCreateGroupResponse
--
--         , responseTagMFADevice $
--             newTagMFADeviceResponse
--
--         , responseTagInstanceProfile $
--             newTagInstanceProfileResponse
--
--         , responseGetOpenIDConnectProvider $
--             newGetOpenIDConnectProviderResponse
--
--         , responseCreateRole $
--             newCreateRoleResponse
--
--         , responsePutUserPermissionsBoundary $
--             newPutUserPermissionsBoundaryResponse
--
--         , responseDeleteUserPolicy $
--             newDeleteUserPolicyResponse
--
--         , responseDeleteRolePermissionsBoundary $
--             newDeleteRolePermissionsBoundaryResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseListOpenIDConnectProviderTags $
--             newListOpenIDConnectProviderTagsResponse
--
--         , responseListRoles $
--             newListRolesResponse
--
--         , responseUploadSigningCertificate $
--             newUploadSigningCertificateResponse
--
--         , responseDeleteRolePolicy $
--             newDeleteRolePolicyResponse
--
--         , responseListAttachedRolePolicies $
--             newListAttachedRolePoliciesResponse
--
--         , responseGetRolePolicy $
--             newGetRolePolicyResponse
--
--         , responseDeleteAccessKey $
--             newDeleteAccessKeyResponse
--
--         , responseListVirtualMFADevices $
--             newListVirtualMFADevicesResponse
--
--         , responseTagPolicy $
--             newTagPolicyResponse
--
--         , responseRemoveClientIDFromOpenIDConnectProvider $
--             newRemoveClientIDFromOpenIDConnectProviderResponse
--
--         , responseDeleteVirtualMFADevice $
--             newDeleteVirtualMFADeviceResponse
--
--         , responseUpdateAccessKey $
--             newUpdateAccessKeyResponse
--
--         , responseCreateServiceSpecificCredential $
--             newCreateServiceSpecificCredentialResponse
--
--         , responseResyncMFADevice $
--             newResyncMFADeviceResponse
--
--         , responseUpdateServiceSpecificCredential $
--             newUpdateServiceSpecificCredentialResponse
--
--         , responseGetUserPolicy $
--             newGetUserPolicyResponse
--
--         , responseUpdateAccountPasswordPolicy $
--             newUpdateAccountPasswordPolicyResponse
--
--         , responseListServiceSpecificCredentials $
--             newListServiceSpecificCredentialsResponse
--
--         , responseDeleteSigningCertificate $
--             newDeleteSigningCertificateResponse
--
--         , responseListAttachedUserPolicies $
--             newListAttachedUserPoliciesResponse
--
--         , responseUpdateSigningCertificate $
--             newUpdateSigningCertificateResponse
--
--         , responseListSSHPublicKeys $
--             newListSSHPublicKeysResponse
--
--         , responseDeleteServiceSpecificCredential $
--             newDeleteServiceSpecificCredentialResponse
--
--         , responseCreateAccessKey $
--             newCreateAccessKeyResponse
--
--         , responseDeleteAccountPasswordPolicy $
--             newDeleteAccountPasswordPolicyResponse
--
--         , responseGetOrganizationsAccessReport $
--             newGetOrganizationsAccessReportResponse
--
--         , responseListInstanceProfilesForRole $
--             newListInstanceProfilesForRoleResponse
--
--         , responseGetPolicyVersion $
--             newGetPolicyVersionResponse
--
--         , responseCreateLoginProfile $
--             newCreateLoginProfileResponse
--
--         , responseAddRoleToInstanceProfile $
--             newAddRoleToInstanceProfileResponse
--
--         , responseGetInstanceProfile $
--             newGetInstanceProfileResponse
--
--         , responseTagOpenIDConnectProvider $
--             newTagOpenIDConnectProviderResponse
--
--         , responseAddUserToGroup $
--             newAddUserToGroupResponse
--
--         , responseAttachGroupPolicy $
--             newAttachGroupPolicyResponse
--
--         , responseUpdateLoginProfile $
--             newUpdateLoginProfileResponse
--
--         , responseListSAMLProviderTags $
--             newListSAMLProviderTagsResponse
--
--         , responseGetGroup $
--             newGetGroupResponse
--
--         , responseDeleteLoginProfile $
--             newDeleteLoginProfileResponse
--
--         , responseDeleteServiceLinkedRole $
--             newDeleteServiceLinkedRoleResponse
--
--         , responseGenerateOrganizationsAccessReport $
--             newGenerateOrganizationsAccessReportResponse
--
--         , responseGetServiceLastAccessedDetailsWithEntities $
--             newGetServiceLastAccessedDetailsWithEntitiesResponse
--
--         , responsePutGroupPolicy $
--             newPutGroupPolicyResponse
--
--         , responseGetServiceLastAccessedDetails $
--             newGetServiceLastAccessedDetailsResponse
--
--         , responseDeleteAccountAlias $
--             newDeleteAccountAliasResponse
--
--         , responseCreateSAMLProvider $
--             newCreateSAMLProviderResponse
--
--         , responseGetPolicy $
--             newGetPolicyResponse
--
--         , responseDetachUserPolicy $
--             newDetachUserPolicyResponse
--
--         , responseUpdateGroup $
--             newUpdateGroupResponse
--
--         , responseDeleteGroup $
--             newDeleteGroupResponse
--
--         , responseGetServiceLinkedRoleDeletionStatus $
--             newGetServiceLinkedRoleDeletionStatusResponse
--
--         , responseGetAccountAuthorizationDetails $
--             newGetAccountAuthorizationDetailsResponse
--
--         , responseListGroupPolicies $
--             newListGroupPoliciesResponse
--
--         , responseDeletePolicyVersion $
--             newDeletePolicyVersionResponse
--
--         , responseDeleteSAMLProvider $
--             newDeleteSAMLProviderResponse
--
--         , responseTagUser $
--             newTagUserResponse
--
--         , responseListInstanceProfiles $
--             newListInstanceProfilesResponse
--
--         , responseGetCredentialReport $
--             newGetCredentialReportResponse
--
--         , responseListMFADevices $
--             newListMFADevicesResponse
--
--         , responseUpdateSAMLProvider $
--             newUpdateSAMLProviderResponse
--
--         , responseUntagInstanceProfile $
--             newUntagInstanceProfileResponse
--
--         , responseCreateAccountAlias $
--             newCreateAccountAliasResponse
--
--         , responseUntagMFADevice $
--             newUntagMFADeviceResponse
--
--         , responseUntagSAMLProvider $
--             newUntagSAMLProviderResponse
--
--         , responseListAccountAliases $
--             newListAccountAliasesResponse
--
--         , responseListPolicyVersions $
--             newListPolicyVersionsResponse
--
--         , responseDeleteInstanceProfile $
--             newDeleteInstanceProfileResponse
--
--         , responseGetAccountSummary $
--             newGetAccountSummaryResponse
--
--         , responseListServerCertificateTags $
--             newListServerCertificateTagsResponse
--
--         , responseGetSSHPublicKey $
--             newGetSSHPublicKeyResponse
--
--         , responseUpdateOpenIDConnectProviderThumbprint $
--             newUpdateOpenIDConnectProviderThumbprintResponse
--
--         , responseGetAccessKeyLastUsed $
--             newGetAccessKeyLastUsedResponse
--
--         , responseTagSAMLProvider $
--             newTagSAMLProviderResponse
--
--         , responseGetAccountPasswordPolicy $
--             newGetAccountPasswordPolicyResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseListUsers $
--             newListUsersResponse
--
--         , responseUpdateUser $
--             newUpdateUserResponse
--
--         , responseListRolePolicies $
--             newListRolePoliciesResponse
--
--         , responseAddClientIDToOpenIDConnectProvider $
--             newAddClientIDToOpenIDConnectProviderResponse
--
--         , responseDeleteUserPermissionsBoundary $
--             newDeleteUserPermissionsBoundaryResponse
--
--         , responsePutUserPolicy $
--             newPutUserPolicyResponse
--
--         , responseDetachGroupPolicy $
--             newDetachGroupPolicyResponse
--
--         , responseUntagUser $
--             newUntagUserResponse
--
--         , responseGetContextKeysForCustomPolicy $
--             newGetContextKeysForPolicyResponse
--
--         , responsePutRolePermissionsBoundary $
--             newPutRolePermissionsBoundaryResponse
--
--         , responseUntagRole $
--             newUntagRoleResponse
--
--         , responseSimulateCustomPolicy $
--             newSimulatePolicyResponse
--
--         , responseUploadSSHPublicKey $
--             newUploadSSHPublicKeyResponse
--
--         , responseDeleteRole $
--             newDeleteRoleResponse
--
--         , responseListUserPolicies $
--             newListUserPoliciesResponse
--
--         , responsePutRolePolicy $
--             newPutRolePolicyResponse
--
--         , responseUpdateRole $
--             newUpdateRoleResponse
--
--         , responseSetSecurityTokenServicePreferences $
--             newSetSecurityTokenServicePreferencesResponse
--
--         , responseAttachUserPolicy $
--             newAttachUserPolicyResponse
--
--         , responseTagServerCertificate $
--             newTagServerCertificateResponse
--
--         , responseListAccessKeys $
--             newListAccessKeysResponse
--
--         , responseCreateOpenIDConnectProvider $
--             newCreateOpenIDConnectProviderResponse
--
--         , responseDeactivateMFADevice $
--             newDeactivateMFADeviceResponse
--
--         , responseListUserTags $
--             newListUserTagsResponse
--
--         , responseGetRole $
--             newGetRoleResponse
--
--           ]
--     ]

-- Requests

requestCreateVirtualMFADevice :: CreateVirtualMFADevice -> TestTree
requestCreateVirtualMFADevice =
  req
    "CreateVirtualMFADevice"
    "fixture/CreateVirtualMFADevice.yaml"

requestAttachRolePolicy :: AttachRolePolicy -> TestTree
requestAttachRolePolicy =
  req
    "AttachRolePolicy"
    "fixture/AttachRolePolicy.yaml"

requestDeleteSSHPublicKey :: DeleteSSHPublicKey -> TestTree
requestDeleteSSHPublicKey =
  req
    "DeleteSSHPublicKey"
    "fixture/DeleteSSHPublicKey.yaml"

requestGetUser :: GetUser -> TestTree
requestGetUser =
  req
    "GetUser"
    "fixture/GetUser.yaml"

requestUpdateSSHPublicKey :: UpdateSSHPublicKey -> TestTree
requestUpdateSSHPublicKey =
  req
    "UpdateSSHPublicKey"
    "fixture/UpdateSSHPublicKey.yaml"

requestUntagOpenIDConnectProvider :: UntagOpenIDConnectProvider -> TestTree
requestUntagOpenIDConnectProvider =
  req
    "UntagOpenIDConnectProvider"
    "fixture/UntagOpenIDConnectProvider.yaml"

requestListSigningCertificates :: ListSigningCertificates -> TestTree
requestListSigningCertificates =
  req
    "ListSigningCertificates"
    "fixture/ListSigningCertificates.yaml"

requestDeleteOpenIDConnectProvider :: DeleteOpenIDConnectProvider -> TestTree
requestDeleteOpenIDConnectProvider =
  req
    "DeleteOpenIDConnectProvider"
    "fixture/DeleteOpenIDConnectProvider.yaml"

requestListRoleTags :: ListRoleTags -> TestTree
requestListRoleTags =
  req
    "ListRoleTags"
    "fixture/ListRoleTags.yaml"

requestListOpenIDConnectProviders :: ListOpenIDConnectProviders -> TestTree
requestListOpenIDConnectProviders =
  req
    "ListOpenIDConnectProviders"
    "fixture/ListOpenIDConnectProviders.yaml"

requestCreatePolicy :: CreatePolicy -> TestTree
requestCreatePolicy =
  req
    "CreatePolicy"
    "fixture/CreatePolicy.yaml"

requestGetSAMLProvider :: GetSAMLProvider -> TestTree
requestGetSAMLProvider =
  req
    "GetSAMLProvider"
    "fixture/GetSAMLProvider.yaml"

requestGetContextKeysForPrincipalPolicy :: GetContextKeysForPrincipalPolicy -> TestTree
requestGetContextKeysForPrincipalPolicy =
  req
    "GetContextKeysForPrincipalPolicy"
    "fixture/GetContextKeysForPrincipalPolicy.yaml"

requestListEntitiesForPolicy :: ListEntitiesForPolicy -> TestTree
requestListEntitiesForPolicy =
  req
    "ListEntitiesForPolicy"
    "fixture/ListEntitiesForPolicy.yaml"

requestListGroupsForUser :: ListGroupsForUser -> TestTree
requestListGroupsForUser =
  req
    "ListGroupsForUser"
    "fixture/ListGroupsForUser.yaml"

requestSimulatePrincipalPolicy :: SimulatePrincipalPolicy -> TestTree
requestSimulatePrincipalPolicy =
  req
    "SimulatePrincipalPolicy"
    "fixture/SimulatePrincipalPolicy.yaml"

requestListPolicies :: ListPolicies -> TestTree
requestListPolicies =
  req
    "ListPolicies"
    "fixture/ListPolicies.yaml"

requestCreateServiceLinkedRole :: CreateServiceLinkedRole -> TestTree
requestCreateServiceLinkedRole =
  req
    "CreateServiceLinkedRole"
    "fixture/CreateServiceLinkedRole.yaml"

requestUntagPolicy :: UntagPolicy -> TestTree
requestUntagPolicy =
  req
    "UntagPolicy"
    "fixture/UntagPolicy.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy =
  req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

requestDeleteServerCertificate :: DeleteServerCertificate -> TestTree
requestDeleteServerCertificate =
  req
    "DeleteServerCertificate"
    "fixture/DeleteServerCertificate.yaml"

requestListAttachedGroupPolicies :: ListAttachedGroupPolicies -> TestTree
requestListAttachedGroupPolicies =
  req
    "ListAttachedGroupPolicies"
    "fixture/ListAttachedGroupPolicies.yaml"

requestChangePassword :: ChangePassword -> TestTree
requestChangePassword =
  req
    "ChangePassword"
    "fixture/ChangePassword.yaml"

requestListMFADeviceTags :: ListMFADeviceTags -> TestTree
requestListMFADeviceTags =
  req
    "ListMFADeviceTags"
    "fixture/ListMFADeviceTags.yaml"

requestUntagServerCertificate :: UntagServerCertificate -> TestTree
requestUntagServerCertificate =
  req
    "UntagServerCertificate"
    "fixture/UntagServerCertificate.yaml"

requestUpdateAssumeRolePolicy :: UpdateAssumeRolePolicy -> TestTree
requestUpdateAssumeRolePolicy =
  req
    "UpdateAssumeRolePolicy"
    "fixture/UpdateAssumeRolePolicy.yaml"

requestGetGroupPolicy :: GetGroupPolicy -> TestTree
requestGetGroupPolicy =
  req
    "GetGroupPolicy"
    "fixture/GetGroupPolicy.yaml"

requestUpdateServerCertificate :: UpdateServerCertificate -> TestTree
requestUpdateServerCertificate =
  req
    "UpdateServerCertificate"
    "fixture/UpdateServerCertificate.yaml"

requestListServerCertificates :: ListServerCertificates -> TestTree
requestListServerCertificates =
  req
    "ListServerCertificates"
    "fixture/ListServerCertificates.yaml"

requestListInstanceProfileTags :: ListInstanceProfileTags -> TestTree
requestListInstanceProfileTags =
  req
    "ListInstanceProfileTags"
    "fixture/ListInstanceProfileTags.yaml"

requestDeleteGroupPolicy :: DeleteGroupPolicy -> TestTree
requestDeleteGroupPolicy =
  req
    "DeleteGroupPolicy"
    "fixture/DeleteGroupPolicy.yaml"

requestCreateInstanceProfile :: CreateInstanceProfile -> TestTree
requestCreateInstanceProfile =
  req
    "CreateInstanceProfile"
    "fixture/CreateInstanceProfile.yaml"

requestListGroups :: ListGroups -> TestTree
requestListGroups =
  req
    "ListGroups"
    "fixture/ListGroups.yaml"

requestGetLoginProfile :: GetLoginProfile -> TestTree
requestGetLoginProfile =
  req
    "GetLoginProfile"
    "fixture/GetLoginProfile.yaml"

requestTagRole :: TagRole -> TestTree
requestTagRole =
  req
    "TagRole"
    "fixture/TagRole.yaml"

requestRemoveRoleFromInstanceProfile :: RemoveRoleFromInstanceProfile -> TestTree
requestRemoveRoleFromInstanceProfile =
  req
    "RemoveRoleFromInstanceProfile"
    "fixture/RemoveRoleFromInstanceProfile.yaml"

requestGenerateCredentialReport :: GenerateCredentialReport -> TestTree
requestGenerateCredentialReport =
  req
    "GenerateCredentialReport"
    "fixture/GenerateCredentialReport.yaml"

requestCreatePolicyVersion :: CreatePolicyVersion -> TestTree
requestCreatePolicyVersion =
  req
    "CreatePolicyVersion"
    "fixture/CreatePolicyVersion.yaml"

requestGetServerCertificate :: GetServerCertificate -> TestTree
requestGetServerCertificate =
  req
    "GetServerCertificate"
    "fixture/GetServerCertificate.yaml"

requestRemoveUserFromGroup :: RemoveUserFromGroup -> TestTree
requestRemoveUserFromGroup =
  req
    "RemoveUserFromGroup"
    "fixture/RemoveUserFromGroup.yaml"

requestSetDefaultPolicyVersion :: SetDefaultPolicyVersion -> TestTree
requestSetDefaultPolicyVersion =
  req
    "SetDefaultPolicyVersion"
    "fixture/SetDefaultPolicyVersion.yaml"

requestResetServiceSpecificCredential :: ResetServiceSpecificCredential -> TestTree
requestResetServiceSpecificCredential =
  req
    "ResetServiceSpecificCredential"
    "fixture/ResetServiceSpecificCredential.yaml"

requestGenerateServiceLastAccessedDetails :: GenerateServiceLastAccessedDetails -> TestTree
requestGenerateServiceLastAccessedDetails =
  req
    "GenerateServiceLastAccessedDetails"
    "fixture/GenerateServiceLastAccessedDetails.yaml"

requestListPoliciesGrantingServiceAccess :: ListPoliciesGrantingServiceAccess -> TestTree
requestListPoliciesGrantingServiceAccess =
  req
    "ListPoliciesGrantingServiceAccess"
    "fixture/ListPoliciesGrantingServiceAccess.yaml"

requestUpdateRoleDescription :: UpdateRoleDescription -> TestTree
requestUpdateRoleDescription =
  req
    "UpdateRoleDescription"
    "fixture/UpdateRoleDescription.yaml"

requestUploadServerCertificate :: UploadServerCertificate -> TestTree
requestUploadServerCertificate =
  req
    "UploadServerCertificate"
    "fixture/UploadServerCertificate.yaml"

requestDetachRolePolicy :: DetachRolePolicy -> TestTree
requestDetachRolePolicy =
  req
    "DetachRolePolicy"
    "fixture/DetachRolePolicy.yaml"

requestEnableMFADevice :: EnableMFADevice -> TestTree
requestEnableMFADevice =
  req
    "EnableMFADevice"
    "fixture/EnableMFADevice.yaml"

requestListSAMLProviders :: ListSAMLProviders -> TestTree
requestListSAMLProviders =
  req
    "ListSAMLProviders"
    "fixture/ListSAMLProviders.yaml"

requestListPolicyTags :: ListPolicyTags -> TestTree
requestListPolicyTags =
  req
    "ListPolicyTags"
    "fixture/ListPolicyTags.yaml"

requestCreateGroup :: CreateGroup -> TestTree
requestCreateGroup =
  req
    "CreateGroup"
    "fixture/CreateGroup.yaml"

requestTagMFADevice :: TagMFADevice -> TestTree
requestTagMFADevice =
  req
    "TagMFADevice"
    "fixture/TagMFADevice.yaml"

requestTagInstanceProfile :: TagInstanceProfile -> TestTree
requestTagInstanceProfile =
  req
    "TagInstanceProfile"
    "fixture/TagInstanceProfile.yaml"

requestGetOpenIDConnectProvider :: GetOpenIDConnectProvider -> TestTree
requestGetOpenIDConnectProvider =
  req
    "GetOpenIDConnectProvider"
    "fixture/GetOpenIDConnectProvider.yaml"

requestCreateRole :: CreateRole -> TestTree
requestCreateRole =
  req
    "CreateRole"
    "fixture/CreateRole.yaml"

requestPutUserPermissionsBoundary :: PutUserPermissionsBoundary -> TestTree
requestPutUserPermissionsBoundary =
  req
    "PutUserPermissionsBoundary"
    "fixture/PutUserPermissionsBoundary.yaml"

requestDeleteUserPolicy :: DeleteUserPolicy -> TestTree
requestDeleteUserPolicy =
  req
    "DeleteUserPolicy"
    "fixture/DeleteUserPolicy.yaml"

requestDeleteRolePermissionsBoundary :: DeleteRolePermissionsBoundary -> TestTree
requestDeleteRolePermissionsBoundary =
  req
    "DeleteRolePermissionsBoundary"
    "fixture/DeleteRolePermissionsBoundary.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestListOpenIDConnectProviderTags :: ListOpenIDConnectProviderTags -> TestTree
requestListOpenIDConnectProviderTags =
  req
    "ListOpenIDConnectProviderTags"
    "fixture/ListOpenIDConnectProviderTags.yaml"

requestListRoles :: ListRoles -> TestTree
requestListRoles =
  req
    "ListRoles"
    "fixture/ListRoles.yaml"

requestUploadSigningCertificate :: UploadSigningCertificate -> TestTree
requestUploadSigningCertificate =
  req
    "UploadSigningCertificate"
    "fixture/UploadSigningCertificate.yaml"

requestDeleteRolePolicy :: DeleteRolePolicy -> TestTree
requestDeleteRolePolicy =
  req
    "DeleteRolePolicy"
    "fixture/DeleteRolePolicy.yaml"

requestListAttachedRolePolicies :: ListAttachedRolePolicies -> TestTree
requestListAttachedRolePolicies =
  req
    "ListAttachedRolePolicies"
    "fixture/ListAttachedRolePolicies.yaml"

requestGetRolePolicy :: GetRolePolicy -> TestTree
requestGetRolePolicy =
  req
    "GetRolePolicy"
    "fixture/GetRolePolicy.yaml"

requestDeleteAccessKey :: DeleteAccessKey -> TestTree
requestDeleteAccessKey =
  req
    "DeleteAccessKey"
    "fixture/DeleteAccessKey.yaml"

requestListVirtualMFADevices :: ListVirtualMFADevices -> TestTree
requestListVirtualMFADevices =
  req
    "ListVirtualMFADevices"
    "fixture/ListVirtualMFADevices.yaml"

requestTagPolicy :: TagPolicy -> TestTree
requestTagPolicy =
  req
    "TagPolicy"
    "fixture/TagPolicy.yaml"

requestRemoveClientIDFromOpenIDConnectProvider :: RemoveClientIDFromOpenIDConnectProvider -> TestTree
requestRemoveClientIDFromOpenIDConnectProvider =
  req
    "RemoveClientIDFromOpenIDConnectProvider"
    "fixture/RemoveClientIDFromOpenIDConnectProvider.yaml"

requestDeleteVirtualMFADevice :: DeleteVirtualMFADevice -> TestTree
requestDeleteVirtualMFADevice =
  req
    "DeleteVirtualMFADevice"
    "fixture/DeleteVirtualMFADevice.yaml"

requestUpdateAccessKey :: UpdateAccessKey -> TestTree
requestUpdateAccessKey =
  req
    "UpdateAccessKey"
    "fixture/UpdateAccessKey.yaml"

requestCreateServiceSpecificCredential :: CreateServiceSpecificCredential -> TestTree
requestCreateServiceSpecificCredential =
  req
    "CreateServiceSpecificCredential"
    "fixture/CreateServiceSpecificCredential.yaml"

requestResyncMFADevice :: ResyncMFADevice -> TestTree
requestResyncMFADevice =
  req
    "ResyncMFADevice"
    "fixture/ResyncMFADevice.yaml"

requestUpdateServiceSpecificCredential :: UpdateServiceSpecificCredential -> TestTree
requestUpdateServiceSpecificCredential =
  req
    "UpdateServiceSpecificCredential"
    "fixture/UpdateServiceSpecificCredential.yaml"

requestGetUserPolicy :: GetUserPolicy -> TestTree
requestGetUserPolicy =
  req
    "GetUserPolicy"
    "fixture/GetUserPolicy.yaml"

requestUpdateAccountPasswordPolicy :: UpdateAccountPasswordPolicy -> TestTree
requestUpdateAccountPasswordPolicy =
  req
    "UpdateAccountPasswordPolicy"
    "fixture/UpdateAccountPasswordPolicy.yaml"

requestListServiceSpecificCredentials :: ListServiceSpecificCredentials -> TestTree
requestListServiceSpecificCredentials =
  req
    "ListServiceSpecificCredentials"
    "fixture/ListServiceSpecificCredentials.yaml"

requestDeleteSigningCertificate :: DeleteSigningCertificate -> TestTree
requestDeleteSigningCertificate =
  req
    "DeleteSigningCertificate"
    "fixture/DeleteSigningCertificate.yaml"

requestListAttachedUserPolicies :: ListAttachedUserPolicies -> TestTree
requestListAttachedUserPolicies =
  req
    "ListAttachedUserPolicies"
    "fixture/ListAttachedUserPolicies.yaml"

requestUpdateSigningCertificate :: UpdateSigningCertificate -> TestTree
requestUpdateSigningCertificate =
  req
    "UpdateSigningCertificate"
    "fixture/UpdateSigningCertificate.yaml"

requestListSSHPublicKeys :: ListSSHPublicKeys -> TestTree
requestListSSHPublicKeys =
  req
    "ListSSHPublicKeys"
    "fixture/ListSSHPublicKeys.yaml"

requestDeleteServiceSpecificCredential :: DeleteServiceSpecificCredential -> TestTree
requestDeleteServiceSpecificCredential =
  req
    "DeleteServiceSpecificCredential"
    "fixture/DeleteServiceSpecificCredential.yaml"

requestCreateAccessKey :: CreateAccessKey -> TestTree
requestCreateAccessKey =
  req
    "CreateAccessKey"
    "fixture/CreateAccessKey.yaml"

requestDeleteAccountPasswordPolicy :: DeleteAccountPasswordPolicy -> TestTree
requestDeleteAccountPasswordPolicy =
  req
    "DeleteAccountPasswordPolicy"
    "fixture/DeleteAccountPasswordPolicy.yaml"

requestGetOrganizationsAccessReport :: GetOrganizationsAccessReport -> TestTree
requestGetOrganizationsAccessReport =
  req
    "GetOrganizationsAccessReport"
    "fixture/GetOrganizationsAccessReport.yaml"

requestListInstanceProfilesForRole :: ListInstanceProfilesForRole -> TestTree
requestListInstanceProfilesForRole =
  req
    "ListInstanceProfilesForRole"
    "fixture/ListInstanceProfilesForRole.yaml"

requestGetPolicyVersion :: GetPolicyVersion -> TestTree
requestGetPolicyVersion =
  req
    "GetPolicyVersion"
    "fixture/GetPolicyVersion.yaml"

requestCreateLoginProfile :: CreateLoginProfile -> TestTree
requestCreateLoginProfile =
  req
    "CreateLoginProfile"
    "fixture/CreateLoginProfile.yaml"

requestAddRoleToInstanceProfile :: AddRoleToInstanceProfile -> TestTree
requestAddRoleToInstanceProfile =
  req
    "AddRoleToInstanceProfile"
    "fixture/AddRoleToInstanceProfile.yaml"

requestGetInstanceProfile :: GetInstanceProfile -> TestTree
requestGetInstanceProfile =
  req
    "GetInstanceProfile"
    "fixture/GetInstanceProfile.yaml"

requestTagOpenIDConnectProvider :: TagOpenIDConnectProvider -> TestTree
requestTagOpenIDConnectProvider =
  req
    "TagOpenIDConnectProvider"
    "fixture/TagOpenIDConnectProvider.yaml"

requestAddUserToGroup :: AddUserToGroup -> TestTree
requestAddUserToGroup =
  req
    "AddUserToGroup"
    "fixture/AddUserToGroup.yaml"

requestAttachGroupPolicy :: AttachGroupPolicy -> TestTree
requestAttachGroupPolicy =
  req
    "AttachGroupPolicy"
    "fixture/AttachGroupPolicy.yaml"

requestUpdateLoginProfile :: UpdateLoginProfile -> TestTree
requestUpdateLoginProfile =
  req
    "UpdateLoginProfile"
    "fixture/UpdateLoginProfile.yaml"

requestListSAMLProviderTags :: ListSAMLProviderTags -> TestTree
requestListSAMLProviderTags =
  req
    "ListSAMLProviderTags"
    "fixture/ListSAMLProviderTags.yaml"

requestGetGroup :: GetGroup -> TestTree
requestGetGroup =
  req
    "GetGroup"
    "fixture/GetGroup.yaml"

requestDeleteLoginProfile :: DeleteLoginProfile -> TestTree
requestDeleteLoginProfile =
  req
    "DeleteLoginProfile"
    "fixture/DeleteLoginProfile.yaml"

requestDeleteServiceLinkedRole :: DeleteServiceLinkedRole -> TestTree
requestDeleteServiceLinkedRole =
  req
    "DeleteServiceLinkedRole"
    "fixture/DeleteServiceLinkedRole.yaml"

requestGenerateOrganizationsAccessReport :: GenerateOrganizationsAccessReport -> TestTree
requestGenerateOrganizationsAccessReport =
  req
    "GenerateOrganizationsAccessReport"
    "fixture/GenerateOrganizationsAccessReport.yaml"

requestGetServiceLastAccessedDetailsWithEntities :: GetServiceLastAccessedDetailsWithEntities -> TestTree
requestGetServiceLastAccessedDetailsWithEntities =
  req
    "GetServiceLastAccessedDetailsWithEntities"
    "fixture/GetServiceLastAccessedDetailsWithEntities.yaml"

requestPutGroupPolicy :: PutGroupPolicy -> TestTree
requestPutGroupPolicy =
  req
    "PutGroupPolicy"
    "fixture/PutGroupPolicy.yaml"

requestGetServiceLastAccessedDetails :: GetServiceLastAccessedDetails -> TestTree
requestGetServiceLastAccessedDetails =
  req
    "GetServiceLastAccessedDetails"
    "fixture/GetServiceLastAccessedDetails.yaml"

requestDeleteAccountAlias :: DeleteAccountAlias -> TestTree
requestDeleteAccountAlias =
  req
    "DeleteAccountAlias"
    "fixture/DeleteAccountAlias.yaml"

requestCreateSAMLProvider :: CreateSAMLProvider -> TestTree
requestCreateSAMLProvider =
  req
    "CreateSAMLProvider"
    "fixture/CreateSAMLProvider.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy =
  req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

requestDetachUserPolicy :: DetachUserPolicy -> TestTree
requestDetachUserPolicy =
  req
    "DetachUserPolicy"
    "fixture/DetachUserPolicy.yaml"

requestUpdateGroup :: UpdateGroup -> TestTree
requestUpdateGroup =
  req
    "UpdateGroup"
    "fixture/UpdateGroup.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup =
  req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

requestGetServiceLinkedRoleDeletionStatus :: GetServiceLinkedRoleDeletionStatus -> TestTree
requestGetServiceLinkedRoleDeletionStatus =
  req
    "GetServiceLinkedRoleDeletionStatus"
    "fixture/GetServiceLinkedRoleDeletionStatus.yaml"

requestGetAccountAuthorizationDetails :: GetAccountAuthorizationDetails -> TestTree
requestGetAccountAuthorizationDetails =
  req
    "GetAccountAuthorizationDetails"
    "fixture/GetAccountAuthorizationDetails.yaml"

requestListGroupPolicies :: ListGroupPolicies -> TestTree
requestListGroupPolicies =
  req
    "ListGroupPolicies"
    "fixture/ListGroupPolicies.yaml"

requestDeletePolicyVersion :: DeletePolicyVersion -> TestTree
requestDeletePolicyVersion =
  req
    "DeletePolicyVersion"
    "fixture/DeletePolicyVersion.yaml"

requestDeleteSAMLProvider :: DeleteSAMLProvider -> TestTree
requestDeleteSAMLProvider =
  req
    "DeleteSAMLProvider"
    "fixture/DeleteSAMLProvider.yaml"

requestTagUser :: TagUser -> TestTree
requestTagUser =
  req
    "TagUser"
    "fixture/TagUser.yaml"

requestListInstanceProfiles :: ListInstanceProfiles -> TestTree
requestListInstanceProfiles =
  req
    "ListInstanceProfiles"
    "fixture/ListInstanceProfiles.yaml"

requestGetCredentialReport :: GetCredentialReport -> TestTree
requestGetCredentialReport =
  req
    "GetCredentialReport"
    "fixture/GetCredentialReport.yaml"

requestListMFADevices :: ListMFADevices -> TestTree
requestListMFADevices =
  req
    "ListMFADevices"
    "fixture/ListMFADevices.yaml"

requestUpdateSAMLProvider :: UpdateSAMLProvider -> TestTree
requestUpdateSAMLProvider =
  req
    "UpdateSAMLProvider"
    "fixture/UpdateSAMLProvider.yaml"

requestUntagInstanceProfile :: UntagInstanceProfile -> TestTree
requestUntagInstanceProfile =
  req
    "UntagInstanceProfile"
    "fixture/UntagInstanceProfile.yaml"

requestCreateAccountAlias :: CreateAccountAlias -> TestTree
requestCreateAccountAlias =
  req
    "CreateAccountAlias"
    "fixture/CreateAccountAlias.yaml"

requestUntagMFADevice :: UntagMFADevice -> TestTree
requestUntagMFADevice =
  req
    "UntagMFADevice"
    "fixture/UntagMFADevice.yaml"

requestUntagSAMLProvider :: UntagSAMLProvider -> TestTree
requestUntagSAMLProvider =
  req
    "UntagSAMLProvider"
    "fixture/UntagSAMLProvider.yaml"

requestListAccountAliases :: ListAccountAliases -> TestTree
requestListAccountAliases =
  req
    "ListAccountAliases"
    "fixture/ListAccountAliases.yaml"

requestListPolicyVersions :: ListPolicyVersions -> TestTree
requestListPolicyVersions =
  req
    "ListPolicyVersions"
    "fixture/ListPolicyVersions.yaml"

requestDeleteInstanceProfile :: DeleteInstanceProfile -> TestTree
requestDeleteInstanceProfile =
  req
    "DeleteInstanceProfile"
    "fixture/DeleteInstanceProfile.yaml"

requestGetAccountSummary :: GetAccountSummary -> TestTree
requestGetAccountSummary =
  req
    "GetAccountSummary"
    "fixture/GetAccountSummary.yaml"

requestListServerCertificateTags :: ListServerCertificateTags -> TestTree
requestListServerCertificateTags =
  req
    "ListServerCertificateTags"
    "fixture/ListServerCertificateTags.yaml"

requestGetSSHPublicKey :: GetSSHPublicKey -> TestTree
requestGetSSHPublicKey =
  req
    "GetSSHPublicKey"
    "fixture/GetSSHPublicKey.yaml"

requestUpdateOpenIDConnectProviderThumbprint :: UpdateOpenIDConnectProviderThumbprint -> TestTree
requestUpdateOpenIDConnectProviderThumbprint =
  req
    "UpdateOpenIDConnectProviderThumbprint"
    "fixture/UpdateOpenIDConnectProviderThumbprint.yaml"

requestGetAccessKeyLastUsed :: GetAccessKeyLastUsed -> TestTree
requestGetAccessKeyLastUsed =
  req
    "GetAccessKeyLastUsed"
    "fixture/GetAccessKeyLastUsed.yaml"

requestTagSAMLProvider :: TagSAMLProvider -> TestTree
requestTagSAMLProvider =
  req
    "TagSAMLProvider"
    "fixture/TagSAMLProvider.yaml"

requestGetAccountPasswordPolicy :: GetAccountPasswordPolicy -> TestTree
requestGetAccountPasswordPolicy =
  req
    "GetAccountPasswordPolicy"
    "fixture/GetAccountPasswordPolicy.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers =
  req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestUpdateUser :: UpdateUser -> TestTree
requestUpdateUser =
  req
    "UpdateUser"
    "fixture/UpdateUser.yaml"

requestListRolePolicies :: ListRolePolicies -> TestTree
requestListRolePolicies =
  req
    "ListRolePolicies"
    "fixture/ListRolePolicies.yaml"

requestAddClientIDToOpenIDConnectProvider :: AddClientIDToOpenIDConnectProvider -> TestTree
requestAddClientIDToOpenIDConnectProvider =
  req
    "AddClientIDToOpenIDConnectProvider"
    "fixture/AddClientIDToOpenIDConnectProvider.yaml"

requestDeleteUserPermissionsBoundary :: DeleteUserPermissionsBoundary -> TestTree
requestDeleteUserPermissionsBoundary =
  req
    "DeleteUserPermissionsBoundary"
    "fixture/DeleteUserPermissionsBoundary.yaml"

requestPutUserPolicy :: PutUserPolicy -> TestTree
requestPutUserPolicy =
  req
    "PutUserPolicy"
    "fixture/PutUserPolicy.yaml"

requestDetachGroupPolicy :: DetachGroupPolicy -> TestTree
requestDetachGroupPolicy =
  req
    "DetachGroupPolicy"
    "fixture/DetachGroupPolicy.yaml"

requestUntagUser :: UntagUser -> TestTree
requestUntagUser =
  req
    "UntagUser"
    "fixture/UntagUser.yaml"

requestGetContextKeysForCustomPolicy :: GetContextKeysForCustomPolicy -> TestTree
requestGetContextKeysForCustomPolicy =
  req
    "GetContextKeysForCustomPolicy"
    "fixture/GetContextKeysForCustomPolicy.yaml"

requestPutRolePermissionsBoundary :: PutRolePermissionsBoundary -> TestTree
requestPutRolePermissionsBoundary =
  req
    "PutRolePermissionsBoundary"
    "fixture/PutRolePermissionsBoundary.yaml"

requestUntagRole :: UntagRole -> TestTree
requestUntagRole =
  req
    "UntagRole"
    "fixture/UntagRole.yaml"

requestSimulateCustomPolicy :: SimulateCustomPolicy -> TestTree
requestSimulateCustomPolicy =
  req
    "SimulateCustomPolicy"
    "fixture/SimulateCustomPolicy.yaml"

requestUploadSSHPublicKey :: UploadSSHPublicKey -> TestTree
requestUploadSSHPublicKey =
  req
    "UploadSSHPublicKey"
    "fixture/UploadSSHPublicKey.yaml"

requestDeleteRole :: DeleteRole -> TestTree
requestDeleteRole =
  req
    "DeleteRole"
    "fixture/DeleteRole.yaml"

requestListUserPolicies :: ListUserPolicies -> TestTree
requestListUserPolicies =
  req
    "ListUserPolicies"
    "fixture/ListUserPolicies.yaml"

requestPutRolePolicy :: PutRolePolicy -> TestTree
requestPutRolePolicy =
  req
    "PutRolePolicy"
    "fixture/PutRolePolicy.yaml"

requestUpdateRole :: UpdateRole -> TestTree
requestUpdateRole =
  req
    "UpdateRole"
    "fixture/UpdateRole.yaml"

requestSetSecurityTokenServicePreferences :: SetSecurityTokenServicePreferences -> TestTree
requestSetSecurityTokenServicePreferences =
  req
    "SetSecurityTokenServicePreferences"
    "fixture/SetSecurityTokenServicePreferences.yaml"

requestAttachUserPolicy :: AttachUserPolicy -> TestTree
requestAttachUserPolicy =
  req
    "AttachUserPolicy"
    "fixture/AttachUserPolicy.yaml"

requestTagServerCertificate :: TagServerCertificate -> TestTree
requestTagServerCertificate =
  req
    "TagServerCertificate"
    "fixture/TagServerCertificate.yaml"

requestListAccessKeys :: ListAccessKeys -> TestTree
requestListAccessKeys =
  req
    "ListAccessKeys"
    "fixture/ListAccessKeys.yaml"

requestCreateOpenIDConnectProvider :: CreateOpenIDConnectProvider -> TestTree
requestCreateOpenIDConnectProvider =
  req
    "CreateOpenIDConnectProvider"
    "fixture/CreateOpenIDConnectProvider.yaml"

requestDeactivateMFADevice :: DeactivateMFADevice -> TestTree
requestDeactivateMFADevice =
  req
    "DeactivateMFADevice"
    "fixture/DeactivateMFADevice.yaml"

requestListUserTags :: ListUserTags -> TestTree
requestListUserTags =
  req
    "ListUserTags"
    "fixture/ListUserTags.yaml"

requestGetRole :: GetRole -> TestTree
requestGetRole =
  req
    "GetRole"
    "fixture/GetRole.yaml"

-- Responses

responseCreateVirtualMFADevice :: CreateVirtualMFADeviceResponse -> TestTree
responseCreateVirtualMFADevice =
  res
    "CreateVirtualMFADeviceResponse"
    "fixture/CreateVirtualMFADeviceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVirtualMFADevice)

responseAttachRolePolicy :: AttachRolePolicyResponse -> TestTree
responseAttachRolePolicy =
  res
    "AttachRolePolicyResponse"
    "fixture/AttachRolePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy AttachRolePolicy)

responseDeleteSSHPublicKey :: DeleteSSHPublicKeyResponse -> TestTree
responseDeleteSSHPublicKey =
  res
    "DeleteSSHPublicKeyResponse"
    "fixture/DeleteSSHPublicKeyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSSHPublicKey)

responseGetUser :: GetUserResponse -> TestTree
responseGetUser =
  res
    "GetUserResponse"
    "fixture/GetUserResponse.proto"
    defaultService
    (Proxy :: Proxy GetUser)

responseUpdateSSHPublicKey :: UpdateSSHPublicKeyResponse -> TestTree
responseUpdateSSHPublicKey =
  res
    "UpdateSSHPublicKeyResponse"
    "fixture/UpdateSSHPublicKeyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSSHPublicKey)

responseUntagOpenIDConnectProvider :: UntagOpenIDConnectProviderResponse -> TestTree
responseUntagOpenIDConnectProvider =
  res
    "UntagOpenIDConnectProviderResponse"
    "fixture/UntagOpenIDConnectProviderResponse.proto"
    defaultService
    (Proxy :: Proxy UntagOpenIDConnectProvider)

responseListSigningCertificates :: ListSigningCertificatesResponse -> TestTree
responseListSigningCertificates =
  res
    "ListSigningCertificatesResponse"
    "fixture/ListSigningCertificatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListSigningCertificates)

responseDeleteOpenIDConnectProvider :: DeleteOpenIDConnectProviderResponse -> TestTree
responseDeleteOpenIDConnectProvider =
  res
    "DeleteOpenIDConnectProviderResponse"
    "fixture/DeleteOpenIDConnectProviderResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteOpenIDConnectProvider)

responseListRoleTags :: ListRoleTagsResponse -> TestTree
responseListRoleTags =
  res
    "ListRoleTagsResponse"
    "fixture/ListRoleTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListRoleTags)

responseListOpenIDConnectProviders :: ListOpenIDConnectProvidersResponse -> TestTree
responseListOpenIDConnectProviders =
  res
    "ListOpenIDConnectProvidersResponse"
    "fixture/ListOpenIDConnectProvidersResponse.proto"
    defaultService
    (Proxy :: Proxy ListOpenIDConnectProviders)

responseCreatePolicy :: CreatePolicyResponse -> TestTree
responseCreatePolicy =
  res
    "CreatePolicyResponse"
    "fixture/CreatePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePolicy)

responseGetSAMLProvider :: GetSAMLProviderResponse -> TestTree
responseGetSAMLProvider =
  res
    "GetSAMLProviderResponse"
    "fixture/GetSAMLProviderResponse.proto"
    defaultService
    (Proxy :: Proxy GetSAMLProvider)

responseGetContextKeysForPrincipalPolicy :: GetContextKeysForPolicyResponse -> TestTree
responseGetContextKeysForPrincipalPolicy =
  res
    "GetContextKeysForPrincipalPolicyResponse"
    "fixture/GetContextKeysForPrincipalPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetContextKeysForPrincipalPolicy)

responseListEntitiesForPolicy :: ListEntitiesForPolicyResponse -> TestTree
responseListEntitiesForPolicy =
  res
    "ListEntitiesForPolicyResponse"
    "fixture/ListEntitiesForPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy ListEntitiesForPolicy)

responseListGroupsForUser :: ListGroupsForUserResponse -> TestTree
responseListGroupsForUser =
  res
    "ListGroupsForUserResponse"
    "fixture/ListGroupsForUserResponse.proto"
    defaultService
    (Proxy :: Proxy ListGroupsForUser)

responseSimulatePrincipalPolicy :: SimulatePolicyResponse -> TestTree
responseSimulatePrincipalPolicy =
  res
    "SimulatePrincipalPolicyResponse"
    "fixture/SimulatePrincipalPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy SimulatePrincipalPolicy)

responseListPolicies :: ListPoliciesResponse -> TestTree
responseListPolicies =
  res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListPolicies)

responseCreateServiceLinkedRole :: CreateServiceLinkedRoleResponse -> TestTree
responseCreateServiceLinkedRole =
  res
    "CreateServiceLinkedRoleResponse"
    "fixture/CreateServiceLinkedRoleResponse.proto"
    defaultService
    (Proxy :: Proxy CreateServiceLinkedRole)

responseUntagPolicy :: UntagPolicyResponse -> TestTree
responseUntagPolicy =
  res
    "UntagPolicyResponse"
    "fixture/UntagPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy UntagPolicy)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePolicy)

responseDeleteServerCertificate :: DeleteServerCertificateResponse -> TestTree
responseDeleteServerCertificate =
  res
    "DeleteServerCertificateResponse"
    "fixture/DeleteServerCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteServerCertificate)

responseListAttachedGroupPolicies :: ListAttachedGroupPoliciesResponse -> TestTree
responseListAttachedGroupPolicies =
  res
    "ListAttachedGroupPoliciesResponse"
    "fixture/ListAttachedGroupPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAttachedGroupPolicies)

responseChangePassword :: ChangePasswordResponse -> TestTree
responseChangePassword =
  res
    "ChangePasswordResponse"
    "fixture/ChangePasswordResponse.proto"
    defaultService
    (Proxy :: Proxy ChangePassword)

responseListMFADeviceTags :: ListMFADeviceTagsResponse -> TestTree
responseListMFADeviceTags =
  res
    "ListMFADeviceTagsResponse"
    "fixture/ListMFADeviceTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListMFADeviceTags)

responseUntagServerCertificate :: UntagServerCertificateResponse -> TestTree
responseUntagServerCertificate =
  res
    "UntagServerCertificateResponse"
    "fixture/UntagServerCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy UntagServerCertificate)

responseUpdateAssumeRolePolicy :: UpdateAssumeRolePolicyResponse -> TestTree
responseUpdateAssumeRolePolicy =
  res
    "UpdateAssumeRolePolicyResponse"
    "fixture/UpdateAssumeRolePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAssumeRolePolicy)

responseGetGroupPolicy :: GetGroupPolicyResponse -> TestTree
responseGetGroupPolicy =
  res
    "GetGroupPolicyResponse"
    "fixture/GetGroupPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetGroupPolicy)

responseUpdateServerCertificate :: UpdateServerCertificateResponse -> TestTree
responseUpdateServerCertificate =
  res
    "UpdateServerCertificateResponse"
    "fixture/UpdateServerCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateServerCertificate)

responseListServerCertificates :: ListServerCertificatesResponse -> TestTree
responseListServerCertificates =
  res
    "ListServerCertificatesResponse"
    "fixture/ListServerCertificatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListServerCertificates)

responseListInstanceProfileTags :: ListInstanceProfileTagsResponse -> TestTree
responseListInstanceProfileTags =
  res
    "ListInstanceProfileTagsResponse"
    "fixture/ListInstanceProfileTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListInstanceProfileTags)

responseDeleteGroupPolicy :: DeleteGroupPolicyResponse -> TestTree
responseDeleteGroupPolicy =
  res
    "DeleteGroupPolicyResponse"
    "fixture/DeleteGroupPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGroupPolicy)

responseCreateInstanceProfile :: CreateInstanceProfileResponse -> TestTree
responseCreateInstanceProfile =
  res
    "CreateInstanceProfileResponse"
    "fixture/CreateInstanceProfileResponse.proto"
    defaultService
    (Proxy :: Proxy CreateInstanceProfile)

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups =
  res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListGroups)

responseGetLoginProfile :: GetLoginProfileResponse -> TestTree
responseGetLoginProfile =
  res
    "GetLoginProfileResponse"
    "fixture/GetLoginProfileResponse.proto"
    defaultService
    (Proxy :: Proxy GetLoginProfile)

responseTagRole :: TagRoleResponse -> TestTree
responseTagRole =
  res
    "TagRoleResponse"
    "fixture/TagRoleResponse.proto"
    defaultService
    (Proxy :: Proxy TagRole)

responseRemoveRoleFromInstanceProfile :: RemoveRoleFromInstanceProfileResponse -> TestTree
responseRemoveRoleFromInstanceProfile =
  res
    "RemoveRoleFromInstanceProfileResponse"
    "fixture/RemoveRoleFromInstanceProfileResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveRoleFromInstanceProfile)

responseGenerateCredentialReport :: GenerateCredentialReportResponse -> TestTree
responseGenerateCredentialReport =
  res
    "GenerateCredentialReportResponse"
    "fixture/GenerateCredentialReportResponse.proto"
    defaultService
    (Proxy :: Proxy GenerateCredentialReport)

responseCreatePolicyVersion :: CreatePolicyVersionResponse -> TestTree
responseCreatePolicyVersion =
  res
    "CreatePolicyVersionResponse"
    "fixture/CreatePolicyVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePolicyVersion)

responseGetServerCertificate :: GetServerCertificateResponse -> TestTree
responseGetServerCertificate =
  res
    "GetServerCertificateResponse"
    "fixture/GetServerCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy GetServerCertificate)

responseRemoveUserFromGroup :: RemoveUserFromGroupResponse -> TestTree
responseRemoveUserFromGroup =
  res
    "RemoveUserFromGroupResponse"
    "fixture/RemoveUserFromGroupResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveUserFromGroup)

responseSetDefaultPolicyVersion :: SetDefaultPolicyVersionResponse -> TestTree
responseSetDefaultPolicyVersion =
  res
    "SetDefaultPolicyVersionResponse"
    "fixture/SetDefaultPolicyVersionResponse.proto"
    defaultService
    (Proxy :: Proxy SetDefaultPolicyVersion)

responseResetServiceSpecificCredential :: ResetServiceSpecificCredentialResponse -> TestTree
responseResetServiceSpecificCredential =
  res
    "ResetServiceSpecificCredentialResponse"
    "fixture/ResetServiceSpecificCredentialResponse.proto"
    defaultService
    (Proxy :: Proxy ResetServiceSpecificCredential)

responseGenerateServiceLastAccessedDetails :: GenerateServiceLastAccessedDetailsResponse -> TestTree
responseGenerateServiceLastAccessedDetails =
  res
    "GenerateServiceLastAccessedDetailsResponse"
    "fixture/GenerateServiceLastAccessedDetailsResponse.proto"
    defaultService
    (Proxy :: Proxy GenerateServiceLastAccessedDetails)

responseListPoliciesGrantingServiceAccess :: ListPoliciesGrantingServiceAccessResponse -> TestTree
responseListPoliciesGrantingServiceAccess =
  res
    "ListPoliciesGrantingServiceAccessResponse"
    "fixture/ListPoliciesGrantingServiceAccessResponse.proto"
    defaultService
    (Proxy :: Proxy ListPoliciesGrantingServiceAccess)

responseUpdateRoleDescription :: UpdateRoleDescriptionResponse -> TestTree
responseUpdateRoleDescription =
  res
    "UpdateRoleDescriptionResponse"
    "fixture/UpdateRoleDescriptionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRoleDescription)

responseUploadServerCertificate :: UploadServerCertificateResponse -> TestTree
responseUploadServerCertificate =
  res
    "UploadServerCertificateResponse"
    "fixture/UploadServerCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy UploadServerCertificate)

responseDetachRolePolicy :: DetachRolePolicyResponse -> TestTree
responseDetachRolePolicy =
  res
    "DetachRolePolicyResponse"
    "fixture/DetachRolePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DetachRolePolicy)

responseEnableMFADevice :: EnableMFADeviceResponse -> TestTree
responseEnableMFADevice =
  res
    "EnableMFADeviceResponse"
    "fixture/EnableMFADeviceResponse.proto"
    defaultService
    (Proxy :: Proxy EnableMFADevice)

responseListSAMLProviders :: ListSAMLProvidersResponse -> TestTree
responseListSAMLProviders =
  res
    "ListSAMLProvidersResponse"
    "fixture/ListSAMLProvidersResponse.proto"
    defaultService
    (Proxy :: Proxy ListSAMLProviders)

responseListPolicyTags :: ListPolicyTagsResponse -> TestTree
responseListPolicyTags =
  res
    "ListPolicyTagsResponse"
    "fixture/ListPolicyTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPolicyTags)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup =
  res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateGroup)

responseTagMFADevice :: TagMFADeviceResponse -> TestTree
responseTagMFADevice =
  res
    "TagMFADeviceResponse"
    "fixture/TagMFADeviceResponse.proto"
    defaultService
    (Proxy :: Proxy TagMFADevice)

responseTagInstanceProfile :: TagInstanceProfileResponse -> TestTree
responseTagInstanceProfile =
  res
    "TagInstanceProfileResponse"
    "fixture/TagInstanceProfileResponse.proto"
    defaultService
    (Proxy :: Proxy TagInstanceProfile)

responseGetOpenIDConnectProvider :: GetOpenIDConnectProviderResponse -> TestTree
responseGetOpenIDConnectProvider =
  res
    "GetOpenIDConnectProviderResponse"
    "fixture/GetOpenIDConnectProviderResponse.proto"
    defaultService
    (Proxy :: Proxy GetOpenIDConnectProvider)

responseCreateRole :: CreateRoleResponse -> TestTree
responseCreateRole =
  res
    "CreateRoleResponse"
    "fixture/CreateRoleResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRole)

responsePutUserPermissionsBoundary :: PutUserPermissionsBoundaryResponse -> TestTree
responsePutUserPermissionsBoundary =
  res
    "PutUserPermissionsBoundaryResponse"
    "fixture/PutUserPermissionsBoundaryResponse.proto"
    defaultService
    (Proxy :: Proxy PutUserPermissionsBoundary)

responseDeleteUserPolicy :: DeleteUserPolicyResponse -> TestTree
responseDeleteUserPolicy =
  res
    "DeleteUserPolicyResponse"
    "fixture/DeleteUserPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUserPolicy)

responseDeleteRolePermissionsBoundary :: DeleteRolePermissionsBoundaryResponse -> TestTree
responseDeleteRolePermissionsBoundary =
  res
    "DeleteRolePermissionsBoundaryResponse"
    "fixture/DeleteRolePermissionsBoundaryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRolePermissionsBoundary)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUser)

responseListOpenIDConnectProviderTags :: ListOpenIDConnectProviderTagsResponse -> TestTree
responseListOpenIDConnectProviderTags =
  res
    "ListOpenIDConnectProviderTagsResponse"
    "fixture/ListOpenIDConnectProviderTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListOpenIDConnectProviderTags)

responseListRoles :: ListRolesResponse -> TestTree
responseListRoles =
  res
    "ListRolesResponse"
    "fixture/ListRolesResponse.proto"
    defaultService
    (Proxy :: Proxy ListRoles)

responseUploadSigningCertificate :: UploadSigningCertificateResponse -> TestTree
responseUploadSigningCertificate =
  res
    "UploadSigningCertificateResponse"
    "fixture/UploadSigningCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy UploadSigningCertificate)

responseDeleteRolePolicy :: DeleteRolePolicyResponse -> TestTree
responseDeleteRolePolicy =
  res
    "DeleteRolePolicyResponse"
    "fixture/DeleteRolePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRolePolicy)

responseListAttachedRolePolicies :: ListAttachedRolePoliciesResponse -> TestTree
responseListAttachedRolePolicies =
  res
    "ListAttachedRolePoliciesResponse"
    "fixture/ListAttachedRolePoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAttachedRolePolicies)

responseGetRolePolicy :: GetRolePolicyResponse -> TestTree
responseGetRolePolicy =
  res
    "GetRolePolicyResponse"
    "fixture/GetRolePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetRolePolicy)

responseDeleteAccessKey :: DeleteAccessKeyResponse -> TestTree
responseDeleteAccessKey =
  res
    "DeleteAccessKeyResponse"
    "fixture/DeleteAccessKeyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAccessKey)

responseListVirtualMFADevices :: ListVirtualMFADevicesResponse -> TestTree
responseListVirtualMFADevices =
  res
    "ListVirtualMFADevicesResponse"
    "fixture/ListVirtualMFADevicesResponse.proto"
    defaultService
    (Proxy :: Proxy ListVirtualMFADevices)

responseTagPolicy :: TagPolicyResponse -> TestTree
responseTagPolicy =
  res
    "TagPolicyResponse"
    "fixture/TagPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy TagPolicy)

responseRemoveClientIDFromOpenIDConnectProvider :: RemoveClientIDFromOpenIDConnectProviderResponse -> TestTree
responseRemoveClientIDFromOpenIDConnectProvider =
  res
    "RemoveClientIDFromOpenIDConnectProviderResponse"
    "fixture/RemoveClientIDFromOpenIDConnectProviderResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveClientIDFromOpenIDConnectProvider)

responseDeleteVirtualMFADevice :: DeleteVirtualMFADeviceResponse -> TestTree
responseDeleteVirtualMFADevice =
  res
    "DeleteVirtualMFADeviceResponse"
    "fixture/DeleteVirtualMFADeviceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVirtualMFADevice)

responseUpdateAccessKey :: UpdateAccessKeyResponse -> TestTree
responseUpdateAccessKey =
  res
    "UpdateAccessKeyResponse"
    "fixture/UpdateAccessKeyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAccessKey)

responseCreateServiceSpecificCredential :: CreateServiceSpecificCredentialResponse -> TestTree
responseCreateServiceSpecificCredential =
  res
    "CreateServiceSpecificCredentialResponse"
    "fixture/CreateServiceSpecificCredentialResponse.proto"
    defaultService
    (Proxy :: Proxy CreateServiceSpecificCredential)

responseResyncMFADevice :: ResyncMFADeviceResponse -> TestTree
responseResyncMFADevice =
  res
    "ResyncMFADeviceResponse"
    "fixture/ResyncMFADeviceResponse.proto"
    defaultService
    (Proxy :: Proxy ResyncMFADevice)

responseUpdateServiceSpecificCredential :: UpdateServiceSpecificCredentialResponse -> TestTree
responseUpdateServiceSpecificCredential =
  res
    "UpdateServiceSpecificCredentialResponse"
    "fixture/UpdateServiceSpecificCredentialResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateServiceSpecificCredential)

responseGetUserPolicy :: GetUserPolicyResponse -> TestTree
responseGetUserPolicy =
  res
    "GetUserPolicyResponse"
    "fixture/GetUserPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetUserPolicy)

responseUpdateAccountPasswordPolicy :: UpdateAccountPasswordPolicyResponse -> TestTree
responseUpdateAccountPasswordPolicy =
  res
    "UpdateAccountPasswordPolicyResponse"
    "fixture/UpdateAccountPasswordPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAccountPasswordPolicy)

responseListServiceSpecificCredentials :: ListServiceSpecificCredentialsResponse -> TestTree
responseListServiceSpecificCredentials =
  res
    "ListServiceSpecificCredentialsResponse"
    "fixture/ListServiceSpecificCredentialsResponse.proto"
    defaultService
    (Proxy :: Proxy ListServiceSpecificCredentials)

responseDeleteSigningCertificate :: DeleteSigningCertificateResponse -> TestTree
responseDeleteSigningCertificate =
  res
    "DeleteSigningCertificateResponse"
    "fixture/DeleteSigningCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSigningCertificate)

responseListAttachedUserPolicies :: ListAttachedUserPoliciesResponse -> TestTree
responseListAttachedUserPolicies =
  res
    "ListAttachedUserPoliciesResponse"
    "fixture/ListAttachedUserPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAttachedUserPolicies)

responseUpdateSigningCertificate :: UpdateSigningCertificateResponse -> TestTree
responseUpdateSigningCertificate =
  res
    "UpdateSigningCertificateResponse"
    "fixture/UpdateSigningCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSigningCertificate)

responseListSSHPublicKeys :: ListSSHPublicKeysResponse -> TestTree
responseListSSHPublicKeys =
  res
    "ListSSHPublicKeysResponse"
    "fixture/ListSSHPublicKeysResponse.proto"
    defaultService
    (Proxy :: Proxy ListSSHPublicKeys)

responseDeleteServiceSpecificCredential :: DeleteServiceSpecificCredentialResponse -> TestTree
responseDeleteServiceSpecificCredential =
  res
    "DeleteServiceSpecificCredentialResponse"
    "fixture/DeleteServiceSpecificCredentialResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteServiceSpecificCredential)

responseCreateAccessKey :: CreateAccessKeyResponse -> TestTree
responseCreateAccessKey =
  res
    "CreateAccessKeyResponse"
    "fixture/CreateAccessKeyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAccessKey)

responseDeleteAccountPasswordPolicy :: DeleteAccountPasswordPolicyResponse -> TestTree
responseDeleteAccountPasswordPolicy =
  res
    "DeleteAccountPasswordPolicyResponse"
    "fixture/DeleteAccountPasswordPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAccountPasswordPolicy)

responseGetOrganizationsAccessReport :: GetOrganizationsAccessReportResponse -> TestTree
responseGetOrganizationsAccessReport =
  res
    "GetOrganizationsAccessReportResponse"
    "fixture/GetOrganizationsAccessReportResponse.proto"
    defaultService
    (Proxy :: Proxy GetOrganizationsAccessReport)

responseListInstanceProfilesForRole :: ListInstanceProfilesForRoleResponse -> TestTree
responseListInstanceProfilesForRole =
  res
    "ListInstanceProfilesForRoleResponse"
    "fixture/ListInstanceProfilesForRoleResponse.proto"
    defaultService
    (Proxy :: Proxy ListInstanceProfilesForRole)

responseGetPolicyVersion :: GetPolicyVersionResponse -> TestTree
responseGetPolicyVersion =
  res
    "GetPolicyVersionResponse"
    "fixture/GetPolicyVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetPolicyVersion)

responseCreateLoginProfile :: CreateLoginProfileResponse -> TestTree
responseCreateLoginProfile =
  res
    "CreateLoginProfileResponse"
    "fixture/CreateLoginProfileResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLoginProfile)

responseAddRoleToInstanceProfile :: AddRoleToInstanceProfileResponse -> TestTree
responseAddRoleToInstanceProfile =
  res
    "AddRoleToInstanceProfileResponse"
    "fixture/AddRoleToInstanceProfileResponse.proto"
    defaultService
    (Proxy :: Proxy AddRoleToInstanceProfile)

responseGetInstanceProfile :: GetInstanceProfileResponse -> TestTree
responseGetInstanceProfile =
  res
    "GetInstanceProfileResponse"
    "fixture/GetInstanceProfileResponse.proto"
    defaultService
    (Proxy :: Proxy GetInstanceProfile)

responseTagOpenIDConnectProvider :: TagOpenIDConnectProviderResponse -> TestTree
responseTagOpenIDConnectProvider =
  res
    "TagOpenIDConnectProviderResponse"
    "fixture/TagOpenIDConnectProviderResponse.proto"
    defaultService
    (Proxy :: Proxy TagOpenIDConnectProvider)

responseAddUserToGroup :: AddUserToGroupResponse -> TestTree
responseAddUserToGroup =
  res
    "AddUserToGroupResponse"
    "fixture/AddUserToGroupResponse.proto"
    defaultService
    (Proxy :: Proxy AddUserToGroup)

responseAttachGroupPolicy :: AttachGroupPolicyResponse -> TestTree
responseAttachGroupPolicy =
  res
    "AttachGroupPolicyResponse"
    "fixture/AttachGroupPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy AttachGroupPolicy)

responseUpdateLoginProfile :: UpdateLoginProfileResponse -> TestTree
responseUpdateLoginProfile =
  res
    "UpdateLoginProfileResponse"
    "fixture/UpdateLoginProfileResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateLoginProfile)

responseListSAMLProviderTags :: ListSAMLProviderTagsResponse -> TestTree
responseListSAMLProviderTags =
  res
    "ListSAMLProviderTagsResponse"
    "fixture/ListSAMLProviderTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSAMLProviderTags)

responseGetGroup :: GetGroupResponse -> TestTree
responseGetGroup =
  res
    "GetGroupResponse"
    "fixture/GetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy GetGroup)

responseDeleteLoginProfile :: DeleteLoginProfileResponse -> TestTree
responseDeleteLoginProfile =
  res
    "DeleteLoginProfileResponse"
    "fixture/DeleteLoginProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLoginProfile)

responseDeleteServiceLinkedRole :: DeleteServiceLinkedRoleResponse -> TestTree
responseDeleteServiceLinkedRole =
  res
    "DeleteServiceLinkedRoleResponse"
    "fixture/DeleteServiceLinkedRoleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteServiceLinkedRole)

responseGenerateOrganizationsAccessReport :: GenerateOrganizationsAccessReportResponse -> TestTree
responseGenerateOrganizationsAccessReport =
  res
    "GenerateOrganizationsAccessReportResponse"
    "fixture/GenerateOrganizationsAccessReportResponse.proto"
    defaultService
    (Proxy :: Proxy GenerateOrganizationsAccessReport)

responseGetServiceLastAccessedDetailsWithEntities :: GetServiceLastAccessedDetailsWithEntitiesResponse -> TestTree
responseGetServiceLastAccessedDetailsWithEntities =
  res
    "GetServiceLastAccessedDetailsWithEntitiesResponse"
    "fixture/GetServiceLastAccessedDetailsWithEntitiesResponse.proto"
    defaultService
    (Proxy :: Proxy GetServiceLastAccessedDetailsWithEntities)

responsePutGroupPolicy :: PutGroupPolicyResponse -> TestTree
responsePutGroupPolicy =
  res
    "PutGroupPolicyResponse"
    "fixture/PutGroupPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutGroupPolicy)

responseGetServiceLastAccessedDetails :: GetServiceLastAccessedDetailsResponse -> TestTree
responseGetServiceLastAccessedDetails =
  res
    "GetServiceLastAccessedDetailsResponse"
    "fixture/GetServiceLastAccessedDetailsResponse.proto"
    defaultService
    (Proxy :: Proxy GetServiceLastAccessedDetails)

responseDeleteAccountAlias :: DeleteAccountAliasResponse -> TestTree
responseDeleteAccountAlias =
  res
    "DeleteAccountAliasResponse"
    "fixture/DeleteAccountAliasResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAccountAlias)

responseCreateSAMLProvider :: CreateSAMLProviderResponse -> TestTree
responseCreateSAMLProvider =
  res
    "CreateSAMLProviderResponse"
    "fixture/CreateSAMLProviderResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSAMLProvider)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetPolicy)

responseDetachUserPolicy :: DetachUserPolicyResponse -> TestTree
responseDetachUserPolicy =
  res
    "DetachUserPolicyResponse"
    "fixture/DetachUserPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DetachUserPolicy)

responseUpdateGroup :: UpdateGroupResponse -> TestTree
responseUpdateGroup =
  res
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGroup)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGroup)

responseGetServiceLinkedRoleDeletionStatus :: GetServiceLinkedRoleDeletionStatusResponse -> TestTree
responseGetServiceLinkedRoleDeletionStatus =
  res
    "GetServiceLinkedRoleDeletionStatusResponse"
    "fixture/GetServiceLinkedRoleDeletionStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetServiceLinkedRoleDeletionStatus)

responseGetAccountAuthorizationDetails :: GetAccountAuthorizationDetailsResponse -> TestTree
responseGetAccountAuthorizationDetails =
  res
    "GetAccountAuthorizationDetailsResponse"
    "fixture/GetAccountAuthorizationDetailsResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccountAuthorizationDetails)

responseListGroupPolicies :: ListGroupPoliciesResponse -> TestTree
responseListGroupPolicies =
  res
    "ListGroupPoliciesResponse"
    "fixture/ListGroupPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListGroupPolicies)

responseDeletePolicyVersion :: DeletePolicyVersionResponse -> TestTree
responseDeletePolicyVersion =
  res
    "DeletePolicyVersionResponse"
    "fixture/DeletePolicyVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePolicyVersion)

responseDeleteSAMLProvider :: DeleteSAMLProviderResponse -> TestTree
responseDeleteSAMLProvider =
  res
    "DeleteSAMLProviderResponse"
    "fixture/DeleteSAMLProviderResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSAMLProvider)

responseTagUser :: TagUserResponse -> TestTree
responseTagUser =
  res
    "TagUserResponse"
    "fixture/TagUserResponse.proto"
    defaultService
    (Proxy :: Proxy TagUser)

responseListInstanceProfiles :: ListInstanceProfilesResponse -> TestTree
responseListInstanceProfiles =
  res
    "ListInstanceProfilesResponse"
    "fixture/ListInstanceProfilesResponse.proto"
    defaultService
    (Proxy :: Proxy ListInstanceProfiles)

responseGetCredentialReport :: GetCredentialReportResponse -> TestTree
responseGetCredentialReport =
  res
    "GetCredentialReportResponse"
    "fixture/GetCredentialReportResponse.proto"
    defaultService
    (Proxy :: Proxy GetCredentialReport)

responseListMFADevices :: ListMFADevicesResponse -> TestTree
responseListMFADevices =
  res
    "ListMFADevicesResponse"
    "fixture/ListMFADevicesResponse.proto"
    defaultService
    (Proxy :: Proxy ListMFADevices)

responseUpdateSAMLProvider :: UpdateSAMLProviderResponse -> TestTree
responseUpdateSAMLProvider =
  res
    "UpdateSAMLProviderResponse"
    "fixture/UpdateSAMLProviderResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSAMLProvider)

responseUntagInstanceProfile :: UntagInstanceProfileResponse -> TestTree
responseUntagInstanceProfile =
  res
    "UntagInstanceProfileResponse"
    "fixture/UntagInstanceProfileResponse.proto"
    defaultService
    (Proxy :: Proxy UntagInstanceProfile)

responseCreateAccountAlias :: CreateAccountAliasResponse -> TestTree
responseCreateAccountAlias =
  res
    "CreateAccountAliasResponse"
    "fixture/CreateAccountAliasResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAccountAlias)

responseUntagMFADevice :: UntagMFADeviceResponse -> TestTree
responseUntagMFADevice =
  res
    "UntagMFADeviceResponse"
    "fixture/UntagMFADeviceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagMFADevice)

responseUntagSAMLProvider :: UntagSAMLProviderResponse -> TestTree
responseUntagSAMLProvider =
  res
    "UntagSAMLProviderResponse"
    "fixture/UntagSAMLProviderResponse.proto"
    defaultService
    (Proxy :: Proxy UntagSAMLProvider)

responseListAccountAliases :: ListAccountAliasesResponse -> TestTree
responseListAccountAliases =
  res
    "ListAccountAliasesResponse"
    "fixture/ListAccountAliasesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAccountAliases)

responseListPolicyVersions :: ListPolicyVersionsResponse -> TestTree
responseListPolicyVersions =
  res
    "ListPolicyVersionsResponse"
    "fixture/ListPolicyVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPolicyVersions)

responseDeleteInstanceProfile :: DeleteInstanceProfileResponse -> TestTree
responseDeleteInstanceProfile =
  res
    "DeleteInstanceProfileResponse"
    "fixture/DeleteInstanceProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteInstanceProfile)

responseGetAccountSummary :: GetAccountSummaryResponse -> TestTree
responseGetAccountSummary =
  res
    "GetAccountSummaryResponse"
    "fixture/GetAccountSummaryResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccountSummary)

responseListServerCertificateTags :: ListServerCertificateTagsResponse -> TestTree
responseListServerCertificateTags =
  res
    "ListServerCertificateTagsResponse"
    "fixture/ListServerCertificateTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListServerCertificateTags)

responseGetSSHPublicKey :: GetSSHPublicKeyResponse -> TestTree
responseGetSSHPublicKey =
  res
    "GetSSHPublicKeyResponse"
    "fixture/GetSSHPublicKeyResponse.proto"
    defaultService
    (Proxy :: Proxy GetSSHPublicKey)

responseUpdateOpenIDConnectProviderThumbprint :: UpdateOpenIDConnectProviderThumbprintResponse -> TestTree
responseUpdateOpenIDConnectProviderThumbprint =
  res
    "UpdateOpenIDConnectProviderThumbprintResponse"
    "fixture/UpdateOpenIDConnectProviderThumbprintResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateOpenIDConnectProviderThumbprint)

responseGetAccessKeyLastUsed :: GetAccessKeyLastUsedResponse -> TestTree
responseGetAccessKeyLastUsed =
  res
    "GetAccessKeyLastUsedResponse"
    "fixture/GetAccessKeyLastUsedResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccessKeyLastUsed)

responseTagSAMLProvider :: TagSAMLProviderResponse -> TestTree
responseTagSAMLProvider =
  res
    "TagSAMLProviderResponse"
    "fixture/TagSAMLProviderResponse.proto"
    defaultService
    (Proxy :: Proxy TagSAMLProvider)

responseGetAccountPasswordPolicy :: GetAccountPasswordPolicyResponse -> TestTree
responseGetAccountPasswordPolicy =
  res
    "GetAccountPasswordPolicyResponse"
    "fixture/GetAccountPasswordPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccountPasswordPolicy)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUser)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    defaultService
    (Proxy :: Proxy ListUsers)

responseUpdateUser :: UpdateUserResponse -> TestTree
responseUpdateUser =
  res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUser)

responseListRolePolicies :: ListRolePoliciesResponse -> TestTree
responseListRolePolicies =
  res
    "ListRolePoliciesResponse"
    "fixture/ListRolePoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListRolePolicies)

responseAddClientIDToOpenIDConnectProvider :: AddClientIDToOpenIDConnectProviderResponse -> TestTree
responseAddClientIDToOpenIDConnectProvider =
  res
    "AddClientIDToOpenIDConnectProviderResponse"
    "fixture/AddClientIDToOpenIDConnectProviderResponse.proto"
    defaultService
    (Proxy :: Proxy AddClientIDToOpenIDConnectProvider)

responseDeleteUserPermissionsBoundary :: DeleteUserPermissionsBoundaryResponse -> TestTree
responseDeleteUserPermissionsBoundary =
  res
    "DeleteUserPermissionsBoundaryResponse"
    "fixture/DeleteUserPermissionsBoundaryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUserPermissionsBoundary)

responsePutUserPolicy :: PutUserPolicyResponse -> TestTree
responsePutUserPolicy =
  res
    "PutUserPolicyResponse"
    "fixture/PutUserPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutUserPolicy)

responseDetachGroupPolicy :: DetachGroupPolicyResponse -> TestTree
responseDetachGroupPolicy =
  res
    "DetachGroupPolicyResponse"
    "fixture/DetachGroupPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DetachGroupPolicy)

responseUntagUser :: UntagUserResponse -> TestTree
responseUntagUser =
  res
    "UntagUserResponse"
    "fixture/UntagUserResponse.proto"
    defaultService
    (Proxy :: Proxy UntagUser)

responseGetContextKeysForCustomPolicy :: GetContextKeysForPolicyResponse -> TestTree
responseGetContextKeysForCustomPolicy =
  res
    "GetContextKeysForCustomPolicyResponse"
    "fixture/GetContextKeysForCustomPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetContextKeysForCustomPolicy)

responsePutRolePermissionsBoundary :: PutRolePermissionsBoundaryResponse -> TestTree
responsePutRolePermissionsBoundary =
  res
    "PutRolePermissionsBoundaryResponse"
    "fixture/PutRolePermissionsBoundaryResponse.proto"
    defaultService
    (Proxy :: Proxy PutRolePermissionsBoundary)

responseUntagRole :: UntagRoleResponse -> TestTree
responseUntagRole =
  res
    "UntagRoleResponse"
    "fixture/UntagRoleResponse.proto"
    defaultService
    (Proxy :: Proxy UntagRole)

responseSimulateCustomPolicy :: SimulatePolicyResponse -> TestTree
responseSimulateCustomPolicy =
  res
    "SimulateCustomPolicyResponse"
    "fixture/SimulateCustomPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy SimulateCustomPolicy)

responseUploadSSHPublicKey :: UploadSSHPublicKeyResponse -> TestTree
responseUploadSSHPublicKey =
  res
    "UploadSSHPublicKeyResponse"
    "fixture/UploadSSHPublicKeyResponse.proto"
    defaultService
    (Proxy :: Proxy UploadSSHPublicKey)

responseDeleteRole :: DeleteRoleResponse -> TestTree
responseDeleteRole =
  res
    "DeleteRoleResponse"
    "fixture/DeleteRoleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRole)

responseListUserPolicies :: ListUserPoliciesResponse -> TestTree
responseListUserPolicies =
  res
    "ListUserPoliciesResponse"
    "fixture/ListUserPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListUserPolicies)

responsePutRolePolicy :: PutRolePolicyResponse -> TestTree
responsePutRolePolicy =
  res
    "PutRolePolicyResponse"
    "fixture/PutRolePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutRolePolicy)

responseUpdateRole :: UpdateRoleResponse -> TestTree
responseUpdateRole =
  res
    "UpdateRoleResponse"
    "fixture/UpdateRoleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRole)

responseSetSecurityTokenServicePreferences :: SetSecurityTokenServicePreferencesResponse -> TestTree
responseSetSecurityTokenServicePreferences =
  res
    "SetSecurityTokenServicePreferencesResponse"
    "fixture/SetSecurityTokenServicePreferencesResponse.proto"
    defaultService
    (Proxy :: Proxy SetSecurityTokenServicePreferences)

responseAttachUserPolicy :: AttachUserPolicyResponse -> TestTree
responseAttachUserPolicy =
  res
    "AttachUserPolicyResponse"
    "fixture/AttachUserPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy AttachUserPolicy)

responseTagServerCertificate :: TagServerCertificateResponse -> TestTree
responseTagServerCertificate =
  res
    "TagServerCertificateResponse"
    "fixture/TagServerCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy TagServerCertificate)

responseListAccessKeys :: ListAccessKeysResponse -> TestTree
responseListAccessKeys =
  res
    "ListAccessKeysResponse"
    "fixture/ListAccessKeysResponse.proto"
    defaultService
    (Proxy :: Proxy ListAccessKeys)

responseCreateOpenIDConnectProvider :: CreateOpenIDConnectProviderResponse -> TestTree
responseCreateOpenIDConnectProvider =
  res
    "CreateOpenIDConnectProviderResponse"
    "fixture/CreateOpenIDConnectProviderResponse.proto"
    defaultService
    (Proxy :: Proxy CreateOpenIDConnectProvider)

responseDeactivateMFADevice :: DeactivateMFADeviceResponse -> TestTree
responseDeactivateMFADevice =
  res
    "DeactivateMFADeviceResponse"
    "fixture/DeactivateMFADeviceResponse.proto"
    defaultService
    (Proxy :: Proxy DeactivateMFADevice)

responseListUserTags :: ListUserTagsResponse -> TestTree
responseListUserTags =
  res
    "ListUserTagsResponse"
    "fixture/ListUserTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListUserTags)

responseGetRole :: GetRoleResponse -> TestTree
responseGetRole =
  res
    "GetRoleResponse"
    "fixture/GetRoleResponse.proto"
    defaultService
    (Proxy :: Proxy GetRole)
