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
--         [ requestGetContextKeysForPrincipalPolicy $
--             newGetContextKeysForPrincipalPolicy
--
--         , requestListPolicies $
--             newListPolicies
--
--         , requestCreatePolicy $
--             newCreatePolicy
--
--         , requestListInstanceProfilesForRole $
--             newListInstanceProfilesForRole
--
--         , requestAttachGroupPolicy $
--             newAttachGroupPolicy
--
--         , requestCreateAccessKey $
--             newCreateAccessKey
--
--         , requestListRoleTags $
--             newListRoleTags
--
--         , requestListSSHPublicKeys $
--             newListSSHPublicKeys
--
--         , requestUntagOpenIDConnectProvider $
--             newUntagOpenIDConnectProvider
--
--         , requestListOpenIDConnectProviders $
--             newListOpenIDConnectProviders
--
--         , requestCreateVirtualMFADevice $
--             newCreateVirtualMFADevice
--
--         , requestDeleteAccountPasswordPolicy $
--             newDeleteAccountPasswordPolicy
--
--         , requestUpdateAccountPasswordPolicy $
--             newUpdateAccountPasswordPolicy
--
--         , requestAttachRolePolicy $
--             newAttachRolePolicy
--
--         , requestUpdateSSHPublicKey $
--             newUpdateSSHPublicKey
--
--         , requestDeleteSSHPublicKey $
--             newDeleteSSHPublicKey
--
--         , requestGetUserPolicy $
--             newGetUserPolicy
--
--         , requestUpdateServiceSpecificCredential $
--             newUpdateServiceSpecificCredential
--
--         , requestDeleteServiceSpecificCredential $
--             newDeleteServiceSpecificCredential
--
--         , requestListAttachedRolePolicies $
--             newListAttachedRolePolicies
--
--         , requestGetRole $
--             newGetRole
--
--         , requestDeactivateMFADevice $
--             newDeactivateMFADevice
--
--         , requestCreateOpenIDConnectProvider $
--             newCreateOpenIDConnectProvider
--
--         , requestDeleteVirtualMFADevice $
--             newDeleteVirtualMFADevice
--
--         , requestListRoles $
--             newListRoles
--
--         , requestListUserPolicies $
--             newListUserPolicies
--
--         , requestListOpenIDConnectProviderTags $
--             newListOpenIDConnectProviderTags
--
--         , requestPutRolePermissionsBoundary $
--             newPutRolePermissionsBoundary
--
--         , requestUploadSSHPublicKey $
--             newUploadSSHPublicKey
--
--         , requestDeleteRolePermissionsBoundary $
--             newDeleteRolePermissionsBoundary
--
--         , requestSimulateCustomPolicy $
--             newSimulateCustomPolicy
--
--         , requestUpdateRole $
--             newUpdateRole
--
--         , requestDeleteRole $
--             newDeleteRole
--
--         , requestListUsers $
--             newListUsers
--
--         , requestUpdateOpenIDConnectProviderThumbprint $
--             newUpdateOpenIDConnectProviderThumbprint
--
--         , requestPutUserPolicy $
--             newPutUserPolicy
--
--         , requestTagMFADevice $
--             newTagMFADevice
--
--         , requestGetSSHPublicKey $
--             newGetSSHPublicKey
--
--         , requestUntagUser $
--             newUntagUser
--
--         , requestDetachGroupPolicy $
--             newDetachGroupPolicy
--
--         , requestTagInstanceProfile $
--             newTagInstanceProfile
--
--         , requestGetOpenIDConnectProvider $
--             newGetOpenIDConnectProvider
--
--         , requestPutUserPermissionsBoundary $
--             newPutUserPermissionsBoundary
--
--         , requestDeleteUserPolicy $
--             newDeleteUserPolicy
--
--         , requestTagSAMLProvider $
--             newTagSAMLProvider
--
--         , requestDeleteUserPermissionsBoundary $
--             newDeleteUserPermissionsBoundary
--
--         , requestCreateRole $
--             newCreateRole
--
--         , requestResetServiceSpecificCredential $
--             newResetServiceSpecificCredential
--
--         , requestUntagSAMLProvider $
--             newUntagSAMLProvider
--
--         , requestGetCredentialReport $
--             newGetCredentialReport
--
--         , requestListServerCertificateTags $
--             newListServerCertificateTags
--
--         , requestGetAccountSummary $
--             newGetAccountSummary
--
--         , requestGenerateServiceLastAccessedDetails $
--             newGenerateServiceLastAccessedDetails
--
--         , requestListPolicyTags $
--             newListPolicyTags
--
--         , requestListGroupPolicies $
--             newListGroupPolicies
--
--         , requestDeletePolicyVersion $
--             newDeletePolicyVersion
--
--         , requestTagUser $
--             newTagUser
--
--         , requestDeleteInstanceProfile $
--             newDeleteInstanceProfile
--
--         , requestDetachRolePolicy $
--             newDetachRolePolicy
--
--         , requestRemoveRoleFromInstanceProfile $
--             newRemoveRoleFromInstanceProfile
--
--         , requestCreatePolicyVersion $
--             newCreatePolicyVersion
--
--         , requestCreateInstanceProfile $
--             newCreateInstanceProfile
--
--         , requestCreateSAMLProvider $
--             newCreateSAMLProvider
--
--         , requestGetAccountAuthorizationDetails $
--             newGetAccountAuthorizationDetails
--
--         , requestGetServiceLinkedRoleDeletionStatus $
--             newGetServiceLinkedRoleDeletionStatus
--
--         , requestDeleteAccountAlias $
--             newDeleteAccountAlias
--
--         , requestDetachUserPolicy $
--             newDetachUserPolicy
--
--         , requestRemoveUserFromGroup $
--             newRemoveUserFromGroup
--
--         , requestDeleteGroupPolicy $
--             newDeleteGroupPolicy
--
--         , requestTagRole $
--             newTagRole
--
--         , requestPutGroupPolicy $
--             newPutGroupPolicy
--
--         , requestGetLoginProfile $
--             newGetLoginProfile
--
--         , requestGetGroupPolicy $
--             newGetGroupPolicy
--
--         , requestGenerateOrganizationsAccessReport $
--             newGenerateOrganizationsAccessReport
--
--         , requestChangePassword $
--             newChangePassword
--
--         , requestListServerCertificates $
--             newListServerCertificates
--
--         , requestDeleteServiceLinkedRole $
--             newDeleteServiceLinkedRole
--
--         , requestDeletePolicy $
--             newDeletePolicy
--
--         , requestUpdateAssumeRolePolicy $
--             newUpdateAssumeRolePolicy
--
--         , requestGetServiceLastAccessedDetailsWithEntities $
--             newGetServiceLastAccessedDetailsWithEntities
--
--         , requestUntagServerCertificate $
--             newUntagServerCertificate
--
--         , requestGetInstanceProfile $
--             newGetInstanceProfile
--
--         , requestCreateLoginProfile $
--             newCreateLoginProfile
--
--         , requestGetSAMLProvider $
--             newGetSAMLProvider
--
--         , requestAddRoleToInstanceProfile $
--             newAddRoleToInstanceProfile
--
--         , requestListGroupsForUser $
--             newListGroupsForUser
--
--         , requestListEntitiesForPolicy $
--             newListEntitiesForPolicy
--
--         , requestAddUserToGroup $
--             newAddUserToGroup
--
--         , requestTagOpenIDConnectProvider $
--             newTagOpenIDConnectProvider
--
--         , requestSimulatePrincipalPolicy $
--             newSimulatePrincipalPolicy
--
--         , requestGetOrganizationsAccessReport $
--             newGetOrganizationsAccessReport
--
--         , requestGetPolicyVersion $
--             newGetPolicyVersion
--
--         , requestCreateServiceLinkedRole $
--             newCreateServiceLinkedRole
--
--         , requestListServiceSpecificCredentials $
--             newListServiceSpecificCredentials
--
--         , requestDeleteOpenIDConnectProvider $
--             newDeleteOpenIDConnectProvider
--
--         , requestGetUser $
--             newGetUser
--
--         , requestListSigningCertificates $
--             newListSigningCertificates
--
--         , requestDeleteSigningCertificate $
--             newDeleteSigningCertificate
--
--         , requestUpdateSigningCertificate $
--             newUpdateSigningCertificate
--
--         , requestListAttachedUserPolicies $
--             newListAttachedUserPolicies
--
--         , requestRemoveClientIDFromOpenIDConnectProvider $
--             newRemoveClientIDFromOpenIDConnectProvider
--
--         , requestAttachUserPolicy $
--             newAttachUserPolicy
--
--         , requestTagPolicy $
--             newTagPolicy
--
--         , requestCreateServiceSpecificCredential $
--             newCreateServiceSpecificCredential
--
--         , requestListVirtualMFADevices $
--             newListVirtualMFADevices
--
--         , requestResyncMFADevice $
--             newResyncMFADevice
--
--         , requestTagServerCertificate $
--             newTagServerCertificate
--
--         , requestDeleteAccessKey $
--             newDeleteAccessKey
--
--         , requestUpdateAccessKey $
--             newUpdateAccessKey
--
--         , requestListUserTags $
--             newListUserTags
--
--         , requestListAccessKeys $
--             newListAccessKeys
--
--         , requestGetRolePolicy $
--             newGetRolePolicy
--
--         , requestSetSecurityTokenServicePreferences $
--             newSetSecurityTokenServicePreferences
--
--         , requestUntagRole $
--             newUntagRole
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestPutRolePolicy $
--             newPutRolePolicy
--
--         , requestGetContextKeysForCustomPolicy $
--             newGetContextKeysForCustomPolicy
--
--         , requestUploadSigningCertificate $
--             newUploadSigningCertificate
--
--         , requestDeleteRolePolicy $
--             newDeleteRolePolicy
--
--         , requestGetAccountPasswordPolicy $
--             newGetAccountPasswordPolicy
--
--         , requestGetAccessKeyLastUsed $
--             newGetAccessKeyLastUsed
--
--         , requestUpdateUser $
--             newUpdateUser
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestAddClientIDToOpenIDConnectProvider $
--             newAddClientIDToOpenIDConnectProvider
--
--         , requestListRolePolicies $
--             newListRolePolicies
--
--         , requestCreateAccountAlias $
--             newCreateAccountAlias
--
--         , requestListPoliciesGrantingServiceAccess $
--             newListPoliciesGrantingServiceAccess
--
--         , requestListInstanceProfiles $
--             newListInstanceProfiles
--
--         , requestEnableMFADevice $
--             newEnableMFADevice
--
--         , requestListAccountAliases $
--             newListAccountAliases
--
--         , requestDeleteSAMLProvider $
--             newDeleteSAMLProvider
--
--         , requestUpdateSAMLProvider $
--             newUpdateSAMLProvider
--
--         , requestUntagMFADevice $
--             newUntagMFADevice
--
--         , requestCreateGroup $
--             newCreateGroup
--
--         , requestListMFADevices $
--             newListMFADevices
--
--         , requestUntagInstanceProfile $
--             newUntagInstanceProfile
--
--         , requestUploadServerCertificate $
--             newUploadServerCertificate
--
--         , requestSetDefaultPolicyVersion $
--             newSetDefaultPolicyVersion
--
--         , requestListPolicyVersions $
--             newListPolicyVersions
--
--         , requestUpdateRoleDescription $
--             newUpdateRoleDescription
--
--         , requestListSAMLProviders $
--             newListSAMLProviders
--
--         , requestGetServiceLastAccessedDetails $
--             newGetServiceLastAccessedDetails
--
--         , requestGetServerCertificate $
--             newGetServerCertificate
--
--         , requestDeleteGroup $
--             newDeleteGroup
--
--         , requestUpdateGroup $
--             newUpdateGroup
--
--         , requestListGroups $
--             newListGroups
--
--         , requestGenerateCredentialReport $
--             newGenerateCredentialReport
--
--         , requestGetPolicy $
--             newGetPolicy
--
--         , requestListInstanceProfileTags $
--             newListInstanceProfileTags
--
--         , requestUpdateLoginProfile $
--             newUpdateLoginProfile
--
--         , requestDeleteLoginProfile $
--             newDeleteLoginProfile
--
--         , requestListSAMLProviderTags $
--             newListSAMLProviderTags
--
--         , requestGetGroup $
--             newGetGroup
--
--         , requestUntagPolicy $
--             newUntagPolicy
--
--         , requestDeleteServerCertificate $
--             newDeleteServerCertificate
--
--         , requestUpdateServerCertificate $
--             newUpdateServerCertificate
--
--         , requestListAttachedGroupPolicies $
--             newListAttachedGroupPolicies
--
--         , requestListMFADeviceTags $
--             newListMFADeviceTags
--
--           ]

--     , testGroup "response"
--         [ responseGetContextKeysForPrincipalPolicy $
--             newGetContextKeysForPolicyResponse
--
--         , responseListPolicies $
--             newListPoliciesResponse
--
--         , responseCreatePolicy $
--             newCreatePolicyResponse
--
--         , responseListInstanceProfilesForRole $
--             newListInstanceProfilesForRoleResponse
--
--         , responseAttachGroupPolicy $
--             newAttachGroupPolicyResponse
--
--         , responseCreateAccessKey $
--             newCreateAccessKeyResponse
--
--         , responseListRoleTags $
--             newListRoleTagsResponse
--
--         , responseListSSHPublicKeys $
--             newListSSHPublicKeysResponse
--
--         , responseUntagOpenIDConnectProvider $
--             newUntagOpenIDConnectProviderResponse
--
--         , responseListOpenIDConnectProviders $
--             newListOpenIDConnectProvidersResponse
--
--         , responseCreateVirtualMFADevice $
--             newCreateVirtualMFADeviceResponse
--
--         , responseDeleteAccountPasswordPolicy $
--             newDeleteAccountPasswordPolicyResponse
--
--         , responseUpdateAccountPasswordPolicy $
--             newUpdateAccountPasswordPolicyResponse
--
--         , responseAttachRolePolicy $
--             newAttachRolePolicyResponse
--
--         , responseUpdateSSHPublicKey $
--             newUpdateSSHPublicKeyResponse
--
--         , responseDeleteSSHPublicKey $
--             newDeleteSSHPublicKeyResponse
--
--         , responseGetUserPolicy $
--             newGetUserPolicyResponse
--
--         , responseUpdateServiceSpecificCredential $
--             newUpdateServiceSpecificCredentialResponse
--
--         , responseDeleteServiceSpecificCredential $
--             newDeleteServiceSpecificCredentialResponse
--
--         , responseListAttachedRolePolicies $
--             newListAttachedRolePoliciesResponse
--
--         , responseGetRole $
--             newGetRoleResponse
--
--         , responseDeactivateMFADevice $
--             newDeactivateMFADeviceResponse
--
--         , responseCreateOpenIDConnectProvider $
--             newCreateOpenIDConnectProviderResponse
--
--         , responseDeleteVirtualMFADevice $
--             newDeleteVirtualMFADeviceResponse
--
--         , responseListRoles $
--             newListRolesResponse
--
--         , responseListUserPolicies $
--             newListUserPoliciesResponse
--
--         , responseListOpenIDConnectProviderTags $
--             newListOpenIDConnectProviderTagsResponse
--
--         , responsePutRolePermissionsBoundary $
--             newPutRolePermissionsBoundaryResponse
--
--         , responseUploadSSHPublicKey $
--             newUploadSSHPublicKeyResponse
--
--         , responseDeleteRolePermissionsBoundary $
--             newDeleteRolePermissionsBoundaryResponse
--
--         , responseSimulateCustomPolicy $
--             newSimulatePolicyResponse
--
--         , responseUpdateRole $
--             newUpdateRoleResponse
--
--         , responseDeleteRole $
--             newDeleteRoleResponse
--
--         , responseListUsers $
--             newListUsersResponse
--
--         , responseUpdateOpenIDConnectProviderThumbprint $
--             newUpdateOpenIDConnectProviderThumbprintResponse
--
--         , responsePutUserPolicy $
--             newPutUserPolicyResponse
--
--         , responseTagMFADevice $
--             newTagMFADeviceResponse
--
--         , responseGetSSHPublicKey $
--             newGetSSHPublicKeyResponse
--
--         , responseUntagUser $
--             newUntagUserResponse
--
--         , responseDetachGroupPolicy $
--             newDetachGroupPolicyResponse
--
--         , responseTagInstanceProfile $
--             newTagInstanceProfileResponse
--
--         , responseGetOpenIDConnectProvider $
--             newGetOpenIDConnectProviderResponse
--
--         , responsePutUserPermissionsBoundary $
--             newPutUserPermissionsBoundaryResponse
--
--         , responseDeleteUserPolicy $
--             newDeleteUserPolicyResponse
--
--         , responseTagSAMLProvider $
--             newTagSAMLProviderResponse
--
--         , responseDeleteUserPermissionsBoundary $
--             newDeleteUserPermissionsBoundaryResponse
--
--         , responseCreateRole $
--             newCreateRoleResponse
--
--         , responseResetServiceSpecificCredential $
--             newResetServiceSpecificCredentialResponse
--
--         , responseUntagSAMLProvider $
--             newUntagSAMLProviderResponse
--
--         , responseGetCredentialReport $
--             newGetCredentialReportResponse
--
--         , responseListServerCertificateTags $
--             newListServerCertificateTagsResponse
--
--         , responseGetAccountSummary $
--             newGetAccountSummaryResponse
--
--         , responseGenerateServiceLastAccessedDetails $
--             newGenerateServiceLastAccessedDetailsResponse
--
--         , responseListPolicyTags $
--             newListPolicyTagsResponse
--
--         , responseListGroupPolicies $
--             newListGroupPoliciesResponse
--
--         , responseDeletePolicyVersion $
--             newDeletePolicyVersionResponse
--
--         , responseTagUser $
--             newTagUserResponse
--
--         , responseDeleteInstanceProfile $
--             newDeleteInstanceProfileResponse
--
--         , responseDetachRolePolicy $
--             newDetachRolePolicyResponse
--
--         , responseRemoveRoleFromInstanceProfile $
--             newRemoveRoleFromInstanceProfileResponse
--
--         , responseCreatePolicyVersion $
--             newCreatePolicyVersionResponse
--
--         , responseCreateInstanceProfile $
--             newCreateInstanceProfileResponse
--
--         , responseCreateSAMLProvider $
--             newCreateSAMLProviderResponse
--
--         , responseGetAccountAuthorizationDetails $
--             newGetAccountAuthorizationDetailsResponse
--
--         , responseGetServiceLinkedRoleDeletionStatus $
--             newGetServiceLinkedRoleDeletionStatusResponse
--
--         , responseDeleteAccountAlias $
--             newDeleteAccountAliasResponse
--
--         , responseDetachUserPolicy $
--             newDetachUserPolicyResponse
--
--         , responseRemoveUserFromGroup $
--             newRemoveUserFromGroupResponse
--
--         , responseDeleteGroupPolicy $
--             newDeleteGroupPolicyResponse
--
--         , responseTagRole $
--             newTagRoleResponse
--
--         , responsePutGroupPolicy $
--             newPutGroupPolicyResponse
--
--         , responseGetLoginProfile $
--             newGetLoginProfileResponse
--
--         , responseGetGroupPolicy $
--             newGetGroupPolicyResponse
--
--         , responseGenerateOrganizationsAccessReport $
--             newGenerateOrganizationsAccessReportResponse
--
--         , responseChangePassword $
--             newChangePasswordResponse
--
--         , responseListServerCertificates $
--             newListServerCertificatesResponse
--
--         , responseDeleteServiceLinkedRole $
--             newDeleteServiceLinkedRoleResponse
--
--         , responseDeletePolicy $
--             newDeletePolicyResponse
--
--         , responseUpdateAssumeRolePolicy $
--             newUpdateAssumeRolePolicyResponse
--
--         , responseGetServiceLastAccessedDetailsWithEntities $
--             newGetServiceLastAccessedDetailsWithEntitiesResponse
--
--         , responseUntagServerCertificate $
--             newUntagServerCertificateResponse
--
--         , responseGetInstanceProfile $
--             newGetInstanceProfileResponse
--
--         , responseCreateLoginProfile $
--             newCreateLoginProfileResponse
--
--         , responseGetSAMLProvider $
--             newGetSAMLProviderResponse
--
--         , responseAddRoleToInstanceProfile $
--             newAddRoleToInstanceProfileResponse
--
--         , responseListGroupsForUser $
--             newListGroupsForUserResponse
--
--         , responseListEntitiesForPolicy $
--             newListEntitiesForPolicyResponse
--
--         , responseAddUserToGroup $
--             newAddUserToGroupResponse
--
--         , responseTagOpenIDConnectProvider $
--             newTagOpenIDConnectProviderResponse
--
--         , responseSimulatePrincipalPolicy $
--             newSimulatePolicyResponse
--
--         , responseGetOrganizationsAccessReport $
--             newGetOrganizationsAccessReportResponse
--
--         , responseGetPolicyVersion $
--             newGetPolicyVersionResponse
--
--         , responseCreateServiceLinkedRole $
--             newCreateServiceLinkedRoleResponse
--
--         , responseListServiceSpecificCredentials $
--             newListServiceSpecificCredentialsResponse
--
--         , responseDeleteOpenIDConnectProvider $
--             newDeleteOpenIDConnectProviderResponse
--
--         , responseGetUser $
--             newGetUserResponse
--
--         , responseListSigningCertificates $
--             newListSigningCertificatesResponse
--
--         , responseDeleteSigningCertificate $
--             newDeleteSigningCertificateResponse
--
--         , responseUpdateSigningCertificate $
--             newUpdateSigningCertificateResponse
--
--         , responseListAttachedUserPolicies $
--             newListAttachedUserPoliciesResponse
--
--         , responseRemoveClientIDFromOpenIDConnectProvider $
--             newRemoveClientIDFromOpenIDConnectProviderResponse
--
--         , responseAttachUserPolicy $
--             newAttachUserPolicyResponse
--
--         , responseTagPolicy $
--             newTagPolicyResponse
--
--         , responseCreateServiceSpecificCredential $
--             newCreateServiceSpecificCredentialResponse
--
--         , responseListVirtualMFADevices $
--             newListVirtualMFADevicesResponse
--
--         , responseResyncMFADevice $
--             newResyncMFADeviceResponse
--
--         , responseTagServerCertificate $
--             newTagServerCertificateResponse
--
--         , responseDeleteAccessKey $
--             newDeleteAccessKeyResponse
--
--         , responseUpdateAccessKey $
--             newUpdateAccessKeyResponse
--
--         , responseListUserTags $
--             newListUserTagsResponse
--
--         , responseListAccessKeys $
--             newListAccessKeysResponse
--
--         , responseGetRolePolicy $
--             newGetRolePolicyResponse
--
--         , responseSetSecurityTokenServicePreferences $
--             newSetSecurityTokenServicePreferencesResponse
--
--         , responseUntagRole $
--             newUntagRoleResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responsePutRolePolicy $
--             newPutRolePolicyResponse
--
--         , responseGetContextKeysForCustomPolicy $
--             newGetContextKeysForPolicyResponse
--
--         , responseUploadSigningCertificate $
--             newUploadSigningCertificateResponse
--
--         , responseDeleteRolePolicy $
--             newDeleteRolePolicyResponse
--
--         , responseGetAccountPasswordPolicy $
--             newGetAccountPasswordPolicyResponse
--
--         , responseGetAccessKeyLastUsed $
--             newGetAccessKeyLastUsedResponse
--
--         , responseUpdateUser $
--             newUpdateUserResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseAddClientIDToOpenIDConnectProvider $
--             newAddClientIDToOpenIDConnectProviderResponse
--
--         , responseListRolePolicies $
--             newListRolePoliciesResponse
--
--         , responseCreateAccountAlias $
--             newCreateAccountAliasResponse
--
--         , responseListPoliciesGrantingServiceAccess $
--             newListPoliciesGrantingServiceAccessResponse
--
--         , responseListInstanceProfiles $
--             newListInstanceProfilesResponse
--
--         , responseEnableMFADevice $
--             newEnableMFADeviceResponse
--
--         , responseListAccountAliases $
--             newListAccountAliasesResponse
--
--         , responseDeleteSAMLProvider $
--             newDeleteSAMLProviderResponse
--
--         , responseUpdateSAMLProvider $
--             newUpdateSAMLProviderResponse
--
--         , responseUntagMFADevice $
--             newUntagMFADeviceResponse
--
--         , responseCreateGroup $
--             newCreateGroupResponse
--
--         , responseListMFADevices $
--             newListMFADevicesResponse
--
--         , responseUntagInstanceProfile $
--             newUntagInstanceProfileResponse
--
--         , responseUploadServerCertificate $
--             newUploadServerCertificateResponse
--
--         , responseSetDefaultPolicyVersion $
--             newSetDefaultPolicyVersionResponse
--
--         , responseListPolicyVersions $
--             newListPolicyVersionsResponse
--
--         , responseUpdateRoleDescription $
--             newUpdateRoleDescriptionResponse
--
--         , responseListSAMLProviders $
--             newListSAMLProvidersResponse
--
--         , responseGetServiceLastAccessedDetails $
--             newGetServiceLastAccessedDetailsResponse
--
--         , responseGetServerCertificate $
--             newGetServerCertificateResponse
--
--         , responseDeleteGroup $
--             newDeleteGroupResponse
--
--         , responseUpdateGroup $
--             newUpdateGroupResponse
--
--         , responseListGroups $
--             newListGroupsResponse
--
--         , responseGenerateCredentialReport $
--             newGenerateCredentialReportResponse
--
--         , responseGetPolicy $
--             newGetPolicyResponse
--
--         , responseListInstanceProfileTags $
--             newListInstanceProfileTagsResponse
--
--         , responseUpdateLoginProfile $
--             newUpdateLoginProfileResponse
--
--         , responseDeleteLoginProfile $
--             newDeleteLoginProfileResponse
--
--         , responseListSAMLProviderTags $
--             newListSAMLProviderTagsResponse
--
--         , responseGetGroup $
--             newGetGroupResponse
--
--         , responseUntagPolicy $
--             newUntagPolicyResponse
--
--         , responseDeleteServerCertificate $
--             newDeleteServerCertificateResponse
--
--         , responseUpdateServerCertificate $
--             newUpdateServerCertificateResponse
--
--         , responseListAttachedGroupPolicies $
--             newListAttachedGroupPoliciesResponse
--
--         , responseListMFADeviceTags $
--             newListMFADeviceTagsResponse
--
--           ]
--     ]

-- Requests

requestGetContextKeysForPrincipalPolicy :: GetContextKeysForPrincipalPolicy -> TestTree
requestGetContextKeysForPrincipalPolicy =
  req
    "GetContextKeysForPrincipalPolicy"
    "fixture/GetContextKeysForPrincipalPolicy.yaml"

requestListPolicies :: ListPolicies -> TestTree
requestListPolicies =
  req
    "ListPolicies"
    "fixture/ListPolicies.yaml"

requestCreatePolicy :: CreatePolicy -> TestTree
requestCreatePolicy =
  req
    "CreatePolicy"
    "fixture/CreatePolicy.yaml"

requestListInstanceProfilesForRole :: ListInstanceProfilesForRole -> TestTree
requestListInstanceProfilesForRole =
  req
    "ListInstanceProfilesForRole"
    "fixture/ListInstanceProfilesForRole.yaml"

requestAttachGroupPolicy :: AttachGroupPolicy -> TestTree
requestAttachGroupPolicy =
  req
    "AttachGroupPolicy"
    "fixture/AttachGroupPolicy.yaml"

requestCreateAccessKey :: CreateAccessKey -> TestTree
requestCreateAccessKey =
  req
    "CreateAccessKey"
    "fixture/CreateAccessKey.yaml"

requestListRoleTags :: ListRoleTags -> TestTree
requestListRoleTags =
  req
    "ListRoleTags"
    "fixture/ListRoleTags.yaml"

requestListSSHPublicKeys :: ListSSHPublicKeys -> TestTree
requestListSSHPublicKeys =
  req
    "ListSSHPublicKeys"
    "fixture/ListSSHPublicKeys.yaml"

requestUntagOpenIDConnectProvider :: UntagOpenIDConnectProvider -> TestTree
requestUntagOpenIDConnectProvider =
  req
    "UntagOpenIDConnectProvider"
    "fixture/UntagOpenIDConnectProvider.yaml"

requestListOpenIDConnectProviders :: ListOpenIDConnectProviders -> TestTree
requestListOpenIDConnectProviders =
  req
    "ListOpenIDConnectProviders"
    "fixture/ListOpenIDConnectProviders.yaml"

requestCreateVirtualMFADevice :: CreateVirtualMFADevice -> TestTree
requestCreateVirtualMFADevice =
  req
    "CreateVirtualMFADevice"
    "fixture/CreateVirtualMFADevice.yaml"

requestDeleteAccountPasswordPolicy :: DeleteAccountPasswordPolicy -> TestTree
requestDeleteAccountPasswordPolicy =
  req
    "DeleteAccountPasswordPolicy"
    "fixture/DeleteAccountPasswordPolicy.yaml"

requestUpdateAccountPasswordPolicy :: UpdateAccountPasswordPolicy -> TestTree
requestUpdateAccountPasswordPolicy =
  req
    "UpdateAccountPasswordPolicy"
    "fixture/UpdateAccountPasswordPolicy.yaml"

requestAttachRolePolicy :: AttachRolePolicy -> TestTree
requestAttachRolePolicy =
  req
    "AttachRolePolicy"
    "fixture/AttachRolePolicy.yaml"

requestUpdateSSHPublicKey :: UpdateSSHPublicKey -> TestTree
requestUpdateSSHPublicKey =
  req
    "UpdateSSHPublicKey"
    "fixture/UpdateSSHPublicKey.yaml"

requestDeleteSSHPublicKey :: DeleteSSHPublicKey -> TestTree
requestDeleteSSHPublicKey =
  req
    "DeleteSSHPublicKey"
    "fixture/DeleteSSHPublicKey.yaml"

requestGetUserPolicy :: GetUserPolicy -> TestTree
requestGetUserPolicy =
  req
    "GetUserPolicy"
    "fixture/GetUserPolicy.yaml"

requestUpdateServiceSpecificCredential :: UpdateServiceSpecificCredential -> TestTree
requestUpdateServiceSpecificCredential =
  req
    "UpdateServiceSpecificCredential"
    "fixture/UpdateServiceSpecificCredential.yaml"

requestDeleteServiceSpecificCredential :: DeleteServiceSpecificCredential -> TestTree
requestDeleteServiceSpecificCredential =
  req
    "DeleteServiceSpecificCredential"
    "fixture/DeleteServiceSpecificCredential.yaml"

requestListAttachedRolePolicies :: ListAttachedRolePolicies -> TestTree
requestListAttachedRolePolicies =
  req
    "ListAttachedRolePolicies"
    "fixture/ListAttachedRolePolicies.yaml"

requestGetRole :: GetRole -> TestTree
requestGetRole =
  req
    "GetRole"
    "fixture/GetRole.yaml"

requestDeactivateMFADevice :: DeactivateMFADevice -> TestTree
requestDeactivateMFADevice =
  req
    "DeactivateMFADevice"
    "fixture/DeactivateMFADevice.yaml"

requestCreateOpenIDConnectProvider :: CreateOpenIDConnectProvider -> TestTree
requestCreateOpenIDConnectProvider =
  req
    "CreateOpenIDConnectProvider"
    "fixture/CreateOpenIDConnectProvider.yaml"

requestDeleteVirtualMFADevice :: DeleteVirtualMFADevice -> TestTree
requestDeleteVirtualMFADevice =
  req
    "DeleteVirtualMFADevice"
    "fixture/DeleteVirtualMFADevice.yaml"

requestListRoles :: ListRoles -> TestTree
requestListRoles =
  req
    "ListRoles"
    "fixture/ListRoles.yaml"

requestListUserPolicies :: ListUserPolicies -> TestTree
requestListUserPolicies =
  req
    "ListUserPolicies"
    "fixture/ListUserPolicies.yaml"

requestListOpenIDConnectProviderTags :: ListOpenIDConnectProviderTags -> TestTree
requestListOpenIDConnectProviderTags =
  req
    "ListOpenIDConnectProviderTags"
    "fixture/ListOpenIDConnectProviderTags.yaml"

requestPutRolePermissionsBoundary :: PutRolePermissionsBoundary -> TestTree
requestPutRolePermissionsBoundary =
  req
    "PutRolePermissionsBoundary"
    "fixture/PutRolePermissionsBoundary.yaml"

requestUploadSSHPublicKey :: UploadSSHPublicKey -> TestTree
requestUploadSSHPublicKey =
  req
    "UploadSSHPublicKey"
    "fixture/UploadSSHPublicKey.yaml"

requestDeleteRolePermissionsBoundary :: DeleteRolePermissionsBoundary -> TestTree
requestDeleteRolePermissionsBoundary =
  req
    "DeleteRolePermissionsBoundary"
    "fixture/DeleteRolePermissionsBoundary.yaml"

requestSimulateCustomPolicy :: SimulateCustomPolicy -> TestTree
requestSimulateCustomPolicy =
  req
    "SimulateCustomPolicy"
    "fixture/SimulateCustomPolicy.yaml"

requestUpdateRole :: UpdateRole -> TestTree
requestUpdateRole =
  req
    "UpdateRole"
    "fixture/UpdateRole.yaml"

requestDeleteRole :: DeleteRole -> TestTree
requestDeleteRole =
  req
    "DeleteRole"
    "fixture/DeleteRole.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers =
  req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestUpdateOpenIDConnectProviderThumbprint :: UpdateOpenIDConnectProviderThumbprint -> TestTree
requestUpdateOpenIDConnectProviderThumbprint =
  req
    "UpdateOpenIDConnectProviderThumbprint"
    "fixture/UpdateOpenIDConnectProviderThumbprint.yaml"

requestPutUserPolicy :: PutUserPolicy -> TestTree
requestPutUserPolicy =
  req
    "PutUserPolicy"
    "fixture/PutUserPolicy.yaml"

requestTagMFADevice :: TagMFADevice -> TestTree
requestTagMFADevice =
  req
    "TagMFADevice"
    "fixture/TagMFADevice.yaml"

requestGetSSHPublicKey :: GetSSHPublicKey -> TestTree
requestGetSSHPublicKey =
  req
    "GetSSHPublicKey"
    "fixture/GetSSHPublicKey.yaml"

requestUntagUser :: UntagUser -> TestTree
requestUntagUser =
  req
    "UntagUser"
    "fixture/UntagUser.yaml"

requestDetachGroupPolicy :: DetachGroupPolicy -> TestTree
requestDetachGroupPolicy =
  req
    "DetachGroupPolicy"
    "fixture/DetachGroupPolicy.yaml"

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

requestTagSAMLProvider :: TagSAMLProvider -> TestTree
requestTagSAMLProvider =
  req
    "TagSAMLProvider"
    "fixture/TagSAMLProvider.yaml"

requestDeleteUserPermissionsBoundary :: DeleteUserPermissionsBoundary -> TestTree
requestDeleteUserPermissionsBoundary =
  req
    "DeleteUserPermissionsBoundary"
    "fixture/DeleteUserPermissionsBoundary.yaml"

requestCreateRole :: CreateRole -> TestTree
requestCreateRole =
  req
    "CreateRole"
    "fixture/CreateRole.yaml"

requestResetServiceSpecificCredential :: ResetServiceSpecificCredential -> TestTree
requestResetServiceSpecificCredential =
  req
    "ResetServiceSpecificCredential"
    "fixture/ResetServiceSpecificCredential.yaml"

requestUntagSAMLProvider :: UntagSAMLProvider -> TestTree
requestUntagSAMLProvider =
  req
    "UntagSAMLProvider"
    "fixture/UntagSAMLProvider.yaml"

requestGetCredentialReport :: GetCredentialReport -> TestTree
requestGetCredentialReport =
  req
    "GetCredentialReport"
    "fixture/GetCredentialReport.yaml"

requestListServerCertificateTags :: ListServerCertificateTags -> TestTree
requestListServerCertificateTags =
  req
    "ListServerCertificateTags"
    "fixture/ListServerCertificateTags.yaml"

requestGetAccountSummary :: GetAccountSummary -> TestTree
requestGetAccountSummary =
  req
    "GetAccountSummary"
    "fixture/GetAccountSummary.yaml"

requestGenerateServiceLastAccessedDetails :: GenerateServiceLastAccessedDetails -> TestTree
requestGenerateServiceLastAccessedDetails =
  req
    "GenerateServiceLastAccessedDetails"
    "fixture/GenerateServiceLastAccessedDetails.yaml"

requestListPolicyTags :: ListPolicyTags -> TestTree
requestListPolicyTags =
  req
    "ListPolicyTags"
    "fixture/ListPolicyTags.yaml"

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

requestTagUser :: TagUser -> TestTree
requestTagUser =
  req
    "TagUser"
    "fixture/TagUser.yaml"

requestDeleteInstanceProfile :: DeleteInstanceProfile -> TestTree
requestDeleteInstanceProfile =
  req
    "DeleteInstanceProfile"
    "fixture/DeleteInstanceProfile.yaml"

requestDetachRolePolicy :: DetachRolePolicy -> TestTree
requestDetachRolePolicy =
  req
    "DetachRolePolicy"
    "fixture/DetachRolePolicy.yaml"

requestRemoveRoleFromInstanceProfile :: RemoveRoleFromInstanceProfile -> TestTree
requestRemoveRoleFromInstanceProfile =
  req
    "RemoveRoleFromInstanceProfile"
    "fixture/RemoveRoleFromInstanceProfile.yaml"

requestCreatePolicyVersion :: CreatePolicyVersion -> TestTree
requestCreatePolicyVersion =
  req
    "CreatePolicyVersion"
    "fixture/CreatePolicyVersion.yaml"

requestCreateInstanceProfile :: CreateInstanceProfile -> TestTree
requestCreateInstanceProfile =
  req
    "CreateInstanceProfile"
    "fixture/CreateInstanceProfile.yaml"

requestCreateSAMLProvider :: CreateSAMLProvider -> TestTree
requestCreateSAMLProvider =
  req
    "CreateSAMLProvider"
    "fixture/CreateSAMLProvider.yaml"

requestGetAccountAuthorizationDetails :: GetAccountAuthorizationDetails -> TestTree
requestGetAccountAuthorizationDetails =
  req
    "GetAccountAuthorizationDetails"
    "fixture/GetAccountAuthorizationDetails.yaml"

requestGetServiceLinkedRoleDeletionStatus :: GetServiceLinkedRoleDeletionStatus -> TestTree
requestGetServiceLinkedRoleDeletionStatus =
  req
    "GetServiceLinkedRoleDeletionStatus"
    "fixture/GetServiceLinkedRoleDeletionStatus.yaml"

requestDeleteAccountAlias :: DeleteAccountAlias -> TestTree
requestDeleteAccountAlias =
  req
    "DeleteAccountAlias"
    "fixture/DeleteAccountAlias.yaml"

requestDetachUserPolicy :: DetachUserPolicy -> TestTree
requestDetachUserPolicy =
  req
    "DetachUserPolicy"
    "fixture/DetachUserPolicy.yaml"

requestRemoveUserFromGroup :: RemoveUserFromGroup -> TestTree
requestRemoveUserFromGroup =
  req
    "RemoveUserFromGroup"
    "fixture/RemoveUserFromGroup.yaml"

requestDeleteGroupPolicy :: DeleteGroupPolicy -> TestTree
requestDeleteGroupPolicy =
  req
    "DeleteGroupPolicy"
    "fixture/DeleteGroupPolicy.yaml"

requestTagRole :: TagRole -> TestTree
requestTagRole =
  req
    "TagRole"
    "fixture/TagRole.yaml"

requestPutGroupPolicy :: PutGroupPolicy -> TestTree
requestPutGroupPolicy =
  req
    "PutGroupPolicy"
    "fixture/PutGroupPolicy.yaml"

requestGetLoginProfile :: GetLoginProfile -> TestTree
requestGetLoginProfile =
  req
    "GetLoginProfile"
    "fixture/GetLoginProfile.yaml"

requestGetGroupPolicy :: GetGroupPolicy -> TestTree
requestGetGroupPolicy =
  req
    "GetGroupPolicy"
    "fixture/GetGroupPolicy.yaml"

requestGenerateOrganizationsAccessReport :: GenerateOrganizationsAccessReport -> TestTree
requestGenerateOrganizationsAccessReport =
  req
    "GenerateOrganizationsAccessReport"
    "fixture/GenerateOrganizationsAccessReport.yaml"

requestChangePassword :: ChangePassword -> TestTree
requestChangePassword =
  req
    "ChangePassword"
    "fixture/ChangePassword.yaml"

requestListServerCertificates :: ListServerCertificates -> TestTree
requestListServerCertificates =
  req
    "ListServerCertificates"
    "fixture/ListServerCertificates.yaml"

requestDeleteServiceLinkedRole :: DeleteServiceLinkedRole -> TestTree
requestDeleteServiceLinkedRole =
  req
    "DeleteServiceLinkedRole"
    "fixture/DeleteServiceLinkedRole.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy =
  req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

requestUpdateAssumeRolePolicy :: UpdateAssumeRolePolicy -> TestTree
requestUpdateAssumeRolePolicy =
  req
    "UpdateAssumeRolePolicy"
    "fixture/UpdateAssumeRolePolicy.yaml"

requestGetServiceLastAccessedDetailsWithEntities :: GetServiceLastAccessedDetailsWithEntities -> TestTree
requestGetServiceLastAccessedDetailsWithEntities =
  req
    "GetServiceLastAccessedDetailsWithEntities"
    "fixture/GetServiceLastAccessedDetailsWithEntities.yaml"

requestUntagServerCertificate :: UntagServerCertificate -> TestTree
requestUntagServerCertificate =
  req
    "UntagServerCertificate"
    "fixture/UntagServerCertificate.yaml"

requestGetInstanceProfile :: GetInstanceProfile -> TestTree
requestGetInstanceProfile =
  req
    "GetInstanceProfile"
    "fixture/GetInstanceProfile.yaml"

requestCreateLoginProfile :: CreateLoginProfile -> TestTree
requestCreateLoginProfile =
  req
    "CreateLoginProfile"
    "fixture/CreateLoginProfile.yaml"

requestGetSAMLProvider :: GetSAMLProvider -> TestTree
requestGetSAMLProvider =
  req
    "GetSAMLProvider"
    "fixture/GetSAMLProvider.yaml"

requestAddRoleToInstanceProfile :: AddRoleToInstanceProfile -> TestTree
requestAddRoleToInstanceProfile =
  req
    "AddRoleToInstanceProfile"
    "fixture/AddRoleToInstanceProfile.yaml"

requestListGroupsForUser :: ListGroupsForUser -> TestTree
requestListGroupsForUser =
  req
    "ListGroupsForUser"
    "fixture/ListGroupsForUser.yaml"

requestListEntitiesForPolicy :: ListEntitiesForPolicy -> TestTree
requestListEntitiesForPolicy =
  req
    "ListEntitiesForPolicy"
    "fixture/ListEntitiesForPolicy.yaml"

requestAddUserToGroup :: AddUserToGroup -> TestTree
requestAddUserToGroup =
  req
    "AddUserToGroup"
    "fixture/AddUserToGroup.yaml"

requestTagOpenIDConnectProvider :: TagOpenIDConnectProvider -> TestTree
requestTagOpenIDConnectProvider =
  req
    "TagOpenIDConnectProvider"
    "fixture/TagOpenIDConnectProvider.yaml"

requestSimulatePrincipalPolicy :: SimulatePrincipalPolicy -> TestTree
requestSimulatePrincipalPolicy =
  req
    "SimulatePrincipalPolicy"
    "fixture/SimulatePrincipalPolicy.yaml"

requestGetOrganizationsAccessReport :: GetOrganizationsAccessReport -> TestTree
requestGetOrganizationsAccessReport =
  req
    "GetOrganizationsAccessReport"
    "fixture/GetOrganizationsAccessReport.yaml"

requestGetPolicyVersion :: GetPolicyVersion -> TestTree
requestGetPolicyVersion =
  req
    "GetPolicyVersion"
    "fixture/GetPolicyVersion.yaml"

requestCreateServiceLinkedRole :: CreateServiceLinkedRole -> TestTree
requestCreateServiceLinkedRole =
  req
    "CreateServiceLinkedRole"
    "fixture/CreateServiceLinkedRole.yaml"

requestListServiceSpecificCredentials :: ListServiceSpecificCredentials -> TestTree
requestListServiceSpecificCredentials =
  req
    "ListServiceSpecificCredentials"
    "fixture/ListServiceSpecificCredentials.yaml"

requestDeleteOpenIDConnectProvider :: DeleteOpenIDConnectProvider -> TestTree
requestDeleteOpenIDConnectProvider =
  req
    "DeleteOpenIDConnectProvider"
    "fixture/DeleteOpenIDConnectProvider.yaml"

requestGetUser :: GetUser -> TestTree
requestGetUser =
  req
    "GetUser"
    "fixture/GetUser.yaml"

requestListSigningCertificates :: ListSigningCertificates -> TestTree
requestListSigningCertificates =
  req
    "ListSigningCertificates"
    "fixture/ListSigningCertificates.yaml"

requestDeleteSigningCertificate :: DeleteSigningCertificate -> TestTree
requestDeleteSigningCertificate =
  req
    "DeleteSigningCertificate"
    "fixture/DeleteSigningCertificate.yaml"

requestUpdateSigningCertificate :: UpdateSigningCertificate -> TestTree
requestUpdateSigningCertificate =
  req
    "UpdateSigningCertificate"
    "fixture/UpdateSigningCertificate.yaml"

requestListAttachedUserPolicies :: ListAttachedUserPolicies -> TestTree
requestListAttachedUserPolicies =
  req
    "ListAttachedUserPolicies"
    "fixture/ListAttachedUserPolicies.yaml"

requestRemoveClientIDFromOpenIDConnectProvider :: RemoveClientIDFromOpenIDConnectProvider -> TestTree
requestRemoveClientIDFromOpenIDConnectProvider =
  req
    "RemoveClientIDFromOpenIDConnectProvider"
    "fixture/RemoveClientIDFromOpenIDConnectProvider.yaml"

requestAttachUserPolicy :: AttachUserPolicy -> TestTree
requestAttachUserPolicy =
  req
    "AttachUserPolicy"
    "fixture/AttachUserPolicy.yaml"

requestTagPolicy :: TagPolicy -> TestTree
requestTagPolicy =
  req
    "TagPolicy"
    "fixture/TagPolicy.yaml"

requestCreateServiceSpecificCredential :: CreateServiceSpecificCredential -> TestTree
requestCreateServiceSpecificCredential =
  req
    "CreateServiceSpecificCredential"
    "fixture/CreateServiceSpecificCredential.yaml"

requestListVirtualMFADevices :: ListVirtualMFADevices -> TestTree
requestListVirtualMFADevices =
  req
    "ListVirtualMFADevices"
    "fixture/ListVirtualMFADevices.yaml"

requestResyncMFADevice :: ResyncMFADevice -> TestTree
requestResyncMFADevice =
  req
    "ResyncMFADevice"
    "fixture/ResyncMFADevice.yaml"

requestTagServerCertificate :: TagServerCertificate -> TestTree
requestTagServerCertificate =
  req
    "TagServerCertificate"
    "fixture/TagServerCertificate.yaml"

requestDeleteAccessKey :: DeleteAccessKey -> TestTree
requestDeleteAccessKey =
  req
    "DeleteAccessKey"
    "fixture/DeleteAccessKey.yaml"

requestUpdateAccessKey :: UpdateAccessKey -> TestTree
requestUpdateAccessKey =
  req
    "UpdateAccessKey"
    "fixture/UpdateAccessKey.yaml"

requestListUserTags :: ListUserTags -> TestTree
requestListUserTags =
  req
    "ListUserTags"
    "fixture/ListUserTags.yaml"

requestListAccessKeys :: ListAccessKeys -> TestTree
requestListAccessKeys =
  req
    "ListAccessKeys"
    "fixture/ListAccessKeys.yaml"

requestGetRolePolicy :: GetRolePolicy -> TestTree
requestGetRolePolicy =
  req
    "GetRolePolicy"
    "fixture/GetRolePolicy.yaml"

requestSetSecurityTokenServicePreferences :: SetSecurityTokenServicePreferences -> TestTree
requestSetSecurityTokenServicePreferences =
  req
    "SetSecurityTokenServicePreferences"
    "fixture/SetSecurityTokenServicePreferences.yaml"

requestUntagRole :: UntagRole -> TestTree
requestUntagRole =
  req
    "UntagRole"
    "fixture/UntagRole.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestPutRolePolicy :: PutRolePolicy -> TestTree
requestPutRolePolicy =
  req
    "PutRolePolicy"
    "fixture/PutRolePolicy.yaml"

requestGetContextKeysForCustomPolicy :: GetContextKeysForCustomPolicy -> TestTree
requestGetContextKeysForCustomPolicy =
  req
    "GetContextKeysForCustomPolicy"
    "fixture/GetContextKeysForCustomPolicy.yaml"

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

requestGetAccountPasswordPolicy :: GetAccountPasswordPolicy -> TestTree
requestGetAccountPasswordPolicy =
  req
    "GetAccountPasswordPolicy"
    "fixture/GetAccountPasswordPolicy.yaml"

requestGetAccessKeyLastUsed :: GetAccessKeyLastUsed -> TestTree
requestGetAccessKeyLastUsed =
  req
    "GetAccessKeyLastUsed"
    "fixture/GetAccessKeyLastUsed.yaml"

requestUpdateUser :: UpdateUser -> TestTree
requestUpdateUser =
  req
    "UpdateUser"
    "fixture/UpdateUser.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestAddClientIDToOpenIDConnectProvider :: AddClientIDToOpenIDConnectProvider -> TestTree
requestAddClientIDToOpenIDConnectProvider =
  req
    "AddClientIDToOpenIDConnectProvider"
    "fixture/AddClientIDToOpenIDConnectProvider.yaml"

requestListRolePolicies :: ListRolePolicies -> TestTree
requestListRolePolicies =
  req
    "ListRolePolicies"
    "fixture/ListRolePolicies.yaml"

requestCreateAccountAlias :: CreateAccountAlias -> TestTree
requestCreateAccountAlias =
  req
    "CreateAccountAlias"
    "fixture/CreateAccountAlias.yaml"

requestListPoliciesGrantingServiceAccess :: ListPoliciesGrantingServiceAccess -> TestTree
requestListPoliciesGrantingServiceAccess =
  req
    "ListPoliciesGrantingServiceAccess"
    "fixture/ListPoliciesGrantingServiceAccess.yaml"

requestListInstanceProfiles :: ListInstanceProfiles -> TestTree
requestListInstanceProfiles =
  req
    "ListInstanceProfiles"
    "fixture/ListInstanceProfiles.yaml"

requestEnableMFADevice :: EnableMFADevice -> TestTree
requestEnableMFADevice =
  req
    "EnableMFADevice"
    "fixture/EnableMFADevice.yaml"

requestListAccountAliases :: ListAccountAliases -> TestTree
requestListAccountAliases =
  req
    "ListAccountAliases"
    "fixture/ListAccountAliases.yaml"

requestDeleteSAMLProvider :: DeleteSAMLProvider -> TestTree
requestDeleteSAMLProvider =
  req
    "DeleteSAMLProvider"
    "fixture/DeleteSAMLProvider.yaml"

requestUpdateSAMLProvider :: UpdateSAMLProvider -> TestTree
requestUpdateSAMLProvider =
  req
    "UpdateSAMLProvider"
    "fixture/UpdateSAMLProvider.yaml"

requestUntagMFADevice :: UntagMFADevice -> TestTree
requestUntagMFADevice =
  req
    "UntagMFADevice"
    "fixture/UntagMFADevice.yaml"

requestCreateGroup :: CreateGroup -> TestTree
requestCreateGroup =
  req
    "CreateGroup"
    "fixture/CreateGroup.yaml"

requestListMFADevices :: ListMFADevices -> TestTree
requestListMFADevices =
  req
    "ListMFADevices"
    "fixture/ListMFADevices.yaml"

requestUntagInstanceProfile :: UntagInstanceProfile -> TestTree
requestUntagInstanceProfile =
  req
    "UntagInstanceProfile"
    "fixture/UntagInstanceProfile.yaml"

requestUploadServerCertificate :: UploadServerCertificate -> TestTree
requestUploadServerCertificate =
  req
    "UploadServerCertificate"
    "fixture/UploadServerCertificate.yaml"

requestSetDefaultPolicyVersion :: SetDefaultPolicyVersion -> TestTree
requestSetDefaultPolicyVersion =
  req
    "SetDefaultPolicyVersion"
    "fixture/SetDefaultPolicyVersion.yaml"

requestListPolicyVersions :: ListPolicyVersions -> TestTree
requestListPolicyVersions =
  req
    "ListPolicyVersions"
    "fixture/ListPolicyVersions.yaml"

requestUpdateRoleDescription :: UpdateRoleDescription -> TestTree
requestUpdateRoleDescription =
  req
    "UpdateRoleDescription"
    "fixture/UpdateRoleDescription.yaml"

requestListSAMLProviders :: ListSAMLProviders -> TestTree
requestListSAMLProviders =
  req
    "ListSAMLProviders"
    "fixture/ListSAMLProviders.yaml"

requestGetServiceLastAccessedDetails :: GetServiceLastAccessedDetails -> TestTree
requestGetServiceLastAccessedDetails =
  req
    "GetServiceLastAccessedDetails"
    "fixture/GetServiceLastAccessedDetails.yaml"

requestGetServerCertificate :: GetServerCertificate -> TestTree
requestGetServerCertificate =
  req
    "GetServerCertificate"
    "fixture/GetServerCertificate.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup =
  req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

requestUpdateGroup :: UpdateGroup -> TestTree
requestUpdateGroup =
  req
    "UpdateGroup"
    "fixture/UpdateGroup.yaml"

requestListGroups :: ListGroups -> TestTree
requestListGroups =
  req
    "ListGroups"
    "fixture/ListGroups.yaml"

requestGenerateCredentialReport :: GenerateCredentialReport -> TestTree
requestGenerateCredentialReport =
  req
    "GenerateCredentialReport"
    "fixture/GenerateCredentialReport.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy =
  req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

requestListInstanceProfileTags :: ListInstanceProfileTags -> TestTree
requestListInstanceProfileTags =
  req
    "ListInstanceProfileTags"
    "fixture/ListInstanceProfileTags.yaml"

requestUpdateLoginProfile :: UpdateLoginProfile -> TestTree
requestUpdateLoginProfile =
  req
    "UpdateLoginProfile"
    "fixture/UpdateLoginProfile.yaml"

requestDeleteLoginProfile :: DeleteLoginProfile -> TestTree
requestDeleteLoginProfile =
  req
    "DeleteLoginProfile"
    "fixture/DeleteLoginProfile.yaml"

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

requestUntagPolicy :: UntagPolicy -> TestTree
requestUntagPolicy =
  req
    "UntagPolicy"
    "fixture/UntagPolicy.yaml"

requestDeleteServerCertificate :: DeleteServerCertificate -> TestTree
requestDeleteServerCertificate =
  req
    "DeleteServerCertificate"
    "fixture/DeleteServerCertificate.yaml"

requestUpdateServerCertificate :: UpdateServerCertificate -> TestTree
requestUpdateServerCertificate =
  req
    "UpdateServerCertificate"
    "fixture/UpdateServerCertificate.yaml"

requestListAttachedGroupPolicies :: ListAttachedGroupPolicies -> TestTree
requestListAttachedGroupPolicies =
  req
    "ListAttachedGroupPolicies"
    "fixture/ListAttachedGroupPolicies.yaml"

requestListMFADeviceTags :: ListMFADeviceTags -> TestTree
requestListMFADeviceTags =
  req
    "ListMFADeviceTags"
    "fixture/ListMFADeviceTags.yaml"

-- Responses

responseGetContextKeysForPrincipalPolicy :: GetContextKeysForPolicyResponse -> TestTree
responseGetContextKeysForPrincipalPolicy =
  res
    "GetContextKeysForPrincipalPolicyResponse"
    "fixture/GetContextKeysForPrincipalPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetContextKeysForPrincipalPolicy)

responseListPolicies :: ListPoliciesResponse -> TestTree
responseListPolicies =
  res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListPolicies)

responseCreatePolicy :: CreatePolicyResponse -> TestTree
responseCreatePolicy =
  res
    "CreatePolicyResponse"
    "fixture/CreatePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePolicy)

responseListInstanceProfilesForRole :: ListInstanceProfilesForRoleResponse -> TestTree
responseListInstanceProfilesForRole =
  res
    "ListInstanceProfilesForRoleResponse"
    "fixture/ListInstanceProfilesForRoleResponse.proto"
    defaultService
    (Proxy :: Proxy ListInstanceProfilesForRole)

responseAttachGroupPolicy :: AttachGroupPolicyResponse -> TestTree
responseAttachGroupPolicy =
  res
    "AttachGroupPolicyResponse"
    "fixture/AttachGroupPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy AttachGroupPolicy)

responseCreateAccessKey :: CreateAccessKeyResponse -> TestTree
responseCreateAccessKey =
  res
    "CreateAccessKeyResponse"
    "fixture/CreateAccessKeyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAccessKey)

responseListRoleTags :: ListRoleTagsResponse -> TestTree
responseListRoleTags =
  res
    "ListRoleTagsResponse"
    "fixture/ListRoleTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListRoleTags)

responseListSSHPublicKeys :: ListSSHPublicKeysResponse -> TestTree
responseListSSHPublicKeys =
  res
    "ListSSHPublicKeysResponse"
    "fixture/ListSSHPublicKeysResponse.proto"
    defaultService
    (Proxy :: Proxy ListSSHPublicKeys)

responseUntagOpenIDConnectProvider :: UntagOpenIDConnectProviderResponse -> TestTree
responseUntagOpenIDConnectProvider =
  res
    "UntagOpenIDConnectProviderResponse"
    "fixture/UntagOpenIDConnectProviderResponse.proto"
    defaultService
    (Proxy :: Proxy UntagOpenIDConnectProvider)

responseListOpenIDConnectProviders :: ListOpenIDConnectProvidersResponse -> TestTree
responseListOpenIDConnectProviders =
  res
    "ListOpenIDConnectProvidersResponse"
    "fixture/ListOpenIDConnectProvidersResponse.proto"
    defaultService
    (Proxy :: Proxy ListOpenIDConnectProviders)

responseCreateVirtualMFADevice :: CreateVirtualMFADeviceResponse -> TestTree
responseCreateVirtualMFADevice =
  res
    "CreateVirtualMFADeviceResponse"
    "fixture/CreateVirtualMFADeviceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVirtualMFADevice)

responseDeleteAccountPasswordPolicy :: DeleteAccountPasswordPolicyResponse -> TestTree
responseDeleteAccountPasswordPolicy =
  res
    "DeleteAccountPasswordPolicyResponse"
    "fixture/DeleteAccountPasswordPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAccountPasswordPolicy)

responseUpdateAccountPasswordPolicy :: UpdateAccountPasswordPolicyResponse -> TestTree
responseUpdateAccountPasswordPolicy =
  res
    "UpdateAccountPasswordPolicyResponse"
    "fixture/UpdateAccountPasswordPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAccountPasswordPolicy)

responseAttachRolePolicy :: AttachRolePolicyResponse -> TestTree
responseAttachRolePolicy =
  res
    "AttachRolePolicyResponse"
    "fixture/AttachRolePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy AttachRolePolicy)

responseUpdateSSHPublicKey :: UpdateSSHPublicKeyResponse -> TestTree
responseUpdateSSHPublicKey =
  res
    "UpdateSSHPublicKeyResponse"
    "fixture/UpdateSSHPublicKeyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSSHPublicKey)

responseDeleteSSHPublicKey :: DeleteSSHPublicKeyResponse -> TestTree
responseDeleteSSHPublicKey =
  res
    "DeleteSSHPublicKeyResponse"
    "fixture/DeleteSSHPublicKeyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSSHPublicKey)

responseGetUserPolicy :: GetUserPolicyResponse -> TestTree
responseGetUserPolicy =
  res
    "GetUserPolicyResponse"
    "fixture/GetUserPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetUserPolicy)

responseUpdateServiceSpecificCredential :: UpdateServiceSpecificCredentialResponse -> TestTree
responseUpdateServiceSpecificCredential =
  res
    "UpdateServiceSpecificCredentialResponse"
    "fixture/UpdateServiceSpecificCredentialResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateServiceSpecificCredential)

responseDeleteServiceSpecificCredential :: DeleteServiceSpecificCredentialResponse -> TestTree
responseDeleteServiceSpecificCredential =
  res
    "DeleteServiceSpecificCredentialResponse"
    "fixture/DeleteServiceSpecificCredentialResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteServiceSpecificCredential)

responseListAttachedRolePolicies :: ListAttachedRolePoliciesResponse -> TestTree
responseListAttachedRolePolicies =
  res
    "ListAttachedRolePoliciesResponse"
    "fixture/ListAttachedRolePoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAttachedRolePolicies)

responseGetRole :: GetRoleResponse -> TestTree
responseGetRole =
  res
    "GetRoleResponse"
    "fixture/GetRoleResponse.proto"
    defaultService
    (Proxy :: Proxy GetRole)

responseDeactivateMFADevice :: DeactivateMFADeviceResponse -> TestTree
responseDeactivateMFADevice =
  res
    "DeactivateMFADeviceResponse"
    "fixture/DeactivateMFADeviceResponse.proto"
    defaultService
    (Proxy :: Proxy DeactivateMFADevice)

responseCreateOpenIDConnectProvider :: CreateOpenIDConnectProviderResponse -> TestTree
responseCreateOpenIDConnectProvider =
  res
    "CreateOpenIDConnectProviderResponse"
    "fixture/CreateOpenIDConnectProviderResponse.proto"
    defaultService
    (Proxy :: Proxy CreateOpenIDConnectProvider)

responseDeleteVirtualMFADevice :: DeleteVirtualMFADeviceResponse -> TestTree
responseDeleteVirtualMFADevice =
  res
    "DeleteVirtualMFADeviceResponse"
    "fixture/DeleteVirtualMFADeviceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVirtualMFADevice)

responseListRoles :: ListRolesResponse -> TestTree
responseListRoles =
  res
    "ListRolesResponse"
    "fixture/ListRolesResponse.proto"
    defaultService
    (Proxy :: Proxy ListRoles)

responseListUserPolicies :: ListUserPoliciesResponse -> TestTree
responseListUserPolicies =
  res
    "ListUserPoliciesResponse"
    "fixture/ListUserPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListUserPolicies)

responseListOpenIDConnectProviderTags :: ListOpenIDConnectProviderTagsResponse -> TestTree
responseListOpenIDConnectProviderTags =
  res
    "ListOpenIDConnectProviderTagsResponse"
    "fixture/ListOpenIDConnectProviderTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListOpenIDConnectProviderTags)

responsePutRolePermissionsBoundary :: PutRolePermissionsBoundaryResponse -> TestTree
responsePutRolePermissionsBoundary =
  res
    "PutRolePermissionsBoundaryResponse"
    "fixture/PutRolePermissionsBoundaryResponse.proto"
    defaultService
    (Proxy :: Proxy PutRolePermissionsBoundary)

responseUploadSSHPublicKey :: UploadSSHPublicKeyResponse -> TestTree
responseUploadSSHPublicKey =
  res
    "UploadSSHPublicKeyResponse"
    "fixture/UploadSSHPublicKeyResponse.proto"
    defaultService
    (Proxy :: Proxy UploadSSHPublicKey)

responseDeleteRolePermissionsBoundary :: DeleteRolePermissionsBoundaryResponse -> TestTree
responseDeleteRolePermissionsBoundary =
  res
    "DeleteRolePermissionsBoundaryResponse"
    "fixture/DeleteRolePermissionsBoundaryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRolePermissionsBoundary)

responseSimulateCustomPolicy :: SimulatePolicyResponse -> TestTree
responseSimulateCustomPolicy =
  res
    "SimulateCustomPolicyResponse"
    "fixture/SimulateCustomPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy SimulateCustomPolicy)

responseUpdateRole :: UpdateRoleResponse -> TestTree
responseUpdateRole =
  res
    "UpdateRoleResponse"
    "fixture/UpdateRoleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRole)

responseDeleteRole :: DeleteRoleResponse -> TestTree
responseDeleteRole =
  res
    "DeleteRoleResponse"
    "fixture/DeleteRoleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRole)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    defaultService
    (Proxy :: Proxy ListUsers)

responseUpdateOpenIDConnectProviderThumbprint :: UpdateOpenIDConnectProviderThumbprintResponse -> TestTree
responseUpdateOpenIDConnectProviderThumbprint =
  res
    "UpdateOpenIDConnectProviderThumbprintResponse"
    "fixture/UpdateOpenIDConnectProviderThumbprintResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateOpenIDConnectProviderThumbprint)

responsePutUserPolicy :: PutUserPolicyResponse -> TestTree
responsePutUserPolicy =
  res
    "PutUserPolicyResponse"
    "fixture/PutUserPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutUserPolicy)

responseTagMFADevice :: TagMFADeviceResponse -> TestTree
responseTagMFADevice =
  res
    "TagMFADeviceResponse"
    "fixture/TagMFADeviceResponse.proto"
    defaultService
    (Proxy :: Proxy TagMFADevice)

responseGetSSHPublicKey :: GetSSHPublicKeyResponse -> TestTree
responseGetSSHPublicKey =
  res
    "GetSSHPublicKeyResponse"
    "fixture/GetSSHPublicKeyResponse.proto"
    defaultService
    (Proxy :: Proxy GetSSHPublicKey)

responseUntagUser :: UntagUserResponse -> TestTree
responseUntagUser =
  res
    "UntagUserResponse"
    "fixture/UntagUserResponse.proto"
    defaultService
    (Proxy :: Proxy UntagUser)

responseDetachGroupPolicy :: DetachGroupPolicyResponse -> TestTree
responseDetachGroupPolicy =
  res
    "DetachGroupPolicyResponse"
    "fixture/DetachGroupPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DetachGroupPolicy)

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

responseTagSAMLProvider :: TagSAMLProviderResponse -> TestTree
responseTagSAMLProvider =
  res
    "TagSAMLProviderResponse"
    "fixture/TagSAMLProviderResponse.proto"
    defaultService
    (Proxy :: Proxy TagSAMLProvider)

responseDeleteUserPermissionsBoundary :: DeleteUserPermissionsBoundaryResponse -> TestTree
responseDeleteUserPermissionsBoundary =
  res
    "DeleteUserPermissionsBoundaryResponse"
    "fixture/DeleteUserPermissionsBoundaryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUserPermissionsBoundary)

responseCreateRole :: CreateRoleResponse -> TestTree
responseCreateRole =
  res
    "CreateRoleResponse"
    "fixture/CreateRoleResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRole)

responseResetServiceSpecificCredential :: ResetServiceSpecificCredentialResponse -> TestTree
responseResetServiceSpecificCredential =
  res
    "ResetServiceSpecificCredentialResponse"
    "fixture/ResetServiceSpecificCredentialResponse.proto"
    defaultService
    (Proxy :: Proxy ResetServiceSpecificCredential)

responseUntagSAMLProvider :: UntagSAMLProviderResponse -> TestTree
responseUntagSAMLProvider =
  res
    "UntagSAMLProviderResponse"
    "fixture/UntagSAMLProviderResponse.proto"
    defaultService
    (Proxy :: Proxy UntagSAMLProvider)

responseGetCredentialReport :: GetCredentialReportResponse -> TestTree
responseGetCredentialReport =
  res
    "GetCredentialReportResponse"
    "fixture/GetCredentialReportResponse.proto"
    defaultService
    (Proxy :: Proxy GetCredentialReport)

responseListServerCertificateTags :: ListServerCertificateTagsResponse -> TestTree
responseListServerCertificateTags =
  res
    "ListServerCertificateTagsResponse"
    "fixture/ListServerCertificateTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListServerCertificateTags)

responseGetAccountSummary :: GetAccountSummaryResponse -> TestTree
responseGetAccountSummary =
  res
    "GetAccountSummaryResponse"
    "fixture/GetAccountSummaryResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccountSummary)

responseGenerateServiceLastAccessedDetails :: GenerateServiceLastAccessedDetailsResponse -> TestTree
responseGenerateServiceLastAccessedDetails =
  res
    "GenerateServiceLastAccessedDetailsResponse"
    "fixture/GenerateServiceLastAccessedDetailsResponse.proto"
    defaultService
    (Proxy :: Proxy GenerateServiceLastAccessedDetails)

responseListPolicyTags :: ListPolicyTagsResponse -> TestTree
responseListPolicyTags =
  res
    "ListPolicyTagsResponse"
    "fixture/ListPolicyTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPolicyTags)

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

responseTagUser :: TagUserResponse -> TestTree
responseTagUser =
  res
    "TagUserResponse"
    "fixture/TagUserResponse.proto"
    defaultService
    (Proxy :: Proxy TagUser)

responseDeleteInstanceProfile :: DeleteInstanceProfileResponse -> TestTree
responseDeleteInstanceProfile =
  res
    "DeleteInstanceProfileResponse"
    "fixture/DeleteInstanceProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteInstanceProfile)

responseDetachRolePolicy :: DetachRolePolicyResponse -> TestTree
responseDetachRolePolicy =
  res
    "DetachRolePolicyResponse"
    "fixture/DetachRolePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DetachRolePolicy)

responseRemoveRoleFromInstanceProfile :: RemoveRoleFromInstanceProfileResponse -> TestTree
responseRemoveRoleFromInstanceProfile =
  res
    "RemoveRoleFromInstanceProfileResponse"
    "fixture/RemoveRoleFromInstanceProfileResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveRoleFromInstanceProfile)

responseCreatePolicyVersion :: CreatePolicyVersionResponse -> TestTree
responseCreatePolicyVersion =
  res
    "CreatePolicyVersionResponse"
    "fixture/CreatePolicyVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePolicyVersion)

responseCreateInstanceProfile :: CreateInstanceProfileResponse -> TestTree
responseCreateInstanceProfile =
  res
    "CreateInstanceProfileResponse"
    "fixture/CreateInstanceProfileResponse.proto"
    defaultService
    (Proxy :: Proxy CreateInstanceProfile)

responseCreateSAMLProvider :: CreateSAMLProviderResponse -> TestTree
responseCreateSAMLProvider =
  res
    "CreateSAMLProviderResponse"
    "fixture/CreateSAMLProviderResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSAMLProvider)

responseGetAccountAuthorizationDetails :: GetAccountAuthorizationDetailsResponse -> TestTree
responseGetAccountAuthorizationDetails =
  res
    "GetAccountAuthorizationDetailsResponse"
    "fixture/GetAccountAuthorizationDetailsResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccountAuthorizationDetails)

responseGetServiceLinkedRoleDeletionStatus :: GetServiceLinkedRoleDeletionStatusResponse -> TestTree
responseGetServiceLinkedRoleDeletionStatus =
  res
    "GetServiceLinkedRoleDeletionStatusResponse"
    "fixture/GetServiceLinkedRoleDeletionStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetServiceLinkedRoleDeletionStatus)

responseDeleteAccountAlias :: DeleteAccountAliasResponse -> TestTree
responseDeleteAccountAlias =
  res
    "DeleteAccountAliasResponse"
    "fixture/DeleteAccountAliasResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAccountAlias)

responseDetachUserPolicy :: DetachUserPolicyResponse -> TestTree
responseDetachUserPolicy =
  res
    "DetachUserPolicyResponse"
    "fixture/DetachUserPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DetachUserPolicy)

responseRemoveUserFromGroup :: RemoveUserFromGroupResponse -> TestTree
responseRemoveUserFromGroup =
  res
    "RemoveUserFromGroupResponse"
    "fixture/RemoveUserFromGroupResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveUserFromGroup)

responseDeleteGroupPolicy :: DeleteGroupPolicyResponse -> TestTree
responseDeleteGroupPolicy =
  res
    "DeleteGroupPolicyResponse"
    "fixture/DeleteGroupPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGroupPolicy)

responseTagRole :: TagRoleResponse -> TestTree
responseTagRole =
  res
    "TagRoleResponse"
    "fixture/TagRoleResponse.proto"
    defaultService
    (Proxy :: Proxy TagRole)

responsePutGroupPolicy :: PutGroupPolicyResponse -> TestTree
responsePutGroupPolicy =
  res
    "PutGroupPolicyResponse"
    "fixture/PutGroupPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutGroupPolicy)

responseGetLoginProfile :: GetLoginProfileResponse -> TestTree
responseGetLoginProfile =
  res
    "GetLoginProfileResponse"
    "fixture/GetLoginProfileResponse.proto"
    defaultService
    (Proxy :: Proxy GetLoginProfile)

responseGetGroupPolicy :: GetGroupPolicyResponse -> TestTree
responseGetGroupPolicy =
  res
    "GetGroupPolicyResponse"
    "fixture/GetGroupPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetGroupPolicy)

responseGenerateOrganizationsAccessReport :: GenerateOrganizationsAccessReportResponse -> TestTree
responseGenerateOrganizationsAccessReport =
  res
    "GenerateOrganizationsAccessReportResponse"
    "fixture/GenerateOrganizationsAccessReportResponse.proto"
    defaultService
    (Proxy :: Proxy GenerateOrganizationsAccessReport)

responseChangePassword :: ChangePasswordResponse -> TestTree
responseChangePassword =
  res
    "ChangePasswordResponse"
    "fixture/ChangePasswordResponse.proto"
    defaultService
    (Proxy :: Proxy ChangePassword)

responseListServerCertificates :: ListServerCertificatesResponse -> TestTree
responseListServerCertificates =
  res
    "ListServerCertificatesResponse"
    "fixture/ListServerCertificatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListServerCertificates)

responseDeleteServiceLinkedRole :: DeleteServiceLinkedRoleResponse -> TestTree
responseDeleteServiceLinkedRole =
  res
    "DeleteServiceLinkedRoleResponse"
    "fixture/DeleteServiceLinkedRoleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteServiceLinkedRole)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePolicy)

responseUpdateAssumeRolePolicy :: UpdateAssumeRolePolicyResponse -> TestTree
responseUpdateAssumeRolePolicy =
  res
    "UpdateAssumeRolePolicyResponse"
    "fixture/UpdateAssumeRolePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAssumeRolePolicy)

responseGetServiceLastAccessedDetailsWithEntities :: GetServiceLastAccessedDetailsWithEntitiesResponse -> TestTree
responseGetServiceLastAccessedDetailsWithEntities =
  res
    "GetServiceLastAccessedDetailsWithEntitiesResponse"
    "fixture/GetServiceLastAccessedDetailsWithEntitiesResponse.proto"
    defaultService
    (Proxy :: Proxy GetServiceLastAccessedDetailsWithEntities)

responseUntagServerCertificate :: UntagServerCertificateResponse -> TestTree
responseUntagServerCertificate =
  res
    "UntagServerCertificateResponse"
    "fixture/UntagServerCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy UntagServerCertificate)

responseGetInstanceProfile :: GetInstanceProfileResponse -> TestTree
responseGetInstanceProfile =
  res
    "GetInstanceProfileResponse"
    "fixture/GetInstanceProfileResponse.proto"
    defaultService
    (Proxy :: Proxy GetInstanceProfile)

responseCreateLoginProfile :: CreateLoginProfileResponse -> TestTree
responseCreateLoginProfile =
  res
    "CreateLoginProfileResponse"
    "fixture/CreateLoginProfileResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLoginProfile)

responseGetSAMLProvider :: GetSAMLProviderResponse -> TestTree
responseGetSAMLProvider =
  res
    "GetSAMLProviderResponse"
    "fixture/GetSAMLProviderResponse.proto"
    defaultService
    (Proxy :: Proxy GetSAMLProvider)

responseAddRoleToInstanceProfile :: AddRoleToInstanceProfileResponse -> TestTree
responseAddRoleToInstanceProfile =
  res
    "AddRoleToInstanceProfileResponse"
    "fixture/AddRoleToInstanceProfileResponse.proto"
    defaultService
    (Proxy :: Proxy AddRoleToInstanceProfile)

responseListGroupsForUser :: ListGroupsForUserResponse -> TestTree
responseListGroupsForUser =
  res
    "ListGroupsForUserResponse"
    "fixture/ListGroupsForUserResponse.proto"
    defaultService
    (Proxy :: Proxy ListGroupsForUser)

responseListEntitiesForPolicy :: ListEntitiesForPolicyResponse -> TestTree
responseListEntitiesForPolicy =
  res
    "ListEntitiesForPolicyResponse"
    "fixture/ListEntitiesForPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy ListEntitiesForPolicy)

responseAddUserToGroup :: AddUserToGroupResponse -> TestTree
responseAddUserToGroup =
  res
    "AddUserToGroupResponse"
    "fixture/AddUserToGroupResponse.proto"
    defaultService
    (Proxy :: Proxy AddUserToGroup)

responseTagOpenIDConnectProvider :: TagOpenIDConnectProviderResponse -> TestTree
responseTagOpenIDConnectProvider =
  res
    "TagOpenIDConnectProviderResponse"
    "fixture/TagOpenIDConnectProviderResponse.proto"
    defaultService
    (Proxy :: Proxy TagOpenIDConnectProvider)

responseSimulatePrincipalPolicy :: SimulatePolicyResponse -> TestTree
responseSimulatePrincipalPolicy =
  res
    "SimulatePrincipalPolicyResponse"
    "fixture/SimulatePrincipalPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy SimulatePrincipalPolicy)

responseGetOrganizationsAccessReport :: GetOrganizationsAccessReportResponse -> TestTree
responseGetOrganizationsAccessReport =
  res
    "GetOrganizationsAccessReportResponse"
    "fixture/GetOrganizationsAccessReportResponse.proto"
    defaultService
    (Proxy :: Proxy GetOrganizationsAccessReport)

responseGetPolicyVersion :: GetPolicyVersionResponse -> TestTree
responseGetPolicyVersion =
  res
    "GetPolicyVersionResponse"
    "fixture/GetPolicyVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetPolicyVersion)

responseCreateServiceLinkedRole :: CreateServiceLinkedRoleResponse -> TestTree
responseCreateServiceLinkedRole =
  res
    "CreateServiceLinkedRoleResponse"
    "fixture/CreateServiceLinkedRoleResponse.proto"
    defaultService
    (Proxy :: Proxy CreateServiceLinkedRole)

responseListServiceSpecificCredentials :: ListServiceSpecificCredentialsResponse -> TestTree
responseListServiceSpecificCredentials =
  res
    "ListServiceSpecificCredentialsResponse"
    "fixture/ListServiceSpecificCredentialsResponse.proto"
    defaultService
    (Proxy :: Proxy ListServiceSpecificCredentials)

responseDeleteOpenIDConnectProvider :: DeleteOpenIDConnectProviderResponse -> TestTree
responseDeleteOpenIDConnectProvider =
  res
    "DeleteOpenIDConnectProviderResponse"
    "fixture/DeleteOpenIDConnectProviderResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteOpenIDConnectProvider)

responseGetUser :: GetUserResponse -> TestTree
responseGetUser =
  res
    "GetUserResponse"
    "fixture/GetUserResponse.proto"
    defaultService
    (Proxy :: Proxy GetUser)

responseListSigningCertificates :: ListSigningCertificatesResponse -> TestTree
responseListSigningCertificates =
  res
    "ListSigningCertificatesResponse"
    "fixture/ListSigningCertificatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListSigningCertificates)

responseDeleteSigningCertificate :: DeleteSigningCertificateResponse -> TestTree
responseDeleteSigningCertificate =
  res
    "DeleteSigningCertificateResponse"
    "fixture/DeleteSigningCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSigningCertificate)

responseUpdateSigningCertificate :: UpdateSigningCertificateResponse -> TestTree
responseUpdateSigningCertificate =
  res
    "UpdateSigningCertificateResponse"
    "fixture/UpdateSigningCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSigningCertificate)

responseListAttachedUserPolicies :: ListAttachedUserPoliciesResponse -> TestTree
responseListAttachedUserPolicies =
  res
    "ListAttachedUserPoliciesResponse"
    "fixture/ListAttachedUserPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAttachedUserPolicies)

responseRemoveClientIDFromOpenIDConnectProvider :: RemoveClientIDFromOpenIDConnectProviderResponse -> TestTree
responseRemoveClientIDFromOpenIDConnectProvider =
  res
    "RemoveClientIDFromOpenIDConnectProviderResponse"
    "fixture/RemoveClientIDFromOpenIDConnectProviderResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveClientIDFromOpenIDConnectProvider)

responseAttachUserPolicy :: AttachUserPolicyResponse -> TestTree
responseAttachUserPolicy =
  res
    "AttachUserPolicyResponse"
    "fixture/AttachUserPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy AttachUserPolicy)

responseTagPolicy :: TagPolicyResponse -> TestTree
responseTagPolicy =
  res
    "TagPolicyResponse"
    "fixture/TagPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy TagPolicy)

responseCreateServiceSpecificCredential :: CreateServiceSpecificCredentialResponse -> TestTree
responseCreateServiceSpecificCredential =
  res
    "CreateServiceSpecificCredentialResponse"
    "fixture/CreateServiceSpecificCredentialResponse.proto"
    defaultService
    (Proxy :: Proxy CreateServiceSpecificCredential)

responseListVirtualMFADevices :: ListVirtualMFADevicesResponse -> TestTree
responseListVirtualMFADevices =
  res
    "ListVirtualMFADevicesResponse"
    "fixture/ListVirtualMFADevicesResponse.proto"
    defaultService
    (Proxy :: Proxy ListVirtualMFADevices)

responseResyncMFADevice :: ResyncMFADeviceResponse -> TestTree
responseResyncMFADevice =
  res
    "ResyncMFADeviceResponse"
    "fixture/ResyncMFADeviceResponse.proto"
    defaultService
    (Proxy :: Proxy ResyncMFADevice)

responseTagServerCertificate :: TagServerCertificateResponse -> TestTree
responseTagServerCertificate =
  res
    "TagServerCertificateResponse"
    "fixture/TagServerCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy TagServerCertificate)

responseDeleteAccessKey :: DeleteAccessKeyResponse -> TestTree
responseDeleteAccessKey =
  res
    "DeleteAccessKeyResponse"
    "fixture/DeleteAccessKeyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAccessKey)

responseUpdateAccessKey :: UpdateAccessKeyResponse -> TestTree
responseUpdateAccessKey =
  res
    "UpdateAccessKeyResponse"
    "fixture/UpdateAccessKeyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAccessKey)

responseListUserTags :: ListUserTagsResponse -> TestTree
responseListUserTags =
  res
    "ListUserTagsResponse"
    "fixture/ListUserTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListUserTags)

responseListAccessKeys :: ListAccessKeysResponse -> TestTree
responseListAccessKeys =
  res
    "ListAccessKeysResponse"
    "fixture/ListAccessKeysResponse.proto"
    defaultService
    (Proxy :: Proxy ListAccessKeys)

responseGetRolePolicy :: GetRolePolicyResponse -> TestTree
responseGetRolePolicy =
  res
    "GetRolePolicyResponse"
    "fixture/GetRolePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetRolePolicy)

responseSetSecurityTokenServicePreferences :: SetSecurityTokenServicePreferencesResponse -> TestTree
responseSetSecurityTokenServicePreferences =
  res
    "SetSecurityTokenServicePreferencesResponse"
    "fixture/SetSecurityTokenServicePreferencesResponse.proto"
    defaultService
    (Proxy :: Proxy SetSecurityTokenServicePreferences)

responseUntagRole :: UntagRoleResponse -> TestTree
responseUntagRole =
  res
    "UntagRoleResponse"
    "fixture/UntagRoleResponse.proto"
    defaultService
    (Proxy :: Proxy UntagRole)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUser)

responsePutRolePolicy :: PutRolePolicyResponse -> TestTree
responsePutRolePolicy =
  res
    "PutRolePolicyResponse"
    "fixture/PutRolePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutRolePolicy)

responseGetContextKeysForCustomPolicy :: GetContextKeysForPolicyResponse -> TestTree
responseGetContextKeysForCustomPolicy =
  res
    "GetContextKeysForCustomPolicyResponse"
    "fixture/GetContextKeysForCustomPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetContextKeysForCustomPolicy)

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

responseGetAccountPasswordPolicy :: GetAccountPasswordPolicyResponse -> TestTree
responseGetAccountPasswordPolicy =
  res
    "GetAccountPasswordPolicyResponse"
    "fixture/GetAccountPasswordPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccountPasswordPolicy)

responseGetAccessKeyLastUsed :: GetAccessKeyLastUsedResponse -> TestTree
responseGetAccessKeyLastUsed =
  res
    "GetAccessKeyLastUsedResponse"
    "fixture/GetAccessKeyLastUsedResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccessKeyLastUsed)

responseUpdateUser :: UpdateUserResponse -> TestTree
responseUpdateUser =
  res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUser)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUser)

responseAddClientIDToOpenIDConnectProvider :: AddClientIDToOpenIDConnectProviderResponse -> TestTree
responseAddClientIDToOpenIDConnectProvider =
  res
    "AddClientIDToOpenIDConnectProviderResponse"
    "fixture/AddClientIDToOpenIDConnectProviderResponse.proto"
    defaultService
    (Proxy :: Proxy AddClientIDToOpenIDConnectProvider)

responseListRolePolicies :: ListRolePoliciesResponse -> TestTree
responseListRolePolicies =
  res
    "ListRolePoliciesResponse"
    "fixture/ListRolePoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListRolePolicies)

responseCreateAccountAlias :: CreateAccountAliasResponse -> TestTree
responseCreateAccountAlias =
  res
    "CreateAccountAliasResponse"
    "fixture/CreateAccountAliasResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAccountAlias)

responseListPoliciesGrantingServiceAccess :: ListPoliciesGrantingServiceAccessResponse -> TestTree
responseListPoliciesGrantingServiceAccess =
  res
    "ListPoliciesGrantingServiceAccessResponse"
    "fixture/ListPoliciesGrantingServiceAccessResponse.proto"
    defaultService
    (Proxy :: Proxy ListPoliciesGrantingServiceAccess)

responseListInstanceProfiles :: ListInstanceProfilesResponse -> TestTree
responseListInstanceProfiles =
  res
    "ListInstanceProfilesResponse"
    "fixture/ListInstanceProfilesResponse.proto"
    defaultService
    (Proxy :: Proxy ListInstanceProfiles)

responseEnableMFADevice :: EnableMFADeviceResponse -> TestTree
responseEnableMFADevice =
  res
    "EnableMFADeviceResponse"
    "fixture/EnableMFADeviceResponse.proto"
    defaultService
    (Proxy :: Proxy EnableMFADevice)

responseListAccountAliases :: ListAccountAliasesResponse -> TestTree
responseListAccountAliases =
  res
    "ListAccountAliasesResponse"
    "fixture/ListAccountAliasesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAccountAliases)

responseDeleteSAMLProvider :: DeleteSAMLProviderResponse -> TestTree
responseDeleteSAMLProvider =
  res
    "DeleteSAMLProviderResponse"
    "fixture/DeleteSAMLProviderResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSAMLProvider)

responseUpdateSAMLProvider :: UpdateSAMLProviderResponse -> TestTree
responseUpdateSAMLProvider =
  res
    "UpdateSAMLProviderResponse"
    "fixture/UpdateSAMLProviderResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSAMLProvider)

responseUntagMFADevice :: UntagMFADeviceResponse -> TestTree
responseUntagMFADevice =
  res
    "UntagMFADeviceResponse"
    "fixture/UntagMFADeviceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagMFADevice)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup =
  res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateGroup)

responseListMFADevices :: ListMFADevicesResponse -> TestTree
responseListMFADevices =
  res
    "ListMFADevicesResponse"
    "fixture/ListMFADevicesResponse.proto"
    defaultService
    (Proxy :: Proxy ListMFADevices)

responseUntagInstanceProfile :: UntagInstanceProfileResponse -> TestTree
responseUntagInstanceProfile =
  res
    "UntagInstanceProfileResponse"
    "fixture/UntagInstanceProfileResponse.proto"
    defaultService
    (Proxy :: Proxy UntagInstanceProfile)

responseUploadServerCertificate :: UploadServerCertificateResponse -> TestTree
responseUploadServerCertificate =
  res
    "UploadServerCertificateResponse"
    "fixture/UploadServerCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy UploadServerCertificate)

responseSetDefaultPolicyVersion :: SetDefaultPolicyVersionResponse -> TestTree
responseSetDefaultPolicyVersion =
  res
    "SetDefaultPolicyVersionResponse"
    "fixture/SetDefaultPolicyVersionResponse.proto"
    defaultService
    (Proxy :: Proxy SetDefaultPolicyVersion)

responseListPolicyVersions :: ListPolicyVersionsResponse -> TestTree
responseListPolicyVersions =
  res
    "ListPolicyVersionsResponse"
    "fixture/ListPolicyVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPolicyVersions)

responseUpdateRoleDescription :: UpdateRoleDescriptionResponse -> TestTree
responseUpdateRoleDescription =
  res
    "UpdateRoleDescriptionResponse"
    "fixture/UpdateRoleDescriptionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRoleDescription)

responseListSAMLProviders :: ListSAMLProvidersResponse -> TestTree
responseListSAMLProviders =
  res
    "ListSAMLProvidersResponse"
    "fixture/ListSAMLProvidersResponse.proto"
    defaultService
    (Proxy :: Proxy ListSAMLProviders)

responseGetServiceLastAccessedDetails :: GetServiceLastAccessedDetailsResponse -> TestTree
responseGetServiceLastAccessedDetails =
  res
    "GetServiceLastAccessedDetailsResponse"
    "fixture/GetServiceLastAccessedDetailsResponse.proto"
    defaultService
    (Proxy :: Proxy GetServiceLastAccessedDetails)

responseGetServerCertificate :: GetServerCertificateResponse -> TestTree
responseGetServerCertificate =
  res
    "GetServerCertificateResponse"
    "fixture/GetServerCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy GetServerCertificate)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGroup)

responseUpdateGroup :: UpdateGroupResponse -> TestTree
responseUpdateGroup =
  res
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGroup)

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups =
  res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListGroups)

responseGenerateCredentialReport :: GenerateCredentialReportResponse -> TestTree
responseGenerateCredentialReport =
  res
    "GenerateCredentialReportResponse"
    "fixture/GenerateCredentialReportResponse.proto"
    defaultService
    (Proxy :: Proxy GenerateCredentialReport)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetPolicy)

responseListInstanceProfileTags :: ListInstanceProfileTagsResponse -> TestTree
responseListInstanceProfileTags =
  res
    "ListInstanceProfileTagsResponse"
    "fixture/ListInstanceProfileTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListInstanceProfileTags)

responseUpdateLoginProfile :: UpdateLoginProfileResponse -> TestTree
responseUpdateLoginProfile =
  res
    "UpdateLoginProfileResponse"
    "fixture/UpdateLoginProfileResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateLoginProfile)

responseDeleteLoginProfile :: DeleteLoginProfileResponse -> TestTree
responseDeleteLoginProfile =
  res
    "DeleteLoginProfileResponse"
    "fixture/DeleteLoginProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLoginProfile)

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

responseUntagPolicy :: UntagPolicyResponse -> TestTree
responseUntagPolicy =
  res
    "UntagPolicyResponse"
    "fixture/UntagPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy UntagPolicy)

responseDeleteServerCertificate :: DeleteServerCertificateResponse -> TestTree
responseDeleteServerCertificate =
  res
    "DeleteServerCertificateResponse"
    "fixture/DeleteServerCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteServerCertificate)

responseUpdateServerCertificate :: UpdateServerCertificateResponse -> TestTree
responseUpdateServerCertificate =
  res
    "UpdateServerCertificateResponse"
    "fixture/UpdateServerCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateServerCertificate)

responseListAttachedGroupPolicies :: ListAttachedGroupPoliciesResponse -> TestTree
responseListAttachedGroupPolicies =
  res
    "ListAttachedGroupPoliciesResponse"
    "fixture/ListAttachedGroupPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAttachedGroupPolicies)

responseListMFADeviceTags :: ListMFADeviceTagsResponse -> TestTree
responseListMFADeviceTags =
  res
    "ListMFADeviceTagsResponse"
    "fixture/ListMFADeviceTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListMFADeviceTags)
