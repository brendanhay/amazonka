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
--         [ requestListSigningCertificates $
--             newListSigningCertificates
--
--         , requestListRoleTags $
--             newListRoleTags
--
--         , requestAttachRolePolicy $
--             newAttachRolePolicy
--
--         , requestDeleteOpenIDConnectProvider $
--             newDeleteOpenIDConnectProvider
--
--         , requestGetUser $
--             newGetUser
--
--         , requestCreateVirtualMFADevice $
--             newCreateVirtualMFADevice
--
--         , requestUpdateSSHPublicKey $
--             newUpdateSSHPublicKey
--
--         , requestUntagOpenIDConnectProvider $
--             newUntagOpenIDConnectProvider
--
--         , requestDeleteSSHPublicKey $
--             newDeleteSSHPublicKey
--
--         , requestListOpenIDConnectProviders $
--             newListOpenIDConnectProviders
--
--         , requestListEntitiesForPolicy $
--             newListEntitiesForPolicy
--
--         , requestCreateServiceLinkedRole $
--             newCreateServiceLinkedRole
--
--         , requestGetSAMLProvider $
--             newGetSAMLProvider
--
--         , requestListPolicies $
--             newListPolicies
--
--         , requestCreatePolicy $
--             newCreatePolicy
--
--         , requestListGroupsForUser $
--             newListGroupsForUser
--
--         , requestGetContextKeysForPrincipalPolicy $
--             newGetContextKeysForPrincipalPolicy
--
--         , requestSimulatePrincipalPolicy $
--             newSimulatePrincipalPolicy
--
--         , requestListInstanceProfileTags $
--             newListInstanceProfileTags
--
--         , requestDeletePolicy $
--             newDeletePolicy
--
--         , requestUpdateServerCertificate $
--             newUpdateServerCertificate
--
--         , requestListAttachedGroupPolicies $
--             newListAttachedGroupPolicies
--
--         , requestUpdateAssumeRolePolicy $
--             newUpdateAssumeRolePolicy
--
--         , requestUntagServerCertificate $
--             newUntagServerCertificate
--
--         , requestChangePassword $
--             newChangePassword
--
--         , requestListServerCertificates $
--             newListServerCertificates
--
--         , requestGetGroupPolicy $
--             newGetGroupPolicy
--
--         , requestDeleteServerCertificate $
--             newDeleteServerCertificate
--
--         , requestListMFADeviceTags $
--             newListMFADeviceTags
--
--         , requestUntagPolicy $
--             newUntagPolicy
--
--         , requestGetLoginProfile $
--             newGetLoginProfile
--
--         , requestCreatePolicyVersion $
--             newCreatePolicyVersion
--
--         , requestGetServerCertificate $
--             newGetServerCertificate
--
--         , requestListGroups $
--             newListGroups
--
--         , requestTagRole $
--             newTagRole
--
--         , requestRemoveUserFromGroup $
--             newRemoveUserFromGroup
--
--         , requestDeleteGroupPolicy $
--             newDeleteGroupPolicy
--
--         , requestCreateInstanceProfile $
--             newCreateInstanceProfile
--
--         , requestRemoveRoleFromInstanceProfile $
--             newRemoveRoleFromInstanceProfile
--
--         , requestGenerateCredentialReport $
--             newGenerateCredentialReport
--
--         , requestResetServiceSpecificCredential $
--             newResetServiceSpecificCredential
--
--         , requestDetachRolePolicy $
--             newDetachRolePolicy
--
--         , requestGenerateServiceLastAccessedDetails $
--             newGenerateServiceLastAccessedDetails
--
--         , requestSetDefaultPolicyVersion $
--             newSetDefaultPolicyVersion
--
--         , requestUpdateRoleDescription $
--             newUpdateRoleDescription
--
--         , requestUploadServerCertificate $
--             newUploadServerCertificate
--
--         , requestListPolicyTags $
--             newListPolicyTags
--
--         , requestListSAMLProviders $
--             newListSAMLProviders
--
--         , requestCreateGroup $
--             newCreateGroup
--
--         , requestEnableMFADevice $
--             newEnableMFADevice
--
--         , requestListPoliciesGrantingServiceAccess $
--             newListPoliciesGrantingServiceAccess
--
--         , requestGetOpenIDConnectProvider $
--             newGetOpenIDConnectProvider
--
--         , requestCreateRole $
--             newCreateRole
--
--         , requestDeleteUserPolicy $
--             newDeleteUserPolicy
--
--         , requestTagInstanceProfile $
--             newTagInstanceProfile
--
--         , requestPutUserPermissionsBoundary $
--             newPutUserPermissionsBoundary
--
--         , requestTagMFADevice $
--             newTagMFADevice
--
--         , requestUploadSigningCertificate $
--             newUploadSigningCertificate
--
--         , requestListOpenIDConnectProviderTags $
--             newListOpenIDConnectProviderTags
--
--         , requestListRoles $
--             newListRoles
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestDeleteRolePolicy $
--             newDeleteRolePolicy
--
--         , requestDeleteRolePermissionsBoundary $
--             newDeleteRolePermissionsBoundary
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
--         , requestListAttachedRolePolicies $
--             newListAttachedRolePolicies
--
--         , requestCreateServiceSpecificCredential $
--             newCreateServiceSpecificCredential
--
--         , requestDeleteAccessKey $
--             newDeleteAccessKey
--
--         , requestUpdateAccessKey $
--             newUpdateAccessKey
--
--         , requestGetRolePolicy $
--             newGetRolePolicy
--
--         , requestDeleteVirtualMFADevice $
--             newDeleteVirtualMFADevice
--
--         , requestResyncMFADevice $
--             newResyncMFADevice
--
--         , requestListAttachedUserPolicies $
--             newListAttachedUserPolicies
--
--         , requestListSSHPublicKeys $
--             newListSSHPublicKeys
--
--         , requestUpdateAccountPasswordPolicy $
--             newUpdateAccountPasswordPolicy
--
--         , requestUpdateServiceSpecificCredential $
--             newUpdateServiceSpecificCredential
--
--         , requestUpdateSigningCertificate $
--             newUpdateSigningCertificate
--
--         , requestCreateAccessKey $
--             newCreateAccessKey
--
--         , requestListServiceSpecificCredentials $
--             newListServiceSpecificCredentials
--
--         , requestDeleteSigningCertificate $
--             newDeleteSigningCertificate
--
--         , requestGetUserPolicy $
--             newGetUserPolicy
--
--         , requestDeleteAccountPasswordPolicy $
--             newDeleteAccountPasswordPolicy
--
--         , requestDeleteServiceSpecificCredential $
--             newDeleteServiceSpecificCredential
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
--         , requestListInstanceProfilesForRole $
--             newListInstanceProfilesForRole
--
--         , requestTagOpenIDConnectProvider $
--             newTagOpenIDConnectProvider
--
--         , requestGetOrganizationsAccessReport $
--             newGetOrganizationsAccessReport
--
--         , requestGetInstanceProfile $
--             newGetInstanceProfile
--
--         , requestAddUserToGroup $
--             newAddUserToGroup
--
--         , requestAttachGroupPolicy $
--             newAttachGroupPolicy
--
--         , requestDeleteServiceLinkedRole $
--             newDeleteServiceLinkedRole
--
--         , requestListSAMLProviderTags $
--             newListSAMLProviderTags
--
--         , requestGetServiceLastAccessedDetailsWithEntities $
--             newGetServiceLastAccessedDetailsWithEntities
--
--         , requestUpdateLoginProfile $
--             newUpdateLoginProfile
--
--         , requestDeleteLoginProfile $
--             newDeleteLoginProfile
--
--         , requestGetGroup $
--             newGetGroup
--
--         , requestGenerateOrganizationsAccessReport $
--             newGenerateOrganizationsAccessReport
--
--         , requestGetPolicy $
--             newGetPolicy
--
--         , requestPutGroupPolicy $
--             newPutGroupPolicy
--
--         , requestDeleteAccountAlias $
--             newDeleteAccountAlias
--
--         , requestGetServiceLastAccessedDetails $
--             newGetServiceLastAccessedDetails
--
--         , requestGetAccountAuthorizationDetails $
--             newGetAccountAuthorizationDetails
--
--         , requestDeleteGroup $
--             newDeleteGroup
--
--         , requestGetServiceLinkedRoleDeletionStatus $
--             newGetServiceLinkedRoleDeletionStatus
--
--         , requestCreateSAMLProvider $
--             newCreateSAMLProvider
--
--         , requestDetachUserPolicy $
--             newDetachUserPolicy
--
--         , requestUpdateGroup $
--             newUpdateGroup
--
--         , requestListMFADevices $
--             newListMFADevices
--
--         , requestListServerCertificateTags $
--             newListServerCertificateTags
--
--         , requestListGroupPolicies $
--             newListGroupPolicies
--
--         , requestUntagSAMLProvider $
--             newUntagSAMLProvider
--
--         , requestDeleteSAMLProvider $
--             newDeleteSAMLProvider
--
--         , requestCreateAccountAlias $
--             newCreateAccountAlias
--
--         , requestTagUser $
--             newTagUser
--
--         , requestUntagInstanceProfile $
--             newUntagInstanceProfile
--
--         , requestListAccountAliases $
--             newListAccountAliases
--
--         , requestUpdateSAMLProvider $
--             newUpdateSAMLProvider
--
--         , requestListInstanceProfiles $
--             newListInstanceProfiles
--
--         , requestDeleteInstanceProfile $
--             newDeleteInstanceProfile
--
--         , requestGetAccountSummary $
--             newGetAccountSummary
--
--         , requestDeletePolicyVersion $
--             newDeletePolicyVersion
--
--         , requestUntagMFADevice $
--             newUntagMFADevice
--
--         , requestGetCredentialReport $
--             newGetCredentialReport
--
--         , requestListPolicyVersions $
--             newListPolicyVersions
--
--         , requestDeleteUserPermissionsBoundary $
--             newDeleteUserPermissionsBoundary
--
--         , requestUntagUser $
--             newUntagUser
--
--         , requestDetachGroupPolicy $
--             newDetachGroupPolicy
--
--         , requestAddClientIDToOpenIDConnectProvider $
--             newAddClientIDToOpenIDConnectProvider
--
--         , requestGetSSHPublicKey $
--             newGetSSHPublicKey
--
--         , requestUpdateUser $
--             newUpdateUser
--
--         , requestListUsers $
--             newListUsers
--
--         , requestListRolePolicies $
--             newListRolePolicies
--
--         , requestPutUserPolicy $
--             newPutUserPolicy
--
--         , requestTagSAMLProvider $
--             newTagSAMLProvider
--
--         , requestGetAccessKeyLastUsed $
--             newGetAccessKeyLastUsed
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestGetAccountPasswordPolicy $
--             newGetAccountPasswordPolicy
--
--         , requestUpdateOpenIDConnectProviderThumbprint $
--             newUpdateOpenIDConnectProviderThumbprint
--
--         , requestGetContextKeysForCustomPolicy $
--             newGetContextKeysForCustomPolicy
--
--         , requestUpdateRole $
--             newUpdateRole
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
--         , requestSimulateCustomPolicy $
--             newSimulateCustomPolicy
--
--         , requestPutRolePolicy $
--             newPutRolePolicy
--
--         , requestPutRolePermissionsBoundary $
--             newPutRolePermissionsBoundary
--
--         , requestUntagRole $
--             newUntagRole
--
--         , requestTagServerCertificate $
--             newTagServerCertificate
--
--         , requestCreateOpenIDConnectProvider $
--             newCreateOpenIDConnectProvider
--
--         , requestListAccessKeys $
--             newListAccessKeys
--
--         , requestSetSecurityTokenServicePreferences $
--             newSetSecurityTokenServicePreferences
--
--         , requestAttachUserPolicy $
--             newAttachUserPolicy
--
--         , requestListUserTags $
--             newListUserTags
--
--         , requestDeactivateMFADevice $
--             newDeactivateMFADevice
--
--         , requestGetRole $
--             newGetRole
--
--           ]

--     , testGroup "response"
--         [ responseListSigningCertificates $
--             newListSigningCertificatesResponse
--
--         , responseListRoleTags $
--             newListRoleTagsResponse
--
--         , responseAttachRolePolicy $
--             newAttachRolePolicyResponse
--
--         , responseDeleteOpenIDConnectProvider $
--             newDeleteOpenIDConnectProviderResponse
--
--         , responseGetUser $
--             newGetUserResponse
--
--         , responseCreateVirtualMFADevice $
--             newCreateVirtualMFADeviceResponse
--
--         , responseUpdateSSHPublicKey $
--             newUpdateSSHPublicKeyResponse
--
--         , responseUntagOpenIDConnectProvider $
--             newUntagOpenIDConnectProviderResponse
--
--         , responseDeleteSSHPublicKey $
--             newDeleteSSHPublicKeyResponse
--
--         , responseListOpenIDConnectProviders $
--             newListOpenIDConnectProvidersResponse
--
--         , responseListEntitiesForPolicy $
--             newListEntitiesForPolicyResponse
--
--         , responseCreateServiceLinkedRole $
--             newCreateServiceLinkedRoleResponse
--
--         , responseGetSAMLProvider $
--             newGetSAMLProviderResponse
--
--         , responseListPolicies $
--             newListPoliciesResponse
--
--         , responseCreatePolicy $
--             newCreatePolicyResponse
--
--         , responseListGroupsForUser $
--             newListGroupsForUserResponse
--
--         , responseGetContextKeysForPrincipalPolicy $
--             newGetContextKeysForPolicyResponse
--
--         , responseSimulatePrincipalPolicy $
--             newSimulatePolicyResponse
--
--         , responseListInstanceProfileTags $
--             newListInstanceProfileTagsResponse
--
--         , responseDeletePolicy $
--             newDeletePolicyResponse
--
--         , responseUpdateServerCertificate $
--             newUpdateServerCertificateResponse
--
--         , responseListAttachedGroupPolicies $
--             newListAttachedGroupPoliciesResponse
--
--         , responseUpdateAssumeRolePolicy $
--             newUpdateAssumeRolePolicyResponse
--
--         , responseUntagServerCertificate $
--             newUntagServerCertificateResponse
--
--         , responseChangePassword $
--             newChangePasswordResponse
--
--         , responseListServerCertificates $
--             newListServerCertificatesResponse
--
--         , responseGetGroupPolicy $
--             newGetGroupPolicyResponse
--
--         , responseDeleteServerCertificate $
--             newDeleteServerCertificateResponse
--
--         , responseListMFADeviceTags $
--             newListMFADeviceTagsResponse
--
--         , responseUntagPolicy $
--             newUntagPolicyResponse
--
--         , responseGetLoginProfile $
--             newGetLoginProfileResponse
--
--         , responseCreatePolicyVersion $
--             newCreatePolicyVersionResponse
--
--         , responseGetServerCertificate $
--             newGetServerCertificateResponse
--
--         , responseListGroups $
--             newListGroupsResponse
--
--         , responseTagRole $
--             newTagRoleResponse
--
--         , responseRemoveUserFromGroup $
--             newRemoveUserFromGroupResponse
--
--         , responseDeleteGroupPolicy $
--             newDeleteGroupPolicyResponse
--
--         , responseCreateInstanceProfile $
--             newCreateInstanceProfileResponse
--
--         , responseRemoveRoleFromInstanceProfile $
--             newRemoveRoleFromInstanceProfileResponse
--
--         , responseGenerateCredentialReport $
--             newGenerateCredentialReportResponse
--
--         , responseResetServiceSpecificCredential $
--             newResetServiceSpecificCredentialResponse
--
--         , responseDetachRolePolicy $
--             newDetachRolePolicyResponse
--
--         , responseGenerateServiceLastAccessedDetails $
--             newGenerateServiceLastAccessedDetailsResponse
--
--         , responseSetDefaultPolicyVersion $
--             newSetDefaultPolicyVersionResponse
--
--         , responseUpdateRoleDescription $
--             newUpdateRoleDescriptionResponse
--
--         , responseUploadServerCertificate $
--             newUploadServerCertificateResponse
--
--         , responseListPolicyTags $
--             newListPolicyTagsResponse
--
--         , responseListSAMLProviders $
--             newListSAMLProvidersResponse
--
--         , responseCreateGroup $
--             newCreateGroupResponse
--
--         , responseEnableMFADevice $
--             newEnableMFADeviceResponse
--
--         , responseListPoliciesGrantingServiceAccess $
--             newListPoliciesGrantingServiceAccessResponse
--
--         , responseGetOpenIDConnectProvider $
--             newGetOpenIDConnectProviderResponse
--
--         , responseCreateRole $
--             newCreateRoleResponse
--
--         , responseDeleteUserPolicy $
--             newDeleteUserPolicyResponse
--
--         , responseTagInstanceProfile $
--             newTagInstanceProfileResponse
--
--         , responsePutUserPermissionsBoundary $
--             newPutUserPermissionsBoundaryResponse
--
--         , responseTagMFADevice $
--             newTagMFADeviceResponse
--
--         , responseUploadSigningCertificate $
--             newUploadSigningCertificateResponse
--
--         , responseListOpenIDConnectProviderTags $
--             newListOpenIDConnectProviderTagsResponse
--
--         , responseListRoles $
--             newListRolesResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseDeleteRolePolicy $
--             newDeleteRolePolicyResponse
--
--         , responseDeleteRolePermissionsBoundary $
--             newDeleteRolePermissionsBoundaryResponse
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
--         , responseListAttachedRolePolicies $
--             newListAttachedRolePoliciesResponse
--
--         , responseCreateServiceSpecificCredential $
--             newCreateServiceSpecificCredentialResponse
--
--         , responseDeleteAccessKey $
--             newDeleteAccessKeyResponse
--
--         , responseUpdateAccessKey $
--             newUpdateAccessKeyResponse
--
--         , responseGetRolePolicy $
--             newGetRolePolicyResponse
--
--         , responseDeleteVirtualMFADevice $
--             newDeleteVirtualMFADeviceResponse
--
--         , responseResyncMFADevice $
--             newResyncMFADeviceResponse
--
--         , responseListAttachedUserPolicies $
--             newListAttachedUserPoliciesResponse
--
--         , responseListSSHPublicKeys $
--             newListSSHPublicKeysResponse
--
--         , responseUpdateAccountPasswordPolicy $
--             newUpdateAccountPasswordPolicyResponse
--
--         , responseUpdateServiceSpecificCredential $
--             newUpdateServiceSpecificCredentialResponse
--
--         , responseUpdateSigningCertificate $
--             newUpdateSigningCertificateResponse
--
--         , responseCreateAccessKey $
--             newCreateAccessKeyResponse
--
--         , responseListServiceSpecificCredentials $
--             newListServiceSpecificCredentialsResponse
--
--         , responseDeleteSigningCertificate $
--             newDeleteSigningCertificateResponse
--
--         , responseGetUserPolicy $
--             newGetUserPolicyResponse
--
--         , responseDeleteAccountPasswordPolicy $
--             newDeleteAccountPasswordPolicyResponse
--
--         , responseDeleteServiceSpecificCredential $
--             newDeleteServiceSpecificCredentialResponse
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
--         , responseListInstanceProfilesForRole $
--             newListInstanceProfilesForRoleResponse
--
--         , responseTagOpenIDConnectProvider $
--             newTagOpenIDConnectProviderResponse
--
--         , responseGetOrganizationsAccessReport $
--             newGetOrganizationsAccessReportResponse
--
--         , responseGetInstanceProfile $
--             newGetInstanceProfileResponse
--
--         , responseAddUserToGroup $
--             newAddUserToGroupResponse
--
--         , responseAttachGroupPolicy $
--             newAttachGroupPolicyResponse
--
--         , responseDeleteServiceLinkedRole $
--             newDeleteServiceLinkedRoleResponse
--
--         , responseListSAMLProviderTags $
--             newListSAMLProviderTagsResponse
--
--         , responseGetServiceLastAccessedDetailsWithEntities $
--             newGetServiceLastAccessedDetailsWithEntitiesResponse
--
--         , responseUpdateLoginProfile $
--             newUpdateLoginProfileResponse
--
--         , responseDeleteLoginProfile $
--             newDeleteLoginProfileResponse
--
--         , responseGetGroup $
--             newGetGroupResponse
--
--         , responseGenerateOrganizationsAccessReport $
--             newGenerateOrganizationsAccessReportResponse
--
--         , responseGetPolicy $
--             newGetPolicyResponse
--
--         , responsePutGroupPolicy $
--             newPutGroupPolicyResponse
--
--         , responseDeleteAccountAlias $
--             newDeleteAccountAliasResponse
--
--         , responseGetServiceLastAccessedDetails $
--             newGetServiceLastAccessedDetailsResponse
--
--         , responseGetAccountAuthorizationDetails $
--             newGetAccountAuthorizationDetailsResponse
--
--         , responseDeleteGroup $
--             newDeleteGroupResponse
--
--         , responseGetServiceLinkedRoleDeletionStatus $
--             newGetServiceLinkedRoleDeletionStatusResponse
--
--         , responseCreateSAMLProvider $
--             newCreateSAMLProviderResponse
--
--         , responseDetachUserPolicy $
--             newDetachUserPolicyResponse
--
--         , responseUpdateGroup $
--             newUpdateGroupResponse
--
--         , responseListMFADevices $
--             newListMFADevicesResponse
--
--         , responseListServerCertificateTags $
--             newListServerCertificateTagsResponse
--
--         , responseListGroupPolicies $
--             newListGroupPoliciesResponse
--
--         , responseUntagSAMLProvider $
--             newUntagSAMLProviderResponse
--
--         , responseDeleteSAMLProvider $
--             newDeleteSAMLProviderResponse
--
--         , responseCreateAccountAlias $
--             newCreateAccountAliasResponse
--
--         , responseTagUser $
--             newTagUserResponse
--
--         , responseUntagInstanceProfile $
--             newUntagInstanceProfileResponse
--
--         , responseListAccountAliases $
--             newListAccountAliasesResponse
--
--         , responseUpdateSAMLProvider $
--             newUpdateSAMLProviderResponse
--
--         , responseListInstanceProfiles $
--             newListInstanceProfilesResponse
--
--         , responseDeleteInstanceProfile $
--             newDeleteInstanceProfileResponse
--
--         , responseGetAccountSummary $
--             newGetAccountSummaryResponse
--
--         , responseDeletePolicyVersion $
--             newDeletePolicyVersionResponse
--
--         , responseUntagMFADevice $
--             newUntagMFADeviceResponse
--
--         , responseGetCredentialReport $
--             newGetCredentialReportResponse
--
--         , responseListPolicyVersions $
--             newListPolicyVersionsResponse
--
--         , responseDeleteUserPermissionsBoundary $
--             newDeleteUserPermissionsBoundaryResponse
--
--         , responseUntagUser $
--             newUntagUserResponse
--
--         , responseDetachGroupPolicy $
--             newDetachGroupPolicyResponse
--
--         , responseAddClientIDToOpenIDConnectProvider $
--             newAddClientIDToOpenIDConnectProviderResponse
--
--         , responseGetSSHPublicKey $
--             newGetSSHPublicKeyResponse
--
--         , responseUpdateUser $
--             newUpdateUserResponse
--
--         , responseListUsers $
--             newListUsersResponse
--
--         , responseListRolePolicies $
--             newListRolePoliciesResponse
--
--         , responsePutUserPolicy $
--             newPutUserPolicyResponse
--
--         , responseTagSAMLProvider $
--             newTagSAMLProviderResponse
--
--         , responseGetAccessKeyLastUsed $
--             newGetAccessKeyLastUsedResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseGetAccountPasswordPolicy $
--             newGetAccountPasswordPolicyResponse
--
--         , responseUpdateOpenIDConnectProviderThumbprint $
--             newUpdateOpenIDConnectProviderThumbprintResponse
--
--         , responseGetContextKeysForCustomPolicy $
--             newGetContextKeysForPolicyResponse
--
--         , responseUpdateRole $
--             newUpdateRoleResponse
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
--         , responseSimulateCustomPolicy $
--             newSimulatePolicyResponse
--
--         , responsePutRolePolicy $
--             newPutRolePolicyResponse
--
--         , responsePutRolePermissionsBoundary $
--             newPutRolePermissionsBoundaryResponse
--
--         , responseUntagRole $
--             newUntagRoleResponse
--
--         , responseTagServerCertificate $
--             newTagServerCertificateResponse
--
--         , responseCreateOpenIDConnectProvider $
--             newCreateOpenIDConnectProviderResponse
--
--         , responseListAccessKeys $
--             newListAccessKeysResponse
--
--         , responseSetSecurityTokenServicePreferences $
--             newSetSecurityTokenServicePreferencesResponse
--
--         , responseAttachUserPolicy $
--             newAttachUserPolicyResponse
--
--         , responseListUserTags $
--             newListUserTagsResponse
--
--         , responseDeactivateMFADevice $
--             newDeactivateMFADeviceResponse
--
--         , responseGetRole $
--             newGetRoleResponse
--
--           ]
--     ]

-- Requests

requestListSigningCertificates :: ListSigningCertificates -> TestTree
requestListSigningCertificates =
  req
    "ListSigningCertificates"
    "fixture/ListSigningCertificates.yaml"

requestListRoleTags :: ListRoleTags -> TestTree
requestListRoleTags =
  req
    "ListRoleTags"
    "fixture/ListRoleTags.yaml"

requestAttachRolePolicy :: AttachRolePolicy -> TestTree
requestAttachRolePolicy =
  req
    "AttachRolePolicy"
    "fixture/AttachRolePolicy.yaml"

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

requestCreateVirtualMFADevice :: CreateVirtualMFADevice -> TestTree
requestCreateVirtualMFADevice =
  req
    "CreateVirtualMFADevice"
    "fixture/CreateVirtualMFADevice.yaml"

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

requestDeleteSSHPublicKey :: DeleteSSHPublicKey -> TestTree
requestDeleteSSHPublicKey =
  req
    "DeleteSSHPublicKey"
    "fixture/DeleteSSHPublicKey.yaml"

requestListOpenIDConnectProviders :: ListOpenIDConnectProviders -> TestTree
requestListOpenIDConnectProviders =
  req
    "ListOpenIDConnectProviders"
    "fixture/ListOpenIDConnectProviders.yaml"

requestListEntitiesForPolicy :: ListEntitiesForPolicy -> TestTree
requestListEntitiesForPolicy =
  req
    "ListEntitiesForPolicy"
    "fixture/ListEntitiesForPolicy.yaml"

requestCreateServiceLinkedRole :: CreateServiceLinkedRole -> TestTree
requestCreateServiceLinkedRole =
  req
    "CreateServiceLinkedRole"
    "fixture/CreateServiceLinkedRole.yaml"

requestGetSAMLProvider :: GetSAMLProvider -> TestTree
requestGetSAMLProvider =
  req
    "GetSAMLProvider"
    "fixture/GetSAMLProvider.yaml"

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

requestListGroupsForUser :: ListGroupsForUser -> TestTree
requestListGroupsForUser =
  req
    "ListGroupsForUser"
    "fixture/ListGroupsForUser.yaml"

requestGetContextKeysForPrincipalPolicy :: GetContextKeysForPrincipalPolicy -> TestTree
requestGetContextKeysForPrincipalPolicy =
  req
    "GetContextKeysForPrincipalPolicy"
    "fixture/GetContextKeysForPrincipalPolicy.yaml"

requestSimulatePrincipalPolicy :: SimulatePrincipalPolicy -> TestTree
requestSimulatePrincipalPolicy =
  req
    "SimulatePrincipalPolicy"
    "fixture/SimulatePrincipalPolicy.yaml"

requestListInstanceProfileTags :: ListInstanceProfileTags -> TestTree
requestListInstanceProfileTags =
  req
    "ListInstanceProfileTags"
    "fixture/ListInstanceProfileTags.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy =
  req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

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

requestUpdateAssumeRolePolicy :: UpdateAssumeRolePolicy -> TestTree
requestUpdateAssumeRolePolicy =
  req
    "UpdateAssumeRolePolicy"
    "fixture/UpdateAssumeRolePolicy.yaml"

requestUntagServerCertificate :: UntagServerCertificate -> TestTree
requestUntagServerCertificate =
  req
    "UntagServerCertificate"
    "fixture/UntagServerCertificate.yaml"

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

requestGetGroupPolicy :: GetGroupPolicy -> TestTree
requestGetGroupPolicy =
  req
    "GetGroupPolicy"
    "fixture/GetGroupPolicy.yaml"

requestDeleteServerCertificate :: DeleteServerCertificate -> TestTree
requestDeleteServerCertificate =
  req
    "DeleteServerCertificate"
    "fixture/DeleteServerCertificate.yaml"

requestListMFADeviceTags :: ListMFADeviceTags -> TestTree
requestListMFADeviceTags =
  req
    "ListMFADeviceTags"
    "fixture/ListMFADeviceTags.yaml"

requestUntagPolicy :: UntagPolicy -> TestTree
requestUntagPolicy =
  req
    "UntagPolicy"
    "fixture/UntagPolicy.yaml"

requestGetLoginProfile :: GetLoginProfile -> TestTree
requestGetLoginProfile =
  req
    "GetLoginProfile"
    "fixture/GetLoginProfile.yaml"

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

requestListGroups :: ListGroups -> TestTree
requestListGroups =
  req
    "ListGroups"
    "fixture/ListGroups.yaml"

requestTagRole :: TagRole -> TestTree
requestTagRole =
  req
    "TagRole"
    "fixture/TagRole.yaml"

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

requestCreateInstanceProfile :: CreateInstanceProfile -> TestTree
requestCreateInstanceProfile =
  req
    "CreateInstanceProfile"
    "fixture/CreateInstanceProfile.yaml"

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

requestResetServiceSpecificCredential :: ResetServiceSpecificCredential -> TestTree
requestResetServiceSpecificCredential =
  req
    "ResetServiceSpecificCredential"
    "fixture/ResetServiceSpecificCredential.yaml"

requestDetachRolePolicy :: DetachRolePolicy -> TestTree
requestDetachRolePolicy =
  req
    "DetachRolePolicy"
    "fixture/DetachRolePolicy.yaml"

requestGenerateServiceLastAccessedDetails :: GenerateServiceLastAccessedDetails -> TestTree
requestGenerateServiceLastAccessedDetails =
  req
    "GenerateServiceLastAccessedDetails"
    "fixture/GenerateServiceLastAccessedDetails.yaml"

requestSetDefaultPolicyVersion :: SetDefaultPolicyVersion -> TestTree
requestSetDefaultPolicyVersion =
  req
    "SetDefaultPolicyVersion"
    "fixture/SetDefaultPolicyVersion.yaml"

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

requestListPolicyTags :: ListPolicyTags -> TestTree
requestListPolicyTags =
  req
    "ListPolicyTags"
    "fixture/ListPolicyTags.yaml"

requestListSAMLProviders :: ListSAMLProviders -> TestTree
requestListSAMLProviders =
  req
    "ListSAMLProviders"
    "fixture/ListSAMLProviders.yaml"

requestCreateGroup :: CreateGroup -> TestTree
requestCreateGroup =
  req
    "CreateGroup"
    "fixture/CreateGroup.yaml"

requestEnableMFADevice :: EnableMFADevice -> TestTree
requestEnableMFADevice =
  req
    "EnableMFADevice"
    "fixture/EnableMFADevice.yaml"

requestListPoliciesGrantingServiceAccess :: ListPoliciesGrantingServiceAccess -> TestTree
requestListPoliciesGrantingServiceAccess =
  req
    "ListPoliciesGrantingServiceAccess"
    "fixture/ListPoliciesGrantingServiceAccess.yaml"

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

requestDeleteUserPolicy :: DeleteUserPolicy -> TestTree
requestDeleteUserPolicy =
  req
    "DeleteUserPolicy"
    "fixture/DeleteUserPolicy.yaml"

requestTagInstanceProfile :: TagInstanceProfile -> TestTree
requestTagInstanceProfile =
  req
    "TagInstanceProfile"
    "fixture/TagInstanceProfile.yaml"

requestPutUserPermissionsBoundary :: PutUserPermissionsBoundary -> TestTree
requestPutUserPermissionsBoundary =
  req
    "PutUserPermissionsBoundary"
    "fixture/PutUserPermissionsBoundary.yaml"

requestTagMFADevice :: TagMFADevice -> TestTree
requestTagMFADevice =
  req
    "TagMFADevice"
    "fixture/TagMFADevice.yaml"

requestUploadSigningCertificate :: UploadSigningCertificate -> TestTree
requestUploadSigningCertificate =
  req
    "UploadSigningCertificate"
    "fixture/UploadSigningCertificate.yaml"

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

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestDeleteRolePolicy :: DeleteRolePolicy -> TestTree
requestDeleteRolePolicy =
  req
    "DeleteRolePolicy"
    "fixture/DeleteRolePolicy.yaml"

requestDeleteRolePermissionsBoundary :: DeleteRolePermissionsBoundary -> TestTree
requestDeleteRolePermissionsBoundary =
  req
    "DeleteRolePermissionsBoundary"
    "fixture/DeleteRolePermissionsBoundary.yaml"

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

requestListAttachedRolePolicies :: ListAttachedRolePolicies -> TestTree
requestListAttachedRolePolicies =
  req
    "ListAttachedRolePolicies"
    "fixture/ListAttachedRolePolicies.yaml"

requestCreateServiceSpecificCredential :: CreateServiceSpecificCredential -> TestTree
requestCreateServiceSpecificCredential =
  req
    "CreateServiceSpecificCredential"
    "fixture/CreateServiceSpecificCredential.yaml"

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

requestGetRolePolicy :: GetRolePolicy -> TestTree
requestGetRolePolicy =
  req
    "GetRolePolicy"
    "fixture/GetRolePolicy.yaml"

requestDeleteVirtualMFADevice :: DeleteVirtualMFADevice -> TestTree
requestDeleteVirtualMFADevice =
  req
    "DeleteVirtualMFADevice"
    "fixture/DeleteVirtualMFADevice.yaml"

requestResyncMFADevice :: ResyncMFADevice -> TestTree
requestResyncMFADevice =
  req
    "ResyncMFADevice"
    "fixture/ResyncMFADevice.yaml"

requestListAttachedUserPolicies :: ListAttachedUserPolicies -> TestTree
requestListAttachedUserPolicies =
  req
    "ListAttachedUserPolicies"
    "fixture/ListAttachedUserPolicies.yaml"

requestListSSHPublicKeys :: ListSSHPublicKeys -> TestTree
requestListSSHPublicKeys =
  req
    "ListSSHPublicKeys"
    "fixture/ListSSHPublicKeys.yaml"

requestUpdateAccountPasswordPolicy :: UpdateAccountPasswordPolicy -> TestTree
requestUpdateAccountPasswordPolicy =
  req
    "UpdateAccountPasswordPolicy"
    "fixture/UpdateAccountPasswordPolicy.yaml"

requestUpdateServiceSpecificCredential :: UpdateServiceSpecificCredential -> TestTree
requestUpdateServiceSpecificCredential =
  req
    "UpdateServiceSpecificCredential"
    "fixture/UpdateServiceSpecificCredential.yaml"

requestUpdateSigningCertificate :: UpdateSigningCertificate -> TestTree
requestUpdateSigningCertificate =
  req
    "UpdateSigningCertificate"
    "fixture/UpdateSigningCertificate.yaml"

requestCreateAccessKey :: CreateAccessKey -> TestTree
requestCreateAccessKey =
  req
    "CreateAccessKey"
    "fixture/CreateAccessKey.yaml"

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

requestGetUserPolicy :: GetUserPolicy -> TestTree
requestGetUserPolicy =
  req
    "GetUserPolicy"
    "fixture/GetUserPolicy.yaml"

requestDeleteAccountPasswordPolicy :: DeleteAccountPasswordPolicy -> TestTree
requestDeleteAccountPasswordPolicy =
  req
    "DeleteAccountPasswordPolicy"
    "fixture/DeleteAccountPasswordPolicy.yaml"

requestDeleteServiceSpecificCredential :: DeleteServiceSpecificCredential -> TestTree
requestDeleteServiceSpecificCredential =
  req
    "DeleteServiceSpecificCredential"
    "fixture/DeleteServiceSpecificCredential.yaml"

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

requestListInstanceProfilesForRole :: ListInstanceProfilesForRole -> TestTree
requestListInstanceProfilesForRole =
  req
    "ListInstanceProfilesForRole"
    "fixture/ListInstanceProfilesForRole.yaml"

requestTagOpenIDConnectProvider :: TagOpenIDConnectProvider -> TestTree
requestTagOpenIDConnectProvider =
  req
    "TagOpenIDConnectProvider"
    "fixture/TagOpenIDConnectProvider.yaml"

requestGetOrganizationsAccessReport :: GetOrganizationsAccessReport -> TestTree
requestGetOrganizationsAccessReport =
  req
    "GetOrganizationsAccessReport"
    "fixture/GetOrganizationsAccessReport.yaml"

requestGetInstanceProfile :: GetInstanceProfile -> TestTree
requestGetInstanceProfile =
  req
    "GetInstanceProfile"
    "fixture/GetInstanceProfile.yaml"

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

requestDeleteServiceLinkedRole :: DeleteServiceLinkedRole -> TestTree
requestDeleteServiceLinkedRole =
  req
    "DeleteServiceLinkedRole"
    "fixture/DeleteServiceLinkedRole.yaml"

requestListSAMLProviderTags :: ListSAMLProviderTags -> TestTree
requestListSAMLProviderTags =
  req
    "ListSAMLProviderTags"
    "fixture/ListSAMLProviderTags.yaml"

requestGetServiceLastAccessedDetailsWithEntities :: GetServiceLastAccessedDetailsWithEntities -> TestTree
requestGetServiceLastAccessedDetailsWithEntities =
  req
    "GetServiceLastAccessedDetailsWithEntities"
    "fixture/GetServiceLastAccessedDetailsWithEntities.yaml"

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

requestGetGroup :: GetGroup -> TestTree
requestGetGroup =
  req
    "GetGroup"
    "fixture/GetGroup.yaml"

requestGenerateOrganizationsAccessReport :: GenerateOrganizationsAccessReport -> TestTree
requestGenerateOrganizationsAccessReport =
  req
    "GenerateOrganizationsAccessReport"
    "fixture/GenerateOrganizationsAccessReport.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy =
  req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

requestPutGroupPolicy :: PutGroupPolicy -> TestTree
requestPutGroupPolicy =
  req
    "PutGroupPolicy"
    "fixture/PutGroupPolicy.yaml"

requestDeleteAccountAlias :: DeleteAccountAlias -> TestTree
requestDeleteAccountAlias =
  req
    "DeleteAccountAlias"
    "fixture/DeleteAccountAlias.yaml"

requestGetServiceLastAccessedDetails :: GetServiceLastAccessedDetails -> TestTree
requestGetServiceLastAccessedDetails =
  req
    "GetServiceLastAccessedDetails"
    "fixture/GetServiceLastAccessedDetails.yaml"

requestGetAccountAuthorizationDetails :: GetAccountAuthorizationDetails -> TestTree
requestGetAccountAuthorizationDetails =
  req
    "GetAccountAuthorizationDetails"
    "fixture/GetAccountAuthorizationDetails.yaml"

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

requestCreateSAMLProvider :: CreateSAMLProvider -> TestTree
requestCreateSAMLProvider =
  req
    "CreateSAMLProvider"
    "fixture/CreateSAMLProvider.yaml"

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

requestListMFADevices :: ListMFADevices -> TestTree
requestListMFADevices =
  req
    "ListMFADevices"
    "fixture/ListMFADevices.yaml"

requestListServerCertificateTags :: ListServerCertificateTags -> TestTree
requestListServerCertificateTags =
  req
    "ListServerCertificateTags"
    "fixture/ListServerCertificateTags.yaml"

requestListGroupPolicies :: ListGroupPolicies -> TestTree
requestListGroupPolicies =
  req
    "ListGroupPolicies"
    "fixture/ListGroupPolicies.yaml"

requestUntagSAMLProvider :: UntagSAMLProvider -> TestTree
requestUntagSAMLProvider =
  req
    "UntagSAMLProvider"
    "fixture/UntagSAMLProvider.yaml"

requestDeleteSAMLProvider :: DeleteSAMLProvider -> TestTree
requestDeleteSAMLProvider =
  req
    "DeleteSAMLProvider"
    "fixture/DeleteSAMLProvider.yaml"

requestCreateAccountAlias :: CreateAccountAlias -> TestTree
requestCreateAccountAlias =
  req
    "CreateAccountAlias"
    "fixture/CreateAccountAlias.yaml"

requestTagUser :: TagUser -> TestTree
requestTagUser =
  req
    "TagUser"
    "fixture/TagUser.yaml"

requestUntagInstanceProfile :: UntagInstanceProfile -> TestTree
requestUntagInstanceProfile =
  req
    "UntagInstanceProfile"
    "fixture/UntagInstanceProfile.yaml"

requestListAccountAliases :: ListAccountAliases -> TestTree
requestListAccountAliases =
  req
    "ListAccountAliases"
    "fixture/ListAccountAliases.yaml"

requestUpdateSAMLProvider :: UpdateSAMLProvider -> TestTree
requestUpdateSAMLProvider =
  req
    "UpdateSAMLProvider"
    "fixture/UpdateSAMLProvider.yaml"

requestListInstanceProfiles :: ListInstanceProfiles -> TestTree
requestListInstanceProfiles =
  req
    "ListInstanceProfiles"
    "fixture/ListInstanceProfiles.yaml"

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

requestDeletePolicyVersion :: DeletePolicyVersion -> TestTree
requestDeletePolicyVersion =
  req
    "DeletePolicyVersion"
    "fixture/DeletePolicyVersion.yaml"

requestUntagMFADevice :: UntagMFADevice -> TestTree
requestUntagMFADevice =
  req
    "UntagMFADevice"
    "fixture/UntagMFADevice.yaml"

requestGetCredentialReport :: GetCredentialReport -> TestTree
requestGetCredentialReport =
  req
    "GetCredentialReport"
    "fixture/GetCredentialReport.yaml"

requestListPolicyVersions :: ListPolicyVersions -> TestTree
requestListPolicyVersions =
  req
    "ListPolicyVersions"
    "fixture/ListPolicyVersions.yaml"

requestDeleteUserPermissionsBoundary :: DeleteUserPermissionsBoundary -> TestTree
requestDeleteUserPermissionsBoundary =
  req
    "DeleteUserPermissionsBoundary"
    "fixture/DeleteUserPermissionsBoundary.yaml"

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

requestAddClientIDToOpenIDConnectProvider :: AddClientIDToOpenIDConnectProvider -> TestTree
requestAddClientIDToOpenIDConnectProvider =
  req
    "AddClientIDToOpenIDConnectProvider"
    "fixture/AddClientIDToOpenIDConnectProvider.yaml"

requestGetSSHPublicKey :: GetSSHPublicKey -> TestTree
requestGetSSHPublicKey =
  req
    "GetSSHPublicKey"
    "fixture/GetSSHPublicKey.yaml"

requestUpdateUser :: UpdateUser -> TestTree
requestUpdateUser =
  req
    "UpdateUser"
    "fixture/UpdateUser.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers =
  req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestListRolePolicies :: ListRolePolicies -> TestTree
requestListRolePolicies =
  req
    "ListRolePolicies"
    "fixture/ListRolePolicies.yaml"

requestPutUserPolicy :: PutUserPolicy -> TestTree
requestPutUserPolicy =
  req
    "PutUserPolicy"
    "fixture/PutUserPolicy.yaml"

requestTagSAMLProvider :: TagSAMLProvider -> TestTree
requestTagSAMLProvider =
  req
    "TagSAMLProvider"
    "fixture/TagSAMLProvider.yaml"

requestGetAccessKeyLastUsed :: GetAccessKeyLastUsed -> TestTree
requestGetAccessKeyLastUsed =
  req
    "GetAccessKeyLastUsed"
    "fixture/GetAccessKeyLastUsed.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestGetAccountPasswordPolicy :: GetAccountPasswordPolicy -> TestTree
requestGetAccountPasswordPolicy =
  req
    "GetAccountPasswordPolicy"
    "fixture/GetAccountPasswordPolicy.yaml"

requestUpdateOpenIDConnectProviderThumbprint :: UpdateOpenIDConnectProviderThumbprint -> TestTree
requestUpdateOpenIDConnectProviderThumbprint =
  req
    "UpdateOpenIDConnectProviderThumbprint"
    "fixture/UpdateOpenIDConnectProviderThumbprint.yaml"

requestGetContextKeysForCustomPolicy :: GetContextKeysForCustomPolicy -> TestTree
requestGetContextKeysForCustomPolicy =
  req
    "GetContextKeysForCustomPolicy"
    "fixture/GetContextKeysForCustomPolicy.yaml"

requestUpdateRole :: UpdateRole -> TestTree
requestUpdateRole =
  req
    "UpdateRole"
    "fixture/UpdateRole.yaml"

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

requestSimulateCustomPolicy :: SimulateCustomPolicy -> TestTree
requestSimulateCustomPolicy =
  req
    "SimulateCustomPolicy"
    "fixture/SimulateCustomPolicy.yaml"

requestPutRolePolicy :: PutRolePolicy -> TestTree
requestPutRolePolicy =
  req
    "PutRolePolicy"
    "fixture/PutRolePolicy.yaml"

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

requestTagServerCertificate :: TagServerCertificate -> TestTree
requestTagServerCertificate =
  req
    "TagServerCertificate"
    "fixture/TagServerCertificate.yaml"

requestCreateOpenIDConnectProvider :: CreateOpenIDConnectProvider -> TestTree
requestCreateOpenIDConnectProvider =
  req
    "CreateOpenIDConnectProvider"
    "fixture/CreateOpenIDConnectProvider.yaml"

requestListAccessKeys :: ListAccessKeys -> TestTree
requestListAccessKeys =
  req
    "ListAccessKeys"
    "fixture/ListAccessKeys.yaml"

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

requestListUserTags :: ListUserTags -> TestTree
requestListUserTags =
  req
    "ListUserTags"
    "fixture/ListUserTags.yaml"

requestDeactivateMFADevice :: DeactivateMFADevice -> TestTree
requestDeactivateMFADevice =
  req
    "DeactivateMFADevice"
    "fixture/DeactivateMFADevice.yaml"

requestGetRole :: GetRole -> TestTree
requestGetRole =
  req
    "GetRole"
    "fixture/GetRole.yaml"

-- Responses

responseListSigningCertificates :: ListSigningCertificatesResponse -> TestTree
responseListSigningCertificates =
  res
    "ListSigningCertificatesResponse"
    "fixture/ListSigningCertificatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListSigningCertificates)

responseListRoleTags :: ListRoleTagsResponse -> TestTree
responseListRoleTags =
  res
    "ListRoleTagsResponse"
    "fixture/ListRoleTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListRoleTags)

responseAttachRolePolicy :: AttachRolePolicyResponse -> TestTree
responseAttachRolePolicy =
  res
    "AttachRolePolicyResponse"
    "fixture/AttachRolePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy AttachRolePolicy)

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

responseCreateVirtualMFADevice :: CreateVirtualMFADeviceResponse -> TestTree
responseCreateVirtualMFADevice =
  res
    "CreateVirtualMFADeviceResponse"
    "fixture/CreateVirtualMFADeviceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVirtualMFADevice)

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

responseDeleteSSHPublicKey :: DeleteSSHPublicKeyResponse -> TestTree
responseDeleteSSHPublicKey =
  res
    "DeleteSSHPublicKeyResponse"
    "fixture/DeleteSSHPublicKeyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSSHPublicKey)

responseListOpenIDConnectProviders :: ListOpenIDConnectProvidersResponse -> TestTree
responseListOpenIDConnectProviders =
  res
    "ListOpenIDConnectProvidersResponse"
    "fixture/ListOpenIDConnectProvidersResponse.proto"
    defaultService
    (Proxy :: Proxy ListOpenIDConnectProviders)

responseListEntitiesForPolicy :: ListEntitiesForPolicyResponse -> TestTree
responseListEntitiesForPolicy =
  res
    "ListEntitiesForPolicyResponse"
    "fixture/ListEntitiesForPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy ListEntitiesForPolicy)

responseCreateServiceLinkedRole :: CreateServiceLinkedRoleResponse -> TestTree
responseCreateServiceLinkedRole =
  res
    "CreateServiceLinkedRoleResponse"
    "fixture/CreateServiceLinkedRoleResponse.proto"
    defaultService
    (Proxy :: Proxy CreateServiceLinkedRole)

responseGetSAMLProvider :: GetSAMLProviderResponse -> TestTree
responseGetSAMLProvider =
  res
    "GetSAMLProviderResponse"
    "fixture/GetSAMLProviderResponse.proto"
    defaultService
    (Proxy :: Proxy GetSAMLProvider)

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

responseListGroupsForUser :: ListGroupsForUserResponse -> TestTree
responseListGroupsForUser =
  res
    "ListGroupsForUserResponse"
    "fixture/ListGroupsForUserResponse.proto"
    defaultService
    (Proxy :: Proxy ListGroupsForUser)

responseGetContextKeysForPrincipalPolicy :: GetContextKeysForPolicyResponse -> TestTree
responseGetContextKeysForPrincipalPolicy =
  res
    "GetContextKeysForPrincipalPolicyResponse"
    "fixture/GetContextKeysForPrincipalPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetContextKeysForPrincipalPolicy)

responseSimulatePrincipalPolicy :: SimulatePolicyResponse -> TestTree
responseSimulatePrincipalPolicy =
  res
    "SimulatePrincipalPolicyResponse"
    "fixture/SimulatePrincipalPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy SimulatePrincipalPolicy)

responseListInstanceProfileTags :: ListInstanceProfileTagsResponse -> TestTree
responseListInstanceProfileTags =
  res
    "ListInstanceProfileTagsResponse"
    "fixture/ListInstanceProfileTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListInstanceProfileTags)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePolicy)

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

responseUpdateAssumeRolePolicy :: UpdateAssumeRolePolicyResponse -> TestTree
responseUpdateAssumeRolePolicy =
  res
    "UpdateAssumeRolePolicyResponse"
    "fixture/UpdateAssumeRolePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAssumeRolePolicy)

responseUntagServerCertificate :: UntagServerCertificateResponse -> TestTree
responseUntagServerCertificate =
  res
    "UntagServerCertificateResponse"
    "fixture/UntagServerCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy UntagServerCertificate)

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

responseGetGroupPolicy :: GetGroupPolicyResponse -> TestTree
responseGetGroupPolicy =
  res
    "GetGroupPolicyResponse"
    "fixture/GetGroupPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetGroupPolicy)

responseDeleteServerCertificate :: DeleteServerCertificateResponse -> TestTree
responseDeleteServerCertificate =
  res
    "DeleteServerCertificateResponse"
    "fixture/DeleteServerCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteServerCertificate)

responseListMFADeviceTags :: ListMFADeviceTagsResponse -> TestTree
responseListMFADeviceTags =
  res
    "ListMFADeviceTagsResponse"
    "fixture/ListMFADeviceTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListMFADeviceTags)

responseUntagPolicy :: UntagPolicyResponse -> TestTree
responseUntagPolicy =
  res
    "UntagPolicyResponse"
    "fixture/UntagPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy UntagPolicy)

responseGetLoginProfile :: GetLoginProfileResponse -> TestTree
responseGetLoginProfile =
  res
    "GetLoginProfileResponse"
    "fixture/GetLoginProfileResponse.proto"
    defaultService
    (Proxy :: Proxy GetLoginProfile)

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

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups =
  res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListGroups)

responseTagRole :: TagRoleResponse -> TestTree
responseTagRole =
  res
    "TagRoleResponse"
    "fixture/TagRoleResponse.proto"
    defaultService
    (Proxy :: Proxy TagRole)

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

responseCreateInstanceProfile :: CreateInstanceProfileResponse -> TestTree
responseCreateInstanceProfile =
  res
    "CreateInstanceProfileResponse"
    "fixture/CreateInstanceProfileResponse.proto"
    defaultService
    (Proxy :: Proxy CreateInstanceProfile)

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

responseResetServiceSpecificCredential :: ResetServiceSpecificCredentialResponse -> TestTree
responseResetServiceSpecificCredential =
  res
    "ResetServiceSpecificCredentialResponse"
    "fixture/ResetServiceSpecificCredentialResponse.proto"
    defaultService
    (Proxy :: Proxy ResetServiceSpecificCredential)

responseDetachRolePolicy :: DetachRolePolicyResponse -> TestTree
responseDetachRolePolicy =
  res
    "DetachRolePolicyResponse"
    "fixture/DetachRolePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DetachRolePolicy)

responseGenerateServiceLastAccessedDetails :: GenerateServiceLastAccessedDetailsResponse -> TestTree
responseGenerateServiceLastAccessedDetails =
  res
    "GenerateServiceLastAccessedDetailsResponse"
    "fixture/GenerateServiceLastAccessedDetailsResponse.proto"
    defaultService
    (Proxy :: Proxy GenerateServiceLastAccessedDetails)

responseSetDefaultPolicyVersion :: SetDefaultPolicyVersionResponse -> TestTree
responseSetDefaultPolicyVersion =
  res
    "SetDefaultPolicyVersionResponse"
    "fixture/SetDefaultPolicyVersionResponse.proto"
    defaultService
    (Proxy :: Proxy SetDefaultPolicyVersion)

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

responseListPolicyTags :: ListPolicyTagsResponse -> TestTree
responseListPolicyTags =
  res
    "ListPolicyTagsResponse"
    "fixture/ListPolicyTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPolicyTags)

responseListSAMLProviders :: ListSAMLProvidersResponse -> TestTree
responseListSAMLProviders =
  res
    "ListSAMLProvidersResponse"
    "fixture/ListSAMLProvidersResponse.proto"
    defaultService
    (Proxy :: Proxy ListSAMLProviders)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup =
  res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateGroup)

responseEnableMFADevice :: EnableMFADeviceResponse -> TestTree
responseEnableMFADevice =
  res
    "EnableMFADeviceResponse"
    "fixture/EnableMFADeviceResponse.proto"
    defaultService
    (Proxy :: Proxy EnableMFADevice)

responseListPoliciesGrantingServiceAccess :: ListPoliciesGrantingServiceAccessResponse -> TestTree
responseListPoliciesGrantingServiceAccess =
  res
    "ListPoliciesGrantingServiceAccessResponse"
    "fixture/ListPoliciesGrantingServiceAccessResponse.proto"
    defaultService
    (Proxy :: Proxy ListPoliciesGrantingServiceAccess)

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

responseDeleteUserPolicy :: DeleteUserPolicyResponse -> TestTree
responseDeleteUserPolicy =
  res
    "DeleteUserPolicyResponse"
    "fixture/DeleteUserPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUserPolicy)

responseTagInstanceProfile :: TagInstanceProfileResponse -> TestTree
responseTagInstanceProfile =
  res
    "TagInstanceProfileResponse"
    "fixture/TagInstanceProfileResponse.proto"
    defaultService
    (Proxy :: Proxy TagInstanceProfile)

responsePutUserPermissionsBoundary :: PutUserPermissionsBoundaryResponse -> TestTree
responsePutUserPermissionsBoundary =
  res
    "PutUserPermissionsBoundaryResponse"
    "fixture/PutUserPermissionsBoundaryResponse.proto"
    defaultService
    (Proxy :: Proxy PutUserPermissionsBoundary)

responseTagMFADevice :: TagMFADeviceResponse -> TestTree
responseTagMFADevice =
  res
    "TagMFADeviceResponse"
    "fixture/TagMFADeviceResponse.proto"
    defaultService
    (Proxy :: Proxy TagMFADevice)

responseUploadSigningCertificate :: UploadSigningCertificateResponse -> TestTree
responseUploadSigningCertificate =
  res
    "UploadSigningCertificateResponse"
    "fixture/UploadSigningCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy UploadSigningCertificate)

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

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUser)

responseDeleteRolePolicy :: DeleteRolePolicyResponse -> TestTree
responseDeleteRolePolicy =
  res
    "DeleteRolePolicyResponse"
    "fixture/DeleteRolePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRolePolicy)

responseDeleteRolePermissionsBoundary :: DeleteRolePermissionsBoundaryResponse -> TestTree
responseDeleteRolePermissionsBoundary =
  res
    "DeleteRolePermissionsBoundaryResponse"
    "fixture/DeleteRolePermissionsBoundaryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRolePermissionsBoundary)

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

responseListAttachedRolePolicies :: ListAttachedRolePoliciesResponse -> TestTree
responseListAttachedRolePolicies =
  res
    "ListAttachedRolePoliciesResponse"
    "fixture/ListAttachedRolePoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAttachedRolePolicies)

responseCreateServiceSpecificCredential :: CreateServiceSpecificCredentialResponse -> TestTree
responseCreateServiceSpecificCredential =
  res
    "CreateServiceSpecificCredentialResponse"
    "fixture/CreateServiceSpecificCredentialResponse.proto"
    defaultService
    (Proxy :: Proxy CreateServiceSpecificCredential)

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

responseGetRolePolicy :: GetRolePolicyResponse -> TestTree
responseGetRolePolicy =
  res
    "GetRolePolicyResponse"
    "fixture/GetRolePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetRolePolicy)

responseDeleteVirtualMFADevice :: DeleteVirtualMFADeviceResponse -> TestTree
responseDeleteVirtualMFADevice =
  res
    "DeleteVirtualMFADeviceResponse"
    "fixture/DeleteVirtualMFADeviceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVirtualMFADevice)

responseResyncMFADevice :: ResyncMFADeviceResponse -> TestTree
responseResyncMFADevice =
  res
    "ResyncMFADeviceResponse"
    "fixture/ResyncMFADeviceResponse.proto"
    defaultService
    (Proxy :: Proxy ResyncMFADevice)

responseListAttachedUserPolicies :: ListAttachedUserPoliciesResponse -> TestTree
responseListAttachedUserPolicies =
  res
    "ListAttachedUserPoliciesResponse"
    "fixture/ListAttachedUserPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAttachedUserPolicies)

responseListSSHPublicKeys :: ListSSHPublicKeysResponse -> TestTree
responseListSSHPublicKeys =
  res
    "ListSSHPublicKeysResponse"
    "fixture/ListSSHPublicKeysResponse.proto"
    defaultService
    (Proxy :: Proxy ListSSHPublicKeys)

responseUpdateAccountPasswordPolicy :: UpdateAccountPasswordPolicyResponse -> TestTree
responseUpdateAccountPasswordPolicy =
  res
    "UpdateAccountPasswordPolicyResponse"
    "fixture/UpdateAccountPasswordPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAccountPasswordPolicy)

responseUpdateServiceSpecificCredential :: UpdateServiceSpecificCredentialResponse -> TestTree
responseUpdateServiceSpecificCredential =
  res
    "UpdateServiceSpecificCredentialResponse"
    "fixture/UpdateServiceSpecificCredentialResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateServiceSpecificCredential)

responseUpdateSigningCertificate :: UpdateSigningCertificateResponse -> TestTree
responseUpdateSigningCertificate =
  res
    "UpdateSigningCertificateResponse"
    "fixture/UpdateSigningCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSigningCertificate)

responseCreateAccessKey :: CreateAccessKeyResponse -> TestTree
responseCreateAccessKey =
  res
    "CreateAccessKeyResponse"
    "fixture/CreateAccessKeyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAccessKey)

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

responseGetUserPolicy :: GetUserPolicyResponse -> TestTree
responseGetUserPolicy =
  res
    "GetUserPolicyResponse"
    "fixture/GetUserPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetUserPolicy)

responseDeleteAccountPasswordPolicy :: DeleteAccountPasswordPolicyResponse -> TestTree
responseDeleteAccountPasswordPolicy =
  res
    "DeleteAccountPasswordPolicyResponse"
    "fixture/DeleteAccountPasswordPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAccountPasswordPolicy)

responseDeleteServiceSpecificCredential :: DeleteServiceSpecificCredentialResponse -> TestTree
responseDeleteServiceSpecificCredential =
  res
    "DeleteServiceSpecificCredentialResponse"
    "fixture/DeleteServiceSpecificCredentialResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteServiceSpecificCredential)

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

responseListInstanceProfilesForRole :: ListInstanceProfilesForRoleResponse -> TestTree
responseListInstanceProfilesForRole =
  res
    "ListInstanceProfilesForRoleResponse"
    "fixture/ListInstanceProfilesForRoleResponse.proto"
    defaultService
    (Proxy :: Proxy ListInstanceProfilesForRole)

responseTagOpenIDConnectProvider :: TagOpenIDConnectProviderResponse -> TestTree
responseTagOpenIDConnectProvider =
  res
    "TagOpenIDConnectProviderResponse"
    "fixture/TagOpenIDConnectProviderResponse.proto"
    defaultService
    (Proxy :: Proxy TagOpenIDConnectProvider)

responseGetOrganizationsAccessReport :: GetOrganizationsAccessReportResponse -> TestTree
responseGetOrganizationsAccessReport =
  res
    "GetOrganizationsAccessReportResponse"
    "fixture/GetOrganizationsAccessReportResponse.proto"
    defaultService
    (Proxy :: Proxy GetOrganizationsAccessReport)

responseGetInstanceProfile :: GetInstanceProfileResponse -> TestTree
responseGetInstanceProfile =
  res
    "GetInstanceProfileResponse"
    "fixture/GetInstanceProfileResponse.proto"
    defaultService
    (Proxy :: Proxy GetInstanceProfile)

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

responseDeleteServiceLinkedRole :: DeleteServiceLinkedRoleResponse -> TestTree
responseDeleteServiceLinkedRole =
  res
    "DeleteServiceLinkedRoleResponse"
    "fixture/DeleteServiceLinkedRoleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteServiceLinkedRole)

responseListSAMLProviderTags :: ListSAMLProviderTagsResponse -> TestTree
responseListSAMLProviderTags =
  res
    "ListSAMLProviderTagsResponse"
    "fixture/ListSAMLProviderTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSAMLProviderTags)

responseGetServiceLastAccessedDetailsWithEntities :: GetServiceLastAccessedDetailsWithEntitiesResponse -> TestTree
responseGetServiceLastAccessedDetailsWithEntities =
  res
    "GetServiceLastAccessedDetailsWithEntitiesResponse"
    "fixture/GetServiceLastAccessedDetailsWithEntitiesResponse.proto"
    defaultService
    (Proxy :: Proxy GetServiceLastAccessedDetailsWithEntities)

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

responseGetGroup :: GetGroupResponse -> TestTree
responseGetGroup =
  res
    "GetGroupResponse"
    "fixture/GetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy GetGroup)

responseGenerateOrganizationsAccessReport :: GenerateOrganizationsAccessReportResponse -> TestTree
responseGenerateOrganizationsAccessReport =
  res
    "GenerateOrganizationsAccessReportResponse"
    "fixture/GenerateOrganizationsAccessReportResponse.proto"
    defaultService
    (Proxy :: Proxy GenerateOrganizationsAccessReport)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetPolicy)

responsePutGroupPolicy :: PutGroupPolicyResponse -> TestTree
responsePutGroupPolicy =
  res
    "PutGroupPolicyResponse"
    "fixture/PutGroupPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutGroupPolicy)

responseDeleteAccountAlias :: DeleteAccountAliasResponse -> TestTree
responseDeleteAccountAlias =
  res
    "DeleteAccountAliasResponse"
    "fixture/DeleteAccountAliasResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAccountAlias)

responseGetServiceLastAccessedDetails :: GetServiceLastAccessedDetailsResponse -> TestTree
responseGetServiceLastAccessedDetails =
  res
    "GetServiceLastAccessedDetailsResponse"
    "fixture/GetServiceLastAccessedDetailsResponse.proto"
    defaultService
    (Proxy :: Proxy GetServiceLastAccessedDetails)

responseGetAccountAuthorizationDetails :: GetAccountAuthorizationDetailsResponse -> TestTree
responseGetAccountAuthorizationDetails =
  res
    "GetAccountAuthorizationDetailsResponse"
    "fixture/GetAccountAuthorizationDetailsResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccountAuthorizationDetails)

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

responseCreateSAMLProvider :: CreateSAMLProviderResponse -> TestTree
responseCreateSAMLProvider =
  res
    "CreateSAMLProviderResponse"
    "fixture/CreateSAMLProviderResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSAMLProvider)

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

responseListMFADevices :: ListMFADevicesResponse -> TestTree
responseListMFADevices =
  res
    "ListMFADevicesResponse"
    "fixture/ListMFADevicesResponse.proto"
    defaultService
    (Proxy :: Proxy ListMFADevices)

responseListServerCertificateTags :: ListServerCertificateTagsResponse -> TestTree
responseListServerCertificateTags =
  res
    "ListServerCertificateTagsResponse"
    "fixture/ListServerCertificateTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListServerCertificateTags)

responseListGroupPolicies :: ListGroupPoliciesResponse -> TestTree
responseListGroupPolicies =
  res
    "ListGroupPoliciesResponse"
    "fixture/ListGroupPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListGroupPolicies)

responseUntagSAMLProvider :: UntagSAMLProviderResponse -> TestTree
responseUntagSAMLProvider =
  res
    "UntagSAMLProviderResponse"
    "fixture/UntagSAMLProviderResponse.proto"
    defaultService
    (Proxy :: Proxy UntagSAMLProvider)

responseDeleteSAMLProvider :: DeleteSAMLProviderResponse -> TestTree
responseDeleteSAMLProvider =
  res
    "DeleteSAMLProviderResponse"
    "fixture/DeleteSAMLProviderResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSAMLProvider)

responseCreateAccountAlias :: CreateAccountAliasResponse -> TestTree
responseCreateAccountAlias =
  res
    "CreateAccountAliasResponse"
    "fixture/CreateAccountAliasResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAccountAlias)

responseTagUser :: TagUserResponse -> TestTree
responseTagUser =
  res
    "TagUserResponse"
    "fixture/TagUserResponse.proto"
    defaultService
    (Proxy :: Proxy TagUser)

responseUntagInstanceProfile :: UntagInstanceProfileResponse -> TestTree
responseUntagInstanceProfile =
  res
    "UntagInstanceProfileResponse"
    "fixture/UntagInstanceProfileResponse.proto"
    defaultService
    (Proxy :: Proxy UntagInstanceProfile)

responseListAccountAliases :: ListAccountAliasesResponse -> TestTree
responseListAccountAliases =
  res
    "ListAccountAliasesResponse"
    "fixture/ListAccountAliasesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAccountAliases)

responseUpdateSAMLProvider :: UpdateSAMLProviderResponse -> TestTree
responseUpdateSAMLProvider =
  res
    "UpdateSAMLProviderResponse"
    "fixture/UpdateSAMLProviderResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSAMLProvider)

responseListInstanceProfiles :: ListInstanceProfilesResponse -> TestTree
responseListInstanceProfiles =
  res
    "ListInstanceProfilesResponse"
    "fixture/ListInstanceProfilesResponse.proto"
    defaultService
    (Proxy :: Proxy ListInstanceProfiles)

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

responseDeletePolicyVersion :: DeletePolicyVersionResponse -> TestTree
responseDeletePolicyVersion =
  res
    "DeletePolicyVersionResponse"
    "fixture/DeletePolicyVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePolicyVersion)

responseUntagMFADevice :: UntagMFADeviceResponse -> TestTree
responseUntagMFADevice =
  res
    "UntagMFADeviceResponse"
    "fixture/UntagMFADeviceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagMFADevice)

responseGetCredentialReport :: GetCredentialReportResponse -> TestTree
responseGetCredentialReport =
  res
    "GetCredentialReportResponse"
    "fixture/GetCredentialReportResponse.proto"
    defaultService
    (Proxy :: Proxy GetCredentialReport)

responseListPolicyVersions :: ListPolicyVersionsResponse -> TestTree
responseListPolicyVersions =
  res
    "ListPolicyVersionsResponse"
    "fixture/ListPolicyVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPolicyVersions)

responseDeleteUserPermissionsBoundary :: DeleteUserPermissionsBoundaryResponse -> TestTree
responseDeleteUserPermissionsBoundary =
  res
    "DeleteUserPermissionsBoundaryResponse"
    "fixture/DeleteUserPermissionsBoundaryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUserPermissionsBoundary)

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

responseAddClientIDToOpenIDConnectProvider :: AddClientIDToOpenIDConnectProviderResponse -> TestTree
responseAddClientIDToOpenIDConnectProvider =
  res
    "AddClientIDToOpenIDConnectProviderResponse"
    "fixture/AddClientIDToOpenIDConnectProviderResponse.proto"
    defaultService
    (Proxy :: Proxy AddClientIDToOpenIDConnectProvider)

responseGetSSHPublicKey :: GetSSHPublicKeyResponse -> TestTree
responseGetSSHPublicKey =
  res
    "GetSSHPublicKeyResponse"
    "fixture/GetSSHPublicKeyResponse.proto"
    defaultService
    (Proxy :: Proxy GetSSHPublicKey)

responseUpdateUser :: UpdateUserResponse -> TestTree
responseUpdateUser =
  res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUser)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    defaultService
    (Proxy :: Proxy ListUsers)

responseListRolePolicies :: ListRolePoliciesResponse -> TestTree
responseListRolePolicies =
  res
    "ListRolePoliciesResponse"
    "fixture/ListRolePoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListRolePolicies)

responsePutUserPolicy :: PutUserPolicyResponse -> TestTree
responsePutUserPolicy =
  res
    "PutUserPolicyResponse"
    "fixture/PutUserPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutUserPolicy)

responseTagSAMLProvider :: TagSAMLProviderResponse -> TestTree
responseTagSAMLProvider =
  res
    "TagSAMLProviderResponse"
    "fixture/TagSAMLProviderResponse.proto"
    defaultService
    (Proxy :: Proxy TagSAMLProvider)

responseGetAccessKeyLastUsed :: GetAccessKeyLastUsedResponse -> TestTree
responseGetAccessKeyLastUsed =
  res
    "GetAccessKeyLastUsedResponse"
    "fixture/GetAccessKeyLastUsedResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccessKeyLastUsed)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUser)

responseGetAccountPasswordPolicy :: GetAccountPasswordPolicyResponse -> TestTree
responseGetAccountPasswordPolicy =
  res
    "GetAccountPasswordPolicyResponse"
    "fixture/GetAccountPasswordPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccountPasswordPolicy)

responseUpdateOpenIDConnectProviderThumbprint :: UpdateOpenIDConnectProviderThumbprintResponse -> TestTree
responseUpdateOpenIDConnectProviderThumbprint =
  res
    "UpdateOpenIDConnectProviderThumbprintResponse"
    "fixture/UpdateOpenIDConnectProviderThumbprintResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateOpenIDConnectProviderThumbprint)

responseGetContextKeysForCustomPolicy :: GetContextKeysForPolicyResponse -> TestTree
responseGetContextKeysForCustomPolicy =
  res
    "GetContextKeysForCustomPolicyResponse"
    "fixture/GetContextKeysForCustomPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetContextKeysForCustomPolicy)

responseUpdateRole :: UpdateRoleResponse -> TestTree
responseUpdateRole =
  res
    "UpdateRoleResponse"
    "fixture/UpdateRoleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRole)

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

responseSimulateCustomPolicy :: SimulatePolicyResponse -> TestTree
responseSimulateCustomPolicy =
  res
    "SimulateCustomPolicyResponse"
    "fixture/SimulateCustomPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy SimulateCustomPolicy)

responsePutRolePolicy :: PutRolePolicyResponse -> TestTree
responsePutRolePolicy =
  res
    "PutRolePolicyResponse"
    "fixture/PutRolePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutRolePolicy)

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

responseTagServerCertificate :: TagServerCertificateResponse -> TestTree
responseTagServerCertificate =
  res
    "TagServerCertificateResponse"
    "fixture/TagServerCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy TagServerCertificate)

responseCreateOpenIDConnectProvider :: CreateOpenIDConnectProviderResponse -> TestTree
responseCreateOpenIDConnectProvider =
  res
    "CreateOpenIDConnectProviderResponse"
    "fixture/CreateOpenIDConnectProviderResponse.proto"
    defaultService
    (Proxy :: Proxy CreateOpenIDConnectProvider)

responseListAccessKeys :: ListAccessKeysResponse -> TestTree
responseListAccessKeys =
  res
    "ListAccessKeysResponse"
    "fixture/ListAccessKeysResponse.proto"
    defaultService
    (Proxy :: Proxy ListAccessKeys)

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

responseListUserTags :: ListUserTagsResponse -> TestTree
responseListUserTags =
  res
    "ListUserTagsResponse"
    "fixture/ListUserTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListUserTags)

responseDeactivateMFADevice :: DeactivateMFADeviceResponse -> TestTree
responseDeactivateMFADevice =
  res
    "DeactivateMFADeviceResponse"
    "fixture/DeactivateMFADeviceResponse.proto"
    defaultService
    (Proxy :: Proxy DeactivateMFADevice)

responseGetRole :: GetRoleResponse -> TestTree
responseGetRole =
  res
    "GetRoleResponse"
    "fixture/GetRoleResponse.proto"
    defaultService
    (Proxy :: Proxy GetRole)
