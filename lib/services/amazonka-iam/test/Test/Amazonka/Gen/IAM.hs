{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.IAM
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.IAM where

import Amazonka.IAM
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.IAM.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAddClientIDToOpenIDConnectProvider $
--             newAddClientIDToOpenIDConnectProvider
--
--         , requestAddRoleToInstanceProfile $
--             newAddRoleToInstanceProfile
--
--         , requestAddUserToGroup $
--             newAddUserToGroup
--
--         , requestAttachGroupPolicy $
--             newAttachGroupPolicy
--
--         , requestAttachRolePolicy $
--             newAttachRolePolicy
--
--         , requestAttachUserPolicy $
--             newAttachUserPolicy
--
--         , requestChangePassword $
--             newChangePassword
--
--         , requestCreateAccessKey $
--             newCreateAccessKey
--
--         , requestCreateAccountAlias $
--             newCreateAccountAlias
--
--         , requestCreateGroup $
--             newCreateGroup
--
--         , requestCreateInstanceProfile $
--             newCreateInstanceProfile
--
--         , requestCreateLoginProfile $
--             newCreateLoginProfile
--
--         , requestCreateOpenIDConnectProvider $
--             newCreateOpenIDConnectProvider
--
--         , requestCreatePolicy $
--             newCreatePolicy
--
--         , requestCreatePolicyVersion $
--             newCreatePolicyVersion
--
--         , requestCreateRole $
--             newCreateRole
--
--         , requestCreateSAMLProvider $
--             newCreateSAMLProvider
--
--         , requestCreateServiceLinkedRole $
--             newCreateServiceLinkedRole
--
--         , requestCreateServiceSpecificCredential $
--             newCreateServiceSpecificCredential
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestCreateVirtualMFADevice $
--             newCreateVirtualMFADevice
--
--         , requestDeactivateMFADevice $
--             newDeactivateMFADevice
--
--         , requestDeleteAccessKey $
--             newDeleteAccessKey
--
--         , requestDeleteAccountAlias $
--             newDeleteAccountAlias
--
--         , requestDeleteAccountPasswordPolicy $
--             newDeleteAccountPasswordPolicy
--
--         , requestDeleteGroup $
--             newDeleteGroup
--
--         , requestDeleteGroupPolicy $
--             newDeleteGroupPolicy
--
--         , requestDeleteInstanceProfile $
--             newDeleteInstanceProfile
--
--         , requestDeleteLoginProfile $
--             newDeleteLoginProfile
--
--         , requestDeleteOpenIDConnectProvider $
--             newDeleteOpenIDConnectProvider
--
--         , requestDeletePolicy $
--             newDeletePolicy
--
--         , requestDeletePolicyVersion $
--             newDeletePolicyVersion
--
--         , requestDeleteRole $
--             newDeleteRole
--
--         , requestDeleteRolePermissionsBoundary $
--             newDeleteRolePermissionsBoundary
--
--         , requestDeleteRolePolicy $
--             newDeleteRolePolicy
--
--         , requestDeleteSAMLProvider $
--             newDeleteSAMLProvider
--
--         , requestDeleteSSHPublicKey $
--             newDeleteSSHPublicKey
--
--         , requestDeleteServerCertificate $
--             newDeleteServerCertificate
--
--         , requestDeleteServiceLinkedRole $
--             newDeleteServiceLinkedRole
--
--         , requestDeleteServiceSpecificCredential $
--             newDeleteServiceSpecificCredential
--
--         , requestDeleteSigningCertificate $
--             newDeleteSigningCertificate
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestDeleteUserPermissionsBoundary $
--             newDeleteUserPermissionsBoundary
--
--         , requestDeleteUserPolicy $
--             newDeleteUserPolicy
--
--         , requestDeleteVirtualMFADevice $
--             newDeleteVirtualMFADevice
--
--         , requestDetachGroupPolicy $
--             newDetachGroupPolicy
--
--         , requestDetachRolePolicy $
--             newDetachRolePolicy
--
--         , requestDetachUserPolicy $
--             newDetachUserPolicy
--
--         , requestEnableMFADevice $
--             newEnableMFADevice
--
--         , requestGenerateCredentialReport $
--             newGenerateCredentialReport
--
--         , requestGenerateOrganizationsAccessReport $
--             newGenerateOrganizationsAccessReport
--
--         , requestGenerateServiceLastAccessedDetails $
--             newGenerateServiceLastAccessedDetails
--
--         , requestGetAccessKeyLastUsed $
--             newGetAccessKeyLastUsed
--
--         , requestGetAccountAuthorizationDetails $
--             newGetAccountAuthorizationDetails
--
--         , requestGetAccountPasswordPolicy $
--             newGetAccountPasswordPolicy
--
--         , requestGetAccountSummary $
--             newGetAccountSummary
--
--         , requestGetContextKeysForCustomPolicy $
--             newGetContextKeysForCustomPolicy
--
--         , requestGetContextKeysForPrincipalPolicy $
--             newGetContextKeysForPrincipalPolicy
--
--         , requestGetCredentialReport $
--             newGetCredentialReport
--
--         , requestGetGroup $
--             newGetGroup
--
--         , requestGetGroupPolicy $
--             newGetGroupPolicy
--
--         , requestGetInstanceProfile $
--             newGetInstanceProfile
--
--         , requestGetLoginProfile $
--             newGetLoginProfile
--
--         , requestGetOpenIDConnectProvider $
--             newGetOpenIDConnectProvider
--
--         , requestGetOrganizationsAccessReport $
--             newGetOrganizationsAccessReport
--
--         , requestGetPolicy $
--             newGetPolicy
--
--         , requestGetPolicyVersion $
--             newGetPolicyVersion
--
--         , requestGetRole $
--             newGetRole
--
--         , requestGetRolePolicy $
--             newGetRolePolicy
--
--         , requestGetSAMLProvider $
--             newGetSAMLProvider
--
--         , requestGetSSHPublicKey $
--             newGetSSHPublicKey
--
--         , requestGetServerCertificate $
--             newGetServerCertificate
--
--         , requestGetServiceLastAccessedDetails $
--             newGetServiceLastAccessedDetails
--
--         , requestGetServiceLastAccessedDetailsWithEntities $
--             newGetServiceLastAccessedDetailsWithEntities
--
--         , requestGetServiceLinkedRoleDeletionStatus $
--             newGetServiceLinkedRoleDeletionStatus
--
--         , requestGetUser $
--             newGetUser
--
--         , requestGetUserPolicy $
--             newGetUserPolicy
--
--         , requestListAccessKeys $
--             newListAccessKeys
--
--         , requestListAccountAliases $
--             newListAccountAliases
--
--         , requestListAttachedGroupPolicies $
--             newListAttachedGroupPolicies
--
--         , requestListAttachedRolePolicies $
--             newListAttachedRolePolicies
--
--         , requestListAttachedUserPolicies $
--             newListAttachedUserPolicies
--
--         , requestListEntitiesForPolicy $
--             newListEntitiesForPolicy
--
--         , requestListGroupPolicies $
--             newListGroupPolicies
--
--         , requestListGroups $
--             newListGroups
--
--         , requestListGroupsForUser $
--             newListGroupsForUser
--
--         , requestListInstanceProfileTags $
--             newListInstanceProfileTags
--
--         , requestListInstanceProfiles $
--             newListInstanceProfiles
--
--         , requestListInstanceProfilesForRole $
--             newListInstanceProfilesForRole
--
--         , requestListMFADeviceTags $
--             newListMFADeviceTags
--
--         , requestListMFADevices $
--             newListMFADevices
--
--         , requestListOpenIDConnectProviderTags $
--             newListOpenIDConnectProviderTags
--
--         , requestListOpenIDConnectProviders $
--             newListOpenIDConnectProviders
--
--         , requestListPolicies $
--             newListPolicies
--
--         , requestListPoliciesGrantingServiceAccess $
--             newListPoliciesGrantingServiceAccess
--
--         , requestListPolicyTags $
--             newListPolicyTags
--
--         , requestListPolicyVersions $
--             newListPolicyVersions
--
--         , requestListRolePolicies $
--             newListRolePolicies
--
--         , requestListRoleTags $
--             newListRoleTags
--
--         , requestListRoles $
--             newListRoles
--
--         , requestListSAMLProviderTags $
--             newListSAMLProviderTags
--
--         , requestListSAMLProviders $
--             newListSAMLProviders
--
--         , requestListSSHPublicKeys $
--             newListSSHPublicKeys
--
--         , requestListServerCertificateTags $
--             newListServerCertificateTags
--
--         , requestListServerCertificates $
--             newListServerCertificates
--
--         , requestListServiceSpecificCredentials $
--             newListServiceSpecificCredentials
--
--         , requestListSigningCertificates $
--             newListSigningCertificates
--
--         , requestListUserPolicies $
--             newListUserPolicies
--
--         , requestListUserTags $
--             newListUserTags
--
--         , requestListUsers $
--             newListUsers
--
--         , requestListVirtualMFADevices $
--             newListVirtualMFADevices
--
--         , requestPutGroupPolicy $
--             newPutGroupPolicy
--
--         , requestPutRolePermissionsBoundary $
--             newPutRolePermissionsBoundary
--
--         , requestPutRolePolicy $
--             newPutRolePolicy
--
--         , requestPutUserPermissionsBoundary $
--             newPutUserPermissionsBoundary
--
--         , requestPutUserPolicy $
--             newPutUserPolicy
--
--         , requestRemoveClientIDFromOpenIDConnectProvider $
--             newRemoveClientIDFromOpenIDConnectProvider
--
--         , requestRemoveRoleFromInstanceProfile $
--             newRemoveRoleFromInstanceProfile
--
--         , requestRemoveUserFromGroup $
--             newRemoveUserFromGroup
--
--         , requestResetServiceSpecificCredential $
--             newResetServiceSpecificCredential
--
--         , requestResyncMFADevice $
--             newResyncMFADevice
--
--         , requestSetDefaultPolicyVersion $
--             newSetDefaultPolicyVersion
--
--         , requestSetSecurityTokenServicePreferences $
--             newSetSecurityTokenServicePreferences
--
--         , requestSimulateCustomPolicy $
--             newSimulateCustomPolicy
--
--         , requestSimulatePrincipalPolicy $
--             newSimulatePrincipalPolicy
--
--         , requestTagInstanceProfile $
--             newTagInstanceProfile
--
--         , requestTagMFADevice $
--             newTagMFADevice
--
--         , requestTagOpenIDConnectProvider $
--             newTagOpenIDConnectProvider
--
--         , requestTagPolicy $
--             newTagPolicy
--
--         , requestTagRole $
--             newTagRole
--
--         , requestTagSAMLProvider $
--             newTagSAMLProvider
--
--         , requestTagServerCertificate $
--             newTagServerCertificate
--
--         , requestTagUser $
--             newTagUser
--
--         , requestUntagInstanceProfile $
--             newUntagInstanceProfile
--
--         , requestUntagMFADevice $
--             newUntagMFADevice
--
--         , requestUntagOpenIDConnectProvider $
--             newUntagOpenIDConnectProvider
--
--         , requestUntagPolicy $
--             newUntagPolicy
--
--         , requestUntagRole $
--             newUntagRole
--
--         , requestUntagSAMLProvider $
--             newUntagSAMLProvider
--
--         , requestUntagServerCertificate $
--             newUntagServerCertificate
--
--         , requestUntagUser $
--             newUntagUser
--
--         , requestUpdateAccessKey $
--             newUpdateAccessKey
--
--         , requestUpdateAccountPasswordPolicy $
--             newUpdateAccountPasswordPolicy
--
--         , requestUpdateAssumeRolePolicy $
--             newUpdateAssumeRolePolicy
--
--         , requestUpdateGroup $
--             newUpdateGroup
--
--         , requestUpdateLoginProfile $
--             newUpdateLoginProfile
--
--         , requestUpdateOpenIDConnectProviderThumbprint $
--             newUpdateOpenIDConnectProviderThumbprint
--
--         , requestUpdateRole $
--             newUpdateRole
--
--         , requestUpdateRoleDescription $
--             newUpdateRoleDescription
--
--         , requestUpdateSAMLProvider $
--             newUpdateSAMLProvider
--
--         , requestUpdateSSHPublicKey $
--             newUpdateSSHPublicKey
--
--         , requestUpdateServerCertificate $
--             newUpdateServerCertificate
--
--         , requestUpdateServiceSpecificCredential $
--             newUpdateServiceSpecificCredential
--
--         , requestUpdateSigningCertificate $
--             newUpdateSigningCertificate
--
--         , requestUpdateUser $
--             newUpdateUser
--
--         , requestUploadSSHPublicKey $
--             newUploadSSHPublicKey
--
--         , requestUploadServerCertificate $
--             newUploadServerCertificate
--
--         , requestUploadSigningCertificate $
--             newUploadSigningCertificate
--
--           ]

--     , testGroup "response"
--         [ responseAddClientIDToOpenIDConnectProvider $
--             newAddClientIDToOpenIDConnectProviderResponse
--
--         , responseAddRoleToInstanceProfile $
--             newAddRoleToInstanceProfileResponse
--
--         , responseAddUserToGroup $
--             newAddUserToGroupResponse
--
--         , responseAttachGroupPolicy $
--             newAttachGroupPolicyResponse
--
--         , responseAttachRolePolicy $
--             newAttachRolePolicyResponse
--
--         , responseAttachUserPolicy $
--             newAttachUserPolicyResponse
--
--         , responseChangePassword $
--             newChangePasswordResponse
--
--         , responseCreateAccessKey $
--             newCreateAccessKeyResponse
--
--         , responseCreateAccountAlias $
--             newCreateAccountAliasResponse
--
--         , responseCreateGroup $
--             newCreateGroupResponse
--
--         , responseCreateInstanceProfile $
--             newCreateInstanceProfileResponse
--
--         , responseCreateLoginProfile $
--             newCreateLoginProfileResponse
--
--         , responseCreateOpenIDConnectProvider $
--             newCreateOpenIDConnectProviderResponse
--
--         , responseCreatePolicy $
--             newCreatePolicyResponse
--
--         , responseCreatePolicyVersion $
--             newCreatePolicyVersionResponse
--
--         , responseCreateRole $
--             newCreateRoleResponse
--
--         , responseCreateSAMLProvider $
--             newCreateSAMLProviderResponse
--
--         , responseCreateServiceLinkedRole $
--             newCreateServiceLinkedRoleResponse
--
--         , responseCreateServiceSpecificCredential $
--             newCreateServiceSpecificCredentialResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseCreateVirtualMFADevice $
--             newCreateVirtualMFADeviceResponse
--
--         , responseDeactivateMFADevice $
--             newDeactivateMFADeviceResponse
--
--         , responseDeleteAccessKey $
--             newDeleteAccessKeyResponse
--
--         , responseDeleteAccountAlias $
--             newDeleteAccountAliasResponse
--
--         , responseDeleteAccountPasswordPolicy $
--             newDeleteAccountPasswordPolicyResponse
--
--         , responseDeleteGroup $
--             newDeleteGroupResponse
--
--         , responseDeleteGroupPolicy $
--             newDeleteGroupPolicyResponse
--
--         , responseDeleteInstanceProfile $
--             newDeleteInstanceProfileResponse
--
--         , responseDeleteLoginProfile $
--             newDeleteLoginProfileResponse
--
--         , responseDeleteOpenIDConnectProvider $
--             newDeleteOpenIDConnectProviderResponse
--
--         , responseDeletePolicy $
--             newDeletePolicyResponse
--
--         , responseDeletePolicyVersion $
--             newDeletePolicyVersionResponse
--
--         , responseDeleteRole $
--             newDeleteRoleResponse
--
--         , responseDeleteRolePermissionsBoundary $
--             newDeleteRolePermissionsBoundaryResponse
--
--         , responseDeleteRolePolicy $
--             newDeleteRolePolicyResponse
--
--         , responseDeleteSAMLProvider $
--             newDeleteSAMLProviderResponse
--
--         , responseDeleteSSHPublicKey $
--             newDeleteSSHPublicKeyResponse
--
--         , responseDeleteServerCertificate $
--             newDeleteServerCertificateResponse
--
--         , responseDeleteServiceLinkedRole $
--             newDeleteServiceLinkedRoleResponse
--
--         , responseDeleteServiceSpecificCredential $
--             newDeleteServiceSpecificCredentialResponse
--
--         , responseDeleteSigningCertificate $
--             newDeleteSigningCertificateResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseDeleteUserPermissionsBoundary $
--             newDeleteUserPermissionsBoundaryResponse
--
--         , responseDeleteUserPolicy $
--             newDeleteUserPolicyResponse
--
--         , responseDeleteVirtualMFADevice $
--             newDeleteVirtualMFADeviceResponse
--
--         , responseDetachGroupPolicy $
--             newDetachGroupPolicyResponse
--
--         , responseDetachRolePolicy $
--             newDetachRolePolicyResponse
--
--         , responseDetachUserPolicy $
--             newDetachUserPolicyResponse
--
--         , responseEnableMFADevice $
--             newEnableMFADeviceResponse
--
--         , responseGenerateCredentialReport $
--             newGenerateCredentialReportResponse
--
--         , responseGenerateOrganizationsAccessReport $
--             newGenerateOrganizationsAccessReportResponse
--
--         , responseGenerateServiceLastAccessedDetails $
--             newGenerateServiceLastAccessedDetailsResponse
--
--         , responseGetAccessKeyLastUsed $
--             newGetAccessKeyLastUsedResponse
--
--         , responseGetAccountAuthorizationDetails $
--             newGetAccountAuthorizationDetailsResponse
--
--         , responseGetAccountPasswordPolicy $
--             newGetAccountPasswordPolicyResponse
--
--         , responseGetAccountSummary $
--             newGetAccountSummaryResponse
--
--         , responseGetContextKeysForCustomPolicy $
--             newGetContextKeysForPolicyResponse
--
--         , responseGetContextKeysForPrincipalPolicy $
--             newGetContextKeysForPolicyResponse
--
--         , responseGetCredentialReport $
--             newGetCredentialReportResponse
--
--         , responseGetGroup $
--             newGetGroupResponse
--
--         , responseGetGroupPolicy $
--             newGetGroupPolicyResponse
--
--         , responseGetInstanceProfile $
--             newGetInstanceProfileResponse
--
--         , responseGetLoginProfile $
--             newGetLoginProfileResponse
--
--         , responseGetOpenIDConnectProvider $
--             newGetOpenIDConnectProviderResponse
--
--         , responseGetOrganizationsAccessReport $
--             newGetOrganizationsAccessReportResponse
--
--         , responseGetPolicy $
--             newGetPolicyResponse
--
--         , responseGetPolicyVersion $
--             newGetPolicyVersionResponse
--
--         , responseGetRole $
--             newGetRoleResponse
--
--         , responseGetRolePolicy $
--             newGetRolePolicyResponse
--
--         , responseGetSAMLProvider $
--             newGetSAMLProviderResponse
--
--         , responseGetSSHPublicKey $
--             newGetSSHPublicKeyResponse
--
--         , responseGetServerCertificate $
--             newGetServerCertificateResponse
--
--         , responseGetServiceLastAccessedDetails $
--             newGetServiceLastAccessedDetailsResponse
--
--         , responseGetServiceLastAccessedDetailsWithEntities $
--             newGetServiceLastAccessedDetailsWithEntitiesResponse
--
--         , responseGetServiceLinkedRoleDeletionStatus $
--             newGetServiceLinkedRoleDeletionStatusResponse
--
--         , responseGetUser $
--             newGetUserResponse
--
--         , responseGetUserPolicy $
--             newGetUserPolicyResponse
--
--         , responseListAccessKeys $
--             newListAccessKeysResponse
--
--         , responseListAccountAliases $
--             newListAccountAliasesResponse
--
--         , responseListAttachedGroupPolicies $
--             newListAttachedGroupPoliciesResponse
--
--         , responseListAttachedRolePolicies $
--             newListAttachedRolePoliciesResponse
--
--         , responseListAttachedUserPolicies $
--             newListAttachedUserPoliciesResponse
--
--         , responseListEntitiesForPolicy $
--             newListEntitiesForPolicyResponse
--
--         , responseListGroupPolicies $
--             newListGroupPoliciesResponse
--
--         , responseListGroups $
--             newListGroupsResponse
--
--         , responseListGroupsForUser $
--             newListGroupsForUserResponse
--
--         , responseListInstanceProfileTags $
--             newListInstanceProfileTagsResponse
--
--         , responseListInstanceProfiles $
--             newListInstanceProfilesResponse
--
--         , responseListInstanceProfilesForRole $
--             newListInstanceProfilesForRoleResponse
--
--         , responseListMFADeviceTags $
--             newListMFADeviceTagsResponse
--
--         , responseListMFADevices $
--             newListMFADevicesResponse
--
--         , responseListOpenIDConnectProviderTags $
--             newListOpenIDConnectProviderTagsResponse
--
--         , responseListOpenIDConnectProviders $
--             newListOpenIDConnectProvidersResponse
--
--         , responseListPolicies $
--             newListPoliciesResponse
--
--         , responseListPoliciesGrantingServiceAccess $
--             newListPoliciesGrantingServiceAccessResponse
--
--         , responseListPolicyTags $
--             newListPolicyTagsResponse
--
--         , responseListPolicyVersions $
--             newListPolicyVersionsResponse
--
--         , responseListRolePolicies $
--             newListRolePoliciesResponse
--
--         , responseListRoleTags $
--             newListRoleTagsResponse
--
--         , responseListRoles $
--             newListRolesResponse
--
--         , responseListSAMLProviderTags $
--             newListSAMLProviderTagsResponse
--
--         , responseListSAMLProviders $
--             newListSAMLProvidersResponse
--
--         , responseListSSHPublicKeys $
--             newListSSHPublicKeysResponse
--
--         , responseListServerCertificateTags $
--             newListServerCertificateTagsResponse
--
--         , responseListServerCertificates $
--             newListServerCertificatesResponse
--
--         , responseListServiceSpecificCredentials $
--             newListServiceSpecificCredentialsResponse
--
--         , responseListSigningCertificates $
--             newListSigningCertificatesResponse
--
--         , responseListUserPolicies $
--             newListUserPoliciesResponse
--
--         , responseListUserTags $
--             newListUserTagsResponse
--
--         , responseListUsers $
--             newListUsersResponse
--
--         , responseListVirtualMFADevices $
--             newListVirtualMFADevicesResponse
--
--         , responsePutGroupPolicy $
--             newPutGroupPolicyResponse
--
--         , responsePutRolePermissionsBoundary $
--             newPutRolePermissionsBoundaryResponse
--
--         , responsePutRolePolicy $
--             newPutRolePolicyResponse
--
--         , responsePutUserPermissionsBoundary $
--             newPutUserPermissionsBoundaryResponse
--
--         , responsePutUserPolicy $
--             newPutUserPolicyResponse
--
--         , responseRemoveClientIDFromOpenIDConnectProvider $
--             newRemoveClientIDFromOpenIDConnectProviderResponse
--
--         , responseRemoveRoleFromInstanceProfile $
--             newRemoveRoleFromInstanceProfileResponse
--
--         , responseRemoveUserFromGroup $
--             newRemoveUserFromGroupResponse
--
--         , responseResetServiceSpecificCredential $
--             newResetServiceSpecificCredentialResponse
--
--         , responseResyncMFADevice $
--             newResyncMFADeviceResponse
--
--         , responseSetDefaultPolicyVersion $
--             newSetDefaultPolicyVersionResponse
--
--         , responseSetSecurityTokenServicePreferences $
--             newSetSecurityTokenServicePreferencesResponse
--
--         , responseSimulateCustomPolicy $
--             newSimulatePolicyResponse
--
--         , responseSimulatePrincipalPolicy $
--             newSimulatePolicyResponse
--
--         , responseTagInstanceProfile $
--             newTagInstanceProfileResponse
--
--         , responseTagMFADevice $
--             newTagMFADeviceResponse
--
--         , responseTagOpenIDConnectProvider $
--             newTagOpenIDConnectProviderResponse
--
--         , responseTagPolicy $
--             newTagPolicyResponse
--
--         , responseTagRole $
--             newTagRoleResponse
--
--         , responseTagSAMLProvider $
--             newTagSAMLProviderResponse
--
--         , responseTagServerCertificate $
--             newTagServerCertificateResponse
--
--         , responseTagUser $
--             newTagUserResponse
--
--         , responseUntagInstanceProfile $
--             newUntagInstanceProfileResponse
--
--         , responseUntagMFADevice $
--             newUntagMFADeviceResponse
--
--         , responseUntagOpenIDConnectProvider $
--             newUntagOpenIDConnectProviderResponse
--
--         , responseUntagPolicy $
--             newUntagPolicyResponse
--
--         , responseUntagRole $
--             newUntagRoleResponse
--
--         , responseUntagSAMLProvider $
--             newUntagSAMLProviderResponse
--
--         , responseUntagServerCertificate $
--             newUntagServerCertificateResponse
--
--         , responseUntagUser $
--             newUntagUserResponse
--
--         , responseUpdateAccessKey $
--             newUpdateAccessKeyResponse
--
--         , responseUpdateAccountPasswordPolicy $
--             newUpdateAccountPasswordPolicyResponse
--
--         , responseUpdateAssumeRolePolicy $
--             newUpdateAssumeRolePolicyResponse
--
--         , responseUpdateGroup $
--             newUpdateGroupResponse
--
--         , responseUpdateLoginProfile $
--             newUpdateLoginProfileResponse
--
--         , responseUpdateOpenIDConnectProviderThumbprint $
--             newUpdateOpenIDConnectProviderThumbprintResponse
--
--         , responseUpdateRole $
--             newUpdateRoleResponse
--
--         , responseUpdateRoleDescription $
--             newUpdateRoleDescriptionResponse
--
--         , responseUpdateSAMLProvider $
--             newUpdateSAMLProviderResponse
--
--         , responseUpdateSSHPublicKey $
--             newUpdateSSHPublicKeyResponse
--
--         , responseUpdateServerCertificate $
--             newUpdateServerCertificateResponse
--
--         , responseUpdateServiceSpecificCredential $
--             newUpdateServiceSpecificCredentialResponse
--
--         , responseUpdateSigningCertificate $
--             newUpdateSigningCertificateResponse
--
--         , responseUpdateUser $
--             newUpdateUserResponse
--
--         , responseUploadSSHPublicKey $
--             newUploadSSHPublicKeyResponse
--
--         , responseUploadServerCertificate $
--             newUploadServerCertificateResponse
--
--         , responseUploadSigningCertificate $
--             newUploadSigningCertificateResponse
--
--           ]
--     ]

-- Requests

requestAddClientIDToOpenIDConnectProvider :: AddClientIDToOpenIDConnectProvider -> TestTree
requestAddClientIDToOpenIDConnectProvider =
  req
    "AddClientIDToOpenIDConnectProvider"
    "fixture/AddClientIDToOpenIDConnectProvider.yaml"

requestAddRoleToInstanceProfile :: AddRoleToInstanceProfile -> TestTree
requestAddRoleToInstanceProfile =
  req
    "AddRoleToInstanceProfile"
    "fixture/AddRoleToInstanceProfile.yaml"

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

requestAttachRolePolicy :: AttachRolePolicy -> TestTree
requestAttachRolePolicy =
  req
    "AttachRolePolicy"
    "fixture/AttachRolePolicy.yaml"

requestAttachUserPolicy :: AttachUserPolicy -> TestTree
requestAttachUserPolicy =
  req
    "AttachUserPolicy"
    "fixture/AttachUserPolicy.yaml"

requestChangePassword :: ChangePassword -> TestTree
requestChangePassword =
  req
    "ChangePassword"
    "fixture/ChangePassword.yaml"

requestCreateAccessKey :: CreateAccessKey -> TestTree
requestCreateAccessKey =
  req
    "CreateAccessKey"
    "fixture/CreateAccessKey.yaml"

requestCreateAccountAlias :: CreateAccountAlias -> TestTree
requestCreateAccountAlias =
  req
    "CreateAccountAlias"
    "fixture/CreateAccountAlias.yaml"

requestCreateGroup :: CreateGroup -> TestTree
requestCreateGroup =
  req
    "CreateGroup"
    "fixture/CreateGroup.yaml"

requestCreateInstanceProfile :: CreateInstanceProfile -> TestTree
requestCreateInstanceProfile =
  req
    "CreateInstanceProfile"
    "fixture/CreateInstanceProfile.yaml"

requestCreateLoginProfile :: CreateLoginProfile -> TestTree
requestCreateLoginProfile =
  req
    "CreateLoginProfile"
    "fixture/CreateLoginProfile.yaml"

requestCreateOpenIDConnectProvider :: CreateOpenIDConnectProvider -> TestTree
requestCreateOpenIDConnectProvider =
  req
    "CreateOpenIDConnectProvider"
    "fixture/CreateOpenIDConnectProvider.yaml"

requestCreatePolicy :: CreatePolicy -> TestTree
requestCreatePolicy =
  req
    "CreatePolicy"
    "fixture/CreatePolicy.yaml"

requestCreatePolicyVersion :: CreatePolicyVersion -> TestTree
requestCreatePolicyVersion =
  req
    "CreatePolicyVersion"
    "fixture/CreatePolicyVersion.yaml"

requestCreateRole :: CreateRole -> TestTree
requestCreateRole =
  req
    "CreateRole"
    "fixture/CreateRole.yaml"

requestCreateSAMLProvider :: CreateSAMLProvider -> TestTree
requestCreateSAMLProvider =
  req
    "CreateSAMLProvider"
    "fixture/CreateSAMLProvider.yaml"

requestCreateServiceLinkedRole :: CreateServiceLinkedRole -> TestTree
requestCreateServiceLinkedRole =
  req
    "CreateServiceLinkedRole"
    "fixture/CreateServiceLinkedRole.yaml"

requestCreateServiceSpecificCredential :: CreateServiceSpecificCredential -> TestTree
requestCreateServiceSpecificCredential =
  req
    "CreateServiceSpecificCredential"
    "fixture/CreateServiceSpecificCredential.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestCreateVirtualMFADevice :: CreateVirtualMFADevice -> TestTree
requestCreateVirtualMFADevice =
  req
    "CreateVirtualMFADevice"
    "fixture/CreateVirtualMFADevice.yaml"

requestDeactivateMFADevice :: DeactivateMFADevice -> TestTree
requestDeactivateMFADevice =
  req
    "DeactivateMFADevice"
    "fixture/DeactivateMFADevice.yaml"

requestDeleteAccessKey :: DeleteAccessKey -> TestTree
requestDeleteAccessKey =
  req
    "DeleteAccessKey"
    "fixture/DeleteAccessKey.yaml"

requestDeleteAccountAlias :: DeleteAccountAlias -> TestTree
requestDeleteAccountAlias =
  req
    "DeleteAccountAlias"
    "fixture/DeleteAccountAlias.yaml"

requestDeleteAccountPasswordPolicy :: DeleteAccountPasswordPolicy -> TestTree
requestDeleteAccountPasswordPolicy =
  req
    "DeleteAccountPasswordPolicy"
    "fixture/DeleteAccountPasswordPolicy.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup =
  req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

requestDeleteGroupPolicy :: DeleteGroupPolicy -> TestTree
requestDeleteGroupPolicy =
  req
    "DeleteGroupPolicy"
    "fixture/DeleteGroupPolicy.yaml"

requestDeleteInstanceProfile :: DeleteInstanceProfile -> TestTree
requestDeleteInstanceProfile =
  req
    "DeleteInstanceProfile"
    "fixture/DeleteInstanceProfile.yaml"

requestDeleteLoginProfile :: DeleteLoginProfile -> TestTree
requestDeleteLoginProfile =
  req
    "DeleteLoginProfile"
    "fixture/DeleteLoginProfile.yaml"

requestDeleteOpenIDConnectProvider :: DeleteOpenIDConnectProvider -> TestTree
requestDeleteOpenIDConnectProvider =
  req
    "DeleteOpenIDConnectProvider"
    "fixture/DeleteOpenIDConnectProvider.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy =
  req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

requestDeletePolicyVersion :: DeletePolicyVersion -> TestTree
requestDeletePolicyVersion =
  req
    "DeletePolicyVersion"
    "fixture/DeletePolicyVersion.yaml"

requestDeleteRole :: DeleteRole -> TestTree
requestDeleteRole =
  req
    "DeleteRole"
    "fixture/DeleteRole.yaml"

requestDeleteRolePermissionsBoundary :: DeleteRolePermissionsBoundary -> TestTree
requestDeleteRolePermissionsBoundary =
  req
    "DeleteRolePermissionsBoundary"
    "fixture/DeleteRolePermissionsBoundary.yaml"

requestDeleteRolePolicy :: DeleteRolePolicy -> TestTree
requestDeleteRolePolicy =
  req
    "DeleteRolePolicy"
    "fixture/DeleteRolePolicy.yaml"

requestDeleteSAMLProvider :: DeleteSAMLProvider -> TestTree
requestDeleteSAMLProvider =
  req
    "DeleteSAMLProvider"
    "fixture/DeleteSAMLProvider.yaml"

requestDeleteSSHPublicKey :: DeleteSSHPublicKey -> TestTree
requestDeleteSSHPublicKey =
  req
    "DeleteSSHPublicKey"
    "fixture/DeleteSSHPublicKey.yaml"

requestDeleteServerCertificate :: DeleteServerCertificate -> TestTree
requestDeleteServerCertificate =
  req
    "DeleteServerCertificate"
    "fixture/DeleteServerCertificate.yaml"

requestDeleteServiceLinkedRole :: DeleteServiceLinkedRole -> TestTree
requestDeleteServiceLinkedRole =
  req
    "DeleteServiceLinkedRole"
    "fixture/DeleteServiceLinkedRole.yaml"

requestDeleteServiceSpecificCredential :: DeleteServiceSpecificCredential -> TestTree
requestDeleteServiceSpecificCredential =
  req
    "DeleteServiceSpecificCredential"
    "fixture/DeleteServiceSpecificCredential.yaml"

requestDeleteSigningCertificate :: DeleteSigningCertificate -> TestTree
requestDeleteSigningCertificate =
  req
    "DeleteSigningCertificate"
    "fixture/DeleteSigningCertificate.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestDeleteUserPermissionsBoundary :: DeleteUserPermissionsBoundary -> TestTree
requestDeleteUserPermissionsBoundary =
  req
    "DeleteUserPermissionsBoundary"
    "fixture/DeleteUserPermissionsBoundary.yaml"

requestDeleteUserPolicy :: DeleteUserPolicy -> TestTree
requestDeleteUserPolicy =
  req
    "DeleteUserPolicy"
    "fixture/DeleteUserPolicy.yaml"

requestDeleteVirtualMFADevice :: DeleteVirtualMFADevice -> TestTree
requestDeleteVirtualMFADevice =
  req
    "DeleteVirtualMFADevice"
    "fixture/DeleteVirtualMFADevice.yaml"

requestDetachGroupPolicy :: DetachGroupPolicy -> TestTree
requestDetachGroupPolicy =
  req
    "DetachGroupPolicy"
    "fixture/DetachGroupPolicy.yaml"

requestDetachRolePolicy :: DetachRolePolicy -> TestTree
requestDetachRolePolicy =
  req
    "DetachRolePolicy"
    "fixture/DetachRolePolicy.yaml"

requestDetachUserPolicy :: DetachUserPolicy -> TestTree
requestDetachUserPolicy =
  req
    "DetachUserPolicy"
    "fixture/DetachUserPolicy.yaml"

requestEnableMFADevice :: EnableMFADevice -> TestTree
requestEnableMFADevice =
  req
    "EnableMFADevice"
    "fixture/EnableMFADevice.yaml"

requestGenerateCredentialReport :: GenerateCredentialReport -> TestTree
requestGenerateCredentialReport =
  req
    "GenerateCredentialReport"
    "fixture/GenerateCredentialReport.yaml"

requestGenerateOrganizationsAccessReport :: GenerateOrganizationsAccessReport -> TestTree
requestGenerateOrganizationsAccessReport =
  req
    "GenerateOrganizationsAccessReport"
    "fixture/GenerateOrganizationsAccessReport.yaml"

requestGenerateServiceLastAccessedDetails :: GenerateServiceLastAccessedDetails -> TestTree
requestGenerateServiceLastAccessedDetails =
  req
    "GenerateServiceLastAccessedDetails"
    "fixture/GenerateServiceLastAccessedDetails.yaml"

requestGetAccessKeyLastUsed :: GetAccessKeyLastUsed -> TestTree
requestGetAccessKeyLastUsed =
  req
    "GetAccessKeyLastUsed"
    "fixture/GetAccessKeyLastUsed.yaml"

requestGetAccountAuthorizationDetails :: GetAccountAuthorizationDetails -> TestTree
requestGetAccountAuthorizationDetails =
  req
    "GetAccountAuthorizationDetails"
    "fixture/GetAccountAuthorizationDetails.yaml"

requestGetAccountPasswordPolicy :: GetAccountPasswordPolicy -> TestTree
requestGetAccountPasswordPolicy =
  req
    "GetAccountPasswordPolicy"
    "fixture/GetAccountPasswordPolicy.yaml"

requestGetAccountSummary :: GetAccountSummary -> TestTree
requestGetAccountSummary =
  req
    "GetAccountSummary"
    "fixture/GetAccountSummary.yaml"

requestGetContextKeysForCustomPolicy :: GetContextKeysForCustomPolicy -> TestTree
requestGetContextKeysForCustomPolicy =
  req
    "GetContextKeysForCustomPolicy"
    "fixture/GetContextKeysForCustomPolicy.yaml"

requestGetContextKeysForPrincipalPolicy :: GetContextKeysForPrincipalPolicy -> TestTree
requestGetContextKeysForPrincipalPolicy =
  req
    "GetContextKeysForPrincipalPolicy"
    "fixture/GetContextKeysForPrincipalPolicy.yaml"

requestGetCredentialReport :: GetCredentialReport -> TestTree
requestGetCredentialReport =
  req
    "GetCredentialReport"
    "fixture/GetCredentialReport.yaml"

requestGetGroup :: GetGroup -> TestTree
requestGetGroup =
  req
    "GetGroup"
    "fixture/GetGroup.yaml"

requestGetGroupPolicy :: GetGroupPolicy -> TestTree
requestGetGroupPolicy =
  req
    "GetGroupPolicy"
    "fixture/GetGroupPolicy.yaml"

requestGetInstanceProfile :: GetInstanceProfile -> TestTree
requestGetInstanceProfile =
  req
    "GetInstanceProfile"
    "fixture/GetInstanceProfile.yaml"

requestGetLoginProfile :: GetLoginProfile -> TestTree
requestGetLoginProfile =
  req
    "GetLoginProfile"
    "fixture/GetLoginProfile.yaml"

requestGetOpenIDConnectProvider :: GetOpenIDConnectProvider -> TestTree
requestGetOpenIDConnectProvider =
  req
    "GetOpenIDConnectProvider"
    "fixture/GetOpenIDConnectProvider.yaml"

requestGetOrganizationsAccessReport :: GetOrganizationsAccessReport -> TestTree
requestGetOrganizationsAccessReport =
  req
    "GetOrganizationsAccessReport"
    "fixture/GetOrganizationsAccessReport.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy =
  req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

requestGetPolicyVersion :: GetPolicyVersion -> TestTree
requestGetPolicyVersion =
  req
    "GetPolicyVersion"
    "fixture/GetPolicyVersion.yaml"

requestGetRole :: GetRole -> TestTree
requestGetRole =
  req
    "GetRole"
    "fixture/GetRole.yaml"

requestGetRolePolicy :: GetRolePolicy -> TestTree
requestGetRolePolicy =
  req
    "GetRolePolicy"
    "fixture/GetRolePolicy.yaml"

requestGetSAMLProvider :: GetSAMLProvider -> TestTree
requestGetSAMLProvider =
  req
    "GetSAMLProvider"
    "fixture/GetSAMLProvider.yaml"

requestGetSSHPublicKey :: GetSSHPublicKey -> TestTree
requestGetSSHPublicKey =
  req
    "GetSSHPublicKey"
    "fixture/GetSSHPublicKey.yaml"

requestGetServerCertificate :: GetServerCertificate -> TestTree
requestGetServerCertificate =
  req
    "GetServerCertificate"
    "fixture/GetServerCertificate.yaml"

requestGetServiceLastAccessedDetails :: GetServiceLastAccessedDetails -> TestTree
requestGetServiceLastAccessedDetails =
  req
    "GetServiceLastAccessedDetails"
    "fixture/GetServiceLastAccessedDetails.yaml"

requestGetServiceLastAccessedDetailsWithEntities :: GetServiceLastAccessedDetailsWithEntities -> TestTree
requestGetServiceLastAccessedDetailsWithEntities =
  req
    "GetServiceLastAccessedDetailsWithEntities"
    "fixture/GetServiceLastAccessedDetailsWithEntities.yaml"

requestGetServiceLinkedRoleDeletionStatus :: GetServiceLinkedRoleDeletionStatus -> TestTree
requestGetServiceLinkedRoleDeletionStatus =
  req
    "GetServiceLinkedRoleDeletionStatus"
    "fixture/GetServiceLinkedRoleDeletionStatus.yaml"

requestGetUser :: GetUser -> TestTree
requestGetUser =
  req
    "GetUser"
    "fixture/GetUser.yaml"

requestGetUserPolicy :: GetUserPolicy -> TestTree
requestGetUserPolicy =
  req
    "GetUserPolicy"
    "fixture/GetUserPolicy.yaml"

requestListAccessKeys :: ListAccessKeys -> TestTree
requestListAccessKeys =
  req
    "ListAccessKeys"
    "fixture/ListAccessKeys.yaml"

requestListAccountAliases :: ListAccountAliases -> TestTree
requestListAccountAliases =
  req
    "ListAccountAliases"
    "fixture/ListAccountAliases.yaml"

requestListAttachedGroupPolicies :: ListAttachedGroupPolicies -> TestTree
requestListAttachedGroupPolicies =
  req
    "ListAttachedGroupPolicies"
    "fixture/ListAttachedGroupPolicies.yaml"

requestListAttachedRolePolicies :: ListAttachedRolePolicies -> TestTree
requestListAttachedRolePolicies =
  req
    "ListAttachedRolePolicies"
    "fixture/ListAttachedRolePolicies.yaml"

requestListAttachedUserPolicies :: ListAttachedUserPolicies -> TestTree
requestListAttachedUserPolicies =
  req
    "ListAttachedUserPolicies"
    "fixture/ListAttachedUserPolicies.yaml"

requestListEntitiesForPolicy :: ListEntitiesForPolicy -> TestTree
requestListEntitiesForPolicy =
  req
    "ListEntitiesForPolicy"
    "fixture/ListEntitiesForPolicy.yaml"

requestListGroupPolicies :: ListGroupPolicies -> TestTree
requestListGroupPolicies =
  req
    "ListGroupPolicies"
    "fixture/ListGroupPolicies.yaml"

requestListGroups :: ListGroups -> TestTree
requestListGroups =
  req
    "ListGroups"
    "fixture/ListGroups.yaml"

requestListGroupsForUser :: ListGroupsForUser -> TestTree
requestListGroupsForUser =
  req
    "ListGroupsForUser"
    "fixture/ListGroupsForUser.yaml"

requestListInstanceProfileTags :: ListInstanceProfileTags -> TestTree
requestListInstanceProfileTags =
  req
    "ListInstanceProfileTags"
    "fixture/ListInstanceProfileTags.yaml"

requestListInstanceProfiles :: ListInstanceProfiles -> TestTree
requestListInstanceProfiles =
  req
    "ListInstanceProfiles"
    "fixture/ListInstanceProfiles.yaml"

requestListInstanceProfilesForRole :: ListInstanceProfilesForRole -> TestTree
requestListInstanceProfilesForRole =
  req
    "ListInstanceProfilesForRole"
    "fixture/ListInstanceProfilesForRole.yaml"

requestListMFADeviceTags :: ListMFADeviceTags -> TestTree
requestListMFADeviceTags =
  req
    "ListMFADeviceTags"
    "fixture/ListMFADeviceTags.yaml"

requestListMFADevices :: ListMFADevices -> TestTree
requestListMFADevices =
  req
    "ListMFADevices"
    "fixture/ListMFADevices.yaml"

requestListOpenIDConnectProviderTags :: ListOpenIDConnectProviderTags -> TestTree
requestListOpenIDConnectProviderTags =
  req
    "ListOpenIDConnectProviderTags"
    "fixture/ListOpenIDConnectProviderTags.yaml"

requestListOpenIDConnectProviders :: ListOpenIDConnectProviders -> TestTree
requestListOpenIDConnectProviders =
  req
    "ListOpenIDConnectProviders"
    "fixture/ListOpenIDConnectProviders.yaml"

requestListPolicies :: ListPolicies -> TestTree
requestListPolicies =
  req
    "ListPolicies"
    "fixture/ListPolicies.yaml"

requestListPoliciesGrantingServiceAccess :: ListPoliciesGrantingServiceAccess -> TestTree
requestListPoliciesGrantingServiceAccess =
  req
    "ListPoliciesGrantingServiceAccess"
    "fixture/ListPoliciesGrantingServiceAccess.yaml"

requestListPolicyTags :: ListPolicyTags -> TestTree
requestListPolicyTags =
  req
    "ListPolicyTags"
    "fixture/ListPolicyTags.yaml"

requestListPolicyVersions :: ListPolicyVersions -> TestTree
requestListPolicyVersions =
  req
    "ListPolicyVersions"
    "fixture/ListPolicyVersions.yaml"

requestListRolePolicies :: ListRolePolicies -> TestTree
requestListRolePolicies =
  req
    "ListRolePolicies"
    "fixture/ListRolePolicies.yaml"

requestListRoleTags :: ListRoleTags -> TestTree
requestListRoleTags =
  req
    "ListRoleTags"
    "fixture/ListRoleTags.yaml"

requestListRoles :: ListRoles -> TestTree
requestListRoles =
  req
    "ListRoles"
    "fixture/ListRoles.yaml"

requestListSAMLProviderTags :: ListSAMLProviderTags -> TestTree
requestListSAMLProviderTags =
  req
    "ListSAMLProviderTags"
    "fixture/ListSAMLProviderTags.yaml"

requestListSAMLProviders :: ListSAMLProviders -> TestTree
requestListSAMLProviders =
  req
    "ListSAMLProviders"
    "fixture/ListSAMLProviders.yaml"

requestListSSHPublicKeys :: ListSSHPublicKeys -> TestTree
requestListSSHPublicKeys =
  req
    "ListSSHPublicKeys"
    "fixture/ListSSHPublicKeys.yaml"

requestListServerCertificateTags :: ListServerCertificateTags -> TestTree
requestListServerCertificateTags =
  req
    "ListServerCertificateTags"
    "fixture/ListServerCertificateTags.yaml"

requestListServerCertificates :: ListServerCertificates -> TestTree
requestListServerCertificates =
  req
    "ListServerCertificates"
    "fixture/ListServerCertificates.yaml"

requestListServiceSpecificCredentials :: ListServiceSpecificCredentials -> TestTree
requestListServiceSpecificCredentials =
  req
    "ListServiceSpecificCredentials"
    "fixture/ListServiceSpecificCredentials.yaml"

requestListSigningCertificates :: ListSigningCertificates -> TestTree
requestListSigningCertificates =
  req
    "ListSigningCertificates"
    "fixture/ListSigningCertificates.yaml"

requestListUserPolicies :: ListUserPolicies -> TestTree
requestListUserPolicies =
  req
    "ListUserPolicies"
    "fixture/ListUserPolicies.yaml"

requestListUserTags :: ListUserTags -> TestTree
requestListUserTags =
  req
    "ListUserTags"
    "fixture/ListUserTags.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers =
  req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestListVirtualMFADevices :: ListVirtualMFADevices -> TestTree
requestListVirtualMFADevices =
  req
    "ListVirtualMFADevices"
    "fixture/ListVirtualMFADevices.yaml"

requestPutGroupPolicy :: PutGroupPolicy -> TestTree
requestPutGroupPolicy =
  req
    "PutGroupPolicy"
    "fixture/PutGroupPolicy.yaml"

requestPutRolePermissionsBoundary :: PutRolePermissionsBoundary -> TestTree
requestPutRolePermissionsBoundary =
  req
    "PutRolePermissionsBoundary"
    "fixture/PutRolePermissionsBoundary.yaml"

requestPutRolePolicy :: PutRolePolicy -> TestTree
requestPutRolePolicy =
  req
    "PutRolePolicy"
    "fixture/PutRolePolicy.yaml"

requestPutUserPermissionsBoundary :: PutUserPermissionsBoundary -> TestTree
requestPutUserPermissionsBoundary =
  req
    "PutUserPermissionsBoundary"
    "fixture/PutUserPermissionsBoundary.yaml"

requestPutUserPolicy :: PutUserPolicy -> TestTree
requestPutUserPolicy =
  req
    "PutUserPolicy"
    "fixture/PutUserPolicy.yaml"

requestRemoveClientIDFromOpenIDConnectProvider :: RemoveClientIDFromOpenIDConnectProvider -> TestTree
requestRemoveClientIDFromOpenIDConnectProvider =
  req
    "RemoveClientIDFromOpenIDConnectProvider"
    "fixture/RemoveClientIDFromOpenIDConnectProvider.yaml"

requestRemoveRoleFromInstanceProfile :: RemoveRoleFromInstanceProfile -> TestTree
requestRemoveRoleFromInstanceProfile =
  req
    "RemoveRoleFromInstanceProfile"
    "fixture/RemoveRoleFromInstanceProfile.yaml"

requestRemoveUserFromGroup :: RemoveUserFromGroup -> TestTree
requestRemoveUserFromGroup =
  req
    "RemoveUserFromGroup"
    "fixture/RemoveUserFromGroup.yaml"

requestResetServiceSpecificCredential :: ResetServiceSpecificCredential -> TestTree
requestResetServiceSpecificCredential =
  req
    "ResetServiceSpecificCredential"
    "fixture/ResetServiceSpecificCredential.yaml"

requestResyncMFADevice :: ResyncMFADevice -> TestTree
requestResyncMFADevice =
  req
    "ResyncMFADevice"
    "fixture/ResyncMFADevice.yaml"

requestSetDefaultPolicyVersion :: SetDefaultPolicyVersion -> TestTree
requestSetDefaultPolicyVersion =
  req
    "SetDefaultPolicyVersion"
    "fixture/SetDefaultPolicyVersion.yaml"

requestSetSecurityTokenServicePreferences :: SetSecurityTokenServicePreferences -> TestTree
requestSetSecurityTokenServicePreferences =
  req
    "SetSecurityTokenServicePreferences"
    "fixture/SetSecurityTokenServicePreferences.yaml"

requestSimulateCustomPolicy :: SimulateCustomPolicy -> TestTree
requestSimulateCustomPolicy =
  req
    "SimulateCustomPolicy"
    "fixture/SimulateCustomPolicy.yaml"

requestSimulatePrincipalPolicy :: SimulatePrincipalPolicy -> TestTree
requestSimulatePrincipalPolicy =
  req
    "SimulatePrincipalPolicy"
    "fixture/SimulatePrincipalPolicy.yaml"

requestTagInstanceProfile :: TagInstanceProfile -> TestTree
requestTagInstanceProfile =
  req
    "TagInstanceProfile"
    "fixture/TagInstanceProfile.yaml"

requestTagMFADevice :: TagMFADevice -> TestTree
requestTagMFADevice =
  req
    "TagMFADevice"
    "fixture/TagMFADevice.yaml"

requestTagOpenIDConnectProvider :: TagOpenIDConnectProvider -> TestTree
requestTagOpenIDConnectProvider =
  req
    "TagOpenIDConnectProvider"
    "fixture/TagOpenIDConnectProvider.yaml"

requestTagPolicy :: TagPolicy -> TestTree
requestTagPolicy =
  req
    "TagPolicy"
    "fixture/TagPolicy.yaml"

requestTagRole :: TagRole -> TestTree
requestTagRole =
  req
    "TagRole"
    "fixture/TagRole.yaml"

requestTagSAMLProvider :: TagSAMLProvider -> TestTree
requestTagSAMLProvider =
  req
    "TagSAMLProvider"
    "fixture/TagSAMLProvider.yaml"

requestTagServerCertificate :: TagServerCertificate -> TestTree
requestTagServerCertificate =
  req
    "TagServerCertificate"
    "fixture/TagServerCertificate.yaml"

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

requestUntagMFADevice :: UntagMFADevice -> TestTree
requestUntagMFADevice =
  req
    "UntagMFADevice"
    "fixture/UntagMFADevice.yaml"

requestUntagOpenIDConnectProvider :: UntagOpenIDConnectProvider -> TestTree
requestUntagOpenIDConnectProvider =
  req
    "UntagOpenIDConnectProvider"
    "fixture/UntagOpenIDConnectProvider.yaml"

requestUntagPolicy :: UntagPolicy -> TestTree
requestUntagPolicy =
  req
    "UntagPolicy"
    "fixture/UntagPolicy.yaml"

requestUntagRole :: UntagRole -> TestTree
requestUntagRole =
  req
    "UntagRole"
    "fixture/UntagRole.yaml"

requestUntagSAMLProvider :: UntagSAMLProvider -> TestTree
requestUntagSAMLProvider =
  req
    "UntagSAMLProvider"
    "fixture/UntagSAMLProvider.yaml"

requestUntagServerCertificate :: UntagServerCertificate -> TestTree
requestUntagServerCertificate =
  req
    "UntagServerCertificate"
    "fixture/UntagServerCertificate.yaml"

requestUntagUser :: UntagUser -> TestTree
requestUntagUser =
  req
    "UntagUser"
    "fixture/UntagUser.yaml"

requestUpdateAccessKey :: UpdateAccessKey -> TestTree
requestUpdateAccessKey =
  req
    "UpdateAccessKey"
    "fixture/UpdateAccessKey.yaml"

requestUpdateAccountPasswordPolicy :: UpdateAccountPasswordPolicy -> TestTree
requestUpdateAccountPasswordPolicy =
  req
    "UpdateAccountPasswordPolicy"
    "fixture/UpdateAccountPasswordPolicy.yaml"

requestUpdateAssumeRolePolicy :: UpdateAssumeRolePolicy -> TestTree
requestUpdateAssumeRolePolicy =
  req
    "UpdateAssumeRolePolicy"
    "fixture/UpdateAssumeRolePolicy.yaml"

requestUpdateGroup :: UpdateGroup -> TestTree
requestUpdateGroup =
  req
    "UpdateGroup"
    "fixture/UpdateGroup.yaml"

requestUpdateLoginProfile :: UpdateLoginProfile -> TestTree
requestUpdateLoginProfile =
  req
    "UpdateLoginProfile"
    "fixture/UpdateLoginProfile.yaml"

requestUpdateOpenIDConnectProviderThumbprint :: UpdateOpenIDConnectProviderThumbprint -> TestTree
requestUpdateOpenIDConnectProviderThumbprint =
  req
    "UpdateOpenIDConnectProviderThumbprint"
    "fixture/UpdateOpenIDConnectProviderThumbprint.yaml"

requestUpdateRole :: UpdateRole -> TestTree
requestUpdateRole =
  req
    "UpdateRole"
    "fixture/UpdateRole.yaml"

requestUpdateRoleDescription :: UpdateRoleDescription -> TestTree
requestUpdateRoleDescription =
  req
    "UpdateRoleDescription"
    "fixture/UpdateRoleDescription.yaml"

requestUpdateSAMLProvider :: UpdateSAMLProvider -> TestTree
requestUpdateSAMLProvider =
  req
    "UpdateSAMLProvider"
    "fixture/UpdateSAMLProvider.yaml"

requestUpdateSSHPublicKey :: UpdateSSHPublicKey -> TestTree
requestUpdateSSHPublicKey =
  req
    "UpdateSSHPublicKey"
    "fixture/UpdateSSHPublicKey.yaml"

requestUpdateServerCertificate :: UpdateServerCertificate -> TestTree
requestUpdateServerCertificate =
  req
    "UpdateServerCertificate"
    "fixture/UpdateServerCertificate.yaml"

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

requestUpdateUser :: UpdateUser -> TestTree
requestUpdateUser =
  req
    "UpdateUser"
    "fixture/UpdateUser.yaml"

requestUploadSSHPublicKey :: UploadSSHPublicKey -> TestTree
requestUploadSSHPublicKey =
  req
    "UploadSSHPublicKey"
    "fixture/UploadSSHPublicKey.yaml"

requestUploadServerCertificate :: UploadServerCertificate -> TestTree
requestUploadServerCertificate =
  req
    "UploadServerCertificate"
    "fixture/UploadServerCertificate.yaml"

requestUploadSigningCertificate :: UploadSigningCertificate -> TestTree
requestUploadSigningCertificate =
  req
    "UploadSigningCertificate"
    "fixture/UploadSigningCertificate.yaml"

-- Responses

responseAddClientIDToOpenIDConnectProvider :: AddClientIDToOpenIDConnectProviderResponse -> TestTree
responseAddClientIDToOpenIDConnectProvider =
  res
    "AddClientIDToOpenIDConnectProviderResponse"
    "fixture/AddClientIDToOpenIDConnectProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddClientIDToOpenIDConnectProvider)

responseAddRoleToInstanceProfile :: AddRoleToInstanceProfileResponse -> TestTree
responseAddRoleToInstanceProfile =
  res
    "AddRoleToInstanceProfileResponse"
    "fixture/AddRoleToInstanceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddRoleToInstanceProfile)

responseAddUserToGroup :: AddUserToGroupResponse -> TestTree
responseAddUserToGroup =
  res
    "AddUserToGroupResponse"
    "fixture/AddUserToGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddUserToGroup)

responseAttachGroupPolicy :: AttachGroupPolicyResponse -> TestTree
responseAttachGroupPolicy =
  res
    "AttachGroupPolicyResponse"
    "fixture/AttachGroupPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachGroupPolicy)

responseAttachRolePolicy :: AttachRolePolicyResponse -> TestTree
responseAttachRolePolicy =
  res
    "AttachRolePolicyResponse"
    "fixture/AttachRolePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachRolePolicy)

responseAttachUserPolicy :: AttachUserPolicyResponse -> TestTree
responseAttachUserPolicy =
  res
    "AttachUserPolicyResponse"
    "fixture/AttachUserPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachUserPolicy)

responseChangePassword :: ChangePasswordResponse -> TestTree
responseChangePassword =
  res
    "ChangePasswordResponse"
    "fixture/ChangePasswordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ChangePassword)

responseCreateAccessKey :: CreateAccessKeyResponse -> TestTree
responseCreateAccessKey =
  res
    "CreateAccessKeyResponse"
    "fixture/CreateAccessKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAccessKey)

responseCreateAccountAlias :: CreateAccountAliasResponse -> TestTree
responseCreateAccountAlias =
  res
    "CreateAccountAliasResponse"
    "fixture/CreateAccountAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAccountAlias)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup =
  res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGroup)

responseCreateInstanceProfile :: CreateInstanceProfileResponse -> TestTree
responseCreateInstanceProfile =
  res
    "CreateInstanceProfileResponse"
    "fixture/CreateInstanceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInstanceProfile)

responseCreateLoginProfile :: CreateLoginProfileResponse -> TestTree
responseCreateLoginProfile =
  res
    "CreateLoginProfileResponse"
    "fixture/CreateLoginProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLoginProfile)

responseCreateOpenIDConnectProvider :: CreateOpenIDConnectProviderResponse -> TestTree
responseCreateOpenIDConnectProvider =
  res
    "CreateOpenIDConnectProviderResponse"
    "fixture/CreateOpenIDConnectProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateOpenIDConnectProvider)

responseCreatePolicy :: CreatePolicyResponse -> TestTree
responseCreatePolicy =
  res
    "CreatePolicyResponse"
    "fixture/CreatePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePolicy)

responseCreatePolicyVersion :: CreatePolicyVersionResponse -> TestTree
responseCreatePolicyVersion =
  res
    "CreatePolicyVersionResponse"
    "fixture/CreatePolicyVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePolicyVersion)

responseCreateRole :: CreateRoleResponse -> TestTree
responseCreateRole =
  res
    "CreateRoleResponse"
    "fixture/CreateRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRole)

responseCreateSAMLProvider :: CreateSAMLProviderResponse -> TestTree
responseCreateSAMLProvider =
  res
    "CreateSAMLProviderResponse"
    "fixture/CreateSAMLProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSAMLProvider)

responseCreateServiceLinkedRole :: CreateServiceLinkedRoleResponse -> TestTree
responseCreateServiceLinkedRole =
  res
    "CreateServiceLinkedRoleResponse"
    "fixture/CreateServiceLinkedRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateServiceLinkedRole)

responseCreateServiceSpecificCredential :: CreateServiceSpecificCredentialResponse -> TestTree
responseCreateServiceSpecificCredential =
  res
    "CreateServiceSpecificCredentialResponse"
    "fixture/CreateServiceSpecificCredentialResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateServiceSpecificCredential)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUser)

responseCreateVirtualMFADevice :: CreateVirtualMFADeviceResponse -> TestTree
responseCreateVirtualMFADevice =
  res
    "CreateVirtualMFADeviceResponse"
    "fixture/CreateVirtualMFADeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVirtualMFADevice)

responseDeactivateMFADevice :: DeactivateMFADeviceResponse -> TestTree
responseDeactivateMFADevice =
  res
    "DeactivateMFADeviceResponse"
    "fixture/DeactivateMFADeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeactivateMFADevice)

responseDeleteAccessKey :: DeleteAccessKeyResponse -> TestTree
responseDeleteAccessKey =
  res
    "DeleteAccessKeyResponse"
    "fixture/DeleteAccessKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccessKey)

responseDeleteAccountAlias :: DeleteAccountAliasResponse -> TestTree
responseDeleteAccountAlias =
  res
    "DeleteAccountAliasResponse"
    "fixture/DeleteAccountAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccountAlias)

responseDeleteAccountPasswordPolicy :: DeleteAccountPasswordPolicyResponse -> TestTree
responseDeleteAccountPasswordPolicy =
  res
    "DeleteAccountPasswordPolicyResponse"
    "fixture/DeleteAccountPasswordPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccountPasswordPolicy)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGroup)

responseDeleteGroupPolicy :: DeleteGroupPolicyResponse -> TestTree
responseDeleteGroupPolicy =
  res
    "DeleteGroupPolicyResponse"
    "fixture/DeleteGroupPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGroupPolicy)

responseDeleteInstanceProfile :: DeleteInstanceProfileResponse -> TestTree
responseDeleteInstanceProfile =
  res
    "DeleteInstanceProfileResponse"
    "fixture/DeleteInstanceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInstanceProfile)

responseDeleteLoginProfile :: DeleteLoginProfileResponse -> TestTree
responseDeleteLoginProfile =
  res
    "DeleteLoginProfileResponse"
    "fixture/DeleteLoginProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLoginProfile)

responseDeleteOpenIDConnectProvider :: DeleteOpenIDConnectProviderResponse -> TestTree
responseDeleteOpenIDConnectProvider =
  res
    "DeleteOpenIDConnectProviderResponse"
    "fixture/DeleteOpenIDConnectProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOpenIDConnectProvider)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePolicy)

responseDeletePolicyVersion :: DeletePolicyVersionResponse -> TestTree
responseDeletePolicyVersion =
  res
    "DeletePolicyVersionResponse"
    "fixture/DeletePolicyVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePolicyVersion)

responseDeleteRole :: DeleteRoleResponse -> TestTree
responseDeleteRole =
  res
    "DeleteRoleResponse"
    "fixture/DeleteRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRole)

responseDeleteRolePermissionsBoundary :: DeleteRolePermissionsBoundaryResponse -> TestTree
responseDeleteRolePermissionsBoundary =
  res
    "DeleteRolePermissionsBoundaryResponse"
    "fixture/DeleteRolePermissionsBoundaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRolePermissionsBoundary)

responseDeleteRolePolicy :: DeleteRolePolicyResponse -> TestTree
responseDeleteRolePolicy =
  res
    "DeleteRolePolicyResponse"
    "fixture/DeleteRolePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRolePolicy)

responseDeleteSAMLProvider :: DeleteSAMLProviderResponse -> TestTree
responseDeleteSAMLProvider =
  res
    "DeleteSAMLProviderResponse"
    "fixture/DeleteSAMLProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSAMLProvider)

responseDeleteSSHPublicKey :: DeleteSSHPublicKeyResponse -> TestTree
responseDeleteSSHPublicKey =
  res
    "DeleteSSHPublicKeyResponse"
    "fixture/DeleteSSHPublicKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSSHPublicKey)

responseDeleteServerCertificate :: DeleteServerCertificateResponse -> TestTree
responseDeleteServerCertificate =
  res
    "DeleteServerCertificateResponse"
    "fixture/DeleteServerCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteServerCertificate)

responseDeleteServiceLinkedRole :: DeleteServiceLinkedRoleResponse -> TestTree
responseDeleteServiceLinkedRole =
  res
    "DeleteServiceLinkedRoleResponse"
    "fixture/DeleteServiceLinkedRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteServiceLinkedRole)

responseDeleteServiceSpecificCredential :: DeleteServiceSpecificCredentialResponse -> TestTree
responseDeleteServiceSpecificCredential =
  res
    "DeleteServiceSpecificCredentialResponse"
    "fixture/DeleteServiceSpecificCredentialResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteServiceSpecificCredential)

responseDeleteSigningCertificate :: DeleteSigningCertificateResponse -> TestTree
responseDeleteSigningCertificate =
  res
    "DeleteSigningCertificateResponse"
    "fixture/DeleteSigningCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSigningCertificate)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUser)

responseDeleteUserPermissionsBoundary :: DeleteUserPermissionsBoundaryResponse -> TestTree
responseDeleteUserPermissionsBoundary =
  res
    "DeleteUserPermissionsBoundaryResponse"
    "fixture/DeleteUserPermissionsBoundaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUserPermissionsBoundary)

responseDeleteUserPolicy :: DeleteUserPolicyResponse -> TestTree
responseDeleteUserPolicy =
  res
    "DeleteUserPolicyResponse"
    "fixture/DeleteUserPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUserPolicy)

responseDeleteVirtualMFADevice :: DeleteVirtualMFADeviceResponse -> TestTree
responseDeleteVirtualMFADevice =
  res
    "DeleteVirtualMFADeviceResponse"
    "fixture/DeleteVirtualMFADeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVirtualMFADevice)

responseDetachGroupPolicy :: DetachGroupPolicyResponse -> TestTree
responseDetachGroupPolicy =
  res
    "DetachGroupPolicyResponse"
    "fixture/DetachGroupPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachGroupPolicy)

responseDetachRolePolicy :: DetachRolePolicyResponse -> TestTree
responseDetachRolePolicy =
  res
    "DetachRolePolicyResponse"
    "fixture/DetachRolePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachRolePolicy)

responseDetachUserPolicy :: DetachUserPolicyResponse -> TestTree
responseDetachUserPolicy =
  res
    "DetachUserPolicyResponse"
    "fixture/DetachUserPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachUserPolicy)

responseEnableMFADevice :: EnableMFADeviceResponse -> TestTree
responseEnableMFADevice =
  res
    "EnableMFADeviceResponse"
    "fixture/EnableMFADeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableMFADevice)

responseGenerateCredentialReport :: GenerateCredentialReportResponse -> TestTree
responseGenerateCredentialReport =
  res
    "GenerateCredentialReportResponse"
    "fixture/GenerateCredentialReportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateCredentialReport)

responseGenerateOrganizationsAccessReport :: GenerateOrganizationsAccessReportResponse -> TestTree
responseGenerateOrganizationsAccessReport =
  res
    "GenerateOrganizationsAccessReportResponse"
    "fixture/GenerateOrganizationsAccessReportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateOrganizationsAccessReport)

responseGenerateServiceLastAccessedDetails :: GenerateServiceLastAccessedDetailsResponse -> TestTree
responseGenerateServiceLastAccessedDetails =
  res
    "GenerateServiceLastAccessedDetailsResponse"
    "fixture/GenerateServiceLastAccessedDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateServiceLastAccessedDetails)

responseGetAccessKeyLastUsed :: GetAccessKeyLastUsedResponse -> TestTree
responseGetAccessKeyLastUsed =
  res
    "GetAccessKeyLastUsedResponse"
    "fixture/GetAccessKeyLastUsedResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccessKeyLastUsed)

responseGetAccountAuthorizationDetails :: GetAccountAuthorizationDetailsResponse -> TestTree
responseGetAccountAuthorizationDetails =
  res
    "GetAccountAuthorizationDetailsResponse"
    "fixture/GetAccountAuthorizationDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccountAuthorizationDetails)

responseGetAccountPasswordPolicy :: GetAccountPasswordPolicyResponse -> TestTree
responseGetAccountPasswordPolicy =
  res
    "GetAccountPasswordPolicyResponse"
    "fixture/GetAccountPasswordPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccountPasswordPolicy)

responseGetAccountSummary :: GetAccountSummaryResponse -> TestTree
responseGetAccountSummary =
  res
    "GetAccountSummaryResponse"
    "fixture/GetAccountSummaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccountSummary)

responseGetContextKeysForCustomPolicy :: GetContextKeysForPolicyResponse -> TestTree
responseGetContextKeysForCustomPolicy =
  res
    "GetContextKeysForCustomPolicyResponse"
    "fixture/GetContextKeysForCustomPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContextKeysForCustomPolicy)

responseGetContextKeysForPrincipalPolicy :: GetContextKeysForPolicyResponse -> TestTree
responseGetContextKeysForPrincipalPolicy =
  res
    "GetContextKeysForPrincipalPolicyResponse"
    "fixture/GetContextKeysForPrincipalPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContextKeysForPrincipalPolicy)

responseGetCredentialReport :: GetCredentialReportResponse -> TestTree
responseGetCredentialReport =
  res
    "GetCredentialReportResponse"
    "fixture/GetCredentialReportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCredentialReport)

responseGetGroup :: GetGroupResponse -> TestTree
responseGetGroup =
  res
    "GetGroupResponse"
    "fixture/GetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGroup)

responseGetGroupPolicy :: GetGroupPolicyResponse -> TestTree
responseGetGroupPolicy =
  res
    "GetGroupPolicyResponse"
    "fixture/GetGroupPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGroupPolicy)

responseGetInstanceProfile :: GetInstanceProfileResponse -> TestTree
responseGetInstanceProfile =
  res
    "GetInstanceProfileResponse"
    "fixture/GetInstanceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstanceProfile)

responseGetLoginProfile :: GetLoginProfileResponse -> TestTree
responseGetLoginProfile =
  res
    "GetLoginProfileResponse"
    "fixture/GetLoginProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLoginProfile)

responseGetOpenIDConnectProvider :: GetOpenIDConnectProviderResponse -> TestTree
responseGetOpenIDConnectProvider =
  res
    "GetOpenIDConnectProviderResponse"
    "fixture/GetOpenIDConnectProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOpenIDConnectProvider)

responseGetOrganizationsAccessReport :: GetOrganizationsAccessReportResponse -> TestTree
responseGetOrganizationsAccessReport =
  res
    "GetOrganizationsAccessReportResponse"
    "fixture/GetOrganizationsAccessReportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOrganizationsAccessReport)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPolicy)

responseGetPolicyVersion :: GetPolicyVersionResponse -> TestTree
responseGetPolicyVersion =
  res
    "GetPolicyVersionResponse"
    "fixture/GetPolicyVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPolicyVersion)

responseGetRole :: GetRoleResponse -> TestTree
responseGetRole =
  res
    "GetRoleResponse"
    "fixture/GetRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRole)

responseGetRolePolicy :: GetRolePolicyResponse -> TestTree
responseGetRolePolicy =
  res
    "GetRolePolicyResponse"
    "fixture/GetRolePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRolePolicy)

responseGetSAMLProvider :: GetSAMLProviderResponse -> TestTree
responseGetSAMLProvider =
  res
    "GetSAMLProviderResponse"
    "fixture/GetSAMLProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSAMLProvider)

responseGetSSHPublicKey :: GetSSHPublicKeyResponse -> TestTree
responseGetSSHPublicKey =
  res
    "GetSSHPublicKeyResponse"
    "fixture/GetSSHPublicKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSSHPublicKey)

responseGetServerCertificate :: GetServerCertificateResponse -> TestTree
responseGetServerCertificate =
  res
    "GetServerCertificateResponse"
    "fixture/GetServerCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServerCertificate)

responseGetServiceLastAccessedDetails :: GetServiceLastAccessedDetailsResponse -> TestTree
responseGetServiceLastAccessedDetails =
  res
    "GetServiceLastAccessedDetailsResponse"
    "fixture/GetServiceLastAccessedDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServiceLastAccessedDetails)

responseGetServiceLastAccessedDetailsWithEntities :: GetServiceLastAccessedDetailsWithEntitiesResponse -> TestTree
responseGetServiceLastAccessedDetailsWithEntities =
  res
    "GetServiceLastAccessedDetailsWithEntitiesResponse"
    "fixture/GetServiceLastAccessedDetailsWithEntitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServiceLastAccessedDetailsWithEntities)

responseGetServiceLinkedRoleDeletionStatus :: GetServiceLinkedRoleDeletionStatusResponse -> TestTree
responseGetServiceLinkedRoleDeletionStatus =
  res
    "GetServiceLinkedRoleDeletionStatusResponse"
    "fixture/GetServiceLinkedRoleDeletionStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServiceLinkedRoleDeletionStatus)

responseGetUser :: GetUserResponse -> TestTree
responseGetUser =
  res
    "GetUserResponse"
    "fixture/GetUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUser)

responseGetUserPolicy :: GetUserPolicyResponse -> TestTree
responseGetUserPolicy =
  res
    "GetUserPolicyResponse"
    "fixture/GetUserPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUserPolicy)

responseListAccessKeys :: ListAccessKeysResponse -> TestTree
responseListAccessKeys =
  res
    "ListAccessKeysResponse"
    "fixture/ListAccessKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccessKeys)

responseListAccountAliases :: ListAccountAliasesResponse -> TestTree
responseListAccountAliases =
  res
    "ListAccountAliasesResponse"
    "fixture/ListAccountAliasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccountAliases)

responseListAttachedGroupPolicies :: ListAttachedGroupPoliciesResponse -> TestTree
responseListAttachedGroupPolicies =
  res
    "ListAttachedGroupPoliciesResponse"
    "fixture/ListAttachedGroupPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAttachedGroupPolicies)

responseListAttachedRolePolicies :: ListAttachedRolePoliciesResponse -> TestTree
responseListAttachedRolePolicies =
  res
    "ListAttachedRolePoliciesResponse"
    "fixture/ListAttachedRolePoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAttachedRolePolicies)

responseListAttachedUserPolicies :: ListAttachedUserPoliciesResponse -> TestTree
responseListAttachedUserPolicies =
  res
    "ListAttachedUserPoliciesResponse"
    "fixture/ListAttachedUserPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAttachedUserPolicies)

responseListEntitiesForPolicy :: ListEntitiesForPolicyResponse -> TestTree
responseListEntitiesForPolicy =
  res
    "ListEntitiesForPolicyResponse"
    "fixture/ListEntitiesForPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEntitiesForPolicy)

responseListGroupPolicies :: ListGroupPoliciesResponse -> TestTree
responseListGroupPolicies =
  res
    "ListGroupPoliciesResponse"
    "fixture/ListGroupPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGroupPolicies)

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups =
  res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGroups)

responseListGroupsForUser :: ListGroupsForUserResponse -> TestTree
responseListGroupsForUser =
  res
    "ListGroupsForUserResponse"
    "fixture/ListGroupsForUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGroupsForUser)

responseListInstanceProfileTags :: ListInstanceProfileTagsResponse -> TestTree
responseListInstanceProfileTags =
  res
    "ListInstanceProfileTagsResponse"
    "fixture/ListInstanceProfileTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInstanceProfileTags)

responseListInstanceProfiles :: ListInstanceProfilesResponse -> TestTree
responseListInstanceProfiles =
  res
    "ListInstanceProfilesResponse"
    "fixture/ListInstanceProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInstanceProfiles)

responseListInstanceProfilesForRole :: ListInstanceProfilesForRoleResponse -> TestTree
responseListInstanceProfilesForRole =
  res
    "ListInstanceProfilesForRoleResponse"
    "fixture/ListInstanceProfilesForRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInstanceProfilesForRole)

responseListMFADeviceTags :: ListMFADeviceTagsResponse -> TestTree
responseListMFADeviceTags =
  res
    "ListMFADeviceTagsResponse"
    "fixture/ListMFADeviceTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMFADeviceTags)

responseListMFADevices :: ListMFADevicesResponse -> TestTree
responseListMFADevices =
  res
    "ListMFADevicesResponse"
    "fixture/ListMFADevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMFADevices)

responseListOpenIDConnectProviderTags :: ListOpenIDConnectProviderTagsResponse -> TestTree
responseListOpenIDConnectProviderTags =
  res
    "ListOpenIDConnectProviderTagsResponse"
    "fixture/ListOpenIDConnectProviderTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOpenIDConnectProviderTags)

responseListOpenIDConnectProviders :: ListOpenIDConnectProvidersResponse -> TestTree
responseListOpenIDConnectProviders =
  res
    "ListOpenIDConnectProvidersResponse"
    "fixture/ListOpenIDConnectProvidersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOpenIDConnectProviders)

responseListPolicies :: ListPoliciesResponse -> TestTree
responseListPolicies =
  res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPolicies)

responseListPoliciesGrantingServiceAccess :: ListPoliciesGrantingServiceAccessResponse -> TestTree
responseListPoliciesGrantingServiceAccess =
  res
    "ListPoliciesGrantingServiceAccessResponse"
    "fixture/ListPoliciesGrantingServiceAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPoliciesGrantingServiceAccess)

responseListPolicyTags :: ListPolicyTagsResponse -> TestTree
responseListPolicyTags =
  res
    "ListPolicyTagsResponse"
    "fixture/ListPolicyTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPolicyTags)

responseListPolicyVersions :: ListPolicyVersionsResponse -> TestTree
responseListPolicyVersions =
  res
    "ListPolicyVersionsResponse"
    "fixture/ListPolicyVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPolicyVersions)

responseListRolePolicies :: ListRolePoliciesResponse -> TestTree
responseListRolePolicies =
  res
    "ListRolePoliciesResponse"
    "fixture/ListRolePoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRolePolicies)

responseListRoleTags :: ListRoleTagsResponse -> TestTree
responseListRoleTags =
  res
    "ListRoleTagsResponse"
    "fixture/ListRoleTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRoleTags)

responseListRoles :: ListRolesResponse -> TestTree
responseListRoles =
  res
    "ListRolesResponse"
    "fixture/ListRolesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRoles)

responseListSAMLProviderTags :: ListSAMLProviderTagsResponse -> TestTree
responseListSAMLProviderTags =
  res
    "ListSAMLProviderTagsResponse"
    "fixture/ListSAMLProviderTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSAMLProviderTags)

responseListSAMLProviders :: ListSAMLProvidersResponse -> TestTree
responseListSAMLProviders =
  res
    "ListSAMLProvidersResponse"
    "fixture/ListSAMLProvidersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSAMLProviders)

responseListSSHPublicKeys :: ListSSHPublicKeysResponse -> TestTree
responseListSSHPublicKeys =
  res
    "ListSSHPublicKeysResponse"
    "fixture/ListSSHPublicKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSSHPublicKeys)

responseListServerCertificateTags :: ListServerCertificateTagsResponse -> TestTree
responseListServerCertificateTags =
  res
    "ListServerCertificateTagsResponse"
    "fixture/ListServerCertificateTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServerCertificateTags)

responseListServerCertificates :: ListServerCertificatesResponse -> TestTree
responseListServerCertificates =
  res
    "ListServerCertificatesResponse"
    "fixture/ListServerCertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServerCertificates)

responseListServiceSpecificCredentials :: ListServiceSpecificCredentialsResponse -> TestTree
responseListServiceSpecificCredentials =
  res
    "ListServiceSpecificCredentialsResponse"
    "fixture/ListServiceSpecificCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServiceSpecificCredentials)

responseListSigningCertificates :: ListSigningCertificatesResponse -> TestTree
responseListSigningCertificates =
  res
    "ListSigningCertificatesResponse"
    "fixture/ListSigningCertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSigningCertificates)

responseListUserPolicies :: ListUserPoliciesResponse -> TestTree
responseListUserPolicies =
  res
    "ListUserPoliciesResponse"
    "fixture/ListUserPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUserPolicies)

responseListUserTags :: ListUserTagsResponse -> TestTree
responseListUserTags =
  res
    "ListUserTagsResponse"
    "fixture/ListUserTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUserTags)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUsers)

responseListVirtualMFADevices :: ListVirtualMFADevicesResponse -> TestTree
responseListVirtualMFADevices =
  res
    "ListVirtualMFADevicesResponse"
    "fixture/ListVirtualMFADevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVirtualMFADevices)

responsePutGroupPolicy :: PutGroupPolicyResponse -> TestTree
responsePutGroupPolicy =
  res
    "PutGroupPolicyResponse"
    "fixture/PutGroupPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutGroupPolicy)

responsePutRolePermissionsBoundary :: PutRolePermissionsBoundaryResponse -> TestTree
responsePutRolePermissionsBoundary =
  res
    "PutRolePermissionsBoundaryResponse"
    "fixture/PutRolePermissionsBoundaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRolePermissionsBoundary)

responsePutRolePolicy :: PutRolePolicyResponse -> TestTree
responsePutRolePolicy =
  res
    "PutRolePolicyResponse"
    "fixture/PutRolePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRolePolicy)

responsePutUserPermissionsBoundary :: PutUserPermissionsBoundaryResponse -> TestTree
responsePutUserPermissionsBoundary =
  res
    "PutUserPermissionsBoundaryResponse"
    "fixture/PutUserPermissionsBoundaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutUserPermissionsBoundary)

responsePutUserPolicy :: PutUserPolicyResponse -> TestTree
responsePutUserPolicy =
  res
    "PutUserPolicyResponse"
    "fixture/PutUserPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutUserPolicy)

responseRemoveClientIDFromOpenIDConnectProvider :: RemoveClientIDFromOpenIDConnectProviderResponse -> TestTree
responseRemoveClientIDFromOpenIDConnectProvider =
  res
    "RemoveClientIDFromOpenIDConnectProviderResponse"
    "fixture/RemoveClientIDFromOpenIDConnectProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveClientIDFromOpenIDConnectProvider)

responseRemoveRoleFromInstanceProfile :: RemoveRoleFromInstanceProfileResponse -> TestTree
responseRemoveRoleFromInstanceProfile =
  res
    "RemoveRoleFromInstanceProfileResponse"
    "fixture/RemoveRoleFromInstanceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveRoleFromInstanceProfile)

responseRemoveUserFromGroup :: RemoveUserFromGroupResponse -> TestTree
responseRemoveUserFromGroup =
  res
    "RemoveUserFromGroupResponse"
    "fixture/RemoveUserFromGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveUserFromGroup)

responseResetServiceSpecificCredential :: ResetServiceSpecificCredentialResponse -> TestTree
responseResetServiceSpecificCredential =
  res
    "ResetServiceSpecificCredentialResponse"
    "fixture/ResetServiceSpecificCredentialResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetServiceSpecificCredential)

responseResyncMFADevice :: ResyncMFADeviceResponse -> TestTree
responseResyncMFADevice =
  res
    "ResyncMFADeviceResponse"
    "fixture/ResyncMFADeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResyncMFADevice)

responseSetDefaultPolicyVersion :: SetDefaultPolicyVersionResponse -> TestTree
responseSetDefaultPolicyVersion =
  res
    "SetDefaultPolicyVersionResponse"
    "fixture/SetDefaultPolicyVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetDefaultPolicyVersion)

responseSetSecurityTokenServicePreferences :: SetSecurityTokenServicePreferencesResponse -> TestTree
responseSetSecurityTokenServicePreferences =
  res
    "SetSecurityTokenServicePreferencesResponse"
    "fixture/SetSecurityTokenServicePreferencesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetSecurityTokenServicePreferences)

responseSimulateCustomPolicy :: SimulatePolicyResponse -> TestTree
responseSimulateCustomPolicy =
  res
    "SimulateCustomPolicyResponse"
    "fixture/SimulateCustomPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SimulateCustomPolicy)

responseSimulatePrincipalPolicy :: SimulatePolicyResponse -> TestTree
responseSimulatePrincipalPolicy =
  res
    "SimulatePrincipalPolicyResponse"
    "fixture/SimulatePrincipalPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SimulatePrincipalPolicy)

responseTagInstanceProfile :: TagInstanceProfileResponse -> TestTree
responseTagInstanceProfile =
  res
    "TagInstanceProfileResponse"
    "fixture/TagInstanceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagInstanceProfile)

responseTagMFADevice :: TagMFADeviceResponse -> TestTree
responseTagMFADevice =
  res
    "TagMFADeviceResponse"
    "fixture/TagMFADeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagMFADevice)

responseTagOpenIDConnectProvider :: TagOpenIDConnectProviderResponse -> TestTree
responseTagOpenIDConnectProvider =
  res
    "TagOpenIDConnectProviderResponse"
    "fixture/TagOpenIDConnectProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagOpenIDConnectProvider)

responseTagPolicy :: TagPolicyResponse -> TestTree
responseTagPolicy =
  res
    "TagPolicyResponse"
    "fixture/TagPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagPolicy)

responseTagRole :: TagRoleResponse -> TestTree
responseTagRole =
  res
    "TagRoleResponse"
    "fixture/TagRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagRole)

responseTagSAMLProvider :: TagSAMLProviderResponse -> TestTree
responseTagSAMLProvider =
  res
    "TagSAMLProviderResponse"
    "fixture/TagSAMLProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagSAMLProvider)

responseTagServerCertificate :: TagServerCertificateResponse -> TestTree
responseTagServerCertificate =
  res
    "TagServerCertificateResponse"
    "fixture/TagServerCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagServerCertificate)

responseTagUser :: TagUserResponse -> TestTree
responseTagUser =
  res
    "TagUserResponse"
    "fixture/TagUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagUser)

responseUntagInstanceProfile :: UntagInstanceProfileResponse -> TestTree
responseUntagInstanceProfile =
  res
    "UntagInstanceProfileResponse"
    "fixture/UntagInstanceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagInstanceProfile)

responseUntagMFADevice :: UntagMFADeviceResponse -> TestTree
responseUntagMFADevice =
  res
    "UntagMFADeviceResponse"
    "fixture/UntagMFADeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagMFADevice)

responseUntagOpenIDConnectProvider :: UntagOpenIDConnectProviderResponse -> TestTree
responseUntagOpenIDConnectProvider =
  res
    "UntagOpenIDConnectProviderResponse"
    "fixture/UntagOpenIDConnectProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagOpenIDConnectProvider)

responseUntagPolicy :: UntagPolicyResponse -> TestTree
responseUntagPolicy =
  res
    "UntagPolicyResponse"
    "fixture/UntagPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagPolicy)

responseUntagRole :: UntagRoleResponse -> TestTree
responseUntagRole =
  res
    "UntagRoleResponse"
    "fixture/UntagRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagRole)

responseUntagSAMLProvider :: UntagSAMLProviderResponse -> TestTree
responseUntagSAMLProvider =
  res
    "UntagSAMLProviderResponse"
    "fixture/UntagSAMLProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagSAMLProvider)

responseUntagServerCertificate :: UntagServerCertificateResponse -> TestTree
responseUntagServerCertificate =
  res
    "UntagServerCertificateResponse"
    "fixture/UntagServerCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagServerCertificate)

responseUntagUser :: UntagUserResponse -> TestTree
responseUntagUser =
  res
    "UntagUserResponse"
    "fixture/UntagUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagUser)

responseUpdateAccessKey :: UpdateAccessKeyResponse -> TestTree
responseUpdateAccessKey =
  res
    "UpdateAccessKeyResponse"
    "fixture/UpdateAccessKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAccessKey)

responseUpdateAccountPasswordPolicy :: UpdateAccountPasswordPolicyResponse -> TestTree
responseUpdateAccountPasswordPolicy =
  res
    "UpdateAccountPasswordPolicyResponse"
    "fixture/UpdateAccountPasswordPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAccountPasswordPolicy)

responseUpdateAssumeRolePolicy :: UpdateAssumeRolePolicyResponse -> TestTree
responseUpdateAssumeRolePolicy =
  res
    "UpdateAssumeRolePolicyResponse"
    "fixture/UpdateAssumeRolePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAssumeRolePolicy)

responseUpdateGroup :: UpdateGroupResponse -> TestTree
responseUpdateGroup =
  res
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGroup)

responseUpdateLoginProfile :: UpdateLoginProfileResponse -> TestTree
responseUpdateLoginProfile =
  res
    "UpdateLoginProfileResponse"
    "fixture/UpdateLoginProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLoginProfile)

responseUpdateOpenIDConnectProviderThumbprint :: UpdateOpenIDConnectProviderThumbprintResponse -> TestTree
responseUpdateOpenIDConnectProviderThumbprint =
  res
    "UpdateOpenIDConnectProviderThumbprintResponse"
    "fixture/UpdateOpenIDConnectProviderThumbprintResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateOpenIDConnectProviderThumbprint)

responseUpdateRole :: UpdateRoleResponse -> TestTree
responseUpdateRole =
  res
    "UpdateRoleResponse"
    "fixture/UpdateRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRole)

responseUpdateRoleDescription :: UpdateRoleDescriptionResponse -> TestTree
responseUpdateRoleDescription =
  res
    "UpdateRoleDescriptionResponse"
    "fixture/UpdateRoleDescriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRoleDescription)

responseUpdateSAMLProvider :: UpdateSAMLProviderResponse -> TestTree
responseUpdateSAMLProvider =
  res
    "UpdateSAMLProviderResponse"
    "fixture/UpdateSAMLProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSAMLProvider)

responseUpdateSSHPublicKey :: UpdateSSHPublicKeyResponse -> TestTree
responseUpdateSSHPublicKey =
  res
    "UpdateSSHPublicKeyResponse"
    "fixture/UpdateSSHPublicKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSSHPublicKey)

responseUpdateServerCertificate :: UpdateServerCertificateResponse -> TestTree
responseUpdateServerCertificate =
  res
    "UpdateServerCertificateResponse"
    "fixture/UpdateServerCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServerCertificate)

responseUpdateServiceSpecificCredential :: UpdateServiceSpecificCredentialResponse -> TestTree
responseUpdateServiceSpecificCredential =
  res
    "UpdateServiceSpecificCredentialResponse"
    "fixture/UpdateServiceSpecificCredentialResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServiceSpecificCredential)

responseUpdateSigningCertificate :: UpdateSigningCertificateResponse -> TestTree
responseUpdateSigningCertificate =
  res
    "UpdateSigningCertificateResponse"
    "fixture/UpdateSigningCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSigningCertificate)

responseUpdateUser :: UpdateUserResponse -> TestTree
responseUpdateUser =
  res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUser)

responseUploadSSHPublicKey :: UploadSSHPublicKeyResponse -> TestTree
responseUploadSSHPublicKey =
  res
    "UploadSSHPublicKeyResponse"
    "fixture/UploadSSHPublicKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UploadSSHPublicKey)

responseUploadServerCertificate :: UploadServerCertificateResponse -> TestTree
responseUploadServerCertificate =
  res
    "UploadServerCertificateResponse"
    "fixture/UploadServerCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UploadServerCertificate)

responseUploadSigningCertificate :: UploadSigningCertificateResponse -> TestTree
responseUploadSigningCertificate =
  res
    "UploadSigningCertificateResponse"
    "fixture/UploadSigningCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UploadSigningCertificate)
