{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.IAM
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--             mkGetContextKeysForPrincipalPolicy
--
--         , requestListPolicies $
--             mkListPolicies
--
--         , requestCreatePolicy $
--             mkCreatePolicy
--
--         , requestListInstanceProfilesForRole $
--             mkListInstanceProfilesForRole
--
--         , requestAttachGroupPolicy $
--             mkAttachGroupPolicy
--
--         , requestCreateAccessKey $
--             mkCreateAccessKey
--
--         , requestListRoleTags $
--             mkListRoleTags
--
--         , requestListSSHPublicKeys $
--             mkListSSHPublicKeys
--
--         , requestListOpenIDConnectProviders $
--             mkListOpenIDConnectProviders
--
--         , requestCreateVirtualMFADevice $
--             mkCreateVirtualMFADevice
--
--         , requestDeleteAccountPasswordPolicy $
--             mkDeleteAccountPasswordPolicy
--
--         , requestUpdateAccountPasswordPolicy $
--             mkUpdateAccountPasswordPolicy
--
--         , requestAttachRolePolicy $
--             mkAttachRolePolicy
--
--         , requestUpdateSSHPublicKey $
--             mkUpdateSSHPublicKey
--
--         , requestDeleteSSHPublicKey $
--             mkDeleteSSHPublicKey
--
--         , requestGetUserPolicy $
--             mkGetUserPolicy
--
--         , requestUpdateServiceSpecificCredential $
--             mkUpdateServiceSpecificCredential
--
--         , requestDeleteServiceSpecificCredential $
--             mkDeleteServiceSpecificCredential
--
--         , requestListAttachedRolePolicies $
--             mkListAttachedRolePolicies
--
--         , requestGetRole $
--             mkGetRole
--
--         , requestDeactivateMFADevice $
--             mkDeactivateMFADevice
--
--         , requestCreateOpenIDConnectProvider $
--             mkCreateOpenIDConnectProvider
--
--         , requestDeleteVirtualMFADevice $
--             mkDeleteVirtualMFADevice
--
--         , requestListRoles $
--             mkListRoles
--
--         , requestListUserPolicies $
--             mkListUserPolicies
--
--         , requestPutRolePermissionsBoundary $
--             mkPutRolePermissionsBoundary
--
--         , requestUploadSSHPublicKey $
--             mkUploadSSHPublicKey
--
--         , requestDeleteRolePermissionsBoundary $
--             mkDeleteRolePermissionsBoundary
--
--         , requestSimulateCustomPolicy $
--             mkSimulateCustomPolicy
--
--         , requestUpdateRole $
--             mkUpdateRole
--
--         , requestDeleteRole $
--             mkDeleteRole
--
--         , requestListUsers $
--             mkListUsers
--
--         , requestUpdateOpenIDConnectProviderThumbprint $
--             mkUpdateOpenIDConnectProviderThumbprint
--
--         , requestPutUserPolicy $
--             mkPutUserPolicy
--
--         , requestGetSSHPublicKey $
--             mkGetSSHPublicKey
--
--         , requestUntagUser $
--             mkUntagUser
--
--         , requestDetachGroupPolicy $
--             mkDetachGroupPolicy
--
--         , requestGetOpenIDConnectProvider $
--             mkGetOpenIDConnectProvider
--
--         , requestPutUserPermissionsBoundary $
--             mkPutUserPermissionsBoundary
--
--         , requestDeleteUserPolicy $
--             mkDeleteUserPolicy
--
--         , requestDeleteUserPermissionsBoundary $
--             mkDeleteUserPermissionsBoundary
--
--         , requestCreateRole $
--             mkCreateRole
--
--         , requestResetServiceSpecificCredential $
--             mkResetServiceSpecificCredential
--
--         , requestGetCredentialReport $
--             mkGetCredentialReport
--
--         , requestGetAccountSummary $
--             mkGetAccountSummary
--
--         , requestGenerateServiceLastAccessedDetails $
--             mkGenerateServiceLastAccessedDetails
--
--         , requestListGroupPolicies $
--             mkListGroupPolicies
--
--         , requestDeletePolicyVersion $
--             mkDeletePolicyVersion
--
--         , requestTagUser $
--             mkTagUser
--
--         , requestDeleteInstanceProfile $
--             mkDeleteInstanceProfile
--
--         , requestDetachRolePolicy $
--             mkDetachRolePolicy
--
--         , requestRemoveRoleFromInstanceProfile $
--             mkRemoveRoleFromInstanceProfile
--
--         , requestCreatePolicyVersion $
--             mkCreatePolicyVersion
--
--         , requestCreateInstanceProfile $
--             mkCreateInstanceProfile
--
--         , requestCreateSAMLProvider $
--             mkCreateSAMLProvider
--
--         , requestGetAccountAuthorizationDetails $
--             mkGetAccountAuthorizationDetails
--
--         , requestGetServiceLinkedRoleDeletionStatus $
--             mkGetServiceLinkedRoleDeletionStatus
--
--         , requestDeleteAccountAlias $
--             mkDeleteAccountAlias
--
--         , requestDetachUserPolicy $
--             mkDetachUserPolicy
--
--         , requestRemoveUserFromGroup $
--             mkRemoveUserFromGroup
--
--         , requestDeleteGroupPolicy $
--             mkDeleteGroupPolicy
--
--         , requestTagRole $
--             mkTagRole
--
--         , requestPutGroupPolicy $
--             mkPutGroupPolicy
--
--         , requestGetLoginProfile $
--             mkGetLoginProfile
--
--         , requestGetGroupPolicy $
--             mkGetGroupPolicy
--
--         , requestGenerateOrganizationsAccessReport $
--             mkGenerateOrganizationsAccessReport
--
--         , requestChangePassword $
--             mkChangePassword
--
--         , requestListServerCertificates $
--             mkListServerCertificates
--
--         , requestDeleteServiceLinkedRole $
--             mkDeleteServiceLinkedRole
--
--         , requestDeletePolicy $
--             mkDeletePolicy
--
--         , requestUpdateAssumeRolePolicy $
--             mkUpdateAssumeRolePolicy
--
--         , requestGetServiceLastAccessedDetailsWithEntities $
--             mkGetServiceLastAccessedDetailsWithEntities
--
--         , requestGetInstanceProfile $
--             mkGetInstanceProfile
--
--         , requestCreateLoginProfile $
--             mkCreateLoginProfile
--
--         , requestGetSAMLProvider $
--             mkGetSAMLProvider
--
--         , requestAddRoleToInstanceProfile $
--             mkAddRoleToInstanceProfile
--
--         , requestListGroupsForUser $
--             mkListGroupsForUser
--
--         , requestListEntitiesForPolicy $
--             mkListEntitiesForPolicy
--
--         , requestAddUserToGroup $
--             mkAddUserToGroup
--
--         , requestSimulatePrincipalPolicy $
--             mkSimulatePrincipalPolicy
--
--         , requestGetOrganizationsAccessReport $
--             mkGetOrganizationsAccessReport
--
--         , requestGetPolicyVersion $
--             mkGetPolicyVersion
--
--         , requestCreateServiceLinkedRole $
--             mkCreateServiceLinkedRole
--
--         , requestListServiceSpecificCredentials $
--             mkListServiceSpecificCredentials
--
--         , requestDeleteOpenIDConnectProvider $
--             mkDeleteOpenIDConnectProvider
--
--         , requestGetUser $
--             mkGetUser
--
--         , requestListSigningCertificates $
--             mkListSigningCertificates
--
--         , requestDeleteSigningCertificate $
--             mkDeleteSigningCertificate
--
--         , requestUpdateSigningCertificate $
--             mkUpdateSigningCertificate
--
--         , requestListAttachedUserPolicies $
--             mkListAttachedUserPolicies
--
--         , requestRemoveClientIDFromOpenIDConnectProvider $
--             mkRemoveClientIDFromOpenIDConnectProvider
--
--         , requestAttachUserPolicy $
--             mkAttachUserPolicy
--
--         , requestCreateServiceSpecificCredential $
--             mkCreateServiceSpecificCredential
--
--         , requestListVirtualMFADevices $
--             mkListVirtualMFADevices
--
--         , requestResyncMFADevice $
--             mkResyncMFADevice
--
--         , requestDeleteAccessKey $
--             mkDeleteAccessKey
--
--         , requestUpdateAccessKey $
--             mkUpdateAccessKey
--
--         , requestListUserTags $
--             mkListUserTags
--
--         , requestListAccessKeys $
--             mkListAccessKeys
--
--         , requestGetRolePolicy $
--             mkGetRolePolicy
--
--         , requestSetSecurityTokenServicePreferences $
--             mkSetSecurityTokenServicePreferences
--
--         , requestUntagRole $
--             mkUntagRole
--
--         , requestCreateUser $
--             mkCreateUser
--
--         , requestPutRolePolicy $
--             mkPutRolePolicy
--
--         , requestGetContextKeysForCustomPolicy $
--             mkGetContextKeysForCustomPolicy
--
--         , requestUploadSigningCertificate $
--             mkUploadSigningCertificate
--
--         , requestDeleteRolePolicy $
--             mkDeleteRolePolicy
--
--         , requestGetAccountPasswordPolicy $
--             mkGetAccountPasswordPolicy
--
--         , requestGetAccessKeyLastUsed $
--             mkGetAccessKeyLastUsed
--
--         , requestUpdateUser $
--             mkUpdateUser
--
--         , requestDeleteUser $
--             mkDeleteUser
--
--         , requestAddClientIDToOpenIDConnectProvider $
--             mkAddClientIDToOpenIDConnectProvider
--
--         , requestListRolePolicies $
--             mkListRolePolicies
--
--         , requestCreateAccountAlias $
--             mkCreateAccountAlias
--
--         , requestListPoliciesGrantingServiceAccess $
--             mkListPoliciesGrantingServiceAccess
--
--         , requestListInstanceProfiles $
--             mkListInstanceProfiles
--
--         , requestEnableMFADevice $
--             mkEnableMFADevice
--
--         , requestListAccountAliases $
--             mkListAccountAliases
--
--         , requestDeleteSAMLProvider $
--             mkDeleteSAMLProvider
--
--         , requestUpdateSAMLProvider $
--             mkUpdateSAMLProvider
--
--         , requestCreateGroup $
--             mkCreateGroup
--
--         , requestListMFADevices $
--             mkListMFADevices
--
--         , requestUploadServerCertificate $
--             mkUploadServerCertificate
--
--         , requestSetDefaultPolicyVersion $
--             mkSetDefaultPolicyVersion
--
--         , requestListPolicyVersions $
--             mkListPolicyVersions
--
--         , requestUpdateRoleDescription $
--             mkUpdateRoleDescription
--
--         , requestListSAMLProviders $
--             mkListSAMLProviders
--
--         , requestGetServiceLastAccessedDetails $
--             mkGetServiceLastAccessedDetails
--
--         , requestGetServerCertificate $
--             mkGetServerCertificate
--
--         , requestDeleteGroup $
--             mkDeleteGroup
--
--         , requestUpdateGroup $
--             mkUpdateGroup
--
--         , requestListGroups $
--             mkListGroups
--
--         , requestGenerateCredentialReport $
--             mkGenerateCredentialReport
--
--         , requestGetPolicy $
--             mkGetPolicy
--
--         , requestUpdateLoginProfile $
--             mkUpdateLoginProfile
--
--         , requestDeleteLoginProfile $
--             mkDeleteLoginProfile
--
--         , requestGetGroup $
--             mkGetGroup
--
--         , requestDeleteServerCertificate $
--             mkDeleteServerCertificate
--
--         , requestUpdateServerCertificate $
--             mkUpdateServerCertificate
--
--         , requestListAttachedGroupPolicies $
--             mkListAttachedGroupPolicies
--
--           ]

--     , testGroup "response"
--         [ responseGetContextKeysForPrincipalPolicy $
--             mkGetContextKeysForPolicyResponse
--
--         , responseListPolicies $
--             mkListPoliciesResponse
--
--         , responseCreatePolicy $
--             mkCreatePolicyResponse
--
--         , responseListInstanceProfilesForRole $
--             mkListInstanceProfilesForRoleResponse
--
--         , responseAttachGroupPolicy $
--             mkAttachGroupPolicyResponse
--
--         , responseCreateAccessKey $
--             mkCreateAccessKeyResponse
--
--         , responseListRoleTags $
--             mkListRoleTagsResponse
--
--         , responseListSSHPublicKeys $
--             mkListSSHPublicKeysResponse
--
--         , responseListOpenIDConnectProviders $
--             mkListOpenIDConnectProvidersResponse
--
--         , responseCreateVirtualMFADevice $
--             mkCreateVirtualMFADeviceResponse
--
--         , responseDeleteAccountPasswordPolicy $
--             mkDeleteAccountPasswordPolicyResponse
--
--         , responseUpdateAccountPasswordPolicy $
--             mkUpdateAccountPasswordPolicyResponse
--
--         , responseAttachRolePolicy $
--             mkAttachRolePolicyResponse
--
--         , responseUpdateSSHPublicKey $
--             mkUpdateSSHPublicKeyResponse
--
--         , responseDeleteSSHPublicKey $
--             mkDeleteSSHPublicKeyResponse
--
--         , responseGetUserPolicy $
--             mkGetUserPolicyResponse
--
--         , responseUpdateServiceSpecificCredential $
--             mkUpdateServiceSpecificCredentialResponse
--
--         , responseDeleteServiceSpecificCredential $
--             mkDeleteServiceSpecificCredentialResponse
--
--         , responseListAttachedRolePolicies $
--             mkListAttachedRolePoliciesResponse
--
--         , responseGetRole $
--             mkGetRoleResponse
--
--         , responseDeactivateMFADevice $
--             mkDeactivateMFADeviceResponse
--
--         , responseCreateOpenIDConnectProvider $
--             mkCreateOpenIDConnectProviderResponse
--
--         , responseDeleteVirtualMFADevice $
--             mkDeleteVirtualMFADeviceResponse
--
--         , responseListRoles $
--             mkListRolesResponse
--
--         , responseListUserPolicies $
--             mkListUserPoliciesResponse
--
--         , responsePutRolePermissionsBoundary $
--             mkPutRolePermissionsBoundaryResponse
--
--         , responseUploadSSHPublicKey $
--             mkUploadSSHPublicKeyResponse
--
--         , responseDeleteRolePermissionsBoundary $
--             mkDeleteRolePermissionsBoundaryResponse
--
--         , responseSimulateCustomPolicy $
--             mkSimulatePolicyResponse
--
--         , responseUpdateRole $
--             mkUpdateRoleResponse
--
--         , responseDeleteRole $
--             mkDeleteRoleResponse
--
--         , responseListUsers $
--             mkListUsersResponse
--
--         , responseUpdateOpenIDConnectProviderThumbprint $
--             mkUpdateOpenIDConnectProviderThumbprintResponse
--
--         , responsePutUserPolicy $
--             mkPutUserPolicyResponse
--
--         , responseGetSSHPublicKey $
--             mkGetSSHPublicKeyResponse
--
--         , responseUntagUser $
--             mkUntagUserResponse
--
--         , responseDetachGroupPolicy $
--             mkDetachGroupPolicyResponse
--
--         , responseGetOpenIDConnectProvider $
--             mkGetOpenIDConnectProviderResponse
--
--         , responsePutUserPermissionsBoundary $
--             mkPutUserPermissionsBoundaryResponse
--
--         , responseDeleteUserPolicy $
--             mkDeleteUserPolicyResponse
--
--         , responseDeleteUserPermissionsBoundary $
--             mkDeleteUserPermissionsBoundaryResponse
--
--         , responseCreateRole $
--             mkCreateRoleResponse
--
--         , responseResetServiceSpecificCredential $
--             mkResetServiceSpecificCredentialResponse
--
--         , responseGetCredentialReport $
--             mkGetCredentialReportResponse
--
--         , responseGetAccountSummary $
--             mkGetAccountSummaryResponse
--
--         , responseGenerateServiceLastAccessedDetails $
--             mkGenerateServiceLastAccessedDetailsResponse
--
--         , responseListGroupPolicies $
--             mkListGroupPoliciesResponse
--
--         , responseDeletePolicyVersion $
--             mkDeletePolicyVersionResponse
--
--         , responseTagUser $
--             mkTagUserResponse
--
--         , responseDeleteInstanceProfile $
--             mkDeleteInstanceProfileResponse
--
--         , responseDetachRolePolicy $
--             mkDetachRolePolicyResponse
--
--         , responseRemoveRoleFromInstanceProfile $
--             mkRemoveRoleFromInstanceProfileResponse
--
--         , responseCreatePolicyVersion $
--             mkCreatePolicyVersionResponse
--
--         , responseCreateInstanceProfile $
--             mkCreateInstanceProfileResponse
--
--         , responseCreateSAMLProvider $
--             mkCreateSAMLProviderResponse
--
--         , responseGetAccountAuthorizationDetails $
--             mkGetAccountAuthorizationDetailsResponse
--
--         , responseGetServiceLinkedRoleDeletionStatus $
--             mkGetServiceLinkedRoleDeletionStatusResponse
--
--         , responseDeleteAccountAlias $
--             mkDeleteAccountAliasResponse
--
--         , responseDetachUserPolicy $
--             mkDetachUserPolicyResponse
--
--         , responseRemoveUserFromGroup $
--             mkRemoveUserFromGroupResponse
--
--         , responseDeleteGroupPolicy $
--             mkDeleteGroupPolicyResponse
--
--         , responseTagRole $
--             mkTagRoleResponse
--
--         , responsePutGroupPolicy $
--             mkPutGroupPolicyResponse
--
--         , responseGetLoginProfile $
--             mkGetLoginProfileResponse
--
--         , responseGetGroupPolicy $
--             mkGetGroupPolicyResponse
--
--         , responseGenerateOrganizationsAccessReport $
--             mkGenerateOrganizationsAccessReportResponse
--
--         , responseChangePassword $
--             mkChangePasswordResponse
--
--         , responseListServerCertificates $
--             mkListServerCertificatesResponse
--
--         , responseDeleteServiceLinkedRole $
--             mkDeleteServiceLinkedRoleResponse
--
--         , responseDeletePolicy $
--             mkDeletePolicyResponse
--
--         , responseUpdateAssumeRolePolicy $
--             mkUpdateAssumeRolePolicyResponse
--
--         , responseGetServiceLastAccessedDetailsWithEntities $
--             mkGetServiceLastAccessedDetailsWithEntitiesResponse
--
--         , responseGetInstanceProfile $
--             mkGetInstanceProfileResponse
--
--         , responseCreateLoginProfile $
--             mkCreateLoginProfileResponse
--
--         , responseGetSAMLProvider $
--             mkGetSAMLProviderResponse
--
--         , responseAddRoleToInstanceProfile $
--             mkAddRoleToInstanceProfileResponse
--
--         , responseListGroupsForUser $
--             mkListGroupsForUserResponse
--
--         , responseListEntitiesForPolicy $
--             mkListEntitiesForPolicyResponse
--
--         , responseAddUserToGroup $
--             mkAddUserToGroupResponse
--
--         , responseSimulatePrincipalPolicy $
--             mkSimulatePolicyResponse
--
--         , responseGetOrganizationsAccessReport $
--             mkGetOrganizationsAccessReportResponse
--
--         , responseGetPolicyVersion $
--             mkGetPolicyVersionResponse
--
--         , responseCreateServiceLinkedRole $
--             mkCreateServiceLinkedRoleResponse
--
--         , responseListServiceSpecificCredentials $
--             mkListServiceSpecificCredentialsResponse
--
--         , responseDeleteOpenIDConnectProvider $
--             mkDeleteOpenIDConnectProviderResponse
--
--         , responseGetUser $
--             mkGetUserResponse
--
--         , responseListSigningCertificates $
--             mkListSigningCertificatesResponse
--
--         , responseDeleteSigningCertificate $
--             mkDeleteSigningCertificateResponse
--
--         , responseUpdateSigningCertificate $
--             mkUpdateSigningCertificateResponse
--
--         , responseListAttachedUserPolicies $
--             mkListAttachedUserPoliciesResponse
--
--         , responseRemoveClientIDFromOpenIDConnectProvider $
--             mkRemoveClientIDFromOpenIDConnectProviderResponse
--
--         , responseAttachUserPolicy $
--             mkAttachUserPolicyResponse
--
--         , responseCreateServiceSpecificCredential $
--             mkCreateServiceSpecificCredentialResponse
--
--         , responseListVirtualMFADevices $
--             mkListVirtualMFADevicesResponse
--
--         , responseResyncMFADevice $
--             mkResyncMFADeviceResponse
--
--         , responseDeleteAccessKey $
--             mkDeleteAccessKeyResponse
--
--         , responseUpdateAccessKey $
--             mkUpdateAccessKeyResponse
--
--         , responseListUserTags $
--             mkListUserTagsResponse
--
--         , responseListAccessKeys $
--             mkListAccessKeysResponse
--
--         , responseGetRolePolicy $
--             mkGetRolePolicyResponse
--
--         , responseSetSecurityTokenServicePreferences $
--             mkSetSecurityTokenServicePreferencesResponse
--
--         , responseUntagRole $
--             mkUntagRoleResponse
--
--         , responseCreateUser $
--             mkCreateUserResponse
--
--         , responsePutRolePolicy $
--             mkPutRolePolicyResponse
--
--         , responseGetContextKeysForCustomPolicy $
--             mkGetContextKeysForPolicyResponse
--
--         , responseUploadSigningCertificate $
--             mkUploadSigningCertificateResponse
--
--         , responseDeleteRolePolicy $
--             mkDeleteRolePolicyResponse
--
--         , responseGetAccountPasswordPolicy $
--             mkGetAccountPasswordPolicyResponse
--
--         , responseGetAccessKeyLastUsed $
--             mkGetAccessKeyLastUsedResponse
--
--         , responseUpdateUser $
--             mkUpdateUserResponse
--
--         , responseDeleteUser $
--             mkDeleteUserResponse
--
--         , responseAddClientIDToOpenIDConnectProvider $
--             mkAddClientIDToOpenIDConnectProviderResponse
--
--         , responseListRolePolicies $
--             mkListRolePoliciesResponse
--
--         , responseCreateAccountAlias $
--             mkCreateAccountAliasResponse
--
--         , responseListPoliciesGrantingServiceAccess $
--             mkListPoliciesGrantingServiceAccessResponse
--
--         , responseListInstanceProfiles $
--             mkListInstanceProfilesResponse
--
--         , responseEnableMFADevice $
--             mkEnableMFADeviceResponse
--
--         , responseListAccountAliases $
--             mkListAccountAliasesResponse
--
--         , responseDeleteSAMLProvider $
--             mkDeleteSAMLProviderResponse
--
--         , responseUpdateSAMLProvider $
--             mkUpdateSAMLProviderResponse
--
--         , responseCreateGroup $
--             mkCreateGroupResponse
--
--         , responseListMFADevices $
--             mkListMFADevicesResponse
--
--         , responseUploadServerCertificate $
--             mkUploadServerCertificateResponse
--
--         , responseSetDefaultPolicyVersion $
--             mkSetDefaultPolicyVersionResponse
--
--         , responseListPolicyVersions $
--             mkListPolicyVersionsResponse
--
--         , responseUpdateRoleDescription $
--             mkUpdateRoleDescriptionResponse
--
--         , responseListSAMLProviders $
--             mkListSAMLProvidersResponse
--
--         , responseGetServiceLastAccessedDetails $
--             mkGetServiceLastAccessedDetailsResponse
--
--         , responseGetServerCertificate $
--             mkGetServerCertificateResponse
--
--         , responseDeleteGroup $
--             mkDeleteGroupResponse
--
--         , responseUpdateGroup $
--             mkUpdateGroupResponse
--
--         , responseListGroups $
--             mkListGroupsResponse
--
--         , responseGenerateCredentialReport $
--             mkGenerateCredentialReportResponse
--
--         , responseGetPolicy $
--             mkGetPolicyResponse
--
--         , responseUpdateLoginProfile $
--             mkUpdateLoginProfileResponse
--
--         , responseDeleteLoginProfile $
--             mkDeleteLoginProfileResponse
--
--         , responseGetGroup $
--             mkGetGroupResponse
--
--         , responseDeleteServerCertificate $
--             mkDeleteServerCertificateResponse
--
--         , responseUpdateServerCertificate $
--             mkUpdateServerCertificateResponse
--
--         , responseListAttachedGroupPolicies $
--             mkListAttachedGroupPoliciesResponse
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

requestGetCredentialReport :: GetCredentialReport -> TestTree
requestGetCredentialReport =
  req
    "GetCredentialReport"
    "fixture/GetCredentialReport.yaml"

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

-- Responses

responseGetContextKeysForPrincipalPolicy :: GetContextKeysForPolicyResponse -> TestTree
responseGetContextKeysForPrincipalPolicy =
  res
    "GetContextKeysForPrincipalPolicyResponse"
    "fixture/GetContextKeysForPrincipalPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetContextKeysForPrincipalPolicy)

responseListPolicies :: ListPoliciesResponse -> TestTree
responseListPolicies =
  res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListPolicies)

responseCreatePolicy :: CreatePolicyResponse -> TestTree
responseCreatePolicy =
  res
    "CreatePolicyResponse"
    "fixture/CreatePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreatePolicy)

responseListInstanceProfilesForRole :: ListInstanceProfilesForRoleResponse -> TestTree
responseListInstanceProfilesForRole =
  res
    "ListInstanceProfilesForRoleResponse"
    "fixture/ListInstanceProfilesForRoleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListInstanceProfilesForRole)

responseAttachGroupPolicy :: AttachGroupPolicyResponse -> TestTree
responseAttachGroupPolicy =
  res
    "AttachGroupPolicyResponse"
    "fixture/AttachGroupPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AttachGroupPolicy)

responseCreateAccessKey :: CreateAccessKeyResponse -> TestTree
responseCreateAccessKey =
  res
    "CreateAccessKeyResponse"
    "fixture/CreateAccessKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateAccessKey)

responseListRoleTags :: ListRoleTagsResponse -> TestTree
responseListRoleTags =
  res
    "ListRoleTagsResponse"
    "fixture/ListRoleTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListRoleTags)

responseListSSHPublicKeys :: ListSSHPublicKeysResponse -> TestTree
responseListSSHPublicKeys =
  res
    "ListSSHPublicKeysResponse"
    "fixture/ListSSHPublicKeysResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListSSHPublicKeys)

responseListOpenIDConnectProviders :: ListOpenIDConnectProvidersResponse -> TestTree
responseListOpenIDConnectProviders =
  res
    "ListOpenIDConnectProvidersResponse"
    "fixture/ListOpenIDConnectProvidersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListOpenIDConnectProviders)

responseCreateVirtualMFADevice :: CreateVirtualMFADeviceResponse -> TestTree
responseCreateVirtualMFADevice =
  res
    "CreateVirtualMFADeviceResponse"
    "fixture/CreateVirtualMFADeviceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateVirtualMFADevice)

responseDeleteAccountPasswordPolicy :: DeleteAccountPasswordPolicyResponse -> TestTree
responseDeleteAccountPasswordPolicy =
  res
    "DeleteAccountPasswordPolicyResponse"
    "fixture/DeleteAccountPasswordPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteAccountPasswordPolicy)

responseUpdateAccountPasswordPolicy :: UpdateAccountPasswordPolicyResponse -> TestTree
responseUpdateAccountPasswordPolicy =
  res
    "UpdateAccountPasswordPolicyResponse"
    "fixture/UpdateAccountPasswordPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateAccountPasswordPolicy)

responseAttachRolePolicy :: AttachRolePolicyResponse -> TestTree
responseAttachRolePolicy =
  res
    "AttachRolePolicyResponse"
    "fixture/AttachRolePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AttachRolePolicy)

responseUpdateSSHPublicKey :: UpdateSSHPublicKeyResponse -> TestTree
responseUpdateSSHPublicKey =
  res
    "UpdateSSHPublicKeyResponse"
    "fixture/UpdateSSHPublicKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateSSHPublicKey)

responseDeleteSSHPublicKey :: DeleteSSHPublicKeyResponse -> TestTree
responseDeleteSSHPublicKey =
  res
    "DeleteSSHPublicKeyResponse"
    "fixture/DeleteSSHPublicKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSSHPublicKey)

responseGetUserPolicy :: GetUserPolicyResponse -> TestTree
responseGetUserPolicy =
  res
    "GetUserPolicyResponse"
    "fixture/GetUserPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetUserPolicy)

responseUpdateServiceSpecificCredential :: UpdateServiceSpecificCredentialResponse -> TestTree
responseUpdateServiceSpecificCredential =
  res
    "UpdateServiceSpecificCredentialResponse"
    "fixture/UpdateServiceSpecificCredentialResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateServiceSpecificCredential)

responseDeleteServiceSpecificCredential :: DeleteServiceSpecificCredentialResponse -> TestTree
responseDeleteServiceSpecificCredential =
  res
    "DeleteServiceSpecificCredentialResponse"
    "fixture/DeleteServiceSpecificCredentialResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteServiceSpecificCredential)

responseListAttachedRolePolicies :: ListAttachedRolePoliciesResponse -> TestTree
responseListAttachedRolePolicies =
  res
    "ListAttachedRolePoliciesResponse"
    "fixture/ListAttachedRolePoliciesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListAttachedRolePolicies)

responseGetRole :: GetRoleResponse -> TestTree
responseGetRole =
  res
    "GetRoleResponse"
    "fixture/GetRoleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRole)

responseDeactivateMFADevice :: DeactivateMFADeviceResponse -> TestTree
responseDeactivateMFADevice =
  res
    "DeactivateMFADeviceResponse"
    "fixture/DeactivateMFADeviceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeactivateMFADevice)

responseCreateOpenIDConnectProvider :: CreateOpenIDConnectProviderResponse -> TestTree
responseCreateOpenIDConnectProvider =
  res
    "CreateOpenIDConnectProviderResponse"
    "fixture/CreateOpenIDConnectProviderResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateOpenIDConnectProvider)

responseDeleteVirtualMFADevice :: DeleteVirtualMFADeviceResponse -> TestTree
responseDeleteVirtualMFADevice =
  res
    "DeleteVirtualMFADeviceResponse"
    "fixture/DeleteVirtualMFADeviceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteVirtualMFADevice)

responseListRoles :: ListRolesResponse -> TestTree
responseListRoles =
  res
    "ListRolesResponse"
    "fixture/ListRolesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListRoles)

responseListUserPolicies :: ListUserPoliciesResponse -> TestTree
responseListUserPolicies =
  res
    "ListUserPoliciesResponse"
    "fixture/ListUserPoliciesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListUserPolicies)

responsePutRolePermissionsBoundary :: PutRolePermissionsBoundaryResponse -> TestTree
responsePutRolePermissionsBoundary =
  res
    "PutRolePermissionsBoundaryResponse"
    "fixture/PutRolePermissionsBoundaryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutRolePermissionsBoundary)

responseUploadSSHPublicKey :: UploadSSHPublicKeyResponse -> TestTree
responseUploadSSHPublicKey =
  res
    "UploadSSHPublicKeyResponse"
    "fixture/UploadSSHPublicKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UploadSSHPublicKey)

responseDeleteRolePermissionsBoundary :: DeleteRolePermissionsBoundaryResponse -> TestTree
responseDeleteRolePermissionsBoundary =
  res
    "DeleteRolePermissionsBoundaryResponse"
    "fixture/DeleteRolePermissionsBoundaryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteRolePermissionsBoundary)

responseSimulateCustomPolicy :: SimulatePolicyResponse -> TestTree
responseSimulateCustomPolicy =
  res
    "SimulateCustomPolicyResponse"
    "fixture/SimulateCustomPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SimulateCustomPolicy)

responseUpdateRole :: UpdateRoleResponse -> TestTree
responseUpdateRole =
  res
    "UpdateRoleResponse"
    "fixture/UpdateRoleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateRole)

responseDeleteRole :: DeleteRoleResponse -> TestTree
responseDeleteRole =
  res
    "DeleteRoleResponse"
    "fixture/DeleteRoleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteRole)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListUsers)

responseUpdateOpenIDConnectProviderThumbprint :: UpdateOpenIDConnectProviderThumbprintResponse -> TestTree
responseUpdateOpenIDConnectProviderThumbprint =
  res
    "UpdateOpenIDConnectProviderThumbprintResponse"
    "fixture/UpdateOpenIDConnectProviderThumbprintResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateOpenIDConnectProviderThumbprint)

responsePutUserPolicy :: PutUserPolicyResponse -> TestTree
responsePutUserPolicy =
  res
    "PutUserPolicyResponse"
    "fixture/PutUserPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutUserPolicy)

responseGetSSHPublicKey :: GetSSHPublicKeyResponse -> TestTree
responseGetSSHPublicKey =
  res
    "GetSSHPublicKeyResponse"
    "fixture/GetSSHPublicKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSSHPublicKey)

responseUntagUser :: UntagUserResponse -> TestTree
responseUntagUser =
  res
    "UntagUserResponse"
    "fixture/UntagUserResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagUser)

responseDetachGroupPolicy :: DetachGroupPolicyResponse -> TestTree
responseDetachGroupPolicy =
  res
    "DetachGroupPolicyResponse"
    "fixture/DetachGroupPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetachGroupPolicy)

responseGetOpenIDConnectProvider :: GetOpenIDConnectProviderResponse -> TestTree
responseGetOpenIDConnectProvider =
  res
    "GetOpenIDConnectProviderResponse"
    "fixture/GetOpenIDConnectProviderResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetOpenIDConnectProvider)

responsePutUserPermissionsBoundary :: PutUserPermissionsBoundaryResponse -> TestTree
responsePutUserPermissionsBoundary =
  res
    "PutUserPermissionsBoundaryResponse"
    "fixture/PutUserPermissionsBoundaryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutUserPermissionsBoundary)

responseDeleteUserPolicy :: DeleteUserPolicyResponse -> TestTree
responseDeleteUserPolicy =
  res
    "DeleteUserPolicyResponse"
    "fixture/DeleteUserPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteUserPolicy)

responseDeleteUserPermissionsBoundary :: DeleteUserPermissionsBoundaryResponse -> TestTree
responseDeleteUserPermissionsBoundary =
  res
    "DeleteUserPermissionsBoundaryResponse"
    "fixture/DeleteUserPermissionsBoundaryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteUserPermissionsBoundary)

responseCreateRole :: CreateRoleResponse -> TestTree
responseCreateRole =
  res
    "CreateRoleResponse"
    "fixture/CreateRoleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateRole)

responseResetServiceSpecificCredential :: ResetServiceSpecificCredentialResponse -> TestTree
responseResetServiceSpecificCredential =
  res
    "ResetServiceSpecificCredentialResponse"
    "fixture/ResetServiceSpecificCredentialResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ResetServiceSpecificCredential)

responseGetCredentialReport :: GetCredentialReportResponse -> TestTree
responseGetCredentialReport =
  res
    "GetCredentialReportResponse"
    "fixture/GetCredentialReportResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCredentialReport)

responseGetAccountSummary :: GetAccountSummaryResponse -> TestTree
responseGetAccountSummary =
  res
    "GetAccountSummaryResponse"
    "fixture/GetAccountSummaryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAccountSummary)

responseGenerateServiceLastAccessedDetails :: GenerateServiceLastAccessedDetailsResponse -> TestTree
responseGenerateServiceLastAccessedDetails =
  res
    "GenerateServiceLastAccessedDetailsResponse"
    "fixture/GenerateServiceLastAccessedDetailsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GenerateServiceLastAccessedDetails)

responseListGroupPolicies :: ListGroupPoliciesResponse -> TestTree
responseListGroupPolicies =
  res
    "ListGroupPoliciesResponse"
    "fixture/ListGroupPoliciesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListGroupPolicies)

responseDeletePolicyVersion :: DeletePolicyVersionResponse -> TestTree
responseDeletePolicyVersion =
  res
    "DeletePolicyVersionResponse"
    "fixture/DeletePolicyVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeletePolicyVersion)

responseTagUser :: TagUserResponse -> TestTree
responseTagUser =
  res
    "TagUserResponse"
    "fixture/TagUserResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagUser)

responseDeleteInstanceProfile :: DeleteInstanceProfileResponse -> TestTree
responseDeleteInstanceProfile =
  res
    "DeleteInstanceProfileResponse"
    "fixture/DeleteInstanceProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteInstanceProfile)

responseDetachRolePolicy :: DetachRolePolicyResponse -> TestTree
responseDetachRolePolicy =
  res
    "DetachRolePolicyResponse"
    "fixture/DetachRolePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetachRolePolicy)

responseRemoveRoleFromInstanceProfile :: RemoveRoleFromInstanceProfileResponse -> TestTree
responseRemoveRoleFromInstanceProfile =
  res
    "RemoveRoleFromInstanceProfileResponse"
    "fixture/RemoveRoleFromInstanceProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RemoveRoleFromInstanceProfile)

responseCreatePolicyVersion :: CreatePolicyVersionResponse -> TestTree
responseCreatePolicyVersion =
  res
    "CreatePolicyVersionResponse"
    "fixture/CreatePolicyVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreatePolicyVersion)

responseCreateInstanceProfile :: CreateInstanceProfileResponse -> TestTree
responseCreateInstanceProfile =
  res
    "CreateInstanceProfileResponse"
    "fixture/CreateInstanceProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateInstanceProfile)

responseCreateSAMLProvider :: CreateSAMLProviderResponse -> TestTree
responseCreateSAMLProvider =
  res
    "CreateSAMLProviderResponse"
    "fixture/CreateSAMLProviderResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateSAMLProvider)

responseGetAccountAuthorizationDetails :: GetAccountAuthorizationDetailsResponse -> TestTree
responseGetAccountAuthorizationDetails =
  res
    "GetAccountAuthorizationDetailsResponse"
    "fixture/GetAccountAuthorizationDetailsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAccountAuthorizationDetails)

responseGetServiceLinkedRoleDeletionStatus :: GetServiceLinkedRoleDeletionStatusResponse -> TestTree
responseGetServiceLinkedRoleDeletionStatus =
  res
    "GetServiceLinkedRoleDeletionStatusResponse"
    "fixture/GetServiceLinkedRoleDeletionStatusResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetServiceLinkedRoleDeletionStatus)

responseDeleteAccountAlias :: DeleteAccountAliasResponse -> TestTree
responseDeleteAccountAlias =
  res
    "DeleteAccountAliasResponse"
    "fixture/DeleteAccountAliasResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteAccountAlias)

responseDetachUserPolicy :: DetachUserPolicyResponse -> TestTree
responseDetachUserPolicy =
  res
    "DetachUserPolicyResponse"
    "fixture/DetachUserPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetachUserPolicy)

responseRemoveUserFromGroup :: RemoveUserFromGroupResponse -> TestTree
responseRemoveUserFromGroup =
  res
    "RemoveUserFromGroupResponse"
    "fixture/RemoveUserFromGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RemoveUserFromGroup)

responseDeleteGroupPolicy :: DeleteGroupPolicyResponse -> TestTree
responseDeleteGroupPolicy =
  res
    "DeleteGroupPolicyResponse"
    "fixture/DeleteGroupPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteGroupPolicy)

responseTagRole :: TagRoleResponse -> TestTree
responseTagRole =
  res
    "TagRoleResponse"
    "fixture/TagRoleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagRole)

responsePutGroupPolicy :: PutGroupPolicyResponse -> TestTree
responsePutGroupPolicy =
  res
    "PutGroupPolicyResponse"
    "fixture/PutGroupPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutGroupPolicy)

responseGetLoginProfile :: GetLoginProfileResponse -> TestTree
responseGetLoginProfile =
  res
    "GetLoginProfileResponse"
    "fixture/GetLoginProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetLoginProfile)

responseGetGroupPolicy :: GetGroupPolicyResponse -> TestTree
responseGetGroupPolicy =
  res
    "GetGroupPolicyResponse"
    "fixture/GetGroupPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetGroupPolicy)

responseGenerateOrganizationsAccessReport :: GenerateOrganizationsAccessReportResponse -> TestTree
responseGenerateOrganizationsAccessReport =
  res
    "GenerateOrganizationsAccessReportResponse"
    "fixture/GenerateOrganizationsAccessReportResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GenerateOrganizationsAccessReport)

responseChangePassword :: ChangePasswordResponse -> TestTree
responseChangePassword =
  res
    "ChangePasswordResponse"
    "fixture/ChangePasswordResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ChangePassword)

responseListServerCertificates :: ListServerCertificatesResponse -> TestTree
responseListServerCertificates =
  res
    "ListServerCertificatesResponse"
    "fixture/ListServerCertificatesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListServerCertificates)

responseDeleteServiceLinkedRole :: DeleteServiceLinkedRoleResponse -> TestTree
responseDeleteServiceLinkedRole =
  res
    "DeleteServiceLinkedRoleResponse"
    "fixture/DeleteServiceLinkedRoleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteServiceLinkedRole)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeletePolicy)

responseUpdateAssumeRolePolicy :: UpdateAssumeRolePolicyResponse -> TestTree
responseUpdateAssumeRolePolicy =
  res
    "UpdateAssumeRolePolicyResponse"
    "fixture/UpdateAssumeRolePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateAssumeRolePolicy)

responseGetServiceLastAccessedDetailsWithEntities :: GetServiceLastAccessedDetailsWithEntitiesResponse -> TestTree
responseGetServiceLastAccessedDetailsWithEntities =
  res
    "GetServiceLastAccessedDetailsWithEntitiesResponse"
    "fixture/GetServiceLastAccessedDetailsWithEntitiesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetServiceLastAccessedDetailsWithEntities)

responseGetInstanceProfile :: GetInstanceProfileResponse -> TestTree
responseGetInstanceProfile =
  res
    "GetInstanceProfileResponse"
    "fixture/GetInstanceProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetInstanceProfile)

responseCreateLoginProfile :: CreateLoginProfileResponse -> TestTree
responseCreateLoginProfile =
  res
    "CreateLoginProfileResponse"
    "fixture/CreateLoginProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateLoginProfile)

responseGetSAMLProvider :: GetSAMLProviderResponse -> TestTree
responseGetSAMLProvider =
  res
    "GetSAMLProviderResponse"
    "fixture/GetSAMLProviderResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSAMLProvider)

responseAddRoleToInstanceProfile :: AddRoleToInstanceProfileResponse -> TestTree
responseAddRoleToInstanceProfile =
  res
    "AddRoleToInstanceProfileResponse"
    "fixture/AddRoleToInstanceProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddRoleToInstanceProfile)

responseListGroupsForUser :: ListGroupsForUserResponse -> TestTree
responseListGroupsForUser =
  res
    "ListGroupsForUserResponse"
    "fixture/ListGroupsForUserResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListGroupsForUser)

responseListEntitiesForPolicy :: ListEntitiesForPolicyResponse -> TestTree
responseListEntitiesForPolicy =
  res
    "ListEntitiesForPolicyResponse"
    "fixture/ListEntitiesForPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListEntitiesForPolicy)

responseAddUserToGroup :: AddUserToGroupResponse -> TestTree
responseAddUserToGroup =
  res
    "AddUserToGroupResponse"
    "fixture/AddUserToGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddUserToGroup)

responseSimulatePrincipalPolicy :: SimulatePolicyResponse -> TestTree
responseSimulatePrincipalPolicy =
  res
    "SimulatePrincipalPolicyResponse"
    "fixture/SimulatePrincipalPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SimulatePrincipalPolicy)

responseGetOrganizationsAccessReport :: GetOrganizationsAccessReportResponse -> TestTree
responseGetOrganizationsAccessReport =
  res
    "GetOrganizationsAccessReportResponse"
    "fixture/GetOrganizationsAccessReportResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetOrganizationsAccessReport)

responseGetPolicyVersion :: GetPolicyVersionResponse -> TestTree
responseGetPolicyVersion =
  res
    "GetPolicyVersionResponse"
    "fixture/GetPolicyVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetPolicyVersion)

responseCreateServiceLinkedRole :: CreateServiceLinkedRoleResponse -> TestTree
responseCreateServiceLinkedRole =
  res
    "CreateServiceLinkedRoleResponse"
    "fixture/CreateServiceLinkedRoleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateServiceLinkedRole)

responseListServiceSpecificCredentials :: ListServiceSpecificCredentialsResponse -> TestTree
responseListServiceSpecificCredentials =
  res
    "ListServiceSpecificCredentialsResponse"
    "fixture/ListServiceSpecificCredentialsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListServiceSpecificCredentials)

responseDeleteOpenIDConnectProvider :: DeleteOpenIDConnectProviderResponse -> TestTree
responseDeleteOpenIDConnectProvider =
  res
    "DeleteOpenIDConnectProviderResponse"
    "fixture/DeleteOpenIDConnectProviderResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteOpenIDConnectProvider)

responseGetUser :: GetUserResponse -> TestTree
responseGetUser =
  res
    "GetUserResponse"
    "fixture/GetUserResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetUser)

responseListSigningCertificates :: ListSigningCertificatesResponse -> TestTree
responseListSigningCertificates =
  res
    "ListSigningCertificatesResponse"
    "fixture/ListSigningCertificatesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListSigningCertificates)

responseDeleteSigningCertificate :: DeleteSigningCertificateResponse -> TestTree
responseDeleteSigningCertificate =
  res
    "DeleteSigningCertificateResponse"
    "fixture/DeleteSigningCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSigningCertificate)

responseUpdateSigningCertificate :: UpdateSigningCertificateResponse -> TestTree
responseUpdateSigningCertificate =
  res
    "UpdateSigningCertificateResponse"
    "fixture/UpdateSigningCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateSigningCertificate)

responseListAttachedUserPolicies :: ListAttachedUserPoliciesResponse -> TestTree
responseListAttachedUserPolicies =
  res
    "ListAttachedUserPoliciesResponse"
    "fixture/ListAttachedUserPoliciesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListAttachedUserPolicies)

responseRemoveClientIDFromOpenIDConnectProvider :: RemoveClientIDFromOpenIDConnectProviderResponse -> TestTree
responseRemoveClientIDFromOpenIDConnectProvider =
  res
    "RemoveClientIDFromOpenIDConnectProviderResponse"
    "fixture/RemoveClientIDFromOpenIDConnectProviderResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RemoveClientIDFromOpenIDConnectProvider)

responseAttachUserPolicy :: AttachUserPolicyResponse -> TestTree
responseAttachUserPolicy =
  res
    "AttachUserPolicyResponse"
    "fixture/AttachUserPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AttachUserPolicy)

responseCreateServiceSpecificCredential :: CreateServiceSpecificCredentialResponse -> TestTree
responseCreateServiceSpecificCredential =
  res
    "CreateServiceSpecificCredentialResponse"
    "fixture/CreateServiceSpecificCredentialResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateServiceSpecificCredential)

responseListVirtualMFADevices :: ListVirtualMFADevicesResponse -> TestTree
responseListVirtualMFADevices =
  res
    "ListVirtualMFADevicesResponse"
    "fixture/ListVirtualMFADevicesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListVirtualMFADevices)

responseResyncMFADevice :: ResyncMFADeviceResponse -> TestTree
responseResyncMFADevice =
  res
    "ResyncMFADeviceResponse"
    "fixture/ResyncMFADeviceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ResyncMFADevice)

responseDeleteAccessKey :: DeleteAccessKeyResponse -> TestTree
responseDeleteAccessKey =
  res
    "DeleteAccessKeyResponse"
    "fixture/DeleteAccessKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteAccessKey)

responseUpdateAccessKey :: UpdateAccessKeyResponse -> TestTree
responseUpdateAccessKey =
  res
    "UpdateAccessKeyResponse"
    "fixture/UpdateAccessKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateAccessKey)

responseListUserTags :: ListUserTagsResponse -> TestTree
responseListUserTags =
  res
    "ListUserTagsResponse"
    "fixture/ListUserTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListUserTags)

responseListAccessKeys :: ListAccessKeysResponse -> TestTree
responseListAccessKeys =
  res
    "ListAccessKeysResponse"
    "fixture/ListAccessKeysResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListAccessKeys)

responseGetRolePolicy :: GetRolePolicyResponse -> TestTree
responseGetRolePolicy =
  res
    "GetRolePolicyResponse"
    "fixture/GetRolePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRolePolicy)

responseSetSecurityTokenServicePreferences :: SetSecurityTokenServicePreferencesResponse -> TestTree
responseSetSecurityTokenServicePreferences =
  res
    "SetSecurityTokenServicePreferencesResponse"
    "fixture/SetSecurityTokenServicePreferencesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetSecurityTokenServicePreferences)

responseUntagRole :: UntagRoleResponse -> TestTree
responseUntagRole =
  res
    "UntagRoleResponse"
    "fixture/UntagRoleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagRole)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateUser)

responsePutRolePolicy :: PutRolePolicyResponse -> TestTree
responsePutRolePolicy =
  res
    "PutRolePolicyResponse"
    "fixture/PutRolePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutRolePolicy)

responseGetContextKeysForCustomPolicy :: GetContextKeysForPolicyResponse -> TestTree
responseGetContextKeysForCustomPolicy =
  res
    "GetContextKeysForCustomPolicyResponse"
    "fixture/GetContextKeysForCustomPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetContextKeysForCustomPolicy)

responseUploadSigningCertificate :: UploadSigningCertificateResponse -> TestTree
responseUploadSigningCertificate =
  res
    "UploadSigningCertificateResponse"
    "fixture/UploadSigningCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UploadSigningCertificate)

responseDeleteRolePolicy :: DeleteRolePolicyResponse -> TestTree
responseDeleteRolePolicy =
  res
    "DeleteRolePolicyResponse"
    "fixture/DeleteRolePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteRolePolicy)

responseGetAccountPasswordPolicy :: GetAccountPasswordPolicyResponse -> TestTree
responseGetAccountPasswordPolicy =
  res
    "GetAccountPasswordPolicyResponse"
    "fixture/GetAccountPasswordPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAccountPasswordPolicy)

responseGetAccessKeyLastUsed :: GetAccessKeyLastUsedResponse -> TestTree
responseGetAccessKeyLastUsed =
  res
    "GetAccessKeyLastUsedResponse"
    "fixture/GetAccessKeyLastUsedResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAccessKeyLastUsed)

responseUpdateUser :: UpdateUserResponse -> TestTree
responseUpdateUser =
  res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateUser)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteUser)

responseAddClientIDToOpenIDConnectProvider :: AddClientIDToOpenIDConnectProviderResponse -> TestTree
responseAddClientIDToOpenIDConnectProvider =
  res
    "AddClientIDToOpenIDConnectProviderResponse"
    "fixture/AddClientIDToOpenIDConnectProviderResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddClientIDToOpenIDConnectProvider)

responseListRolePolicies :: ListRolePoliciesResponse -> TestTree
responseListRolePolicies =
  res
    "ListRolePoliciesResponse"
    "fixture/ListRolePoliciesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListRolePolicies)

responseCreateAccountAlias :: CreateAccountAliasResponse -> TestTree
responseCreateAccountAlias =
  res
    "CreateAccountAliasResponse"
    "fixture/CreateAccountAliasResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateAccountAlias)

responseListPoliciesGrantingServiceAccess :: ListPoliciesGrantingServiceAccessResponse -> TestTree
responseListPoliciesGrantingServiceAccess =
  res
    "ListPoliciesGrantingServiceAccessResponse"
    "fixture/ListPoliciesGrantingServiceAccessResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListPoliciesGrantingServiceAccess)

responseListInstanceProfiles :: ListInstanceProfilesResponse -> TestTree
responseListInstanceProfiles =
  res
    "ListInstanceProfilesResponse"
    "fixture/ListInstanceProfilesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListInstanceProfiles)

responseEnableMFADevice :: EnableMFADeviceResponse -> TestTree
responseEnableMFADevice =
  res
    "EnableMFADeviceResponse"
    "fixture/EnableMFADeviceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy EnableMFADevice)

responseListAccountAliases :: ListAccountAliasesResponse -> TestTree
responseListAccountAliases =
  res
    "ListAccountAliasesResponse"
    "fixture/ListAccountAliasesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListAccountAliases)

responseDeleteSAMLProvider :: DeleteSAMLProviderResponse -> TestTree
responseDeleteSAMLProvider =
  res
    "DeleteSAMLProviderResponse"
    "fixture/DeleteSAMLProviderResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSAMLProvider)

responseUpdateSAMLProvider :: UpdateSAMLProviderResponse -> TestTree
responseUpdateSAMLProvider =
  res
    "UpdateSAMLProviderResponse"
    "fixture/UpdateSAMLProviderResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateSAMLProvider)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup =
  res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateGroup)

responseListMFADevices :: ListMFADevicesResponse -> TestTree
responseListMFADevices =
  res
    "ListMFADevicesResponse"
    "fixture/ListMFADevicesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListMFADevices)

responseUploadServerCertificate :: UploadServerCertificateResponse -> TestTree
responseUploadServerCertificate =
  res
    "UploadServerCertificateResponse"
    "fixture/UploadServerCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UploadServerCertificate)

responseSetDefaultPolicyVersion :: SetDefaultPolicyVersionResponse -> TestTree
responseSetDefaultPolicyVersion =
  res
    "SetDefaultPolicyVersionResponse"
    "fixture/SetDefaultPolicyVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetDefaultPolicyVersion)

responseListPolicyVersions :: ListPolicyVersionsResponse -> TestTree
responseListPolicyVersions =
  res
    "ListPolicyVersionsResponse"
    "fixture/ListPolicyVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListPolicyVersions)

responseUpdateRoleDescription :: UpdateRoleDescriptionResponse -> TestTree
responseUpdateRoleDescription =
  res
    "UpdateRoleDescriptionResponse"
    "fixture/UpdateRoleDescriptionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateRoleDescription)

responseListSAMLProviders :: ListSAMLProvidersResponse -> TestTree
responseListSAMLProviders =
  res
    "ListSAMLProvidersResponse"
    "fixture/ListSAMLProvidersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListSAMLProviders)

responseGetServiceLastAccessedDetails :: GetServiceLastAccessedDetailsResponse -> TestTree
responseGetServiceLastAccessedDetails =
  res
    "GetServiceLastAccessedDetailsResponse"
    "fixture/GetServiceLastAccessedDetailsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetServiceLastAccessedDetails)

responseGetServerCertificate :: GetServerCertificateResponse -> TestTree
responseGetServerCertificate =
  res
    "GetServerCertificateResponse"
    "fixture/GetServerCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetServerCertificate)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteGroup)

responseUpdateGroup :: UpdateGroupResponse -> TestTree
responseUpdateGroup =
  res
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateGroup)

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups =
  res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListGroups)

responseGenerateCredentialReport :: GenerateCredentialReportResponse -> TestTree
responseGenerateCredentialReport =
  res
    "GenerateCredentialReportResponse"
    "fixture/GenerateCredentialReportResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GenerateCredentialReport)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetPolicy)

responseUpdateLoginProfile :: UpdateLoginProfileResponse -> TestTree
responseUpdateLoginProfile =
  res
    "UpdateLoginProfileResponse"
    "fixture/UpdateLoginProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateLoginProfile)

responseDeleteLoginProfile :: DeleteLoginProfileResponse -> TestTree
responseDeleteLoginProfile =
  res
    "DeleteLoginProfileResponse"
    "fixture/DeleteLoginProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteLoginProfile)

responseGetGroup :: GetGroupResponse -> TestTree
responseGetGroup =
  res
    "GetGroupResponse"
    "fixture/GetGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetGroup)

responseDeleteServerCertificate :: DeleteServerCertificateResponse -> TestTree
responseDeleteServerCertificate =
  res
    "DeleteServerCertificateResponse"
    "fixture/DeleteServerCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteServerCertificate)

responseUpdateServerCertificate :: UpdateServerCertificateResponse -> TestTree
responseUpdateServerCertificate =
  res
    "UpdateServerCertificateResponse"
    "fixture/UpdateServerCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateServerCertificate)

responseListAttachedGroupPolicies :: ListAttachedGroupPoliciesResponse -> TestTree
responseListAttachedGroupPolicies =
  res
    "ListAttachedGroupPoliciesResponse"
    "fixture/ListAttachedGroupPoliciesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListAttachedGroupPolicies)
