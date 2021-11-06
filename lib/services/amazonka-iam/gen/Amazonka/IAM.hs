{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.IAM
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2010-05-08@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Identity and Access Management
--
-- Identity and Access Management (IAM) is a web service for securely
-- controlling access to Amazon Web Services services. With IAM, you can
-- centrally manage users, security credentials such as access keys, and
-- permissions that control which Amazon Web Services resources users and
-- applications can access. For more information about IAM, see
-- <http://aws.amazon.com/iam/ Identity and Access Management (IAM)> and
-- the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/ Identity and Access Management User Guide>.
module Amazonka.IAM
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** CredentialReportNotPresentException
    _CredentialReportNotPresentException,

    -- ** CredentialReportNotReadyException
    _CredentialReportNotReadyException,

    -- ** MalformedPolicyDocumentException
    _MalformedPolicyDocumentException,

    -- ** EntityAlreadyExistsException
    _EntityAlreadyExistsException,

    -- ** MalformedCertificateException
    _MalformedCertificateException,

    -- ** CredentialReportExpiredException
    _CredentialReportExpiredException,

    -- ** UnmodifiableEntityException
    _UnmodifiableEntityException,

    -- ** DuplicateCertificateException
    _DuplicateCertificateException,

    -- ** DeleteConflictException
    _DeleteConflictException,

    -- ** NoSuchEntityException
    _NoSuchEntityException,

    -- ** InvalidCertificateException
    _InvalidCertificateException,

    -- ** PolicyNotAttachableException
    _PolicyNotAttachableException,

    -- ** ServiceNotSupportedException
    _ServiceNotSupportedException,

    -- ** UnrecognizedPublicKeyEncodingException
    _UnrecognizedPublicKeyEncodingException,

    -- ** ReportGenerationLimitExceededException
    _ReportGenerationLimitExceededException,

    -- ** InvalidUserTypeException
    _InvalidUserTypeException,

    -- ** ServiceFailureException
    _ServiceFailureException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** InvalidPublicKeyException
    _InvalidPublicKeyException,

    -- ** InvalidAuthenticationCodeException
    _InvalidAuthenticationCodeException,

    -- ** EntityTemporarilyUnmodifiableException
    _EntityTemporarilyUnmodifiableException,

    -- ** DuplicateSSHPublicKeyException
    _DuplicateSSHPublicKeyException,

    -- ** KeyPairMismatchException
    _KeyPairMismatchException,

    -- ** PolicyEvaluationException
    _PolicyEvaluationException,

    -- ** PasswordPolicyViolationException
    _PasswordPolicyViolationException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- ** InstanceProfileExists
    newInstanceProfileExists,

    -- ** UserExists
    newUserExists,

    -- ** RoleExists
    newRoleExists,

    -- ** PolicyExists
    newPolicyExists,

    -- * Operations
    -- $operations

    -- ** GetContextKeysForPrincipalPolicy
    GetContextKeysForPrincipalPolicy (GetContextKeysForPrincipalPolicy'),
    newGetContextKeysForPrincipalPolicy,
    GetContextKeysForPolicyResponse (GetContextKeysForPolicyResponse'),
    newGetContextKeysForPolicyResponse,

    -- ** ListPolicies (Paginated)
    ListPolicies (ListPolicies'),
    newListPolicies,
    ListPoliciesResponse (ListPoliciesResponse'),
    newListPoliciesResponse,

    -- ** CreatePolicy
    CreatePolicy (CreatePolicy'),
    newCreatePolicy,
    CreatePolicyResponse (CreatePolicyResponse'),
    newCreatePolicyResponse,

    -- ** ListInstanceProfilesForRole (Paginated)
    ListInstanceProfilesForRole (ListInstanceProfilesForRole'),
    newListInstanceProfilesForRole,
    ListInstanceProfilesForRoleResponse (ListInstanceProfilesForRoleResponse'),
    newListInstanceProfilesForRoleResponse,

    -- ** AttachGroupPolicy
    AttachGroupPolicy (AttachGroupPolicy'),
    newAttachGroupPolicy,
    AttachGroupPolicyResponse (AttachGroupPolicyResponse'),
    newAttachGroupPolicyResponse,

    -- ** CreateAccessKey
    CreateAccessKey (CreateAccessKey'),
    newCreateAccessKey,
    CreateAccessKeyResponse (CreateAccessKeyResponse'),
    newCreateAccessKeyResponse,

    -- ** ListRoleTags
    ListRoleTags (ListRoleTags'),
    newListRoleTags,
    ListRoleTagsResponse (ListRoleTagsResponse'),
    newListRoleTagsResponse,

    -- ** ListSSHPublicKeys (Paginated)
    ListSSHPublicKeys (ListSSHPublicKeys'),
    newListSSHPublicKeys,
    ListSSHPublicKeysResponse (ListSSHPublicKeysResponse'),
    newListSSHPublicKeysResponse,

    -- ** UntagOpenIDConnectProvider
    UntagOpenIDConnectProvider (UntagOpenIDConnectProvider'),
    newUntagOpenIDConnectProvider,
    UntagOpenIDConnectProviderResponse (UntagOpenIDConnectProviderResponse'),
    newUntagOpenIDConnectProviderResponse,

    -- ** ListOpenIDConnectProviders
    ListOpenIDConnectProviders (ListOpenIDConnectProviders'),
    newListOpenIDConnectProviders,
    ListOpenIDConnectProvidersResponse (ListOpenIDConnectProvidersResponse'),
    newListOpenIDConnectProvidersResponse,

    -- ** CreateVirtualMFADevice
    CreateVirtualMFADevice (CreateVirtualMFADevice'),
    newCreateVirtualMFADevice,
    CreateVirtualMFADeviceResponse (CreateVirtualMFADeviceResponse'),
    newCreateVirtualMFADeviceResponse,

    -- ** DeleteAccountPasswordPolicy
    DeleteAccountPasswordPolicy (DeleteAccountPasswordPolicy'),
    newDeleteAccountPasswordPolicy,
    DeleteAccountPasswordPolicyResponse (DeleteAccountPasswordPolicyResponse'),
    newDeleteAccountPasswordPolicyResponse,

    -- ** UpdateAccountPasswordPolicy
    UpdateAccountPasswordPolicy (UpdateAccountPasswordPolicy'),
    newUpdateAccountPasswordPolicy,
    UpdateAccountPasswordPolicyResponse (UpdateAccountPasswordPolicyResponse'),
    newUpdateAccountPasswordPolicyResponse,

    -- ** AttachRolePolicy
    AttachRolePolicy (AttachRolePolicy'),
    newAttachRolePolicy,
    AttachRolePolicyResponse (AttachRolePolicyResponse'),
    newAttachRolePolicyResponse,

    -- ** UpdateSSHPublicKey
    UpdateSSHPublicKey (UpdateSSHPublicKey'),
    newUpdateSSHPublicKey,
    UpdateSSHPublicKeyResponse (UpdateSSHPublicKeyResponse'),
    newUpdateSSHPublicKeyResponse,

    -- ** DeleteSSHPublicKey
    DeleteSSHPublicKey (DeleteSSHPublicKey'),
    newDeleteSSHPublicKey,
    DeleteSSHPublicKeyResponse (DeleteSSHPublicKeyResponse'),
    newDeleteSSHPublicKeyResponse,

    -- ** GetUserPolicy
    GetUserPolicy (GetUserPolicy'),
    newGetUserPolicy,
    GetUserPolicyResponse (GetUserPolicyResponse'),
    newGetUserPolicyResponse,

    -- ** UpdateServiceSpecificCredential
    UpdateServiceSpecificCredential (UpdateServiceSpecificCredential'),
    newUpdateServiceSpecificCredential,
    UpdateServiceSpecificCredentialResponse (UpdateServiceSpecificCredentialResponse'),
    newUpdateServiceSpecificCredentialResponse,

    -- ** DeleteServiceSpecificCredential
    DeleteServiceSpecificCredential (DeleteServiceSpecificCredential'),
    newDeleteServiceSpecificCredential,
    DeleteServiceSpecificCredentialResponse (DeleteServiceSpecificCredentialResponse'),
    newDeleteServiceSpecificCredentialResponse,

    -- ** ListAttachedRolePolicies (Paginated)
    ListAttachedRolePolicies (ListAttachedRolePolicies'),
    newListAttachedRolePolicies,
    ListAttachedRolePoliciesResponse (ListAttachedRolePoliciesResponse'),
    newListAttachedRolePoliciesResponse,

    -- ** GetRole
    GetRole (GetRole'),
    newGetRole,
    GetRoleResponse (GetRoleResponse'),
    newGetRoleResponse,

    -- ** DeactivateMFADevice
    DeactivateMFADevice (DeactivateMFADevice'),
    newDeactivateMFADevice,
    DeactivateMFADeviceResponse (DeactivateMFADeviceResponse'),
    newDeactivateMFADeviceResponse,

    -- ** CreateOpenIDConnectProvider
    CreateOpenIDConnectProvider (CreateOpenIDConnectProvider'),
    newCreateOpenIDConnectProvider,
    CreateOpenIDConnectProviderResponse (CreateOpenIDConnectProviderResponse'),
    newCreateOpenIDConnectProviderResponse,

    -- ** DeleteVirtualMFADevice
    DeleteVirtualMFADevice (DeleteVirtualMFADevice'),
    newDeleteVirtualMFADevice,
    DeleteVirtualMFADeviceResponse (DeleteVirtualMFADeviceResponse'),
    newDeleteVirtualMFADeviceResponse,

    -- ** ListRoles (Paginated)
    ListRoles (ListRoles'),
    newListRoles,
    ListRolesResponse (ListRolesResponse'),
    newListRolesResponse,

    -- ** ListUserPolicies (Paginated)
    ListUserPolicies (ListUserPolicies'),
    newListUserPolicies,
    ListUserPoliciesResponse (ListUserPoliciesResponse'),
    newListUserPoliciesResponse,

    -- ** ListOpenIDConnectProviderTags
    ListOpenIDConnectProviderTags (ListOpenIDConnectProviderTags'),
    newListOpenIDConnectProviderTags,
    ListOpenIDConnectProviderTagsResponse (ListOpenIDConnectProviderTagsResponse'),
    newListOpenIDConnectProviderTagsResponse,

    -- ** PutRolePermissionsBoundary
    PutRolePermissionsBoundary (PutRolePermissionsBoundary'),
    newPutRolePermissionsBoundary,
    PutRolePermissionsBoundaryResponse (PutRolePermissionsBoundaryResponse'),
    newPutRolePermissionsBoundaryResponse,

    -- ** UploadSSHPublicKey
    UploadSSHPublicKey (UploadSSHPublicKey'),
    newUploadSSHPublicKey,
    UploadSSHPublicKeyResponse (UploadSSHPublicKeyResponse'),
    newUploadSSHPublicKeyResponse,

    -- ** DeleteRolePermissionsBoundary
    DeleteRolePermissionsBoundary (DeleteRolePermissionsBoundary'),
    newDeleteRolePermissionsBoundary,
    DeleteRolePermissionsBoundaryResponse (DeleteRolePermissionsBoundaryResponse'),
    newDeleteRolePermissionsBoundaryResponse,

    -- ** SimulateCustomPolicy (Paginated)
    SimulateCustomPolicy (SimulateCustomPolicy'),
    newSimulateCustomPolicy,
    SimulatePolicyResponse (SimulatePolicyResponse'),
    newSimulatePolicyResponse,

    -- ** UpdateRole
    UpdateRole (UpdateRole'),
    newUpdateRole,
    UpdateRoleResponse (UpdateRoleResponse'),
    newUpdateRoleResponse,

    -- ** DeleteRole
    DeleteRole (DeleteRole'),
    newDeleteRole,
    DeleteRoleResponse (DeleteRoleResponse'),
    newDeleteRoleResponse,

    -- ** ListUsers (Paginated)
    ListUsers (ListUsers'),
    newListUsers,
    ListUsersResponse (ListUsersResponse'),
    newListUsersResponse,

    -- ** UpdateOpenIDConnectProviderThumbprint
    UpdateOpenIDConnectProviderThumbprint (UpdateOpenIDConnectProviderThumbprint'),
    newUpdateOpenIDConnectProviderThumbprint,
    UpdateOpenIDConnectProviderThumbprintResponse (UpdateOpenIDConnectProviderThumbprintResponse'),
    newUpdateOpenIDConnectProviderThumbprintResponse,

    -- ** PutUserPolicy
    PutUserPolicy (PutUserPolicy'),
    newPutUserPolicy,
    PutUserPolicyResponse (PutUserPolicyResponse'),
    newPutUserPolicyResponse,

    -- ** TagMFADevice
    TagMFADevice (TagMFADevice'),
    newTagMFADevice,
    TagMFADeviceResponse (TagMFADeviceResponse'),
    newTagMFADeviceResponse,

    -- ** GetSSHPublicKey
    GetSSHPublicKey (GetSSHPublicKey'),
    newGetSSHPublicKey,
    GetSSHPublicKeyResponse (GetSSHPublicKeyResponse'),
    newGetSSHPublicKeyResponse,

    -- ** UntagUser
    UntagUser (UntagUser'),
    newUntagUser,
    UntagUserResponse (UntagUserResponse'),
    newUntagUserResponse,

    -- ** DetachGroupPolicy
    DetachGroupPolicy (DetachGroupPolicy'),
    newDetachGroupPolicy,
    DetachGroupPolicyResponse (DetachGroupPolicyResponse'),
    newDetachGroupPolicyResponse,

    -- ** TagInstanceProfile
    TagInstanceProfile (TagInstanceProfile'),
    newTagInstanceProfile,
    TagInstanceProfileResponse (TagInstanceProfileResponse'),
    newTagInstanceProfileResponse,

    -- ** GetOpenIDConnectProvider
    GetOpenIDConnectProvider (GetOpenIDConnectProvider'),
    newGetOpenIDConnectProvider,
    GetOpenIDConnectProviderResponse (GetOpenIDConnectProviderResponse'),
    newGetOpenIDConnectProviderResponse,

    -- ** PutUserPermissionsBoundary
    PutUserPermissionsBoundary (PutUserPermissionsBoundary'),
    newPutUserPermissionsBoundary,
    PutUserPermissionsBoundaryResponse (PutUserPermissionsBoundaryResponse'),
    newPutUserPermissionsBoundaryResponse,

    -- ** DeleteUserPolicy
    DeleteUserPolicy (DeleteUserPolicy'),
    newDeleteUserPolicy,
    DeleteUserPolicyResponse (DeleteUserPolicyResponse'),
    newDeleteUserPolicyResponse,

    -- ** TagSAMLProvider
    TagSAMLProvider (TagSAMLProvider'),
    newTagSAMLProvider,
    TagSAMLProviderResponse (TagSAMLProviderResponse'),
    newTagSAMLProviderResponse,

    -- ** DeleteUserPermissionsBoundary
    DeleteUserPermissionsBoundary (DeleteUserPermissionsBoundary'),
    newDeleteUserPermissionsBoundary,
    DeleteUserPermissionsBoundaryResponse (DeleteUserPermissionsBoundaryResponse'),
    newDeleteUserPermissionsBoundaryResponse,

    -- ** CreateRole
    CreateRole (CreateRole'),
    newCreateRole,
    CreateRoleResponse (CreateRoleResponse'),
    newCreateRoleResponse,

    -- ** ResetServiceSpecificCredential
    ResetServiceSpecificCredential (ResetServiceSpecificCredential'),
    newResetServiceSpecificCredential,
    ResetServiceSpecificCredentialResponse (ResetServiceSpecificCredentialResponse'),
    newResetServiceSpecificCredentialResponse,

    -- ** UntagSAMLProvider
    UntagSAMLProvider (UntagSAMLProvider'),
    newUntagSAMLProvider,
    UntagSAMLProviderResponse (UntagSAMLProviderResponse'),
    newUntagSAMLProviderResponse,

    -- ** GetCredentialReport
    GetCredentialReport (GetCredentialReport'),
    newGetCredentialReport,
    GetCredentialReportResponse (GetCredentialReportResponse'),
    newGetCredentialReportResponse,

    -- ** ListServerCertificateTags
    ListServerCertificateTags (ListServerCertificateTags'),
    newListServerCertificateTags,
    ListServerCertificateTagsResponse (ListServerCertificateTagsResponse'),
    newListServerCertificateTagsResponse,

    -- ** GetAccountSummary
    GetAccountSummary (GetAccountSummary'),
    newGetAccountSummary,
    GetAccountSummaryResponse (GetAccountSummaryResponse'),
    newGetAccountSummaryResponse,

    -- ** GenerateServiceLastAccessedDetails
    GenerateServiceLastAccessedDetails (GenerateServiceLastAccessedDetails'),
    newGenerateServiceLastAccessedDetails,
    GenerateServiceLastAccessedDetailsResponse (GenerateServiceLastAccessedDetailsResponse'),
    newGenerateServiceLastAccessedDetailsResponse,

    -- ** ListPolicyTags
    ListPolicyTags (ListPolicyTags'),
    newListPolicyTags,
    ListPolicyTagsResponse (ListPolicyTagsResponse'),
    newListPolicyTagsResponse,

    -- ** ListGroupPolicies (Paginated)
    ListGroupPolicies (ListGroupPolicies'),
    newListGroupPolicies,
    ListGroupPoliciesResponse (ListGroupPoliciesResponse'),
    newListGroupPoliciesResponse,

    -- ** DeletePolicyVersion
    DeletePolicyVersion (DeletePolicyVersion'),
    newDeletePolicyVersion,
    DeletePolicyVersionResponse (DeletePolicyVersionResponse'),
    newDeletePolicyVersionResponse,

    -- ** TagUser
    TagUser (TagUser'),
    newTagUser,
    TagUserResponse (TagUserResponse'),
    newTagUserResponse,

    -- ** DeleteInstanceProfile
    DeleteInstanceProfile (DeleteInstanceProfile'),
    newDeleteInstanceProfile,
    DeleteInstanceProfileResponse (DeleteInstanceProfileResponse'),
    newDeleteInstanceProfileResponse,

    -- ** DetachRolePolicy
    DetachRolePolicy (DetachRolePolicy'),
    newDetachRolePolicy,
    DetachRolePolicyResponse (DetachRolePolicyResponse'),
    newDetachRolePolicyResponse,

    -- ** RemoveRoleFromInstanceProfile
    RemoveRoleFromInstanceProfile (RemoveRoleFromInstanceProfile'),
    newRemoveRoleFromInstanceProfile,
    RemoveRoleFromInstanceProfileResponse (RemoveRoleFromInstanceProfileResponse'),
    newRemoveRoleFromInstanceProfileResponse,

    -- ** CreatePolicyVersion
    CreatePolicyVersion (CreatePolicyVersion'),
    newCreatePolicyVersion,
    CreatePolicyVersionResponse (CreatePolicyVersionResponse'),
    newCreatePolicyVersionResponse,

    -- ** CreateInstanceProfile
    CreateInstanceProfile (CreateInstanceProfile'),
    newCreateInstanceProfile,
    CreateInstanceProfileResponse (CreateInstanceProfileResponse'),
    newCreateInstanceProfileResponse,

    -- ** CreateSAMLProvider
    CreateSAMLProvider (CreateSAMLProvider'),
    newCreateSAMLProvider,
    CreateSAMLProviderResponse (CreateSAMLProviderResponse'),
    newCreateSAMLProviderResponse,

    -- ** GetAccountAuthorizationDetails (Paginated)
    GetAccountAuthorizationDetails (GetAccountAuthorizationDetails'),
    newGetAccountAuthorizationDetails,
    GetAccountAuthorizationDetailsResponse (GetAccountAuthorizationDetailsResponse'),
    newGetAccountAuthorizationDetailsResponse,

    -- ** GetServiceLinkedRoleDeletionStatus
    GetServiceLinkedRoleDeletionStatus (GetServiceLinkedRoleDeletionStatus'),
    newGetServiceLinkedRoleDeletionStatus,
    GetServiceLinkedRoleDeletionStatusResponse (GetServiceLinkedRoleDeletionStatusResponse'),
    newGetServiceLinkedRoleDeletionStatusResponse,

    -- ** DeleteAccountAlias
    DeleteAccountAlias (DeleteAccountAlias'),
    newDeleteAccountAlias,
    DeleteAccountAliasResponse (DeleteAccountAliasResponse'),
    newDeleteAccountAliasResponse,

    -- ** DetachUserPolicy
    DetachUserPolicy (DetachUserPolicy'),
    newDetachUserPolicy,
    DetachUserPolicyResponse (DetachUserPolicyResponse'),
    newDetachUserPolicyResponse,

    -- ** RemoveUserFromGroup
    RemoveUserFromGroup (RemoveUserFromGroup'),
    newRemoveUserFromGroup,
    RemoveUserFromGroupResponse (RemoveUserFromGroupResponse'),
    newRemoveUserFromGroupResponse,

    -- ** DeleteGroupPolicy
    DeleteGroupPolicy (DeleteGroupPolicy'),
    newDeleteGroupPolicy,
    DeleteGroupPolicyResponse (DeleteGroupPolicyResponse'),
    newDeleteGroupPolicyResponse,

    -- ** TagRole
    TagRole (TagRole'),
    newTagRole,
    TagRoleResponse (TagRoleResponse'),
    newTagRoleResponse,

    -- ** PutGroupPolicy
    PutGroupPolicy (PutGroupPolicy'),
    newPutGroupPolicy,
    PutGroupPolicyResponse (PutGroupPolicyResponse'),
    newPutGroupPolicyResponse,

    -- ** GetLoginProfile
    GetLoginProfile (GetLoginProfile'),
    newGetLoginProfile,
    GetLoginProfileResponse (GetLoginProfileResponse'),
    newGetLoginProfileResponse,

    -- ** GetGroupPolicy
    GetGroupPolicy (GetGroupPolicy'),
    newGetGroupPolicy,
    GetGroupPolicyResponse (GetGroupPolicyResponse'),
    newGetGroupPolicyResponse,

    -- ** GenerateOrganizationsAccessReport
    GenerateOrganizationsAccessReport (GenerateOrganizationsAccessReport'),
    newGenerateOrganizationsAccessReport,
    GenerateOrganizationsAccessReportResponse (GenerateOrganizationsAccessReportResponse'),
    newGenerateOrganizationsAccessReportResponse,

    -- ** ChangePassword
    ChangePassword (ChangePassword'),
    newChangePassword,
    ChangePasswordResponse (ChangePasswordResponse'),
    newChangePasswordResponse,

    -- ** ListServerCertificates (Paginated)
    ListServerCertificates (ListServerCertificates'),
    newListServerCertificates,
    ListServerCertificatesResponse (ListServerCertificatesResponse'),
    newListServerCertificatesResponse,

    -- ** DeleteServiceLinkedRole
    DeleteServiceLinkedRole (DeleteServiceLinkedRole'),
    newDeleteServiceLinkedRole,
    DeleteServiceLinkedRoleResponse (DeleteServiceLinkedRoleResponse'),
    newDeleteServiceLinkedRoleResponse,

    -- ** DeletePolicy
    DeletePolicy (DeletePolicy'),
    newDeletePolicy,
    DeletePolicyResponse (DeletePolicyResponse'),
    newDeletePolicyResponse,

    -- ** UpdateAssumeRolePolicy
    UpdateAssumeRolePolicy (UpdateAssumeRolePolicy'),
    newUpdateAssumeRolePolicy,
    UpdateAssumeRolePolicyResponse (UpdateAssumeRolePolicyResponse'),
    newUpdateAssumeRolePolicyResponse,

    -- ** GetServiceLastAccessedDetailsWithEntities
    GetServiceLastAccessedDetailsWithEntities (GetServiceLastAccessedDetailsWithEntities'),
    newGetServiceLastAccessedDetailsWithEntities,
    GetServiceLastAccessedDetailsWithEntitiesResponse (GetServiceLastAccessedDetailsWithEntitiesResponse'),
    newGetServiceLastAccessedDetailsWithEntitiesResponse,

    -- ** UntagServerCertificate
    UntagServerCertificate (UntagServerCertificate'),
    newUntagServerCertificate,
    UntagServerCertificateResponse (UntagServerCertificateResponse'),
    newUntagServerCertificateResponse,

    -- ** GetInstanceProfile
    GetInstanceProfile (GetInstanceProfile'),
    newGetInstanceProfile,
    GetInstanceProfileResponse (GetInstanceProfileResponse'),
    newGetInstanceProfileResponse,

    -- ** CreateLoginProfile
    CreateLoginProfile (CreateLoginProfile'),
    newCreateLoginProfile,
    CreateLoginProfileResponse (CreateLoginProfileResponse'),
    newCreateLoginProfileResponse,

    -- ** GetSAMLProvider
    GetSAMLProvider (GetSAMLProvider'),
    newGetSAMLProvider,
    GetSAMLProviderResponse (GetSAMLProviderResponse'),
    newGetSAMLProviderResponse,

    -- ** AddRoleToInstanceProfile
    AddRoleToInstanceProfile (AddRoleToInstanceProfile'),
    newAddRoleToInstanceProfile,
    AddRoleToInstanceProfileResponse (AddRoleToInstanceProfileResponse'),
    newAddRoleToInstanceProfileResponse,

    -- ** ListGroupsForUser (Paginated)
    ListGroupsForUser (ListGroupsForUser'),
    newListGroupsForUser,
    ListGroupsForUserResponse (ListGroupsForUserResponse'),
    newListGroupsForUserResponse,

    -- ** ListEntitiesForPolicy (Paginated)
    ListEntitiesForPolicy (ListEntitiesForPolicy'),
    newListEntitiesForPolicy,
    ListEntitiesForPolicyResponse (ListEntitiesForPolicyResponse'),
    newListEntitiesForPolicyResponse,

    -- ** AddUserToGroup
    AddUserToGroup (AddUserToGroup'),
    newAddUserToGroup,
    AddUserToGroupResponse (AddUserToGroupResponse'),
    newAddUserToGroupResponse,

    -- ** TagOpenIDConnectProvider
    TagOpenIDConnectProvider (TagOpenIDConnectProvider'),
    newTagOpenIDConnectProvider,
    TagOpenIDConnectProviderResponse (TagOpenIDConnectProviderResponse'),
    newTagOpenIDConnectProviderResponse,

    -- ** SimulatePrincipalPolicy (Paginated)
    SimulatePrincipalPolicy (SimulatePrincipalPolicy'),
    newSimulatePrincipalPolicy,
    SimulatePolicyResponse (SimulatePolicyResponse'),
    newSimulatePolicyResponse,

    -- ** GetOrganizationsAccessReport
    GetOrganizationsAccessReport (GetOrganizationsAccessReport'),
    newGetOrganizationsAccessReport,
    GetOrganizationsAccessReportResponse (GetOrganizationsAccessReportResponse'),
    newGetOrganizationsAccessReportResponse,

    -- ** GetPolicyVersion
    GetPolicyVersion (GetPolicyVersion'),
    newGetPolicyVersion,
    GetPolicyVersionResponse (GetPolicyVersionResponse'),
    newGetPolicyVersionResponse,

    -- ** CreateServiceLinkedRole
    CreateServiceLinkedRole (CreateServiceLinkedRole'),
    newCreateServiceLinkedRole,
    CreateServiceLinkedRoleResponse (CreateServiceLinkedRoleResponse'),
    newCreateServiceLinkedRoleResponse,

    -- ** ListServiceSpecificCredentials
    ListServiceSpecificCredentials (ListServiceSpecificCredentials'),
    newListServiceSpecificCredentials,
    ListServiceSpecificCredentialsResponse (ListServiceSpecificCredentialsResponse'),
    newListServiceSpecificCredentialsResponse,

    -- ** DeleteOpenIDConnectProvider
    DeleteOpenIDConnectProvider (DeleteOpenIDConnectProvider'),
    newDeleteOpenIDConnectProvider,
    DeleteOpenIDConnectProviderResponse (DeleteOpenIDConnectProviderResponse'),
    newDeleteOpenIDConnectProviderResponse,

    -- ** GetUser
    GetUser (GetUser'),
    newGetUser,
    GetUserResponse (GetUserResponse'),
    newGetUserResponse,

    -- ** ListSigningCertificates (Paginated)
    ListSigningCertificates (ListSigningCertificates'),
    newListSigningCertificates,
    ListSigningCertificatesResponse (ListSigningCertificatesResponse'),
    newListSigningCertificatesResponse,

    -- ** DeleteSigningCertificate
    DeleteSigningCertificate (DeleteSigningCertificate'),
    newDeleteSigningCertificate,
    DeleteSigningCertificateResponse (DeleteSigningCertificateResponse'),
    newDeleteSigningCertificateResponse,

    -- ** UpdateSigningCertificate
    UpdateSigningCertificate (UpdateSigningCertificate'),
    newUpdateSigningCertificate,
    UpdateSigningCertificateResponse (UpdateSigningCertificateResponse'),
    newUpdateSigningCertificateResponse,

    -- ** ListAttachedUserPolicies (Paginated)
    ListAttachedUserPolicies (ListAttachedUserPolicies'),
    newListAttachedUserPolicies,
    ListAttachedUserPoliciesResponse (ListAttachedUserPoliciesResponse'),
    newListAttachedUserPoliciesResponse,

    -- ** RemoveClientIDFromOpenIDConnectProvider
    RemoveClientIDFromOpenIDConnectProvider (RemoveClientIDFromOpenIDConnectProvider'),
    newRemoveClientIDFromOpenIDConnectProvider,
    RemoveClientIDFromOpenIDConnectProviderResponse (RemoveClientIDFromOpenIDConnectProviderResponse'),
    newRemoveClientIDFromOpenIDConnectProviderResponse,

    -- ** AttachUserPolicy
    AttachUserPolicy (AttachUserPolicy'),
    newAttachUserPolicy,
    AttachUserPolicyResponse (AttachUserPolicyResponse'),
    newAttachUserPolicyResponse,

    -- ** TagPolicy
    TagPolicy (TagPolicy'),
    newTagPolicy,
    TagPolicyResponse (TagPolicyResponse'),
    newTagPolicyResponse,

    -- ** CreateServiceSpecificCredential
    CreateServiceSpecificCredential (CreateServiceSpecificCredential'),
    newCreateServiceSpecificCredential,
    CreateServiceSpecificCredentialResponse (CreateServiceSpecificCredentialResponse'),
    newCreateServiceSpecificCredentialResponse,

    -- ** ListVirtualMFADevices (Paginated)
    ListVirtualMFADevices (ListVirtualMFADevices'),
    newListVirtualMFADevices,
    ListVirtualMFADevicesResponse (ListVirtualMFADevicesResponse'),
    newListVirtualMFADevicesResponse,

    -- ** ResyncMFADevice
    ResyncMFADevice (ResyncMFADevice'),
    newResyncMFADevice,
    ResyncMFADeviceResponse (ResyncMFADeviceResponse'),
    newResyncMFADeviceResponse,

    -- ** TagServerCertificate
    TagServerCertificate (TagServerCertificate'),
    newTagServerCertificate,
    TagServerCertificateResponse (TagServerCertificateResponse'),
    newTagServerCertificateResponse,

    -- ** DeleteAccessKey
    DeleteAccessKey (DeleteAccessKey'),
    newDeleteAccessKey,
    DeleteAccessKeyResponse (DeleteAccessKeyResponse'),
    newDeleteAccessKeyResponse,

    -- ** UpdateAccessKey
    UpdateAccessKey (UpdateAccessKey'),
    newUpdateAccessKey,
    UpdateAccessKeyResponse (UpdateAccessKeyResponse'),
    newUpdateAccessKeyResponse,

    -- ** ListUserTags (Paginated)
    ListUserTags (ListUserTags'),
    newListUserTags,
    ListUserTagsResponse (ListUserTagsResponse'),
    newListUserTagsResponse,

    -- ** ListAccessKeys (Paginated)
    ListAccessKeys (ListAccessKeys'),
    newListAccessKeys,
    ListAccessKeysResponse (ListAccessKeysResponse'),
    newListAccessKeysResponse,

    -- ** GetRolePolicy
    GetRolePolicy (GetRolePolicy'),
    newGetRolePolicy,
    GetRolePolicyResponse (GetRolePolicyResponse'),
    newGetRolePolicyResponse,

    -- ** SetSecurityTokenServicePreferences
    SetSecurityTokenServicePreferences (SetSecurityTokenServicePreferences'),
    newSetSecurityTokenServicePreferences,
    SetSecurityTokenServicePreferencesResponse (SetSecurityTokenServicePreferencesResponse'),
    newSetSecurityTokenServicePreferencesResponse,

    -- ** UntagRole
    UntagRole (UntagRole'),
    newUntagRole,
    UntagRoleResponse (UntagRoleResponse'),
    newUntagRoleResponse,

    -- ** CreateUser
    CreateUser (CreateUser'),
    newCreateUser,
    CreateUserResponse (CreateUserResponse'),
    newCreateUserResponse,

    -- ** PutRolePolicy
    PutRolePolicy (PutRolePolicy'),
    newPutRolePolicy,
    PutRolePolicyResponse (PutRolePolicyResponse'),
    newPutRolePolicyResponse,

    -- ** GetContextKeysForCustomPolicy
    GetContextKeysForCustomPolicy (GetContextKeysForCustomPolicy'),
    newGetContextKeysForCustomPolicy,
    GetContextKeysForPolicyResponse (GetContextKeysForPolicyResponse'),
    newGetContextKeysForPolicyResponse,

    -- ** UploadSigningCertificate
    UploadSigningCertificate (UploadSigningCertificate'),
    newUploadSigningCertificate,
    UploadSigningCertificateResponse (UploadSigningCertificateResponse'),
    newUploadSigningCertificateResponse,

    -- ** DeleteRolePolicy
    DeleteRolePolicy (DeleteRolePolicy'),
    newDeleteRolePolicy,
    DeleteRolePolicyResponse (DeleteRolePolicyResponse'),
    newDeleteRolePolicyResponse,

    -- ** GetAccountPasswordPolicy
    GetAccountPasswordPolicy (GetAccountPasswordPolicy'),
    newGetAccountPasswordPolicy,
    GetAccountPasswordPolicyResponse (GetAccountPasswordPolicyResponse'),
    newGetAccountPasswordPolicyResponse,

    -- ** GetAccessKeyLastUsed
    GetAccessKeyLastUsed (GetAccessKeyLastUsed'),
    newGetAccessKeyLastUsed,
    GetAccessKeyLastUsedResponse (GetAccessKeyLastUsedResponse'),
    newGetAccessKeyLastUsedResponse,

    -- ** UpdateUser
    UpdateUser (UpdateUser'),
    newUpdateUser,
    UpdateUserResponse (UpdateUserResponse'),
    newUpdateUserResponse,

    -- ** DeleteUser
    DeleteUser (DeleteUser'),
    newDeleteUser,
    DeleteUserResponse (DeleteUserResponse'),
    newDeleteUserResponse,

    -- ** AddClientIDToOpenIDConnectProvider
    AddClientIDToOpenIDConnectProvider (AddClientIDToOpenIDConnectProvider'),
    newAddClientIDToOpenIDConnectProvider,
    AddClientIDToOpenIDConnectProviderResponse (AddClientIDToOpenIDConnectProviderResponse'),
    newAddClientIDToOpenIDConnectProviderResponse,

    -- ** ListRolePolicies (Paginated)
    ListRolePolicies (ListRolePolicies'),
    newListRolePolicies,
    ListRolePoliciesResponse (ListRolePoliciesResponse'),
    newListRolePoliciesResponse,

    -- ** CreateAccountAlias
    CreateAccountAlias (CreateAccountAlias'),
    newCreateAccountAlias,
    CreateAccountAliasResponse (CreateAccountAliasResponse'),
    newCreateAccountAliasResponse,

    -- ** ListPoliciesGrantingServiceAccess
    ListPoliciesGrantingServiceAccess (ListPoliciesGrantingServiceAccess'),
    newListPoliciesGrantingServiceAccess,
    ListPoliciesGrantingServiceAccessResponse (ListPoliciesGrantingServiceAccessResponse'),
    newListPoliciesGrantingServiceAccessResponse,

    -- ** ListInstanceProfiles (Paginated)
    ListInstanceProfiles (ListInstanceProfiles'),
    newListInstanceProfiles,
    ListInstanceProfilesResponse (ListInstanceProfilesResponse'),
    newListInstanceProfilesResponse,

    -- ** EnableMFADevice
    EnableMFADevice (EnableMFADevice'),
    newEnableMFADevice,
    EnableMFADeviceResponse (EnableMFADeviceResponse'),
    newEnableMFADeviceResponse,

    -- ** ListAccountAliases (Paginated)
    ListAccountAliases (ListAccountAliases'),
    newListAccountAliases,
    ListAccountAliasesResponse (ListAccountAliasesResponse'),
    newListAccountAliasesResponse,

    -- ** DeleteSAMLProvider
    DeleteSAMLProvider (DeleteSAMLProvider'),
    newDeleteSAMLProvider,
    DeleteSAMLProviderResponse (DeleteSAMLProviderResponse'),
    newDeleteSAMLProviderResponse,

    -- ** UpdateSAMLProvider
    UpdateSAMLProvider (UpdateSAMLProvider'),
    newUpdateSAMLProvider,
    UpdateSAMLProviderResponse (UpdateSAMLProviderResponse'),
    newUpdateSAMLProviderResponse,

    -- ** UntagMFADevice
    UntagMFADevice (UntagMFADevice'),
    newUntagMFADevice,
    UntagMFADeviceResponse (UntagMFADeviceResponse'),
    newUntagMFADeviceResponse,

    -- ** CreateGroup
    CreateGroup (CreateGroup'),
    newCreateGroup,
    CreateGroupResponse (CreateGroupResponse'),
    newCreateGroupResponse,

    -- ** ListMFADevices (Paginated)
    ListMFADevices (ListMFADevices'),
    newListMFADevices,
    ListMFADevicesResponse (ListMFADevicesResponse'),
    newListMFADevicesResponse,

    -- ** UntagInstanceProfile
    UntagInstanceProfile (UntagInstanceProfile'),
    newUntagInstanceProfile,
    UntagInstanceProfileResponse (UntagInstanceProfileResponse'),
    newUntagInstanceProfileResponse,

    -- ** UploadServerCertificate
    UploadServerCertificate (UploadServerCertificate'),
    newUploadServerCertificate,
    UploadServerCertificateResponse (UploadServerCertificateResponse'),
    newUploadServerCertificateResponse,

    -- ** SetDefaultPolicyVersion
    SetDefaultPolicyVersion (SetDefaultPolicyVersion'),
    newSetDefaultPolicyVersion,
    SetDefaultPolicyVersionResponse (SetDefaultPolicyVersionResponse'),
    newSetDefaultPolicyVersionResponse,

    -- ** ListPolicyVersions (Paginated)
    ListPolicyVersions (ListPolicyVersions'),
    newListPolicyVersions,
    ListPolicyVersionsResponse (ListPolicyVersionsResponse'),
    newListPolicyVersionsResponse,

    -- ** UpdateRoleDescription
    UpdateRoleDescription (UpdateRoleDescription'),
    newUpdateRoleDescription,
    UpdateRoleDescriptionResponse (UpdateRoleDescriptionResponse'),
    newUpdateRoleDescriptionResponse,

    -- ** ListSAMLProviders
    ListSAMLProviders (ListSAMLProviders'),
    newListSAMLProviders,
    ListSAMLProvidersResponse (ListSAMLProvidersResponse'),
    newListSAMLProvidersResponse,

    -- ** GetServiceLastAccessedDetails
    GetServiceLastAccessedDetails (GetServiceLastAccessedDetails'),
    newGetServiceLastAccessedDetails,
    GetServiceLastAccessedDetailsResponse (GetServiceLastAccessedDetailsResponse'),
    newGetServiceLastAccessedDetailsResponse,

    -- ** GetServerCertificate
    GetServerCertificate (GetServerCertificate'),
    newGetServerCertificate,
    GetServerCertificateResponse (GetServerCertificateResponse'),
    newGetServerCertificateResponse,

    -- ** DeleteGroup
    DeleteGroup (DeleteGroup'),
    newDeleteGroup,
    DeleteGroupResponse (DeleteGroupResponse'),
    newDeleteGroupResponse,

    -- ** UpdateGroup
    UpdateGroup (UpdateGroup'),
    newUpdateGroup,
    UpdateGroupResponse (UpdateGroupResponse'),
    newUpdateGroupResponse,

    -- ** ListGroups (Paginated)
    ListGroups (ListGroups'),
    newListGroups,
    ListGroupsResponse (ListGroupsResponse'),
    newListGroupsResponse,

    -- ** GenerateCredentialReport
    GenerateCredentialReport (GenerateCredentialReport'),
    newGenerateCredentialReport,
    GenerateCredentialReportResponse (GenerateCredentialReportResponse'),
    newGenerateCredentialReportResponse,

    -- ** GetPolicy
    GetPolicy (GetPolicy'),
    newGetPolicy,
    GetPolicyResponse (GetPolicyResponse'),
    newGetPolicyResponse,

    -- ** ListInstanceProfileTags
    ListInstanceProfileTags (ListInstanceProfileTags'),
    newListInstanceProfileTags,
    ListInstanceProfileTagsResponse (ListInstanceProfileTagsResponse'),
    newListInstanceProfileTagsResponse,

    -- ** UpdateLoginProfile
    UpdateLoginProfile (UpdateLoginProfile'),
    newUpdateLoginProfile,
    UpdateLoginProfileResponse (UpdateLoginProfileResponse'),
    newUpdateLoginProfileResponse,

    -- ** DeleteLoginProfile
    DeleteLoginProfile (DeleteLoginProfile'),
    newDeleteLoginProfile,
    DeleteLoginProfileResponse (DeleteLoginProfileResponse'),
    newDeleteLoginProfileResponse,

    -- ** ListSAMLProviderTags
    ListSAMLProviderTags (ListSAMLProviderTags'),
    newListSAMLProviderTags,
    ListSAMLProviderTagsResponse (ListSAMLProviderTagsResponse'),
    newListSAMLProviderTagsResponse,

    -- ** GetGroup (Paginated)
    GetGroup (GetGroup'),
    newGetGroup,
    GetGroupResponse (GetGroupResponse'),
    newGetGroupResponse,

    -- ** UntagPolicy
    UntagPolicy (UntagPolicy'),
    newUntagPolicy,
    UntagPolicyResponse (UntagPolicyResponse'),
    newUntagPolicyResponse,

    -- ** DeleteServerCertificate
    DeleteServerCertificate (DeleteServerCertificate'),
    newDeleteServerCertificate,
    DeleteServerCertificateResponse (DeleteServerCertificateResponse'),
    newDeleteServerCertificateResponse,

    -- ** UpdateServerCertificate
    UpdateServerCertificate (UpdateServerCertificate'),
    newUpdateServerCertificate,
    UpdateServerCertificateResponse (UpdateServerCertificateResponse'),
    newUpdateServerCertificateResponse,

    -- ** ListAttachedGroupPolicies (Paginated)
    ListAttachedGroupPolicies (ListAttachedGroupPolicies'),
    newListAttachedGroupPolicies,
    ListAttachedGroupPoliciesResponse (ListAttachedGroupPoliciesResponse'),
    newListAttachedGroupPoliciesResponse,

    -- ** ListMFADeviceTags
    ListMFADeviceTags (ListMFADeviceTags'),
    newListMFADeviceTags,
    ListMFADeviceTagsResponse (ListMFADeviceTagsResponse'),
    newListMFADeviceTagsResponse,

    -- * Types

    -- ** AccessAdvisorUsageGranularityType
    AccessAdvisorUsageGranularityType (..),

    -- ** AssignmentStatusType
    AssignmentStatusType (..),

    -- ** ContextKeyTypeEnum
    ContextKeyTypeEnum (..),

    -- ** DeletionTaskStatusType
    DeletionTaskStatusType (..),

    -- ** EncodingType
    EncodingType (..),

    -- ** EntityType
    EntityType (..),

    -- ** GlobalEndpointTokenVersion
    GlobalEndpointTokenVersion (..),

    -- ** JobStatusType
    JobStatusType (..),

    -- ** PermissionsBoundaryAttachmentType
    PermissionsBoundaryAttachmentType (..),

    -- ** PolicyEvaluationDecisionType
    PolicyEvaluationDecisionType (..),

    -- ** PolicyOwnerEntityType
    PolicyOwnerEntityType (..),

    -- ** PolicyScopeType
    PolicyScopeType (..),

    -- ** PolicySourceType
    PolicySourceType (..),

    -- ** PolicyType
    PolicyType (..),

    -- ** PolicyUsageType
    PolicyUsageType (..),

    -- ** ReportFormatType
    ReportFormatType (..),

    -- ** ReportStateType
    ReportStateType (..),

    -- ** SortKeyType
    SortKeyType (..),

    -- ** StatusType
    StatusType (..),

    -- ** SummaryKeyType
    SummaryKeyType (..),

    -- ** AccessDetail
    AccessDetail (AccessDetail'),
    newAccessDetail,

    -- ** AccessKeyInfo
    AccessKeyInfo (AccessKeyInfo'),
    newAccessKeyInfo,

    -- ** AccessKeyLastUsed
    AccessKeyLastUsed (AccessKeyLastUsed'),
    newAccessKeyLastUsed,

    -- ** AccessKeyMetadata
    AccessKeyMetadata (AccessKeyMetadata'),
    newAccessKeyMetadata,

    -- ** AttachedPermissionsBoundary
    AttachedPermissionsBoundary (AttachedPermissionsBoundary'),
    newAttachedPermissionsBoundary,

    -- ** AttachedPolicy
    AttachedPolicy (AttachedPolicy'),
    newAttachedPolicy,

    -- ** ContextEntry
    ContextEntry (ContextEntry'),
    newContextEntry,

    -- ** DeletionTaskFailureReasonType
    DeletionTaskFailureReasonType (DeletionTaskFailureReasonType'),
    newDeletionTaskFailureReasonType,

    -- ** EntityDetails
    EntityDetails (EntityDetails'),
    newEntityDetails,

    -- ** EntityInfo
    EntityInfo (EntityInfo'),
    newEntityInfo,

    -- ** ErrorDetails
    ErrorDetails (ErrorDetails'),
    newErrorDetails,

    -- ** EvaluationResult
    EvaluationResult (EvaluationResult'),
    newEvaluationResult,

    -- ** GetContextKeysForPolicyResponse
    GetContextKeysForPolicyResponse (GetContextKeysForPolicyResponse'),
    newGetContextKeysForPolicyResponse,

    -- ** Group
    Group (Group'),
    newGroup,

    -- ** GroupDetail
    GroupDetail (GroupDetail'),
    newGroupDetail,

    -- ** InstanceProfile
    InstanceProfile (InstanceProfile'),
    newInstanceProfile,

    -- ** ListPoliciesGrantingServiceAccessEntry
    ListPoliciesGrantingServiceAccessEntry (ListPoliciesGrantingServiceAccessEntry'),
    newListPoliciesGrantingServiceAccessEntry,

    -- ** LoginProfile
    LoginProfile (LoginProfile'),
    newLoginProfile,

    -- ** MFADevice
    MFADevice (MFADevice'),
    newMFADevice,

    -- ** ManagedPolicyDetail
    ManagedPolicyDetail (ManagedPolicyDetail'),
    newManagedPolicyDetail,

    -- ** OpenIDConnectProviderListEntry
    OpenIDConnectProviderListEntry (OpenIDConnectProviderListEntry'),
    newOpenIDConnectProviderListEntry,

    -- ** OrganizationsDecisionDetail
    OrganizationsDecisionDetail (OrganizationsDecisionDetail'),
    newOrganizationsDecisionDetail,

    -- ** PasswordPolicy
    PasswordPolicy (PasswordPolicy'),
    newPasswordPolicy,

    -- ** PermissionsBoundaryDecisionDetail
    PermissionsBoundaryDecisionDetail (PermissionsBoundaryDecisionDetail'),
    newPermissionsBoundaryDecisionDetail,

    -- ** Policy
    Policy (Policy'),
    newPolicy,

    -- ** PolicyDetail
    PolicyDetail (PolicyDetail'),
    newPolicyDetail,

    -- ** PolicyGrantingServiceAccess
    PolicyGrantingServiceAccess (PolicyGrantingServiceAccess'),
    newPolicyGrantingServiceAccess,

    -- ** PolicyGroup
    PolicyGroup (PolicyGroup'),
    newPolicyGroup,

    -- ** PolicyRole
    PolicyRole (PolicyRole'),
    newPolicyRole,

    -- ** PolicyUser
    PolicyUser (PolicyUser'),
    newPolicyUser,

    -- ** PolicyVersion
    PolicyVersion (PolicyVersion'),
    newPolicyVersion,

    -- ** Position
    Position (Position'),
    newPosition,

    -- ** ResourceSpecificResult
    ResourceSpecificResult (ResourceSpecificResult'),
    newResourceSpecificResult,

    -- ** Role
    Role (Role'),
    newRole,

    -- ** RoleDetail
    RoleDetail (RoleDetail'),
    newRoleDetail,

    -- ** RoleLastUsed
    RoleLastUsed (RoleLastUsed'),
    newRoleLastUsed,

    -- ** RoleUsageType
    RoleUsageType (RoleUsageType'),
    newRoleUsageType,

    -- ** SAMLProviderListEntry
    SAMLProviderListEntry (SAMLProviderListEntry'),
    newSAMLProviderListEntry,

    -- ** SSHPublicKey
    SSHPublicKey (SSHPublicKey'),
    newSSHPublicKey,

    -- ** SSHPublicKeyMetadata
    SSHPublicKeyMetadata (SSHPublicKeyMetadata'),
    newSSHPublicKeyMetadata,

    -- ** ServerCertificate
    ServerCertificate (ServerCertificate'),
    newServerCertificate,

    -- ** ServerCertificateMetadata
    ServerCertificateMetadata (ServerCertificateMetadata'),
    newServerCertificateMetadata,

    -- ** ServiceLastAccessed
    ServiceLastAccessed (ServiceLastAccessed'),
    newServiceLastAccessed,

    -- ** ServiceSpecificCredential
    ServiceSpecificCredential (ServiceSpecificCredential'),
    newServiceSpecificCredential,

    -- ** ServiceSpecificCredentialMetadata
    ServiceSpecificCredentialMetadata (ServiceSpecificCredentialMetadata'),
    newServiceSpecificCredentialMetadata,

    -- ** SigningCertificate
    SigningCertificate (SigningCertificate'),
    newSigningCertificate,

    -- ** SimulatePolicyResponse
    SimulatePolicyResponse (SimulatePolicyResponse'),
    newSimulatePolicyResponse,

    -- ** Statement
    Statement (Statement'),
    newStatement,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TrackedActionLastAccessed
    TrackedActionLastAccessed (TrackedActionLastAccessed'),
    newTrackedActionLastAccessed,

    -- ** User
    User (User'),
    newUser,

    -- ** UserDetail
    UserDetail (UserDetail'),
    newUserDetail,

    -- ** VirtualMFADevice
    VirtualMFADevice (VirtualMFADevice'),
    newVirtualMFADevice,
  )
where

import Amazonka.IAM.AddClientIDToOpenIDConnectProvider
import Amazonka.IAM.AddRoleToInstanceProfile
import Amazonka.IAM.AddUserToGroup
import Amazonka.IAM.AttachGroupPolicy
import Amazonka.IAM.AttachRolePolicy
import Amazonka.IAM.AttachUserPolicy
import Amazonka.IAM.ChangePassword
import Amazonka.IAM.CreateAccessKey
import Amazonka.IAM.CreateAccountAlias
import Amazonka.IAM.CreateGroup
import Amazonka.IAM.CreateInstanceProfile
import Amazonka.IAM.CreateLoginProfile
import Amazonka.IAM.CreateOpenIDConnectProvider
import Amazonka.IAM.CreatePolicy
import Amazonka.IAM.CreatePolicyVersion
import Amazonka.IAM.CreateRole
import Amazonka.IAM.CreateSAMLProvider
import Amazonka.IAM.CreateServiceLinkedRole
import Amazonka.IAM.CreateServiceSpecificCredential
import Amazonka.IAM.CreateUser
import Amazonka.IAM.CreateVirtualMFADevice
import Amazonka.IAM.DeactivateMFADevice
import Amazonka.IAM.DeleteAccessKey
import Amazonka.IAM.DeleteAccountAlias
import Amazonka.IAM.DeleteAccountPasswordPolicy
import Amazonka.IAM.DeleteGroup
import Amazonka.IAM.DeleteGroupPolicy
import Amazonka.IAM.DeleteInstanceProfile
import Amazonka.IAM.DeleteLoginProfile
import Amazonka.IAM.DeleteOpenIDConnectProvider
import Amazonka.IAM.DeletePolicy
import Amazonka.IAM.DeletePolicyVersion
import Amazonka.IAM.DeleteRole
import Amazonka.IAM.DeleteRolePermissionsBoundary
import Amazonka.IAM.DeleteRolePolicy
import Amazonka.IAM.DeleteSAMLProvider
import Amazonka.IAM.DeleteSSHPublicKey
import Amazonka.IAM.DeleteServerCertificate
import Amazonka.IAM.DeleteServiceLinkedRole
import Amazonka.IAM.DeleteServiceSpecificCredential
import Amazonka.IAM.DeleteSigningCertificate
import Amazonka.IAM.DeleteUser
import Amazonka.IAM.DeleteUserPermissionsBoundary
import Amazonka.IAM.DeleteUserPolicy
import Amazonka.IAM.DeleteVirtualMFADevice
import Amazonka.IAM.DetachGroupPolicy
import Amazonka.IAM.DetachRolePolicy
import Amazonka.IAM.DetachUserPolicy
import Amazonka.IAM.EnableMFADevice
import Amazonka.IAM.GenerateCredentialReport
import Amazonka.IAM.GenerateOrganizationsAccessReport
import Amazonka.IAM.GenerateServiceLastAccessedDetails
import Amazonka.IAM.GetAccessKeyLastUsed
import Amazonka.IAM.GetAccountAuthorizationDetails
import Amazonka.IAM.GetAccountPasswordPolicy
import Amazonka.IAM.GetAccountSummary
import Amazonka.IAM.GetContextKeysForCustomPolicy
import Amazonka.IAM.GetContextKeysForPrincipalPolicy
import Amazonka.IAM.GetCredentialReport
import Amazonka.IAM.GetGroup
import Amazonka.IAM.GetGroupPolicy
import Amazonka.IAM.GetInstanceProfile
import Amazonka.IAM.GetLoginProfile
import Amazonka.IAM.GetOpenIDConnectProvider
import Amazonka.IAM.GetOrganizationsAccessReport
import Amazonka.IAM.GetPolicy
import Amazonka.IAM.GetPolicyVersion
import Amazonka.IAM.GetRole
import Amazonka.IAM.GetRolePolicy
import Amazonka.IAM.GetSAMLProvider
import Amazonka.IAM.GetSSHPublicKey
import Amazonka.IAM.GetServerCertificate
import Amazonka.IAM.GetServiceLastAccessedDetails
import Amazonka.IAM.GetServiceLastAccessedDetailsWithEntities
import Amazonka.IAM.GetServiceLinkedRoleDeletionStatus
import Amazonka.IAM.GetUser
import Amazonka.IAM.GetUserPolicy
import Amazonka.IAM.Lens
import Amazonka.IAM.ListAccessKeys
import Amazonka.IAM.ListAccountAliases
import Amazonka.IAM.ListAttachedGroupPolicies
import Amazonka.IAM.ListAttachedRolePolicies
import Amazonka.IAM.ListAttachedUserPolicies
import Amazonka.IAM.ListEntitiesForPolicy
import Amazonka.IAM.ListGroupPolicies
import Amazonka.IAM.ListGroups
import Amazonka.IAM.ListGroupsForUser
import Amazonka.IAM.ListInstanceProfileTags
import Amazonka.IAM.ListInstanceProfiles
import Amazonka.IAM.ListInstanceProfilesForRole
import Amazonka.IAM.ListMFADeviceTags
import Amazonka.IAM.ListMFADevices
import Amazonka.IAM.ListOpenIDConnectProviderTags
import Amazonka.IAM.ListOpenIDConnectProviders
import Amazonka.IAM.ListPolicies
import Amazonka.IAM.ListPoliciesGrantingServiceAccess
import Amazonka.IAM.ListPolicyTags
import Amazonka.IAM.ListPolicyVersions
import Amazonka.IAM.ListRolePolicies
import Amazonka.IAM.ListRoleTags
import Amazonka.IAM.ListRoles
import Amazonka.IAM.ListSAMLProviderTags
import Amazonka.IAM.ListSAMLProviders
import Amazonka.IAM.ListSSHPublicKeys
import Amazonka.IAM.ListServerCertificateTags
import Amazonka.IAM.ListServerCertificates
import Amazonka.IAM.ListServiceSpecificCredentials
import Amazonka.IAM.ListSigningCertificates
import Amazonka.IAM.ListUserPolicies
import Amazonka.IAM.ListUserTags
import Amazonka.IAM.ListUsers
import Amazonka.IAM.ListVirtualMFADevices
import Amazonka.IAM.PutGroupPolicy
import Amazonka.IAM.PutRolePermissionsBoundary
import Amazonka.IAM.PutRolePolicy
import Amazonka.IAM.PutUserPermissionsBoundary
import Amazonka.IAM.PutUserPolicy
import Amazonka.IAM.RemoveClientIDFromOpenIDConnectProvider
import Amazonka.IAM.RemoveRoleFromInstanceProfile
import Amazonka.IAM.RemoveUserFromGroup
import Amazonka.IAM.ResetServiceSpecificCredential
import Amazonka.IAM.ResyncMFADevice
import Amazonka.IAM.SetDefaultPolicyVersion
import Amazonka.IAM.SetSecurityTokenServicePreferences
import Amazonka.IAM.SimulateCustomPolicy
import Amazonka.IAM.SimulatePrincipalPolicy
import Amazonka.IAM.TagInstanceProfile
import Amazonka.IAM.TagMFADevice
import Amazonka.IAM.TagOpenIDConnectProvider
import Amazonka.IAM.TagPolicy
import Amazonka.IAM.TagRole
import Amazonka.IAM.TagSAMLProvider
import Amazonka.IAM.TagServerCertificate
import Amazonka.IAM.TagUser
import Amazonka.IAM.Types
import Amazonka.IAM.UntagInstanceProfile
import Amazonka.IAM.UntagMFADevice
import Amazonka.IAM.UntagOpenIDConnectProvider
import Amazonka.IAM.UntagPolicy
import Amazonka.IAM.UntagRole
import Amazonka.IAM.UntagSAMLProvider
import Amazonka.IAM.UntagServerCertificate
import Amazonka.IAM.UntagUser
import Amazonka.IAM.UpdateAccessKey
import Amazonka.IAM.UpdateAccountPasswordPolicy
import Amazonka.IAM.UpdateAssumeRolePolicy
import Amazonka.IAM.UpdateGroup
import Amazonka.IAM.UpdateLoginProfile
import Amazonka.IAM.UpdateOpenIDConnectProviderThumbprint
import Amazonka.IAM.UpdateRole
import Amazonka.IAM.UpdateRoleDescription
import Amazonka.IAM.UpdateSAMLProvider
import Amazonka.IAM.UpdateSSHPublicKey
import Amazonka.IAM.UpdateServerCertificate
import Amazonka.IAM.UpdateServiceSpecificCredential
import Amazonka.IAM.UpdateSigningCertificate
import Amazonka.IAM.UpdateUser
import Amazonka.IAM.UploadSSHPublicKey
import Amazonka.IAM.UploadServerCertificate
import Amazonka.IAM.UploadSigningCertificate
import Amazonka.IAM.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'IAM'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
