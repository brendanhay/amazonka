{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.IAM
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
module Network.AWS.IAM
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

import Network.AWS.IAM.AddClientIDToOpenIDConnectProvider
import Network.AWS.IAM.AddRoleToInstanceProfile
import Network.AWS.IAM.AddUserToGroup
import Network.AWS.IAM.AttachGroupPolicy
import Network.AWS.IAM.AttachRolePolicy
import Network.AWS.IAM.AttachUserPolicy
import Network.AWS.IAM.ChangePassword
import Network.AWS.IAM.CreateAccessKey
import Network.AWS.IAM.CreateAccountAlias
import Network.AWS.IAM.CreateGroup
import Network.AWS.IAM.CreateInstanceProfile
import Network.AWS.IAM.CreateLoginProfile
import Network.AWS.IAM.CreateOpenIDConnectProvider
import Network.AWS.IAM.CreatePolicy
import Network.AWS.IAM.CreatePolicyVersion
import Network.AWS.IAM.CreateRole
import Network.AWS.IAM.CreateSAMLProvider
import Network.AWS.IAM.CreateServiceLinkedRole
import Network.AWS.IAM.CreateServiceSpecificCredential
import Network.AWS.IAM.CreateUser
import Network.AWS.IAM.CreateVirtualMFADevice
import Network.AWS.IAM.DeactivateMFADevice
import Network.AWS.IAM.DeleteAccessKey
import Network.AWS.IAM.DeleteAccountAlias
import Network.AWS.IAM.DeleteAccountPasswordPolicy
import Network.AWS.IAM.DeleteGroup
import Network.AWS.IAM.DeleteGroupPolicy
import Network.AWS.IAM.DeleteInstanceProfile
import Network.AWS.IAM.DeleteLoginProfile
import Network.AWS.IAM.DeleteOpenIDConnectProvider
import Network.AWS.IAM.DeletePolicy
import Network.AWS.IAM.DeletePolicyVersion
import Network.AWS.IAM.DeleteRole
import Network.AWS.IAM.DeleteRolePermissionsBoundary
import Network.AWS.IAM.DeleteRolePolicy
import Network.AWS.IAM.DeleteSAMLProvider
import Network.AWS.IAM.DeleteSSHPublicKey
import Network.AWS.IAM.DeleteServerCertificate
import Network.AWS.IAM.DeleteServiceLinkedRole
import Network.AWS.IAM.DeleteServiceSpecificCredential
import Network.AWS.IAM.DeleteSigningCertificate
import Network.AWS.IAM.DeleteUser
import Network.AWS.IAM.DeleteUserPermissionsBoundary
import Network.AWS.IAM.DeleteUserPolicy
import Network.AWS.IAM.DeleteVirtualMFADevice
import Network.AWS.IAM.DetachGroupPolicy
import Network.AWS.IAM.DetachRolePolicy
import Network.AWS.IAM.DetachUserPolicy
import Network.AWS.IAM.EnableMFADevice
import Network.AWS.IAM.GenerateCredentialReport
import Network.AWS.IAM.GenerateOrganizationsAccessReport
import Network.AWS.IAM.GenerateServiceLastAccessedDetails
import Network.AWS.IAM.GetAccessKeyLastUsed
import Network.AWS.IAM.GetAccountAuthorizationDetails
import Network.AWS.IAM.GetAccountPasswordPolicy
import Network.AWS.IAM.GetAccountSummary
import Network.AWS.IAM.GetContextKeysForCustomPolicy
import Network.AWS.IAM.GetContextKeysForPrincipalPolicy
import Network.AWS.IAM.GetCredentialReport
import Network.AWS.IAM.GetGroup
import Network.AWS.IAM.GetGroupPolicy
import Network.AWS.IAM.GetInstanceProfile
import Network.AWS.IAM.GetLoginProfile
import Network.AWS.IAM.GetOpenIDConnectProvider
import Network.AWS.IAM.GetOrganizationsAccessReport
import Network.AWS.IAM.GetPolicy
import Network.AWS.IAM.GetPolicyVersion
import Network.AWS.IAM.GetRole
import Network.AWS.IAM.GetRolePolicy
import Network.AWS.IAM.GetSAMLProvider
import Network.AWS.IAM.GetSSHPublicKey
import Network.AWS.IAM.GetServerCertificate
import Network.AWS.IAM.GetServiceLastAccessedDetails
import Network.AWS.IAM.GetServiceLastAccessedDetailsWithEntities
import Network.AWS.IAM.GetServiceLinkedRoleDeletionStatus
import Network.AWS.IAM.GetUser
import Network.AWS.IAM.GetUserPolicy
import Network.AWS.IAM.Lens
import Network.AWS.IAM.ListAccessKeys
import Network.AWS.IAM.ListAccountAliases
import Network.AWS.IAM.ListAttachedGroupPolicies
import Network.AWS.IAM.ListAttachedRolePolicies
import Network.AWS.IAM.ListAttachedUserPolicies
import Network.AWS.IAM.ListEntitiesForPolicy
import Network.AWS.IAM.ListGroupPolicies
import Network.AWS.IAM.ListGroups
import Network.AWS.IAM.ListGroupsForUser
import Network.AWS.IAM.ListInstanceProfileTags
import Network.AWS.IAM.ListInstanceProfiles
import Network.AWS.IAM.ListInstanceProfilesForRole
import Network.AWS.IAM.ListMFADeviceTags
import Network.AWS.IAM.ListMFADevices
import Network.AWS.IAM.ListOpenIDConnectProviderTags
import Network.AWS.IAM.ListOpenIDConnectProviders
import Network.AWS.IAM.ListPolicies
import Network.AWS.IAM.ListPoliciesGrantingServiceAccess
import Network.AWS.IAM.ListPolicyTags
import Network.AWS.IAM.ListPolicyVersions
import Network.AWS.IAM.ListRolePolicies
import Network.AWS.IAM.ListRoleTags
import Network.AWS.IAM.ListRoles
import Network.AWS.IAM.ListSAMLProviderTags
import Network.AWS.IAM.ListSAMLProviders
import Network.AWS.IAM.ListSSHPublicKeys
import Network.AWS.IAM.ListServerCertificateTags
import Network.AWS.IAM.ListServerCertificates
import Network.AWS.IAM.ListServiceSpecificCredentials
import Network.AWS.IAM.ListSigningCertificates
import Network.AWS.IAM.ListUserPolicies
import Network.AWS.IAM.ListUserTags
import Network.AWS.IAM.ListUsers
import Network.AWS.IAM.ListVirtualMFADevices
import Network.AWS.IAM.PutGroupPolicy
import Network.AWS.IAM.PutRolePermissionsBoundary
import Network.AWS.IAM.PutRolePolicy
import Network.AWS.IAM.PutUserPermissionsBoundary
import Network.AWS.IAM.PutUserPolicy
import Network.AWS.IAM.RemoveClientIDFromOpenIDConnectProvider
import Network.AWS.IAM.RemoveRoleFromInstanceProfile
import Network.AWS.IAM.RemoveUserFromGroup
import Network.AWS.IAM.ResetServiceSpecificCredential
import Network.AWS.IAM.ResyncMFADevice
import Network.AWS.IAM.SetDefaultPolicyVersion
import Network.AWS.IAM.SetSecurityTokenServicePreferences
import Network.AWS.IAM.SimulateCustomPolicy
import Network.AWS.IAM.SimulatePrincipalPolicy
import Network.AWS.IAM.TagInstanceProfile
import Network.AWS.IAM.TagMFADevice
import Network.AWS.IAM.TagOpenIDConnectProvider
import Network.AWS.IAM.TagPolicy
import Network.AWS.IAM.TagRole
import Network.AWS.IAM.TagSAMLProvider
import Network.AWS.IAM.TagServerCertificate
import Network.AWS.IAM.TagUser
import Network.AWS.IAM.Types
import Network.AWS.IAM.UntagInstanceProfile
import Network.AWS.IAM.UntagMFADevice
import Network.AWS.IAM.UntagOpenIDConnectProvider
import Network.AWS.IAM.UntagPolicy
import Network.AWS.IAM.UntagRole
import Network.AWS.IAM.UntagSAMLProvider
import Network.AWS.IAM.UntagServerCertificate
import Network.AWS.IAM.UntagUser
import Network.AWS.IAM.UpdateAccessKey
import Network.AWS.IAM.UpdateAccountPasswordPolicy
import Network.AWS.IAM.UpdateAssumeRolePolicy
import Network.AWS.IAM.UpdateGroup
import Network.AWS.IAM.UpdateLoginProfile
import Network.AWS.IAM.UpdateOpenIDConnectProviderThumbprint
import Network.AWS.IAM.UpdateRole
import Network.AWS.IAM.UpdateRoleDescription
import Network.AWS.IAM.UpdateSAMLProvider
import Network.AWS.IAM.UpdateSSHPublicKey
import Network.AWS.IAM.UpdateServerCertificate
import Network.AWS.IAM.UpdateServiceSpecificCredential
import Network.AWS.IAM.UpdateSigningCertificate
import Network.AWS.IAM.UpdateUser
import Network.AWS.IAM.UploadSSHPublicKey
import Network.AWS.IAM.UploadServerCertificate
import Network.AWS.IAM.UploadSigningCertificate
import Network.AWS.IAM.Waiters

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
