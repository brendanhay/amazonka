{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Identity and Access Management
--
-- AWS Identity and Access Management (IAM) is a web service for securely
-- controlling access to AWS services. With IAM, you can centrally manage
-- users, security credentials such as access keys, and permissions that
-- control which AWS resources users and applications can access. For more
-- information about IAM, see
-- <http://aws.amazon.com/iam/ AWS Identity and Access Management (IAM)>
-- and the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/ AWS Identity and Access Management User Guide>.
module Network.AWS.IAM
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** MalformedPolicyDocumentException
    _MalformedPolicyDocumentException,

    -- ** PolicyEvaluationException
    _PolicyEvaluationException,

    -- ** UnrecognizedPublicKeyEncodingException
    _UnrecognizedPublicKeyEncodingException,

    -- ** ServiceNotSupportedException
    _ServiceNotSupportedException,

    -- ** ReportGenerationLimitExceededException
    _ReportGenerationLimitExceededException,

    -- ** DuplicateSSHPublicKeyException
    _DuplicateSSHPublicKeyException,

    -- ** KeyPairMismatchException
    _KeyPairMismatchException,

    -- ** PolicyNotAttachableException
    _PolicyNotAttachableException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** InvalidPublicKeyException
    _InvalidPublicKeyException,

    -- ** UnmodifiableEntityException
    _UnmodifiableEntityException,

    -- ** DuplicateCertificateException
    _DuplicateCertificateException,

    -- ** MalformedCertificateException
    _MalformedCertificateException,

    -- ** EntityAlreadyExistsException
    _EntityAlreadyExistsException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** ServiceFailureException
    _ServiceFailureException,

    -- ** InvalidUserTypeException
    _InvalidUserTypeException,

    -- ** CredentialReportNotReadyException
    _CredentialReportNotReadyException,

    -- ** CredentialReportNotPresentException
    _CredentialReportNotPresentException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** PasswordPolicyViolationException
    _PasswordPolicyViolationException,

    -- ** InvalidAuthenticationCodeException
    _InvalidAuthenticationCodeException,

    -- ** EntityTemporarilyUnmodifiableException
    _EntityTemporarilyUnmodifiableException,

    -- ** InvalidCertificateException
    _InvalidCertificateException,

    -- ** NoSuchEntityException
    _NoSuchEntityException,

    -- ** DeleteConflictException
    _DeleteConflictException,

    -- ** CredentialReportExpiredException
    _CredentialReportExpiredException,

    -- * Waiters
    -- $waiters

    -- ** UserExists
    newUserExists,

    -- ** RoleExists
    newRoleExists,

    -- ** PolicyExists
    newPolicyExists,

    -- ** InstanceProfileExists
    newInstanceProfileExists,

    -- * Operations
    -- $operations

    -- ** CreateVirtualMFADevice
    CreateVirtualMFADevice (CreateVirtualMFADevice'),
    newCreateVirtualMFADevice,
    CreateVirtualMFADeviceResponse (CreateVirtualMFADeviceResponse'),
    newCreateVirtualMFADeviceResponse,

    -- ** AttachRolePolicy
    AttachRolePolicy (AttachRolePolicy'),
    newAttachRolePolicy,
    AttachRolePolicyResponse (AttachRolePolicyResponse'),
    newAttachRolePolicyResponse,

    -- ** DeleteSSHPublicKey
    DeleteSSHPublicKey (DeleteSSHPublicKey'),
    newDeleteSSHPublicKey,
    DeleteSSHPublicKeyResponse (DeleteSSHPublicKeyResponse'),
    newDeleteSSHPublicKeyResponse,

    -- ** GetUser
    GetUser (GetUser'),
    newGetUser,
    GetUserResponse (GetUserResponse'),
    newGetUserResponse,

    -- ** UpdateSSHPublicKey
    UpdateSSHPublicKey (UpdateSSHPublicKey'),
    newUpdateSSHPublicKey,
    UpdateSSHPublicKeyResponse (UpdateSSHPublicKeyResponse'),
    newUpdateSSHPublicKeyResponse,

    -- ** UntagOpenIDConnectProvider
    UntagOpenIDConnectProvider (UntagOpenIDConnectProvider'),
    newUntagOpenIDConnectProvider,
    UntagOpenIDConnectProviderResponse (UntagOpenIDConnectProviderResponse'),
    newUntagOpenIDConnectProviderResponse,

    -- ** ListSigningCertificates (Paginated)
    ListSigningCertificates (ListSigningCertificates'),
    newListSigningCertificates,
    ListSigningCertificatesResponse (ListSigningCertificatesResponse'),
    newListSigningCertificatesResponse,

    -- ** DeleteOpenIDConnectProvider
    DeleteOpenIDConnectProvider (DeleteOpenIDConnectProvider'),
    newDeleteOpenIDConnectProvider,
    DeleteOpenIDConnectProviderResponse (DeleteOpenIDConnectProviderResponse'),
    newDeleteOpenIDConnectProviderResponse,

    -- ** ListRoleTags
    ListRoleTags (ListRoleTags'),
    newListRoleTags,
    ListRoleTagsResponse (ListRoleTagsResponse'),
    newListRoleTagsResponse,

    -- ** ListOpenIDConnectProviders
    ListOpenIDConnectProviders (ListOpenIDConnectProviders'),
    newListOpenIDConnectProviders,
    ListOpenIDConnectProvidersResponse (ListOpenIDConnectProvidersResponse'),
    newListOpenIDConnectProvidersResponse,

    -- ** CreatePolicy
    CreatePolicy (CreatePolicy'),
    newCreatePolicy,
    CreatePolicyResponse (CreatePolicyResponse'),
    newCreatePolicyResponse,

    -- ** GetSAMLProvider
    GetSAMLProvider (GetSAMLProvider'),
    newGetSAMLProvider,
    GetSAMLProviderResponse (GetSAMLProviderResponse'),
    newGetSAMLProviderResponse,

    -- ** GetContextKeysForPrincipalPolicy
    GetContextKeysForPrincipalPolicy (GetContextKeysForPrincipalPolicy'),
    newGetContextKeysForPrincipalPolicy,
    GetContextKeysForPolicyResponse (GetContextKeysForPolicyResponse'),
    newGetContextKeysForPolicyResponse,

    -- ** ListEntitiesForPolicy (Paginated)
    ListEntitiesForPolicy (ListEntitiesForPolicy'),
    newListEntitiesForPolicy,
    ListEntitiesForPolicyResponse (ListEntitiesForPolicyResponse'),
    newListEntitiesForPolicyResponse,

    -- ** ListGroupsForUser (Paginated)
    ListGroupsForUser (ListGroupsForUser'),
    newListGroupsForUser,
    ListGroupsForUserResponse (ListGroupsForUserResponse'),
    newListGroupsForUserResponse,

    -- ** SimulatePrincipalPolicy (Paginated)
    SimulatePrincipalPolicy (SimulatePrincipalPolicy'),
    newSimulatePrincipalPolicy,
    SimulatePolicyResponse (SimulatePolicyResponse'),
    newSimulatePolicyResponse,

    -- ** ListPolicies (Paginated)
    ListPolicies (ListPolicies'),
    newListPolicies,
    ListPoliciesResponse (ListPoliciesResponse'),
    newListPoliciesResponse,

    -- ** CreateServiceLinkedRole
    CreateServiceLinkedRole (CreateServiceLinkedRole'),
    newCreateServiceLinkedRole,
    CreateServiceLinkedRoleResponse (CreateServiceLinkedRoleResponse'),
    newCreateServiceLinkedRoleResponse,

    -- ** UntagPolicy
    UntagPolicy (UntagPolicy'),
    newUntagPolicy,
    UntagPolicyResponse (UntagPolicyResponse'),
    newUntagPolicyResponse,

    -- ** DeletePolicy
    DeletePolicy (DeletePolicy'),
    newDeletePolicy,
    DeletePolicyResponse (DeletePolicyResponse'),
    newDeletePolicyResponse,

    -- ** DeleteServerCertificate
    DeleteServerCertificate (DeleteServerCertificate'),
    newDeleteServerCertificate,
    DeleteServerCertificateResponse (DeleteServerCertificateResponse'),
    newDeleteServerCertificateResponse,

    -- ** ListAttachedGroupPolicies (Paginated)
    ListAttachedGroupPolicies (ListAttachedGroupPolicies'),
    newListAttachedGroupPolicies,
    ListAttachedGroupPoliciesResponse (ListAttachedGroupPoliciesResponse'),
    newListAttachedGroupPoliciesResponse,

    -- ** ChangePassword
    ChangePassword (ChangePassword'),
    newChangePassword,
    ChangePasswordResponse (ChangePasswordResponse'),
    newChangePasswordResponse,

    -- ** ListMFADeviceTags
    ListMFADeviceTags (ListMFADeviceTags'),
    newListMFADeviceTags,
    ListMFADeviceTagsResponse (ListMFADeviceTagsResponse'),
    newListMFADeviceTagsResponse,

    -- ** UntagServerCertificate
    UntagServerCertificate (UntagServerCertificate'),
    newUntagServerCertificate,
    UntagServerCertificateResponse (UntagServerCertificateResponse'),
    newUntagServerCertificateResponse,

    -- ** UpdateAssumeRolePolicy
    UpdateAssumeRolePolicy (UpdateAssumeRolePolicy'),
    newUpdateAssumeRolePolicy,
    UpdateAssumeRolePolicyResponse (UpdateAssumeRolePolicyResponse'),
    newUpdateAssumeRolePolicyResponse,

    -- ** GetGroupPolicy
    GetGroupPolicy (GetGroupPolicy'),
    newGetGroupPolicy,
    GetGroupPolicyResponse (GetGroupPolicyResponse'),
    newGetGroupPolicyResponse,

    -- ** UpdateServerCertificate
    UpdateServerCertificate (UpdateServerCertificate'),
    newUpdateServerCertificate,
    UpdateServerCertificateResponse (UpdateServerCertificateResponse'),
    newUpdateServerCertificateResponse,

    -- ** ListServerCertificates (Paginated)
    ListServerCertificates (ListServerCertificates'),
    newListServerCertificates,
    ListServerCertificatesResponse (ListServerCertificatesResponse'),
    newListServerCertificatesResponse,

    -- ** ListInstanceProfileTags
    ListInstanceProfileTags (ListInstanceProfileTags'),
    newListInstanceProfileTags,
    ListInstanceProfileTagsResponse (ListInstanceProfileTagsResponse'),
    newListInstanceProfileTagsResponse,

    -- ** DeleteGroupPolicy
    DeleteGroupPolicy (DeleteGroupPolicy'),
    newDeleteGroupPolicy,
    DeleteGroupPolicyResponse (DeleteGroupPolicyResponse'),
    newDeleteGroupPolicyResponse,

    -- ** CreateInstanceProfile
    CreateInstanceProfile (CreateInstanceProfile'),
    newCreateInstanceProfile,
    CreateInstanceProfileResponse (CreateInstanceProfileResponse'),
    newCreateInstanceProfileResponse,

    -- ** ListGroups (Paginated)
    ListGroups (ListGroups'),
    newListGroups,
    ListGroupsResponse (ListGroupsResponse'),
    newListGroupsResponse,

    -- ** GetLoginProfile
    GetLoginProfile (GetLoginProfile'),
    newGetLoginProfile,
    GetLoginProfileResponse (GetLoginProfileResponse'),
    newGetLoginProfileResponse,

    -- ** TagRole
    TagRole (TagRole'),
    newTagRole,
    TagRoleResponse (TagRoleResponse'),
    newTagRoleResponse,

    -- ** RemoveRoleFromInstanceProfile
    RemoveRoleFromInstanceProfile (RemoveRoleFromInstanceProfile'),
    newRemoveRoleFromInstanceProfile,
    RemoveRoleFromInstanceProfileResponse (RemoveRoleFromInstanceProfileResponse'),
    newRemoveRoleFromInstanceProfileResponse,

    -- ** GenerateCredentialReport
    GenerateCredentialReport (GenerateCredentialReport'),
    newGenerateCredentialReport,
    GenerateCredentialReportResponse (GenerateCredentialReportResponse'),
    newGenerateCredentialReportResponse,

    -- ** CreatePolicyVersion
    CreatePolicyVersion (CreatePolicyVersion'),
    newCreatePolicyVersion,
    CreatePolicyVersionResponse (CreatePolicyVersionResponse'),
    newCreatePolicyVersionResponse,

    -- ** GetServerCertificate
    GetServerCertificate (GetServerCertificate'),
    newGetServerCertificate,
    GetServerCertificateResponse (GetServerCertificateResponse'),
    newGetServerCertificateResponse,

    -- ** RemoveUserFromGroup
    RemoveUserFromGroup (RemoveUserFromGroup'),
    newRemoveUserFromGroup,
    RemoveUserFromGroupResponse (RemoveUserFromGroupResponse'),
    newRemoveUserFromGroupResponse,

    -- ** SetDefaultPolicyVersion
    SetDefaultPolicyVersion (SetDefaultPolicyVersion'),
    newSetDefaultPolicyVersion,
    SetDefaultPolicyVersionResponse (SetDefaultPolicyVersionResponse'),
    newSetDefaultPolicyVersionResponse,

    -- ** ResetServiceSpecificCredential
    ResetServiceSpecificCredential (ResetServiceSpecificCredential'),
    newResetServiceSpecificCredential,
    ResetServiceSpecificCredentialResponse (ResetServiceSpecificCredentialResponse'),
    newResetServiceSpecificCredentialResponse,

    -- ** GenerateServiceLastAccessedDetails
    GenerateServiceLastAccessedDetails (GenerateServiceLastAccessedDetails'),
    newGenerateServiceLastAccessedDetails,
    GenerateServiceLastAccessedDetailsResponse (GenerateServiceLastAccessedDetailsResponse'),
    newGenerateServiceLastAccessedDetailsResponse,

    -- ** ListPoliciesGrantingServiceAccess
    ListPoliciesGrantingServiceAccess (ListPoliciesGrantingServiceAccess'),
    newListPoliciesGrantingServiceAccess,
    ListPoliciesGrantingServiceAccessResponse (ListPoliciesGrantingServiceAccessResponse'),
    newListPoliciesGrantingServiceAccessResponse,

    -- ** UpdateRoleDescription
    UpdateRoleDescription (UpdateRoleDescription'),
    newUpdateRoleDescription,
    UpdateRoleDescriptionResponse (UpdateRoleDescriptionResponse'),
    newUpdateRoleDescriptionResponse,

    -- ** UploadServerCertificate
    UploadServerCertificate (UploadServerCertificate'),
    newUploadServerCertificate,
    UploadServerCertificateResponse (UploadServerCertificateResponse'),
    newUploadServerCertificateResponse,

    -- ** DetachRolePolicy
    DetachRolePolicy (DetachRolePolicy'),
    newDetachRolePolicy,
    DetachRolePolicyResponse (DetachRolePolicyResponse'),
    newDetachRolePolicyResponse,

    -- ** EnableMFADevice
    EnableMFADevice (EnableMFADevice'),
    newEnableMFADevice,
    EnableMFADeviceResponse (EnableMFADeviceResponse'),
    newEnableMFADeviceResponse,

    -- ** ListSAMLProviders
    ListSAMLProviders (ListSAMLProviders'),
    newListSAMLProviders,
    ListSAMLProvidersResponse (ListSAMLProvidersResponse'),
    newListSAMLProvidersResponse,

    -- ** ListPolicyTags
    ListPolicyTags (ListPolicyTags'),
    newListPolicyTags,
    ListPolicyTagsResponse (ListPolicyTagsResponse'),
    newListPolicyTagsResponse,

    -- ** CreateGroup
    CreateGroup (CreateGroup'),
    newCreateGroup,
    CreateGroupResponse (CreateGroupResponse'),
    newCreateGroupResponse,

    -- ** TagMFADevice
    TagMFADevice (TagMFADevice'),
    newTagMFADevice,
    TagMFADeviceResponse (TagMFADeviceResponse'),
    newTagMFADeviceResponse,

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

    -- ** CreateRole
    CreateRole (CreateRole'),
    newCreateRole,
    CreateRoleResponse (CreateRoleResponse'),
    newCreateRoleResponse,

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

    -- ** DeleteRolePermissionsBoundary
    DeleteRolePermissionsBoundary (DeleteRolePermissionsBoundary'),
    newDeleteRolePermissionsBoundary,
    DeleteRolePermissionsBoundaryResponse (DeleteRolePermissionsBoundaryResponse'),
    newDeleteRolePermissionsBoundaryResponse,

    -- ** CreateUser
    CreateUser (CreateUser'),
    newCreateUser,
    CreateUserResponse (CreateUserResponse'),
    newCreateUserResponse,

    -- ** ListOpenIDConnectProviderTags
    ListOpenIDConnectProviderTags (ListOpenIDConnectProviderTags'),
    newListOpenIDConnectProviderTags,
    ListOpenIDConnectProviderTagsResponse (ListOpenIDConnectProviderTagsResponse'),
    newListOpenIDConnectProviderTagsResponse,

    -- ** ListRoles (Paginated)
    ListRoles (ListRoles'),
    newListRoles,
    ListRolesResponse (ListRolesResponse'),
    newListRolesResponse,

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

    -- ** ListAttachedRolePolicies (Paginated)
    ListAttachedRolePolicies (ListAttachedRolePolicies'),
    newListAttachedRolePolicies,
    ListAttachedRolePoliciesResponse (ListAttachedRolePoliciesResponse'),
    newListAttachedRolePoliciesResponse,

    -- ** GetRolePolicy
    GetRolePolicy (GetRolePolicy'),
    newGetRolePolicy,
    GetRolePolicyResponse (GetRolePolicyResponse'),
    newGetRolePolicyResponse,

    -- ** DeleteAccessKey
    DeleteAccessKey (DeleteAccessKey'),
    newDeleteAccessKey,
    DeleteAccessKeyResponse (DeleteAccessKeyResponse'),
    newDeleteAccessKeyResponse,

    -- ** ListVirtualMFADevices (Paginated)
    ListVirtualMFADevices (ListVirtualMFADevices'),
    newListVirtualMFADevices,
    ListVirtualMFADevicesResponse (ListVirtualMFADevicesResponse'),
    newListVirtualMFADevicesResponse,

    -- ** TagPolicy
    TagPolicy (TagPolicy'),
    newTagPolicy,
    TagPolicyResponse (TagPolicyResponse'),
    newTagPolicyResponse,

    -- ** RemoveClientIDFromOpenIDConnectProvider
    RemoveClientIDFromOpenIDConnectProvider (RemoveClientIDFromOpenIDConnectProvider'),
    newRemoveClientIDFromOpenIDConnectProvider,
    RemoveClientIDFromOpenIDConnectProviderResponse (RemoveClientIDFromOpenIDConnectProviderResponse'),
    newRemoveClientIDFromOpenIDConnectProviderResponse,

    -- ** DeleteVirtualMFADevice
    DeleteVirtualMFADevice (DeleteVirtualMFADevice'),
    newDeleteVirtualMFADevice,
    DeleteVirtualMFADeviceResponse (DeleteVirtualMFADeviceResponse'),
    newDeleteVirtualMFADeviceResponse,

    -- ** UpdateAccessKey
    UpdateAccessKey (UpdateAccessKey'),
    newUpdateAccessKey,
    UpdateAccessKeyResponse (UpdateAccessKeyResponse'),
    newUpdateAccessKeyResponse,

    -- ** CreateServiceSpecificCredential
    CreateServiceSpecificCredential (CreateServiceSpecificCredential'),
    newCreateServiceSpecificCredential,
    CreateServiceSpecificCredentialResponse (CreateServiceSpecificCredentialResponse'),
    newCreateServiceSpecificCredentialResponse,

    -- ** ResyncMFADevice
    ResyncMFADevice (ResyncMFADevice'),
    newResyncMFADevice,
    ResyncMFADeviceResponse (ResyncMFADeviceResponse'),
    newResyncMFADeviceResponse,

    -- ** UpdateServiceSpecificCredential
    UpdateServiceSpecificCredential (UpdateServiceSpecificCredential'),
    newUpdateServiceSpecificCredential,
    UpdateServiceSpecificCredentialResponse (UpdateServiceSpecificCredentialResponse'),
    newUpdateServiceSpecificCredentialResponse,

    -- ** GetUserPolicy
    GetUserPolicy (GetUserPolicy'),
    newGetUserPolicy,
    GetUserPolicyResponse (GetUserPolicyResponse'),
    newGetUserPolicyResponse,

    -- ** UpdateAccountPasswordPolicy
    UpdateAccountPasswordPolicy (UpdateAccountPasswordPolicy'),
    newUpdateAccountPasswordPolicy,
    UpdateAccountPasswordPolicyResponse (UpdateAccountPasswordPolicyResponse'),
    newUpdateAccountPasswordPolicyResponse,

    -- ** ListServiceSpecificCredentials
    ListServiceSpecificCredentials (ListServiceSpecificCredentials'),
    newListServiceSpecificCredentials,
    ListServiceSpecificCredentialsResponse (ListServiceSpecificCredentialsResponse'),
    newListServiceSpecificCredentialsResponse,

    -- ** DeleteSigningCertificate
    DeleteSigningCertificate (DeleteSigningCertificate'),
    newDeleteSigningCertificate,
    DeleteSigningCertificateResponse (DeleteSigningCertificateResponse'),
    newDeleteSigningCertificateResponse,

    -- ** ListAttachedUserPolicies (Paginated)
    ListAttachedUserPolicies (ListAttachedUserPolicies'),
    newListAttachedUserPolicies,
    ListAttachedUserPoliciesResponse (ListAttachedUserPoliciesResponse'),
    newListAttachedUserPoliciesResponse,

    -- ** UpdateSigningCertificate
    UpdateSigningCertificate (UpdateSigningCertificate'),
    newUpdateSigningCertificate,
    UpdateSigningCertificateResponse (UpdateSigningCertificateResponse'),
    newUpdateSigningCertificateResponse,

    -- ** ListSSHPublicKeys (Paginated)
    ListSSHPublicKeys (ListSSHPublicKeys'),
    newListSSHPublicKeys,
    ListSSHPublicKeysResponse (ListSSHPublicKeysResponse'),
    newListSSHPublicKeysResponse,

    -- ** DeleteServiceSpecificCredential
    DeleteServiceSpecificCredential (DeleteServiceSpecificCredential'),
    newDeleteServiceSpecificCredential,
    DeleteServiceSpecificCredentialResponse (DeleteServiceSpecificCredentialResponse'),
    newDeleteServiceSpecificCredentialResponse,

    -- ** CreateAccessKey
    CreateAccessKey (CreateAccessKey'),
    newCreateAccessKey,
    CreateAccessKeyResponse (CreateAccessKeyResponse'),
    newCreateAccessKeyResponse,

    -- ** DeleteAccountPasswordPolicy
    DeleteAccountPasswordPolicy (DeleteAccountPasswordPolicy'),
    newDeleteAccountPasswordPolicy,
    DeleteAccountPasswordPolicyResponse (DeleteAccountPasswordPolicyResponse'),
    newDeleteAccountPasswordPolicyResponse,

    -- ** GetOrganizationsAccessReport
    GetOrganizationsAccessReport (GetOrganizationsAccessReport'),
    newGetOrganizationsAccessReport,
    GetOrganizationsAccessReportResponse (GetOrganizationsAccessReportResponse'),
    newGetOrganizationsAccessReportResponse,

    -- ** ListInstanceProfilesForRole (Paginated)
    ListInstanceProfilesForRole (ListInstanceProfilesForRole'),
    newListInstanceProfilesForRole,
    ListInstanceProfilesForRoleResponse (ListInstanceProfilesForRoleResponse'),
    newListInstanceProfilesForRoleResponse,

    -- ** GetPolicyVersion
    GetPolicyVersion (GetPolicyVersion'),
    newGetPolicyVersion,
    GetPolicyVersionResponse (GetPolicyVersionResponse'),
    newGetPolicyVersionResponse,

    -- ** CreateLoginProfile
    CreateLoginProfile (CreateLoginProfile'),
    newCreateLoginProfile,
    CreateLoginProfileResponse (CreateLoginProfileResponse'),
    newCreateLoginProfileResponse,

    -- ** AddRoleToInstanceProfile
    AddRoleToInstanceProfile (AddRoleToInstanceProfile'),
    newAddRoleToInstanceProfile,
    AddRoleToInstanceProfileResponse (AddRoleToInstanceProfileResponse'),
    newAddRoleToInstanceProfileResponse,

    -- ** GetInstanceProfile
    GetInstanceProfile (GetInstanceProfile'),
    newGetInstanceProfile,
    GetInstanceProfileResponse (GetInstanceProfileResponse'),
    newGetInstanceProfileResponse,

    -- ** TagOpenIDConnectProvider
    TagOpenIDConnectProvider (TagOpenIDConnectProvider'),
    newTagOpenIDConnectProvider,
    TagOpenIDConnectProviderResponse (TagOpenIDConnectProviderResponse'),
    newTagOpenIDConnectProviderResponse,

    -- ** AddUserToGroup
    AddUserToGroup (AddUserToGroup'),
    newAddUserToGroup,
    AddUserToGroupResponse (AddUserToGroupResponse'),
    newAddUserToGroupResponse,

    -- ** AttachGroupPolicy
    AttachGroupPolicy (AttachGroupPolicy'),
    newAttachGroupPolicy,
    AttachGroupPolicyResponse (AttachGroupPolicyResponse'),
    newAttachGroupPolicyResponse,

    -- ** UpdateLoginProfile
    UpdateLoginProfile (UpdateLoginProfile'),
    newUpdateLoginProfile,
    UpdateLoginProfileResponse (UpdateLoginProfileResponse'),
    newUpdateLoginProfileResponse,

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

    -- ** DeleteLoginProfile
    DeleteLoginProfile (DeleteLoginProfile'),
    newDeleteLoginProfile,
    DeleteLoginProfileResponse (DeleteLoginProfileResponse'),
    newDeleteLoginProfileResponse,

    -- ** DeleteServiceLinkedRole
    DeleteServiceLinkedRole (DeleteServiceLinkedRole'),
    newDeleteServiceLinkedRole,
    DeleteServiceLinkedRoleResponse (DeleteServiceLinkedRoleResponse'),
    newDeleteServiceLinkedRoleResponse,

    -- ** GenerateOrganizationsAccessReport
    GenerateOrganizationsAccessReport (GenerateOrganizationsAccessReport'),
    newGenerateOrganizationsAccessReport,
    GenerateOrganizationsAccessReportResponse (GenerateOrganizationsAccessReportResponse'),
    newGenerateOrganizationsAccessReportResponse,

    -- ** GetServiceLastAccessedDetailsWithEntities
    GetServiceLastAccessedDetailsWithEntities (GetServiceLastAccessedDetailsWithEntities'),
    newGetServiceLastAccessedDetailsWithEntities,
    GetServiceLastAccessedDetailsWithEntitiesResponse (GetServiceLastAccessedDetailsWithEntitiesResponse'),
    newGetServiceLastAccessedDetailsWithEntitiesResponse,

    -- ** PutGroupPolicy
    PutGroupPolicy (PutGroupPolicy'),
    newPutGroupPolicy,
    PutGroupPolicyResponse (PutGroupPolicyResponse'),
    newPutGroupPolicyResponse,

    -- ** GetServiceLastAccessedDetails
    GetServiceLastAccessedDetails (GetServiceLastAccessedDetails'),
    newGetServiceLastAccessedDetails,
    GetServiceLastAccessedDetailsResponse (GetServiceLastAccessedDetailsResponse'),
    newGetServiceLastAccessedDetailsResponse,

    -- ** DeleteAccountAlias
    DeleteAccountAlias (DeleteAccountAlias'),
    newDeleteAccountAlias,
    DeleteAccountAliasResponse (DeleteAccountAliasResponse'),
    newDeleteAccountAliasResponse,

    -- ** CreateSAMLProvider
    CreateSAMLProvider (CreateSAMLProvider'),
    newCreateSAMLProvider,
    CreateSAMLProviderResponse (CreateSAMLProviderResponse'),
    newCreateSAMLProviderResponse,

    -- ** GetPolicy
    GetPolicy (GetPolicy'),
    newGetPolicy,
    GetPolicyResponse (GetPolicyResponse'),
    newGetPolicyResponse,

    -- ** DetachUserPolicy
    DetachUserPolicy (DetachUserPolicy'),
    newDetachUserPolicy,
    DetachUserPolicyResponse (DetachUserPolicyResponse'),
    newDetachUserPolicyResponse,

    -- ** UpdateGroup
    UpdateGroup (UpdateGroup'),
    newUpdateGroup,
    UpdateGroupResponse (UpdateGroupResponse'),
    newUpdateGroupResponse,

    -- ** DeleteGroup
    DeleteGroup (DeleteGroup'),
    newDeleteGroup,
    DeleteGroupResponse (DeleteGroupResponse'),
    newDeleteGroupResponse,

    -- ** GetServiceLinkedRoleDeletionStatus
    GetServiceLinkedRoleDeletionStatus (GetServiceLinkedRoleDeletionStatus'),
    newGetServiceLinkedRoleDeletionStatus,
    GetServiceLinkedRoleDeletionStatusResponse (GetServiceLinkedRoleDeletionStatusResponse'),
    newGetServiceLinkedRoleDeletionStatusResponse,

    -- ** GetAccountAuthorizationDetails (Paginated)
    GetAccountAuthorizationDetails (GetAccountAuthorizationDetails'),
    newGetAccountAuthorizationDetails,
    GetAccountAuthorizationDetailsResponse (GetAccountAuthorizationDetailsResponse'),
    newGetAccountAuthorizationDetailsResponse,

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

    -- ** DeleteSAMLProvider
    DeleteSAMLProvider (DeleteSAMLProvider'),
    newDeleteSAMLProvider,
    DeleteSAMLProviderResponse (DeleteSAMLProviderResponse'),
    newDeleteSAMLProviderResponse,

    -- ** TagUser
    TagUser (TagUser'),
    newTagUser,
    TagUserResponse (TagUserResponse'),
    newTagUserResponse,

    -- ** ListInstanceProfiles (Paginated)
    ListInstanceProfiles (ListInstanceProfiles'),
    newListInstanceProfiles,
    ListInstanceProfilesResponse (ListInstanceProfilesResponse'),
    newListInstanceProfilesResponse,

    -- ** GetCredentialReport
    GetCredentialReport (GetCredentialReport'),
    newGetCredentialReport,
    GetCredentialReportResponse (GetCredentialReportResponse'),
    newGetCredentialReportResponse,

    -- ** ListMFADevices (Paginated)
    ListMFADevices (ListMFADevices'),
    newListMFADevices,
    ListMFADevicesResponse (ListMFADevicesResponse'),
    newListMFADevicesResponse,

    -- ** UpdateSAMLProvider
    UpdateSAMLProvider (UpdateSAMLProvider'),
    newUpdateSAMLProvider,
    UpdateSAMLProviderResponse (UpdateSAMLProviderResponse'),
    newUpdateSAMLProviderResponse,

    -- ** UntagInstanceProfile
    UntagInstanceProfile (UntagInstanceProfile'),
    newUntagInstanceProfile,
    UntagInstanceProfileResponse (UntagInstanceProfileResponse'),
    newUntagInstanceProfileResponse,

    -- ** CreateAccountAlias
    CreateAccountAlias (CreateAccountAlias'),
    newCreateAccountAlias,
    CreateAccountAliasResponse (CreateAccountAliasResponse'),
    newCreateAccountAliasResponse,

    -- ** UntagMFADevice
    UntagMFADevice (UntagMFADevice'),
    newUntagMFADevice,
    UntagMFADeviceResponse (UntagMFADeviceResponse'),
    newUntagMFADeviceResponse,

    -- ** UntagSAMLProvider
    UntagSAMLProvider (UntagSAMLProvider'),
    newUntagSAMLProvider,
    UntagSAMLProviderResponse (UntagSAMLProviderResponse'),
    newUntagSAMLProviderResponse,

    -- ** ListAccountAliases (Paginated)
    ListAccountAliases (ListAccountAliases'),
    newListAccountAliases,
    ListAccountAliasesResponse (ListAccountAliasesResponse'),
    newListAccountAliasesResponse,

    -- ** ListPolicyVersions (Paginated)
    ListPolicyVersions (ListPolicyVersions'),
    newListPolicyVersions,
    ListPolicyVersionsResponse (ListPolicyVersionsResponse'),
    newListPolicyVersionsResponse,

    -- ** DeleteInstanceProfile
    DeleteInstanceProfile (DeleteInstanceProfile'),
    newDeleteInstanceProfile,
    DeleteInstanceProfileResponse (DeleteInstanceProfileResponse'),
    newDeleteInstanceProfileResponse,

    -- ** GetAccountSummary
    GetAccountSummary (GetAccountSummary'),
    newGetAccountSummary,
    GetAccountSummaryResponse (GetAccountSummaryResponse'),
    newGetAccountSummaryResponse,

    -- ** ListServerCertificateTags
    ListServerCertificateTags (ListServerCertificateTags'),
    newListServerCertificateTags,
    ListServerCertificateTagsResponse (ListServerCertificateTagsResponse'),
    newListServerCertificateTagsResponse,

    -- ** GetSSHPublicKey
    GetSSHPublicKey (GetSSHPublicKey'),
    newGetSSHPublicKey,
    GetSSHPublicKeyResponse (GetSSHPublicKeyResponse'),
    newGetSSHPublicKeyResponse,

    -- ** UpdateOpenIDConnectProviderThumbprint
    UpdateOpenIDConnectProviderThumbprint (UpdateOpenIDConnectProviderThumbprint'),
    newUpdateOpenIDConnectProviderThumbprint,
    UpdateOpenIDConnectProviderThumbprintResponse (UpdateOpenIDConnectProviderThumbprintResponse'),
    newUpdateOpenIDConnectProviderThumbprintResponse,

    -- ** GetAccessKeyLastUsed
    GetAccessKeyLastUsed (GetAccessKeyLastUsed'),
    newGetAccessKeyLastUsed,
    GetAccessKeyLastUsedResponse (GetAccessKeyLastUsedResponse'),
    newGetAccessKeyLastUsedResponse,

    -- ** TagSAMLProvider
    TagSAMLProvider (TagSAMLProvider'),
    newTagSAMLProvider,
    TagSAMLProviderResponse (TagSAMLProviderResponse'),
    newTagSAMLProviderResponse,

    -- ** GetAccountPasswordPolicy
    GetAccountPasswordPolicy (GetAccountPasswordPolicy'),
    newGetAccountPasswordPolicy,
    GetAccountPasswordPolicyResponse (GetAccountPasswordPolicyResponse'),
    newGetAccountPasswordPolicyResponse,

    -- ** DeleteUser
    DeleteUser (DeleteUser'),
    newDeleteUser,
    DeleteUserResponse (DeleteUserResponse'),
    newDeleteUserResponse,

    -- ** ListUsers (Paginated)
    ListUsers (ListUsers'),
    newListUsers,
    ListUsersResponse (ListUsersResponse'),
    newListUsersResponse,

    -- ** UpdateUser
    UpdateUser (UpdateUser'),
    newUpdateUser,
    UpdateUserResponse (UpdateUserResponse'),
    newUpdateUserResponse,

    -- ** ListRolePolicies (Paginated)
    ListRolePolicies (ListRolePolicies'),
    newListRolePolicies,
    ListRolePoliciesResponse (ListRolePoliciesResponse'),
    newListRolePoliciesResponse,

    -- ** AddClientIDToOpenIDConnectProvider
    AddClientIDToOpenIDConnectProvider (AddClientIDToOpenIDConnectProvider'),
    newAddClientIDToOpenIDConnectProvider,
    AddClientIDToOpenIDConnectProviderResponse (AddClientIDToOpenIDConnectProviderResponse'),
    newAddClientIDToOpenIDConnectProviderResponse,

    -- ** DeleteUserPermissionsBoundary
    DeleteUserPermissionsBoundary (DeleteUserPermissionsBoundary'),
    newDeleteUserPermissionsBoundary,
    DeleteUserPermissionsBoundaryResponse (DeleteUserPermissionsBoundaryResponse'),
    newDeleteUserPermissionsBoundaryResponse,

    -- ** PutUserPolicy
    PutUserPolicy (PutUserPolicy'),
    newPutUserPolicy,
    PutUserPolicyResponse (PutUserPolicyResponse'),
    newPutUserPolicyResponse,

    -- ** DetachGroupPolicy
    DetachGroupPolicy (DetachGroupPolicy'),
    newDetachGroupPolicy,
    DetachGroupPolicyResponse (DetachGroupPolicyResponse'),
    newDetachGroupPolicyResponse,

    -- ** UntagUser
    UntagUser (UntagUser'),
    newUntagUser,
    UntagUserResponse (UntagUserResponse'),
    newUntagUserResponse,

    -- ** GetContextKeysForCustomPolicy
    GetContextKeysForCustomPolicy (GetContextKeysForCustomPolicy'),
    newGetContextKeysForCustomPolicy,
    GetContextKeysForPolicyResponse (GetContextKeysForPolicyResponse'),
    newGetContextKeysForPolicyResponse,

    -- ** PutRolePermissionsBoundary
    PutRolePermissionsBoundary (PutRolePermissionsBoundary'),
    newPutRolePermissionsBoundary,
    PutRolePermissionsBoundaryResponse (PutRolePermissionsBoundaryResponse'),
    newPutRolePermissionsBoundaryResponse,

    -- ** UntagRole
    UntagRole (UntagRole'),
    newUntagRole,
    UntagRoleResponse (UntagRoleResponse'),
    newUntagRoleResponse,

    -- ** SimulateCustomPolicy (Paginated)
    SimulateCustomPolicy (SimulateCustomPolicy'),
    newSimulateCustomPolicy,
    SimulatePolicyResponse (SimulatePolicyResponse'),
    newSimulatePolicyResponse,

    -- ** UploadSSHPublicKey
    UploadSSHPublicKey (UploadSSHPublicKey'),
    newUploadSSHPublicKey,
    UploadSSHPublicKeyResponse (UploadSSHPublicKeyResponse'),
    newUploadSSHPublicKeyResponse,

    -- ** DeleteRole
    DeleteRole (DeleteRole'),
    newDeleteRole,
    DeleteRoleResponse (DeleteRoleResponse'),
    newDeleteRoleResponse,

    -- ** ListUserPolicies (Paginated)
    ListUserPolicies (ListUserPolicies'),
    newListUserPolicies,
    ListUserPoliciesResponse (ListUserPoliciesResponse'),
    newListUserPoliciesResponse,

    -- ** PutRolePolicy
    PutRolePolicy (PutRolePolicy'),
    newPutRolePolicy,
    PutRolePolicyResponse (PutRolePolicyResponse'),
    newPutRolePolicyResponse,

    -- ** UpdateRole
    UpdateRole (UpdateRole'),
    newUpdateRole,
    UpdateRoleResponse (UpdateRoleResponse'),
    newUpdateRoleResponse,

    -- ** SetSecurityTokenServicePreferences
    SetSecurityTokenServicePreferences (SetSecurityTokenServicePreferences'),
    newSetSecurityTokenServicePreferences,
    SetSecurityTokenServicePreferencesResponse (SetSecurityTokenServicePreferencesResponse'),
    newSetSecurityTokenServicePreferencesResponse,

    -- ** AttachUserPolicy
    AttachUserPolicy (AttachUserPolicy'),
    newAttachUserPolicy,
    AttachUserPolicyResponse (AttachUserPolicyResponse'),
    newAttachUserPolicyResponse,

    -- ** TagServerCertificate
    TagServerCertificate (TagServerCertificate'),
    newTagServerCertificate,
    TagServerCertificateResponse (TagServerCertificateResponse'),
    newTagServerCertificateResponse,

    -- ** ListAccessKeys (Paginated)
    ListAccessKeys (ListAccessKeys'),
    newListAccessKeys,
    ListAccessKeysResponse (ListAccessKeysResponse'),
    newListAccessKeysResponse,

    -- ** CreateOpenIDConnectProvider
    CreateOpenIDConnectProvider (CreateOpenIDConnectProvider'),
    newCreateOpenIDConnectProvider,
    CreateOpenIDConnectProviderResponse (CreateOpenIDConnectProviderResponse'),
    newCreateOpenIDConnectProviderResponse,

    -- ** DeactivateMFADevice
    DeactivateMFADevice (DeactivateMFADevice'),
    newDeactivateMFADevice,
    DeactivateMFADeviceResponse (DeactivateMFADeviceResponse'),
    newDeactivateMFADeviceResponse,

    -- ** ListUserTags
    ListUserTags (ListUserTags'),
    newListUserTags,
    ListUserTagsResponse (ListUserTagsResponse'),
    newListUserTagsResponse,

    -- ** GetRole
    GetRole (GetRole'),
    newGetRole,
    GetRoleResponse (GetRoleResponse'),
    newGetRoleResponse,

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
