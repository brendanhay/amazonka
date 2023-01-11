{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.IAM
-- Copyright   : (c) 2013-2023 Brendan Hay
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

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** CredentialReportExpiredException
    _CredentialReportExpiredException,

    -- ** CredentialReportNotPresentException
    _CredentialReportNotPresentException,

    -- ** CredentialReportNotReadyException
    _CredentialReportNotReadyException,

    -- ** DeleteConflictException
    _DeleteConflictException,

    -- ** DuplicateCertificateException
    _DuplicateCertificateException,

    -- ** DuplicateSSHPublicKeyException
    _DuplicateSSHPublicKeyException,

    -- ** EntityAlreadyExistsException
    _EntityAlreadyExistsException,

    -- ** EntityTemporarilyUnmodifiableException
    _EntityTemporarilyUnmodifiableException,

    -- ** InvalidAuthenticationCodeException
    _InvalidAuthenticationCodeException,

    -- ** InvalidCertificateException
    _InvalidCertificateException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** InvalidPublicKeyException
    _InvalidPublicKeyException,

    -- ** InvalidUserTypeException
    _InvalidUserTypeException,

    -- ** KeyPairMismatchException
    _KeyPairMismatchException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** MalformedCertificateException
    _MalformedCertificateException,

    -- ** MalformedPolicyDocumentException
    _MalformedPolicyDocumentException,

    -- ** NoSuchEntityException
    _NoSuchEntityException,

    -- ** PasswordPolicyViolationException
    _PasswordPolicyViolationException,

    -- ** PolicyEvaluationException
    _PolicyEvaluationException,

    -- ** PolicyNotAttachableException
    _PolicyNotAttachableException,

    -- ** ReportGenerationLimitExceededException
    _ReportGenerationLimitExceededException,

    -- ** ServiceFailureException
    _ServiceFailureException,

    -- ** ServiceNotSupportedException
    _ServiceNotSupportedException,

    -- ** UnmodifiableEntityException
    _UnmodifiableEntityException,

    -- ** UnrecognizedPublicKeyEncodingException
    _UnrecognizedPublicKeyEncodingException,

    -- * Waiters
    -- $waiters

    -- ** InstanceProfileExists
    newInstanceProfileExists,

    -- ** PolicyExists
    newPolicyExists,

    -- ** RoleExists
    newRoleExists,

    -- ** UserExists
    newUserExists,

    -- * Operations
    -- $operations

    -- ** AddClientIDToOpenIDConnectProvider
    AddClientIDToOpenIDConnectProvider (AddClientIDToOpenIDConnectProvider'),
    newAddClientIDToOpenIDConnectProvider,
    AddClientIDToOpenIDConnectProviderResponse (AddClientIDToOpenIDConnectProviderResponse'),
    newAddClientIDToOpenIDConnectProviderResponse,

    -- ** AddRoleToInstanceProfile
    AddRoleToInstanceProfile (AddRoleToInstanceProfile'),
    newAddRoleToInstanceProfile,
    AddRoleToInstanceProfileResponse (AddRoleToInstanceProfileResponse'),
    newAddRoleToInstanceProfileResponse,

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

    -- ** AttachRolePolicy
    AttachRolePolicy (AttachRolePolicy'),
    newAttachRolePolicy,
    AttachRolePolicyResponse (AttachRolePolicyResponse'),
    newAttachRolePolicyResponse,

    -- ** AttachUserPolicy
    AttachUserPolicy (AttachUserPolicy'),
    newAttachUserPolicy,
    AttachUserPolicyResponse (AttachUserPolicyResponse'),
    newAttachUserPolicyResponse,

    -- ** ChangePassword
    ChangePassword (ChangePassword'),
    newChangePassword,
    ChangePasswordResponse (ChangePasswordResponse'),
    newChangePasswordResponse,

    -- ** CreateAccessKey
    CreateAccessKey (CreateAccessKey'),
    newCreateAccessKey,
    CreateAccessKeyResponse (CreateAccessKeyResponse'),
    newCreateAccessKeyResponse,

    -- ** CreateAccountAlias
    CreateAccountAlias (CreateAccountAlias'),
    newCreateAccountAlias,
    CreateAccountAliasResponse (CreateAccountAliasResponse'),
    newCreateAccountAliasResponse,

    -- ** CreateGroup
    CreateGroup (CreateGroup'),
    newCreateGroup,
    CreateGroupResponse (CreateGroupResponse'),
    newCreateGroupResponse,

    -- ** CreateInstanceProfile
    CreateInstanceProfile (CreateInstanceProfile'),
    newCreateInstanceProfile,
    CreateInstanceProfileResponse (CreateInstanceProfileResponse'),
    newCreateInstanceProfileResponse,

    -- ** CreateLoginProfile
    CreateLoginProfile (CreateLoginProfile'),
    newCreateLoginProfile,
    CreateLoginProfileResponse (CreateLoginProfileResponse'),
    newCreateLoginProfileResponse,

    -- ** CreateOpenIDConnectProvider
    CreateOpenIDConnectProvider (CreateOpenIDConnectProvider'),
    newCreateOpenIDConnectProvider,
    CreateOpenIDConnectProviderResponse (CreateOpenIDConnectProviderResponse'),
    newCreateOpenIDConnectProviderResponse,

    -- ** CreatePolicy
    CreatePolicy (CreatePolicy'),
    newCreatePolicy,
    CreatePolicyResponse (CreatePolicyResponse'),
    newCreatePolicyResponse,

    -- ** CreatePolicyVersion
    CreatePolicyVersion (CreatePolicyVersion'),
    newCreatePolicyVersion,
    CreatePolicyVersionResponse (CreatePolicyVersionResponse'),
    newCreatePolicyVersionResponse,

    -- ** CreateRole
    CreateRole (CreateRole'),
    newCreateRole,
    CreateRoleResponse (CreateRoleResponse'),
    newCreateRoleResponse,

    -- ** CreateSAMLProvider
    CreateSAMLProvider (CreateSAMLProvider'),
    newCreateSAMLProvider,
    CreateSAMLProviderResponse (CreateSAMLProviderResponse'),
    newCreateSAMLProviderResponse,

    -- ** CreateServiceLinkedRole
    CreateServiceLinkedRole (CreateServiceLinkedRole'),
    newCreateServiceLinkedRole,
    CreateServiceLinkedRoleResponse (CreateServiceLinkedRoleResponse'),
    newCreateServiceLinkedRoleResponse,

    -- ** CreateServiceSpecificCredential
    CreateServiceSpecificCredential (CreateServiceSpecificCredential'),
    newCreateServiceSpecificCredential,
    CreateServiceSpecificCredentialResponse (CreateServiceSpecificCredentialResponse'),
    newCreateServiceSpecificCredentialResponse,

    -- ** CreateUser
    CreateUser (CreateUser'),
    newCreateUser,
    CreateUserResponse (CreateUserResponse'),
    newCreateUserResponse,

    -- ** CreateVirtualMFADevice
    CreateVirtualMFADevice (CreateVirtualMFADevice'),
    newCreateVirtualMFADevice,
    CreateVirtualMFADeviceResponse (CreateVirtualMFADeviceResponse'),
    newCreateVirtualMFADeviceResponse,

    -- ** DeactivateMFADevice
    DeactivateMFADevice (DeactivateMFADevice'),
    newDeactivateMFADevice,
    DeactivateMFADeviceResponse (DeactivateMFADeviceResponse'),
    newDeactivateMFADeviceResponse,

    -- ** DeleteAccessKey
    DeleteAccessKey (DeleteAccessKey'),
    newDeleteAccessKey,
    DeleteAccessKeyResponse (DeleteAccessKeyResponse'),
    newDeleteAccessKeyResponse,

    -- ** DeleteAccountAlias
    DeleteAccountAlias (DeleteAccountAlias'),
    newDeleteAccountAlias,
    DeleteAccountAliasResponse (DeleteAccountAliasResponse'),
    newDeleteAccountAliasResponse,

    -- ** DeleteAccountPasswordPolicy
    DeleteAccountPasswordPolicy (DeleteAccountPasswordPolicy'),
    newDeleteAccountPasswordPolicy,
    DeleteAccountPasswordPolicyResponse (DeleteAccountPasswordPolicyResponse'),
    newDeleteAccountPasswordPolicyResponse,

    -- ** DeleteGroup
    DeleteGroup (DeleteGroup'),
    newDeleteGroup,
    DeleteGroupResponse (DeleteGroupResponse'),
    newDeleteGroupResponse,

    -- ** DeleteGroupPolicy
    DeleteGroupPolicy (DeleteGroupPolicy'),
    newDeleteGroupPolicy,
    DeleteGroupPolicyResponse (DeleteGroupPolicyResponse'),
    newDeleteGroupPolicyResponse,

    -- ** DeleteInstanceProfile
    DeleteInstanceProfile (DeleteInstanceProfile'),
    newDeleteInstanceProfile,
    DeleteInstanceProfileResponse (DeleteInstanceProfileResponse'),
    newDeleteInstanceProfileResponse,

    -- ** DeleteLoginProfile
    DeleteLoginProfile (DeleteLoginProfile'),
    newDeleteLoginProfile,
    DeleteLoginProfileResponse (DeleteLoginProfileResponse'),
    newDeleteLoginProfileResponse,

    -- ** DeleteOpenIDConnectProvider
    DeleteOpenIDConnectProvider (DeleteOpenIDConnectProvider'),
    newDeleteOpenIDConnectProvider,
    DeleteOpenIDConnectProviderResponse (DeleteOpenIDConnectProviderResponse'),
    newDeleteOpenIDConnectProviderResponse,

    -- ** DeletePolicy
    DeletePolicy (DeletePolicy'),
    newDeletePolicy,
    DeletePolicyResponse (DeletePolicyResponse'),
    newDeletePolicyResponse,

    -- ** DeletePolicyVersion
    DeletePolicyVersion (DeletePolicyVersion'),
    newDeletePolicyVersion,
    DeletePolicyVersionResponse (DeletePolicyVersionResponse'),
    newDeletePolicyVersionResponse,

    -- ** DeleteRole
    DeleteRole (DeleteRole'),
    newDeleteRole,
    DeleteRoleResponse (DeleteRoleResponse'),
    newDeleteRoleResponse,

    -- ** DeleteRolePermissionsBoundary
    DeleteRolePermissionsBoundary (DeleteRolePermissionsBoundary'),
    newDeleteRolePermissionsBoundary,
    DeleteRolePermissionsBoundaryResponse (DeleteRolePermissionsBoundaryResponse'),
    newDeleteRolePermissionsBoundaryResponse,

    -- ** DeleteRolePolicy
    DeleteRolePolicy (DeleteRolePolicy'),
    newDeleteRolePolicy,
    DeleteRolePolicyResponse (DeleteRolePolicyResponse'),
    newDeleteRolePolicyResponse,

    -- ** DeleteSAMLProvider
    DeleteSAMLProvider (DeleteSAMLProvider'),
    newDeleteSAMLProvider,
    DeleteSAMLProviderResponse (DeleteSAMLProviderResponse'),
    newDeleteSAMLProviderResponse,

    -- ** DeleteSSHPublicKey
    DeleteSSHPublicKey (DeleteSSHPublicKey'),
    newDeleteSSHPublicKey,
    DeleteSSHPublicKeyResponse (DeleteSSHPublicKeyResponse'),
    newDeleteSSHPublicKeyResponse,

    -- ** DeleteServerCertificate
    DeleteServerCertificate (DeleteServerCertificate'),
    newDeleteServerCertificate,
    DeleteServerCertificateResponse (DeleteServerCertificateResponse'),
    newDeleteServerCertificateResponse,

    -- ** DeleteServiceLinkedRole
    DeleteServiceLinkedRole (DeleteServiceLinkedRole'),
    newDeleteServiceLinkedRole,
    DeleteServiceLinkedRoleResponse (DeleteServiceLinkedRoleResponse'),
    newDeleteServiceLinkedRoleResponse,

    -- ** DeleteServiceSpecificCredential
    DeleteServiceSpecificCredential (DeleteServiceSpecificCredential'),
    newDeleteServiceSpecificCredential,
    DeleteServiceSpecificCredentialResponse (DeleteServiceSpecificCredentialResponse'),
    newDeleteServiceSpecificCredentialResponse,

    -- ** DeleteSigningCertificate
    DeleteSigningCertificate (DeleteSigningCertificate'),
    newDeleteSigningCertificate,
    DeleteSigningCertificateResponse (DeleteSigningCertificateResponse'),
    newDeleteSigningCertificateResponse,

    -- ** DeleteUser
    DeleteUser (DeleteUser'),
    newDeleteUser,
    DeleteUserResponse (DeleteUserResponse'),
    newDeleteUserResponse,

    -- ** DeleteUserPermissionsBoundary
    DeleteUserPermissionsBoundary (DeleteUserPermissionsBoundary'),
    newDeleteUserPermissionsBoundary,
    DeleteUserPermissionsBoundaryResponse (DeleteUserPermissionsBoundaryResponse'),
    newDeleteUserPermissionsBoundaryResponse,

    -- ** DeleteUserPolicy
    DeleteUserPolicy (DeleteUserPolicy'),
    newDeleteUserPolicy,
    DeleteUserPolicyResponse (DeleteUserPolicyResponse'),
    newDeleteUserPolicyResponse,

    -- ** DeleteVirtualMFADevice
    DeleteVirtualMFADevice (DeleteVirtualMFADevice'),
    newDeleteVirtualMFADevice,
    DeleteVirtualMFADeviceResponse (DeleteVirtualMFADeviceResponse'),
    newDeleteVirtualMFADeviceResponse,

    -- ** DetachGroupPolicy
    DetachGroupPolicy (DetachGroupPolicy'),
    newDetachGroupPolicy,
    DetachGroupPolicyResponse (DetachGroupPolicyResponse'),
    newDetachGroupPolicyResponse,

    -- ** DetachRolePolicy
    DetachRolePolicy (DetachRolePolicy'),
    newDetachRolePolicy,
    DetachRolePolicyResponse (DetachRolePolicyResponse'),
    newDetachRolePolicyResponse,

    -- ** DetachUserPolicy
    DetachUserPolicy (DetachUserPolicy'),
    newDetachUserPolicy,
    DetachUserPolicyResponse (DetachUserPolicyResponse'),
    newDetachUserPolicyResponse,

    -- ** EnableMFADevice
    EnableMFADevice (EnableMFADevice'),
    newEnableMFADevice,
    EnableMFADeviceResponse (EnableMFADeviceResponse'),
    newEnableMFADeviceResponse,

    -- ** GenerateCredentialReport
    GenerateCredentialReport (GenerateCredentialReport'),
    newGenerateCredentialReport,
    GenerateCredentialReportResponse (GenerateCredentialReportResponse'),
    newGenerateCredentialReportResponse,

    -- ** GenerateOrganizationsAccessReport
    GenerateOrganizationsAccessReport (GenerateOrganizationsAccessReport'),
    newGenerateOrganizationsAccessReport,
    GenerateOrganizationsAccessReportResponse (GenerateOrganizationsAccessReportResponse'),
    newGenerateOrganizationsAccessReportResponse,

    -- ** GenerateServiceLastAccessedDetails
    GenerateServiceLastAccessedDetails (GenerateServiceLastAccessedDetails'),
    newGenerateServiceLastAccessedDetails,
    GenerateServiceLastAccessedDetailsResponse (GenerateServiceLastAccessedDetailsResponse'),
    newGenerateServiceLastAccessedDetailsResponse,

    -- ** GetAccessKeyLastUsed
    GetAccessKeyLastUsed (GetAccessKeyLastUsed'),
    newGetAccessKeyLastUsed,
    GetAccessKeyLastUsedResponse (GetAccessKeyLastUsedResponse'),
    newGetAccessKeyLastUsedResponse,

    -- ** GetAccountAuthorizationDetails (Paginated)
    GetAccountAuthorizationDetails (GetAccountAuthorizationDetails'),
    newGetAccountAuthorizationDetails,
    GetAccountAuthorizationDetailsResponse (GetAccountAuthorizationDetailsResponse'),
    newGetAccountAuthorizationDetailsResponse,

    -- ** GetAccountPasswordPolicy
    GetAccountPasswordPolicy (GetAccountPasswordPolicy'),
    newGetAccountPasswordPolicy,
    GetAccountPasswordPolicyResponse (GetAccountPasswordPolicyResponse'),
    newGetAccountPasswordPolicyResponse,

    -- ** GetAccountSummary
    GetAccountSummary (GetAccountSummary'),
    newGetAccountSummary,
    GetAccountSummaryResponse (GetAccountSummaryResponse'),
    newGetAccountSummaryResponse,

    -- ** GetContextKeysForCustomPolicy
    GetContextKeysForCustomPolicy (GetContextKeysForCustomPolicy'),
    newGetContextKeysForCustomPolicy,
    GetContextKeysForPolicyResponse (GetContextKeysForPolicyResponse'),
    newGetContextKeysForPolicyResponse,

    -- ** GetContextKeysForPrincipalPolicy
    GetContextKeysForPrincipalPolicy (GetContextKeysForPrincipalPolicy'),
    newGetContextKeysForPrincipalPolicy,
    GetContextKeysForPolicyResponse (GetContextKeysForPolicyResponse'),
    newGetContextKeysForPolicyResponse,

    -- ** GetCredentialReport
    GetCredentialReport (GetCredentialReport'),
    newGetCredentialReport,
    GetCredentialReportResponse (GetCredentialReportResponse'),
    newGetCredentialReportResponse,

    -- ** GetGroup (Paginated)
    GetGroup (GetGroup'),
    newGetGroup,
    GetGroupResponse (GetGroupResponse'),
    newGetGroupResponse,

    -- ** GetGroupPolicy
    GetGroupPolicy (GetGroupPolicy'),
    newGetGroupPolicy,
    GetGroupPolicyResponse (GetGroupPolicyResponse'),
    newGetGroupPolicyResponse,

    -- ** GetInstanceProfile
    GetInstanceProfile (GetInstanceProfile'),
    newGetInstanceProfile,
    GetInstanceProfileResponse (GetInstanceProfileResponse'),
    newGetInstanceProfileResponse,

    -- ** GetLoginProfile
    GetLoginProfile (GetLoginProfile'),
    newGetLoginProfile,
    GetLoginProfileResponse (GetLoginProfileResponse'),
    newGetLoginProfileResponse,

    -- ** GetOpenIDConnectProvider
    GetOpenIDConnectProvider (GetOpenIDConnectProvider'),
    newGetOpenIDConnectProvider,
    GetOpenIDConnectProviderResponse (GetOpenIDConnectProviderResponse'),
    newGetOpenIDConnectProviderResponse,

    -- ** GetOrganizationsAccessReport
    GetOrganizationsAccessReport (GetOrganizationsAccessReport'),
    newGetOrganizationsAccessReport,
    GetOrganizationsAccessReportResponse (GetOrganizationsAccessReportResponse'),
    newGetOrganizationsAccessReportResponse,

    -- ** GetPolicy
    GetPolicy (GetPolicy'),
    newGetPolicy,
    GetPolicyResponse (GetPolicyResponse'),
    newGetPolicyResponse,

    -- ** GetPolicyVersion
    GetPolicyVersion (GetPolicyVersion'),
    newGetPolicyVersion,
    GetPolicyVersionResponse (GetPolicyVersionResponse'),
    newGetPolicyVersionResponse,

    -- ** GetRole
    GetRole (GetRole'),
    newGetRole,
    GetRoleResponse (GetRoleResponse'),
    newGetRoleResponse,

    -- ** GetRolePolicy
    GetRolePolicy (GetRolePolicy'),
    newGetRolePolicy,
    GetRolePolicyResponse (GetRolePolicyResponse'),
    newGetRolePolicyResponse,

    -- ** GetSAMLProvider
    GetSAMLProvider (GetSAMLProvider'),
    newGetSAMLProvider,
    GetSAMLProviderResponse (GetSAMLProviderResponse'),
    newGetSAMLProviderResponse,

    -- ** GetSSHPublicKey
    GetSSHPublicKey (GetSSHPublicKey'),
    newGetSSHPublicKey,
    GetSSHPublicKeyResponse (GetSSHPublicKeyResponse'),
    newGetSSHPublicKeyResponse,

    -- ** GetServerCertificate
    GetServerCertificate (GetServerCertificate'),
    newGetServerCertificate,
    GetServerCertificateResponse (GetServerCertificateResponse'),
    newGetServerCertificateResponse,

    -- ** GetServiceLastAccessedDetails
    GetServiceLastAccessedDetails (GetServiceLastAccessedDetails'),
    newGetServiceLastAccessedDetails,
    GetServiceLastAccessedDetailsResponse (GetServiceLastAccessedDetailsResponse'),
    newGetServiceLastAccessedDetailsResponse,

    -- ** GetServiceLastAccessedDetailsWithEntities
    GetServiceLastAccessedDetailsWithEntities (GetServiceLastAccessedDetailsWithEntities'),
    newGetServiceLastAccessedDetailsWithEntities,
    GetServiceLastAccessedDetailsWithEntitiesResponse (GetServiceLastAccessedDetailsWithEntitiesResponse'),
    newGetServiceLastAccessedDetailsWithEntitiesResponse,

    -- ** GetServiceLinkedRoleDeletionStatus
    GetServiceLinkedRoleDeletionStatus (GetServiceLinkedRoleDeletionStatus'),
    newGetServiceLinkedRoleDeletionStatus,
    GetServiceLinkedRoleDeletionStatusResponse (GetServiceLinkedRoleDeletionStatusResponse'),
    newGetServiceLinkedRoleDeletionStatusResponse,

    -- ** GetUser
    GetUser (GetUser'),
    newGetUser,
    GetUserResponse (GetUserResponse'),
    newGetUserResponse,

    -- ** GetUserPolicy
    GetUserPolicy (GetUserPolicy'),
    newGetUserPolicy,
    GetUserPolicyResponse (GetUserPolicyResponse'),
    newGetUserPolicyResponse,

    -- ** ListAccessKeys (Paginated)
    ListAccessKeys (ListAccessKeys'),
    newListAccessKeys,
    ListAccessKeysResponse (ListAccessKeysResponse'),
    newListAccessKeysResponse,

    -- ** ListAccountAliases (Paginated)
    ListAccountAliases (ListAccountAliases'),
    newListAccountAliases,
    ListAccountAliasesResponse (ListAccountAliasesResponse'),
    newListAccountAliasesResponse,

    -- ** ListAttachedGroupPolicies (Paginated)
    ListAttachedGroupPolicies (ListAttachedGroupPolicies'),
    newListAttachedGroupPolicies,
    ListAttachedGroupPoliciesResponse (ListAttachedGroupPoliciesResponse'),
    newListAttachedGroupPoliciesResponse,

    -- ** ListAttachedRolePolicies (Paginated)
    ListAttachedRolePolicies (ListAttachedRolePolicies'),
    newListAttachedRolePolicies,
    ListAttachedRolePoliciesResponse (ListAttachedRolePoliciesResponse'),
    newListAttachedRolePoliciesResponse,

    -- ** ListAttachedUserPolicies (Paginated)
    ListAttachedUserPolicies (ListAttachedUserPolicies'),
    newListAttachedUserPolicies,
    ListAttachedUserPoliciesResponse (ListAttachedUserPoliciesResponse'),
    newListAttachedUserPoliciesResponse,

    -- ** ListEntitiesForPolicy (Paginated)
    ListEntitiesForPolicy (ListEntitiesForPolicy'),
    newListEntitiesForPolicy,
    ListEntitiesForPolicyResponse (ListEntitiesForPolicyResponse'),
    newListEntitiesForPolicyResponse,

    -- ** ListGroupPolicies (Paginated)
    ListGroupPolicies (ListGroupPolicies'),
    newListGroupPolicies,
    ListGroupPoliciesResponse (ListGroupPoliciesResponse'),
    newListGroupPoliciesResponse,

    -- ** ListGroups (Paginated)
    ListGroups (ListGroups'),
    newListGroups,
    ListGroupsResponse (ListGroupsResponse'),
    newListGroupsResponse,

    -- ** ListGroupsForUser (Paginated)
    ListGroupsForUser (ListGroupsForUser'),
    newListGroupsForUser,
    ListGroupsForUserResponse (ListGroupsForUserResponse'),
    newListGroupsForUserResponse,

    -- ** ListInstanceProfileTags
    ListInstanceProfileTags (ListInstanceProfileTags'),
    newListInstanceProfileTags,
    ListInstanceProfileTagsResponse (ListInstanceProfileTagsResponse'),
    newListInstanceProfileTagsResponse,

    -- ** ListInstanceProfiles (Paginated)
    ListInstanceProfiles (ListInstanceProfiles'),
    newListInstanceProfiles,
    ListInstanceProfilesResponse (ListInstanceProfilesResponse'),
    newListInstanceProfilesResponse,

    -- ** ListInstanceProfilesForRole (Paginated)
    ListInstanceProfilesForRole (ListInstanceProfilesForRole'),
    newListInstanceProfilesForRole,
    ListInstanceProfilesForRoleResponse (ListInstanceProfilesForRoleResponse'),
    newListInstanceProfilesForRoleResponse,

    -- ** ListMFADeviceTags
    ListMFADeviceTags (ListMFADeviceTags'),
    newListMFADeviceTags,
    ListMFADeviceTagsResponse (ListMFADeviceTagsResponse'),
    newListMFADeviceTagsResponse,

    -- ** ListMFADevices (Paginated)
    ListMFADevices (ListMFADevices'),
    newListMFADevices,
    ListMFADevicesResponse (ListMFADevicesResponse'),
    newListMFADevicesResponse,

    -- ** ListOpenIDConnectProviderTags
    ListOpenIDConnectProviderTags (ListOpenIDConnectProviderTags'),
    newListOpenIDConnectProviderTags,
    ListOpenIDConnectProviderTagsResponse (ListOpenIDConnectProviderTagsResponse'),
    newListOpenIDConnectProviderTagsResponse,

    -- ** ListOpenIDConnectProviders
    ListOpenIDConnectProviders (ListOpenIDConnectProviders'),
    newListOpenIDConnectProviders,
    ListOpenIDConnectProvidersResponse (ListOpenIDConnectProvidersResponse'),
    newListOpenIDConnectProvidersResponse,

    -- ** ListPolicies (Paginated)
    ListPolicies (ListPolicies'),
    newListPolicies,
    ListPoliciesResponse (ListPoliciesResponse'),
    newListPoliciesResponse,

    -- ** ListPoliciesGrantingServiceAccess
    ListPoliciesGrantingServiceAccess (ListPoliciesGrantingServiceAccess'),
    newListPoliciesGrantingServiceAccess,
    ListPoliciesGrantingServiceAccessResponse (ListPoliciesGrantingServiceAccessResponse'),
    newListPoliciesGrantingServiceAccessResponse,

    -- ** ListPolicyTags
    ListPolicyTags (ListPolicyTags'),
    newListPolicyTags,
    ListPolicyTagsResponse (ListPolicyTagsResponse'),
    newListPolicyTagsResponse,

    -- ** ListPolicyVersions (Paginated)
    ListPolicyVersions (ListPolicyVersions'),
    newListPolicyVersions,
    ListPolicyVersionsResponse (ListPolicyVersionsResponse'),
    newListPolicyVersionsResponse,

    -- ** ListRolePolicies (Paginated)
    ListRolePolicies (ListRolePolicies'),
    newListRolePolicies,
    ListRolePoliciesResponse (ListRolePoliciesResponse'),
    newListRolePoliciesResponse,

    -- ** ListRoleTags
    ListRoleTags (ListRoleTags'),
    newListRoleTags,
    ListRoleTagsResponse (ListRoleTagsResponse'),
    newListRoleTagsResponse,

    -- ** ListRoles (Paginated)
    ListRoles (ListRoles'),
    newListRoles,
    ListRolesResponse (ListRolesResponse'),
    newListRolesResponse,

    -- ** ListSAMLProviderTags
    ListSAMLProviderTags (ListSAMLProviderTags'),
    newListSAMLProviderTags,
    ListSAMLProviderTagsResponse (ListSAMLProviderTagsResponse'),
    newListSAMLProviderTagsResponse,

    -- ** ListSAMLProviders
    ListSAMLProviders (ListSAMLProviders'),
    newListSAMLProviders,
    ListSAMLProvidersResponse (ListSAMLProvidersResponse'),
    newListSAMLProvidersResponse,

    -- ** ListSSHPublicKeys (Paginated)
    ListSSHPublicKeys (ListSSHPublicKeys'),
    newListSSHPublicKeys,
    ListSSHPublicKeysResponse (ListSSHPublicKeysResponse'),
    newListSSHPublicKeysResponse,

    -- ** ListServerCertificateTags
    ListServerCertificateTags (ListServerCertificateTags'),
    newListServerCertificateTags,
    ListServerCertificateTagsResponse (ListServerCertificateTagsResponse'),
    newListServerCertificateTagsResponse,

    -- ** ListServerCertificates (Paginated)
    ListServerCertificates (ListServerCertificates'),
    newListServerCertificates,
    ListServerCertificatesResponse (ListServerCertificatesResponse'),
    newListServerCertificatesResponse,

    -- ** ListServiceSpecificCredentials
    ListServiceSpecificCredentials (ListServiceSpecificCredentials'),
    newListServiceSpecificCredentials,
    ListServiceSpecificCredentialsResponse (ListServiceSpecificCredentialsResponse'),
    newListServiceSpecificCredentialsResponse,

    -- ** ListSigningCertificates (Paginated)
    ListSigningCertificates (ListSigningCertificates'),
    newListSigningCertificates,
    ListSigningCertificatesResponse (ListSigningCertificatesResponse'),
    newListSigningCertificatesResponse,

    -- ** ListUserPolicies (Paginated)
    ListUserPolicies (ListUserPolicies'),
    newListUserPolicies,
    ListUserPoliciesResponse (ListUserPoliciesResponse'),
    newListUserPoliciesResponse,

    -- ** ListUserTags (Paginated)
    ListUserTags (ListUserTags'),
    newListUserTags,
    ListUserTagsResponse (ListUserTagsResponse'),
    newListUserTagsResponse,

    -- ** ListUsers (Paginated)
    ListUsers (ListUsers'),
    newListUsers,
    ListUsersResponse (ListUsersResponse'),
    newListUsersResponse,

    -- ** ListVirtualMFADevices (Paginated)
    ListVirtualMFADevices (ListVirtualMFADevices'),
    newListVirtualMFADevices,
    ListVirtualMFADevicesResponse (ListVirtualMFADevicesResponse'),
    newListVirtualMFADevicesResponse,

    -- ** PutGroupPolicy
    PutGroupPolicy (PutGroupPolicy'),
    newPutGroupPolicy,
    PutGroupPolicyResponse (PutGroupPolicyResponse'),
    newPutGroupPolicyResponse,

    -- ** PutRolePermissionsBoundary
    PutRolePermissionsBoundary (PutRolePermissionsBoundary'),
    newPutRolePermissionsBoundary,
    PutRolePermissionsBoundaryResponse (PutRolePermissionsBoundaryResponse'),
    newPutRolePermissionsBoundaryResponse,

    -- ** PutRolePolicy
    PutRolePolicy (PutRolePolicy'),
    newPutRolePolicy,
    PutRolePolicyResponse (PutRolePolicyResponse'),
    newPutRolePolicyResponse,

    -- ** PutUserPermissionsBoundary
    PutUserPermissionsBoundary (PutUserPermissionsBoundary'),
    newPutUserPermissionsBoundary,
    PutUserPermissionsBoundaryResponse (PutUserPermissionsBoundaryResponse'),
    newPutUserPermissionsBoundaryResponse,

    -- ** PutUserPolicy
    PutUserPolicy (PutUserPolicy'),
    newPutUserPolicy,
    PutUserPolicyResponse (PutUserPolicyResponse'),
    newPutUserPolicyResponse,

    -- ** RemoveClientIDFromOpenIDConnectProvider
    RemoveClientIDFromOpenIDConnectProvider (RemoveClientIDFromOpenIDConnectProvider'),
    newRemoveClientIDFromOpenIDConnectProvider,
    RemoveClientIDFromOpenIDConnectProviderResponse (RemoveClientIDFromOpenIDConnectProviderResponse'),
    newRemoveClientIDFromOpenIDConnectProviderResponse,

    -- ** RemoveRoleFromInstanceProfile
    RemoveRoleFromInstanceProfile (RemoveRoleFromInstanceProfile'),
    newRemoveRoleFromInstanceProfile,
    RemoveRoleFromInstanceProfileResponse (RemoveRoleFromInstanceProfileResponse'),
    newRemoveRoleFromInstanceProfileResponse,

    -- ** RemoveUserFromGroup
    RemoveUserFromGroup (RemoveUserFromGroup'),
    newRemoveUserFromGroup,
    RemoveUserFromGroupResponse (RemoveUserFromGroupResponse'),
    newRemoveUserFromGroupResponse,

    -- ** ResetServiceSpecificCredential
    ResetServiceSpecificCredential (ResetServiceSpecificCredential'),
    newResetServiceSpecificCredential,
    ResetServiceSpecificCredentialResponse (ResetServiceSpecificCredentialResponse'),
    newResetServiceSpecificCredentialResponse,

    -- ** ResyncMFADevice
    ResyncMFADevice (ResyncMFADevice'),
    newResyncMFADevice,
    ResyncMFADeviceResponse (ResyncMFADeviceResponse'),
    newResyncMFADeviceResponse,

    -- ** SetDefaultPolicyVersion
    SetDefaultPolicyVersion (SetDefaultPolicyVersion'),
    newSetDefaultPolicyVersion,
    SetDefaultPolicyVersionResponse (SetDefaultPolicyVersionResponse'),
    newSetDefaultPolicyVersionResponse,

    -- ** SetSecurityTokenServicePreferences
    SetSecurityTokenServicePreferences (SetSecurityTokenServicePreferences'),
    newSetSecurityTokenServicePreferences,
    SetSecurityTokenServicePreferencesResponse (SetSecurityTokenServicePreferencesResponse'),
    newSetSecurityTokenServicePreferencesResponse,

    -- ** SimulateCustomPolicy (Paginated)
    SimulateCustomPolicy (SimulateCustomPolicy'),
    newSimulateCustomPolicy,
    SimulatePolicyResponse (SimulatePolicyResponse'),
    newSimulatePolicyResponse,

    -- ** SimulatePrincipalPolicy (Paginated)
    SimulatePrincipalPolicy (SimulatePrincipalPolicy'),
    newSimulatePrincipalPolicy,
    SimulatePolicyResponse (SimulatePolicyResponse'),
    newSimulatePolicyResponse,

    -- ** TagInstanceProfile
    TagInstanceProfile (TagInstanceProfile'),
    newTagInstanceProfile,
    TagInstanceProfileResponse (TagInstanceProfileResponse'),
    newTagInstanceProfileResponse,

    -- ** TagMFADevice
    TagMFADevice (TagMFADevice'),
    newTagMFADevice,
    TagMFADeviceResponse (TagMFADeviceResponse'),
    newTagMFADeviceResponse,

    -- ** TagOpenIDConnectProvider
    TagOpenIDConnectProvider (TagOpenIDConnectProvider'),
    newTagOpenIDConnectProvider,
    TagOpenIDConnectProviderResponse (TagOpenIDConnectProviderResponse'),
    newTagOpenIDConnectProviderResponse,

    -- ** TagPolicy
    TagPolicy (TagPolicy'),
    newTagPolicy,
    TagPolicyResponse (TagPolicyResponse'),
    newTagPolicyResponse,

    -- ** TagRole
    TagRole (TagRole'),
    newTagRole,
    TagRoleResponse (TagRoleResponse'),
    newTagRoleResponse,

    -- ** TagSAMLProvider
    TagSAMLProvider (TagSAMLProvider'),
    newTagSAMLProvider,
    TagSAMLProviderResponse (TagSAMLProviderResponse'),
    newTagSAMLProviderResponse,

    -- ** TagServerCertificate
    TagServerCertificate (TagServerCertificate'),
    newTagServerCertificate,
    TagServerCertificateResponse (TagServerCertificateResponse'),
    newTagServerCertificateResponse,

    -- ** TagUser
    TagUser (TagUser'),
    newTagUser,
    TagUserResponse (TagUserResponse'),
    newTagUserResponse,

    -- ** UntagInstanceProfile
    UntagInstanceProfile (UntagInstanceProfile'),
    newUntagInstanceProfile,
    UntagInstanceProfileResponse (UntagInstanceProfileResponse'),
    newUntagInstanceProfileResponse,

    -- ** UntagMFADevice
    UntagMFADevice (UntagMFADevice'),
    newUntagMFADevice,
    UntagMFADeviceResponse (UntagMFADeviceResponse'),
    newUntagMFADeviceResponse,

    -- ** UntagOpenIDConnectProvider
    UntagOpenIDConnectProvider (UntagOpenIDConnectProvider'),
    newUntagOpenIDConnectProvider,
    UntagOpenIDConnectProviderResponse (UntagOpenIDConnectProviderResponse'),
    newUntagOpenIDConnectProviderResponse,

    -- ** UntagPolicy
    UntagPolicy (UntagPolicy'),
    newUntagPolicy,
    UntagPolicyResponse (UntagPolicyResponse'),
    newUntagPolicyResponse,

    -- ** UntagRole
    UntagRole (UntagRole'),
    newUntagRole,
    UntagRoleResponse (UntagRoleResponse'),
    newUntagRoleResponse,

    -- ** UntagSAMLProvider
    UntagSAMLProvider (UntagSAMLProvider'),
    newUntagSAMLProvider,
    UntagSAMLProviderResponse (UntagSAMLProviderResponse'),
    newUntagSAMLProviderResponse,

    -- ** UntagServerCertificate
    UntagServerCertificate (UntagServerCertificate'),
    newUntagServerCertificate,
    UntagServerCertificateResponse (UntagServerCertificateResponse'),
    newUntagServerCertificateResponse,

    -- ** UntagUser
    UntagUser (UntagUser'),
    newUntagUser,
    UntagUserResponse (UntagUserResponse'),
    newUntagUserResponse,

    -- ** UpdateAccessKey
    UpdateAccessKey (UpdateAccessKey'),
    newUpdateAccessKey,
    UpdateAccessKeyResponse (UpdateAccessKeyResponse'),
    newUpdateAccessKeyResponse,

    -- ** UpdateAccountPasswordPolicy
    UpdateAccountPasswordPolicy (UpdateAccountPasswordPolicy'),
    newUpdateAccountPasswordPolicy,
    UpdateAccountPasswordPolicyResponse (UpdateAccountPasswordPolicyResponse'),
    newUpdateAccountPasswordPolicyResponse,

    -- ** UpdateAssumeRolePolicy
    UpdateAssumeRolePolicy (UpdateAssumeRolePolicy'),
    newUpdateAssumeRolePolicy,
    UpdateAssumeRolePolicyResponse (UpdateAssumeRolePolicyResponse'),
    newUpdateAssumeRolePolicyResponse,

    -- ** UpdateGroup
    UpdateGroup (UpdateGroup'),
    newUpdateGroup,
    UpdateGroupResponse (UpdateGroupResponse'),
    newUpdateGroupResponse,

    -- ** UpdateLoginProfile
    UpdateLoginProfile (UpdateLoginProfile'),
    newUpdateLoginProfile,
    UpdateLoginProfileResponse (UpdateLoginProfileResponse'),
    newUpdateLoginProfileResponse,

    -- ** UpdateOpenIDConnectProviderThumbprint
    UpdateOpenIDConnectProviderThumbprint (UpdateOpenIDConnectProviderThumbprint'),
    newUpdateOpenIDConnectProviderThumbprint,
    UpdateOpenIDConnectProviderThumbprintResponse (UpdateOpenIDConnectProviderThumbprintResponse'),
    newUpdateOpenIDConnectProviderThumbprintResponse,

    -- ** UpdateRole
    UpdateRole (UpdateRole'),
    newUpdateRole,
    UpdateRoleResponse (UpdateRoleResponse'),
    newUpdateRoleResponse,

    -- ** UpdateRoleDescription
    UpdateRoleDescription (UpdateRoleDescription'),
    newUpdateRoleDescription,
    UpdateRoleDescriptionResponse (UpdateRoleDescriptionResponse'),
    newUpdateRoleDescriptionResponse,

    -- ** UpdateSAMLProvider
    UpdateSAMLProvider (UpdateSAMLProvider'),
    newUpdateSAMLProvider,
    UpdateSAMLProviderResponse (UpdateSAMLProviderResponse'),
    newUpdateSAMLProviderResponse,

    -- ** UpdateSSHPublicKey
    UpdateSSHPublicKey (UpdateSSHPublicKey'),
    newUpdateSSHPublicKey,
    UpdateSSHPublicKeyResponse (UpdateSSHPublicKeyResponse'),
    newUpdateSSHPublicKeyResponse,

    -- ** UpdateServerCertificate
    UpdateServerCertificate (UpdateServerCertificate'),
    newUpdateServerCertificate,
    UpdateServerCertificateResponse (UpdateServerCertificateResponse'),
    newUpdateServerCertificateResponse,

    -- ** UpdateServiceSpecificCredential
    UpdateServiceSpecificCredential (UpdateServiceSpecificCredential'),
    newUpdateServiceSpecificCredential,
    UpdateServiceSpecificCredentialResponse (UpdateServiceSpecificCredentialResponse'),
    newUpdateServiceSpecificCredentialResponse,

    -- ** UpdateSigningCertificate
    UpdateSigningCertificate (UpdateSigningCertificate'),
    newUpdateSigningCertificate,
    UpdateSigningCertificateResponse (UpdateSigningCertificateResponse'),
    newUpdateSigningCertificateResponse,

    -- ** UpdateUser
    UpdateUser (UpdateUser'),
    newUpdateUser,
    UpdateUserResponse (UpdateUserResponse'),
    newUpdateUserResponse,

    -- ** UploadSSHPublicKey
    UploadSSHPublicKey (UploadSSHPublicKey'),
    newUploadSSHPublicKey,
    UploadSSHPublicKeyResponse (UploadSSHPublicKeyResponse'),
    newUploadSSHPublicKeyResponse,

    -- ** UploadServerCertificate
    UploadServerCertificate (UploadServerCertificate'),
    newUploadServerCertificate,
    UploadServerCertificateResponse (UploadServerCertificateResponse'),
    newUploadServerCertificateResponse,

    -- ** UploadSigningCertificate
    UploadSigningCertificate (UploadSigningCertificate'),
    newUploadSigningCertificate,
    UploadSigningCertificateResponse (UploadSigningCertificateResponse'),
    newUploadSigningCertificateResponse,

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
