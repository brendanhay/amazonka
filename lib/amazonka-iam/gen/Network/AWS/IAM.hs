{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Identity and Access Management__
--
-- AWS Identity and Access Management (IAM) is a web service for securely controlling access to AWS services. With IAM, you can centrally manage users, security credentials such as access keys, and permissions that control which AWS resources users and applications can access. For more information about IAM, see <http://aws.amazon.com/iam/ AWS Identity and Access Management (IAM)> and the <https://docs.aws.amazon.com/IAM/latest/UserGuide/ AWS Identity and Access Management User Guide> .
module Network.AWS.IAM
  ( -- * Service configuration
    mkServiceConfig,

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
    mkInstanceProfileExists,

    -- ** UserExists
    mkUserExists,

    -- ** RoleExists
    mkRoleExists,

    -- ** PolicyExists
    mkPolicyExists,

    -- * Operations
    -- $operations

    -- ** GetContextKeysForPrincipalPolicy
    module Network.AWS.IAM.GetContextKeysForPrincipalPolicy,

    -- ** ListPolicies (Paginated)
    module Network.AWS.IAM.ListPolicies,

    -- ** CreatePolicy
    module Network.AWS.IAM.CreatePolicy,

    -- ** ListInstanceProfilesForRole (Paginated)
    module Network.AWS.IAM.ListInstanceProfilesForRole,

    -- ** AttachGroupPolicy
    module Network.AWS.IAM.AttachGroupPolicy,

    -- ** CreateAccessKey
    module Network.AWS.IAM.CreateAccessKey,

    -- ** ListRoleTags
    module Network.AWS.IAM.ListRoleTags,

    -- ** ListSSHPublicKeys (Paginated)
    module Network.AWS.IAM.ListSSHPublicKeys,

    -- ** ListOpenIDConnectProviders
    module Network.AWS.IAM.ListOpenIDConnectProviders,

    -- ** CreateVirtualMFADevice
    module Network.AWS.IAM.CreateVirtualMFADevice,

    -- ** DeleteAccountPasswordPolicy
    module Network.AWS.IAM.DeleteAccountPasswordPolicy,

    -- ** UpdateAccountPasswordPolicy
    module Network.AWS.IAM.UpdateAccountPasswordPolicy,

    -- ** AttachRolePolicy
    module Network.AWS.IAM.AttachRolePolicy,

    -- ** UpdateSSHPublicKey
    module Network.AWS.IAM.UpdateSSHPublicKey,

    -- ** DeleteSSHPublicKey
    module Network.AWS.IAM.DeleteSSHPublicKey,

    -- ** GetUserPolicy
    module Network.AWS.IAM.GetUserPolicy,

    -- ** UpdateServiceSpecificCredential
    module Network.AWS.IAM.UpdateServiceSpecificCredential,

    -- ** DeleteServiceSpecificCredential
    module Network.AWS.IAM.DeleteServiceSpecificCredential,

    -- ** ListAttachedRolePolicies (Paginated)
    module Network.AWS.IAM.ListAttachedRolePolicies,

    -- ** GetRole
    module Network.AWS.IAM.GetRole,

    -- ** DeactivateMFADevice
    module Network.AWS.IAM.DeactivateMFADevice,

    -- ** CreateOpenIDConnectProvider
    module Network.AWS.IAM.CreateOpenIDConnectProvider,

    -- ** DeleteVirtualMFADevice
    module Network.AWS.IAM.DeleteVirtualMFADevice,

    -- ** ListRoles (Paginated)
    module Network.AWS.IAM.ListRoles,

    -- ** ListUserPolicies (Paginated)
    module Network.AWS.IAM.ListUserPolicies,

    -- ** PutRolePermissionsBoundary
    module Network.AWS.IAM.PutRolePermissionsBoundary,

    -- ** UploadSSHPublicKey
    module Network.AWS.IAM.UploadSSHPublicKey,

    -- ** DeleteRolePermissionsBoundary
    module Network.AWS.IAM.DeleteRolePermissionsBoundary,

    -- ** SimulateCustomPolicy (Paginated)
    module Network.AWS.IAM.SimulateCustomPolicy,

    -- ** UpdateRole
    module Network.AWS.IAM.UpdateRole,

    -- ** DeleteRole
    module Network.AWS.IAM.DeleteRole,

    -- ** ListUsers (Paginated)
    module Network.AWS.IAM.ListUsers,

    -- ** UpdateOpenIDConnectProviderThumbprint
    module Network.AWS.IAM.UpdateOpenIDConnectProviderThumbprint,

    -- ** PutUserPolicy
    module Network.AWS.IAM.PutUserPolicy,

    -- ** GetSSHPublicKey
    module Network.AWS.IAM.GetSSHPublicKey,

    -- ** UntagUser
    module Network.AWS.IAM.UntagUser,

    -- ** DetachGroupPolicy
    module Network.AWS.IAM.DetachGroupPolicy,

    -- ** GetOpenIDConnectProvider
    module Network.AWS.IAM.GetOpenIDConnectProvider,

    -- ** PutUserPermissionsBoundary
    module Network.AWS.IAM.PutUserPermissionsBoundary,

    -- ** DeleteUserPolicy
    module Network.AWS.IAM.DeleteUserPolicy,

    -- ** DeleteUserPermissionsBoundary
    module Network.AWS.IAM.DeleteUserPermissionsBoundary,

    -- ** CreateRole
    module Network.AWS.IAM.CreateRole,

    -- ** ResetServiceSpecificCredential
    module Network.AWS.IAM.ResetServiceSpecificCredential,

    -- ** GetCredentialReport
    module Network.AWS.IAM.GetCredentialReport,

    -- ** GetAccountSummary
    module Network.AWS.IAM.GetAccountSummary,

    -- ** GenerateServiceLastAccessedDetails
    module Network.AWS.IAM.GenerateServiceLastAccessedDetails,

    -- ** ListGroupPolicies (Paginated)
    module Network.AWS.IAM.ListGroupPolicies,

    -- ** DeletePolicyVersion
    module Network.AWS.IAM.DeletePolicyVersion,

    -- ** TagUser
    module Network.AWS.IAM.TagUser,

    -- ** DeleteInstanceProfile
    module Network.AWS.IAM.DeleteInstanceProfile,

    -- ** DetachRolePolicy
    module Network.AWS.IAM.DetachRolePolicy,

    -- ** RemoveRoleFromInstanceProfile
    module Network.AWS.IAM.RemoveRoleFromInstanceProfile,

    -- ** CreatePolicyVersion
    module Network.AWS.IAM.CreatePolicyVersion,

    -- ** CreateInstanceProfile
    module Network.AWS.IAM.CreateInstanceProfile,

    -- ** CreateSAMLProvider
    module Network.AWS.IAM.CreateSAMLProvider,

    -- ** GetAccountAuthorizationDetails (Paginated)
    module Network.AWS.IAM.GetAccountAuthorizationDetails,

    -- ** GetServiceLinkedRoleDeletionStatus
    module Network.AWS.IAM.GetServiceLinkedRoleDeletionStatus,

    -- ** DeleteAccountAlias
    module Network.AWS.IAM.DeleteAccountAlias,

    -- ** DetachUserPolicy
    module Network.AWS.IAM.DetachUserPolicy,

    -- ** RemoveUserFromGroup
    module Network.AWS.IAM.RemoveUserFromGroup,

    -- ** DeleteGroupPolicy
    module Network.AWS.IAM.DeleteGroupPolicy,

    -- ** TagRole
    module Network.AWS.IAM.TagRole,

    -- ** PutGroupPolicy
    module Network.AWS.IAM.PutGroupPolicy,

    -- ** GetLoginProfile
    module Network.AWS.IAM.GetLoginProfile,

    -- ** GetGroupPolicy
    module Network.AWS.IAM.GetGroupPolicy,

    -- ** GenerateOrganizationsAccessReport
    module Network.AWS.IAM.GenerateOrganizationsAccessReport,

    -- ** ChangePassword
    module Network.AWS.IAM.ChangePassword,

    -- ** ListServerCertificates (Paginated)
    module Network.AWS.IAM.ListServerCertificates,

    -- ** DeleteServiceLinkedRole
    module Network.AWS.IAM.DeleteServiceLinkedRole,

    -- ** DeletePolicy
    module Network.AWS.IAM.DeletePolicy,

    -- ** UpdateAssumeRolePolicy
    module Network.AWS.IAM.UpdateAssumeRolePolicy,

    -- ** GetServiceLastAccessedDetailsWithEntities
    module Network.AWS.IAM.GetServiceLastAccessedDetailsWithEntities,

    -- ** GetInstanceProfile
    module Network.AWS.IAM.GetInstanceProfile,

    -- ** CreateLoginProfile
    module Network.AWS.IAM.CreateLoginProfile,

    -- ** GetSAMLProvider
    module Network.AWS.IAM.GetSAMLProvider,

    -- ** AddRoleToInstanceProfile
    module Network.AWS.IAM.AddRoleToInstanceProfile,

    -- ** ListGroupsForUser (Paginated)
    module Network.AWS.IAM.ListGroupsForUser,

    -- ** ListEntitiesForPolicy (Paginated)
    module Network.AWS.IAM.ListEntitiesForPolicy,

    -- ** AddUserToGroup
    module Network.AWS.IAM.AddUserToGroup,

    -- ** SimulatePrincipalPolicy (Paginated)
    module Network.AWS.IAM.SimulatePrincipalPolicy,

    -- ** GetOrganizationsAccessReport
    module Network.AWS.IAM.GetOrganizationsAccessReport,

    -- ** GetPolicyVersion
    module Network.AWS.IAM.GetPolicyVersion,

    -- ** CreateServiceLinkedRole
    module Network.AWS.IAM.CreateServiceLinkedRole,

    -- ** ListServiceSpecificCredentials
    module Network.AWS.IAM.ListServiceSpecificCredentials,

    -- ** DeleteOpenIDConnectProvider
    module Network.AWS.IAM.DeleteOpenIDConnectProvider,

    -- ** GetUser
    module Network.AWS.IAM.GetUser,

    -- ** ListSigningCertificates (Paginated)
    module Network.AWS.IAM.ListSigningCertificates,

    -- ** DeleteSigningCertificate
    module Network.AWS.IAM.DeleteSigningCertificate,

    -- ** UpdateSigningCertificate
    module Network.AWS.IAM.UpdateSigningCertificate,

    -- ** ListAttachedUserPolicies (Paginated)
    module Network.AWS.IAM.ListAttachedUserPolicies,

    -- ** RemoveClientIDFromOpenIDConnectProvider
    module Network.AWS.IAM.RemoveClientIDFromOpenIDConnectProvider,

    -- ** AttachUserPolicy
    module Network.AWS.IAM.AttachUserPolicy,

    -- ** CreateServiceSpecificCredential
    module Network.AWS.IAM.CreateServiceSpecificCredential,

    -- ** ListVirtualMFADevices (Paginated)
    module Network.AWS.IAM.ListVirtualMFADevices,

    -- ** ResyncMFADevice
    module Network.AWS.IAM.ResyncMFADevice,

    -- ** DeleteAccessKey
    module Network.AWS.IAM.DeleteAccessKey,

    -- ** UpdateAccessKey
    module Network.AWS.IAM.UpdateAccessKey,

    -- ** ListUserTags
    module Network.AWS.IAM.ListUserTags,

    -- ** ListAccessKeys (Paginated)
    module Network.AWS.IAM.ListAccessKeys,

    -- ** GetRolePolicy
    module Network.AWS.IAM.GetRolePolicy,

    -- ** SetSecurityTokenServicePreferences
    module Network.AWS.IAM.SetSecurityTokenServicePreferences,

    -- ** UntagRole
    module Network.AWS.IAM.UntagRole,

    -- ** CreateUser
    module Network.AWS.IAM.CreateUser,

    -- ** PutRolePolicy
    module Network.AWS.IAM.PutRolePolicy,

    -- ** GetContextKeysForCustomPolicy
    module Network.AWS.IAM.GetContextKeysForCustomPolicy,

    -- ** UploadSigningCertificate
    module Network.AWS.IAM.UploadSigningCertificate,

    -- ** DeleteRolePolicy
    module Network.AWS.IAM.DeleteRolePolicy,

    -- ** GetAccountPasswordPolicy
    module Network.AWS.IAM.GetAccountPasswordPolicy,

    -- ** GetAccessKeyLastUsed
    module Network.AWS.IAM.GetAccessKeyLastUsed,

    -- ** UpdateUser
    module Network.AWS.IAM.UpdateUser,

    -- ** DeleteUser
    module Network.AWS.IAM.DeleteUser,

    -- ** AddClientIDToOpenIDConnectProvider
    module Network.AWS.IAM.AddClientIDToOpenIDConnectProvider,

    -- ** ListRolePolicies (Paginated)
    module Network.AWS.IAM.ListRolePolicies,

    -- ** CreateAccountAlias
    module Network.AWS.IAM.CreateAccountAlias,

    -- ** ListPoliciesGrantingServiceAccess
    module Network.AWS.IAM.ListPoliciesGrantingServiceAccess,

    -- ** ListInstanceProfiles (Paginated)
    module Network.AWS.IAM.ListInstanceProfiles,

    -- ** EnableMFADevice
    module Network.AWS.IAM.EnableMFADevice,

    -- ** ListAccountAliases (Paginated)
    module Network.AWS.IAM.ListAccountAliases,

    -- ** DeleteSAMLProvider
    module Network.AWS.IAM.DeleteSAMLProvider,

    -- ** UpdateSAMLProvider
    module Network.AWS.IAM.UpdateSAMLProvider,

    -- ** CreateGroup
    module Network.AWS.IAM.CreateGroup,

    -- ** ListMFADevices (Paginated)
    module Network.AWS.IAM.ListMFADevices,

    -- ** UploadServerCertificate
    module Network.AWS.IAM.UploadServerCertificate,

    -- ** SetDefaultPolicyVersion
    module Network.AWS.IAM.SetDefaultPolicyVersion,

    -- ** ListPolicyVersions (Paginated)
    module Network.AWS.IAM.ListPolicyVersions,

    -- ** UpdateRoleDescription
    module Network.AWS.IAM.UpdateRoleDescription,

    -- ** ListSAMLProviders
    module Network.AWS.IAM.ListSAMLProviders,

    -- ** GetServiceLastAccessedDetails
    module Network.AWS.IAM.GetServiceLastAccessedDetails,

    -- ** GetServerCertificate
    module Network.AWS.IAM.GetServerCertificate,

    -- ** DeleteGroup
    module Network.AWS.IAM.DeleteGroup,

    -- ** UpdateGroup
    module Network.AWS.IAM.UpdateGroup,

    -- ** ListGroups (Paginated)
    module Network.AWS.IAM.ListGroups,

    -- ** GenerateCredentialReport
    module Network.AWS.IAM.GenerateCredentialReport,

    -- ** GetPolicy
    module Network.AWS.IAM.GetPolicy,

    -- ** UpdateLoginProfile
    module Network.AWS.IAM.UpdateLoginProfile,

    -- ** DeleteLoginProfile
    module Network.AWS.IAM.DeleteLoginProfile,

    -- ** GetGroup (Paginated)
    module Network.AWS.IAM.GetGroup,

    -- ** DeleteServerCertificate
    module Network.AWS.IAM.DeleteServerCertificate,

    -- ** UpdateServerCertificate
    module Network.AWS.IAM.UpdateServerCertificate,

    -- ** ListAttachedGroupPolicies (Paginated)
    module Network.AWS.IAM.ListAttachedGroupPolicies,

    -- * Types

    -- ** ManagedPolicyDetail
    ManagedPolicyDetail (..),
    mkManagedPolicyDetail,
    mpdArn,
    mpdAttachmentCount,
    mpdCreateDate,
    mpdDefaultVersionId,
    mpdDescription,
    mpdIsAttachable,
    mpdPath,
    mpdPermissionsBoundaryUsageCount,
    mpdPolicyId,
    mpdPolicyName,
    mpdPolicyVersionList,
    mpdUpdateDate,

    -- ** VirtualMFADeviceName
    VirtualMFADeviceName (..),

    -- ** ReasonType
    ReasonType (..),

    -- ** PolicyRole
    PolicyRole (..),
    mkPolicyRole,
    prRoleId,
    prRoleName,

    -- ** AssignmentStatusType
    AssignmentStatusType (..),

    -- ** PasswordPolicy
    PasswordPolicy (..),
    mkPasswordPolicy,
    ppAllowUsersToChangePassword,
    ppExpirePasswords,
    ppHardExpiry,
    ppMaxPasswordAge,
    ppMinimumPasswordLength,
    ppPasswordReusePrevention,
    ppRequireLowercaseCharacters,
    ppRequireNumbers,
    ppRequireSymbols,
    ppRequireUppercaseCharacters,

    -- ** Group
    Group (..),
    mkGroup,
    gPath,
    gGroupName,
    gGroupId,
    gArn,
    gCreateDate,

    -- ** ThumbprintType
    ThumbprintType (..),

    -- ** PasswordType
    PasswordType (..),

    -- ** ContextKeyNameType
    ContextKeyNameType (..),

    -- ** PolicyVersionIdType
    PolicyVersionIdType (..),

    -- ** EvaluationResult
    EvaluationResult (..),
    mkEvaluationResult,
    erEvalActionName,
    erEvalDecision,
    erEvalDecisionDetails,
    erEvalResourceName,
    erMatchedStatements,
    erMissingContextValues,
    erOrganizationsDecisionDetail,
    erPermissionsBoundaryDecisionDetail,
    erResourceSpecificResults,

    -- ** TrackedActionLastAccessed
    TrackedActionLastAccessed (..),
    mkTrackedActionLastAccessed,
    talaActionName,
    talaLastAccessedEntity,
    talaLastAccessedRegion,
    talaLastAccessedTime,

    -- ** PolicyGrantingServiceAccess
    PolicyGrantingServiceAccess (..),
    mkPolicyGrantingServiceAccess,
    pgsaPolicyName,
    pgsaPolicyType,
    pgsaEntityName,
    pgsaEntityType,
    pgsaPolicyArn,

    -- ** MarkerType
    MarkerType (..),

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** AttachedPolicy
    AttachedPolicy (..),
    mkAttachedPolicy,
    apPolicyArn,
    apPolicyName,

    -- ** ServiceLastAccessed
    ServiceLastAccessed (..),
    mkServiceLastAccessed,
    slaServiceName,
    slaServiceNamespace,
    slaLastAuthenticated,
    slaLastAuthenticatedEntity,
    slaLastAuthenticatedRegion,
    slaTotalAuthenticatedEntities,
    slaTrackedActionsLastAccessed,

    -- ** MFADevice
    MFADevice (..),
    mkMFADevice,
    mfadUserName,
    mfadSerialNumber,
    mfadEnableDate,

    -- ** PolicyVersion
    PolicyVersion (..),
    mkPolicyVersion,
    pvCreateDate,
    pvDocument,
    pvIsDefaultVersion,
    pvVersionId,

    -- ** ServiceNameType
    ServiceNameType (..),

    -- ** AttachedPermissionsBoundary
    AttachedPermissionsBoundary (..),
    mkAttachedPermissionsBoundary,
    apbPermissionsBoundaryArn,
    apbPermissionsBoundaryType,

    -- ** InstanceProfile
    InstanceProfile (..),
    mkInstanceProfile,
    ipPath,
    ipInstanceProfileName,
    ipInstanceProfileId,
    ipArn,
    ipCreateDate,
    ipRoles,

    -- ** RoleDescriptionType
    RoleDescriptionType (..),

    -- ** PublicKeyMaterialType
    PublicKeyMaterialType (..),

    -- ** RoleDetail
    RoleDetail (..),
    mkRoleDetail,
    rdArn,
    rdAssumeRolePolicyDocument,
    rdAttachedManagedPolicies,
    rdCreateDate,
    rdInstanceProfileList,
    rdPath,
    rdPermissionsBoundary,
    rdRoleId,
    rdRoleLastUsed,
    rdRoleName,
    rdRolePolicyList,
    rdTags,

    -- ** SimulatePolicyResponse
    SimulatePolicyResponse (..),
    mkSimulatePolicyResponse,
    sprEvaluationResults,
    sprIsTruncated,
    sprMarker,

    -- ** ReportFormatType
    ReportFormatType (..),

    -- ** Statement
    Statement (..),
    mkStatement,
    sEndPosition,
    sSourcePolicyId,
    sSourcePolicyType,
    sStartPosition,

    -- ** InstanceProfileNameType
    InstanceProfileNameType (..),

    -- ** TagKeyType
    TagKeyType (..),

    -- ** AuthenticationCodeType
    AuthenticationCodeType (..),

    -- ** IdType
    IdType (..),

    -- ** OrganizationsPolicyIdType
    OrganizationsPolicyIdType (..),

    -- ** OpenIDConnectProviderUrlType
    OpenIDConnectProviderUrlType (..),

    -- ** CertificateBodyType
    CertificateBodyType (..),

    -- ** ServerCertificateMetadata
    ServerCertificateMetadata (..),
    mkServerCertificateMetadata,
    scmPath,
    scmServerCertificateName,
    scmServerCertificateId,
    scmArn,
    scmExpiration,
    scmUploadDate,

    -- ** DeletionTaskStatusType
    DeletionTaskStatusType (..),

    -- ** PolicyPathType
    PolicyPathType (..),

    -- ** GroupNameType
    GroupNameType (..),

    -- ** CertificateChainType
    CertificateChainType (..),

    -- ** PolicyType
    PolicyType (..),

    -- ** OpenIDConnectProviderListEntry
    OpenIDConnectProviderListEntry (..),
    mkOpenIDConnectProviderListEntry,
    oidcpleArn,

    -- ** LoginProfile
    LoginProfile (..),
    mkLoginProfile,
    lpUserName,
    lpCreateDate,
    lpPasswordResetRequired,

    -- ** EncodingType
    EncodingType (..),

    -- ** SerialNumberType
    SerialNumberType (..),

    -- ** EntityType
    EntityType (..),

    -- ** SSHPublicKeyMetadata
    SSHPublicKeyMetadata (..),
    mkSSHPublicKeyMetadata,
    sshpkmUserName,
    sshpkmSSHPublicKeyId,
    sshpkmStatus,
    sshpkmUploadDate,

    -- ** SummaryKeyType
    SummaryKeyType (..),

    -- ** ContextEntry
    ContextEntry (..),
    mkContextEntry,
    ceContextKeyName,
    ceContextKeyType,
    ceContextKeyValues,

    -- ** EvalDecisionSourceType
    EvalDecisionSourceType (..),

    -- ** JobStatusType
    JobStatusType (..),

    -- ** ContextKeyTypeEnum
    ContextKeyTypeEnum (..),

    -- ** OrganizationsEntityPathType
    OrganizationsEntityPathType (..),

    -- ** SSHPublicKey
    SSHPublicKey (..),
    mkSSHPublicKey,
    sshpkUserName,
    sshpkSSHPublicKeyId,
    sshpkFingerprint,
    sshpkSSHPublicKeyBody,
    sshpkStatus,
    sshpkUploadDate,

    -- ** PolicyEvaluationDecisionType
    PolicyEvaluationDecisionType (..),

    -- ** GroupDetail
    GroupDetail (..),
    mkGroupDetail,
    gdArn,
    gdAttachedManagedPolicies,
    gdCreateDate,
    gdGroupId,
    gdGroupName,
    gdGroupPolicyList,
    gdPath,

    -- ** ReportStateType
    ReportStateType (..),

    -- ** PermissionsBoundaryAttachmentType
    PermissionsBoundaryAttachmentType (..),

    -- ** ResponseMarkerType
    ResponseMarkerType (..),

    -- ** User
    User (..),
    mkUser,
    uPath,
    uUserName,
    uUserId,
    uArn,
    uCreateDate,
    uPasswordLastUsed,
    uPermissionsBoundary,
    uTags,

    -- ** RoleLastUsed
    RoleLastUsed (..),
    mkRoleLastUsed,
    rluLastUsedDate,
    rluRegion,

    -- ** PolicyDetail
    PolicyDetail (..),
    mkPolicyDetail,
    pdPolicyDocument,
    pdPolicyName,

    -- ** SAMLMetadataDocumentType
    SAMLMetadataDocumentType (..),

    -- ** GlobalEndpointTokenVersion
    GlobalEndpointTokenVersion (..),

    -- ** StatusType
    StatusType (..),

    -- ** SAMLProviderListEntry
    SAMLProviderListEntry (..),
    mkSAMLProviderListEntry,
    samlpleArn,
    samlpleCreateDate,
    samlpleValidUntil,

    -- ** ClientIDType
    ClientIDType (..),

    -- ** Role
    Role (..),
    mkRole,
    rPath,
    rRoleName,
    rRoleId,
    rArn,
    rCreateDate,
    rAssumeRolePolicyDocument,
    rDescription,
    rMaxSessionDuration,
    rPermissionsBoundary,
    rRoleLastUsed,
    rTags,

    -- ** PolicyNameType
    PolicyNameType (..),

    -- ** PolicyDocumentType
    PolicyDocumentType (..),

    -- ** AccountAliasType
    AccountAliasType (..),

    -- ** PolicyGroup
    PolicyGroup (..),
    mkPolicyGroup,
    pgGroupId,
    pgGroupName,

    -- ** ServerCertificateNameType
    ServerCertificateNameType (..),

    -- ** RegionNameType
    RegionNameType (..),

    -- ** ListPoliciesGrantingServiceAccessEntry
    ListPoliciesGrantingServiceAccessEntry (..),
    mkListPoliciesGrantingServiceAccessEntry,
    lpgsaePolicies,
    lpgsaeServiceNamespace,

    -- ** PolicyOwnerEntityType
    PolicyOwnerEntityType (..),

    -- ** PolicyScopeType
    PolicyScopeType (..),

    -- ** PrivateKeyType
    PrivateKeyType (..),

    -- ** ErrorDetails
    ErrorDetails (..),
    mkErrorDetails,
    edMessage,
    edCode,

    -- ** ServiceName
    ServiceName (..),

    -- ** ServicePassword
    ServicePassword (..),

    -- ** StringType
    StringType (..),

    -- ** ExistingUserNameType
    ExistingUserNameType (..),

    -- ** GetContextKeysForPolicyResponse
    GetContextKeysForPolicyResponse (..),
    mkGetContextKeysForPolicyResponse,
    gckfprContextKeyNames,

    -- ** ResourceNameType
    ResourceNameType (..),

    -- ** AccessDetail
    AccessDetail (..),
    mkAccessDetail,
    adServiceName,
    adServiceNamespace,
    adEntityPath,
    adLastAuthenticatedTime,
    adRegion,
    adTotalAuthenticatedEntities,

    -- ** PolicySourceType
    PolicySourceType (..),

    -- ** ArnType
    ArnType (..),

    -- ** PathType
    PathType (..),

    -- ** SortKeyType
    SortKeyType (..),

    -- ** EntityInfo
    EntityInfo (..),
    mkEntityInfo,
    eiArn,
    eiName,
    eiType,
    eiId,
    eiPath,

    -- ** DeletionTaskFailureReasonType
    DeletionTaskFailureReasonType (..),
    mkDeletionTaskFailureReasonType,
    dtfrtReason,
    dtfrtRoleUsageList,

    -- ** AccessKeySecretType
    AccessKeySecretType (..),

    -- ** PolicyUsageType
    PolicyUsageType (..),

    -- ** PathPrefixType
    PathPrefixType (..),

    -- ** UserDetail
    UserDetail (..),
    mkUserDetail,
    udArn,
    udAttachedManagedPolicies,
    udCreateDate,
    udGroupList,
    udPath,
    udPermissionsBoundary,
    udTags,
    udUserId,
    udUserName,
    udUserPolicyList,

    -- ** Policy
    Policy (..),
    mkPolicy,
    pArn,
    pAttachmentCount,
    pCreateDate,
    pDefaultVersionId,
    pDescription,
    pIsAttachable,
    pPath,
    pPermissionsBoundaryUsageCount,
    pPolicyId,
    pPolicyName,
    pUpdateDate,

    -- ** CertificateIdType
    CertificateIdType (..),

    -- ** ServerCertificate
    ServerCertificate (..),
    mkServerCertificate,
    sServerCertificateMetadata,
    sCertificateBody,
    sCertificateChain,

    -- ** ContextKeyValueType
    ContextKeyValueType (..),

    -- ** PublicKeyIdType
    PublicKeyIdType (..),

    -- ** ResourceSpecificResult
    ResourceSpecificResult (..),
    mkResourceSpecificResult,
    rsrEvalResourceName,
    rsrEvalResourceDecision,
    rsrEvalDecisionDetails,
    rsrMatchedStatements,
    rsrMissingContextValues,
    rsrPermissionsBoundaryDecisionDetail,

    -- ** ServiceSpecificCredentialMetadata
    ServiceSpecificCredentialMetadata (..),
    mkServiceSpecificCredentialMetadata,
    sscmUserName,
    sscmStatus,
    sscmServiceUserName,
    sscmCreateDate,
    sscmServiceSpecificCredentialId,
    sscmServiceName,

    -- ** AccessKeyInfo
    AccessKeyInfo (..),
    mkAccessKeyInfo,
    akiUserName,
    akiAccessKeyId,
    akiStatus,
    akiSecretAccessKey,
    akiCreateDate,

    -- ** ServiceSpecificCredentialId
    ServiceSpecificCredentialId (..),

    -- ** ServiceUserName
    ServiceUserName (..),

    -- ** RoleNameType
    RoleNameType (..),

    -- ** VirtualMFADevice
    VirtualMFADevice (..),
    mkVirtualMFADevice,
    vmfadSerialNumber,
    vmfadBase32StringSeed,
    vmfadEnableDate,
    vmfadQRCodePNG,
    vmfadUser,

    -- ** SigningCertificate
    SigningCertificate (..),
    mkSigningCertificate,
    scUserName,
    scCertificateId,
    scCertificateBody,
    scStatus,
    scUploadDate,

    -- ** UserNameType
    UserNameType (..),

    -- ** ServiceNamespaceType
    ServiceNamespaceType (..),

    -- ** ServiceSpecificCredential
    ServiceSpecificCredential (..),
    mkServiceSpecificCredential,
    sscCreateDate,
    sscServiceName,
    sscServiceUserName,
    sscServicePassword,
    sscServiceSpecificCredentialId,
    sscUserName,
    sscStatus,

    -- ** AccessKeyLastUsed
    AccessKeyLastUsed (..),
    mkAccessKeyLastUsed,
    akluLastUsedDate,
    akluServiceName,
    akluRegion,

    -- ** AccessKeyMetadata
    AccessKeyMetadata (..),
    mkAccessKeyMetadata,
    akmAccessKeyId,
    akmCreateDate,
    akmStatus,
    akmUserName,

    -- ** AccessAdvisorUsageGranularityType
    AccessAdvisorUsageGranularityType (..),

    -- ** PolicyUser
    PolicyUser (..),
    mkPolicyUser,
    puUserId,
    puUserName,

    -- ** RoleUsageType
    RoleUsageType (..),
    mkRoleUsageType,
    rutRegion,
    rutResources,

    -- ** PermissionsBoundaryDecisionDetail
    PermissionsBoundaryDecisionDetail (..),
    mkPermissionsBoundaryDecisionDetail,
    pbddAllowedByPermissionsBoundary,

    -- ** OrganizationsDecisionDetail
    OrganizationsDecisionDetail (..),
    mkOrganizationsDecisionDetail,
    oddAllowedByOrganizations,

    -- ** Position
    Position (..),
    mkPosition,
    pColumn,
    pLine,

    -- ** EntityDetails
    EntityDetails (..),
    mkEntityDetails,
    edEntityInfo,
    edLastAuthenticated,

    -- ** ActionNameType
    ActionNameType (..),

    -- ** Arn
    Arn (..),

    -- ** DefaultVersionId
    DefaultVersionId (..),

    -- ** Description
    Description (..),

    -- ** Path
    Path (..),

    -- ** PolicyId
    PolicyId (..),

    -- ** PolicyName
    PolicyName (..),

    -- ** UserName
    UserName (..),

    -- ** PolicyDocument
    PolicyDocument (..),

    -- ** Marker
    Marker (..),

    -- ** PathPrefix
    PathPrefix (..),

    -- ** RoleName
    RoleName (..),

    -- ** Message
    Message (..),

    -- ** RoleId
    RoleId (..),

    -- ** CallerArn
    CallerArn (..),

    -- ** ResourceHandlingOption
    ResourceHandlingOption (..),

    -- ** ResourceOwner
    ResourceOwner (..),

    -- ** ResourcePolicy
    ResourcePolicy (..),

    -- ** SSHPublicKeyBody
    SSHPublicKeyBody (..),

    -- ** PermissionsBoundary
    PermissionsBoundary (..),

    -- ** InstanceProfileName
    InstanceProfileName (..),

    -- ** GroupName
    GroupName (..),

    -- ** GroupId
    GroupId (..),

    -- ** PolicyArn
    PolicyArn (..),

    -- ** EvalActionName
    EvalActionName (..),

    -- ** EvalResourceName
    EvalResourceName (..),

    -- ** AccountAlias
    AccountAlias (..),

    -- ** DeletionTaskId
    DeletionTaskId (..),

    -- ** ActionName
    ActionName (..),

    -- ** LastAccessedEntity
    LastAccessedEntity (..),

    -- ** LastAccessedRegion
    LastAccessedRegion (..),

    -- ** SAMLMetadataDocument
    SAMLMetadataDocument (..),

    -- ** Name
    Name (..),

    -- ** EntityName
    EntityName (..),

    -- ** ServiceSpecificCredentialId
    ServiceSpecificCredentialId (..),

    -- ** Key
    Key (..),

    -- ** Value
    Value (..),

    -- ** ServiceName
    ServiceName (..),

    -- ** ServiceNamespace
    ServiceNamespace (..),

    -- ** LastAuthenticatedEntity
    LastAuthenticatedEntity (..),

    -- ** LastAuthenticatedRegion
    LastAuthenticatedRegion (..),

    -- ** SerialNumber
    SerialNumber (..),

    -- ** Document
    Document (..),

    -- ** PermissionsBoundaryArn
    PermissionsBoundaryArn (..),

    -- ** InstanceProfileId
    InstanceProfileId (..),

    -- ** JobId
    JobId (..),

    -- ** SAMLProviderArn
    SAMLProviderArn (..),

    -- ** PolicySourceArn
    PolicySourceArn (..),

    -- ** AssumeRolePolicyDocument
    AssumeRolePolicyDocument (..),

    -- ** SourcePolicyId
    SourcePolicyId (..),

    -- ** ServerCertificateName
    ServerCertificateName (..),

    -- ** AWSServiceName
    AWSServiceName (..),

    -- ** CustomSuffix
    CustomSuffix (..),

    -- ** EntityPath
    EntityPath (..),

    -- ** AccessKeyId
    AccessKeyId (..),

    -- ** SSHPublicKeyId
    SSHPublicKeyId (..),

    -- ** OpenIDConnectProviderArn
    OpenIDConnectProviderArn (..),

    -- ** ClientID
    ClientID (..),

    -- ** Fingerprint
    Fingerprint (..),

    -- ** CertificateId
    CertificateId (..),

    -- ** Region
    Region (..),

    -- ** NewPath
    NewPath (..),

    -- ** NewUserName
    NewUserName (..),

    -- ** Code
    Code (..),

    -- ** ServiceUserName
    ServiceUserName (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
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
import Network.AWS.IAM.ListAccessKeys
import Network.AWS.IAM.ListAccountAliases
import Network.AWS.IAM.ListAttachedGroupPolicies
import Network.AWS.IAM.ListAttachedRolePolicies
import Network.AWS.IAM.ListAttachedUserPolicies
import Network.AWS.IAM.ListEntitiesForPolicy
import Network.AWS.IAM.ListGroupPolicies
import Network.AWS.IAM.ListGroups
import Network.AWS.IAM.ListGroupsForUser
import Network.AWS.IAM.ListInstanceProfiles
import Network.AWS.IAM.ListInstanceProfilesForRole
import Network.AWS.IAM.ListMFADevices
import Network.AWS.IAM.ListOpenIDConnectProviders
import Network.AWS.IAM.ListPolicies
import Network.AWS.IAM.ListPoliciesGrantingServiceAccess
import Network.AWS.IAM.ListPolicyVersions
import Network.AWS.IAM.ListRolePolicies
import Network.AWS.IAM.ListRoleTags
import Network.AWS.IAM.ListRoles
import Network.AWS.IAM.ListSAMLProviders
import Network.AWS.IAM.ListSSHPublicKeys
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
import Network.AWS.IAM.TagRole
import Network.AWS.IAM.TagUser
import Network.AWS.IAM.Types
import Network.AWS.IAM.UntagRole
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
import qualified Network.AWS.Prelude as Lude

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
