{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _MalformedPolicyDocumentException,
    _PolicyEvaluationException,
    _UnrecognizedPublicKeyEncodingException,
    _ServiceNotSupportedException,
    _ReportGenerationLimitExceededException,
    _DuplicateSSHPublicKeyException,
    _KeyPairMismatchException,
    _PolicyNotAttachableException,
    _InvalidInputException,
    _InvalidPublicKeyException,
    _UnmodifiableEntityException,
    _DuplicateCertificateException,
    _MalformedCertificateException,
    _EntityAlreadyExistsException,
    _ConcurrentModificationException,
    _ServiceFailureException,
    _InvalidUserTypeException,
    _CredentialReportNotReadyException,
    _CredentialReportNotPresentException,
    _LimitExceededException,
    _PasswordPolicyViolationException,
    _InvalidAuthenticationCodeException,
    _EntityTemporarilyUnmodifiableException,
    _InvalidCertificateException,
    _NoSuchEntityException,
    _DeleteConflictException,
    _CredentialReportExpiredException,

    -- * AccessAdvisorUsageGranularityType
    AccessAdvisorUsageGranularityType (..),

    -- * AssignmentStatusType
    AssignmentStatusType (..),

    -- * ContextKeyTypeEnum
    ContextKeyTypeEnum (..),

    -- * DeletionTaskStatusType
    DeletionTaskStatusType (..),

    -- * EncodingType
    EncodingType (..),

    -- * EntityType
    EntityType (..),

    -- * GlobalEndpointTokenVersion
    GlobalEndpointTokenVersion (..),

    -- * JobStatusType
    JobStatusType (..),

    -- * PermissionsBoundaryAttachmentType
    PermissionsBoundaryAttachmentType (..),

    -- * PolicyEvaluationDecisionType
    PolicyEvaluationDecisionType (..),

    -- * PolicyOwnerEntityType
    PolicyOwnerEntityType (..),

    -- * PolicyScopeType
    PolicyScopeType (..),

    -- * PolicySourceType
    PolicySourceType (..),

    -- * PolicyType
    PolicyType (..),

    -- * PolicyUsageType
    PolicyUsageType (..),

    -- * ReportFormatType
    ReportFormatType (..),

    -- * ReportStateType
    ReportStateType (..),

    -- * SortKeyType
    SortKeyType (..),

    -- * StatusType
    StatusType (..),

    -- * SummaryKeyType
    SummaryKeyType (..),

    -- * AccessDetail
    AccessDetail (..),
    newAccessDetail,
    accessDetail_totalAuthenticatedEntities,
    accessDetail_entityPath,
    accessDetail_lastAuthenticatedTime,
    accessDetail_region,
    accessDetail_serviceName,
    accessDetail_serviceNamespace,

    -- * AccessKeyInfo
    AccessKeyInfo (..),
    newAccessKeyInfo,
    accessKeyInfo_createDate,
    accessKeyInfo_userName,
    accessKeyInfo_accessKeyId,
    accessKeyInfo_status,
    accessKeyInfo_secretAccessKey,

    -- * AccessKeyLastUsed
    AccessKeyLastUsed (..),
    newAccessKeyLastUsed,
    accessKeyLastUsed_lastUsedDate,
    accessKeyLastUsed_serviceName,
    accessKeyLastUsed_region,

    -- * AccessKeyMetadata
    AccessKeyMetadata (..),
    newAccessKeyMetadata,
    accessKeyMetadata_status,
    accessKeyMetadata_createDate,
    accessKeyMetadata_accessKeyId,
    accessKeyMetadata_userName,

    -- * AttachedPermissionsBoundary
    AttachedPermissionsBoundary (..),
    newAttachedPermissionsBoundary,
    attachedPermissionsBoundary_permissionsBoundaryArn,
    attachedPermissionsBoundary_permissionsBoundaryType,

    -- * AttachedPolicy
    AttachedPolicy (..),
    newAttachedPolicy,
    attachedPolicy_policyName,
    attachedPolicy_policyArn,

    -- * ContextEntry
    ContextEntry (..),
    newContextEntry,
    contextEntry_contextKeyValues,
    contextEntry_contextKeyName,
    contextEntry_contextKeyType,

    -- * DeletionTaskFailureReasonType
    DeletionTaskFailureReasonType (..),
    newDeletionTaskFailureReasonType,
    deletionTaskFailureReasonType_reason,
    deletionTaskFailureReasonType_roleUsageList,

    -- * EntityDetails
    EntityDetails (..),
    newEntityDetails,
    entityDetails_lastAuthenticated,
    entityDetails_entityInfo,

    -- * EntityInfo
    EntityInfo (..),
    newEntityInfo,
    entityInfo_path,
    entityInfo_arn,
    entityInfo_name,
    entityInfo_type,
    entityInfo_id,

    -- * ErrorDetails
    ErrorDetails (..),
    newErrorDetails,
    errorDetails_message,
    errorDetails_code,

    -- * EvaluationResult
    EvaluationResult (..),
    newEvaluationResult,
    evaluationResult_evalDecisionDetails,
    evaluationResult_permissionsBoundaryDecisionDetail,
    evaluationResult_organizationsDecisionDetail,
    evaluationResult_resourceSpecificResults,
    evaluationResult_matchedStatements,
    evaluationResult_evalResourceName,
    evaluationResult_missingContextValues,
    evaluationResult_evalActionName,
    evaluationResult_evalDecision,

    -- * GetContextKeysForPolicyResponse
    GetContextKeysForPolicyResponse (..),
    newGetContextKeysForPolicyResponse,
    getContextKeysForPolicyResponse_contextKeyNames,

    -- * Group
    Group (..),
    newGroup,
    group_path,
    group_groupName,
    group_groupId,
    group_arn,
    group_createDate,

    -- * GroupDetail
    GroupDetail (..),
    newGroupDetail,
    groupDetail_attachedManagedPolicies,
    groupDetail_groupName,
    groupDetail_createDate,
    groupDetail_arn,
    groupDetail_groupId,
    groupDetail_groupPolicyList,
    groupDetail_path,

    -- * InstanceProfile
    InstanceProfile (..),
    newInstanceProfile,
    instanceProfile_tags,
    instanceProfile_path,
    instanceProfile_instanceProfileName,
    instanceProfile_instanceProfileId,
    instanceProfile_arn,
    instanceProfile_createDate,
    instanceProfile_roles,

    -- * ListPoliciesGrantingServiceAccessEntry
    ListPoliciesGrantingServiceAccessEntry (..),
    newListPoliciesGrantingServiceAccessEntry,
    listPoliciesGrantingServiceAccessEntry_policies,
    listPoliciesGrantingServiceAccessEntry_serviceNamespace,

    -- * LoginProfile
    LoginProfile (..),
    newLoginProfile,
    loginProfile_passwordResetRequired,
    loginProfile_userName,
    loginProfile_createDate,

    -- * MFADevice
    MFADevice (..),
    newMFADevice,
    mfaDevice_userName,
    mfaDevice_serialNumber,
    mfaDevice_enableDate,

    -- * ManagedPolicyDetail
    ManagedPolicyDetail (..),
    newManagedPolicyDetail,
    managedPolicyDetail_policyName,
    managedPolicyDetail_permissionsBoundaryUsageCount,
    managedPolicyDetail_isAttachable,
    managedPolicyDetail_createDate,
    managedPolicyDetail_arn,
    managedPolicyDetail_attachmentCount,
    managedPolicyDetail_defaultVersionId,
    managedPolicyDetail_description,
    managedPolicyDetail_policyVersionList,
    managedPolicyDetail_path,
    managedPolicyDetail_policyId,
    managedPolicyDetail_updateDate,

    -- * OpenIDConnectProviderListEntry
    OpenIDConnectProviderListEntry (..),
    newOpenIDConnectProviderListEntry,
    openIDConnectProviderListEntry_arn,

    -- * OrganizationsDecisionDetail
    OrganizationsDecisionDetail (..),
    newOrganizationsDecisionDetail,
    organizationsDecisionDetail_allowedByOrganizations,

    -- * PasswordPolicy
    PasswordPolicy (..),
    newPasswordPolicy,
    passwordPolicy_maxPasswordAge,
    passwordPolicy_requireLowercaseCharacters,
    passwordPolicy_minimumPasswordLength,
    passwordPolicy_passwordReusePrevention,
    passwordPolicy_expirePasswords,
    passwordPolicy_requireUppercaseCharacters,
    passwordPolicy_allowUsersToChangePassword,
    passwordPolicy_hardExpiry,
    passwordPolicy_requireSymbols,
    passwordPolicy_requireNumbers,

    -- * PermissionsBoundaryDecisionDetail
    PermissionsBoundaryDecisionDetail (..),
    newPermissionsBoundaryDecisionDetail,
    permissionsBoundaryDecisionDetail_allowedByPermissionsBoundary,

    -- * Policy
    Policy (..),
    newPolicy,
    policy_policyName,
    policy_permissionsBoundaryUsageCount,
    policy_isAttachable,
    policy_createDate,
    policy_arn,
    policy_attachmentCount,
    policy_defaultVersionId,
    policy_tags,
    policy_description,
    policy_path,
    policy_policyId,
    policy_updateDate,

    -- * PolicyDetail
    PolicyDetail (..),
    newPolicyDetail,
    policyDetail_policyName,
    policyDetail_policyDocument,

    -- * PolicyGrantingServiceAccess
    PolicyGrantingServiceAccess (..),
    newPolicyGrantingServiceAccess,
    policyGrantingServiceAccess_entityName,
    policyGrantingServiceAccess_entityType,
    policyGrantingServiceAccess_policyArn,
    policyGrantingServiceAccess_policyName,
    policyGrantingServiceAccess_policyType,

    -- * PolicyGroup
    PolicyGroup (..),
    newPolicyGroup,
    policyGroup_groupName,
    policyGroup_groupId,

    -- * PolicyRole
    PolicyRole (..),
    newPolicyRole,
    policyRole_roleId,
    policyRole_roleName,

    -- * PolicyUser
    PolicyUser (..),
    newPolicyUser,
    policyUser_userId,
    policyUser_userName,

    -- * PolicyVersion
    PolicyVersion (..),
    newPolicyVersion,
    policyVersion_createDate,
    policyVersion_versionId,
    policyVersion_document,
    policyVersion_isDefaultVersion,

    -- * Position
    Position (..),
    newPosition,
    position_column,
    position_line,

    -- * ResourceSpecificResult
    ResourceSpecificResult (..),
    newResourceSpecificResult,
    resourceSpecificResult_evalDecisionDetails,
    resourceSpecificResult_permissionsBoundaryDecisionDetail,
    resourceSpecificResult_matchedStatements,
    resourceSpecificResult_missingContextValues,
    resourceSpecificResult_evalResourceName,
    resourceSpecificResult_evalResourceDecision,

    -- * Role
    Role (..),
    newRole,
    role_assumeRolePolicyDocument,
    role_maxSessionDuration,
    role_roleLastUsed,
    role_permissionsBoundary,
    role_tags,
    role_description,
    role_path,
    role_roleName,
    role_roleId,
    role_arn,
    role_createDate,

    -- * RoleDetail
    RoleDetail (..),
    newRoleDetail,
    roleDetail_assumeRolePolicyDocument,
    roleDetail_roleId,
    roleDetail_roleLastUsed,
    roleDetail_attachedManagedPolicies,
    roleDetail_rolePolicyList,
    roleDetail_roleName,
    roleDetail_permissionsBoundary,
    roleDetail_createDate,
    roleDetail_arn,
    roleDetail_tags,
    roleDetail_instanceProfileList,
    roleDetail_path,

    -- * RoleLastUsed
    RoleLastUsed (..),
    newRoleLastUsed,
    roleLastUsed_lastUsedDate,
    roleLastUsed_region,

    -- * RoleUsageType
    RoleUsageType (..),
    newRoleUsageType,
    roleUsageType_resources,
    roleUsageType_region,

    -- * SAMLProviderListEntry
    SAMLProviderListEntry (..),
    newSAMLProviderListEntry,
    sAMLProviderListEntry_createDate,
    sAMLProviderListEntry_arn,
    sAMLProviderListEntry_validUntil,

    -- * SSHPublicKey
    SSHPublicKey (..),
    newSSHPublicKey,
    sSHPublicKey_uploadDate,
    sSHPublicKey_userName,
    sSHPublicKey_sSHPublicKeyId,
    sSHPublicKey_fingerprint,
    sSHPublicKey_sSHPublicKeyBody,
    sSHPublicKey_status,

    -- * SSHPublicKeyMetadata
    SSHPublicKeyMetadata (..),
    newSSHPublicKeyMetadata,
    sSHPublicKeyMetadata_userName,
    sSHPublicKeyMetadata_sSHPublicKeyId,
    sSHPublicKeyMetadata_status,
    sSHPublicKeyMetadata_uploadDate,

    -- * ServerCertificate
    ServerCertificate (..),
    newServerCertificate,
    serverCertificate_tags,
    serverCertificate_certificateChain,
    serverCertificate_serverCertificateMetadata,
    serverCertificate_certificateBody,

    -- * ServerCertificateMetadata
    ServerCertificateMetadata (..),
    newServerCertificateMetadata,
    serverCertificateMetadata_uploadDate,
    serverCertificateMetadata_expiration,
    serverCertificateMetadata_path,
    serverCertificateMetadata_serverCertificateName,
    serverCertificateMetadata_serverCertificateId,
    serverCertificateMetadata_arn,

    -- * ServiceLastAccessed
    ServiceLastAccessed (..),
    newServiceLastAccessed,
    serviceLastAccessed_totalAuthenticatedEntities,
    serviceLastAccessed_lastAuthenticatedRegion,
    serviceLastAccessed_lastAuthenticatedEntity,
    serviceLastAccessed_trackedActionsLastAccessed,
    serviceLastAccessed_lastAuthenticated,
    serviceLastAccessed_serviceName,
    serviceLastAccessed_serviceNamespace,

    -- * ServiceSpecificCredential
    ServiceSpecificCredential (..),
    newServiceSpecificCredential,
    serviceSpecificCredential_createDate,
    serviceSpecificCredential_serviceName,
    serviceSpecificCredential_serviceUserName,
    serviceSpecificCredential_servicePassword,
    serviceSpecificCredential_serviceSpecificCredentialId,
    serviceSpecificCredential_userName,
    serviceSpecificCredential_status,

    -- * ServiceSpecificCredentialMetadata
    ServiceSpecificCredentialMetadata (..),
    newServiceSpecificCredentialMetadata,
    serviceSpecificCredentialMetadata_userName,
    serviceSpecificCredentialMetadata_status,
    serviceSpecificCredentialMetadata_serviceUserName,
    serviceSpecificCredentialMetadata_createDate,
    serviceSpecificCredentialMetadata_serviceSpecificCredentialId,
    serviceSpecificCredentialMetadata_serviceName,

    -- * SigningCertificate
    SigningCertificate (..),
    newSigningCertificate,
    signingCertificate_uploadDate,
    signingCertificate_userName,
    signingCertificate_certificateId,
    signingCertificate_certificateBody,
    signingCertificate_status,

    -- * SimulatePolicyResponse
    SimulatePolicyResponse (..),
    newSimulatePolicyResponse,
    simulatePolicyResponse_isTruncated,
    simulatePolicyResponse_evaluationResults,
    simulatePolicyResponse_marker,

    -- * Statement
    Statement (..),
    newStatement,
    statement_startPosition,
    statement_sourcePolicyType,
    statement_endPosition,
    statement_sourcePolicyId,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TrackedActionLastAccessed
    TrackedActionLastAccessed (..),
    newTrackedActionLastAccessed,
    trackedActionLastAccessed_actionName,
    trackedActionLastAccessed_lastAccessedTime,
    trackedActionLastAccessed_lastAccessedRegion,
    trackedActionLastAccessed_lastAccessedEntity,

    -- * User
    User (..),
    newUser,
    user_permissionsBoundary,
    user_passwordLastUsed,
    user_tags,
    user_path,
    user_userName,
    user_userId,
    user_arn,
    user_createDate,

    -- * UserDetail
    UserDetail (..),
    newUserDetail,
    userDetail_attachedManagedPolicies,
    userDetail_permissionsBoundary,
    userDetail_createDate,
    userDetail_arn,
    userDetail_groupList,
    userDetail_userId,
    userDetail_tags,
    userDetail_userName,
    userDetail_userPolicyList,
    userDetail_path,

    -- * VirtualMFADevice
    VirtualMFADevice (..),
    newVirtualMFADevice,
    virtualMFADevice_user,
    virtualMFADevice_enableDate,
    virtualMFADevice_qRCodePNG,
    virtualMFADevice_tags,
    virtualMFADevice_base32StringSeed,
    virtualMFADevice_serialNumber,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types.AccessAdvisorUsageGranularityType
import Network.AWS.IAM.Types.AccessDetail
import Network.AWS.IAM.Types.AccessKeyInfo
import Network.AWS.IAM.Types.AccessKeyLastUsed
import Network.AWS.IAM.Types.AccessKeyMetadata
import Network.AWS.IAM.Types.AssignmentStatusType
import Network.AWS.IAM.Types.AttachedPermissionsBoundary
import Network.AWS.IAM.Types.AttachedPolicy
import Network.AWS.IAM.Types.ContextEntry
import Network.AWS.IAM.Types.ContextKeyTypeEnum
import Network.AWS.IAM.Types.DeletionTaskFailureReasonType
import Network.AWS.IAM.Types.DeletionTaskStatusType
import Network.AWS.IAM.Types.EncodingType
import Network.AWS.IAM.Types.EntityDetails
import Network.AWS.IAM.Types.EntityInfo
import Network.AWS.IAM.Types.EntityType
import Network.AWS.IAM.Types.ErrorDetails
import Network.AWS.IAM.Types.EvaluationResult
import Network.AWS.IAM.Types.GetContextKeysForPolicyResponse
import Network.AWS.IAM.Types.GlobalEndpointTokenVersion
import Network.AWS.IAM.Types.Group
import Network.AWS.IAM.Types.GroupDetail
import Network.AWS.IAM.Types.InstanceProfile
import Network.AWS.IAM.Types.JobStatusType
import Network.AWS.IAM.Types.ListPoliciesGrantingServiceAccessEntry
import Network.AWS.IAM.Types.LoginProfile
import Network.AWS.IAM.Types.MFADevice
import Network.AWS.IAM.Types.ManagedPolicyDetail
import Network.AWS.IAM.Types.OpenIDConnectProviderListEntry
import Network.AWS.IAM.Types.OrganizationsDecisionDetail
import Network.AWS.IAM.Types.PasswordPolicy
import Network.AWS.IAM.Types.PermissionsBoundaryAttachmentType
import Network.AWS.IAM.Types.PermissionsBoundaryDecisionDetail
import Network.AWS.IAM.Types.Policy
import Network.AWS.IAM.Types.PolicyDetail
import Network.AWS.IAM.Types.PolicyEvaluationDecisionType
import Network.AWS.IAM.Types.PolicyGrantingServiceAccess
import Network.AWS.IAM.Types.PolicyGroup
import Network.AWS.IAM.Types.PolicyOwnerEntityType
import Network.AWS.IAM.Types.PolicyRole
import Network.AWS.IAM.Types.PolicyScopeType
import Network.AWS.IAM.Types.PolicySourceType
import Network.AWS.IAM.Types.PolicyType
import Network.AWS.IAM.Types.PolicyUsageType
import Network.AWS.IAM.Types.PolicyUser
import Network.AWS.IAM.Types.PolicyVersion
import Network.AWS.IAM.Types.Position
import Network.AWS.IAM.Types.ReportFormatType
import Network.AWS.IAM.Types.ReportStateType
import Network.AWS.IAM.Types.ResourceSpecificResult
import Network.AWS.IAM.Types.Role
import Network.AWS.IAM.Types.RoleDetail
import Network.AWS.IAM.Types.RoleLastUsed
import Network.AWS.IAM.Types.RoleUsageType
import Network.AWS.IAM.Types.SAMLProviderListEntry
import Network.AWS.IAM.Types.SSHPublicKey
import Network.AWS.IAM.Types.SSHPublicKeyMetadata
import Network.AWS.IAM.Types.ServerCertificate
import Network.AWS.IAM.Types.ServerCertificateMetadata
import Network.AWS.IAM.Types.ServiceLastAccessed
import Network.AWS.IAM.Types.ServiceSpecificCredential
import Network.AWS.IAM.Types.ServiceSpecificCredentialMetadata
import Network.AWS.IAM.Types.SigningCertificate
import Network.AWS.IAM.Types.SimulatePolicyResponse
import Network.AWS.IAM.Types.SortKeyType
import Network.AWS.IAM.Types.Statement
import Network.AWS.IAM.Types.StatusType
import Network.AWS.IAM.Types.SummaryKeyType
import Network.AWS.IAM.Types.Tag
import Network.AWS.IAM.Types.TrackedActionLastAccessed
import Network.AWS.IAM.Types.User
import Network.AWS.IAM.Types.UserDetail
import Network.AWS.IAM.Types.VirtualMFADevice
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2010-05-08@ of the Amazon Identity and Access Management SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "IAM",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "iam",
      Core._serviceSigningName = "iam",
      Core._serviceVersion = "2010-05-08",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseXMLError "IAM",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | The request was rejected because the policy document was malformed. The
-- error message describes the specific error.
_MalformedPolicyDocumentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MalformedPolicyDocumentException =
  Core._MatchServiceError
    defaultService
    "MalformedPolicyDocument"
    Core.. Core.hasStatus 400

-- | The request failed because a provided policy could not be successfully
-- evaluated. An additional detailed message indicates the source of the
-- failure.
_PolicyEvaluationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PolicyEvaluationException =
  Core._MatchServiceError
    defaultService
    "PolicyEvaluation"
    Core.. Core.hasStatus 500

-- | The request was rejected because the public key encoding format is
-- unsupported or unrecognized.
_UnrecognizedPublicKeyEncodingException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnrecognizedPublicKeyEncodingException =
  Core._MatchServiceError
    defaultService
    "UnrecognizedPublicKeyEncoding"
    Core.. Core.hasStatus 400

-- | The specified service does not support service-specific credentials.
_ServiceNotSupportedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceNotSupportedException =
  Core._MatchServiceError
    defaultService
    "NotSupportedService"
    Core.. Core.hasStatus 404

-- | The request failed because the maximum number of concurrent requests for
-- this account are already running.
_ReportGenerationLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReportGenerationLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ReportGenerationLimitExceeded"
    Core.. Core.hasStatus 409

-- | The request was rejected because the SSH public key is already
-- associated with the specified IAM user.
_DuplicateSSHPublicKeyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateSSHPublicKeyException =
  Core._MatchServiceError
    defaultService
    "DuplicateSSHPublicKey"
    Core.. Core.hasStatus 400

-- | The request was rejected because the public key certificate and the
-- private key do not match.
_KeyPairMismatchException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KeyPairMismatchException =
  Core._MatchServiceError
    defaultService
    "KeyPairMismatch"
    Core.. Core.hasStatus 400

-- | The request failed because AWS service role policies can only be
-- attached to the service-linked role for that service.
_PolicyNotAttachableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PolicyNotAttachableException =
  Core._MatchServiceError
    defaultService
    "PolicyNotAttachable"
    Core.. Core.hasStatus 400

-- | The request was rejected because an invalid or out-of-range value was
-- supplied for an input parameter.
_InvalidInputException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInput"
    Core.. Core.hasStatus 400

-- | The request was rejected because the public key is malformed or
-- otherwise invalid.
_InvalidPublicKeyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidPublicKeyException =
  Core._MatchServiceError
    defaultService
    "InvalidPublicKey"
    Core.. Core.hasStatus 400

-- | The request was rejected because only the service that depends on the
-- service-linked role can modify or delete the role on your behalf. The
-- error message includes the name of the service that depends on this
-- service-linked role. You must request the change through that service.
_UnmodifiableEntityException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnmodifiableEntityException =
  Core._MatchServiceError
    defaultService
    "UnmodifiableEntity"
    Core.. Core.hasStatus 400

-- | The request was rejected because the same certificate is associated with
-- an IAM user in the account.
_DuplicateCertificateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateCertificateException =
  Core._MatchServiceError
    defaultService
    "DuplicateCertificate"
    Core.. Core.hasStatus 409

-- | The request was rejected because the certificate was malformed or
-- expired. The error message describes the specific error.
_MalformedCertificateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MalformedCertificateException =
  Core._MatchServiceError
    defaultService
    "MalformedCertificate"
    Core.. Core.hasStatus 400

-- | The request was rejected because it attempted to create a resource that
-- already exists.
_EntityAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EntityAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "EntityAlreadyExists"
    Core.. Core.hasStatus 409

-- | The request was rejected because multiple requests to change this object
-- were submitted simultaneously. Wait a few minutes and submit your
-- request again.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModification"
    Core.. Core.hasStatus 409

-- | The request processing has failed because of an unknown error, exception
-- or failure.
_ServiceFailureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceFailureException =
  Core._MatchServiceError
    defaultService
    "ServiceFailure"
    Core.. Core.hasStatus 500

-- | The request was rejected because the type of user for the transaction
-- was incorrect.
_InvalidUserTypeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidUserTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidUserType"
    Core.. Core.hasStatus 400

-- | The request was rejected because the credential report is still being
-- generated.
_CredentialReportNotReadyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CredentialReportNotReadyException =
  Core._MatchServiceError
    defaultService
    "ReportInProgress"
    Core.. Core.hasStatus 404

-- | The request was rejected because the credential report does not exist.
-- To generate a credential report, use GenerateCredentialReport.
_CredentialReportNotPresentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CredentialReportNotPresentException =
  Core._MatchServiceError
    defaultService
    "ReportNotPresent"
    Core.. Core.hasStatus 410

-- | The request was rejected because it attempted to create resources beyond
-- the current AWS account limits. The error message describes the limit
-- exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceeded"
    Core.. Core.hasStatus 409

-- | The request was rejected because the provided password did not meet the
-- requirements imposed by the account password policy.
_PasswordPolicyViolationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PasswordPolicyViolationException =
  Core._MatchServiceError
    defaultService
    "PasswordPolicyViolation"
    Core.. Core.hasStatus 400

-- | The request was rejected because the authentication code was not
-- recognized. The error message describes the specific error.
_InvalidAuthenticationCodeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidAuthenticationCodeException =
  Core._MatchServiceError
    defaultService
    "InvalidAuthenticationCode"
    Core.. Core.hasStatus 403

-- | The request was rejected because it referenced an entity that is
-- temporarily unmodifiable, such as a user name that was deleted and then
-- recreated. The error indicates that the request is likely to succeed if
-- you try again after waiting several minutes. The error message describes
-- the entity.
_EntityTemporarilyUnmodifiableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EntityTemporarilyUnmodifiableException =
  Core._MatchServiceError
    defaultService
    "EntityTemporarilyUnmodifiable"
    Core.. Core.hasStatus 409

-- | The request was rejected because the certificate is invalid.
_InvalidCertificateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidCertificateException =
  Core._MatchServiceError
    defaultService
    "InvalidCertificate"
    Core.. Core.hasStatus 400

-- | The request was rejected because it referenced a resource entity that
-- does not exist. The error message describes the resource.
_NoSuchEntityException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchEntityException =
  Core._MatchServiceError
    defaultService
    "NoSuchEntity"
    Core.. Core.hasStatus 404

-- | The request was rejected because it attempted to delete a resource that
-- has attached subordinate entities. The error message describes these
-- entities.
_DeleteConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DeleteConflictException =
  Core._MatchServiceError
    defaultService
    "DeleteConflict"
    Core.. Core.hasStatus 409

-- | The request was rejected because the most recent credential report has
-- expired. To generate a new credential report, use
-- GenerateCredentialReport. For more information about credential report
-- expiration, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html Getting credential reports>
-- in the /IAM User Guide/.
_CredentialReportExpiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CredentialReportExpiredException =
  Core._MatchServiceError
    defaultService
    "ReportExpired"
    Core.. Core.hasStatus 410
