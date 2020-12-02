{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types
  ( -- * Service Configuration
    iam,

    -- * Errors

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
    AccessDetail,
    accessDetail,
    adEntityPath,
    adRegion,
    adLastAuthenticatedTime,
    adTotalAuthenticatedEntities,
    adServiceName,
    adServiceNamespace,

    -- * AccessKeyInfo
    AccessKeyInfo,
    accessKeyInfo,
    akiCreateDate,
    akiUserName,
    akiAccessKeyId,
    akiStatus,
    akiSecretAccessKey,

    -- * AccessKeyLastUsed
    AccessKeyLastUsed,
    accessKeyLastUsed,
    akluLastUsedDate,
    akluServiceName,
    akluRegion,

    -- * AccessKeyMetadata
    AccessKeyMetadata,
    accessKeyMetadata,
    akmStatus,
    akmCreateDate,
    akmUserName,
    akmAccessKeyId,

    -- * AttachedPermissionsBoundary
    AttachedPermissionsBoundary,
    attachedPermissionsBoundary,
    apbPermissionsBoundaryType,
    apbPermissionsBoundaryARN,

    -- * AttachedPolicy
    AttachedPolicy,
    attachedPolicy,
    apPolicyName,
    apPolicyARN,

    -- * ContextEntry
    ContextEntry,
    contextEntry,
    ceContextKeyValues,
    ceContextKeyName,
    ceContextKeyType,

    -- * DeletionTaskFailureReasonType
    DeletionTaskFailureReasonType,
    deletionTaskFailureReasonType,
    dtfrtRoleUsageList,
    dtfrtReason,

    -- * EntityDetails
    EntityDetails,
    entityDetails,
    edLastAuthenticated,
    edEntityInfo,

    -- * EntityInfo
    EntityInfo,
    entityInfo,
    eiPath,
    eiARN,
    eiName,
    eiType,
    eiId,

    -- * ErrorDetails
    ErrorDetails,
    errorDetails,
    edMessage,
    edCode,

    -- * EvaluationResult
    EvaluationResult,
    evaluationResult,
    erMatchedStatements,
    erEvalDecisionDetails,
    erResourceSpecificResults,
    erEvalResourceName,
    erMissingContextValues,
    erPermissionsBoundaryDecisionDetail,
    erOrganizationsDecisionDetail,
    erEvalActionName,
    erEvalDecision,

    -- * GetContextKeysForPolicyResponse
    GetContextKeysForPolicyResponse,
    getContextKeysForPolicyResponse,
    gckfpContextKeyNames,

    -- * Group
    Group,
    group',
    gPath,
    gGroupName,
    gGroupId,
    gARN,
    gCreateDate,

    -- * GroupDetail
    GroupDetail,
    groupDetail,
    gdARN,
    gdPath,
    gdCreateDate,
    gdGroupId,
    gdGroupPolicyList,
    gdGroupName,
    gdAttachedManagedPolicies,

    -- * InstanceProfile
    InstanceProfile,
    instanceProfile,
    ipPath,
    ipInstanceProfileName,
    ipInstanceProfileId,
    ipARN,
    ipCreateDate,
    ipRoles,

    -- * ListPoliciesGrantingServiceAccessEntry
    ListPoliciesGrantingServiceAccessEntry,
    listPoliciesGrantingServiceAccessEntry,
    lpgsaeServiceNamespace,
    lpgsaePolicies,

    -- * LoginProfile
    LoginProfile,
    loginProfile,
    lpPasswordResetRequired,
    lpUserName,
    lpCreateDate,

    -- * MFADevice
    MFADevice,
    mfaDevice,
    mdUserName,
    mdSerialNumber,
    mdEnableDate,

    -- * ManagedPolicyDetail
    ManagedPolicyDetail,
    managedPolicyDetail,
    mpdPolicyName,
    mpdARN,
    mpdUpdateDate,
    mpdPolicyId,
    mpdPath,
    mpdPolicyVersionList,
    mpdCreateDate,
    mpdIsAttachable,
    mpdPermissionsBoundaryUsageCount,
    mpdDefaultVersionId,
    mpdAttachmentCount,
    mpdDescription,

    -- * OpenIdConnectProviderListEntry
    OpenIdConnectProviderListEntry,
    openIdConnectProviderListEntry,
    oicpleARN,

    -- * OrganizationsDecisionDetail
    OrganizationsDecisionDetail,
    organizationsDecisionDetail,
    oddAllowedByOrganizations,

    -- * PasswordPolicy
    PasswordPolicy,
    passwordPolicy,
    ppExpirePasswords,
    ppMinimumPasswordLength,
    ppRequireNumbers,
    ppPasswordReusePrevention,
    ppRequireLowercaseCharacters,
    ppMaxPasswordAge,
    ppHardExpiry,
    ppRequireSymbols,
    ppRequireUppercaseCharacters,
    ppAllowUsersToChangePassword,

    -- * PermissionsBoundaryDecisionDetail
    PermissionsBoundaryDecisionDetail,
    permissionsBoundaryDecisionDetail,
    pbddAllowedByPermissionsBoundary,

    -- * Policy
    Policy,
    policy,
    pPolicyName,
    pARN,
    pUpdateDate,
    pPolicyId,
    pPath,
    pCreateDate,
    pIsAttachable,
    pPermissionsBoundaryUsageCount,
    pDefaultVersionId,
    pAttachmentCount,
    pDescription,

    -- * PolicyDetail
    PolicyDetail,
    policyDetail,
    pdPolicyDocument,
    pdPolicyName,

    -- * PolicyGrantingServiceAccess
    PolicyGrantingServiceAccess,
    policyGrantingServiceAccess,
    pgsaEntityName,
    pgsaEntityType,
    pgsaPolicyARN,
    pgsaPolicyName,
    pgsaPolicyType,

    -- * PolicyGroup
    PolicyGroup,
    policyGroup,
    pgGroupId,
    pgGroupName,

    -- * PolicyRole
    PolicyRole,
    policyRole,
    prRoleName,
    prRoleId,

    -- * PolicyUser
    PolicyUser,
    policyUser,
    puUserName,
    puUserId,

    -- * PolicyVersion
    PolicyVersion,
    policyVersion,
    pvVersionId,
    pvCreateDate,
    pvDocument,
    pvIsDefaultVersion,

    -- * Position
    Position,
    position,
    pLine,
    pColumn,

    -- * ResourceSpecificResult
    ResourceSpecificResult,
    resourceSpecificResult,
    rsrMatchedStatements,
    rsrEvalDecisionDetails,
    rsrMissingContextValues,
    rsrPermissionsBoundaryDecisionDetail,
    rsrEvalResourceName,
    rsrEvalResourceDecision,

    -- * Role
    Role,
    role',
    rMaxSessionDuration,
    rAssumeRolePolicyDocument,
    rRoleLastUsed,
    rPermissionsBoundary,
    rDescription,
    rTags,
    rPath,
    rRoleName,
    rRoleId,
    rARN,
    rCreateDate,

    -- * RoleDetail
    RoleDetail,
    roleDetail,
    rdAssumeRolePolicyDocument,
    rdARN,
    rdPath,
    rdInstanceProfileList,
    rdCreateDate,
    rdRoleName,
    rdRoleId,
    rdRoleLastUsed,
    rdPermissionsBoundary,
    rdRolePolicyList,
    rdTags,
    rdAttachedManagedPolicies,

    -- * RoleLastUsed
    RoleLastUsed,
    roleLastUsed,
    rluLastUsedDate,
    rluRegion,

    -- * RoleUsageType
    RoleUsageType,
    roleUsageType,
    rutResources,
    rutRegion,

    -- * SAMLProviderListEntry
    SAMLProviderListEntry,
    sAMLProviderListEntry,
    samlpleARN,
    samlpleCreateDate,
    samlpleValidUntil,

    -- * SSHPublicKey
    SSHPublicKey,
    sshPublicKey,
    spkUploadDate,
    spkUserName,
    spkSSHPublicKeyId,
    spkFingerprint,
    spkSSHPublicKeyBody,
    spkStatus,

    -- * SSHPublicKeyMetadata
    SSHPublicKeyMetadata,
    sshPublicKeyMetadata,
    spkmUserName,
    spkmSSHPublicKeyId,
    spkmStatus,
    spkmUploadDate,

    -- * ServerCertificate
    ServerCertificate,
    serverCertificate,
    sCertificateChain,
    sServerCertificateMetadata,
    sCertificateBody,

    -- * ServerCertificateMetadata
    ServerCertificateMetadata,
    serverCertificateMetadata,
    scmUploadDate,
    scmExpiration,
    scmPath,
    scmServerCertificateName,
    scmServerCertificateId,
    scmARN,

    -- * ServiceLastAccessed
    ServiceLastAccessed,
    serviceLastAccessed,
    slaLastAuthenticated,
    slaTrackedActionsLastAccessed,
    slaLastAuthenticatedEntity,
    slaLastAuthenticatedRegion,
    slaTotalAuthenticatedEntities,
    slaServiceName,
    slaServiceNamespace,

    -- * ServiceSpecificCredential
    ServiceSpecificCredential,
    serviceSpecificCredential,
    sscCreateDate,
    sscServiceName,
    sscServiceUserName,
    sscServicePassword,
    sscServiceSpecificCredentialId,
    sscUserName,
    sscStatus,

    -- * ServiceSpecificCredentialMetadata
    ServiceSpecificCredentialMetadata,
    serviceSpecificCredentialMetadata,
    sscmUserName,
    sscmStatus,
    sscmServiceUserName,
    sscmCreateDate,
    sscmServiceSpecificCredentialId,
    sscmServiceName,

    -- * SigningCertificate
    SigningCertificate,
    signingCertificate,
    scUploadDate,
    scUserName,
    scCertificateId,
    scCertificateBody,
    scStatus,

    -- * SimulatePolicyResponse
    SimulatePolicyResponse,
    simulatePolicyResponse,
    spEvaluationResults,
    spMarker,
    spIsTruncated,

    -- * Statement
    Statement,
    statement,
    sSourcePolicyType,
    sSourcePolicyId,
    sEndPosition,
    sStartPosition,

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- * TrackedActionLastAccessed
    TrackedActionLastAccessed,
    trackedActionLastAccessed,
    talaLastAccessedTime,
    talaActionName,
    talaLastAccessedEntity,
    talaLastAccessedRegion,

    -- * User
    User,
    user,
    uPasswordLastUsed,
    uPermissionsBoundary,
    uTags,
    uPath,
    uUserName,
    uUserId,
    uARN,
    uCreateDate,

    -- * UserDetail
    UserDetail,
    userDetail,
    udGroupList,
    udARN,
    udPath,
    udCreateDate,
    udUserName,
    udUserId,
    udPermissionsBoundary,
    udUserPolicyList,
    udTags,
    udAttachedManagedPolicies,

    -- * VirtualMFADevice
    VirtualMFADevice,
    virtualMFADevice,
    vmdQRCodePNG,
    vmdBase32StringSeed,
    vmdUser,
    vmdEnableDate,
    vmdSerialNumber,
  )
where

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
import Network.AWS.IAM.Types.OpenIdConnectProviderListEntry
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2010-05-08@ of the Amazon Identity and Access Management SDK configuration.
iam :: Service
iam =
  Service
    { _svcAbbrev = "IAM",
      _svcSigner = v4,
      _svcPrefix = "iam",
      _svcVersion = "2010-05-08",
      _svcEndpoint = defaultEndpoint iam,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseXMLError "IAM",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
