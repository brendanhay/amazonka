-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types
  ( -- * Service configuration
    iamService,

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
    AccessDetail (..),
    mkAccessDetail,
    adEntityPath,
    adRegion,
    adLastAuthenticatedTime,
    adTotalAuthenticatedEntities,
    adServiceName,
    adServiceNamespace,

    -- * AccessKeyInfo
    AccessKeyInfo (..),
    mkAccessKeyInfo,
    akiCreateDate,
    akiUserName,
    akiAccessKeyId,
    akiStatus,
    akiSecretAccessKey,

    -- * AccessKeyLastUsed
    AccessKeyLastUsed (..),
    mkAccessKeyLastUsed,
    akluLastUsedDate,
    akluServiceName,
    akluRegion,

    -- * AccessKeyMetadata
    AccessKeyMetadata (..),
    mkAccessKeyMetadata,
    akmStatus,
    akmCreateDate,
    akmUserName,
    akmAccessKeyId,

    -- * AttachedPermissionsBoundary
    AttachedPermissionsBoundary (..),
    mkAttachedPermissionsBoundary,
    apbPermissionsBoundaryType,
    apbPermissionsBoundaryARN,

    -- * AttachedPolicy
    AttachedPolicy (..),
    mkAttachedPolicy,
    apPolicyName,
    apPolicyARN,

    -- * ContextEntry
    ContextEntry (..),
    mkContextEntry,
    ceContextKeyValues,
    ceContextKeyName,
    ceContextKeyType,

    -- * DeletionTaskFailureReasonType
    DeletionTaskFailureReasonType (..),
    mkDeletionTaskFailureReasonType,
    dtfrtRoleUsageList,
    dtfrtReason,

    -- * EntityDetails
    EntityDetails (..),
    mkEntityDetails,
    edLastAuthenticated,
    edEntityInfo,

    -- * EntityInfo
    EntityInfo (..),
    mkEntityInfo,
    eiPath,
    eiARN,
    eiName,
    eiType,
    eiId,

    -- * ErrorDetails
    ErrorDetails (..),
    mkErrorDetails,
    edMessage,
    edCode,

    -- * EvaluationResult
    EvaluationResult (..),
    mkEvaluationResult,
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
    GetContextKeysForPolicyResponse (..),
    mkGetContextKeysForPolicyResponse,
    gckfpContextKeyNames,

    -- * Group
    Group (..),
    mkGroup,
    gPath,
    gGroupName,
    gGroupId,
    gARN,
    gCreateDate,

    -- * GroupDetail
    GroupDetail (..),
    mkGroupDetail,
    gdARN,
    gdPath,
    gdCreateDate,
    gdGroupId,
    gdGroupPolicyList,
    gdGroupName,
    gdAttachedManagedPolicies,

    -- * InstanceProfile
    InstanceProfile (..),
    mkInstanceProfile,
    ipPath,
    ipInstanceProfileName,
    ipInstanceProfileId,
    ipARN,
    ipCreateDate,
    ipRoles,

    -- * ListPoliciesGrantingServiceAccessEntry
    ListPoliciesGrantingServiceAccessEntry (..),
    mkListPoliciesGrantingServiceAccessEntry,
    lpgsaeServiceNamespace,
    lpgsaePolicies,

    -- * LoginProfile
    LoginProfile (..),
    mkLoginProfile,
    lpPasswordResetRequired,
    lpUserName,
    lpCreateDate,

    -- * MFADevice
    MFADevice (..),
    mkMFADevice,
    mdUserName,
    mdSerialNumber,
    mdEnableDate,

    -- * ManagedPolicyDetail
    ManagedPolicyDetail (..),
    mkManagedPolicyDetail,
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
    OpenIdConnectProviderListEntry (..),
    mkOpenIdConnectProviderListEntry,
    oicpleARN,

    -- * OrganizationsDecisionDetail
    OrganizationsDecisionDetail (..),
    mkOrganizationsDecisionDetail,
    oddAllowedByOrganizations,

    -- * PasswordPolicy
    PasswordPolicy (..),
    mkPasswordPolicy,
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
    PermissionsBoundaryDecisionDetail (..),
    mkPermissionsBoundaryDecisionDetail,
    pbddAllowedByPermissionsBoundary,

    -- * Policy
    Policy (..),
    mkPolicy,
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
    PolicyDetail (..),
    mkPolicyDetail,
    pdPolicyDocument,
    pdPolicyName,

    -- * PolicyGrantingServiceAccess
    PolicyGrantingServiceAccess (..),
    mkPolicyGrantingServiceAccess,
    pgsaEntityName,
    pgsaEntityType,
    pgsaPolicyARN,
    pgsaPolicyName,
    pgsaPolicyType,

    -- * PolicyGroup
    PolicyGroup (..),
    mkPolicyGroup,
    pgGroupId,
    pgGroupName,

    -- * PolicyRole
    PolicyRole (..),
    mkPolicyRole,
    prRoleName,
    prRoleId,

    -- * PolicyUser
    PolicyUser (..),
    mkPolicyUser,
    puUserName,
    puUserId,

    -- * PolicyVersion
    PolicyVersion (..),
    mkPolicyVersion,
    pvVersionId,
    pvCreateDate,
    pvDocument,
    pvIsDefaultVersion,

    -- * Position
    Position (..),
    mkPosition,
    pLine,
    pColumn,

    -- * ResourceSpecificResult
    ResourceSpecificResult (..),
    mkResourceSpecificResult,
    rsrMatchedStatements,
    rsrEvalDecisionDetails,
    rsrMissingContextValues,
    rsrPermissionsBoundaryDecisionDetail,
    rsrEvalResourceName,
    rsrEvalResourceDecision,

    -- * Role
    Role (..),
    mkRole,
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
    RoleDetail (..),
    mkRoleDetail,
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
    RoleLastUsed (..),
    mkRoleLastUsed,
    rluLastUsedDate,
    rluRegion,

    -- * RoleUsageType
    RoleUsageType (..),
    mkRoleUsageType,
    rutResources,
    rutRegion,

    -- * SAMLProviderListEntry
    SAMLProviderListEntry (..),
    mkSAMLProviderListEntry,
    samlpleARN,
    samlpleCreateDate,
    samlpleValidUntil,

    -- * SSHPublicKey
    SSHPublicKey (..),
    mkSSHPublicKey,
    spkUploadDate,
    spkUserName,
    spkSSHPublicKeyId,
    spkFingerprint,
    spkSSHPublicKeyBody,
    spkStatus,

    -- * SSHPublicKeyMetadata
    SSHPublicKeyMetadata (..),
    mkSSHPublicKeyMetadata,
    spkmUserName,
    spkmSSHPublicKeyId,
    spkmStatus,
    spkmUploadDate,

    -- * ServerCertificate
    ServerCertificate (..),
    mkServerCertificate,
    sCertificateChain,
    sServerCertificateMetadata,
    sCertificateBody,

    -- * ServerCertificateMetadata
    ServerCertificateMetadata (..),
    mkServerCertificateMetadata,
    scmUploadDate,
    scmExpiration,
    scmPath,
    scmServerCertificateName,
    scmServerCertificateId,
    scmARN,

    -- * ServiceLastAccessed
    ServiceLastAccessed (..),
    mkServiceLastAccessed,
    slaLastAuthenticated,
    slaTrackedActionsLastAccessed,
    slaLastAuthenticatedEntity,
    slaLastAuthenticatedRegion,
    slaTotalAuthenticatedEntities,
    slaServiceName,
    slaServiceNamespace,

    -- * ServiceSpecificCredential
    ServiceSpecificCredential (..),
    mkServiceSpecificCredential,
    sscCreateDate,
    sscServiceName,
    sscServiceUserName,
    sscServicePassword,
    sscServiceSpecificCredentialId,
    sscUserName,
    sscStatus,

    -- * ServiceSpecificCredentialMetadata
    ServiceSpecificCredentialMetadata (..),
    mkServiceSpecificCredentialMetadata,
    sscmUserName,
    sscmStatus,
    sscmServiceUserName,
    sscmCreateDate,
    sscmServiceSpecificCredentialId,
    sscmServiceName,

    -- * SigningCertificate
    SigningCertificate (..),
    mkSigningCertificate,
    scUploadDate,
    scUserName,
    scCertificateId,
    scCertificateBody,
    scStatus,

    -- * SimulatePolicyResponse
    SimulatePolicyResponse (..),
    mkSimulatePolicyResponse,
    spEvaluationResults,
    spMarker,
    spIsTruncated,

    -- * Statement
    Statement (..),
    mkStatement,
    sSourcePolicyType,
    sSourcePolicyId,
    sEndPosition,
    sStartPosition,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * TrackedActionLastAccessed
    TrackedActionLastAccessed (..),
    mkTrackedActionLastAccessed,
    talaLastAccessedTime,
    talaActionName,
    talaLastAccessedEntity,
    talaLastAccessedRegion,

    -- * User
    User (..),
    mkUser,
    uPasswordLastUsed,
    uPermissionsBoundary,
    uTags,
    uPath,
    uUserName,
    uUserId,
    uARN,
    uCreateDate,

    -- * UserDetail
    UserDetail (..),
    mkUserDetail,
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
    VirtualMFADevice (..),
    mkVirtualMFADevice,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2010-05-08@ of the Amazon Identity and Access Management SDK configuration.
iamService :: Lude.Service
iamService =
  Lude.Service
    { Lude._svcAbbrev = "IAM",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "iam",
      Lude._svcVersion = "2010-05-08",
      Lude._svcEndpoint = Lude.defaultEndpoint iamService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseXMLError "IAM",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
