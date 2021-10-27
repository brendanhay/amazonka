{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AccessAnalyzer.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AccessAnalyzer.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _ConflictException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * AccessPreviewStatus
    AccessPreviewStatus (..),

    -- * AccessPreviewStatusReasonCode
    AccessPreviewStatusReasonCode (..),

    -- * AclPermission
    AclPermission (..),

    -- * AnalyzerStatus
    AnalyzerStatus (..),

    -- * FindingChangeType
    FindingChangeType (..),

    -- * FindingSourceType
    FindingSourceType (..),

    -- * FindingStatus
    FindingStatus (..),

    -- * FindingStatusUpdate
    FindingStatusUpdate (..),

    -- * JobErrorCode
    JobErrorCode (..),

    -- * JobStatus
    JobStatus (..),

    -- * KmsGrantOperation
    KmsGrantOperation (..),

    -- * Locale
    Locale (..),

    -- * OrderBy
    OrderBy (..),

    -- * PolicyType
    PolicyType (..),

    -- * ReasonCode
    ReasonCode (..),

    -- * ResourceType
    ResourceType (..),

    -- * Type
    Type (..),

    -- * ValidatePolicyFindingType
    ValidatePolicyFindingType (..),

    -- * AccessPreview
    AccessPreview (..),
    newAccessPreview,
    accessPreview_statusReason,
    accessPreview_analyzerArn,
    accessPreview_configurations,
    accessPreview_createdAt,
    accessPreview_id,
    accessPreview_status,

    -- * AccessPreviewFinding
    AccessPreviewFinding (..),
    newAccessPreviewFinding,
    accessPreviewFinding_existingFindingStatus,
    accessPreviewFinding_error,
    accessPreviewFinding_isPublic,
    accessPreviewFinding_action,
    accessPreviewFinding_sources,
    accessPreviewFinding_resource,
    accessPreviewFinding_principal,
    accessPreviewFinding_existingFindingId,
    accessPreviewFinding_condition,
    accessPreviewFinding_changeType,
    accessPreviewFinding_createdAt,
    accessPreviewFinding_id,
    accessPreviewFinding_resourceOwnerAccount,
    accessPreviewFinding_resourceType,
    accessPreviewFinding_status,

    -- * AccessPreviewStatusReason
    AccessPreviewStatusReason (..),
    newAccessPreviewStatusReason,
    accessPreviewStatusReason_code,

    -- * AccessPreviewSummary
    AccessPreviewSummary (..),
    newAccessPreviewSummary,
    accessPreviewSummary_statusReason,
    accessPreviewSummary_analyzerArn,
    accessPreviewSummary_createdAt,
    accessPreviewSummary_id,
    accessPreviewSummary_status,

    -- * AclGrantee
    AclGrantee (..),
    newAclGrantee,
    aclGrantee_uri,
    aclGrantee_id,

    -- * AnalyzedResource
    AnalyzedResource (..),
    newAnalyzedResource,
    analyzedResource_status,
    analyzedResource_actions,
    analyzedResource_error,
    analyzedResource_sharedVia,
    analyzedResource_analyzedAt,
    analyzedResource_createdAt,
    analyzedResource_isPublic,
    analyzedResource_resourceArn,
    analyzedResource_resourceOwnerAccount,
    analyzedResource_resourceType,
    analyzedResource_updatedAt,

    -- * AnalyzedResourceSummary
    AnalyzedResourceSummary (..),
    newAnalyzedResourceSummary,
    analyzedResourceSummary_resourceArn,
    analyzedResourceSummary_resourceOwnerAccount,
    analyzedResourceSummary_resourceType,

    -- * AnalyzerSummary
    AnalyzerSummary (..),
    newAnalyzerSummary,
    analyzerSummary_lastResourceAnalyzedAt,
    analyzerSummary_lastResourceAnalyzed,
    analyzerSummary_statusReason,
    analyzerSummary_tags,
    analyzerSummary_arn,
    analyzerSummary_createdAt,
    analyzerSummary_name,
    analyzerSummary_status,
    analyzerSummary_type,

    -- * ArchiveRuleSummary
    ArchiveRuleSummary (..),
    newArchiveRuleSummary,
    archiveRuleSummary_createdAt,
    archiveRuleSummary_filter,
    archiveRuleSummary_ruleName,
    archiveRuleSummary_updatedAt,

    -- * CloudTrailDetails
    CloudTrailDetails (..),
    newCloudTrailDetails,
    cloudTrailDetails_endTime,
    cloudTrailDetails_accessRole,
    cloudTrailDetails_startTime,
    cloudTrailDetails_trails,

    -- * CloudTrailProperties
    CloudTrailProperties (..),
    newCloudTrailProperties,
    cloudTrailProperties_endTime,
    cloudTrailProperties_startTime,
    cloudTrailProperties_trailProperties,

    -- * Configuration
    Configuration (..),
    newConfiguration,
    configuration_kmsKey,
    configuration_secretsManagerSecret,
    configuration_sqsQueue,
    configuration_s3Bucket,
    configuration_iamRole,

    -- * Criterion
    Criterion (..),
    newCriterion,
    criterion_eq,
    criterion_exists,
    criterion_neq,
    criterion_contains,

    -- * Finding
    Finding (..),
    newFinding,
    finding_error,
    finding_isPublic,
    finding_action,
    finding_sources,
    finding_resource,
    finding_principal,
    finding_analyzedAt,
    finding_condition,
    finding_createdAt,
    finding_id,
    finding_resourceOwnerAccount,
    finding_resourceType,
    finding_status,
    finding_updatedAt,

    -- * FindingSource
    FindingSource (..),
    newFindingSource,
    findingSource_detail,
    findingSource_type,

    -- * FindingSourceDetail
    FindingSourceDetail (..),
    newFindingSourceDetail,
    findingSourceDetail_accessPointArn,

    -- * FindingSummary
    FindingSummary (..),
    newFindingSummary,
    findingSummary_error,
    findingSummary_isPublic,
    findingSummary_action,
    findingSummary_sources,
    findingSummary_resource,
    findingSummary_principal,
    findingSummary_analyzedAt,
    findingSummary_condition,
    findingSummary_createdAt,
    findingSummary_id,
    findingSummary_resourceOwnerAccount,
    findingSummary_resourceType,
    findingSummary_status,
    findingSummary_updatedAt,

    -- * GeneratedPolicy
    GeneratedPolicy (..),
    newGeneratedPolicy,
    generatedPolicy_policy,

    -- * GeneratedPolicyProperties
    GeneratedPolicyProperties (..),
    newGeneratedPolicyProperties,
    generatedPolicyProperties_isComplete,
    generatedPolicyProperties_cloudTrailProperties,
    generatedPolicyProperties_principalArn,

    -- * GeneratedPolicyResult
    GeneratedPolicyResult (..),
    newGeneratedPolicyResult,
    generatedPolicyResult_generatedPolicies,
    generatedPolicyResult_properties,

    -- * IamRoleConfiguration
    IamRoleConfiguration (..),
    newIamRoleConfiguration,
    iamRoleConfiguration_trustPolicy,

    -- * InlineArchiveRule
    InlineArchiveRule (..),
    newInlineArchiveRule,
    inlineArchiveRule_filter,
    inlineArchiveRule_ruleName,

    -- * InternetConfiguration
    InternetConfiguration (..),
    newInternetConfiguration,

    -- * JobDetails
    JobDetails (..),
    newJobDetails,
    jobDetails_completedOn,
    jobDetails_jobError,
    jobDetails_jobId,
    jobDetails_startedOn,
    jobDetails_status,

    -- * JobError
    JobError (..),
    newJobError,
    jobError_code,
    jobError_message,

    -- * KmsGrantConfiguration
    KmsGrantConfiguration (..),
    newKmsGrantConfiguration,
    kmsGrantConfiguration_retiringPrincipal,
    kmsGrantConfiguration_constraints,
    kmsGrantConfiguration_granteePrincipal,
    kmsGrantConfiguration_issuingAccount,
    kmsGrantConfiguration_operations,

    -- * KmsGrantConstraints
    KmsGrantConstraints (..),
    newKmsGrantConstraints,
    kmsGrantConstraints_encryptionContextEquals,
    kmsGrantConstraints_encryptionContextSubset,

    -- * KmsKeyConfiguration
    KmsKeyConfiguration (..),
    newKmsKeyConfiguration,
    kmsKeyConfiguration_grants,
    kmsKeyConfiguration_keyPolicies,

    -- * Location
    Location (..),
    newLocation,
    location_path,
    location_span,

    -- * NetworkOriginConfiguration
    NetworkOriginConfiguration (..),
    newNetworkOriginConfiguration,
    networkOriginConfiguration_internetConfiguration,
    networkOriginConfiguration_vpcConfiguration,

    -- * PathElement
    PathElement (..),
    newPathElement,
    pathElement_value,
    pathElement_substring,
    pathElement_key,
    pathElement_index,

    -- * PolicyGeneration
    PolicyGeneration (..),
    newPolicyGeneration,
    policyGeneration_completedOn,
    policyGeneration_jobId,
    policyGeneration_principalArn,
    policyGeneration_startedOn,
    policyGeneration_status,

    -- * PolicyGenerationDetails
    PolicyGenerationDetails (..),
    newPolicyGenerationDetails,
    policyGenerationDetails_principalArn,

    -- * Position
    Position (..),
    newPosition,
    position_column,
    position_line,
    position_offset,

    -- * S3AccessPointConfiguration
    S3AccessPointConfiguration (..),
    newS3AccessPointConfiguration,
    s3AccessPointConfiguration_publicAccessBlock,
    s3AccessPointConfiguration_accessPointPolicy,
    s3AccessPointConfiguration_networkOrigin,

    -- * S3BucketAclGrantConfiguration
    S3BucketAclGrantConfiguration (..),
    newS3BucketAclGrantConfiguration,
    s3BucketAclGrantConfiguration_grantee,
    s3BucketAclGrantConfiguration_permission,

    -- * S3BucketConfiguration
    S3BucketConfiguration (..),
    newS3BucketConfiguration,
    s3BucketConfiguration_accessPoints,
    s3BucketConfiguration_bucketPublicAccessBlock,
    s3BucketConfiguration_bucketAclGrants,
    s3BucketConfiguration_bucketPolicy,

    -- * S3PublicAccessBlockConfiguration
    S3PublicAccessBlockConfiguration (..),
    newS3PublicAccessBlockConfiguration,
    s3PublicAccessBlockConfiguration_ignorePublicAcls,
    s3PublicAccessBlockConfiguration_restrictPublicBuckets,

    -- * SecretsManagerSecretConfiguration
    SecretsManagerSecretConfiguration (..),
    newSecretsManagerSecretConfiguration,
    secretsManagerSecretConfiguration_kmsKeyId,
    secretsManagerSecretConfiguration_secretPolicy,

    -- * SortCriteria
    SortCriteria (..),
    newSortCriteria,
    sortCriteria_orderBy,
    sortCriteria_attributeName,

    -- * Span
    Span (..),
    newSpan,
    span_end,
    span_start,

    -- * SqsQueueConfiguration
    SqsQueueConfiguration (..),
    newSqsQueueConfiguration,
    sqsQueueConfiguration_queuePolicy,

    -- * StatusReason
    StatusReason (..),
    newStatusReason,
    statusReason_code,

    -- * Substring
    Substring (..),
    newSubstring,
    substring_length,
    substring_start,

    -- * Trail
    Trail (..),
    newTrail,
    trail_regions,
    trail_allRegions,
    trail_cloudTrailArn,

    -- * TrailProperties
    TrailProperties (..),
    newTrailProperties,
    trailProperties_regions,
    trailProperties_allRegions,
    trailProperties_cloudTrailArn,

    -- * ValidatePolicyFinding
    ValidatePolicyFinding (..),
    newValidatePolicyFinding,
    validatePolicyFinding_findingDetails,
    validatePolicyFinding_findingType,
    validatePolicyFinding_issueCode,
    validatePolicyFinding_learnMoreLink,
    validatePolicyFinding_locations,

    -- * VpcConfiguration
    VpcConfiguration (..),
    newVpcConfiguration,
    vpcConfiguration_vpcId,
  )
where

import Network.AWS.AccessAnalyzer.Types.AccessPreview
import Network.AWS.AccessAnalyzer.Types.AccessPreviewFinding
import Network.AWS.AccessAnalyzer.Types.AccessPreviewStatus
import Network.AWS.AccessAnalyzer.Types.AccessPreviewStatusReason
import Network.AWS.AccessAnalyzer.Types.AccessPreviewStatusReasonCode
import Network.AWS.AccessAnalyzer.Types.AccessPreviewSummary
import Network.AWS.AccessAnalyzer.Types.AclGrantee
import Network.AWS.AccessAnalyzer.Types.AclPermission
import Network.AWS.AccessAnalyzer.Types.AnalyzedResource
import Network.AWS.AccessAnalyzer.Types.AnalyzedResourceSummary
import Network.AWS.AccessAnalyzer.Types.AnalyzerStatus
import Network.AWS.AccessAnalyzer.Types.AnalyzerSummary
import Network.AWS.AccessAnalyzer.Types.ArchiveRuleSummary
import Network.AWS.AccessAnalyzer.Types.CloudTrailDetails
import Network.AWS.AccessAnalyzer.Types.CloudTrailProperties
import Network.AWS.AccessAnalyzer.Types.Configuration
import Network.AWS.AccessAnalyzer.Types.Criterion
import Network.AWS.AccessAnalyzer.Types.Finding
import Network.AWS.AccessAnalyzer.Types.FindingChangeType
import Network.AWS.AccessAnalyzer.Types.FindingSource
import Network.AWS.AccessAnalyzer.Types.FindingSourceDetail
import Network.AWS.AccessAnalyzer.Types.FindingSourceType
import Network.AWS.AccessAnalyzer.Types.FindingStatus
import Network.AWS.AccessAnalyzer.Types.FindingStatusUpdate
import Network.AWS.AccessAnalyzer.Types.FindingSummary
import Network.AWS.AccessAnalyzer.Types.GeneratedPolicy
import Network.AWS.AccessAnalyzer.Types.GeneratedPolicyProperties
import Network.AWS.AccessAnalyzer.Types.GeneratedPolicyResult
import Network.AWS.AccessAnalyzer.Types.IamRoleConfiguration
import Network.AWS.AccessAnalyzer.Types.InlineArchiveRule
import Network.AWS.AccessAnalyzer.Types.InternetConfiguration
import Network.AWS.AccessAnalyzer.Types.JobDetails
import Network.AWS.AccessAnalyzer.Types.JobError
import Network.AWS.AccessAnalyzer.Types.JobErrorCode
import Network.AWS.AccessAnalyzer.Types.JobStatus
import Network.AWS.AccessAnalyzer.Types.KmsGrantConfiguration
import Network.AWS.AccessAnalyzer.Types.KmsGrantConstraints
import Network.AWS.AccessAnalyzer.Types.KmsGrantOperation
import Network.AWS.AccessAnalyzer.Types.KmsKeyConfiguration
import Network.AWS.AccessAnalyzer.Types.Locale
import Network.AWS.AccessAnalyzer.Types.Location
import Network.AWS.AccessAnalyzer.Types.NetworkOriginConfiguration
import Network.AWS.AccessAnalyzer.Types.OrderBy
import Network.AWS.AccessAnalyzer.Types.PathElement
import Network.AWS.AccessAnalyzer.Types.PolicyGeneration
import Network.AWS.AccessAnalyzer.Types.PolicyGenerationDetails
import Network.AWS.AccessAnalyzer.Types.PolicyType
import Network.AWS.AccessAnalyzer.Types.Position
import Network.AWS.AccessAnalyzer.Types.ReasonCode
import Network.AWS.AccessAnalyzer.Types.ResourceType
import Network.AWS.AccessAnalyzer.Types.S3AccessPointConfiguration
import Network.AWS.AccessAnalyzer.Types.S3BucketAclGrantConfiguration
import Network.AWS.AccessAnalyzer.Types.S3BucketConfiguration
import Network.AWS.AccessAnalyzer.Types.S3PublicAccessBlockConfiguration
import Network.AWS.AccessAnalyzer.Types.SecretsManagerSecretConfiguration
import Network.AWS.AccessAnalyzer.Types.SortCriteria
import Network.AWS.AccessAnalyzer.Types.Span
import Network.AWS.AccessAnalyzer.Types.SqsQueueConfiguration
import Network.AWS.AccessAnalyzer.Types.StatusReason
import Network.AWS.AccessAnalyzer.Types.Substring
import Network.AWS.AccessAnalyzer.Types.Trail
import Network.AWS.AccessAnalyzer.Types.TrailProperties
import Network.AWS.AccessAnalyzer.Types.Type
import Network.AWS.AccessAnalyzer.Types.ValidatePolicyFinding
import Network.AWS.AccessAnalyzer.Types.ValidatePolicyFindingType
import Network.AWS.AccessAnalyzer.Types.VpcConfiguration
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2019-11-01@ of the Amazon Access Analyzer SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "AccessAnalyzer",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "access-analyzer",
      Core._serviceSigningName = "access-analyzer",
      Core._serviceVersion = "2019-11-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "AccessAnalyzer",
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
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Validation exception error.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | A conflict exception error.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Service quote met error.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | Throttling limit exceeded error.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | Internal server error.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The specified resource could not be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
