{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AccessAnalyzer.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

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

    -- * ValidatePolicyResourceType
    ValidatePolicyResourceType (..),

    -- * AccessPreview
    AccessPreview (..),
    newAccessPreview,
    accessPreview_statusReason,
    accessPreview_id,
    accessPreview_analyzerArn,
    accessPreview_configurations,
    accessPreview_createdAt,
    accessPreview_status,

    -- * AccessPreviewFinding
    AccessPreviewFinding (..),
    newAccessPreviewFinding,
    accessPreviewFinding_action,
    accessPreviewFinding_condition,
    accessPreviewFinding_error,
    accessPreviewFinding_existingFindingId,
    accessPreviewFinding_existingFindingStatus,
    accessPreviewFinding_isPublic,
    accessPreviewFinding_principal,
    accessPreviewFinding_resource,
    accessPreviewFinding_sources,
    accessPreviewFinding_id,
    accessPreviewFinding_resourceType,
    accessPreviewFinding_createdAt,
    accessPreviewFinding_changeType,
    accessPreviewFinding_status,
    accessPreviewFinding_resourceOwnerAccount,

    -- * AccessPreviewStatusReason
    AccessPreviewStatusReason (..),
    newAccessPreviewStatusReason,
    accessPreviewStatusReason_code,

    -- * AccessPreviewSummary
    AccessPreviewSummary (..),
    newAccessPreviewSummary,
    accessPreviewSummary_statusReason,
    accessPreviewSummary_id,
    accessPreviewSummary_analyzerArn,
    accessPreviewSummary_createdAt,
    accessPreviewSummary_status,

    -- * AclGrantee
    AclGrantee (..),
    newAclGrantee,
    aclGrantee_id,
    aclGrantee_uri,

    -- * AnalyzedResource
    AnalyzedResource (..),
    newAnalyzedResource,
    analyzedResource_actions,
    analyzedResource_error,
    analyzedResource_sharedVia,
    analyzedResource_status,
    analyzedResource_resourceArn,
    analyzedResource_resourceType,
    analyzedResource_createdAt,
    analyzedResource_analyzedAt,
    analyzedResource_updatedAt,
    analyzedResource_isPublic,
    analyzedResource_resourceOwnerAccount,

    -- * AnalyzedResourceSummary
    AnalyzedResourceSummary (..),
    newAnalyzedResourceSummary,
    analyzedResourceSummary_resourceArn,
    analyzedResourceSummary_resourceOwnerAccount,
    analyzedResourceSummary_resourceType,

    -- * AnalyzerSummary
    AnalyzerSummary (..),
    newAnalyzerSummary,
    analyzerSummary_lastResourceAnalyzed,
    analyzerSummary_lastResourceAnalyzedAt,
    analyzerSummary_statusReason,
    analyzerSummary_tags,
    analyzerSummary_arn,
    analyzerSummary_name,
    analyzerSummary_type,
    analyzerSummary_createdAt,
    analyzerSummary_status,

    -- * ArchiveRuleSummary
    ArchiveRuleSummary (..),
    newArchiveRuleSummary,
    archiveRuleSummary_ruleName,
    archiveRuleSummary_filter,
    archiveRuleSummary_createdAt,
    archiveRuleSummary_updatedAt,

    -- * CloudTrailDetails
    CloudTrailDetails (..),
    newCloudTrailDetails,
    cloudTrailDetails_endTime,
    cloudTrailDetails_trails,
    cloudTrailDetails_accessRole,
    cloudTrailDetails_startTime,

    -- * CloudTrailProperties
    CloudTrailProperties (..),
    newCloudTrailProperties,
    cloudTrailProperties_trailProperties,
    cloudTrailProperties_startTime,
    cloudTrailProperties_endTime,

    -- * Configuration
    Configuration (..),
    newConfiguration,
    configuration_ebsSnapshot,
    configuration_ecrRepository,
    configuration_efsFileSystem,
    configuration_iamRole,
    configuration_kmsKey,
    configuration_rdsDbClusterSnapshot,
    configuration_rdsDbSnapshot,
    configuration_s3Bucket,
    configuration_secretsManagerSecret,
    configuration_snsTopic,
    configuration_sqsQueue,

    -- * Criterion
    Criterion (..),
    newCriterion,
    criterion_contains,
    criterion_eq,
    criterion_exists,
    criterion_neq,

    -- * EbsSnapshotConfiguration
    EbsSnapshotConfiguration (..),
    newEbsSnapshotConfiguration,
    ebsSnapshotConfiguration_groups,
    ebsSnapshotConfiguration_kmsKeyId,
    ebsSnapshotConfiguration_userIds,

    -- * EcrRepositoryConfiguration
    EcrRepositoryConfiguration (..),
    newEcrRepositoryConfiguration,
    ecrRepositoryConfiguration_repositoryPolicy,

    -- * EfsFileSystemConfiguration
    EfsFileSystemConfiguration (..),
    newEfsFileSystemConfiguration,
    efsFileSystemConfiguration_fileSystemPolicy,

    -- * Finding
    Finding (..),
    newFinding,
    finding_action,
    finding_error,
    finding_isPublic,
    finding_principal,
    finding_resource,
    finding_sources,
    finding_id,
    finding_resourceType,
    finding_condition,
    finding_createdAt,
    finding_analyzedAt,
    finding_updatedAt,
    finding_status,
    finding_resourceOwnerAccount,

    -- * FindingSource
    FindingSource (..),
    newFindingSource,
    findingSource_detail,
    findingSource_type,

    -- * FindingSourceDetail
    FindingSourceDetail (..),
    newFindingSourceDetail,
    findingSourceDetail_accessPointAccount,
    findingSourceDetail_accessPointArn,

    -- * FindingSummary
    FindingSummary (..),
    newFindingSummary,
    findingSummary_action,
    findingSummary_error,
    findingSummary_isPublic,
    findingSummary_principal,
    findingSummary_resource,
    findingSummary_sources,
    findingSummary_id,
    findingSummary_resourceType,
    findingSummary_condition,
    findingSummary_createdAt,
    findingSummary_analyzedAt,
    findingSummary_updatedAt,
    findingSummary_status,
    findingSummary_resourceOwnerAccount,

    -- * GeneratedPolicy
    GeneratedPolicy (..),
    newGeneratedPolicy,
    generatedPolicy_policy,

    -- * GeneratedPolicyProperties
    GeneratedPolicyProperties (..),
    newGeneratedPolicyProperties,
    generatedPolicyProperties_cloudTrailProperties,
    generatedPolicyProperties_isComplete,
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
    inlineArchiveRule_ruleName,
    inlineArchiveRule_filter,

    -- * InternetConfiguration
    InternetConfiguration (..),
    newInternetConfiguration,

    -- * JobDetails
    JobDetails (..),
    newJobDetails,
    jobDetails_completedOn,
    jobDetails_jobError,
    jobDetails_jobId,
    jobDetails_status,
    jobDetails_startedOn,

    -- * JobError
    JobError (..),
    newJobError,
    jobError_code,
    jobError_message,

    -- * KmsGrantConfiguration
    KmsGrantConfiguration (..),
    newKmsGrantConfiguration,
    kmsGrantConfiguration_constraints,
    kmsGrantConfiguration_retiringPrincipal,
    kmsGrantConfiguration_operations,
    kmsGrantConfiguration_granteePrincipal,
    kmsGrantConfiguration_issuingAccount,

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
    pathElement_index,
    pathElement_key,
    pathElement_substring,
    pathElement_value,

    -- * PolicyGeneration
    PolicyGeneration (..),
    newPolicyGeneration,
    policyGeneration_completedOn,
    policyGeneration_jobId,
    policyGeneration_principalArn,
    policyGeneration_status,
    policyGeneration_startedOn,

    -- * PolicyGenerationDetails
    PolicyGenerationDetails (..),
    newPolicyGenerationDetails,
    policyGenerationDetails_principalArn,

    -- * Position
    Position (..),
    newPosition,
    position_line,
    position_column,
    position_offset,

    -- * RdsDbClusterSnapshotAttributeValue
    RdsDbClusterSnapshotAttributeValue (..),
    newRdsDbClusterSnapshotAttributeValue,
    rdsDbClusterSnapshotAttributeValue_accountIds,

    -- * RdsDbClusterSnapshotConfiguration
    RdsDbClusterSnapshotConfiguration (..),
    newRdsDbClusterSnapshotConfiguration,
    rdsDbClusterSnapshotConfiguration_attributes,
    rdsDbClusterSnapshotConfiguration_kmsKeyId,

    -- * RdsDbSnapshotAttributeValue
    RdsDbSnapshotAttributeValue (..),
    newRdsDbSnapshotAttributeValue,
    rdsDbSnapshotAttributeValue_accountIds,

    -- * RdsDbSnapshotConfiguration
    RdsDbSnapshotConfiguration (..),
    newRdsDbSnapshotConfiguration,
    rdsDbSnapshotConfiguration_attributes,
    rdsDbSnapshotConfiguration_kmsKeyId,

    -- * S3AccessPointConfiguration
    S3AccessPointConfiguration (..),
    newS3AccessPointConfiguration,
    s3AccessPointConfiguration_accessPointPolicy,
    s3AccessPointConfiguration_networkOrigin,
    s3AccessPointConfiguration_publicAccessBlock,

    -- * S3BucketAclGrantConfiguration
    S3BucketAclGrantConfiguration (..),
    newS3BucketAclGrantConfiguration,
    s3BucketAclGrantConfiguration_permission,
    s3BucketAclGrantConfiguration_grantee,

    -- * S3BucketConfiguration
    S3BucketConfiguration (..),
    newS3BucketConfiguration,
    s3BucketConfiguration_accessPoints,
    s3BucketConfiguration_bucketAclGrants,
    s3BucketConfiguration_bucketPolicy,
    s3BucketConfiguration_bucketPublicAccessBlock,

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

    -- * SnsTopicConfiguration
    SnsTopicConfiguration (..),
    newSnsTopicConfiguration,
    snsTopicConfiguration_topicPolicy,

    -- * SortCriteria
    SortCriteria (..),
    newSortCriteria,
    sortCriteria_attributeName,
    sortCriteria_orderBy,

    -- * Span
    Span (..),
    newSpan,
    span_start,
    span_end,

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
    substring_start,
    substring_length,

    -- * Trail
    Trail (..),
    newTrail,
    trail_allRegions,
    trail_regions,
    trail_cloudTrailArn,

    -- * TrailProperties
    TrailProperties (..),
    newTrailProperties,
    trailProperties_allRegions,
    trailProperties_regions,
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

import Amazonka.AccessAnalyzer.Types.AccessPreview
import Amazonka.AccessAnalyzer.Types.AccessPreviewFinding
import Amazonka.AccessAnalyzer.Types.AccessPreviewStatus
import Amazonka.AccessAnalyzer.Types.AccessPreviewStatusReason
import Amazonka.AccessAnalyzer.Types.AccessPreviewStatusReasonCode
import Amazonka.AccessAnalyzer.Types.AccessPreviewSummary
import Amazonka.AccessAnalyzer.Types.AclGrantee
import Amazonka.AccessAnalyzer.Types.AclPermission
import Amazonka.AccessAnalyzer.Types.AnalyzedResource
import Amazonka.AccessAnalyzer.Types.AnalyzedResourceSummary
import Amazonka.AccessAnalyzer.Types.AnalyzerStatus
import Amazonka.AccessAnalyzer.Types.AnalyzerSummary
import Amazonka.AccessAnalyzer.Types.ArchiveRuleSummary
import Amazonka.AccessAnalyzer.Types.CloudTrailDetails
import Amazonka.AccessAnalyzer.Types.CloudTrailProperties
import Amazonka.AccessAnalyzer.Types.Configuration
import Amazonka.AccessAnalyzer.Types.Criterion
import Amazonka.AccessAnalyzer.Types.EbsSnapshotConfiguration
import Amazonka.AccessAnalyzer.Types.EcrRepositoryConfiguration
import Amazonka.AccessAnalyzer.Types.EfsFileSystemConfiguration
import Amazonka.AccessAnalyzer.Types.Finding
import Amazonka.AccessAnalyzer.Types.FindingChangeType
import Amazonka.AccessAnalyzer.Types.FindingSource
import Amazonka.AccessAnalyzer.Types.FindingSourceDetail
import Amazonka.AccessAnalyzer.Types.FindingSourceType
import Amazonka.AccessAnalyzer.Types.FindingStatus
import Amazonka.AccessAnalyzer.Types.FindingStatusUpdate
import Amazonka.AccessAnalyzer.Types.FindingSummary
import Amazonka.AccessAnalyzer.Types.GeneratedPolicy
import Amazonka.AccessAnalyzer.Types.GeneratedPolicyProperties
import Amazonka.AccessAnalyzer.Types.GeneratedPolicyResult
import Amazonka.AccessAnalyzer.Types.IamRoleConfiguration
import Amazonka.AccessAnalyzer.Types.InlineArchiveRule
import Amazonka.AccessAnalyzer.Types.InternetConfiguration
import Amazonka.AccessAnalyzer.Types.JobDetails
import Amazonka.AccessAnalyzer.Types.JobError
import Amazonka.AccessAnalyzer.Types.JobErrorCode
import Amazonka.AccessAnalyzer.Types.JobStatus
import Amazonka.AccessAnalyzer.Types.KmsGrantConfiguration
import Amazonka.AccessAnalyzer.Types.KmsGrantConstraints
import Amazonka.AccessAnalyzer.Types.KmsGrantOperation
import Amazonka.AccessAnalyzer.Types.KmsKeyConfiguration
import Amazonka.AccessAnalyzer.Types.Locale
import Amazonka.AccessAnalyzer.Types.Location
import Amazonka.AccessAnalyzer.Types.NetworkOriginConfiguration
import Amazonka.AccessAnalyzer.Types.OrderBy
import Amazonka.AccessAnalyzer.Types.PathElement
import Amazonka.AccessAnalyzer.Types.PolicyGeneration
import Amazonka.AccessAnalyzer.Types.PolicyGenerationDetails
import Amazonka.AccessAnalyzer.Types.PolicyType
import Amazonka.AccessAnalyzer.Types.Position
import Amazonka.AccessAnalyzer.Types.RdsDbClusterSnapshotAttributeValue
import Amazonka.AccessAnalyzer.Types.RdsDbClusterSnapshotConfiguration
import Amazonka.AccessAnalyzer.Types.RdsDbSnapshotAttributeValue
import Amazonka.AccessAnalyzer.Types.RdsDbSnapshotConfiguration
import Amazonka.AccessAnalyzer.Types.ReasonCode
import Amazonka.AccessAnalyzer.Types.ResourceType
import Amazonka.AccessAnalyzer.Types.S3AccessPointConfiguration
import Amazonka.AccessAnalyzer.Types.S3BucketAclGrantConfiguration
import Amazonka.AccessAnalyzer.Types.S3BucketConfiguration
import Amazonka.AccessAnalyzer.Types.S3PublicAccessBlockConfiguration
import Amazonka.AccessAnalyzer.Types.SecretsManagerSecretConfiguration
import Amazonka.AccessAnalyzer.Types.SnsTopicConfiguration
import Amazonka.AccessAnalyzer.Types.SortCriteria
import Amazonka.AccessAnalyzer.Types.Span
import Amazonka.AccessAnalyzer.Types.SqsQueueConfiguration
import Amazonka.AccessAnalyzer.Types.StatusReason
import Amazonka.AccessAnalyzer.Types.Substring
import Amazonka.AccessAnalyzer.Types.Trail
import Amazonka.AccessAnalyzer.Types.TrailProperties
import Amazonka.AccessAnalyzer.Types.Type
import Amazonka.AccessAnalyzer.Types.ValidatePolicyFinding
import Amazonka.AccessAnalyzer.Types.ValidatePolicyFindingType
import Amazonka.AccessAnalyzer.Types.ValidatePolicyResourceType
import Amazonka.AccessAnalyzer.Types.VpcConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2019-11-01@ of the Amazon Access Analyzer SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "AccessAnalyzer",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "access-analyzer",
      Core.signingName = "access-analyzer",
      Core.version = "2019-11-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "AccessAnalyzer",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

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

-- | Validation exception error.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
