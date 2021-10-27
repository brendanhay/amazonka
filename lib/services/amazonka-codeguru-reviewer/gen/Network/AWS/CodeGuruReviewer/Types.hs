{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeGuruReviewer.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeGuruReviewer.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _ConflictException,
    _NotFoundException,
    _ThrottlingException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * AnalysisType
    AnalysisType (..),

    -- * EncryptionOption
    EncryptionOption (..),

    -- * JobState
    JobState (..),

    -- * ProviderType
    ProviderType (..),

    -- * Reaction
    Reaction (..),

    -- * RecommendationCategory
    RecommendationCategory (..),

    -- * RepositoryAssociationState
    RepositoryAssociationState (..),

    -- * Severity
    Severity (..),

    -- * Type
    Type (..),

    -- * VendorName
    VendorName (..),

    -- * BranchDiffSourceCodeType
    BranchDiffSourceCodeType (..),
    newBranchDiffSourceCodeType,
    branchDiffSourceCodeType_sourceBranchName,
    branchDiffSourceCodeType_destinationBranchName,

    -- * CodeArtifacts
    CodeArtifacts (..),
    newCodeArtifacts,
    codeArtifacts_buildArtifactsObjectKey,
    codeArtifacts_sourceCodeArtifactsObjectKey,

    -- * CodeCommitRepository
    CodeCommitRepository (..),
    newCodeCommitRepository,
    codeCommitRepository_name,

    -- * CodeReview
    CodeReview (..),
    newCodeReview,
    codeReview_associationArn,
    codeReview_state,
    codeReview_metrics,
    codeReview_pullRequestId,
    codeReview_providerType,
    codeReview_owner,
    codeReview_name,
    codeReview_codeReviewArn,
    codeReview_repositoryName,
    codeReview_type,
    codeReview_sourceCodeType,
    codeReview_stateReason,
    codeReview_createdTimeStamp,
    codeReview_analysisTypes,
    codeReview_lastUpdatedTimeStamp,

    -- * CodeReviewSummary
    CodeReviewSummary (..),
    newCodeReviewSummary,
    codeReviewSummary_state,
    codeReviewSummary_pullRequestId,
    codeReviewSummary_providerType,
    codeReviewSummary_owner,
    codeReviewSummary_name,
    codeReviewSummary_codeReviewArn,
    codeReviewSummary_repositoryName,
    codeReviewSummary_type,
    codeReviewSummary_sourceCodeType,
    codeReviewSummary_metricsSummary,
    codeReviewSummary_createdTimeStamp,
    codeReviewSummary_lastUpdatedTimeStamp,

    -- * CodeReviewType
    CodeReviewType (..),
    newCodeReviewType,
    codeReviewType_analysisTypes,
    codeReviewType_repositoryAnalysis,

    -- * CommitDiffSourceCodeType
    CommitDiffSourceCodeType (..),
    newCommitDiffSourceCodeType,
    commitDiffSourceCodeType_sourceCommit,
    commitDiffSourceCodeType_mergeBaseCommit,
    commitDiffSourceCodeType_destinationCommit,

    -- * EventInfo
    EventInfo (..),
    newEventInfo,
    eventInfo_state,
    eventInfo_name,

    -- * KMSKeyDetails
    KMSKeyDetails (..),
    newKMSKeyDetails,
    kmsKeyDetails_encryptionOption,
    kmsKeyDetails_kmsKeyId,

    -- * Metrics
    Metrics (..),
    newMetrics,
    metrics_findingsCount,
    metrics_meteredLinesOfCodeCount,

    -- * MetricsSummary
    MetricsSummary (..),
    newMetricsSummary,
    metricsSummary_findingsCount,
    metricsSummary_meteredLinesOfCodeCount,

    -- * RecommendationFeedback
    RecommendationFeedback (..),
    newRecommendationFeedback,
    recommendationFeedback_recommendationId,
    recommendationFeedback_userId,
    recommendationFeedback_reactions,
    recommendationFeedback_codeReviewArn,
    recommendationFeedback_createdTimeStamp,
    recommendationFeedback_lastUpdatedTimeStamp,

    -- * RecommendationFeedbackSummary
    RecommendationFeedbackSummary (..),
    newRecommendationFeedbackSummary,
    recommendationFeedbackSummary_recommendationId,
    recommendationFeedbackSummary_userId,
    recommendationFeedbackSummary_reactions,

    -- * RecommendationSummary
    RecommendationSummary (..),
    newRecommendationSummary,
    recommendationSummary_recommendationId,
    recommendationSummary_filePath,
    recommendationSummary_severity,
    recommendationSummary_ruleMetadata,
    recommendationSummary_startLine,
    recommendationSummary_endLine,
    recommendationSummary_description,
    recommendationSummary_recommendationCategory,

    -- * Repository
    Repository (..),
    newRepository,
    repository_codeCommit,
    repository_gitHubEnterpriseServer,
    repository_s3Bucket,
    repository_bitbucket,

    -- * RepositoryAnalysis
    RepositoryAnalysis (..),
    newRepositoryAnalysis,
    repositoryAnalysis_repositoryHead,
    repositoryAnalysis_sourceCodeType,

    -- * RepositoryAssociation
    RepositoryAssociation (..),
    newRepositoryAssociation,
    repositoryAssociation_associationArn,
    repositoryAssociation_associationId,
    repositoryAssociation_state,
    repositoryAssociation_s3RepositoryDetails,
    repositoryAssociation_providerType,
    repositoryAssociation_owner,
    repositoryAssociation_name,
    repositoryAssociation_kmsKeyDetails,
    repositoryAssociation_connectionArn,
    repositoryAssociation_stateReason,
    repositoryAssociation_createdTimeStamp,
    repositoryAssociation_lastUpdatedTimeStamp,

    -- * RepositoryAssociationSummary
    RepositoryAssociationSummary (..),
    newRepositoryAssociationSummary,
    repositoryAssociationSummary_associationArn,
    repositoryAssociationSummary_associationId,
    repositoryAssociationSummary_state,
    repositoryAssociationSummary_providerType,
    repositoryAssociationSummary_owner,
    repositoryAssociationSummary_name,
    repositoryAssociationSummary_connectionArn,
    repositoryAssociationSummary_lastUpdatedTimeStamp,

    -- * RepositoryHeadSourceCodeType
    RepositoryHeadSourceCodeType (..),
    newRepositoryHeadSourceCodeType,
    repositoryHeadSourceCodeType_branchName,

    -- * RequestMetadata
    RequestMetadata (..),
    newRequestMetadata,
    requestMetadata_requestId,
    requestMetadata_eventInfo,
    requestMetadata_vendorName,
    requestMetadata_requester,

    -- * RuleMetadata
    RuleMetadata (..),
    newRuleMetadata,
    ruleMetadata_longDescription,
    ruleMetadata_ruleTags,
    ruleMetadata_ruleId,
    ruleMetadata_ruleName,
    ruleMetadata_shortDescription,

    -- * S3BucketRepository
    S3BucketRepository (..),
    newS3BucketRepository,
    s3BucketRepository_details,
    s3BucketRepository_name,

    -- * S3Repository
    S3Repository (..),
    newS3Repository,
    s3Repository_name,
    s3Repository_bucketName,

    -- * S3RepositoryDetails
    S3RepositoryDetails (..),
    newS3RepositoryDetails,
    s3RepositoryDetails_codeArtifacts,
    s3RepositoryDetails_bucketName,

    -- * SourceCodeType
    SourceCodeType (..),
    newSourceCodeType,
    sourceCodeType_s3BucketRepository,
    sourceCodeType_requestMetadata,
    sourceCodeType_repositoryHead,
    sourceCodeType_commitDiff,
    sourceCodeType_branchDiff,

    -- * ThirdPartySourceRepository
    ThirdPartySourceRepository (..),
    newThirdPartySourceRepository,
    thirdPartySourceRepository_name,
    thirdPartySourceRepository_connectionArn,
    thirdPartySourceRepository_owner,
  )
where

import Network.AWS.CodeGuruReviewer.Types.AnalysisType
import Network.AWS.CodeGuruReviewer.Types.BranchDiffSourceCodeType
import Network.AWS.CodeGuruReviewer.Types.CodeArtifacts
import Network.AWS.CodeGuruReviewer.Types.CodeCommitRepository
import Network.AWS.CodeGuruReviewer.Types.CodeReview
import Network.AWS.CodeGuruReviewer.Types.CodeReviewSummary
import Network.AWS.CodeGuruReviewer.Types.CodeReviewType
import Network.AWS.CodeGuruReviewer.Types.CommitDiffSourceCodeType
import Network.AWS.CodeGuruReviewer.Types.EncryptionOption
import Network.AWS.CodeGuruReviewer.Types.EventInfo
import Network.AWS.CodeGuruReviewer.Types.JobState
import Network.AWS.CodeGuruReviewer.Types.KMSKeyDetails
import Network.AWS.CodeGuruReviewer.Types.Metrics
import Network.AWS.CodeGuruReviewer.Types.MetricsSummary
import Network.AWS.CodeGuruReviewer.Types.ProviderType
import Network.AWS.CodeGuruReviewer.Types.Reaction
import Network.AWS.CodeGuruReviewer.Types.RecommendationCategory
import Network.AWS.CodeGuruReviewer.Types.RecommendationFeedback
import Network.AWS.CodeGuruReviewer.Types.RecommendationFeedbackSummary
import Network.AWS.CodeGuruReviewer.Types.RecommendationSummary
import Network.AWS.CodeGuruReviewer.Types.Repository
import Network.AWS.CodeGuruReviewer.Types.RepositoryAnalysis
import Network.AWS.CodeGuruReviewer.Types.RepositoryAssociation
import Network.AWS.CodeGuruReviewer.Types.RepositoryAssociationState
import Network.AWS.CodeGuruReviewer.Types.RepositoryAssociationSummary
import Network.AWS.CodeGuruReviewer.Types.RepositoryHeadSourceCodeType
import Network.AWS.CodeGuruReviewer.Types.RequestMetadata
import Network.AWS.CodeGuruReviewer.Types.RuleMetadata
import Network.AWS.CodeGuruReviewer.Types.S3BucketRepository
import Network.AWS.CodeGuruReviewer.Types.S3Repository
import Network.AWS.CodeGuruReviewer.Types.S3RepositoryDetails
import Network.AWS.CodeGuruReviewer.Types.Severity
import Network.AWS.CodeGuruReviewer.Types.SourceCodeType
import Network.AWS.CodeGuruReviewer.Types.ThirdPartySourceRepository
import Network.AWS.CodeGuruReviewer.Types.Type
import Network.AWS.CodeGuruReviewer.Types.VendorName
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2019-09-19@ of the Amazon CodeGuru Reviewer SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "CodeGuruReviewer",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "codeguru-reviewer",
      Core._serviceSigningName = "codeguru-reviewer",
      Core._serviceVersion = "2019-09-19",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "CodeGuruReviewer",
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

-- | The input fails to satisfy the specified constraints.
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

-- | The requested operation would cause a conflict with the current state of
-- a service resource associated with the request. Resolve the conflict
-- before retrying this request.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The resource specified in the request was not found.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The server encountered an internal error and is unable to complete the
-- request.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The resource specified in the request was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
