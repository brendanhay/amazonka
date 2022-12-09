{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeGuruReviewer.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _NotFoundException,
    _ResourceNotFoundException,
    _ThrottlingException,
    _ValidationException,

    -- * AnalysisType
    AnalysisType (..),

    -- * ConfigFileState
    ConfigFileState (..),

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
    codeReview_analysisTypes,
    codeReview_associationArn,
    codeReview_codeReviewArn,
    codeReview_configFileState,
    codeReview_createdTimeStamp,
    codeReview_lastUpdatedTimeStamp,
    codeReview_metrics,
    codeReview_name,
    codeReview_owner,
    codeReview_providerType,
    codeReview_pullRequestId,
    codeReview_repositoryName,
    codeReview_sourceCodeType,
    codeReview_state,
    codeReview_stateReason,
    codeReview_type,

    -- * CodeReviewSummary
    CodeReviewSummary (..),
    newCodeReviewSummary,
    codeReviewSummary_codeReviewArn,
    codeReviewSummary_createdTimeStamp,
    codeReviewSummary_lastUpdatedTimeStamp,
    codeReviewSummary_metricsSummary,
    codeReviewSummary_name,
    codeReviewSummary_owner,
    codeReviewSummary_providerType,
    codeReviewSummary_pullRequestId,
    codeReviewSummary_repositoryName,
    codeReviewSummary_sourceCodeType,
    codeReviewSummary_state,
    codeReviewSummary_type,

    -- * CodeReviewType
    CodeReviewType (..),
    newCodeReviewType,
    codeReviewType_analysisTypes,
    codeReviewType_repositoryAnalysis,

    -- * CommitDiffSourceCodeType
    CommitDiffSourceCodeType (..),
    newCommitDiffSourceCodeType,
    commitDiffSourceCodeType_destinationCommit,
    commitDiffSourceCodeType_mergeBaseCommit,
    commitDiffSourceCodeType_sourceCommit,

    -- * EventInfo
    EventInfo (..),
    newEventInfo,
    eventInfo_name,
    eventInfo_state,

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
    metrics_suppressedLinesOfCodeCount,

    -- * MetricsSummary
    MetricsSummary (..),
    newMetricsSummary,
    metricsSummary_findingsCount,
    metricsSummary_meteredLinesOfCodeCount,
    metricsSummary_suppressedLinesOfCodeCount,

    -- * RecommendationFeedback
    RecommendationFeedback (..),
    newRecommendationFeedback,
    recommendationFeedback_codeReviewArn,
    recommendationFeedback_createdTimeStamp,
    recommendationFeedback_lastUpdatedTimeStamp,
    recommendationFeedback_reactions,
    recommendationFeedback_recommendationId,
    recommendationFeedback_userId,

    -- * RecommendationFeedbackSummary
    RecommendationFeedbackSummary (..),
    newRecommendationFeedbackSummary,
    recommendationFeedbackSummary_reactions,
    recommendationFeedbackSummary_recommendationId,
    recommendationFeedbackSummary_userId,

    -- * RecommendationSummary
    RecommendationSummary (..),
    newRecommendationSummary,
    recommendationSummary_description,
    recommendationSummary_endLine,
    recommendationSummary_filePath,
    recommendationSummary_recommendationCategory,
    recommendationSummary_recommendationId,
    recommendationSummary_ruleMetadata,
    recommendationSummary_severity,
    recommendationSummary_startLine,

    -- * Repository
    Repository (..),
    newRepository,
    repository_bitbucket,
    repository_codeCommit,
    repository_gitHubEnterpriseServer,
    repository_s3Bucket,

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
    repositoryAssociation_connectionArn,
    repositoryAssociation_createdTimeStamp,
    repositoryAssociation_kmsKeyDetails,
    repositoryAssociation_lastUpdatedTimeStamp,
    repositoryAssociation_name,
    repositoryAssociation_owner,
    repositoryAssociation_providerType,
    repositoryAssociation_s3RepositoryDetails,
    repositoryAssociation_state,
    repositoryAssociation_stateReason,

    -- * RepositoryAssociationSummary
    RepositoryAssociationSummary (..),
    newRepositoryAssociationSummary,
    repositoryAssociationSummary_associationArn,
    repositoryAssociationSummary_associationId,
    repositoryAssociationSummary_connectionArn,
    repositoryAssociationSummary_lastUpdatedTimeStamp,
    repositoryAssociationSummary_name,
    repositoryAssociationSummary_owner,
    repositoryAssociationSummary_providerType,
    repositoryAssociationSummary_state,

    -- * RepositoryHeadSourceCodeType
    RepositoryHeadSourceCodeType (..),
    newRepositoryHeadSourceCodeType,
    repositoryHeadSourceCodeType_branchName,

    -- * RequestMetadata
    RequestMetadata (..),
    newRequestMetadata,
    requestMetadata_eventInfo,
    requestMetadata_requestId,
    requestMetadata_requester,
    requestMetadata_vendorName,

    -- * RuleMetadata
    RuleMetadata (..),
    newRuleMetadata,
    ruleMetadata_longDescription,
    ruleMetadata_ruleId,
    ruleMetadata_ruleName,
    ruleMetadata_ruleTags,
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
    s3RepositoryDetails_bucketName,
    s3RepositoryDetails_codeArtifacts,

    -- * SourceCodeType
    SourceCodeType (..),
    newSourceCodeType,
    sourceCodeType_branchDiff,
    sourceCodeType_commitDiff,
    sourceCodeType_repositoryHead,
    sourceCodeType_requestMetadata,
    sourceCodeType_s3BucketRepository,

    -- * ThirdPartySourceRepository
    ThirdPartySourceRepository (..),
    newThirdPartySourceRepository,
    thirdPartySourceRepository_name,
    thirdPartySourceRepository_connectionArn,
    thirdPartySourceRepository_owner,
  )
where

import Amazonka.CodeGuruReviewer.Types.AnalysisType
import Amazonka.CodeGuruReviewer.Types.BranchDiffSourceCodeType
import Amazonka.CodeGuruReviewer.Types.CodeArtifacts
import Amazonka.CodeGuruReviewer.Types.CodeCommitRepository
import Amazonka.CodeGuruReviewer.Types.CodeReview
import Amazonka.CodeGuruReviewer.Types.CodeReviewSummary
import Amazonka.CodeGuruReviewer.Types.CodeReviewType
import Amazonka.CodeGuruReviewer.Types.CommitDiffSourceCodeType
import Amazonka.CodeGuruReviewer.Types.ConfigFileState
import Amazonka.CodeGuruReviewer.Types.EncryptionOption
import Amazonka.CodeGuruReviewer.Types.EventInfo
import Amazonka.CodeGuruReviewer.Types.JobState
import Amazonka.CodeGuruReviewer.Types.KMSKeyDetails
import Amazonka.CodeGuruReviewer.Types.Metrics
import Amazonka.CodeGuruReviewer.Types.MetricsSummary
import Amazonka.CodeGuruReviewer.Types.ProviderType
import Amazonka.CodeGuruReviewer.Types.Reaction
import Amazonka.CodeGuruReviewer.Types.RecommendationCategory
import Amazonka.CodeGuruReviewer.Types.RecommendationFeedback
import Amazonka.CodeGuruReviewer.Types.RecommendationFeedbackSummary
import Amazonka.CodeGuruReviewer.Types.RecommendationSummary
import Amazonka.CodeGuruReviewer.Types.Repository
import Amazonka.CodeGuruReviewer.Types.RepositoryAnalysis
import Amazonka.CodeGuruReviewer.Types.RepositoryAssociation
import Amazonka.CodeGuruReviewer.Types.RepositoryAssociationState
import Amazonka.CodeGuruReviewer.Types.RepositoryAssociationSummary
import Amazonka.CodeGuruReviewer.Types.RepositoryHeadSourceCodeType
import Amazonka.CodeGuruReviewer.Types.RequestMetadata
import Amazonka.CodeGuruReviewer.Types.RuleMetadata
import Amazonka.CodeGuruReviewer.Types.S3BucketRepository
import Amazonka.CodeGuruReviewer.Types.S3Repository
import Amazonka.CodeGuruReviewer.Types.S3RepositoryDetails
import Amazonka.CodeGuruReviewer.Types.Severity
import Amazonka.CodeGuruReviewer.Types.SourceCodeType
import Amazonka.CodeGuruReviewer.Types.ThirdPartySourceRepository
import Amazonka.CodeGuruReviewer.Types.Type
import Amazonka.CodeGuruReviewer.Types.VendorName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2019-09-19@ of the Amazon CodeGuru Reviewer SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "CodeGuruReviewer",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "codeguru-reviewer",
      Core.signingName = "codeguru-reviewer",
      Core.version = "2019-09-19",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "CodeGuruReviewer",
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

-- | The requested operation would cause a conflict with the current state of
-- a service resource associated with the request. Resolve the conflict
-- before retrying this request.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The server encountered an internal error and is unable to complete the
-- request.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The resource specified in the request was not found.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | The resource specified in the request was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The input fails to satisfy the specified constraints.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
