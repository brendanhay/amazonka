{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeGuruReviewer.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Lens
  ( -- * Operations

    -- ** AssociateRepository
    associateRepository_clientRequestToken,
    associateRepository_kmsKeyDetails,
    associateRepository_tags,
    associateRepository_repository,
    associateRepositoryResponse_repositoryAssociation,
    associateRepositoryResponse_tags,
    associateRepositoryResponse_httpStatus,

    -- ** CreateCodeReview
    createCodeReview_clientRequestToken,
    createCodeReview_name,
    createCodeReview_repositoryAssociationArn,
    createCodeReview_type,
    createCodeReviewResponse_codeReview,
    createCodeReviewResponse_httpStatus,

    -- ** DescribeCodeReview
    describeCodeReview_codeReviewArn,
    describeCodeReviewResponse_codeReview,
    describeCodeReviewResponse_httpStatus,

    -- ** DescribeRecommendationFeedback
    describeRecommendationFeedback_userId,
    describeRecommendationFeedback_codeReviewArn,
    describeRecommendationFeedback_recommendationId,
    describeRecommendationFeedbackResponse_recommendationFeedback,
    describeRecommendationFeedbackResponse_httpStatus,

    -- ** DescribeRepositoryAssociation
    describeRepositoryAssociation_associationArn,
    describeRepositoryAssociationResponse_repositoryAssociation,
    describeRepositoryAssociationResponse_tags,
    describeRepositoryAssociationResponse_httpStatus,

    -- ** DisassociateRepository
    disassociateRepository_associationArn,
    disassociateRepositoryResponse_repositoryAssociation,
    disassociateRepositoryResponse_tags,
    disassociateRepositoryResponse_httpStatus,

    -- ** ListCodeReviews
    listCodeReviews_maxResults,
    listCodeReviews_nextToken,
    listCodeReviews_providerTypes,
    listCodeReviews_repositoryNames,
    listCodeReviews_states,
    listCodeReviews_type,
    listCodeReviewsResponse_codeReviewSummaries,
    listCodeReviewsResponse_nextToken,
    listCodeReviewsResponse_httpStatus,

    -- ** ListRecommendationFeedback
    listRecommendationFeedback_maxResults,
    listRecommendationFeedback_nextToken,
    listRecommendationFeedback_recommendationIds,
    listRecommendationFeedback_userIds,
    listRecommendationFeedback_codeReviewArn,
    listRecommendationFeedbackResponse_nextToken,
    listRecommendationFeedbackResponse_recommendationFeedbackSummaries,
    listRecommendationFeedbackResponse_httpStatus,

    -- ** ListRecommendations
    listRecommendations_maxResults,
    listRecommendations_nextToken,
    listRecommendations_codeReviewArn,
    listRecommendationsResponse_nextToken,
    listRecommendationsResponse_recommendationSummaries,
    listRecommendationsResponse_httpStatus,

    -- ** ListRepositoryAssociations
    listRepositoryAssociations_maxResults,
    listRepositoryAssociations_names,
    listRepositoryAssociations_nextToken,
    listRepositoryAssociations_owners,
    listRepositoryAssociations_providerTypes,
    listRepositoryAssociations_states,
    listRepositoryAssociationsResponse_nextToken,
    listRepositoryAssociationsResponse_repositoryAssociationSummaries,
    listRepositoryAssociationsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutRecommendationFeedback
    putRecommendationFeedback_codeReviewArn,
    putRecommendationFeedback_recommendationId,
    putRecommendationFeedback_reactions,
    putRecommendationFeedbackResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- * Types

    -- ** BranchDiffSourceCodeType
    branchDiffSourceCodeType_sourceBranchName,
    branchDiffSourceCodeType_destinationBranchName,

    -- ** CodeArtifacts
    codeArtifacts_buildArtifactsObjectKey,
    codeArtifacts_sourceCodeArtifactsObjectKey,

    -- ** CodeCommitRepository
    codeCommitRepository_name,

    -- ** CodeReview
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

    -- ** CodeReviewSummary
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

    -- ** CodeReviewType
    codeReviewType_analysisTypes,
    codeReviewType_repositoryAnalysis,

    -- ** CommitDiffSourceCodeType
    commitDiffSourceCodeType_destinationCommit,
    commitDiffSourceCodeType_mergeBaseCommit,
    commitDiffSourceCodeType_sourceCommit,

    -- ** EventInfo
    eventInfo_name,
    eventInfo_state,

    -- ** KMSKeyDetails
    kmsKeyDetails_encryptionOption,
    kmsKeyDetails_kmsKeyId,

    -- ** Metrics
    metrics_findingsCount,
    metrics_meteredLinesOfCodeCount,
    metrics_suppressedLinesOfCodeCount,

    -- ** MetricsSummary
    metricsSummary_findingsCount,
    metricsSummary_meteredLinesOfCodeCount,
    metricsSummary_suppressedLinesOfCodeCount,

    -- ** RecommendationFeedback
    recommendationFeedback_codeReviewArn,
    recommendationFeedback_createdTimeStamp,
    recommendationFeedback_lastUpdatedTimeStamp,
    recommendationFeedback_reactions,
    recommendationFeedback_recommendationId,
    recommendationFeedback_userId,

    -- ** RecommendationFeedbackSummary
    recommendationFeedbackSummary_reactions,
    recommendationFeedbackSummary_recommendationId,
    recommendationFeedbackSummary_userId,

    -- ** RecommendationSummary
    recommendationSummary_description,
    recommendationSummary_endLine,
    recommendationSummary_filePath,
    recommendationSummary_recommendationCategory,
    recommendationSummary_recommendationId,
    recommendationSummary_ruleMetadata,
    recommendationSummary_severity,
    recommendationSummary_startLine,

    -- ** Repository
    repository_bitbucket,
    repository_codeCommit,
    repository_gitHubEnterpriseServer,
    repository_s3Bucket,

    -- ** RepositoryAnalysis
    repositoryAnalysis_repositoryHead,
    repositoryAnalysis_sourceCodeType,

    -- ** RepositoryAssociation
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

    -- ** RepositoryAssociationSummary
    repositoryAssociationSummary_associationArn,
    repositoryAssociationSummary_associationId,
    repositoryAssociationSummary_connectionArn,
    repositoryAssociationSummary_lastUpdatedTimeStamp,
    repositoryAssociationSummary_name,
    repositoryAssociationSummary_owner,
    repositoryAssociationSummary_providerType,
    repositoryAssociationSummary_state,

    -- ** RepositoryHeadSourceCodeType
    repositoryHeadSourceCodeType_branchName,

    -- ** RequestMetadata
    requestMetadata_eventInfo,
    requestMetadata_requestId,
    requestMetadata_requester,
    requestMetadata_vendorName,

    -- ** RuleMetadata
    ruleMetadata_longDescription,
    ruleMetadata_ruleId,
    ruleMetadata_ruleName,
    ruleMetadata_ruleTags,
    ruleMetadata_shortDescription,

    -- ** S3BucketRepository
    s3BucketRepository_details,
    s3BucketRepository_name,

    -- ** S3Repository
    s3Repository_name,
    s3Repository_bucketName,

    -- ** S3RepositoryDetails
    s3RepositoryDetails_bucketName,
    s3RepositoryDetails_codeArtifacts,

    -- ** SourceCodeType
    sourceCodeType_branchDiff,
    sourceCodeType_commitDiff,
    sourceCodeType_repositoryHead,
    sourceCodeType_requestMetadata,
    sourceCodeType_s3BucketRepository,

    -- ** ThirdPartySourceRepository
    thirdPartySourceRepository_name,
    thirdPartySourceRepository_connectionArn,
    thirdPartySourceRepository_owner,
  )
where

import Amazonka.CodeGuruReviewer.AssociateRepository
import Amazonka.CodeGuruReviewer.CreateCodeReview
import Amazonka.CodeGuruReviewer.DescribeCodeReview
import Amazonka.CodeGuruReviewer.DescribeRecommendationFeedback
import Amazonka.CodeGuruReviewer.DescribeRepositoryAssociation
import Amazonka.CodeGuruReviewer.DisassociateRepository
import Amazonka.CodeGuruReviewer.ListCodeReviews
import Amazonka.CodeGuruReviewer.ListRecommendationFeedback
import Amazonka.CodeGuruReviewer.ListRecommendations
import Amazonka.CodeGuruReviewer.ListRepositoryAssociations
import Amazonka.CodeGuruReviewer.ListTagsForResource
import Amazonka.CodeGuruReviewer.PutRecommendationFeedback
import Amazonka.CodeGuruReviewer.TagResource
import Amazonka.CodeGuruReviewer.Types.BranchDiffSourceCodeType
import Amazonka.CodeGuruReviewer.Types.CodeArtifacts
import Amazonka.CodeGuruReviewer.Types.CodeCommitRepository
import Amazonka.CodeGuruReviewer.Types.CodeReview
import Amazonka.CodeGuruReviewer.Types.CodeReviewSummary
import Amazonka.CodeGuruReviewer.Types.CodeReviewType
import Amazonka.CodeGuruReviewer.Types.CommitDiffSourceCodeType
import Amazonka.CodeGuruReviewer.Types.EventInfo
import Amazonka.CodeGuruReviewer.Types.KMSKeyDetails
import Amazonka.CodeGuruReviewer.Types.Metrics
import Amazonka.CodeGuruReviewer.Types.MetricsSummary
import Amazonka.CodeGuruReviewer.Types.RecommendationFeedback
import Amazonka.CodeGuruReviewer.Types.RecommendationFeedbackSummary
import Amazonka.CodeGuruReviewer.Types.RecommendationSummary
import Amazonka.CodeGuruReviewer.Types.Repository
import Amazonka.CodeGuruReviewer.Types.RepositoryAnalysis
import Amazonka.CodeGuruReviewer.Types.RepositoryAssociation
import Amazonka.CodeGuruReviewer.Types.RepositoryAssociationSummary
import Amazonka.CodeGuruReviewer.Types.RepositoryHeadSourceCodeType
import Amazonka.CodeGuruReviewer.Types.RequestMetadata
import Amazonka.CodeGuruReviewer.Types.RuleMetadata
import Amazonka.CodeGuruReviewer.Types.S3BucketRepository
import Amazonka.CodeGuruReviewer.Types.S3Repository
import Amazonka.CodeGuruReviewer.Types.S3RepositoryDetails
import Amazonka.CodeGuruReviewer.Types.SourceCodeType
import Amazonka.CodeGuruReviewer.Types.ThirdPartySourceRepository
import Amazonka.CodeGuruReviewer.UntagResource
