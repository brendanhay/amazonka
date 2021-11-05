{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeGuruReviewer.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Lens
  ( -- * Operations

    -- ** ListRecommendationFeedback
    listRecommendationFeedback_userIds,
    listRecommendationFeedback_nextToken,
    listRecommendationFeedback_recommendationIds,
    listRecommendationFeedback_maxResults,
    listRecommendationFeedback_codeReviewArn,
    listRecommendationFeedbackResponse_nextToken,
    listRecommendationFeedbackResponse_recommendationFeedbackSummaries,
    listRecommendationFeedbackResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DisassociateRepository
    disassociateRepository_associationArn,
    disassociateRepositoryResponse_repositoryAssociation,
    disassociateRepositoryResponse_tags,
    disassociateRepositoryResponse_httpStatus,

    -- ** DescribeRepositoryAssociation
    describeRepositoryAssociation_associationArn,
    describeRepositoryAssociationResponse_repositoryAssociation,
    describeRepositoryAssociationResponse_tags,
    describeRepositoryAssociationResponse_httpStatus,

    -- ** DescribeCodeReview
    describeCodeReview_codeReviewArn,
    describeCodeReviewResponse_codeReview,
    describeCodeReviewResponse_httpStatus,

    -- ** ListRepositoryAssociations
    listRepositoryAssociations_states,
    listRepositoryAssociations_owners,
    listRepositoryAssociations_providerTypes,
    listRepositoryAssociations_nextToken,
    listRepositoryAssociations_names,
    listRepositoryAssociations_maxResults,
    listRepositoryAssociationsResponse_nextToken,
    listRepositoryAssociationsResponse_repositoryAssociationSummaries,
    listRepositoryAssociationsResponse_httpStatus,

    -- ** DescribeRecommendationFeedback
    describeRecommendationFeedback_userId,
    describeRecommendationFeedback_codeReviewArn,
    describeRecommendationFeedback_recommendationId,
    describeRecommendationFeedbackResponse_recommendationFeedback,
    describeRecommendationFeedbackResponse_httpStatus,

    -- ** ListRecommendations
    listRecommendations_nextToken,
    listRecommendations_maxResults,
    listRecommendations_codeReviewArn,
    listRecommendationsResponse_nextToken,
    listRecommendationsResponse_recommendationSummaries,
    listRecommendationsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** CreateCodeReview
    createCodeReview_clientRequestToken,
    createCodeReview_name,
    createCodeReview_repositoryAssociationArn,
    createCodeReview_type,
    createCodeReviewResponse_codeReview,
    createCodeReviewResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** ListCodeReviews
    listCodeReviews_states,
    listCodeReviews_providerTypes,
    listCodeReviews_repositoryNames,
    listCodeReviews_nextToken,
    listCodeReviews_maxResults,
    listCodeReviews_type,
    listCodeReviewsResponse_codeReviewSummaries,
    listCodeReviewsResponse_nextToken,
    listCodeReviewsResponse_httpStatus,

    -- ** AssociateRepository
    associateRepository_kmsKeyDetails,
    associateRepository_clientRequestToken,
    associateRepository_tags,
    associateRepository_repository,
    associateRepositoryResponse_repositoryAssociation,
    associateRepositoryResponse_tags,
    associateRepositoryResponse_httpStatus,

    -- ** PutRecommendationFeedback
    putRecommendationFeedback_codeReviewArn,
    putRecommendationFeedback_recommendationId,
    putRecommendationFeedback_reactions,
    putRecommendationFeedbackResponse_httpStatus,

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

    -- ** CodeReviewSummary
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

    -- ** CodeReviewType
    codeReviewType_analysisTypes,
    codeReviewType_repositoryAnalysis,

    -- ** CommitDiffSourceCodeType
    commitDiffSourceCodeType_sourceCommit,
    commitDiffSourceCodeType_mergeBaseCommit,
    commitDiffSourceCodeType_destinationCommit,

    -- ** EventInfo
    eventInfo_state,
    eventInfo_name,

    -- ** KMSKeyDetails
    kmsKeyDetails_encryptionOption,
    kmsKeyDetails_kmsKeyId,

    -- ** Metrics
    metrics_findingsCount,
    metrics_meteredLinesOfCodeCount,

    -- ** MetricsSummary
    metricsSummary_findingsCount,
    metricsSummary_meteredLinesOfCodeCount,

    -- ** RecommendationFeedback
    recommendationFeedback_recommendationId,
    recommendationFeedback_userId,
    recommendationFeedback_reactions,
    recommendationFeedback_codeReviewArn,
    recommendationFeedback_createdTimeStamp,
    recommendationFeedback_lastUpdatedTimeStamp,

    -- ** RecommendationFeedbackSummary
    recommendationFeedbackSummary_recommendationId,
    recommendationFeedbackSummary_userId,
    recommendationFeedbackSummary_reactions,

    -- ** RecommendationSummary
    recommendationSummary_recommendationId,
    recommendationSummary_filePath,
    recommendationSummary_severity,
    recommendationSummary_ruleMetadata,
    recommendationSummary_startLine,
    recommendationSummary_endLine,
    recommendationSummary_description,
    recommendationSummary_recommendationCategory,

    -- ** Repository
    repository_codeCommit,
    repository_gitHubEnterpriseServer,
    repository_s3Bucket,
    repository_bitbucket,

    -- ** RepositoryAnalysis
    repositoryAnalysis_repositoryHead,
    repositoryAnalysis_sourceCodeType,

    -- ** RepositoryAssociation
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

    -- ** RepositoryAssociationSummary
    repositoryAssociationSummary_associationArn,
    repositoryAssociationSummary_associationId,
    repositoryAssociationSummary_state,
    repositoryAssociationSummary_providerType,
    repositoryAssociationSummary_owner,
    repositoryAssociationSummary_name,
    repositoryAssociationSummary_connectionArn,
    repositoryAssociationSummary_lastUpdatedTimeStamp,

    -- ** RepositoryHeadSourceCodeType
    repositoryHeadSourceCodeType_branchName,

    -- ** RequestMetadata
    requestMetadata_requestId,
    requestMetadata_eventInfo,
    requestMetadata_vendorName,
    requestMetadata_requester,

    -- ** RuleMetadata
    ruleMetadata_longDescription,
    ruleMetadata_ruleTags,
    ruleMetadata_ruleId,
    ruleMetadata_ruleName,
    ruleMetadata_shortDescription,

    -- ** S3BucketRepository
    s3BucketRepository_details,
    s3BucketRepository_name,

    -- ** S3Repository
    s3Repository_name,
    s3Repository_bucketName,

    -- ** S3RepositoryDetails
    s3RepositoryDetails_codeArtifacts,
    s3RepositoryDetails_bucketName,

    -- ** SourceCodeType
    sourceCodeType_s3BucketRepository,
    sourceCodeType_requestMetadata,
    sourceCodeType_repositoryHead,
    sourceCodeType_commitDiff,
    sourceCodeType_branchDiff,

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
