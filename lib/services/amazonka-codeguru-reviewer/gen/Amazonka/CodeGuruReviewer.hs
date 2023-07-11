{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CodeGuruReviewer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-09-19@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- This section provides documentation for the Amazon CodeGuru Reviewer API
-- operations. CodeGuru Reviewer is a service that uses program analysis
-- and machine learning to detect potential defects that are difficult for
-- developers to find and recommends fixes in your Java and Python code.
--
-- By proactively detecting and providing recommendations for addressing
-- code defects and implementing best practices, CodeGuru Reviewer improves
-- the overall quality and maintainability of your code base during the
-- code review stage. For more information about CodeGuru Reviewer, see the
-- /<https://docs.aws.amazon.com/codeguru/latest/reviewer-ug/welcome.html Amazon CodeGuru Reviewer User Guide>./
--
-- To improve the security of your CodeGuru Reviewer API calls, you can
-- establish a private connection between your VPC and CodeGuru Reviewer by
-- creating an /interface VPC endpoint/. For more information, see
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-ug/vpc-interface-endpoints.html CodeGuru Reviewer and interface VPC endpoints (Amazon Web Services PrivateLink)>
-- in the /Amazon CodeGuru Reviewer User Guide/.
module Amazonka.CodeGuruReviewer
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- ** CodeReviewCompleted
    newCodeReviewCompleted,

    -- ** RepositoryAssociationSucceeded
    newRepositoryAssociationSucceeded,

    -- * Operations
    -- $operations

    -- ** AssociateRepository
    AssociateRepository (AssociateRepository'),
    newAssociateRepository,
    AssociateRepositoryResponse (AssociateRepositoryResponse'),
    newAssociateRepositoryResponse,

    -- ** CreateCodeReview
    CreateCodeReview (CreateCodeReview'),
    newCreateCodeReview,
    CreateCodeReviewResponse (CreateCodeReviewResponse'),
    newCreateCodeReviewResponse,

    -- ** DescribeCodeReview
    DescribeCodeReview (DescribeCodeReview'),
    newDescribeCodeReview,
    DescribeCodeReviewResponse (DescribeCodeReviewResponse'),
    newDescribeCodeReviewResponse,

    -- ** DescribeRecommendationFeedback
    DescribeRecommendationFeedback (DescribeRecommendationFeedback'),
    newDescribeRecommendationFeedback,
    DescribeRecommendationFeedbackResponse (DescribeRecommendationFeedbackResponse'),
    newDescribeRecommendationFeedbackResponse,

    -- ** DescribeRepositoryAssociation
    DescribeRepositoryAssociation (DescribeRepositoryAssociation'),
    newDescribeRepositoryAssociation,
    DescribeRepositoryAssociationResponse (DescribeRepositoryAssociationResponse'),
    newDescribeRepositoryAssociationResponse,

    -- ** DisassociateRepository
    DisassociateRepository (DisassociateRepository'),
    newDisassociateRepository,
    DisassociateRepositoryResponse (DisassociateRepositoryResponse'),
    newDisassociateRepositoryResponse,

    -- ** ListCodeReviews
    ListCodeReviews (ListCodeReviews'),
    newListCodeReviews,
    ListCodeReviewsResponse (ListCodeReviewsResponse'),
    newListCodeReviewsResponse,

    -- ** ListRecommendationFeedback
    ListRecommendationFeedback (ListRecommendationFeedback'),
    newListRecommendationFeedback,
    ListRecommendationFeedbackResponse (ListRecommendationFeedbackResponse'),
    newListRecommendationFeedbackResponse,

    -- ** ListRecommendations
    ListRecommendations (ListRecommendations'),
    newListRecommendations,
    ListRecommendationsResponse (ListRecommendationsResponse'),
    newListRecommendationsResponse,

    -- ** ListRepositoryAssociations (Paginated)
    ListRepositoryAssociations (ListRepositoryAssociations'),
    newListRepositoryAssociations,
    ListRepositoryAssociationsResponse (ListRepositoryAssociationsResponse'),
    newListRepositoryAssociationsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutRecommendationFeedback
    PutRecommendationFeedback (PutRecommendationFeedback'),
    newPutRecommendationFeedback,
    PutRecommendationFeedbackResponse (PutRecommendationFeedbackResponse'),
    newPutRecommendationFeedbackResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- * Types

    -- ** AnalysisType
    AnalysisType (..),

    -- ** ConfigFileState
    ConfigFileState (..),

    -- ** EncryptionOption
    EncryptionOption (..),

    -- ** JobState
    JobState (..),

    -- ** ProviderType
    ProviderType (..),

    -- ** Reaction
    Reaction (..),

    -- ** RecommendationCategory
    RecommendationCategory (..),

    -- ** RepositoryAssociationState
    RepositoryAssociationState (..),

    -- ** Severity
    Severity (..),

    -- ** Type
    Type (..),

    -- ** VendorName
    VendorName (..),

    -- ** BranchDiffSourceCodeType
    BranchDiffSourceCodeType (BranchDiffSourceCodeType'),
    newBranchDiffSourceCodeType,

    -- ** CodeArtifacts
    CodeArtifacts (CodeArtifacts'),
    newCodeArtifacts,

    -- ** CodeCommitRepository
    CodeCommitRepository (CodeCommitRepository'),
    newCodeCommitRepository,

    -- ** CodeReview
    CodeReview (CodeReview'),
    newCodeReview,

    -- ** CodeReviewSummary
    CodeReviewSummary (CodeReviewSummary'),
    newCodeReviewSummary,

    -- ** CodeReviewType
    CodeReviewType (CodeReviewType'),
    newCodeReviewType,

    -- ** CommitDiffSourceCodeType
    CommitDiffSourceCodeType (CommitDiffSourceCodeType'),
    newCommitDiffSourceCodeType,

    -- ** EventInfo
    EventInfo (EventInfo'),
    newEventInfo,

    -- ** KMSKeyDetails
    KMSKeyDetails (KMSKeyDetails'),
    newKMSKeyDetails,

    -- ** Metrics
    Metrics (Metrics'),
    newMetrics,

    -- ** MetricsSummary
    MetricsSummary (MetricsSummary'),
    newMetricsSummary,

    -- ** RecommendationFeedback
    RecommendationFeedback (RecommendationFeedback'),
    newRecommendationFeedback,

    -- ** RecommendationFeedbackSummary
    RecommendationFeedbackSummary (RecommendationFeedbackSummary'),
    newRecommendationFeedbackSummary,

    -- ** RecommendationSummary
    RecommendationSummary (RecommendationSummary'),
    newRecommendationSummary,

    -- ** Repository
    Repository (Repository'),
    newRepository,

    -- ** RepositoryAnalysis
    RepositoryAnalysis (RepositoryAnalysis'),
    newRepositoryAnalysis,

    -- ** RepositoryAssociation
    RepositoryAssociation (RepositoryAssociation'),
    newRepositoryAssociation,

    -- ** RepositoryAssociationSummary
    RepositoryAssociationSummary (RepositoryAssociationSummary'),
    newRepositoryAssociationSummary,

    -- ** RepositoryHeadSourceCodeType
    RepositoryHeadSourceCodeType (RepositoryHeadSourceCodeType'),
    newRepositoryHeadSourceCodeType,

    -- ** RequestMetadata
    RequestMetadata (RequestMetadata'),
    newRequestMetadata,

    -- ** RuleMetadata
    RuleMetadata (RuleMetadata'),
    newRuleMetadata,

    -- ** S3BucketRepository
    S3BucketRepository (S3BucketRepository'),
    newS3BucketRepository,

    -- ** S3Repository
    S3Repository (S3Repository'),
    newS3Repository,

    -- ** S3RepositoryDetails
    S3RepositoryDetails (S3RepositoryDetails'),
    newS3RepositoryDetails,

    -- ** SourceCodeType
    SourceCodeType (SourceCodeType'),
    newSourceCodeType,

    -- ** ThirdPartySourceRepository
    ThirdPartySourceRepository (ThirdPartySourceRepository'),
    newThirdPartySourceRepository,
  )
where

import Amazonka.CodeGuruReviewer.AssociateRepository
import Amazonka.CodeGuruReviewer.CreateCodeReview
import Amazonka.CodeGuruReviewer.DescribeCodeReview
import Amazonka.CodeGuruReviewer.DescribeRecommendationFeedback
import Amazonka.CodeGuruReviewer.DescribeRepositoryAssociation
import Amazonka.CodeGuruReviewer.DisassociateRepository
import Amazonka.CodeGuruReviewer.Lens
import Amazonka.CodeGuruReviewer.ListCodeReviews
import Amazonka.CodeGuruReviewer.ListRecommendationFeedback
import Amazonka.CodeGuruReviewer.ListRecommendations
import Amazonka.CodeGuruReviewer.ListRepositoryAssociations
import Amazonka.CodeGuruReviewer.ListTagsForResource
import Amazonka.CodeGuruReviewer.PutRecommendationFeedback
import Amazonka.CodeGuruReviewer.TagResource
import Amazonka.CodeGuruReviewer.Types
import Amazonka.CodeGuruReviewer.UntagResource
import Amazonka.CodeGuruReviewer.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CodeGuruReviewer'.

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
