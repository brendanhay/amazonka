{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.AccessAnalyzer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-11-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Identity and Access Management Access Analyzer helps identify potential
-- resource-access risks by enabling you to identify any policies that
-- grant access to an external principal. It does this by using logic-based
-- reasoning to analyze resource-based policies in your Amazon Web Services
-- environment. An external principal can be another Amazon Web Services
-- account, a root user, an IAM user or role, a federated user, an Amazon
-- Web Services service, or an anonymous user. You can also use IAM Access
-- Analyzer to preview and validate public and cross-account access to your
-- resources before deploying permissions changes. This guide describes the
-- Identity and Access Management Access Analyzer operations that you can
-- call programmatically. For general information about IAM Access
-- Analyzer, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/what-is-access-analyzer.html Identity and Access Management Access Analyzer>
-- in the __IAM User Guide__.
--
-- To start using IAM Access Analyzer, you first need to create an
-- analyzer.
module Amazonka.AccessAnalyzer
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListFindings (Paginated)
    ListFindings (ListFindings'),
    newListFindings,
    ListFindingsResponse (ListFindingsResponse'),
    newListFindingsResponse,

    -- ** GetAnalyzedResource
    GetAnalyzedResource (GetAnalyzedResource'),
    newGetAnalyzedResource,
    GetAnalyzedResourceResponse (GetAnalyzedResourceResponse'),
    newGetAnalyzedResourceResponse,

    -- ** ListPolicyGenerations (Paginated)
    ListPolicyGenerations (ListPolicyGenerations'),
    newListPolicyGenerations,
    ListPolicyGenerationsResponse (ListPolicyGenerationsResponse'),
    newListPolicyGenerationsResponse,

    -- ** ListAccessPreviews (Paginated)
    ListAccessPreviews (ListAccessPreviews'),
    newListAccessPreviews,
    ListAccessPreviewsResponse (ListAccessPreviewsResponse'),
    newListAccessPreviewsResponse,

    -- ** CreateAccessPreview
    CreateAccessPreview (CreateAccessPreview'),
    newCreateAccessPreview,
    CreateAccessPreviewResponse (CreateAccessPreviewResponse'),
    newCreateAccessPreviewResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** StartResourceScan
    StartResourceScan (StartResourceScan'),
    newStartResourceScan,
    StartResourceScanResponse (StartResourceScanResponse'),
    newStartResourceScanResponse,

    -- ** DeleteArchiveRule
    DeleteArchiveRule (DeleteArchiveRule'),
    newDeleteArchiveRule,
    DeleteArchiveRuleResponse (DeleteArchiveRuleResponse'),
    newDeleteArchiveRuleResponse,

    -- ** UpdateArchiveRule
    UpdateArchiveRule (UpdateArchiveRule'),
    newUpdateArchiveRule,
    UpdateArchiveRuleResponse (UpdateArchiveRuleResponse'),
    newUpdateArchiveRuleResponse,

    -- ** GetAccessPreview
    GetAccessPreview (GetAccessPreview'),
    newGetAccessPreview,
    GetAccessPreviewResponse (GetAccessPreviewResponse'),
    newGetAccessPreviewResponse,

    -- ** ListAnalyzedResources (Paginated)
    ListAnalyzedResources (ListAnalyzedResources'),
    newListAnalyzedResources,
    ListAnalyzedResourcesResponse (ListAnalyzedResourcesResponse'),
    newListAnalyzedResourcesResponse,

    -- ** StartPolicyGeneration
    StartPolicyGeneration (StartPolicyGeneration'),
    newStartPolicyGeneration,
    StartPolicyGenerationResponse (StartPolicyGenerationResponse'),
    newStartPolicyGenerationResponse,

    -- ** ValidatePolicy (Paginated)
    ValidatePolicy (ValidatePolicy'),
    newValidatePolicy,
    ValidatePolicyResponse (ValidatePolicyResponse'),
    newValidatePolicyResponse,

    -- ** DeleteAnalyzer
    DeleteAnalyzer (DeleteAnalyzer'),
    newDeleteAnalyzer,
    DeleteAnalyzerResponse (DeleteAnalyzerResponse'),
    newDeleteAnalyzerResponse,

    -- ** UpdateFindings
    UpdateFindings (UpdateFindings'),
    newUpdateFindings,
    UpdateFindingsResponse (UpdateFindingsResponse'),
    newUpdateFindingsResponse,

    -- ** ListAnalyzers (Paginated)
    ListAnalyzers (ListAnalyzers'),
    newListAnalyzers,
    ListAnalyzersResponse (ListAnalyzersResponse'),
    newListAnalyzersResponse,

    -- ** ListAccessPreviewFindings (Paginated)
    ListAccessPreviewFindings (ListAccessPreviewFindings'),
    newListAccessPreviewFindings,
    ListAccessPreviewFindingsResponse (ListAccessPreviewFindingsResponse'),
    newListAccessPreviewFindingsResponse,

    -- ** GetArchiveRule
    GetArchiveRule (GetArchiveRule'),
    newGetArchiveRule,
    GetArchiveRuleResponse (GetArchiveRuleResponse'),
    newGetArchiveRuleResponse,

    -- ** CreateAnalyzer
    CreateAnalyzer (CreateAnalyzer'),
    newCreateAnalyzer,
    CreateAnalyzerResponse (CreateAnalyzerResponse'),
    newCreateAnalyzerResponse,

    -- ** ListArchiveRules (Paginated)
    ListArchiveRules (ListArchiveRules'),
    newListArchiveRules,
    ListArchiveRulesResponse (ListArchiveRulesResponse'),
    newListArchiveRulesResponse,

    -- ** CreateArchiveRule
    CreateArchiveRule (CreateArchiveRule'),
    newCreateArchiveRule,
    CreateArchiveRuleResponse (CreateArchiveRuleResponse'),
    newCreateArchiveRuleResponse,

    -- ** CancelPolicyGeneration
    CancelPolicyGeneration (CancelPolicyGeneration'),
    newCancelPolicyGeneration,
    CancelPolicyGenerationResponse (CancelPolicyGenerationResponse'),
    newCancelPolicyGenerationResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ApplyArchiveRule
    ApplyArchiveRule (ApplyArchiveRule'),
    newApplyArchiveRule,
    ApplyArchiveRuleResponse (ApplyArchiveRuleResponse'),
    newApplyArchiveRuleResponse,

    -- ** GetAnalyzer
    GetAnalyzer (GetAnalyzer'),
    newGetAnalyzer,
    GetAnalyzerResponse (GetAnalyzerResponse'),
    newGetAnalyzerResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** GetFinding
    GetFinding (GetFinding'),
    newGetFinding,
    GetFindingResponse (GetFindingResponse'),
    newGetFindingResponse,

    -- ** GetGeneratedPolicy
    GetGeneratedPolicy (GetGeneratedPolicy'),
    newGetGeneratedPolicy,
    GetGeneratedPolicyResponse (GetGeneratedPolicyResponse'),
    newGetGeneratedPolicyResponse,

    -- * Types

    -- ** AccessPreviewStatus
    AccessPreviewStatus (..),

    -- ** AccessPreviewStatusReasonCode
    AccessPreviewStatusReasonCode (..),

    -- ** AclPermission
    AclPermission (..),

    -- ** AnalyzerStatus
    AnalyzerStatus (..),

    -- ** FindingChangeType
    FindingChangeType (..),

    -- ** FindingSourceType
    FindingSourceType (..),

    -- ** FindingStatus
    FindingStatus (..),

    -- ** FindingStatusUpdate
    FindingStatusUpdate (..),

    -- ** JobErrorCode
    JobErrorCode (..),

    -- ** JobStatus
    JobStatus (..),

    -- ** KmsGrantOperation
    KmsGrantOperation (..),

    -- ** Locale
    Locale (..),

    -- ** OrderBy
    OrderBy (..),

    -- ** PolicyType
    PolicyType (..),

    -- ** ReasonCode
    ReasonCode (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** Type
    Type (..),

    -- ** ValidatePolicyFindingType
    ValidatePolicyFindingType (..),

    -- ** AccessPreview
    AccessPreview (AccessPreview'),
    newAccessPreview,

    -- ** AccessPreviewFinding
    AccessPreviewFinding (AccessPreviewFinding'),
    newAccessPreviewFinding,

    -- ** AccessPreviewStatusReason
    AccessPreviewStatusReason (AccessPreviewStatusReason'),
    newAccessPreviewStatusReason,

    -- ** AccessPreviewSummary
    AccessPreviewSummary (AccessPreviewSummary'),
    newAccessPreviewSummary,

    -- ** AclGrantee
    AclGrantee (AclGrantee'),
    newAclGrantee,

    -- ** AnalyzedResource
    AnalyzedResource (AnalyzedResource'),
    newAnalyzedResource,

    -- ** AnalyzedResourceSummary
    AnalyzedResourceSummary (AnalyzedResourceSummary'),
    newAnalyzedResourceSummary,

    -- ** AnalyzerSummary
    AnalyzerSummary (AnalyzerSummary'),
    newAnalyzerSummary,

    -- ** ArchiveRuleSummary
    ArchiveRuleSummary (ArchiveRuleSummary'),
    newArchiveRuleSummary,

    -- ** CloudTrailDetails
    CloudTrailDetails (CloudTrailDetails'),
    newCloudTrailDetails,

    -- ** CloudTrailProperties
    CloudTrailProperties (CloudTrailProperties'),
    newCloudTrailProperties,

    -- ** Configuration
    Configuration (Configuration'),
    newConfiguration,

    -- ** Criterion
    Criterion (Criterion'),
    newCriterion,

    -- ** Finding
    Finding (Finding'),
    newFinding,

    -- ** FindingSource
    FindingSource (FindingSource'),
    newFindingSource,

    -- ** FindingSourceDetail
    FindingSourceDetail (FindingSourceDetail'),
    newFindingSourceDetail,

    -- ** FindingSummary
    FindingSummary (FindingSummary'),
    newFindingSummary,

    -- ** GeneratedPolicy
    GeneratedPolicy (GeneratedPolicy'),
    newGeneratedPolicy,

    -- ** GeneratedPolicyProperties
    GeneratedPolicyProperties (GeneratedPolicyProperties'),
    newGeneratedPolicyProperties,

    -- ** GeneratedPolicyResult
    GeneratedPolicyResult (GeneratedPolicyResult'),
    newGeneratedPolicyResult,

    -- ** IamRoleConfiguration
    IamRoleConfiguration (IamRoleConfiguration'),
    newIamRoleConfiguration,

    -- ** InlineArchiveRule
    InlineArchiveRule (InlineArchiveRule'),
    newInlineArchiveRule,

    -- ** InternetConfiguration
    InternetConfiguration (InternetConfiguration'),
    newInternetConfiguration,

    -- ** JobDetails
    JobDetails (JobDetails'),
    newJobDetails,

    -- ** JobError
    JobError (JobError'),
    newJobError,

    -- ** KmsGrantConfiguration
    KmsGrantConfiguration (KmsGrantConfiguration'),
    newKmsGrantConfiguration,

    -- ** KmsGrantConstraints
    KmsGrantConstraints (KmsGrantConstraints'),
    newKmsGrantConstraints,

    -- ** KmsKeyConfiguration
    KmsKeyConfiguration (KmsKeyConfiguration'),
    newKmsKeyConfiguration,

    -- ** Location
    Location (Location'),
    newLocation,

    -- ** NetworkOriginConfiguration
    NetworkOriginConfiguration (NetworkOriginConfiguration'),
    newNetworkOriginConfiguration,

    -- ** PathElement
    PathElement (PathElement'),
    newPathElement,

    -- ** PolicyGeneration
    PolicyGeneration (PolicyGeneration'),
    newPolicyGeneration,

    -- ** PolicyGenerationDetails
    PolicyGenerationDetails (PolicyGenerationDetails'),
    newPolicyGenerationDetails,

    -- ** Position
    Position (Position'),
    newPosition,

    -- ** S3AccessPointConfiguration
    S3AccessPointConfiguration (S3AccessPointConfiguration'),
    newS3AccessPointConfiguration,

    -- ** S3BucketAclGrantConfiguration
    S3BucketAclGrantConfiguration (S3BucketAclGrantConfiguration'),
    newS3BucketAclGrantConfiguration,

    -- ** S3BucketConfiguration
    S3BucketConfiguration (S3BucketConfiguration'),
    newS3BucketConfiguration,

    -- ** S3PublicAccessBlockConfiguration
    S3PublicAccessBlockConfiguration (S3PublicAccessBlockConfiguration'),
    newS3PublicAccessBlockConfiguration,

    -- ** SecretsManagerSecretConfiguration
    SecretsManagerSecretConfiguration (SecretsManagerSecretConfiguration'),
    newSecretsManagerSecretConfiguration,

    -- ** SortCriteria
    SortCriteria (SortCriteria'),
    newSortCriteria,

    -- ** Span
    Span (Span'),
    newSpan,

    -- ** SqsQueueConfiguration
    SqsQueueConfiguration (SqsQueueConfiguration'),
    newSqsQueueConfiguration,

    -- ** StatusReason
    StatusReason (StatusReason'),
    newStatusReason,

    -- ** Substring
    Substring (Substring'),
    newSubstring,

    -- ** Trail
    Trail (Trail'),
    newTrail,

    -- ** TrailProperties
    TrailProperties (TrailProperties'),
    newTrailProperties,

    -- ** ValidatePolicyFinding
    ValidatePolicyFinding (ValidatePolicyFinding'),
    newValidatePolicyFinding,

    -- ** VpcConfiguration
    VpcConfiguration (VpcConfiguration'),
    newVpcConfiguration,
  )
where

import Amazonka.AccessAnalyzer.ApplyArchiveRule
import Amazonka.AccessAnalyzer.CancelPolicyGeneration
import Amazonka.AccessAnalyzer.CreateAccessPreview
import Amazonka.AccessAnalyzer.CreateAnalyzer
import Amazonka.AccessAnalyzer.CreateArchiveRule
import Amazonka.AccessAnalyzer.DeleteAnalyzer
import Amazonka.AccessAnalyzer.DeleteArchiveRule
import Amazonka.AccessAnalyzer.GetAccessPreview
import Amazonka.AccessAnalyzer.GetAnalyzedResource
import Amazonka.AccessAnalyzer.GetAnalyzer
import Amazonka.AccessAnalyzer.GetArchiveRule
import Amazonka.AccessAnalyzer.GetFinding
import Amazonka.AccessAnalyzer.GetGeneratedPolicy
import Amazonka.AccessAnalyzer.Lens
import Amazonka.AccessAnalyzer.ListAccessPreviewFindings
import Amazonka.AccessAnalyzer.ListAccessPreviews
import Amazonka.AccessAnalyzer.ListAnalyzedResources
import Amazonka.AccessAnalyzer.ListAnalyzers
import Amazonka.AccessAnalyzer.ListArchiveRules
import Amazonka.AccessAnalyzer.ListFindings
import Amazonka.AccessAnalyzer.ListPolicyGenerations
import Amazonka.AccessAnalyzer.ListTagsForResource
import Amazonka.AccessAnalyzer.StartPolicyGeneration
import Amazonka.AccessAnalyzer.StartResourceScan
import Amazonka.AccessAnalyzer.TagResource
import Amazonka.AccessAnalyzer.Types
import Amazonka.AccessAnalyzer.UntagResource
import Amazonka.AccessAnalyzer.UpdateArchiveRule
import Amazonka.AccessAnalyzer.UpdateFindings
import Amazonka.AccessAnalyzer.ValidatePolicy
import Amazonka.AccessAnalyzer.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'AccessAnalyzer'.

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
