{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AccessAnalyzer.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AccessAnalyzer.Lens
  ( -- * Operations

    -- ** ListFindings
    listFindings_nextToken,
    listFindings_sort,
    listFindings_filter,
    listFindings_maxResults,
    listFindings_analyzerArn,
    listFindingsResponse_nextToken,
    listFindingsResponse_httpStatus,
    listFindingsResponse_findings,

    -- ** GetAnalyzedResource
    getAnalyzedResource_analyzerArn,
    getAnalyzedResource_resourceArn,
    getAnalyzedResourceResponse_resource,
    getAnalyzedResourceResponse_httpStatus,

    -- ** ListPolicyGenerations
    listPolicyGenerations_nextToken,
    listPolicyGenerations_principalArn,
    listPolicyGenerations_maxResults,
    listPolicyGenerationsResponse_nextToken,
    listPolicyGenerationsResponse_httpStatus,
    listPolicyGenerationsResponse_policyGenerations,

    -- ** ListAccessPreviews
    listAccessPreviews_nextToken,
    listAccessPreviews_maxResults,
    listAccessPreviews_analyzerArn,
    listAccessPreviewsResponse_nextToken,
    listAccessPreviewsResponse_httpStatus,
    listAccessPreviewsResponse_accessPreviews,

    -- ** CreateAccessPreview
    createAccessPreview_clientToken,
    createAccessPreview_analyzerArn,
    createAccessPreview_configurations,
    createAccessPreviewResponse_httpStatus,
    createAccessPreviewResponse_id,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** StartResourceScan
    startResourceScan_analyzerArn,
    startResourceScan_resourceArn,

    -- ** DeleteArchiveRule
    deleteArchiveRule_clientToken,
    deleteArchiveRule_analyzerName,
    deleteArchiveRule_ruleName,

    -- ** UpdateArchiveRule
    updateArchiveRule_clientToken,
    updateArchiveRule_analyzerName,
    updateArchiveRule_filter,
    updateArchiveRule_ruleName,

    -- ** GetAccessPreview
    getAccessPreview_accessPreviewId,
    getAccessPreview_analyzerArn,
    getAccessPreviewResponse_httpStatus,
    getAccessPreviewResponse_accessPreview,

    -- ** ListAnalyzedResources
    listAnalyzedResources_resourceType,
    listAnalyzedResources_nextToken,
    listAnalyzedResources_maxResults,
    listAnalyzedResources_analyzerArn,
    listAnalyzedResourcesResponse_nextToken,
    listAnalyzedResourcesResponse_httpStatus,
    listAnalyzedResourcesResponse_analyzedResources,

    -- ** StartPolicyGeneration
    startPolicyGeneration_clientToken,
    startPolicyGeneration_cloudTrailDetails,
    startPolicyGeneration_policyGenerationDetails,
    startPolicyGenerationResponse_httpStatus,
    startPolicyGenerationResponse_jobId,

    -- ** ValidatePolicy
    validatePolicy_locale,
    validatePolicy_nextToken,
    validatePolicy_maxResults,
    validatePolicy_policyDocument,
    validatePolicy_policyType,
    validatePolicyResponse_nextToken,
    validatePolicyResponse_httpStatus,
    validatePolicyResponse_findings,

    -- ** DeleteAnalyzer
    deleteAnalyzer_clientToken,
    deleteAnalyzer_analyzerName,

    -- ** UpdateFindings
    updateFindings_clientToken,
    updateFindings_ids,
    updateFindings_resourceArn,
    updateFindings_analyzerArn,
    updateFindings_status,

    -- ** ListAnalyzers
    listAnalyzers_nextToken,
    listAnalyzers_type,
    listAnalyzers_maxResults,
    listAnalyzersResponse_nextToken,
    listAnalyzersResponse_httpStatus,
    listAnalyzersResponse_analyzers,

    -- ** ListAccessPreviewFindings
    listAccessPreviewFindings_nextToken,
    listAccessPreviewFindings_filter,
    listAccessPreviewFindings_maxResults,
    listAccessPreviewFindings_accessPreviewId,
    listAccessPreviewFindings_analyzerArn,
    listAccessPreviewFindingsResponse_nextToken,
    listAccessPreviewFindingsResponse_httpStatus,
    listAccessPreviewFindingsResponse_findings,

    -- ** GetArchiveRule
    getArchiveRule_analyzerName,
    getArchiveRule_ruleName,
    getArchiveRuleResponse_httpStatus,
    getArchiveRuleResponse_archiveRule,

    -- ** CreateAnalyzer
    createAnalyzer_clientToken,
    createAnalyzer_archiveRules,
    createAnalyzer_tags,
    createAnalyzer_analyzerName,
    createAnalyzer_type,
    createAnalyzerResponse_arn,
    createAnalyzerResponse_httpStatus,

    -- ** ListArchiveRules
    listArchiveRules_nextToken,
    listArchiveRules_maxResults,
    listArchiveRules_analyzerName,
    listArchiveRulesResponse_nextToken,
    listArchiveRulesResponse_httpStatus,
    listArchiveRulesResponse_archiveRules,

    -- ** CreateArchiveRule
    createArchiveRule_clientToken,
    createArchiveRule_analyzerName,
    createArchiveRule_filter,
    createArchiveRule_ruleName,

    -- ** CancelPolicyGeneration
    cancelPolicyGeneration_jobId,
    cancelPolicyGenerationResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ApplyArchiveRule
    applyArchiveRule_clientToken,
    applyArchiveRule_analyzerArn,
    applyArchiveRule_ruleName,

    -- ** GetAnalyzer
    getAnalyzer_analyzerName,
    getAnalyzerResponse_httpStatus,
    getAnalyzerResponse_analyzer,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** GetFinding
    getFinding_analyzerArn,
    getFinding_id,
    getFindingResponse_finding,
    getFindingResponse_httpStatus,

    -- ** GetGeneratedPolicy
    getGeneratedPolicy_includeServiceLevelTemplate,
    getGeneratedPolicy_includeResourcePlaceholders,
    getGeneratedPolicy_jobId,
    getGeneratedPolicyResponse_httpStatus,
    getGeneratedPolicyResponse_generatedPolicyResult,
    getGeneratedPolicyResponse_jobDetails,

    -- * Types

    -- ** AccessPreview
    accessPreview_statusReason,
    accessPreview_analyzerArn,
    accessPreview_configurations,
    accessPreview_createdAt,
    accessPreview_id,
    accessPreview_status,

    -- ** AccessPreviewFinding
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

    -- ** AccessPreviewStatusReason
    accessPreviewStatusReason_code,

    -- ** AccessPreviewSummary
    accessPreviewSummary_statusReason,
    accessPreviewSummary_analyzerArn,
    accessPreviewSummary_createdAt,
    accessPreviewSummary_id,
    accessPreviewSummary_status,

    -- ** AclGrantee
    aclGrantee_uri,
    aclGrantee_id,

    -- ** AnalyzedResource
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

    -- ** AnalyzedResourceSummary
    analyzedResourceSummary_resourceArn,
    analyzedResourceSummary_resourceOwnerAccount,
    analyzedResourceSummary_resourceType,

    -- ** AnalyzerSummary
    analyzerSummary_lastResourceAnalyzedAt,
    analyzerSummary_lastResourceAnalyzed,
    analyzerSummary_statusReason,
    analyzerSummary_tags,
    analyzerSummary_arn,
    analyzerSummary_createdAt,
    analyzerSummary_name,
    analyzerSummary_status,
    analyzerSummary_type,

    -- ** ArchiveRuleSummary
    archiveRuleSummary_createdAt,
    archiveRuleSummary_filter,
    archiveRuleSummary_ruleName,
    archiveRuleSummary_updatedAt,

    -- ** CloudTrailDetails
    cloudTrailDetails_endTime,
    cloudTrailDetails_accessRole,
    cloudTrailDetails_startTime,
    cloudTrailDetails_trails,

    -- ** CloudTrailProperties
    cloudTrailProperties_endTime,
    cloudTrailProperties_startTime,
    cloudTrailProperties_trailProperties,

    -- ** Configuration
    configuration_kmsKey,
    configuration_secretsManagerSecret,
    configuration_sqsQueue,
    configuration_s3Bucket,
    configuration_iamRole,

    -- ** Criterion
    criterion_eq,
    criterion_exists,
    criterion_neq,
    criterion_contains,

    -- ** Finding
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

    -- ** FindingSource
    findingSource_detail,
    findingSource_type,

    -- ** FindingSourceDetail
    findingSourceDetail_accessPointArn,

    -- ** FindingSummary
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

    -- ** GeneratedPolicy
    generatedPolicy_policy,

    -- ** GeneratedPolicyProperties
    generatedPolicyProperties_isComplete,
    generatedPolicyProperties_cloudTrailProperties,
    generatedPolicyProperties_principalArn,

    -- ** GeneratedPolicyResult
    generatedPolicyResult_generatedPolicies,
    generatedPolicyResult_properties,

    -- ** IamRoleConfiguration
    iamRoleConfiguration_trustPolicy,

    -- ** InlineArchiveRule
    inlineArchiveRule_filter,
    inlineArchiveRule_ruleName,

    -- ** InternetConfiguration

    -- ** JobDetails
    jobDetails_completedOn,
    jobDetails_jobError,
    jobDetails_jobId,
    jobDetails_startedOn,
    jobDetails_status,

    -- ** JobError
    jobError_code,
    jobError_message,

    -- ** KmsGrantConfiguration
    kmsGrantConfiguration_retiringPrincipal,
    kmsGrantConfiguration_constraints,
    kmsGrantConfiguration_granteePrincipal,
    kmsGrantConfiguration_issuingAccount,
    kmsGrantConfiguration_operations,

    -- ** KmsGrantConstraints
    kmsGrantConstraints_encryptionContextEquals,
    kmsGrantConstraints_encryptionContextSubset,

    -- ** KmsKeyConfiguration
    kmsKeyConfiguration_grants,
    kmsKeyConfiguration_keyPolicies,

    -- ** Location
    location_path,
    location_span,

    -- ** NetworkOriginConfiguration
    networkOriginConfiguration_internetConfiguration,
    networkOriginConfiguration_vpcConfiguration,

    -- ** PathElement
    pathElement_value,
    pathElement_substring,
    pathElement_key,
    pathElement_index,

    -- ** PolicyGeneration
    policyGeneration_completedOn,
    policyGeneration_jobId,
    policyGeneration_principalArn,
    policyGeneration_startedOn,
    policyGeneration_status,

    -- ** PolicyGenerationDetails
    policyGenerationDetails_principalArn,

    -- ** Position
    position_column,
    position_line,
    position_offset,

    -- ** S3AccessPointConfiguration
    s3AccessPointConfiguration_publicAccessBlock,
    s3AccessPointConfiguration_accessPointPolicy,
    s3AccessPointConfiguration_networkOrigin,

    -- ** S3BucketAclGrantConfiguration
    s3BucketAclGrantConfiguration_grantee,
    s3BucketAclGrantConfiguration_permission,

    -- ** S3BucketConfiguration
    s3BucketConfiguration_accessPoints,
    s3BucketConfiguration_bucketPublicAccessBlock,
    s3BucketConfiguration_bucketAclGrants,
    s3BucketConfiguration_bucketPolicy,

    -- ** S3PublicAccessBlockConfiguration
    s3PublicAccessBlockConfiguration_ignorePublicAcls,
    s3PublicAccessBlockConfiguration_restrictPublicBuckets,

    -- ** SecretsManagerSecretConfiguration
    secretsManagerSecretConfiguration_kmsKeyId,
    secretsManagerSecretConfiguration_secretPolicy,

    -- ** SortCriteria
    sortCriteria_orderBy,
    sortCriteria_attributeName,

    -- ** Span
    span_end,
    span_start,

    -- ** SqsQueueConfiguration
    sqsQueueConfiguration_queuePolicy,

    -- ** StatusReason
    statusReason_code,

    -- ** Substring
    substring_length,
    substring_start,

    -- ** Trail
    trail_regions,
    trail_allRegions,
    trail_cloudTrailArn,

    -- ** TrailProperties
    trailProperties_regions,
    trailProperties_allRegions,
    trailProperties_cloudTrailArn,

    -- ** ValidatePolicyFinding
    validatePolicyFinding_findingDetails,
    validatePolicyFinding_findingType,
    validatePolicyFinding_issueCode,
    validatePolicyFinding_learnMoreLink,
    validatePolicyFinding_locations,

    -- ** VpcConfiguration
    vpcConfiguration_vpcId,
  )
where

import Network.AWS.AccessAnalyzer.ApplyArchiveRule
import Network.AWS.AccessAnalyzer.CancelPolicyGeneration
import Network.AWS.AccessAnalyzer.CreateAccessPreview
import Network.AWS.AccessAnalyzer.CreateAnalyzer
import Network.AWS.AccessAnalyzer.CreateArchiveRule
import Network.AWS.AccessAnalyzer.DeleteAnalyzer
import Network.AWS.AccessAnalyzer.DeleteArchiveRule
import Network.AWS.AccessAnalyzer.GetAccessPreview
import Network.AWS.AccessAnalyzer.GetAnalyzedResource
import Network.AWS.AccessAnalyzer.GetAnalyzer
import Network.AWS.AccessAnalyzer.GetArchiveRule
import Network.AWS.AccessAnalyzer.GetFinding
import Network.AWS.AccessAnalyzer.GetGeneratedPolicy
import Network.AWS.AccessAnalyzer.ListAccessPreviewFindings
import Network.AWS.AccessAnalyzer.ListAccessPreviews
import Network.AWS.AccessAnalyzer.ListAnalyzedResources
import Network.AWS.AccessAnalyzer.ListAnalyzers
import Network.AWS.AccessAnalyzer.ListArchiveRules
import Network.AWS.AccessAnalyzer.ListFindings
import Network.AWS.AccessAnalyzer.ListPolicyGenerations
import Network.AWS.AccessAnalyzer.ListTagsForResource
import Network.AWS.AccessAnalyzer.StartPolicyGeneration
import Network.AWS.AccessAnalyzer.StartResourceScan
import Network.AWS.AccessAnalyzer.TagResource
import Network.AWS.AccessAnalyzer.Types.AccessPreview
import Network.AWS.AccessAnalyzer.Types.AccessPreviewFinding
import Network.AWS.AccessAnalyzer.Types.AccessPreviewStatusReason
import Network.AWS.AccessAnalyzer.Types.AccessPreviewSummary
import Network.AWS.AccessAnalyzer.Types.AclGrantee
import Network.AWS.AccessAnalyzer.Types.AnalyzedResource
import Network.AWS.AccessAnalyzer.Types.AnalyzedResourceSummary
import Network.AWS.AccessAnalyzer.Types.AnalyzerSummary
import Network.AWS.AccessAnalyzer.Types.ArchiveRuleSummary
import Network.AWS.AccessAnalyzer.Types.CloudTrailDetails
import Network.AWS.AccessAnalyzer.Types.CloudTrailProperties
import Network.AWS.AccessAnalyzer.Types.Configuration
import Network.AWS.AccessAnalyzer.Types.Criterion
import Network.AWS.AccessAnalyzer.Types.Finding
import Network.AWS.AccessAnalyzer.Types.FindingSource
import Network.AWS.AccessAnalyzer.Types.FindingSourceDetail
import Network.AWS.AccessAnalyzer.Types.FindingSummary
import Network.AWS.AccessAnalyzer.Types.GeneratedPolicy
import Network.AWS.AccessAnalyzer.Types.GeneratedPolicyProperties
import Network.AWS.AccessAnalyzer.Types.GeneratedPolicyResult
import Network.AWS.AccessAnalyzer.Types.IamRoleConfiguration
import Network.AWS.AccessAnalyzer.Types.InlineArchiveRule
import Network.AWS.AccessAnalyzer.Types.InternetConfiguration
import Network.AWS.AccessAnalyzer.Types.JobDetails
import Network.AWS.AccessAnalyzer.Types.JobError
import Network.AWS.AccessAnalyzer.Types.KmsGrantConfiguration
import Network.AWS.AccessAnalyzer.Types.KmsGrantConstraints
import Network.AWS.AccessAnalyzer.Types.KmsKeyConfiguration
import Network.AWS.AccessAnalyzer.Types.Location
import Network.AWS.AccessAnalyzer.Types.NetworkOriginConfiguration
import Network.AWS.AccessAnalyzer.Types.PathElement
import Network.AWS.AccessAnalyzer.Types.PolicyGeneration
import Network.AWS.AccessAnalyzer.Types.PolicyGenerationDetails
import Network.AWS.AccessAnalyzer.Types.Position
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
import Network.AWS.AccessAnalyzer.Types.ValidatePolicyFinding
import Network.AWS.AccessAnalyzer.Types.VpcConfiguration
import Network.AWS.AccessAnalyzer.UntagResource
import Network.AWS.AccessAnalyzer.UpdateArchiveRule
import Network.AWS.AccessAnalyzer.UpdateFindings
import Network.AWS.AccessAnalyzer.ValidatePolicy
