{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AccessAnalyzer.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Lens
  ( -- * Operations

    -- ** ApplyArchiveRule
    applyArchiveRule_clientToken,
    applyArchiveRule_analyzerArn,
    applyArchiveRule_ruleName,

    -- ** CancelPolicyGeneration
    cancelPolicyGeneration_jobId,
    cancelPolicyGenerationResponse_httpStatus,

    -- ** CreateAccessPreview
    createAccessPreview_clientToken,
    createAccessPreview_analyzerArn,
    createAccessPreview_configurations,
    createAccessPreviewResponse_httpStatus,
    createAccessPreviewResponse_id,

    -- ** CreateAnalyzer
    createAnalyzer_tags,
    createAnalyzer_clientToken,
    createAnalyzer_archiveRules,
    createAnalyzer_analyzerName,
    createAnalyzer_type,
    createAnalyzerResponse_arn,
    createAnalyzerResponse_httpStatus,

    -- ** CreateArchiveRule
    createArchiveRule_clientToken,
    createArchiveRule_analyzerName,
    createArchiveRule_filter,
    createArchiveRule_ruleName,

    -- ** DeleteAnalyzer
    deleteAnalyzer_clientToken,
    deleteAnalyzer_analyzerName,

    -- ** DeleteArchiveRule
    deleteArchiveRule_clientToken,
    deleteArchiveRule_analyzerName,
    deleteArchiveRule_ruleName,

    -- ** GetAccessPreview
    getAccessPreview_accessPreviewId,
    getAccessPreview_analyzerArn,
    getAccessPreviewResponse_httpStatus,
    getAccessPreviewResponse_accessPreview,

    -- ** GetAnalyzedResource
    getAnalyzedResource_analyzerArn,
    getAnalyzedResource_resourceArn,
    getAnalyzedResourceResponse_resource,
    getAnalyzedResourceResponse_httpStatus,

    -- ** GetAnalyzer
    getAnalyzer_analyzerName,
    getAnalyzerResponse_httpStatus,
    getAnalyzerResponse_analyzer,

    -- ** GetArchiveRule
    getArchiveRule_analyzerName,
    getArchiveRule_ruleName,
    getArchiveRuleResponse_httpStatus,
    getArchiveRuleResponse_archiveRule,

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

    -- ** ListAccessPreviewFindings
    listAccessPreviewFindings_nextToken,
    listAccessPreviewFindings_filter,
    listAccessPreviewFindings_maxResults,
    listAccessPreviewFindings_accessPreviewId,
    listAccessPreviewFindings_analyzerArn,
    listAccessPreviewFindingsResponse_nextToken,
    listAccessPreviewFindingsResponse_httpStatus,
    listAccessPreviewFindingsResponse_findings,

    -- ** ListAccessPreviews
    listAccessPreviews_nextToken,
    listAccessPreviews_maxResults,
    listAccessPreviews_analyzerArn,
    listAccessPreviewsResponse_nextToken,
    listAccessPreviewsResponse_httpStatus,
    listAccessPreviewsResponse_accessPreviews,

    -- ** ListAnalyzedResources
    listAnalyzedResources_resourceType,
    listAnalyzedResources_nextToken,
    listAnalyzedResources_maxResults,
    listAnalyzedResources_analyzerArn,
    listAnalyzedResourcesResponse_nextToken,
    listAnalyzedResourcesResponse_httpStatus,
    listAnalyzedResourcesResponse_analyzedResources,

    -- ** ListAnalyzers
    listAnalyzers_nextToken,
    listAnalyzers_type,
    listAnalyzers_maxResults,
    listAnalyzersResponse_nextToken,
    listAnalyzersResponse_httpStatus,
    listAnalyzersResponse_analyzers,

    -- ** ListArchiveRules
    listArchiveRules_nextToken,
    listArchiveRules_maxResults,
    listArchiveRules_analyzerName,
    listArchiveRulesResponse_nextToken,
    listArchiveRulesResponse_httpStatus,
    listArchiveRulesResponse_archiveRules,

    -- ** ListFindings
    listFindings_nextToken,
    listFindings_sort,
    listFindings_filter,
    listFindings_maxResults,
    listFindings_analyzerArn,
    listFindingsResponse_nextToken,
    listFindingsResponse_httpStatus,
    listFindingsResponse_findings,

    -- ** ListPolicyGenerations
    listPolicyGenerations_nextToken,
    listPolicyGenerations_principalArn,
    listPolicyGenerations_maxResults,
    listPolicyGenerationsResponse_nextToken,
    listPolicyGenerationsResponse_httpStatus,
    listPolicyGenerationsResponse_policyGenerations,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** StartPolicyGeneration
    startPolicyGeneration_clientToken,
    startPolicyGeneration_cloudTrailDetails,
    startPolicyGeneration_policyGenerationDetails,
    startPolicyGenerationResponse_httpStatus,
    startPolicyGenerationResponse_jobId,

    -- ** StartResourceScan
    startResourceScan_analyzerArn,
    startResourceScan_resourceArn,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateArchiveRule
    updateArchiveRule_clientToken,
    updateArchiveRule_analyzerName,
    updateArchiveRule_filter,
    updateArchiveRule_ruleName,

    -- ** UpdateFindings
    updateFindings_clientToken,
    updateFindings_ids,
    updateFindings_resourceArn,
    updateFindings_analyzerArn,
    updateFindings_status,

    -- ** ValidatePolicy
    validatePolicy_nextToken,
    validatePolicy_locale,
    validatePolicy_maxResults,
    validatePolicy_validatePolicyResourceType,
    validatePolicy_policyDocument,
    validatePolicy_policyType,
    validatePolicyResponse_nextToken,
    validatePolicyResponse_httpStatus,
    validatePolicyResponse_findings,

    -- * Types

    -- ** AccessPreview
    accessPreview_statusReason,
    accessPreview_analyzerArn,
    accessPreview_configurations,
    accessPreview_createdAt,
    accessPreview_id,
    accessPreview_status,

    -- ** AccessPreviewFinding
    accessPreviewFinding_principal,
    accessPreviewFinding_sources,
    accessPreviewFinding_isPublic,
    accessPreviewFinding_existingFindingId,
    accessPreviewFinding_condition,
    accessPreviewFinding_action,
    accessPreviewFinding_error,
    accessPreviewFinding_existingFindingStatus,
    accessPreviewFinding_resource,
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
    analyzedResource_sharedVia,
    analyzedResource_error,
    analyzedResource_actions,
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
    analyzerSummary_tags,
    analyzerSummary_lastResourceAnalyzedAt,
    analyzerSummary_lastResourceAnalyzed,
    analyzerSummary_statusReason,
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
    configuration_s3Bucket,
    configuration_iamRole,
    configuration_kmsKey,
    configuration_sqsQueue,
    configuration_secretsManagerSecret,

    -- ** Criterion
    criterion_neq,
    criterion_exists,
    criterion_contains,
    criterion_eq,

    -- ** Finding
    finding_principal,
    finding_sources,
    finding_isPublic,
    finding_action,
    finding_error,
    finding_resource,
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
    findingSummary_principal,
    findingSummary_sources,
    findingSummary_isPublic,
    findingSummary_action,
    findingSummary_error,
    findingSummary_resource,
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
    jobDetails_jobError,
    jobDetails_completedOn,
    jobDetails_jobId,
    jobDetails_startedOn,
    jobDetails_status,

    -- ** JobError
    jobError_code,
    jobError_message,

    -- ** KmsGrantConfiguration
    kmsGrantConfiguration_constraints,
    kmsGrantConfiguration_retiringPrincipal,
    kmsGrantConfiguration_granteePrincipal,
    kmsGrantConfiguration_issuingAccount,
    kmsGrantConfiguration_operations,

    -- ** KmsGrantConstraints
    kmsGrantConstraints_encryptionContextSubset,
    kmsGrantConstraints_encryptionContextEquals,

    -- ** KmsKeyConfiguration
    kmsKeyConfiguration_grants,
    kmsKeyConfiguration_keyPolicies,

    -- ** Location
    location_path,
    location_span,

    -- ** NetworkOriginConfiguration
    networkOriginConfiguration_vpcConfiguration,
    networkOriginConfiguration_internetConfiguration,

    -- ** PathElement
    pathElement_key,
    pathElement_index,
    pathElement_substring,
    pathElement_value,

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
    s3AccessPointConfiguration_networkOrigin,
    s3AccessPointConfiguration_accessPointPolicy,

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
    secretsManagerSecretConfiguration_secretPolicy,
    secretsManagerSecretConfiguration_kmsKeyId,

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
import Amazonka.AccessAnalyzer.Types.AccessPreview
import Amazonka.AccessAnalyzer.Types.AccessPreviewFinding
import Amazonka.AccessAnalyzer.Types.AccessPreviewStatusReason
import Amazonka.AccessAnalyzer.Types.AccessPreviewSummary
import Amazonka.AccessAnalyzer.Types.AclGrantee
import Amazonka.AccessAnalyzer.Types.AnalyzedResource
import Amazonka.AccessAnalyzer.Types.AnalyzedResourceSummary
import Amazonka.AccessAnalyzer.Types.AnalyzerSummary
import Amazonka.AccessAnalyzer.Types.ArchiveRuleSummary
import Amazonka.AccessAnalyzer.Types.CloudTrailDetails
import Amazonka.AccessAnalyzer.Types.CloudTrailProperties
import Amazonka.AccessAnalyzer.Types.Configuration
import Amazonka.AccessAnalyzer.Types.Criterion
import Amazonka.AccessAnalyzer.Types.Finding
import Amazonka.AccessAnalyzer.Types.FindingSource
import Amazonka.AccessAnalyzer.Types.FindingSourceDetail
import Amazonka.AccessAnalyzer.Types.FindingSummary
import Amazonka.AccessAnalyzer.Types.GeneratedPolicy
import Amazonka.AccessAnalyzer.Types.GeneratedPolicyProperties
import Amazonka.AccessAnalyzer.Types.GeneratedPolicyResult
import Amazonka.AccessAnalyzer.Types.IamRoleConfiguration
import Amazonka.AccessAnalyzer.Types.InlineArchiveRule
import Amazonka.AccessAnalyzer.Types.InternetConfiguration
import Amazonka.AccessAnalyzer.Types.JobDetails
import Amazonka.AccessAnalyzer.Types.JobError
import Amazonka.AccessAnalyzer.Types.KmsGrantConfiguration
import Amazonka.AccessAnalyzer.Types.KmsGrantConstraints
import Amazonka.AccessAnalyzer.Types.KmsKeyConfiguration
import Amazonka.AccessAnalyzer.Types.Location
import Amazonka.AccessAnalyzer.Types.NetworkOriginConfiguration
import Amazonka.AccessAnalyzer.Types.PathElement
import Amazonka.AccessAnalyzer.Types.PolicyGeneration
import Amazonka.AccessAnalyzer.Types.PolicyGenerationDetails
import Amazonka.AccessAnalyzer.Types.Position
import Amazonka.AccessAnalyzer.Types.S3AccessPointConfiguration
import Amazonka.AccessAnalyzer.Types.S3BucketAclGrantConfiguration
import Amazonka.AccessAnalyzer.Types.S3BucketConfiguration
import Amazonka.AccessAnalyzer.Types.S3PublicAccessBlockConfiguration
import Amazonka.AccessAnalyzer.Types.SecretsManagerSecretConfiguration
import Amazonka.AccessAnalyzer.Types.SortCriteria
import Amazonka.AccessAnalyzer.Types.Span
import Amazonka.AccessAnalyzer.Types.SqsQueueConfiguration
import Amazonka.AccessAnalyzer.Types.StatusReason
import Amazonka.AccessAnalyzer.Types.Substring
import Amazonka.AccessAnalyzer.Types.Trail
import Amazonka.AccessAnalyzer.Types.TrailProperties
import Amazonka.AccessAnalyzer.Types.ValidatePolicyFinding
import Amazonka.AccessAnalyzer.Types.VpcConfiguration
import Amazonka.AccessAnalyzer.UntagResource
import Amazonka.AccessAnalyzer.UpdateArchiveRule
import Amazonka.AccessAnalyzer.UpdateFindings
import Amazonka.AccessAnalyzer.ValidatePolicy
