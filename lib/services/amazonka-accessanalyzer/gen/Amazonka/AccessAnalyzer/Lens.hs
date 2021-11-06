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
