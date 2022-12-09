{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AccessAnalyzer.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    createAnalyzer_archiveRules,
    createAnalyzer_clientToken,
    createAnalyzer_tags,
    createAnalyzer_analyzerName,
    createAnalyzer_type,
    createAnalyzerResponse_arn,
    createAnalyzerResponse_httpStatus,

    -- ** CreateArchiveRule
    createArchiveRule_clientToken,
    createArchiveRule_analyzerName,
    createArchiveRule_ruleName,
    createArchiveRule_filter,

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
    getGeneratedPolicy_includeResourcePlaceholders,
    getGeneratedPolicy_includeServiceLevelTemplate,
    getGeneratedPolicy_jobId,
    getGeneratedPolicyResponse_httpStatus,
    getGeneratedPolicyResponse_jobDetails,
    getGeneratedPolicyResponse_generatedPolicyResult,

    -- ** ListAccessPreviewFindings
    listAccessPreviewFindings_filter,
    listAccessPreviewFindings_maxResults,
    listAccessPreviewFindings_nextToken,
    listAccessPreviewFindings_accessPreviewId,
    listAccessPreviewFindings_analyzerArn,
    listAccessPreviewFindingsResponse_nextToken,
    listAccessPreviewFindingsResponse_httpStatus,
    listAccessPreviewFindingsResponse_findings,

    -- ** ListAccessPreviews
    listAccessPreviews_maxResults,
    listAccessPreviews_nextToken,
    listAccessPreviews_analyzerArn,
    listAccessPreviewsResponse_nextToken,
    listAccessPreviewsResponse_httpStatus,
    listAccessPreviewsResponse_accessPreviews,

    -- ** ListAnalyzedResources
    listAnalyzedResources_maxResults,
    listAnalyzedResources_nextToken,
    listAnalyzedResources_resourceType,
    listAnalyzedResources_analyzerArn,
    listAnalyzedResourcesResponse_nextToken,
    listAnalyzedResourcesResponse_httpStatus,
    listAnalyzedResourcesResponse_analyzedResources,

    -- ** ListAnalyzers
    listAnalyzers_maxResults,
    listAnalyzers_nextToken,
    listAnalyzers_type,
    listAnalyzersResponse_nextToken,
    listAnalyzersResponse_httpStatus,
    listAnalyzersResponse_analyzers,

    -- ** ListArchiveRules
    listArchiveRules_maxResults,
    listArchiveRules_nextToken,
    listArchiveRules_analyzerName,
    listArchiveRulesResponse_nextToken,
    listArchiveRulesResponse_httpStatus,
    listArchiveRulesResponse_archiveRules,

    -- ** ListFindings
    listFindings_filter,
    listFindings_maxResults,
    listFindings_nextToken,
    listFindings_sort,
    listFindings_analyzerArn,
    listFindingsResponse_nextToken,
    listFindingsResponse_httpStatus,
    listFindingsResponse_findings,

    -- ** ListPolicyGenerations
    listPolicyGenerations_maxResults,
    listPolicyGenerations_nextToken,
    listPolicyGenerations_principalArn,
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
    startResourceScan_resourceOwnerAccount,
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
    updateArchiveRule_ruleName,
    updateArchiveRule_filter,

    -- ** UpdateFindings
    updateFindings_clientToken,
    updateFindings_ids,
    updateFindings_resourceArn,
    updateFindings_analyzerArn,
    updateFindings_status,

    -- ** ValidatePolicy
    validatePolicy_locale,
    validatePolicy_maxResults,
    validatePolicy_nextToken,
    validatePolicy_validatePolicyResourceType,
    validatePolicy_policyDocument,
    validatePolicy_policyType,
    validatePolicyResponse_nextToken,
    validatePolicyResponse_httpStatus,
    validatePolicyResponse_findings,

    -- * Types

    -- ** AccessPreview
    accessPreview_statusReason,
    accessPreview_id,
    accessPreview_analyzerArn,
    accessPreview_configurations,
    accessPreview_createdAt,
    accessPreview_status,

    -- ** AccessPreviewFinding
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

    -- ** AccessPreviewStatusReason
    accessPreviewStatusReason_code,

    -- ** AccessPreviewSummary
    accessPreviewSummary_statusReason,
    accessPreviewSummary_id,
    accessPreviewSummary_analyzerArn,
    accessPreviewSummary_createdAt,
    accessPreviewSummary_status,

    -- ** AclGrantee
    aclGrantee_id,
    aclGrantee_uri,

    -- ** AnalyzedResource
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

    -- ** AnalyzedResourceSummary
    analyzedResourceSummary_resourceArn,
    analyzedResourceSummary_resourceOwnerAccount,
    analyzedResourceSummary_resourceType,

    -- ** AnalyzerSummary
    analyzerSummary_lastResourceAnalyzed,
    analyzerSummary_lastResourceAnalyzedAt,
    analyzerSummary_statusReason,
    analyzerSummary_tags,
    analyzerSummary_arn,
    analyzerSummary_name,
    analyzerSummary_type,
    analyzerSummary_createdAt,
    analyzerSummary_status,

    -- ** ArchiveRuleSummary
    archiveRuleSummary_ruleName,
    archiveRuleSummary_filter,
    archiveRuleSummary_createdAt,
    archiveRuleSummary_updatedAt,

    -- ** CloudTrailDetails
    cloudTrailDetails_endTime,
    cloudTrailDetails_trails,
    cloudTrailDetails_accessRole,
    cloudTrailDetails_startTime,

    -- ** CloudTrailProperties
    cloudTrailProperties_trailProperties,
    cloudTrailProperties_startTime,
    cloudTrailProperties_endTime,

    -- ** Configuration
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

    -- ** Criterion
    criterion_contains,
    criterion_eq,
    criterion_exists,
    criterion_neq,

    -- ** EbsSnapshotConfiguration
    ebsSnapshotConfiguration_groups,
    ebsSnapshotConfiguration_kmsKeyId,
    ebsSnapshotConfiguration_userIds,

    -- ** EcrRepositoryConfiguration
    ecrRepositoryConfiguration_repositoryPolicy,

    -- ** EfsFileSystemConfiguration
    efsFileSystemConfiguration_fileSystemPolicy,

    -- ** Finding
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

    -- ** FindingSource
    findingSource_detail,
    findingSource_type,

    -- ** FindingSourceDetail
    findingSourceDetail_accessPointAccount,
    findingSourceDetail_accessPointArn,

    -- ** FindingSummary
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

    -- ** GeneratedPolicy
    generatedPolicy_policy,

    -- ** GeneratedPolicyProperties
    generatedPolicyProperties_cloudTrailProperties,
    generatedPolicyProperties_isComplete,
    generatedPolicyProperties_principalArn,

    -- ** GeneratedPolicyResult
    generatedPolicyResult_generatedPolicies,
    generatedPolicyResult_properties,

    -- ** IamRoleConfiguration
    iamRoleConfiguration_trustPolicy,

    -- ** InlineArchiveRule
    inlineArchiveRule_ruleName,
    inlineArchiveRule_filter,

    -- ** InternetConfiguration

    -- ** JobDetails
    jobDetails_completedOn,
    jobDetails_jobError,
    jobDetails_jobId,
    jobDetails_status,
    jobDetails_startedOn,

    -- ** JobError
    jobError_code,
    jobError_message,

    -- ** KmsGrantConfiguration
    kmsGrantConfiguration_constraints,
    kmsGrantConfiguration_retiringPrincipal,
    kmsGrantConfiguration_operations,
    kmsGrantConfiguration_granteePrincipal,
    kmsGrantConfiguration_issuingAccount,

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
    pathElement_index,
    pathElement_key,
    pathElement_substring,
    pathElement_value,

    -- ** PolicyGeneration
    policyGeneration_completedOn,
    policyGeneration_jobId,
    policyGeneration_principalArn,
    policyGeneration_status,
    policyGeneration_startedOn,

    -- ** PolicyGenerationDetails
    policyGenerationDetails_principalArn,

    -- ** Position
    position_line,
    position_column,
    position_offset,

    -- ** RdsDbClusterSnapshotAttributeValue
    rdsDbClusterSnapshotAttributeValue_accountIds,

    -- ** RdsDbClusterSnapshotConfiguration
    rdsDbClusterSnapshotConfiguration_attributes,
    rdsDbClusterSnapshotConfiguration_kmsKeyId,

    -- ** RdsDbSnapshotAttributeValue
    rdsDbSnapshotAttributeValue_accountIds,

    -- ** RdsDbSnapshotConfiguration
    rdsDbSnapshotConfiguration_attributes,
    rdsDbSnapshotConfiguration_kmsKeyId,

    -- ** S3AccessPointConfiguration
    s3AccessPointConfiguration_accessPointPolicy,
    s3AccessPointConfiguration_networkOrigin,
    s3AccessPointConfiguration_publicAccessBlock,

    -- ** S3BucketAclGrantConfiguration
    s3BucketAclGrantConfiguration_permission,
    s3BucketAclGrantConfiguration_grantee,

    -- ** S3BucketConfiguration
    s3BucketConfiguration_accessPoints,
    s3BucketConfiguration_bucketAclGrants,
    s3BucketConfiguration_bucketPolicy,
    s3BucketConfiguration_bucketPublicAccessBlock,

    -- ** S3PublicAccessBlockConfiguration
    s3PublicAccessBlockConfiguration_ignorePublicAcls,
    s3PublicAccessBlockConfiguration_restrictPublicBuckets,

    -- ** SecretsManagerSecretConfiguration
    secretsManagerSecretConfiguration_kmsKeyId,
    secretsManagerSecretConfiguration_secretPolicy,

    -- ** SnsTopicConfiguration
    snsTopicConfiguration_topicPolicy,

    -- ** SortCriteria
    sortCriteria_attributeName,
    sortCriteria_orderBy,

    -- ** Span
    span_start,
    span_end,

    -- ** SqsQueueConfiguration
    sqsQueueConfiguration_queuePolicy,

    -- ** StatusReason
    statusReason_code,

    -- ** Substring
    substring_start,
    substring_length,

    -- ** Trail
    trail_allRegions,
    trail_regions,
    trail_cloudTrailArn,

    -- ** TrailProperties
    trailProperties_allRegions,
    trailProperties_regions,
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
import Amazonka.AccessAnalyzer.Types.EbsSnapshotConfiguration
import Amazonka.AccessAnalyzer.Types.EcrRepositoryConfiguration
import Amazonka.AccessAnalyzer.Types.EfsFileSystemConfiguration
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
import Amazonka.AccessAnalyzer.Types.RdsDbClusterSnapshotAttributeValue
import Amazonka.AccessAnalyzer.Types.RdsDbClusterSnapshotConfiguration
import Amazonka.AccessAnalyzer.Types.RdsDbSnapshotAttributeValue
import Amazonka.AccessAnalyzer.Types.RdsDbSnapshotConfiguration
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
import Amazonka.AccessAnalyzer.Types.ValidatePolicyFinding
import Amazonka.AccessAnalyzer.Types.VpcConfiguration
import Amazonka.AccessAnalyzer.UntagResource
import Amazonka.AccessAnalyzer.UpdateArchiveRule
import Amazonka.AccessAnalyzer.UpdateFindings
import Amazonka.AccessAnalyzer.ValidatePolicy
