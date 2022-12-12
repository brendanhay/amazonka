{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ECR.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Lens
  ( -- * Operations

    -- ** BatchCheckLayerAvailability
    batchCheckLayerAvailability_registryId,
    batchCheckLayerAvailability_repositoryName,
    batchCheckLayerAvailability_layerDigests,
    batchCheckLayerAvailabilityResponse_failures,
    batchCheckLayerAvailabilityResponse_layers,
    batchCheckLayerAvailabilityResponse_httpStatus,

    -- ** BatchDeleteImage
    batchDeleteImage_registryId,
    batchDeleteImage_repositoryName,
    batchDeleteImage_imageIds,
    batchDeleteImageResponse_failures,
    batchDeleteImageResponse_imageIds,
    batchDeleteImageResponse_httpStatus,

    -- ** BatchGetImage
    batchGetImage_acceptedMediaTypes,
    batchGetImage_registryId,
    batchGetImage_repositoryName,
    batchGetImage_imageIds,
    batchGetImageResponse_failures,
    batchGetImageResponse_images,
    batchGetImageResponse_httpStatus,

    -- ** BatchGetRepositoryScanningConfiguration
    batchGetRepositoryScanningConfiguration_repositoryNames,
    batchGetRepositoryScanningConfigurationResponse_failures,
    batchGetRepositoryScanningConfigurationResponse_scanningConfigurations,
    batchGetRepositoryScanningConfigurationResponse_httpStatus,

    -- ** CompleteLayerUpload
    completeLayerUpload_registryId,
    completeLayerUpload_repositoryName,
    completeLayerUpload_uploadId,
    completeLayerUpload_layerDigests,
    completeLayerUploadResponse_layerDigest,
    completeLayerUploadResponse_registryId,
    completeLayerUploadResponse_repositoryName,
    completeLayerUploadResponse_uploadId,
    completeLayerUploadResponse_httpStatus,

    -- ** CreatePullThroughCacheRule
    createPullThroughCacheRule_registryId,
    createPullThroughCacheRule_ecrRepositoryPrefix,
    createPullThroughCacheRule_upstreamRegistryUrl,
    createPullThroughCacheRuleResponse_createdAt,
    createPullThroughCacheRuleResponse_ecrRepositoryPrefix,
    createPullThroughCacheRuleResponse_registryId,
    createPullThroughCacheRuleResponse_upstreamRegistryUrl,
    createPullThroughCacheRuleResponse_httpStatus,

    -- ** CreateRepository
    createRepository_encryptionConfiguration,
    createRepository_imageScanningConfiguration,
    createRepository_imageTagMutability,
    createRepository_registryId,
    createRepository_tags,
    createRepository_repositoryName,
    createRepositoryResponse_repository,
    createRepositoryResponse_httpStatus,

    -- ** DeleteLifecyclePolicy
    deleteLifecyclePolicy_registryId,
    deleteLifecyclePolicy_repositoryName,
    deleteLifecyclePolicyResponse_lastEvaluatedAt,
    deleteLifecyclePolicyResponse_lifecyclePolicyText,
    deleteLifecyclePolicyResponse_registryId,
    deleteLifecyclePolicyResponse_repositoryName,
    deleteLifecyclePolicyResponse_httpStatus,

    -- ** DeletePullThroughCacheRule
    deletePullThroughCacheRule_registryId,
    deletePullThroughCacheRule_ecrRepositoryPrefix,
    deletePullThroughCacheRuleResponse_createdAt,
    deletePullThroughCacheRuleResponse_ecrRepositoryPrefix,
    deletePullThroughCacheRuleResponse_registryId,
    deletePullThroughCacheRuleResponse_upstreamRegistryUrl,
    deletePullThroughCacheRuleResponse_httpStatus,

    -- ** DeleteRegistryPolicy
    deleteRegistryPolicyResponse_policyText,
    deleteRegistryPolicyResponse_registryId,
    deleteRegistryPolicyResponse_httpStatus,

    -- ** DeleteRepository
    deleteRepository_force,
    deleteRepository_registryId,
    deleteRepository_repositoryName,
    deleteRepositoryResponse_repository,
    deleteRepositoryResponse_httpStatus,

    -- ** DeleteRepositoryPolicy
    deleteRepositoryPolicy_registryId,
    deleteRepositoryPolicy_repositoryName,
    deleteRepositoryPolicyResponse_policyText,
    deleteRepositoryPolicyResponse_registryId,
    deleteRepositoryPolicyResponse_repositoryName,
    deleteRepositoryPolicyResponse_httpStatus,

    -- ** DescribeImageReplicationStatus
    describeImageReplicationStatus_registryId,
    describeImageReplicationStatus_repositoryName,
    describeImageReplicationStatus_imageId,
    describeImageReplicationStatusResponse_imageId,
    describeImageReplicationStatusResponse_replicationStatuses,
    describeImageReplicationStatusResponse_repositoryName,
    describeImageReplicationStatusResponse_httpStatus,

    -- ** DescribeImageScanFindings
    describeImageScanFindings_maxResults,
    describeImageScanFindings_nextToken,
    describeImageScanFindings_registryId,
    describeImageScanFindings_repositoryName,
    describeImageScanFindings_imageId,
    describeImageScanFindingsResponse_imageId,
    describeImageScanFindingsResponse_imageScanFindings,
    describeImageScanFindingsResponse_imageScanStatus,
    describeImageScanFindingsResponse_nextToken,
    describeImageScanFindingsResponse_registryId,
    describeImageScanFindingsResponse_repositoryName,
    describeImageScanFindingsResponse_httpStatus,

    -- ** DescribeImages
    describeImages_filter,
    describeImages_imageIds,
    describeImages_maxResults,
    describeImages_nextToken,
    describeImages_registryId,
    describeImages_repositoryName,
    describeImagesResponse_imageDetails,
    describeImagesResponse_nextToken,
    describeImagesResponse_httpStatus,

    -- ** DescribePullThroughCacheRules
    describePullThroughCacheRules_ecrRepositoryPrefixes,
    describePullThroughCacheRules_maxResults,
    describePullThroughCacheRules_nextToken,
    describePullThroughCacheRules_registryId,
    describePullThroughCacheRulesResponse_nextToken,
    describePullThroughCacheRulesResponse_pullThroughCacheRules,
    describePullThroughCacheRulesResponse_httpStatus,

    -- ** DescribeRegistry
    describeRegistryResponse_registryId,
    describeRegistryResponse_replicationConfiguration,
    describeRegistryResponse_httpStatus,

    -- ** DescribeRepositories
    describeRepositories_maxResults,
    describeRepositories_nextToken,
    describeRepositories_registryId,
    describeRepositories_repositoryNames,
    describeRepositoriesResponse_nextToken,
    describeRepositoriesResponse_repositories,
    describeRepositoriesResponse_httpStatus,

    -- ** GetAuthorizationToken
    getAuthorizationToken_registryIds,
    getAuthorizationTokenResponse_authorizationData,
    getAuthorizationTokenResponse_httpStatus,

    -- ** GetDownloadUrlForLayer
    getDownloadUrlForLayer_registryId,
    getDownloadUrlForLayer_repositoryName,
    getDownloadUrlForLayer_layerDigest,
    getDownloadUrlForLayerResponse_downloadUrl,
    getDownloadUrlForLayerResponse_layerDigest,
    getDownloadUrlForLayerResponse_httpStatus,

    -- ** GetLifecyclePolicy
    getLifecyclePolicy_registryId,
    getLifecyclePolicy_repositoryName,
    getLifecyclePolicyResponse_lastEvaluatedAt,
    getLifecyclePolicyResponse_lifecyclePolicyText,
    getLifecyclePolicyResponse_registryId,
    getLifecyclePolicyResponse_repositoryName,
    getLifecyclePolicyResponse_httpStatus,

    -- ** GetLifecyclePolicyPreview
    getLifecyclePolicyPreview_filter,
    getLifecyclePolicyPreview_imageIds,
    getLifecyclePolicyPreview_maxResults,
    getLifecyclePolicyPreview_nextToken,
    getLifecyclePolicyPreview_registryId,
    getLifecyclePolicyPreview_repositoryName,
    getLifecyclePolicyPreviewResponse_lifecyclePolicyText,
    getLifecyclePolicyPreviewResponse_nextToken,
    getLifecyclePolicyPreviewResponse_previewResults,
    getLifecyclePolicyPreviewResponse_registryId,
    getLifecyclePolicyPreviewResponse_repositoryName,
    getLifecyclePolicyPreviewResponse_status,
    getLifecyclePolicyPreviewResponse_summary,
    getLifecyclePolicyPreviewResponse_httpStatus,

    -- ** GetRegistryPolicy
    getRegistryPolicyResponse_policyText,
    getRegistryPolicyResponse_registryId,
    getRegistryPolicyResponse_httpStatus,

    -- ** GetRegistryScanningConfiguration
    getRegistryScanningConfigurationResponse_registryId,
    getRegistryScanningConfigurationResponse_scanningConfiguration,
    getRegistryScanningConfigurationResponse_httpStatus,

    -- ** GetRepositoryPolicy
    getRepositoryPolicy_registryId,
    getRepositoryPolicy_repositoryName,
    getRepositoryPolicyResponse_policyText,
    getRepositoryPolicyResponse_registryId,
    getRepositoryPolicyResponse_repositoryName,
    getRepositoryPolicyResponse_httpStatus,

    -- ** InitiateLayerUpload
    initiateLayerUpload_registryId,
    initiateLayerUpload_repositoryName,
    initiateLayerUploadResponse_partSize,
    initiateLayerUploadResponse_uploadId,
    initiateLayerUploadResponse_httpStatus,

    -- ** ListImages
    listImages_filter,
    listImages_maxResults,
    listImages_nextToken,
    listImages_registryId,
    listImages_repositoryName,
    listImagesResponse_imageIds,
    listImagesResponse_nextToken,
    listImagesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutImage
    putImage_imageDigest,
    putImage_imageManifestMediaType,
    putImage_imageTag,
    putImage_registryId,
    putImage_repositoryName,
    putImage_imageManifest,
    putImageResponse_image,
    putImageResponse_httpStatus,

    -- ** PutImageScanningConfiguration
    putImageScanningConfiguration_registryId,
    putImageScanningConfiguration_repositoryName,
    putImageScanningConfiguration_imageScanningConfiguration,
    putImageScanningConfigurationResponse_imageScanningConfiguration,
    putImageScanningConfigurationResponse_registryId,
    putImageScanningConfigurationResponse_repositoryName,
    putImageScanningConfigurationResponse_httpStatus,

    -- ** PutImageTagMutability
    putImageTagMutability_registryId,
    putImageTagMutability_repositoryName,
    putImageTagMutability_imageTagMutability,
    putImageTagMutabilityResponse_imageTagMutability,
    putImageTagMutabilityResponse_registryId,
    putImageTagMutabilityResponse_repositoryName,
    putImageTagMutabilityResponse_httpStatus,

    -- ** PutLifecyclePolicy
    putLifecyclePolicy_registryId,
    putLifecyclePolicy_repositoryName,
    putLifecyclePolicy_lifecyclePolicyText,
    putLifecyclePolicyResponse_lifecyclePolicyText,
    putLifecyclePolicyResponse_registryId,
    putLifecyclePolicyResponse_repositoryName,
    putLifecyclePolicyResponse_httpStatus,

    -- ** PutRegistryPolicy
    putRegistryPolicy_policyText,
    putRegistryPolicyResponse_policyText,
    putRegistryPolicyResponse_registryId,
    putRegistryPolicyResponse_httpStatus,

    -- ** PutRegistryScanningConfiguration
    putRegistryScanningConfiguration_rules,
    putRegistryScanningConfiguration_scanType,
    putRegistryScanningConfigurationResponse_registryScanningConfiguration,
    putRegistryScanningConfigurationResponse_httpStatus,

    -- ** PutReplicationConfiguration
    putReplicationConfiguration_replicationConfiguration,
    putReplicationConfigurationResponse_replicationConfiguration,
    putReplicationConfigurationResponse_httpStatus,

    -- ** SetRepositoryPolicy
    setRepositoryPolicy_force,
    setRepositoryPolicy_registryId,
    setRepositoryPolicy_repositoryName,
    setRepositoryPolicy_policyText,
    setRepositoryPolicyResponse_policyText,
    setRepositoryPolicyResponse_registryId,
    setRepositoryPolicyResponse_repositoryName,
    setRepositoryPolicyResponse_httpStatus,

    -- ** StartImageScan
    startImageScan_registryId,
    startImageScan_repositoryName,
    startImageScan_imageId,
    startImageScanResponse_imageId,
    startImageScanResponse_imageScanStatus,
    startImageScanResponse_registryId,
    startImageScanResponse_repositoryName,
    startImageScanResponse_httpStatus,

    -- ** StartLifecyclePolicyPreview
    startLifecyclePolicyPreview_lifecyclePolicyText,
    startLifecyclePolicyPreview_registryId,
    startLifecyclePolicyPreview_repositoryName,
    startLifecyclePolicyPreviewResponse_lifecyclePolicyText,
    startLifecyclePolicyPreviewResponse_registryId,
    startLifecyclePolicyPreviewResponse_repositoryName,
    startLifecyclePolicyPreviewResponse_status,
    startLifecyclePolicyPreviewResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UploadLayerPart
    uploadLayerPart_registryId,
    uploadLayerPart_repositoryName,
    uploadLayerPart_uploadId,
    uploadLayerPart_partFirstByte,
    uploadLayerPart_partLastByte,
    uploadLayerPart_layerPartBlob,
    uploadLayerPartResponse_lastByteReceived,
    uploadLayerPartResponse_registryId,
    uploadLayerPartResponse_repositoryName,
    uploadLayerPartResponse_uploadId,
    uploadLayerPartResponse_httpStatus,

    -- * Types

    -- ** Attribute
    attribute_value,
    attribute_key,

    -- ** AuthorizationData
    authorizationData_authorizationToken,
    authorizationData_expiresAt,
    authorizationData_proxyEndpoint,

    -- ** AwsEcrContainerImageDetails
    awsEcrContainerImageDetails_architecture,
    awsEcrContainerImageDetails_author,
    awsEcrContainerImageDetails_imageHash,
    awsEcrContainerImageDetails_imageTags,
    awsEcrContainerImageDetails_platform,
    awsEcrContainerImageDetails_pushedAt,
    awsEcrContainerImageDetails_registry,
    awsEcrContainerImageDetails_repositoryName,

    -- ** CvssScore
    cvssScore_baseScore,
    cvssScore_scoringVector,
    cvssScore_source,
    cvssScore_version,

    -- ** CvssScoreAdjustment
    cvssScoreAdjustment_metric,
    cvssScoreAdjustment_reason,

    -- ** CvssScoreDetails
    cvssScoreDetails_adjustments,
    cvssScoreDetails_score,
    cvssScoreDetails_scoreSource,
    cvssScoreDetails_scoringVector,
    cvssScoreDetails_version,

    -- ** DescribeImagesFilter
    describeImagesFilter_tagStatus,

    -- ** EncryptionConfiguration
    encryptionConfiguration_kmsKey,
    encryptionConfiguration_encryptionType,

    -- ** EnhancedImageScanFinding
    enhancedImageScanFinding_awsAccountId,
    enhancedImageScanFinding_description,
    enhancedImageScanFinding_findingArn,
    enhancedImageScanFinding_firstObservedAt,
    enhancedImageScanFinding_lastObservedAt,
    enhancedImageScanFinding_packageVulnerabilityDetails,
    enhancedImageScanFinding_remediation,
    enhancedImageScanFinding_resources,
    enhancedImageScanFinding_score,
    enhancedImageScanFinding_scoreDetails,
    enhancedImageScanFinding_severity,
    enhancedImageScanFinding_status,
    enhancedImageScanFinding_title,
    enhancedImageScanFinding_type,
    enhancedImageScanFinding_updatedAt,

    -- ** Image
    image_imageId,
    image_imageManifest,
    image_imageManifestMediaType,
    image_registryId,
    image_repositoryName,

    -- ** ImageDetail
    imageDetail_artifactMediaType,
    imageDetail_imageDigest,
    imageDetail_imageManifestMediaType,
    imageDetail_imagePushedAt,
    imageDetail_imageScanFindingsSummary,
    imageDetail_imageScanStatus,
    imageDetail_imageSizeInBytes,
    imageDetail_imageTags,
    imageDetail_lastRecordedPullTime,
    imageDetail_registryId,
    imageDetail_repositoryName,

    -- ** ImageFailure
    imageFailure_failureCode,
    imageFailure_failureReason,
    imageFailure_imageId,

    -- ** ImageIdentifier
    imageIdentifier_imageDigest,
    imageIdentifier_imageTag,

    -- ** ImageReplicationStatus
    imageReplicationStatus_failureCode,
    imageReplicationStatus_region,
    imageReplicationStatus_registryId,
    imageReplicationStatus_status,

    -- ** ImageScanFinding
    imageScanFinding_attributes,
    imageScanFinding_description,
    imageScanFinding_name,
    imageScanFinding_severity,
    imageScanFinding_uri,

    -- ** ImageScanFindings
    imageScanFindings_enhancedFindings,
    imageScanFindings_findingSeverityCounts,
    imageScanFindings_findings,
    imageScanFindings_imageScanCompletedAt,
    imageScanFindings_vulnerabilitySourceUpdatedAt,

    -- ** ImageScanFindingsSummary
    imageScanFindingsSummary_findingSeverityCounts,
    imageScanFindingsSummary_imageScanCompletedAt,
    imageScanFindingsSummary_vulnerabilitySourceUpdatedAt,

    -- ** ImageScanStatus
    imageScanStatus_description,
    imageScanStatus_status,

    -- ** ImageScanningConfiguration
    imageScanningConfiguration_scanOnPush,

    -- ** Layer
    layer_layerAvailability,
    layer_layerDigest,
    layer_layerSize,
    layer_mediaType,

    -- ** LayerFailure
    layerFailure_failureCode,
    layerFailure_failureReason,
    layerFailure_layerDigest,

    -- ** LifecyclePolicyPreviewFilter
    lifecyclePolicyPreviewFilter_tagStatus,

    -- ** LifecyclePolicyPreviewResult
    lifecyclePolicyPreviewResult_action,
    lifecyclePolicyPreviewResult_appliedRulePriority,
    lifecyclePolicyPreviewResult_imageDigest,
    lifecyclePolicyPreviewResult_imagePushedAt,
    lifecyclePolicyPreviewResult_imageTags,

    -- ** LifecyclePolicyPreviewSummary
    lifecyclePolicyPreviewSummary_expiringImageTotalCount,

    -- ** LifecyclePolicyRuleAction
    lifecyclePolicyRuleAction_type,

    -- ** ListImagesFilter
    listImagesFilter_tagStatus,

    -- ** PackageVulnerabilityDetails
    packageVulnerabilityDetails_cvss,
    packageVulnerabilityDetails_referenceUrls,
    packageVulnerabilityDetails_relatedVulnerabilities,
    packageVulnerabilityDetails_source,
    packageVulnerabilityDetails_sourceUrl,
    packageVulnerabilityDetails_vendorCreatedAt,
    packageVulnerabilityDetails_vendorSeverity,
    packageVulnerabilityDetails_vendorUpdatedAt,
    packageVulnerabilityDetails_vulnerabilityId,
    packageVulnerabilityDetails_vulnerablePackages,

    -- ** PullThroughCacheRule
    pullThroughCacheRule_createdAt,
    pullThroughCacheRule_ecrRepositoryPrefix,
    pullThroughCacheRule_registryId,
    pullThroughCacheRule_upstreamRegistryUrl,

    -- ** Recommendation
    recommendation_text,
    recommendation_url,

    -- ** RegistryScanningConfiguration
    registryScanningConfiguration_rules,
    registryScanningConfiguration_scanType,

    -- ** RegistryScanningRule
    registryScanningRule_scanFrequency,
    registryScanningRule_repositoryFilters,

    -- ** Remediation
    remediation_recommendation,

    -- ** ReplicationConfiguration
    replicationConfiguration_rules,

    -- ** ReplicationDestination
    replicationDestination_region,
    replicationDestination_registryId,

    -- ** ReplicationRule
    replicationRule_repositoryFilters,
    replicationRule_destinations,

    -- ** Repository
    repository_createdAt,
    repository_encryptionConfiguration,
    repository_imageScanningConfiguration,
    repository_imageTagMutability,
    repository_registryId,
    repository_repositoryArn,
    repository_repositoryName,
    repository_repositoryUri,

    -- ** RepositoryFilter
    repositoryFilter_filter,
    repositoryFilter_filterType,

    -- ** RepositoryScanningConfiguration
    repositoryScanningConfiguration_appliedScanFilters,
    repositoryScanningConfiguration_repositoryArn,
    repositoryScanningConfiguration_repositoryName,
    repositoryScanningConfiguration_scanFrequency,
    repositoryScanningConfiguration_scanOnPush,

    -- ** RepositoryScanningConfigurationFailure
    repositoryScanningConfigurationFailure_failureCode,
    repositoryScanningConfigurationFailure_failureReason,
    repositoryScanningConfigurationFailure_repositoryName,

    -- ** Resource
    resource_details,
    resource_id,
    resource_tags,
    resource_type,

    -- ** ResourceDetails
    resourceDetails_awsEcrContainerImage,

    -- ** ScanningRepositoryFilter
    scanningRepositoryFilter_filter,
    scanningRepositoryFilter_filterType,

    -- ** ScoreDetails
    scoreDetails_cvss,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** VulnerablePackage
    vulnerablePackage_arch,
    vulnerablePackage_epoch,
    vulnerablePackage_filePath,
    vulnerablePackage_name,
    vulnerablePackage_packageManager,
    vulnerablePackage_release,
    vulnerablePackage_sourceLayerHash,
    vulnerablePackage_version,
  )
where

import Amazonka.ECR.BatchCheckLayerAvailability
import Amazonka.ECR.BatchDeleteImage
import Amazonka.ECR.BatchGetImage
import Amazonka.ECR.BatchGetRepositoryScanningConfiguration
import Amazonka.ECR.CompleteLayerUpload
import Amazonka.ECR.CreatePullThroughCacheRule
import Amazonka.ECR.CreateRepository
import Amazonka.ECR.DeleteLifecyclePolicy
import Amazonka.ECR.DeletePullThroughCacheRule
import Amazonka.ECR.DeleteRegistryPolicy
import Amazonka.ECR.DeleteRepository
import Amazonka.ECR.DeleteRepositoryPolicy
import Amazonka.ECR.DescribeImageReplicationStatus
import Amazonka.ECR.DescribeImageScanFindings
import Amazonka.ECR.DescribeImages
import Amazonka.ECR.DescribePullThroughCacheRules
import Amazonka.ECR.DescribeRegistry
import Amazonka.ECR.DescribeRepositories
import Amazonka.ECR.GetAuthorizationToken
import Amazonka.ECR.GetDownloadUrlForLayer
import Amazonka.ECR.GetLifecyclePolicy
import Amazonka.ECR.GetLifecyclePolicyPreview
import Amazonka.ECR.GetRegistryPolicy
import Amazonka.ECR.GetRegistryScanningConfiguration
import Amazonka.ECR.GetRepositoryPolicy
import Amazonka.ECR.InitiateLayerUpload
import Amazonka.ECR.ListImages
import Amazonka.ECR.ListTagsForResource
import Amazonka.ECR.PutImage
import Amazonka.ECR.PutImageScanningConfiguration
import Amazonka.ECR.PutImageTagMutability
import Amazonka.ECR.PutLifecyclePolicy
import Amazonka.ECR.PutRegistryPolicy
import Amazonka.ECR.PutRegistryScanningConfiguration
import Amazonka.ECR.PutReplicationConfiguration
import Amazonka.ECR.SetRepositoryPolicy
import Amazonka.ECR.StartImageScan
import Amazonka.ECR.StartLifecyclePolicyPreview
import Amazonka.ECR.TagResource
import Amazonka.ECR.Types.Attribute
import Amazonka.ECR.Types.AuthorizationData
import Amazonka.ECR.Types.AwsEcrContainerImageDetails
import Amazonka.ECR.Types.CvssScore
import Amazonka.ECR.Types.CvssScoreAdjustment
import Amazonka.ECR.Types.CvssScoreDetails
import Amazonka.ECR.Types.DescribeImagesFilter
import Amazonka.ECR.Types.EncryptionConfiguration
import Amazonka.ECR.Types.EnhancedImageScanFinding
import Amazonka.ECR.Types.Image
import Amazonka.ECR.Types.ImageDetail
import Amazonka.ECR.Types.ImageFailure
import Amazonka.ECR.Types.ImageIdentifier
import Amazonka.ECR.Types.ImageReplicationStatus
import Amazonka.ECR.Types.ImageScanFinding
import Amazonka.ECR.Types.ImageScanFindings
import Amazonka.ECR.Types.ImageScanFindingsSummary
import Amazonka.ECR.Types.ImageScanStatus
import Amazonka.ECR.Types.ImageScanningConfiguration
import Amazonka.ECR.Types.Layer
import Amazonka.ECR.Types.LayerFailure
import Amazonka.ECR.Types.LifecyclePolicyPreviewFilter
import Amazonka.ECR.Types.LifecyclePolicyPreviewResult
import Amazonka.ECR.Types.LifecyclePolicyPreviewSummary
import Amazonka.ECR.Types.LifecyclePolicyRuleAction
import Amazonka.ECR.Types.ListImagesFilter
import Amazonka.ECR.Types.PackageVulnerabilityDetails
import Amazonka.ECR.Types.PullThroughCacheRule
import Amazonka.ECR.Types.Recommendation
import Amazonka.ECR.Types.RegistryScanningConfiguration
import Amazonka.ECR.Types.RegistryScanningRule
import Amazonka.ECR.Types.Remediation
import Amazonka.ECR.Types.ReplicationConfiguration
import Amazonka.ECR.Types.ReplicationDestination
import Amazonka.ECR.Types.ReplicationRule
import Amazonka.ECR.Types.Repository
import Amazonka.ECR.Types.RepositoryFilter
import Amazonka.ECR.Types.RepositoryScanningConfiguration
import Amazonka.ECR.Types.RepositoryScanningConfigurationFailure
import Amazonka.ECR.Types.Resource
import Amazonka.ECR.Types.ResourceDetails
import Amazonka.ECR.Types.ScanningRepositoryFilter
import Amazonka.ECR.Types.ScoreDetails
import Amazonka.ECR.Types.Tag
import Amazonka.ECR.Types.VulnerablePackage
import Amazonka.ECR.UntagResource
import Amazonka.ECR.UploadLayerPart
