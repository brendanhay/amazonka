{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Lens
  ( -- * Operations

    -- ** GetRepositoryPolicy
    getRepositoryPolicy_registryId,
    getRepositoryPolicy_repositoryName,
    getRepositoryPolicyResponse_registryId,
    getRepositoryPolicyResponse_repositoryName,
    getRepositoryPolicyResponse_policyText,
    getRepositoryPolicyResponse_httpStatus,

    -- ** PutImageScanningConfiguration
    putImageScanningConfiguration_registryId,
    putImageScanningConfiguration_repositoryName,
    putImageScanningConfiguration_imageScanningConfiguration,
    putImageScanningConfigurationResponse_registryId,
    putImageScanningConfigurationResponse_imageScanningConfiguration,
    putImageScanningConfigurationResponse_repositoryName,
    putImageScanningConfigurationResponse_httpStatus,

    -- ** PutLifecyclePolicy
    putLifecyclePolicy_registryId,
    putLifecyclePolicy_repositoryName,
    putLifecyclePolicy_lifecyclePolicyText,
    putLifecyclePolicyResponse_registryId,
    putLifecyclePolicyResponse_lifecyclePolicyText,
    putLifecyclePolicyResponse_repositoryName,
    putLifecyclePolicyResponse_httpStatus,

    -- ** DeleteLifecyclePolicy
    deleteLifecyclePolicy_registryId,
    deleteLifecyclePolicy_repositoryName,
    deleteLifecyclePolicyResponse_registryId,
    deleteLifecyclePolicyResponse_lastEvaluatedAt,
    deleteLifecyclePolicyResponse_lifecyclePolicyText,
    deleteLifecyclePolicyResponse_repositoryName,
    deleteLifecyclePolicyResponse_httpStatus,

    -- ** PutImageTagMutability
    putImageTagMutability_registryId,
    putImageTagMutability_repositoryName,
    putImageTagMutability_imageTagMutability,
    putImageTagMutabilityResponse_registryId,
    putImageTagMutabilityResponse_repositoryName,
    putImageTagMutabilityResponse_imageTagMutability,
    putImageTagMutabilityResponse_httpStatus,

    -- ** BatchDeleteImage
    batchDeleteImage_registryId,
    batchDeleteImage_repositoryName,
    batchDeleteImage_imageIds,
    batchDeleteImageResponse_failures,
    batchDeleteImageResponse_imageIds,
    batchDeleteImageResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** GetLifecyclePolicyPreview
    getLifecyclePolicyPreview_registryId,
    getLifecyclePolicyPreview_imageIds,
    getLifecyclePolicyPreview_nextToken,
    getLifecyclePolicyPreview_filter,
    getLifecyclePolicyPreview_maxResults,
    getLifecyclePolicyPreview_repositoryName,
    getLifecyclePolicyPreviewResponse_summary,
    getLifecyclePolicyPreviewResponse_status,
    getLifecyclePolicyPreviewResponse_registryId,
    getLifecyclePolicyPreviewResponse_lifecyclePolicyText,
    getLifecyclePolicyPreviewResponse_nextToken,
    getLifecyclePolicyPreviewResponse_repositoryName,
    getLifecyclePolicyPreviewResponse_previewResults,
    getLifecyclePolicyPreviewResponse_httpStatus,

    -- ** BatchCheckLayerAvailability
    batchCheckLayerAvailability_registryId,
    batchCheckLayerAvailability_repositoryName,
    batchCheckLayerAvailability_layerDigests,
    batchCheckLayerAvailabilityResponse_failures,
    batchCheckLayerAvailabilityResponse_layers,
    batchCheckLayerAvailabilityResponse_httpStatus,

    -- ** DescribeRegistry
    describeRegistryResponse_replicationConfiguration,
    describeRegistryResponse_registryId,
    describeRegistryResponse_httpStatus,

    -- ** DeleteRepositoryPolicy
    deleteRepositoryPolicy_registryId,
    deleteRepositoryPolicy_repositoryName,
    deleteRepositoryPolicyResponse_registryId,
    deleteRepositoryPolicyResponse_repositoryName,
    deleteRepositoryPolicyResponse_policyText,
    deleteRepositoryPolicyResponse_httpStatus,

    -- ** CreateRepository
    createRepository_registryId,
    createRepository_imageScanningConfiguration,
    createRepository_encryptionConfiguration,
    createRepository_imageTagMutability,
    createRepository_tags,
    createRepository_repositoryName,
    createRepositoryResponse_repository,
    createRepositoryResponse_httpStatus,

    -- ** CompleteLayerUpload
    completeLayerUpload_registryId,
    completeLayerUpload_repositoryName,
    completeLayerUpload_uploadId,
    completeLayerUpload_layerDigests,
    completeLayerUploadResponse_registryId,
    completeLayerUploadResponse_layerDigest,
    completeLayerUploadResponse_repositoryName,
    completeLayerUploadResponse_uploadId,
    completeLayerUploadResponse_httpStatus,

    -- ** DescribeRepositories
    describeRepositories_registryId,
    describeRepositories_repositoryNames,
    describeRepositories_nextToken,
    describeRepositories_maxResults,
    describeRepositoriesResponse_repositories,
    describeRepositoriesResponse_nextToken,
    describeRepositoriesResponse_httpStatus,

    -- ** StartLifecyclePolicyPreview
    startLifecyclePolicyPreview_registryId,
    startLifecyclePolicyPreview_lifecyclePolicyText,
    startLifecyclePolicyPreview_repositoryName,
    startLifecyclePolicyPreviewResponse_status,
    startLifecyclePolicyPreviewResponse_registryId,
    startLifecyclePolicyPreviewResponse_lifecyclePolicyText,
    startLifecyclePolicyPreviewResponse_repositoryName,
    startLifecyclePolicyPreviewResponse_httpStatus,

    -- ** DeleteRegistryPolicy
    deleteRegistryPolicyResponse_registryId,
    deleteRegistryPolicyResponse_policyText,
    deleteRegistryPolicyResponse_httpStatus,

    -- ** PutRegistryPolicy
    putRegistryPolicy_policyText,
    putRegistryPolicyResponse_registryId,
    putRegistryPolicyResponse_policyText,
    putRegistryPolicyResponse_httpStatus,

    -- ** UploadLayerPart
    uploadLayerPart_registryId,
    uploadLayerPart_repositoryName,
    uploadLayerPart_uploadId,
    uploadLayerPart_partFirstByte,
    uploadLayerPart_partLastByte,
    uploadLayerPart_layerPartBlob,
    uploadLayerPartResponse_registryId,
    uploadLayerPartResponse_lastByteReceived,
    uploadLayerPartResponse_repositoryName,
    uploadLayerPartResponse_uploadId,
    uploadLayerPartResponse_httpStatus,

    -- ** DescribeImageReplicationStatus
    describeImageReplicationStatus_registryId,
    describeImageReplicationStatus_repositoryName,
    describeImageReplicationStatus_imageId,
    describeImageReplicationStatusResponse_imageId,
    describeImageReplicationStatusResponse_repositoryName,
    describeImageReplicationStatusResponse_replicationStatuses,
    describeImageReplicationStatusResponse_httpStatus,

    -- ** BatchGetImage
    batchGetImage_registryId,
    batchGetImage_acceptedMediaTypes,
    batchGetImage_repositoryName,
    batchGetImage_imageIds,
    batchGetImageResponse_images,
    batchGetImageResponse_failures,
    batchGetImageResponse_httpStatus,

    -- ** PutReplicationConfiguration
    putReplicationConfiguration_replicationConfiguration,
    putReplicationConfigurationResponse_replicationConfiguration,
    putReplicationConfigurationResponse_httpStatus,

    -- ** StartImageScan
    startImageScan_registryId,
    startImageScan_repositoryName,
    startImageScan_imageId,
    startImageScanResponse_registryId,
    startImageScanResponse_imageScanStatus,
    startImageScanResponse_imageId,
    startImageScanResponse_repositoryName,
    startImageScanResponse_httpStatus,

    -- ** GetLifecyclePolicy
    getLifecyclePolicy_registryId,
    getLifecyclePolicy_repositoryName,
    getLifecyclePolicyResponse_registryId,
    getLifecyclePolicyResponse_lastEvaluatedAt,
    getLifecyclePolicyResponse_lifecyclePolicyText,
    getLifecyclePolicyResponse_repositoryName,
    getLifecyclePolicyResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** SetRepositoryPolicy
    setRepositoryPolicy_force,
    setRepositoryPolicy_registryId,
    setRepositoryPolicy_repositoryName,
    setRepositoryPolicy_policyText,
    setRepositoryPolicyResponse_registryId,
    setRepositoryPolicyResponse_repositoryName,
    setRepositoryPolicyResponse_policyText,
    setRepositoryPolicyResponse_httpStatus,

    -- ** DescribeImageScanFindings
    describeImageScanFindings_registryId,
    describeImageScanFindings_nextToken,
    describeImageScanFindings_maxResults,
    describeImageScanFindings_repositoryName,
    describeImageScanFindings_imageId,
    describeImageScanFindingsResponse_registryId,
    describeImageScanFindingsResponse_imageScanFindings,
    describeImageScanFindingsResponse_imageScanStatus,
    describeImageScanFindingsResponse_nextToken,
    describeImageScanFindingsResponse_imageId,
    describeImageScanFindingsResponse_repositoryName,
    describeImageScanFindingsResponse_httpStatus,

    -- ** InitiateLayerUpload
    initiateLayerUpload_registryId,
    initiateLayerUpload_repositoryName,
    initiateLayerUploadResponse_partSize,
    initiateLayerUploadResponse_uploadId,
    initiateLayerUploadResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DeleteRepository
    deleteRepository_force,
    deleteRepository_registryId,
    deleteRepository_repositoryName,
    deleteRepositoryResponse_repository,
    deleteRepositoryResponse_httpStatus,

    -- ** GetRegistryPolicy
    getRegistryPolicyResponse_registryId,
    getRegistryPolicyResponse_policyText,
    getRegistryPolicyResponse_httpStatus,

    -- ** PutImage
    putImage_registryId,
    putImage_imageManifestMediaType,
    putImage_imageDigest,
    putImage_imageTag,
    putImage_repositoryName,
    putImage_imageManifest,
    putImageResponse_image,
    putImageResponse_httpStatus,

    -- ** ListImages
    listImages_registryId,
    listImages_nextToken,
    listImages_filter,
    listImages_maxResults,
    listImages_repositoryName,
    listImagesResponse_imageIds,
    listImagesResponse_nextToken,
    listImagesResponse_httpStatus,

    -- ** GetAuthorizationToken
    getAuthorizationToken_registryIds,
    getAuthorizationTokenResponse_authorizationData,
    getAuthorizationTokenResponse_httpStatus,

    -- ** GetDownloadUrlForLayer
    getDownloadUrlForLayer_registryId,
    getDownloadUrlForLayer_repositoryName,
    getDownloadUrlForLayer_layerDigest,
    getDownloadUrlForLayerResponse_layerDigest,
    getDownloadUrlForLayerResponse_downloadUrl,
    getDownloadUrlForLayerResponse_httpStatus,

    -- ** DescribeImages
    describeImages_registryId,
    describeImages_imageIds,
    describeImages_nextToken,
    describeImages_filter,
    describeImages_maxResults,
    describeImages_repositoryName,
    describeImagesResponse_imageDetails,
    describeImagesResponse_nextToken,
    describeImagesResponse_httpStatus,

    -- * Types

    -- ** Attribute
    attribute_value,
    attribute_key,

    -- ** AuthorizationData
    authorizationData_expiresAt,
    authorizationData_proxyEndpoint,
    authorizationData_authorizationToken,

    -- ** DescribeImagesFilter
    describeImagesFilter_tagStatus,

    -- ** EncryptionConfiguration
    encryptionConfiguration_kmsKey,
    encryptionConfiguration_encryptionType,

    -- ** Image
    image_registryId,
    image_imageManifestMediaType,
    image_imageId,
    image_repositoryName,
    image_imageManifest,

    -- ** ImageDetail
    imageDetail_registryId,
    imageDetail_imageTags,
    imageDetail_imageScanStatus,
    imageDetail_imageManifestMediaType,
    imageDetail_imageSizeInBytes,
    imageDetail_imageDigest,
    imageDetail_imageScanFindingsSummary,
    imageDetail_artifactMediaType,
    imageDetail_imagePushedAt,
    imageDetail_repositoryName,

    -- ** ImageFailure
    imageFailure_failureReason,
    imageFailure_failureCode,
    imageFailure_imageId,

    -- ** ImageIdentifier
    imageIdentifier_imageDigest,
    imageIdentifier_imageTag,

    -- ** ImageReplicationStatus
    imageReplicationStatus_status,
    imageReplicationStatus_failureCode,
    imageReplicationStatus_registryId,
    imageReplicationStatus_region,

    -- ** ImageScanFinding
    imageScanFinding_severity,
    imageScanFinding_uri,
    imageScanFinding_name,
    imageScanFinding_attributes,
    imageScanFinding_description,

    -- ** ImageScanFindings
    imageScanFindings_imageScanCompletedAt,
    imageScanFindings_findings,
    imageScanFindings_findingSeverityCounts,
    imageScanFindings_vulnerabilitySourceUpdatedAt,

    -- ** ImageScanFindingsSummary
    imageScanFindingsSummary_imageScanCompletedAt,
    imageScanFindingsSummary_findingSeverityCounts,
    imageScanFindingsSummary_vulnerabilitySourceUpdatedAt,

    -- ** ImageScanStatus
    imageScanStatus_status,
    imageScanStatus_description,

    -- ** ImageScanningConfiguration
    imageScanningConfiguration_scanOnPush,

    -- ** Layer
    layer_mediaType,
    layer_layerDigest,
    layer_layerSize,
    layer_layerAvailability,

    -- ** LayerFailure
    layerFailure_failureReason,
    layerFailure_failureCode,
    layerFailure_layerDigest,

    -- ** LifecyclePolicyPreviewFilter
    lifecyclePolicyPreviewFilter_tagStatus,

    -- ** LifecyclePolicyPreviewResult
    lifecyclePolicyPreviewResult_imageTags,
    lifecyclePolicyPreviewResult_action,
    lifecyclePolicyPreviewResult_imageDigest,
    lifecyclePolicyPreviewResult_imagePushedAt,
    lifecyclePolicyPreviewResult_appliedRulePriority,

    -- ** LifecyclePolicyPreviewSummary
    lifecyclePolicyPreviewSummary_expiringImageTotalCount,

    -- ** LifecyclePolicyRuleAction
    lifecyclePolicyRuleAction_type,

    -- ** ListImagesFilter
    listImagesFilter_tagStatus,

    -- ** ReplicationConfiguration
    replicationConfiguration_rules,

    -- ** ReplicationDestination
    replicationDestination_region,
    replicationDestination_registryId,

    -- ** ReplicationRule
    replicationRule_repositoryFilters,
    replicationRule_destinations,

    -- ** Repository
    repository_repositoryArn,
    repository_createdAt,
    repository_registryId,
    repository_imageScanningConfiguration,
    repository_repositoryUri,
    repository_encryptionConfiguration,
    repository_repositoryName,
    repository_imageTagMutability,

    -- ** RepositoryFilter
    repositoryFilter_filter,
    repositoryFilter_filterType,

    -- ** Tag
    tag_value,
    tag_key,
  )
where

import Network.AWS.ECR.BatchCheckLayerAvailability
import Network.AWS.ECR.BatchDeleteImage
import Network.AWS.ECR.BatchGetImage
import Network.AWS.ECR.CompleteLayerUpload
import Network.AWS.ECR.CreateRepository
import Network.AWS.ECR.DeleteLifecyclePolicy
import Network.AWS.ECR.DeleteRegistryPolicy
import Network.AWS.ECR.DeleteRepository
import Network.AWS.ECR.DeleteRepositoryPolicy
import Network.AWS.ECR.DescribeImageReplicationStatus
import Network.AWS.ECR.DescribeImageScanFindings
import Network.AWS.ECR.DescribeImages
import Network.AWS.ECR.DescribeRegistry
import Network.AWS.ECR.DescribeRepositories
import Network.AWS.ECR.GetAuthorizationToken
import Network.AWS.ECR.GetDownloadUrlForLayer
import Network.AWS.ECR.GetLifecyclePolicy
import Network.AWS.ECR.GetLifecyclePolicyPreview
import Network.AWS.ECR.GetRegistryPolicy
import Network.AWS.ECR.GetRepositoryPolicy
import Network.AWS.ECR.InitiateLayerUpload
import Network.AWS.ECR.ListImages
import Network.AWS.ECR.ListTagsForResource
import Network.AWS.ECR.PutImage
import Network.AWS.ECR.PutImageScanningConfiguration
import Network.AWS.ECR.PutImageTagMutability
import Network.AWS.ECR.PutLifecyclePolicy
import Network.AWS.ECR.PutRegistryPolicy
import Network.AWS.ECR.PutReplicationConfiguration
import Network.AWS.ECR.SetRepositoryPolicy
import Network.AWS.ECR.StartImageScan
import Network.AWS.ECR.StartLifecyclePolicyPreview
import Network.AWS.ECR.TagResource
import Network.AWS.ECR.Types.Attribute
import Network.AWS.ECR.Types.AuthorizationData
import Network.AWS.ECR.Types.DescribeImagesFilter
import Network.AWS.ECR.Types.EncryptionConfiguration
import Network.AWS.ECR.Types.Image
import Network.AWS.ECR.Types.ImageDetail
import Network.AWS.ECR.Types.ImageFailure
import Network.AWS.ECR.Types.ImageIdentifier
import Network.AWS.ECR.Types.ImageReplicationStatus
import Network.AWS.ECR.Types.ImageScanFinding
import Network.AWS.ECR.Types.ImageScanFindings
import Network.AWS.ECR.Types.ImageScanFindingsSummary
import Network.AWS.ECR.Types.ImageScanStatus
import Network.AWS.ECR.Types.ImageScanningConfiguration
import Network.AWS.ECR.Types.Layer
import Network.AWS.ECR.Types.LayerFailure
import Network.AWS.ECR.Types.LifecyclePolicyPreviewFilter
import Network.AWS.ECR.Types.LifecyclePolicyPreviewResult
import Network.AWS.ECR.Types.LifecyclePolicyPreviewSummary
import Network.AWS.ECR.Types.LifecyclePolicyRuleAction
import Network.AWS.ECR.Types.ListImagesFilter
import Network.AWS.ECR.Types.ReplicationConfiguration
import Network.AWS.ECR.Types.ReplicationDestination
import Network.AWS.ECR.Types.ReplicationRule
import Network.AWS.ECR.Types.Repository
import Network.AWS.ECR.Types.RepositoryFilter
import Network.AWS.ECR.Types.Tag
import Network.AWS.ECR.UntagResource
import Network.AWS.ECR.UploadLayerPart
