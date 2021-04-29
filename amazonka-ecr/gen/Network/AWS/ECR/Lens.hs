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

    -- ** UploadLayerPart
    uploadLayerPart_registryId,
    uploadLayerPart_repositoryName,
    uploadLayerPart_uploadId,
    uploadLayerPart_partFirstByte,
    uploadLayerPart_partLastByte,
    uploadLayerPart_layerPartBlob,
    uploadLayerPartResponse_uploadId,
    uploadLayerPartResponse_registryId,
    uploadLayerPartResponse_repositoryName,
    uploadLayerPartResponse_lastByteReceived,
    uploadLayerPartResponse_httpStatus,

    -- ** PutLifecyclePolicy
    putLifecyclePolicy_registryId,
    putLifecyclePolicy_repositoryName,
    putLifecyclePolicy_lifecyclePolicyText,
    putLifecyclePolicyResponse_registryId,
    putLifecyclePolicyResponse_repositoryName,
    putLifecyclePolicyResponse_lifecyclePolicyText,
    putLifecyclePolicyResponse_httpStatus,

    -- ** PutRegistryPolicy
    putRegistryPolicy_policyText,
    putRegistryPolicyResponse_registryId,
    putRegistryPolicyResponse_policyText,
    putRegistryPolicyResponse_httpStatus,

    -- ** StartLifecyclePolicyPreview
    startLifecyclePolicyPreview_registryId,
    startLifecyclePolicyPreview_lifecyclePolicyText,
    startLifecyclePolicyPreview_repositoryName,
    startLifecyclePolicyPreviewResponse_status,
    startLifecyclePolicyPreviewResponse_registryId,
    startLifecyclePolicyPreviewResponse_repositoryName,
    startLifecyclePolicyPreviewResponse_lifecyclePolicyText,
    startLifecyclePolicyPreviewResponse_httpStatus,

    -- ** DescribeRepositories
    describeRepositories_nextToken,
    describeRepositories_maxResults,
    describeRepositories_repositoryNames,
    describeRepositories_registryId,
    describeRepositoriesResponse_nextToken,
    describeRepositoriesResponse_repositories,
    describeRepositoriesResponse_httpStatus,

    -- ** ListImages
    listImages_nextToken,
    listImages_maxResults,
    listImages_registryId,
    listImages_filter,
    listImages_repositoryName,
    listImagesResponse_nextToken,
    listImagesResponse_imageIds,
    listImagesResponse_httpStatus,

    -- ** PutImage
    putImage_imageDigest,
    putImage_registryId,
    putImage_imageTag,
    putImage_imageManifestMediaType,
    putImage_repositoryName,
    putImage_imageManifest,
    putImageResponse_image,
    putImageResponse_httpStatus,

    -- ** InitiateLayerUpload
    initiateLayerUpload_registryId,
    initiateLayerUpload_repositoryName,
    initiateLayerUploadResponse_uploadId,
    initiateLayerUploadResponse_partSize,
    initiateLayerUploadResponse_httpStatus,

    -- ** GetRegistryPolicy
    getRegistryPolicyResponse_registryId,
    getRegistryPolicyResponse_policyText,
    getRegistryPolicyResponse_httpStatus,

    -- ** DeleteRepositoryPolicy
    deleteRepositoryPolicy_registryId,
    deleteRepositoryPolicy_repositoryName,
    deleteRepositoryPolicyResponse_registryId,
    deleteRepositoryPolicyResponse_policyText,
    deleteRepositoryPolicyResponse_repositoryName,
    deleteRepositoryPolicyResponse_httpStatus,

    -- ** DescribeImageScanFindings
    describeImageScanFindings_nextToken,
    describeImageScanFindings_maxResults,
    describeImageScanFindings_registryId,
    describeImageScanFindings_repositoryName,
    describeImageScanFindings_imageId,
    describeImageScanFindingsResponse_nextToken,
    describeImageScanFindingsResponse_imageScanStatus,
    describeImageScanFindingsResponse_imageScanFindings,
    describeImageScanFindingsResponse_registryId,
    describeImageScanFindingsResponse_repositoryName,
    describeImageScanFindingsResponse_imageId,
    describeImageScanFindingsResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** SetRepositoryPolicy
    setRepositoryPolicy_registryId,
    setRepositoryPolicy_force,
    setRepositoryPolicy_repositoryName,
    setRepositoryPolicy_policyText,
    setRepositoryPolicyResponse_registryId,
    setRepositoryPolicyResponse_policyText,
    setRepositoryPolicyResponse_repositoryName,
    setRepositoryPolicyResponse_httpStatus,

    -- ** DescribeRegistry
    describeRegistryResponse_replicationConfiguration,
    describeRegistryResponse_registryId,
    describeRegistryResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** BatchDeleteImage
    batchDeleteImage_registryId,
    batchDeleteImage_repositoryName,
    batchDeleteImage_imageIds,
    batchDeleteImageResponse_imageIds,
    batchDeleteImageResponse_failures,
    batchDeleteImageResponse_httpStatus,

    -- ** PutImageScanningConfiguration
    putImageScanningConfiguration_registryId,
    putImageScanningConfiguration_repositoryName,
    putImageScanningConfiguration_imageScanningConfiguration,
    putImageScanningConfigurationResponse_registryId,
    putImageScanningConfigurationResponse_repositoryName,
    putImageScanningConfigurationResponse_imageScanningConfiguration,
    putImageScanningConfigurationResponse_httpStatus,

    -- ** DeleteLifecyclePolicy
    deleteLifecyclePolicy_registryId,
    deleteLifecyclePolicy_repositoryName,
    deleteLifecyclePolicyResponse_registryId,
    deleteLifecyclePolicyResponse_repositoryName,
    deleteLifecyclePolicyResponse_lifecyclePolicyText,
    deleteLifecyclePolicyResponse_lastEvaluatedAt,
    deleteLifecyclePolicyResponse_httpStatus,

    -- ** DeleteRegistryPolicy
    deleteRegistryPolicyResponse_registryId,
    deleteRegistryPolicyResponse_policyText,
    deleteRegistryPolicyResponse_httpStatus,

    -- ** GetRepositoryPolicy
    getRepositoryPolicy_registryId,
    getRepositoryPolicy_repositoryName,
    getRepositoryPolicyResponse_registryId,
    getRepositoryPolicyResponse_policyText,
    getRepositoryPolicyResponse_repositoryName,
    getRepositoryPolicyResponse_httpStatus,

    -- ** DescribeImages
    describeImages_nextToken,
    describeImages_imageIds,
    describeImages_maxResults,
    describeImages_registryId,
    describeImages_filter,
    describeImages_repositoryName,
    describeImagesResponse_nextToken,
    describeImagesResponse_imageDetails,
    describeImagesResponse_httpStatus,

    -- ** GetDownloadUrlForLayer
    getDownloadUrlForLayer_registryId,
    getDownloadUrlForLayer_repositoryName,
    getDownloadUrlForLayer_layerDigest,
    getDownloadUrlForLayerResponse_downloadUrl,
    getDownloadUrlForLayerResponse_layerDigest,
    getDownloadUrlForLayerResponse_httpStatus,

    -- ** CompleteLayerUpload
    completeLayerUpload_registryId,
    completeLayerUpload_repositoryName,
    completeLayerUpload_uploadId,
    completeLayerUpload_layerDigests,
    completeLayerUploadResponse_uploadId,
    completeLayerUploadResponse_registryId,
    completeLayerUploadResponse_repositoryName,
    completeLayerUploadResponse_layerDigest,
    completeLayerUploadResponse_httpStatus,

    -- ** GetAuthorizationToken
    getAuthorizationToken_registryIds,
    getAuthorizationTokenResponse_authorizationData,
    getAuthorizationTokenResponse_httpStatus,

    -- ** CreateRepository
    createRepository_encryptionConfiguration,
    createRepository_tags,
    createRepository_imageScanningConfiguration,
    createRepository_imageTagMutability,
    createRepository_repositoryName,
    createRepositoryResponse_repository,
    createRepositoryResponse_httpStatus,

    -- ** DeleteRepository
    deleteRepository_registryId,
    deleteRepository_force,
    deleteRepository_repositoryName,
    deleteRepositoryResponse_repository,
    deleteRepositoryResponse_httpStatus,

    -- ** BatchCheckLayerAvailability
    batchCheckLayerAvailability_registryId,
    batchCheckLayerAvailability_repositoryName,
    batchCheckLayerAvailability_layerDigests,
    batchCheckLayerAvailabilityResponse_failures,
    batchCheckLayerAvailabilityResponse_layers,
    batchCheckLayerAvailabilityResponse_httpStatus,

    -- ** GetLifecyclePolicy
    getLifecyclePolicy_registryId,
    getLifecyclePolicy_repositoryName,
    getLifecyclePolicyResponse_registryId,
    getLifecyclePolicyResponse_repositoryName,
    getLifecyclePolicyResponse_lifecyclePolicyText,
    getLifecyclePolicyResponse_lastEvaluatedAt,
    getLifecyclePolicyResponse_httpStatus,

    -- ** StartImageScan
    startImageScan_registryId,
    startImageScan_repositoryName,
    startImageScan_imageId,
    startImageScanResponse_imageScanStatus,
    startImageScanResponse_registryId,
    startImageScanResponse_repositoryName,
    startImageScanResponse_imageId,
    startImageScanResponse_httpStatus,

    -- ** PutReplicationConfiguration
    putReplicationConfiguration_replicationConfiguration,
    putReplicationConfigurationResponse_replicationConfiguration,
    putReplicationConfigurationResponse_httpStatus,

    -- ** BatchGetImage
    batchGetImage_acceptedMediaTypes,
    batchGetImage_registryId,
    batchGetImage_repositoryName,
    batchGetImage_imageIds,
    batchGetImageResponse_images,
    batchGetImageResponse_failures,
    batchGetImageResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutImageTagMutability
    putImageTagMutability_registryId,
    putImageTagMutability_repositoryName,
    putImageTagMutability_imageTagMutability,
    putImageTagMutabilityResponse_registryId,
    putImageTagMutabilityResponse_repositoryName,
    putImageTagMutabilityResponse_imageTagMutability,
    putImageTagMutabilityResponse_httpStatus,

    -- ** GetLifecyclePolicyPreview
    getLifecyclePolicyPreview_nextToken,
    getLifecyclePolicyPreview_imageIds,
    getLifecyclePolicyPreview_maxResults,
    getLifecyclePolicyPreview_registryId,
    getLifecyclePolicyPreview_filter,
    getLifecyclePolicyPreview_repositoryName,
    getLifecyclePolicyPreviewResponse_nextToken,
    getLifecyclePolicyPreviewResponse_status,
    getLifecyclePolicyPreviewResponse_registryId,
    getLifecyclePolicyPreviewResponse_repositoryName,
    getLifecyclePolicyPreviewResponse_summary,
    getLifecyclePolicyPreviewResponse_lifecyclePolicyText,
    getLifecyclePolicyPreviewResponse_previewResults,
    getLifecyclePolicyPreviewResponse_httpStatus,

    -- * Types

    -- ** Attribute
    attribute_value,
    attribute_key,

    -- ** AuthorizationData
    authorizationData_proxyEndpoint,
    authorizationData_expiresAt,
    authorizationData_authorizationToken,

    -- ** DescribeImagesFilter
    describeImagesFilter_tagStatus,

    -- ** EncryptionConfiguration
    encryptionConfiguration_kmsKey,
    encryptionConfiguration_encryptionType,

    -- ** Image
    image_imageManifest,
    image_registryId,
    image_repositoryName,
    image_imageId,
    image_imageManifestMediaType,

    -- ** ImageDetail
    imageDetail_imageDigest,
    imageDetail_imageScanStatus,
    imageDetail_imageTags,
    imageDetail_registryId,
    imageDetail_repositoryName,
    imageDetail_artifactMediaType,
    imageDetail_imageSizeInBytes,
    imageDetail_imageManifestMediaType,
    imageDetail_imagePushedAt,
    imageDetail_imageScanFindingsSummary,

    -- ** ImageFailure
    imageFailure_failureCode,
    imageFailure_imageId,
    imageFailure_failureReason,

    -- ** ImageIdentifier
    imageIdentifier_imageDigest,
    imageIdentifier_imageTag,

    -- ** ImageScanFinding
    imageScanFinding_uri,
    imageScanFinding_severity,
    imageScanFinding_name,
    imageScanFinding_attributes,
    imageScanFinding_description,

    -- ** ImageScanFindings
    imageScanFindings_findings,
    imageScanFindings_imageScanCompletedAt,
    imageScanFindings_vulnerabilitySourceUpdatedAt,
    imageScanFindings_findingSeverityCounts,

    -- ** ImageScanFindingsSummary
    imageScanFindingsSummary_imageScanCompletedAt,
    imageScanFindingsSummary_vulnerabilitySourceUpdatedAt,
    imageScanFindingsSummary_findingSeverityCounts,

    -- ** ImageScanStatus
    imageScanStatus_status,
    imageScanStatus_description,

    -- ** ImageScanningConfiguration
    imageScanningConfiguration_scanOnPush,

    -- ** Layer
    layer_layerSize,
    layer_layerAvailability,
    layer_mediaType,
    layer_layerDigest,

    -- ** LayerFailure
    layerFailure_failureCode,
    layerFailure_failureReason,
    layerFailure_layerDigest,

    -- ** LifecyclePolicyPreviewFilter
    lifecyclePolicyPreviewFilter_tagStatus,

    -- ** LifecyclePolicyPreviewResult
    lifecyclePolicyPreviewResult_imageDigest,
    lifecyclePolicyPreviewResult_appliedRulePriority,
    lifecyclePolicyPreviewResult_imageTags,
    lifecyclePolicyPreviewResult_action,
    lifecyclePolicyPreviewResult_imagePushedAt,

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
    replicationRule_destinations,

    -- ** Repository
    repository_encryptionConfiguration,
    repository_repositoryUri,
    repository_registryId,
    repository_createdAt,
    repository_repositoryName,
    repository_repositoryArn,
    repository_imageScanningConfiguration,
    repository_imageTagMutability,

    -- ** Tag
    tag_key,
    tag_value,
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
import Network.AWS.ECR.Types.Tag
import Network.AWS.ECR.UntagResource
import Network.AWS.ECR.UploadLayerPart
