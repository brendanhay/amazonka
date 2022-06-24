{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ECR.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    batchCheckLayerAvailabilityResponse_layers,
    batchCheckLayerAvailabilityResponse_failures,
    batchCheckLayerAvailabilityResponse_httpStatus,

    -- ** BatchDeleteImage
    batchDeleteImage_registryId,
    batchDeleteImage_repositoryName,
    batchDeleteImage_imageIds,
    batchDeleteImageResponse_imageIds,
    batchDeleteImageResponse_failures,
    batchDeleteImageResponse_httpStatus,

    -- ** BatchGetImage
    batchGetImage_acceptedMediaTypes,
    batchGetImage_registryId,
    batchGetImage_repositoryName,
    batchGetImage_imageIds,
    batchGetImageResponse_failures,
    batchGetImageResponse_images,
    batchGetImageResponse_httpStatus,

    -- ** CompleteLayerUpload
    completeLayerUpload_registryId,
    completeLayerUpload_repositoryName,
    completeLayerUpload_uploadId,
    completeLayerUpload_layerDigests,
    completeLayerUploadResponse_uploadId,
    completeLayerUploadResponse_repositoryName,
    completeLayerUploadResponse_layerDigest,
    completeLayerUploadResponse_registryId,
    completeLayerUploadResponse_httpStatus,

    -- ** CreateRepository
    createRepository_tags,
    createRepository_imageTagMutability,
    createRepository_encryptionConfiguration,
    createRepository_registryId,
    createRepository_imageScanningConfiguration,
    createRepository_repositoryName,
    createRepositoryResponse_repository,
    createRepositoryResponse_httpStatus,

    -- ** DeleteLifecyclePolicy
    deleteLifecyclePolicy_registryId,
    deleteLifecyclePolicy_repositoryName,
    deleteLifecyclePolicyResponse_lastEvaluatedAt,
    deleteLifecyclePolicyResponse_repositoryName,
    deleteLifecyclePolicyResponse_registryId,
    deleteLifecyclePolicyResponse_lifecyclePolicyText,
    deleteLifecyclePolicyResponse_httpStatus,

    -- ** DeleteRegistryPolicy
    deleteRegistryPolicyResponse_policyText,
    deleteRegistryPolicyResponse_registryId,
    deleteRegistryPolicyResponse_httpStatus,

    -- ** DeleteRepository
    deleteRepository_registryId,
    deleteRepository_force,
    deleteRepository_repositoryName,
    deleteRepositoryResponse_repository,
    deleteRepositoryResponse_httpStatus,

    -- ** DeleteRepositoryPolicy
    deleteRepositoryPolicy_registryId,
    deleteRepositoryPolicy_repositoryName,
    deleteRepositoryPolicyResponse_policyText,
    deleteRepositoryPolicyResponse_repositoryName,
    deleteRepositoryPolicyResponse_registryId,
    deleteRepositoryPolicyResponse_httpStatus,

    -- ** DescribeImageReplicationStatus
    describeImageReplicationStatus_registryId,
    describeImageReplicationStatus_repositoryName,
    describeImageReplicationStatus_imageId,
    describeImageReplicationStatusResponse_replicationStatuses,
    describeImageReplicationStatusResponse_repositoryName,
    describeImageReplicationStatusResponse_imageId,
    describeImageReplicationStatusResponse_httpStatus,

    -- ** DescribeImageScanFindings
    describeImageScanFindings_nextToken,
    describeImageScanFindings_maxResults,
    describeImageScanFindings_registryId,
    describeImageScanFindings_repositoryName,
    describeImageScanFindings_imageId,
    describeImageScanFindingsResponse_nextToken,
    describeImageScanFindingsResponse_repositoryName,
    describeImageScanFindingsResponse_registryId,
    describeImageScanFindingsResponse_imageScanStatus,
    describeImageScanFindingsResponse_imageScanFindings,
    describeImageScanFindingsResponse_imageId,
    describeImageScanFindingsResponse_httpStatus,

    -- ** DescribeImages
    describeImages_nextToken,
    describeImages_imageIds,
    describeImages_filter,
    describeImages_maxResults,
    describeImages_registryId,
    describeImages_repositoryName,
    describeImagesResponse_nextToken,
    describeImagesResponse_imageDetails,
    describeImagesResponse_httpStatus,

    -- ** DescribeRegistry
    describeRegistryResponse_replicationConfiguration,
    describeRegistryResponse_registryId,
    describeRegistryResponse_httpStatus,

    -- ** DescribeRepositories
    describeRepositories_nextToken,
    describeRepositories_maxResults,
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
    getLifecyclePolicyResponse_repositoryName,
    getLifecyclePolicyResponse_registryId,
    getLifecyclePolicyResponse_lifecyclePolicyText,
    getLifecyclePolicyResponse_httpStatus,

    -- ** GetLifecyclePolicyPreview
    getLifecyclePolicyPreview_nextToken,
    getLifecyclePolicyPreview_imageIds,
    getLifecyclePolicyPreview_filter,
    getLifecyclePolicyPreview_maxResults,
    getLifecyclePolicyPreview_registryId,
    getLifecyclePolicyPreview_repositoryName,
    getLifecyclePolicyPreviewResponse_nextToken,
    getLifecyclePolicyPreviewResponse_repositoryName,
    getLifecyclePolicyPreviewResponse_summary,
    getLifecyclePolicyPreviewResponse_status,
    getLifecyclePolicyPreviewResponse_registryId,
    getLifecyclePolicyPreviewResponse_previewResults,
    getLifecyclePolicyPreviewResponse_lifecyclePolicyText,
    getLifecyclePolicyPreviewResponse_httpStatus,

    -- ** GetRegistryPolicy
    getRegistryPolicyResponse_policyText,
    getRegistryPolicyResponse_registryId,
    getRegistryPolicyResponse_httpStatus,

    -- ** GetRepositoryPolicy
    getRepositoryPolicy_registryId,
    getRepositoryPolicy_repositoryName,
    getRepositoryPolicyResponse_policyText,
    getRepositoryPolicyResponse_repositoryName,
    getRepositoryPolicyResponse_registryId,
    getRepositoryPolicyResponse_httpStatus,

    -- ** InitiateLayerUpload
    initiateLayerUpload_registryId,
    initiateLayerUpload_repositoryName,
    initiateLayerUploadResponse_uploadId,
    initiateLayerUploadResponse_partSize,
    initiateLayerUploadResponse_httpStatus,

    -- ** ListImages
    listImages_nextToken,
    listImages_filter,
    listImages_maxResults,
    listImages_registryId,
    listImages_repositoryName,
    listImagesResponse_nextToken,
    listImagesResponse_imageIds,
    listImagesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutImage
    putImage_imageTag,
    putImage_registryId,
    putImage_imageManifestMediaType,
    putImage_imageDigest,
    putImage_repositoryName,
    putImage_imageManifest,
    putImageResponse_image,
    putImageResponse_httpStatus,

    -- ** PutImageScanningConfiguration
    putImageScanningConfiguration_registryId,
    putImageScanningConfiguration_repositoryName,
    putImageScanningConfiguration_imageScanningConfiguration,
    putImageScanningConfigurationResponse_repositoryName,
    putImageScanningConfigurationResponse_registryId,
    putImageScanningConfigurationResponse_imageScanningConfiguration,
    putImageScanningConfigurationResponse_httpStatus,

    -- ** PutImageTagMutability
    putImageTagMutability_registryId,
    putImageTagMutability_repositoryName,
    putImageTagMutability_imageTagMutability,
    putImageTagMutabilityResponse_repositoryName,
    putImageTagMutabilityResponse_imageTagMutability,
    putImageTagMutabilityResponse_registryId,
    putImageTagMutabilityResponse_httpStatus,

    -- ** PutLifecyclePolicy
    putLifecyclePolicy_registryId,
    putLifecyclePolicy_repositoryName,
    putLifecyclePolicy_lifecyclePolicyText,
    putLifecyclePolicyResponse_repositoryName,
    putLifecyclePolicyResponse_registryId,
    putLifecyclePolicyResponse_lifecyclePolicyText,
    putLifecyclePolicyResponse_httpStatus,

    -- ** PutRegistryPolicy
    putRegistryPolicy_policyText,
    putRegistryPolicyResponse_policyText,
    putRegistryPolicyResponse_registryId,
    putRegistryPolicyResponse_httpStatus,

    -- ** PutReplicationConfiguration
    putReplicationConfiguration_replicationConfiguration,
    putReplicationConfigurationResponse_replicationConfiguration,
    putReplicationConfigurationResponse_httpStatus,

    -- ** SetRepositoryPolicy
    setRepositoryPolicy_registryId,
    setRepositoryPolicy_force,
    setRepositoryPolicy_repositoryName,
    setRepositoryPolicy_policyText,
    setRepositoryPolicyResponse_policyText,
    setRepositoryPolicyResponse_repositoryName,
    setRepositoryPolicyResponse_registryId,
    setRepositoryPolicyResponse_httpStatus,

    -- ** StartImageScan
    startImageScan_registryId,
    startImageScan_repositoryName,
    startImageScan_imageId,
    startImageScanResponse_repositoryName,
    startImageScanResponse_registryId,
    startImageScanResponse_imageScanStatus,
    startImageScanResponse_imageId,
    startImageScanResponse_httpStatus,

    -- ** StartLifecyclePolicyPreview
    startLifecyclePolicyPreview_registryId,
    startLifecyclePolicyPreview_lifecyclePolicyText,
    startLifecyclePolicyPreview_repositoryName,
    startLifecyclePolicyPreviewResponse_repositoryName,
    startLifecyclePolicyPreviewResponse_status,
    startLifecyclePolicyPreviewResponse_registryId,
    startLifecyclePolicyPreviewResponse_lifecyclePolicyText,
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
    uploadLayerPartResponse_uploadId,
    uploadLayerPartResponse_repositoryName,
    uploadLayerPartResponse_registryId,
    uploadLayerPartResponse_lastByteReceived,
    uploadLayerPartResponse_httpStatus,

    -- * Types

    -- ** Attribute
    attribute_value,
    attribute_key,

    -- ** AuthorizationData
    authorizationData_expiresAt,
    authorizationData_authorizationToken,
    authorizationData_proxyEndpoint,

    -- ** DescribeImagesFilter
    describeImagesFilter_tagStatus,

    -- ** EncryptionConfiguration
    encryptionConfiguration_kmsKey,
    encryptionConfiguration_encryptionType,

    -- ** Image
    image_repositoryName,
    image_registryId,
    image_imageManifestMediaType,
    image_imageManifest,
    image_imageId,

    -- ** ImageDetail
    imageDetail_artifactMediaType,
    imageDetail_imagePushedAt,
    imageDetail_repositoryName,
    imageDetail_imageSizeInBytes,
    imageDetail_imageTags,
    imageDetail_registryId,
    imageDetail_imageManifestMediaType,
    imageDetail_imageDigest,
    imageDetail_imageScanStatus,
    imageDetail_imageScanFindingsSummary,

    -- ** ImageFailure
    imageFailure_failureCode,
    imageFailure_imageId,
    imageFailure_failureReason,

    -- ** ImageIdentifier
    imageIdentifier_imageTag,
    imageIdentifier_imageDigest,

    -- ** ImageReplicationStatus
    imageReplicationStatus_failureCode,
    imageReplicationStatus_status,
    imageReplicationStatus_region,
    imageReplicationStatus_registryId,

    -- ** ImageScanFinding
    imageScanFinding_severity,
    imageScanFinding_name,
    imageScanFinding_description,
    imageScanFinding_uri,
    imageScanFinding_attributes,

    -- ** ImageScanFindings
    imageScanFindings_findings,
    imageScanFindings_vulnerabilitySourceUpdatedAt,
    imageScanFindings_findingSeverityCounts,
    imageScanFindings_imageScanCompletedAt,

    -- ** ImageScanFindingsSummary
    imageScanFindingsSummary_vulnerabilitySourceUpdatedAt,
    imageScanFindingsSummary_findingSeverityCounts,
    imageScanFindingsSummary_imageScanCompletedAt,

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
    layerFailure_layerDigest,
    layerFailure_failureReason,

    -- ** LifecyclePolicyPreviewFilter
    lifecyclePolicyPreviewFilter_tagStatus,

    -- ** LifecyclePolicyPreviewResult
    lifecyclePolicyPreviewResult_appliedRulePriority,
    lifecyclePolicyPreviewResult_imagePushedAt,
    lifecyclePolicyPreviewResult_imageTags,
    lifecyclePolicyPreviewResult_action,
    lifecyclePolicyPreviewResult_imageDigest,

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
    repository_repositoryUri,
    repository_repositoryName,
    repository_imageTagMutability,
    repository_encryptionConfiguration,
    repository_registryId,
    repository_imageScanningConfiguration,
    repository_createdAt,

    -- ** RepositoryFilter
    repositoryFilter_filter,
    repositoryFilter_filterType,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Amazonka.ECR.BatchCheckLayerAvailability
import Amazonka.ECR.BatchDeleteImage
import Amazonka.ECR.BatchGetImage
import Amazonka.ECR.CompleteLayerUpload
import Amazonka.ECR.CreateRepository
import Amazonka.ECR.DeleteLifecyclePolicy
import Amazonka.ECR.DeleteRegistryPolicy
import Amazonka.ECR.DeleteRepository
import Amazonka.ECR.DeleteRepositoryPolicy
import Amazonka.ECR.DescribeImageReplicationStatus
import Amazonka.ECR.DescribeImageScanFindings
import Amazonka.ECR.DescribeImages
import Amazonka.ECR.DescribeRegistry
import Amazonka.ECR.DescribeRepositories
import Amazonka.ECR.GetAuthorizationToken
import Amazonka.ECR.GetDownloadUrlForLayer
import Amazonka.ECR.GetLifecyclePolicy
import Amazonka.ECR.GetLifecyclePolicyPreview
import Amazonka.ECR.GetRegistryPolicy
import Amazonka.ECR.GetRepositoryPolicy
import Amazonka.ECR.InitiateLayerUpload
import Amazonka.ECR.ListImages
import Amazonka.ECR.ListTagsForResource
import Amazonka.ECR.PutImage
import Amazonka.ECR.PutImageScanningConfiguration
import Amazonka.ECR.PutImageTagMutability
import Amazonka.ECR.PutLifecyclePolicy
import Amazonka.ECR.PutRegistryPolicy
import Amazonka.ECR.PutReplicationConfiguration
import Amazonka.ECR.SetRepositoryPolicy
import Amazonka.ECR.StartImageScan
import Amazonka.ECR.StartLifecyclePolicyPreview
import Amazonka.ECR.TagResource
import Amazonka.ECR.Types.Attribute
import Amazonka.ECR.Types.AuthorizationData
import Amazonka.ECR.Types.DescribeImagesFilter
import Amazonka.ECR.Types.EncryptionConfiguration
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
import Amazonka.ECR.Types.ReplicationConfiguration
import Amazonka.ECR.Types.ReplicationDestination
import Amazonka.ECR.Types.ReplicationRule
import Amazonka.ECR.Types.Repository
import Amazonka.ECR.Types.RepositoryFilter
import Amazonka.ECR.Types.Tag
import Amazonka.ECR.UntagResource
import Amazonka.ECR.UploadLayerPart
