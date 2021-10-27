{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECRPublic.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECRPublic.Lens
  ( -- * Operations

    -- ** GetRepositoryPolicy
    getRepositoryPolicy_registryId,
    getRepositoryPolicy_repositoryName,
    getRepositoryPolicyResponse_registryId,
    getRepositoryPolicyResponse_repositoryName,
    getRepositoryPolicyResponse_policyText,
    getRepositoryPolicyResponse_httpStatus,

    -- ** PutRegistryCatalogData
    putRegistryCatalogData_displayName,
    putRegistryCatalogDataResponse_httpStatus,
    putRegistryCatalogDataResponse_registryCatalogData,

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

    -- ** BatchCheckLayerAvailability
    batchCheckLayerAvailability_registryId,
    batchCheckLayerAvailability_repositoryName,
    batchCheckLayerAvailability_layerDigests,
    batchCheckLayerAvailabilityResponse_failures,
    batchCheckLayerAvailabilityResponse_layers,
    batchCheckLayerAvailabilityResponse_httpStatus,

    -- ** PutRepositoryCatalogData
    putRepositoryCatalogData_registryId,
    putRepositoryCatalogData_repositoryName,
    putRepositoryCatalogData_catalogData,
    putRepositoryCatalogDataResponse_catalogData,
    putRepositoryCatalogDataResponse_httpStatus,

    -- ** DeleteRepositoryPolicy
    deleteRepositoryPolicy_registryId,
    deleteRepositoryPolicy_repositoryName,
    deleteRepositoryPolicyResponse_registryId,
    deleteRepositoryPolicyResponse_repositoryName,
    deleteRepositoryPolicyResponse_policyText,
    deleteRepositoryPolicyResponse_httpStatus,

    -- ** CreateRepository
    createRepository_catalogData,
    createRepository_tags,
    createRepository_repositoryName,
    createRepositoryResponse_repository,
    createRepositoryResponse_catalogData,
    createRepositoryResponse_httpStatus,

    -- ** DescribeRegistries
    describeRegistries_nextToken,
    describeRegistries_maxResults,
    describeRegistriesResponse_nextToken,
    describeRegistriesResponse_httpStatus,
    describeRegistriesResponse_registries,

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

    -- ** GetRepositoryCatalogData
    getRepositoryCatalogData_registryId,
    getRepositoryCatalogData_repositoryName,
    getRepositoryCatalogDataResponse_catalogData,
    getRepositoryCatalogDataResponse_httpStatus,

    -- ** GetRegistryCatalogData
    getRegistryCatalogDataResponse_httpStatus,
    getRegistryCatalogDataResponse_registryCatalogData,

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

    -- ** DescribeImageTags
    describeImageTags_registryId,
    describeImageTags_nextToken,
    describeImageTags_maxResults,
    describeImageTags_repositoryName,
    describeImageTagsResponse_nextToken,
    describeImageTagsResponse_imageTagDetails,
    describeImageTagsResponse_httpStatus,

    -- ** DeleteRepository
    deleteRepository_force,
    deleteRepository_registryId,
    deleteRepository_repositoryName,
    deleteRepositoryResponse_repository,
    deleteRepositoryResponse_httpStatus,

    -- ** PutImage
    putImage_registryId,
    putImage_imageManifestMediaType,
    putImage_imageDigest,
    putImage_imageTag,
    putImage_repositoryName,
    putImage_imageManifest,
    putImageResponse_image,
    putImageResponse_httpStatus,

    -- ** GetAuthorizationToken
    getAuthorizationTokenResponse_authorizationData,
    getAuthorizationTokenResponse_httpStatus,

    -- ** DescribeImages
    describeImages_registryId,
    describeImages_imageIds,
    describeImages_nextToken,
    describeImages_maxResults,
    describeImages_repositoryName,
    describeImagesResponse_imageDetails,
    describeImagesResponse_nextToken,
    describeImagesResponse_httpStatus,

    -- * Types

    -- ** AuthorizationData
    authorizationData_expiresAt,
    authorizationData_authorizationToken,

    -- ** Image
    image_registryId,
    image_imageManifestMediaType,
    image_imageId,
    image_repositoryName,
    image_imageManifest,

    -- ** ImageDetail
    imageDetail_registryId,
    imageDetail_imageTags,
    imageDetail_imageManifestMediaType,
    imageDetail_imageSizeInBytes,
    imageDetail_imageDigest,
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

    -- ** ImageTagDetail
    imageTagDetail_createdAt,
    imageTagDetail_imageDetail,
    imageTagDetail_imageTag,

    -- ** Layer
    layer_mediaType,
    layer_layerDigest,
    layer_layerSize,
    layer_layerAvailability,

    -- ** LayerFailure
    layerFailure_failureReason,
    layerFailure_failureCode,
    layerFailure_layerDigest,

    -- ** ReferencedImageDetail
    referencedImageDetail_imageManifestMediaType,
    referencedImageDetail_imageSizeInBytes,
    referencedImageDetail_imageDigest,
    referencedImageDetail_artifactMediaType,
    referencedImageDetail_imagePushedAt,

    -- ** Registry
    registry_registryId,
    registry_registryArn,
    registry_registryUri,
    registry_verified,
    registry_aliases,

    -- ** RegistryAlias
    registryAlias_name,
    registryAlias_status,
    registryAlias_primaryRegistryAlias,
    registryAlias_defaultRegistryAlias,

    -- ** RegistryCatalogData
    registryCatalogData_displayName,

    -- ** Repository
    repository_repositoryArn,
    repository_createdAt,
    repository_registryId,
    repository_repositoryUri,
    repository_repositoryName,

    -- ** RepositoryCatalogData
    repositoryCatalogData_logoUrl,
    repositoryCatalogData_architectures,
    repositoryCatalogData_usageText,
    repositoryCatalogData_marketplaceCertified,
    repositoryCatalogData_aboutText,
    repositoryCatalogData_operatingSystems,
    repositoryCatalogData_description,

    -- ** RepositoryCatalogDataInput
    repositoryCatalogDataInput_logoImageBlob,
    repositoryCatalogDataInput_architectures,
    repositoryCatalogDataInput_usageText,
    repositoryCatalogDataInput_aboutText,
    repositoryCatalogDataInput_operatingSystems,
    repositoryCatalogDataInput_description,

    -- ** Tag
    tag_value,
    tag_key,
  )
where

import Network.AWS.ECRPublic.BatchCheckLayerAvailability
import Network.AWS.ECRPublic.BatchDeleteImage
import Network.AWS.ECRPublic.CompleteLayerUpload
import Network.AWS.ECRPublic.CreateRepository
import Network.AWS.ECRPublic.DeleteRepository
import Network.AWS.ECRPublic.DeleteRepositoryPolicy
import Network.AWS.ECRPublic.DescribeImageTags
import Network.AWS.ECRPublic.DescribeImages
import Network.AWS.ECRPublic.DescribeRegistries
import Network.AWS.ECRPublic.DescribeRepositories
import Network.AWS.ECRPublic.GetAuthorizationToken
import Network.AWS.ECRPublic.GetRegistryCatalogData
import Network.AWS.ECRPublic.GetRepositoryCatalogData
import Network.AWS.ECRPublic.GetRepositoryPolicy
import Network.AWS.ECRPublic.InitiateLayerUpload
import Network.AWS.ECRPublic.ListTagsForResource
import Network.AWS.ECRPublic.PutImage
import Network.AWS.ECRPublic.PutRegistryCatalogData
import Network.AWS.ECRPublic.PutRepositoryCatalogData
import Network.AWS.ECRPublic.SetRepositoryPolicy
import Network.AWS.ECRPublic.TagResource
import Network.AWS.ECRPublic.Types.AuthorizationData
import Network.AWS.ECRPublic.Types.Image
import Network.AWS.ECRPublic.Types.ImageDetail
import Network.AWS.ECRPublic.Types.ImageFailure
import Network.AWS.ECRPublic.Types.ImageIdentifier
import Network.AWS.ECRPublic.Types.ImageTagDetail
import Network.AWS.ECRPublic.Types.Layer
import Network.AWS.ECRPublic.Types.LayerFailure
import Network.AWS.ECRPublic.Types.ReferencedImageDetail
import Network.AWS.ECRPublic.Types.Registry
import Network.AWS.ECRPublic.Types.RegistryAlias
import Network.AWS.ECRPublic.Types.RegistryCatalogData
import Network.AWS.ECRPublic.Types.Repository
import Network.AWS.ECRPublic.Types.RepositoryCatalogData
import Network.AWS.ECRPublic.Types.RepositoryCatalogDataInput
import Network.AWS.ECRPublic.Types.Tag
import Network.AWS.ECRPublic.UntagResource
import Network.AWS.ECRPublic.UploadLayerPart
