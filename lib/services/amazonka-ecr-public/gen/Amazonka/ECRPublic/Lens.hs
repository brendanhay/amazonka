{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ECRPublic.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECRPublic.Lens
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

    -- ** CreateRepository
    createRepository_catalogData,
    createRepository_tags,
    createRepository_repositoryName,
    createRepositoryResponse_catalogData,
    createRepositoryResponse_repository,
    createRepositoryResponse_httpStatus,

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

    -- ** DescribeImageTags
    describeImageTags_maxResults,
    describeImageTags_nextToken,
    describeImageTags_registryId,
    describeImageTags_repositoryName,
    describeImageTagsResponse_imageTagDetails,
    describeImageTagsResponse_nextToken,
    describeImageTagsResponse_httpStatus,

    -- ** DescribeImages
    describeImages_imageIds,
    describeImages_maxResults,
    describeImages_nextToken,
    describeImages_registryId,
    describeImages_repositoryName,
    describeImagesResponse_imageDetails,
    describeImagesResponse_nextToken,
    describeImagesResponse_httpStatus,

    -- ** DescribeRegistries
    describeRegistries_maxResults,
    describeRegistries_nextToken,
    describeRegistriesResponse_nextToken,
    describeRegistriesResponse_httpStatus,
    describeRegistriesResponse_registries,

    -- ** DescribeRepositories
    describeRepositories_maxResults,
    describeRepositories_nextToken,
    describeRepositories_registryId,
    describeRepositories_repositoryNames,
    describeRepositoriesResponse_nextToken,
    describeRepositoriesResponse_repositories,
    describeRepositoriesResponse_httpStatus,

    -- ** GetAuthorizationToken
    getAuthorizationTokenResponse_authorizationData,
    getAuthorizationTokenResponse_httpStatus,

    -- ** GetRegistryCatalogData
    getRegistryCatalogDataResponse_httpStatus,
    getRegistryCatalogDataResponse_registryCatalogData,

    -- ** GetRepositoryCatalogData
    getRepositoryCatalogData_registryId,
    getRepositoryCatalogData_repositoryName,
    getRepositoryCatalogDataResponse_catalogData,
    getRepositoryCatalogDataResponse_httpStatus,

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

    -- ** PutRegistryCatalogData
    putRegistryCatalogData_displayName,
    putRegistryCatalogDataResponse_httpStatus,
    putRegistryCatalogDataResponse_registryCatalogData,

    -- ** PutRepositoryCatalogData
    putRepositoryCatalogData_registryId,
    putRepositoryCatalogData_repositoryName,
    putRepositoryCatalogData_catalogData,
    putRepositoryCatalogDataResponse_catalogData,
    putRepositoryCatalogDataResponse_httpStatus,

    -- ** SetRepositoryPolicy
    setRepositoryPolicy_force,
    setRepositoryPolicy_registryId,
    setRepositoryPolicy_repositoryName,
    setRepositoryPolicy_policyText,
    setRepositoryPolicyResponse_policyText,
    setRepositoryPolicyResponse_registryId,
    setRepositoryPolicyResponse_repositoryName,
    setRepositoryPolicyResponse_httpStatus,

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

    -- ** AuthorizationData
    authorizationData_authorizationToken,
    authorizationData_expiresAt,

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
    imageDetail_imageSizeInBytes,
    imageDetail_imageTags,
    imageDetail_registryId,
    imageDetail_repositoryName,

    -- ** ImageFailure
    imageFailure_failureCode,
    imageFailure_failureReason,
    imageFailure_imageId,

    -- ** ImageIdentifier
    imageIdentifier_imageDigest,
    imageIdentifier_imageTag,

    -- ** ImageTagDetail
    imageTagDetail_createdAt,
    imageTagDetail_imageDetail,
    imageTagDetail_imageTag,

    -- ** Layer
    layer_layerAvailability,
    layer_layerDigest,
    layer_layerSize,
    layer_mediaType,

    -- ** LayerFailure
    layerFailure_failureCode,
    layerFailure_failureReason,
    layerFailure_layerDigest,

    -- ** ReferencedImageDetail
    referencedImageDetail_artifactMediaType,
    referencedImageDetail_imageDigest,
    referencedImageDetail_imageManifestMediaType,
    referencedImageDetail_imagePushedAt,
    referencedImageDetail_imageSizeInBytes,

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
    repository_createdAt,
    repository_registryId,
    repository_repositoryArn,
    repository_repositoryName,
    repository_repositoryUri,

    -- ** RepositoryCatalogData
    repositoryCatalogData_aboutText,
    repositoryCatalogData_architectures,
    repositoryCatalogData_description,
    repositoryCatalogData_logoUrl,
    repositoryCatalogData_marketplaceCertified,
    repositoryCatalogData_operatingSystems,
    repositoryCatalogData_usageText,

    -- ** RepositoryCatalogDataInput
    repositoryCatalogDataInput_aboutText,
    repositoryCatalogDataInput_architectures,
    repositoryCatalogDataInput_description,
    repositoryCatalogDataInput_logoImageBlob,
    repositoryCatalogDataInput_operatingSystems,
    repositoryCatalogDataInput_usageText,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Amazonka.ECRPublic.BatchCheckLayerAvailability
import Amazonka.ECRPublic.BatchDeleteImage
import Amazonka.ECRPublic.CompleteLayerUpload
import Amazonka.ECRPublic.CreateRepository
import Amazonka.ECRPublic.DeleteRepository
import Amazonka.ECRPublic.DeleteRepositoryPolicy
import Amazonka.ECRPublic.DescribeImageTags
import Amazonka.ECRPublic.DescribeImages
import Amazonka.ECRPublic.DescribeRegistries
import Amazonka.ECRPublic.DescribeRepositories
import Amazonka.ECRPublic.GetAuthorizationToken
import Amazonka.ECRPublic.GetRegistryCatalogData
import Amazonka.ECRPublic.GetRepositoryCatalogData
import Amazonka.ECRPublic.GetRepositoryPolicy
import Amazonka.ECRPublic.InitiateLayerUpload
import Amazonka.ECRPublic.ListTagsForResource
import Amazonka.ECRPublic.PutImage
import Amazonka.ECRPublic.PutRegistryCatalogData
import Amazonka.ECRPublic.PutRepositoryCatalogData
import Amazonka.ECRPublic.SetRepositoryPolicy
import Amazonka.ECRPublic.TagResource
import Amazonka.ECRPublic.Types.AuthorizationData
import Amazonka.ECRPublic.Types.Image
import Amazonka.ECRPublic.Types.ImageDetail
import Amazonka.ECRPublic.Types.ImageFailure
import Amazonka.ECRPublic.Types.ImageIdentifier
import Amazonka.ECRPublic.Types.ImageTagDetail
import Amazonka.ECRPublic.Types.Layer
import Amazonka.ECRPublic.Types.LayerFailure
import Amazonka.ECRPublic.Types.ReferencedImageDetail
import Amazonka.ECRPublic.Types.Registry
import Amazonka.ECRPublic.Types.RegistryAlias
import Amazonka.ECRPublic.Types.RegistryCatalogData
import Amazonka.ECRPublic.Types.Repository
import Amazonka.ECRPublic.Types.RepositoryCatalogData
import Amazonka.ECRPublic.Types.RepositoryCatalogDataInput
import Amazonka.ECRPublic.Types.Tag
import Amazonka.ECRPublic.UntagResource
import Amazonka.ECRPublic.UploadLayerPart
