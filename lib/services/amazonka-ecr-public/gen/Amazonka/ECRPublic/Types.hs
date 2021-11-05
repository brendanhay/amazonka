{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ECRPublic.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECRPublic.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ImageTagAlreadyExistsException,
    _LayersNotFoundException,
    _ReferencedImagesNotFoundException,
    _InvalidParameterException,
    _LayerAlreadyExistsException,
    _ServerException,
    _UnsupportedCommandException,
    _InvalidLayerException,
    _LayerPartTooSmallException,
    _ImageDigestDoesNotMatchException,
    _ImageNotFoundException,
    _ImageAlreadyExistsException,
    _RepositoryNotFoundException,
    _TooManyTagsException,
    _UploadNotFoundException,
    _InvalidLayerPartException,
    _InvalidTagParameterException,
    _RepositoryNotEmptyException,
    _RepositoryAlreadyExistsException,
    _RepositoryPolicyNotFoundException,
    _EmptyUploadException,
    _LimitExceededException,
    _RegistryNotFoundException,

    -- * ImageFailureCode
    ImageFailureCode (..),

    -- * LayerAvailability
    LayerAvailability (..),

    -- * LayerFailureCode
    LayerFailureCode (..),

    -- * RegistryAliasStatus
    RegistryAliasStatus (..),

    -- * AuthorizationData
    AuthorizationData (..),
    newAuthorizationData,
    authorizationData_expiresAt,
    authorizationData_authorizationToken,

    -- * Image
    Image (..),
    newImage,
    image_registryId,
    image_imageManifestMediaType,
    image_imageId,
    image_repositoryName,
    image_imageManifest,

    -- * ImageDetail
    ImageDetail (..),
    newImageDetail,
    imageDetail_registryId,
    imageDetail_imageTags,
    imageDetail_imageManifestMediaType,
    imageDetail_imageSizeInBytes,
    imageDetail_imageDigest,
    imageDetail_artifactMediaType,
    imageDetail_imagePushedAt,
    imageDetail_repositoryName,

    -- * ImageFailure
    ImageFailure (..),
    newImageFailure,
    imageFailure_failureReason,
    imageFailure_failureCode,
    imageFailure_imageId,

    -- * ImageIdentifier
    ImageIdentifier (..),
    newImageIdentifier,
    imageIdentifier_imageDigest,
    imageIdentifier_imageTag,

    -- * ImageTagDetail
    ImageTagDetail (..),
    newImageTagDetail,
    imageTagDetail_createdAt,
    imageTagDetail_imageDetail,
    imageTagDetail_imageTag,

    -- * Layer
    Layer (..),
    newLayer,
    layer_mediaType,
    layer_layerDigest,
    layer_layerSize,
    layer_layerAvailability,

    -- * LayerFailure
    LayerFailure (..),
    newLayerFailure,
    layerFailure_failureReason,
    layerFailure_failureCode,
    layerFailure_layerDigest,

    -- * ReferencedImageDetail
    ReferencedImageDetail (..),
    newReferencedImageDetail,
    referencedImageDetail_imageManifestMediaType,
    referencedImageDetail_imageSizeInBytes,
    referencedImageDetail_imageDigest,
    referencedImageDetail_artifactMediaType,
    referencedImageDetail_imagePushedAt,

    -- * Registry
    Registry (..),
    newRegistry,
    registry_registryId,
    registry_registryArn,
    registry_registryUri,
    registry_verified,
    registry_aliases,

    -- * RegistryAlias
    RegistryAlias (..),
    newRegistryAlias,
    registryAlias_name,
    registryAlias_status,
    registryAlias_primaryRegistryAlias,
    registryAlias_defaultRegistryAlias,

    -- * RegistryCatalogData
    RegistryCatalogData (..),
    newRegistryCatalogData,
    registryCatalogData_displayName,

    -- * Repository
    Repository (..),
    newRepository,
    repository_repositoryArn,
    repository_createdAt,
    repository_registryId,
    repository_repositoryUri,
    repository_repositoryName,

    -- * RepositoryCatalogData
    RepositoryCatalogData (..),
    newRepositoryCatalogData,
    repositoryCatalogData_logoUrl,
    repositoryCatalogData_architectures,
    repositoryCatalogData_usageText,
    repositoryCatalogData_marketplaceCertified,
    repositoryCatalogData_aboutText,
    repositoryCatalogData_operatingSystems,
    repositoryCatalogData_description,

    -- * RepositoryCatalogDataInput
    RepositoryCatalogDataInput (..),
    newRepositoryCatalogDataInput,
    repositoryCatalogDataInput_logoImageBlob,
    repositoryCatalogDataInput_architectures,
    repositoryCatalogDataInput_usageText,
    repositoryCatalogDataInput_aboutText,
    repositoryCatalogDataInput_operatingSystems,
    repositoryCatalogDataInput_description,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.ECRPublic.Types.AuthorizationData
import Amazonka.ECRPublic.Types.Image
import Amazonka.ECRPublic.Types.ImageDetail
import Amazonka.ECRPublic.Types.ImageFailure
import Amazonka.ECRPublic.Types.ImageFailureCode
import Amazonka.ECRPublic.Types.ImageIdentifier
import Amazonka.ECRPublic.Types.ImageTagDetail
import Amazonka.ECRPublic.Types.Layer
import Amazonka.ECRPublic.Types.LayerAvailability
import Amazonka.ECRPublic.Types.LayerFailure
import Amazonka.ECRPublic.Types.LayerFailureCode
import Amazonka.ECRPublic.Types.ReferencedImageDetail
import Amazonka.ECRPublic.Types.Registry
import Amazonka.ECRPublic.Types.RegistryAlias
import Amazonka.ECRPublic.Types.RegistryAliasStatus
import Amazonka.ECRPublic.Types.RegistryCatalogData
import Amazonka.ECRPublic.Types.Repository
import Amazonka.ECRPublic.Types.RepositoryCatalogData
import Amazonka.ECRPublic.Types.RepositoryCatalogDataInput
import Amazonka.ECRPublic.Types.Tag
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-10-30@ of the Amazon Elastic Container Registry Public SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "ECRPublic",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "api.ecr-public",
      Core._serviceSigningName = "ecr-public",
      Core._serviceVersion = "2020-10-30",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "ECRPublic",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified image is tagged with a tag that already exists. The
-- repository is configured for tag immutability.
_ImageTagAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ImageTagAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ImageTagAlreadyExistsException"

-- | The specified layers could not be found, or the specified layer is not
-- valid for this repository.
_LayersNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LayersNotFoundException =
  Core._MatchServiceError
    defaultService
    "LayersNotFoundException"

-- | The manifest list is referencing an image that does not exist.
_ReferencedImagesNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReferencedImagesNotFoundException =
  Core._MatchServiceError
    defaultService
    "ReferencedImagesNotFoundException"

-- | The specified parameter is invalid. Review the available parameters for
-- the API request.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | The image layer already exists in the associated repository.
_LayerAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LayerAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "LayerAlreadyExistsException"

-- | These errors are usually caused by a server-side issue.
_ServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServerException =
  Core._MatchServiceError
    defaultService
    "ServerException"

-- | The action is not supported in this Region.
_UnsupportedCommandException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedCommandException =
  Core._MatchServiceError
    defaultService
    "UnsupportedCommandException"

-- | The layer digest calculation performed by Amazon ECR upon receipt of the
-- image layer does not match the digest specified.
_InvalidLayerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidLayerException =
  Core._MatchServiceError
    defaultService
    "InvalidLayerException"

-- | Layer parts must be at least 5 MiB in size.
_LayerPartTooSmallException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LayerPartTooSmallException =
  Core._MatchServiceError
    defaultService
    "LayerPartTooSmallException"

-- | The specified image digest does not match the digest that Amazon ECR
-- calculated for the image.
_ImageDigestDoesNotMatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ImageDigestDoesNotMatchException =
  Core._MatchServiceError
    defaultService
    "ImageDigestDoesNotMatchException"

-- | The image requested does not exist in the specified repository.
_ImageNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ImageNotFoundException =
  Core._MatchServiceError
    defaultService
    "ImageNotFoundException"

-- | The specified image has already been pushed, and there were no changes
-- to the manifest or image tag after the last push.
_ImageAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ImageAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ImageAlreadyExistsException"

-- | The specified repository could not be found. Check the spelling of the
-- specified repository and ensure that you are performing operations on
-- the correct registry.
_RepositoryNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryNotFoundException =
  Core._MatchServiceError
    defaultService
    "RepositoryNotFoundException"

-- | The list of tags on the repository is over the limit. The maximum number
-- of tags that can be applied to a repository is 50.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | The upload could not be found, or the specified upload ID is not valid
-- for this repository.
_UploadNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UploadNotFoundException =
  Core._MatchServiceError
    defaultService
    "UploadNotFoundException"

-- | The layer part size is not valid, or the first byte specified is not
-- consecutive to the last byte of a previous layer part upload.
_InvalidLayerPartException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidLayerPartException =
  Core._MatchServiceError
    defaultService
    "InvalidLayerPartException"

-- | An invalid parameter has been specified. Tag keys can have a maximum
-- character length of 128 characters, and tag values can have a maximum
-- length of 256 characters.
_InvalidTagParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTagParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidTagParameterException"

-- | The specified repository contains images. To delete a repository that
-- contains images, you must force the deletion with the @force@ parameter.
_RepositoryNotEmptyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryNotEmptyException =
  Core._MatchServiceError
    defaultService
    "RepositoryNotEmptyException"

-- | The specified repository already exists in the specified registry.
_RepositoryAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "RepositoryAlreadyExistsException"

-- | The specified repository and registry combination does not have an
-- associated repository policy.
_RepositoryPolicyNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryPolicyNotFoundException =
  Core._MatchServiceError
    defaultService
    "RepositoryPolicyNotFoundException"

-- | The specified layer upload does not contain any layer parts.
_EmptyUploadException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EmptyUploadException =
  Core._MatchServiceError
    defaultService
    "EmptyUploadException"

-- | The operation did not succeed because it would have exceeded a service
-- limit for your account. For more information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/service-quotas.html Amazon ECR Service Quotas>
-- in the Amazon Elastic Container Registry User Guide.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The registry does not exist.
_RegistryNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RegistryNotFoundException =
  Core._MatchServiceError
    defaultService
    "RegistryNotFoundException"
