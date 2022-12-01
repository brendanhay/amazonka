{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ECRPublic.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECRPublic.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidLayerPartException,
    _RegistryNotFoundException,
    _RepositoryAlreadyExistsException,
    _LayerAlreadyExistsException,
    _UnsupportedCommandException,
    _LayerPartTooSmallException,
    _ImageAlreadyExistsException,
    _RepositoryNotFoundException,
    _ReferencedImagesNotFoundException,
    _TooManyTagsException,
    _ImageDigestDoesNotMatchException,
    _RepositoryNotEmptyException,
    _LayersNotFoundException,
    _InvalidTagParameterException,
    _LimitExceededException,
    _EmptyUploadException,
    _RepositoryPolicyNotFoundException,
    _ImageTagAlreadyExistsException,
    _UploadNotFoundException,
    _ImageNotFoundException,
    _InvalidLayerException,
    _ServerException,
    _InvalidParameterException,

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
    image_repositoryName,
    image_registryId,
    image_imageManifestMediaType,
    image_imageManifest,
    image_imageId,

    -- * ImageDetail
    ImageDetail (..),
    newImageDetail,
    imageDetail_artifactMediaType,
    imageDetail_imagePushedAt,
    imageDetail_repositoryName,
    imageDetail_imageSizeInBytes,
    imageDetail_imageTags,
    imageDetail_registryId,
    imageDetail_imageManifestMediaType,
    imageDetail_imageDigest,

    -- * ImageFailure
    ImageFailure (..),
    newImageFailure,
    imageFailure_failureCode,
    imageFailure_imageId,
    imageFailure_failureReason,

    -- * ImageIdentifier
    ImageIdentifier (..),
    newImageIdentifier,
    imageIdentifier_imageTag,
    imageIdentifier_imageDigest,

    -- * ImageTagDetail
    ImageTagDetail (..),
    newImageTagDetail,
    imageTagDetail_imageTag,
    imageTagDetail_imageDetail,
    imageTagDetail_createdAt,

    -- * Layer
    Layer (..),
    newLayer,
    layer_layerSize,
    layer_layerAvailability,
    layer_mediaType,
    layer_layerDigest,

    -- * LayerFailure
    LayerFailure (..),
    newLayerFailure,
    layerFailure_failureCode,
    layerFailure_layerDigest,
    layerFailure_failureReason,

    -- * ReferencedImageDetail
    ReferencedImageDetail (..),
    newReferencedImageDetail,
    referencedImageDetail_artifactMediaType,
    referencedImageDetail_imagePushedAt,
    referencedImageDetail_imageSizeInBytes,
    referencedImageDetail_imageManifestMediaType,
    referencedImageDetail_imageDigest,

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
    repository_repositoryUri,
    repository_repositoryName,
    repository_registryId,
    repository_createdAt,

    -- * RepositoryCatalogData
    RepositoryCatalogData (..),
    newRepositoryCatalogData,
    repositoryCatalogData_marketplaceCertified,
    repositoryCatalogData_logoUrl,
    repositoryCatalogData_description,
    repositoryCatalogData_aboutText,
    repositoryCatalogData_usageText,
    repositoryCatalogData_operatingSystems,
    repositoryCatalogData_architectures,

    -- * RepositoryCatalogDataInput
    RepositoryCatalogDataInput (..),
    newRepositoryCatalogDataInput,
    repositoryCatalogDataInput_description,
    repositoryCatalogDataInput_aboutText,
    repositoryCatalogDataInput_usageText,
    repositoryCatalogDataInput_operatingSystems,
    repositoryCatalogDataInput_logoImageBlob,
    repositoryCatalogDataInput_architectures,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-10-30@ of the Amazon Elastic Container Registry Public SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "ECRPublic",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "api.ecr-public",
      Core.signingName = "ecr-public",
      Core.version = "2020-10-30",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "ECRPublic",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The layer part size is not valid, or the first byte specified is not
-- consecutive to the last byte of a previous layer part upload.
_InvalidLayerPartException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidLayerPartException =
  Core._MatchServiceError
    defaultService
    "InvalidLayerPartException"

-- | The registry does not exist.
_RegistryNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RegistryNotFoundException =
  Core._MatchServiceError
    defaultService
    "RegistryNotFoundException"

-- | The specified repository already exists in the specified registry.
_RepositoryAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "RepositoryAlreadyExistsException"

-- | The image layer already exists in the associated repository.
_LayerAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LayerAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "LayerAlreadyExistsException"

-- | The action is not supported in this Region.
_UnsupportedCommandException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedCommandException =
  Core._MatchServiceError
    defaultService
    "UnsupportedCommandException"

-- | Layer parts must be at least 5 MiB in size.
_LayerPartTooSmallException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LayerPartTooSmallException =
  Core._MatchServiceError
    defaultService
    "LayerPartTooSmallException"

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

-- | The manifest list is referencing an image that does not exist.
_ReferencedImagesNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReferencedImagesNotFoundException =
  Core._MatchServiceError
    defaultService
    "ReferencedImagesNotFoundException"

-- | The list of tags on the repository is over the limit. The maximum number
-- of tags that can be applied to a repository is 50.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | The specified image digest does not match the digest that Amazon ECR
-- calculated for the image.
_ImageDigestDoesNotMatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ImageDigestDoesNotMatchException =
  Core._MatchServiceError
    defaultService
    "ImageDigestDoesNotMatchException"

-- | The specified repository contains images. To delete a repository that
-- contains images, you must force the deletion with the @force@ parameter.
_RepositoryNotEmptyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryNotEmptyException =
  Core._MatchServiceError
    defaultService
    "RepositoryNotEmptyException"

-- | The specified layers could not be found, or the specified layer is not
-- valid for this repository.
_LayersNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LayersNotFoundException =
  Core._MatchServiceError
    defaultService
    "LayersNotFoundException"

-- | An invalid parameter has been specified. Tag keys can have a maximum
-- character length of 128 characters, and tag values can have a maximum
-- length of 256 characters.
_InvalidTagParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTagParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidTagParameterException"

-- | The operation did not succeed because it would have exceeded a service
-- limit for your account. For more information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/service-quotas.html Amazon ECR Service Quotas>
-- in the Amazon Elastic Container Registry User Guide.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The specified layer upload does not contain any layer parts.
_EmptyUploadException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EmptyUploadException =
  Core._MatchServiceError
    defaultService
    "EmptyUploadException"

-- | The specified repository and registry combination does not have an
-- associated repository policy.
_RepositoryPolicyNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryPolicyNotFoundException =
  Core._MatchServiceError
    defaultService
    "RepositoryPolicyNotFoundException"

-- | The specified image is tagged with a tag that already exists. The
-- repository is configured for tag immutability.
_ImageTagAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ImageTagAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ImageTagAlreadyExistsException"

-- | The upload could not be found, or the specified upload ID is not valid
-- for this repository.
_UploadNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UploadNotFoundException =
  Core._MatchServiceError
    defaultService
    "UploadNotFoundException"

-- | The image requested does not exist in the specified repository.
_ImageNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ImageNotFoundException =
  Core._MatchServiceError
    defaultService
    "ImageNotFoundException"

-- | The layer digest calculation performed by Amazon ECR upon receipt of the
-- image layer does not match the digest specified.
_InvalidLayerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidLayerException =
  Core._MatchServiceError
    defaultService
    "InvalidLayerException"

-- | These errors are usually caused by a server-side issue.
_ServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServerException =
  Core._MatchServiceError
    defaultService
    "ServerException"

-- | The specified parameter is invalid. Review the available parameters for
-- the API request.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"
