{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECR.Types
    (
    -- * Service Configuration
      ecr

    -- * Errors
    , _LayersNotFoundException
    , _InvalidParameterException
    , _LayerAlreadyExistsException
    , _ServerException
    , _LayerInaccessibleException
    , _InvalidLayerException
    , _LayerPartTooSmallException
    , _ImageAlreadyExistsException
    , _RepositoryNotFoundException
    , _UploadNotFoundException
    , _InvalidLayerPartException
    , _RepositoryNotEmptyException
    , _RepositoryAlreadyExistsException
    , _RepositoryPolicyNotFoundException
    , _EmptyUploadException
    , _LimitExceededException

    -- * ImageFailureCode
    , ImageFailureCode (..)

    -- * LayerAvailability
    , LayerAvailability (..)

    -- * LayerFailureCode
    , LayerFailureCode (..)

    -- * AuthorizationData
    , AuthorizationData
    , authorizationData
    , adExpiresAt
    , adProxyEndpoint
    , adAuthorizationToken

    -- * Image
    , Image
    , image
    , iRegistryId
    , iImageId
    , iRepositoryName
    , iImageManifest

    -- * ImageFailure
    , ImageFailure
    , imageFailure
    , ifFailureReason
    , ifFailureCode
    , ifImageId

    -- * ImageIdentifier
    , ImageIdentifier
    , imageIdentifier
    , iiImageDigest
    , iiImageTag

    -- * Layer
    , Layer
    , layer
    , lLayerDigest
    , lLayerSize
    , lLayerAvailability

    -- * LayerFailure
    , LayerFailure
    , layerFailure
    , lfFailureReason
    , lfFailureCode
    , lfLayerDigest

    -- * Repository
    , Repository
    , repository
    , rRepositoryARN
    , rRegistryId
    , rRepositoryURI
    , rRepositoryName
    ) where

import           Network.AWS.ECR.Types.Product
import           Network.AWS.ECR.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2015-09-21' of the Amazon EC2 Container Registry SDK configuration.
ecr :: Service
ecr =
    Service
    { _svcAbbrev = "ECR"
    , _svcSigner = v4
    , _svcPrefix = "ecr"
    , _svcVersion = "2015-09-21"
    , _svcEndpoint = defaultEndpoint ecr
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "ECR"
    , _svcRetry = retry
    }
  where
    retry =
        Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | The specified layers could not be found, or the specified layer is not valid for this repository.
_LayersNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_LayersNotFoundException = _ServiceError . hasCode "LayersNotFoundException"

-- | The specified parameter is invalid. Review the available parameters for the API request.
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
    _ServiceError . hasCode "InvalidParameterException"

-- | The image layer already exists in the associated repository.
_LayerAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_LayerAlreadyExistsException =
    _ServiceError . hasCode "LayerAlreadyExistsException"

-- | These errors are usually caused by a server-side issue.
_ServerException :: AsError a => Getting (First ServiceError) a ServiceError
_ServerException = _ServiceError . hasCode "ServerException"

-- | The specified layer is not available because it is not associated with an image. Unassociated image layers may be cleaned up at any time.
_LayerInaccessibleException :: AsError a => Getting (First ServiceError) a ServiceError
_LayerInaccessibleException =
    _ServiceError . hasCode "LayerInaccessibleException"

-- | The layer digest calculation performed by Amazon ECR upon receipt of the image layer does not match the digest specified.
_InvalidLayerException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidLayerException = _ServiceError . hasCode "InvalidLayerException"

-- | Layer parts must be at least 5 MiB in size.
_LayerPartTooSmallException :: AsError a => Getting (First ServiceError) a ServiceError
_LayerPartTooSmallException =
    _ServiceError . hasCode "LayerPartTooSmallException"

-- | The specified image has already been pushed, and there are no changes to the manifest or image tag since the last push.
_ImageAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ImageAlreadyExistsException =
    _ServiceError . hasCode "ImageAlreadyExistsException"

-- | The specified repository could not be found. Check the spelling of the specified repository and ensure that you are performing operations on the correct registry.
_RepositoryNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryNotFoundException =
    _ServiceError . hasCode "RepositoryNotFoundException"

-- | The upload could not be found, or the specified upload id is not valid for this repository.
_UploadNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_UploadNotFoundException = _ServiceError . hasCode "UploadNotFoundException"

-- | The layer part size is not valid, or the first byte specified is not consecutive to the last byte of a previous layer part upload.
_InvalidLayerPartException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidLayerPartException =
    _ServiceError . hasCode "InvalidLayerPartException"

-- | The specified repository contains images. To delete a repository that contains images, you must force the deletion with the 'force' parameter.
_RepositoryNotEmptyException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryNotEmptyException =
    _ServiceError . hasCode "RepositoryNotEmptyException"

-- | The specified repository already exists in the specified registry.
_RepositoryAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryAlreadyExistsException =
    _ServiceError . hasCode "RepositoryAlreadyExistsException"

-- | The specified repository and registry combination does not have an associated repository policy.
_RepositoryPolicyNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryPolicyNotFoundException =
    _ServiceError . hasCode "RepositoryPolicyNotFoundException"

-- | The specified layer upload does not contain any layer parts.
_EmptyUploadException :: AsError a => Getting (First ServiceError) a ServiceError
_EmptyUploadException = _ServiceError . hasCode "EmptyUploadException"

-- | The operation did not succeed because it would have exceeded a service limit for your account. For more information, see <http://docs.aws.amazon.com/AmazonECR/latest/userguide/service_limits.html Amazon ECR Default Service Limits> in the Amazon EC2 Container Registry User Guide.
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _ServiceError . hasCode "LimitExceededException"
