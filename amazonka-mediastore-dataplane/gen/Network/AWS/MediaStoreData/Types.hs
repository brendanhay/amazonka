{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStoreData.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaStoreData.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _RequestedRangeNotSatisfiableException,
    _ObjectNotFoundException,
    _InternalServerError,
    _ContainerNotFoundException,

    -- * ItemType
    ItemType (..),

    -- * StorageClass
    StorageClass (..),

    -- * UploadAvailability
    UploadAvailability (..),

    -- * Item
    Item (..),
    newItem,
    item_eTag,
    item_contentType,
    item_contentLength,
    item_name,
    item_lastModified,
    item_type,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStoreData.Types.Item
import Network.AWS.MediaStoreData.Types.ItemType
import Network.AWS.MediaStoreData.Types.StorageClass
import Network.AWS.MediaStoreData.Types.UploadAvailability
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-09-01@ of the Amazon Elemental MediaStore Data Plane SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "MediaStoreData",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "data.mediastore",
      Core._serviceSigningName = "mediastore",
      Core._serviceVersion = "2017-09-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "MediaStoreData",
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
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | The requested content range is not valid.
_RequestedRangeNotSatisfiableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RequestedRangeNotSatisfiableException =
  Core._MatchServiceError
    defaultService
    "RequestedRangeNotSatisfiableException"
    Core.. Core.hasStatus 416

-- | Could not perform an operation on an object that does not exist.
_ObjectNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ObjectNotFoundException =
  Core._MatchServiceError
    defaultService
    "ObjectNotFoundException"
    Core.. Core.hasStatus 404

-- | The service is temporarily unavailable.
_InternalServerError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError
    defaultService
    "InternalServerError"

-- | The specified container was not found for the specified account.
_ContainerNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ContainerNotFoundException =
  Core._MatchServiceError
    defaultService
    "ContainerNotFoundException"
    Core.. Core.hasStatus 404
