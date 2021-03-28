-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStoreData.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaStoreData.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _RequestedRangeNotSatisfiableException
    , _InternalServerError
    , _ContainerNotFoundException
    , _ObjectNotFoundException

    -- * StringPrimitive
    , StringPrimitive (..)

    -- * PathNaming
    , PathNaming (..)

    -- * PaginationToken
    , PaginationToken (..)

    -- * ETag
    , ETag (..)

    -- * RangePattern
    , RangePattern (..)

    -- * ItemName
    , ItemName (..)

    -- * ItemType
    , ItemType (..)

    -- * StorageClass
    , StorageClass (..)

    -- * UploadAvailability
    , UploadAvailability (..)

    -- * Item
    , Item (..)
    , mkItem
    , iContentLength
    , iContentType
    , iETag
    , iLastModified
    , iName
    , iType

    -- * ContentType
    , ContentType (..)

    -- * ContentSHA256
    , ContentSHA256 (..)

    -- * Path
    , Path (..)

    -- * ContentRange
    , ContentRange (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.MediaStoreData.Types.StringPrimitive
  
import Network.AWS.MediaStoreData.Types.PathNaming
  
import Network.AWS.MediaStoreData.Types.PaginationToken
  
import Network.AWS.MediaStoreData.Types.ETag
  
import Network.AWS.MediaStoreData.Types.RangePattern
  
import Network.AWS.MediaStoreData.Types.ItemName
  
import Network.AWS.MediaStoreData.Types.ItemType
  
  
import Network.AWS.MediaStoreData.Types.StorageClass
  
import Network.AWS.MediaStoreData.Types.UploadAvailability
  
  
import Network.AWS.MediaStoreData.Types.Item
  
  
  
import Network.AWS.MediaStoreData.Types.ContentType
  
import Network.AWS.MediaStoreData.Types.ContentSHA256
  
import Network.AWS.MediaStoreData.Types.Path
  
import Network.AWS.MediaStoreData.Types.ContentRange
  

-- | API version @2017-09-01@ of the Amazon Elemental MediaStore Data Plane SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "MediaStoreData",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "data.mediastore",
                 Core._svcVersion = "2017-09-01", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "MediaStoreData",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | The requested content range is not valid.
_RequestedRangeNotSatisfiableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RequestedRangeNotSatisfiableException
  = Core._MatchServiceError mkServiceConfig
      "RequestedRangeNotSatisfiableException"
      Core.. Core.hasStatues 416
{-# INLINEABLE _RequestedRangeNotSatisfiableException #-}
{-# DEPRECATED _RequestedRangeNotSatisfiableException "Use generic-lens or generic-optics instead"  #-}

-- | The service is temporarily unavailable.
_InternalServerError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerError
  = Core._MatchServiceError mkServiceConfig "InternalServerError"
{-# INLINEABLE _InternalServerError #-}
{-# DEPRECATED _InternalServerError "Use generic-lens or generic-optics instead"  #-}

-- | The specified container was not found for the specified account.
_ContainerNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ContainerNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ContainerNotFoundException"
      Core.. Core.hasStatues 404
{-# INLINEABLE _ContainerNotFoundException #-}
{-# DEPRECATED _ContainerNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | Could not perform an operation on an object that does not exist.
_ObjectNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ObjectNotFoundException
  = Core._MatchServiceError mkServiceConfig "ObjectNotFoundException"
      Core.. Core.hasStatues 404
{-# INLINEABLE _ObjectNotFoundException #-}
{-# DEPRECATED _ObjectNotFoundException "Use generic-lens or generic-optics instead"  #-}
