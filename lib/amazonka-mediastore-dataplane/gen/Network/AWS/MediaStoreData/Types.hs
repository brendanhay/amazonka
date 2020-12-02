{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStoreData.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaStoreData.Types
    (
    -- * Service Configuration
      mediaStoreData

    -- * Errors
    , _RequestedRangeNotSatisfiableException
    , _InternalServerError
    , _ContainerNotFoundException
    , _ObjectNotFoundException

    -- * ItemType
    , ItemType (..)

    -- * StorageClass
    , StorageClass (..)

    -- * Item
    , Item
    , item
    , iETag
    , iContentLength
    , iName
    , iType
    , iLastModified
    , iContentType
    ) where

import Network.AWS.Lens
import Network.AWS.MediaStoreData.Types.Product
import Network.AWS.MediaStoreData.Types.Sum
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-09-01@ of the Amazon Elemental MediaStore Data Plane SDK configuration.
mediaStoreData :: Service
mediaStoreData =
  Service
    { _svcAbbrev = "MediaStoreData"
    , _svcSigner = v4
    , _svcPrefix = "data.mediastore"
    , _svcVersion = "2017-09-01"
    , _svcEndpoint = defaultEndpoint mediaStoreData
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "MediaStoreData"
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
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The requested content range is not valid.
--
--
_RequestedRangeNotSatisfiableException :: AsError a => Getting (First ServiceError) a ServiceError
_RequestedRangeNotSatisfiableException =
  _MatchServiceError mediaStoreData "RequestedRangeNotSatisfiableException" .
  hasStatus 416


-- | The service is temporarily unavailable.
--
--
_InternalServerError :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerError = _MatchServiceError mediaStoreData "InternalServerError"


-- | The specified container was not found for the specified account.
--
--
_ContainerNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ContainerNotFoundException =
  _MatchServiceError mediaStoreData "ContainerNotFoundException" . hasStatus 404


-- | Could not perform an operation on an object that does not exist.
--
--
_ObjectNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ObjectNotFoundException =
  _MatchServiceError mediaStoreData "ObjectNotFoundException" . hasStatus 404

