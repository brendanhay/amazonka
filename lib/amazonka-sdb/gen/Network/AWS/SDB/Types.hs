{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Types
  ( -- * Service Configuration
    sdb,

    -- * Errors

    -- * Attribute
    Attribute,
    attribute,
    aAlternateValueEncoding,
    aAlternateNameEncoding,
    aName,
    aValue,

    -- * DeletableItem
    DeletableItem,
    deletableItem,
    diAttributes,
    diName,

    -- * Item
    Item,
    item,
    iAlternateNameEncoding,
    iName,
    iAttributes,

    -- * ReplaceableAttribute
    ReplaceableAttribute,
    replaceableAttribute,
    raReplace,
    raName,
    raValue,

    -- * ReplaceableItem
    ReplaceableItem,
    replaceableItem,
    riName,
    riAttributes,

    -- * UpdateCondition
    UpdateCondition,
    updateCondition,
    ucExists,
    ucValue,
    ucName,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SDB.Types.Attribute
import Network.AWS.SDB.Types.DeletableItem
import Network.AWS.SDB.Types.Item
import Network.AWS.SDB.Types.ReplaceableAttribute
import Network.AWS.SDB.Types.ReplaceableItem
import Network.AWS.SDB.Types.UpdateCondition
import Network.AWS.Sign.V2

-- | API version @2009-04-15@ of the Amazon SimpleDB SDK configuration.
sdb :: Service
sdb =
  Service
    { _svcAbbrev = "SDB",
      _svcSigner = v2,
      _svcPrefix = "sdb",
      _svcVersion = "2009-04-15",
      _svcEndpoint = defaultEndpoint sdb,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseXMLError "SDB",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
