-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Types
  ( -- * Service configuration
    sdbService,

    -- * Errors

    -- * Attribute
    Attribute (..),
    mkAttribute,
    aAlternateValueEncoding,
    aValue,
    aAlternateNameEncoding,
    aName,

    -- * DeletableItem
    DeletableItem (..),
    mkDeletableItem,
    diAttributes,
    diName,

    -- * Item
    Item (..),
    mkItem,
    iAlternateNameEncoding,
    iAttributes,
    iName,

    -- * ReplaceableAttribute
    ReplaceableAttribute (..),
    mkReplaceableAttribute,
    raReplace,
    raValue,
    raName,

    -- * ReplaceableItem
    ReplaceableItem (..),
    mkReplaceableItem,
    riAttributes,
    riName,

    -- * UpdateCondition
    UpdateCondition (..),
    mkUpdateCondition,
    ucExists,
    ucValue,
    ucName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SDB.Types.Attribute
import Network.AWS.SDB.Types.DeletableItem
import Network.AWS.SDB.Types.Item
import Network.AWS.SDB.Types.ReplaceableAttribute
import Network.AWS.SDB.Types.ReplaceableItem
import Network.AWS.SDB.Types.UpdateCondition
import qualified Network.AWS.Sign.V2 as Sign

-- | API version @2009-04-15@ of the Amazon SimpleDB SDK configuration.
sdbService :: Lude.Service
sdbService =
  Lude.Service
    { Lude._svcAbbrev = "SDB",
      Lude._svcSigner = Sign.v2,
      Lude._svcPrefix = "sdb",
      Lude._svcVersion = "2009-04-15",
      Lude._svcEndpoint = Lude.defaultEndpoint sdbService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseXMLError "SDB",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
