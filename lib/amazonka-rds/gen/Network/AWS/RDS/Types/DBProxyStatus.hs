{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBProxyStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBProxyStatus where

import Network.AWS.Prelude

data DBProxyStatus
  = Available
  | Creating
  | Deleting
  | IncompatibleNetwork
  | InsufficientResourceLimits
  | Modifying
  | Reactivating
  | Suspended
  | Suspending
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText DBProxyStatus where
  parser =
    takeLowerText >>= \case
      "available" -> pure Available
      "creating" -> pure Creating
      "deleting" -> pure Deleting
      "incompatible-network" -> pure IncompatibleNetwork
      "insufficient-resource-limits" -> pure InsufficientResourceLimits
      "modifying" -> pure Modifying
      "reactivating" -> pure Reactivating
      "suspended" -> pure Suspended
      "suspending" -> pure Suspending
      e ->
        fromTextError $
          "Failure parsing DBProxyStatus from value: '" <> e
            <> "'. Accepted values: available, creating, deleting, incompatible-network, insufficient-resource-limits, modifying, reactivating, suspended, suspending"

instance ToText DBProxyStatus where
  toText = \case
    Available -> "available"
    Creating -> "creating"
    Deleting -> "deleting"
    IncompatibleNetwork -> "incompatible-network"
    InsufficientResourceLimits -> "insufficient-resource-limits"
    Modifying -> "modifying"
    Reactivating -> "reactivating"
    Suspended -> "suspended"
    Suspending -> "suspending"

instance Hashable DBProxyStatus

instance NFData DBProxyStatus

instance ToByteString DBProxyStatus

instance ToQuery DBProxyStatus

instance ToHeader DBProxyStatus

instance FromXML DBProxyStatus where
  parseXML = parseXMLText "DBProxyStatus"
