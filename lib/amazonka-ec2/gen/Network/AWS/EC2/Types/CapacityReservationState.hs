{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservationState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data CapacityReservationState
  = CRSActive
  | CRSCancelled
  | CRSExpired
  | CRSFailed
  | CRSPending
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

instance FromText CapacityReservationState where
  parser =
    takeLowerText >>= \case
      "active" -> pure CRSActive
      "cancelled" -> pure CRSCancelled
      "expired" -> pure CRSExpired
      "failed" -> pure CRSFailed
      "pending" -> pure CRSPending
      e ->
        fromTextError $
          "Failure parsing CapacityReservationState from value: '" <> e
            <> "'. Accepted values: active, cancelled, expired, failed, pending"

instance ToText CapacityReservationState where
  toText = \case
    CRSActive -> "active"
    CRSCancelled -> "cancelled"
    CRSExpired -> "expired"
    CRSFailed -> "failed"
    CRSPending -> "pending"

instance Hashable CapacityReservationState

instance NFData CapacityReservationState

instance ToByteString CapacityReservationState

instance ToQuery CapacityReservationState

instance ToHeader CapacityReservationState

instance FromXML CapacityReservationState where
  parseXML = parseXMLText "CapacityReservationState"
