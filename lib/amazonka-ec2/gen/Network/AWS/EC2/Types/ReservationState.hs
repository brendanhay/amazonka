{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservationState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ReservationState
  = RSActive
  | RSPaymentFailed
  | RSPaymentPending
  | RSRetired
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

instance FromText ReservationState where
  parser =
    takeLowerText >>= \case
      "active" -> pure RSActive
      "payment-failed" -> pure RSPaymentFailed
      "payment-pending" -> pure RSPaymentPending
      "retired" -> pure RSRetired
      e ->
        fromTextError $
          "Failure parsing ReservationState from value: '" <> e
            <> "'. Accepted values: active, payment-failed, payment-pending, retired"

instance ToText ReservationState where
  toText = \case
    RSActive -> "active"
    RSPaymentFailed -> "payment-failed"
    RSPaymentPending -> "payment-pending"
    RSRetired -> "retired"

instance Hashable ReservationState

instance NFData ReservationState

instance ToByteString ReservationState

instance ToQuery ReservationState

instance ToHeader ReservationState

instance FromXML ReservationState where
  parseXML = parseXMLText "ReservationState"
