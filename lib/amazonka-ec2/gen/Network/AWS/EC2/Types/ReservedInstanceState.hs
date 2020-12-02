{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstanceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstanceState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ReservedInstanceState
  = Active
  | PaymentFailed
  | PaymentPending
  | Queued
  | QueuedDeleted
  | Retired
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

instance FromText ReservedInstanceState where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "payment-failed" -> pure PaymentFailed
      "payment-pending" -> pure PaymentPending
      "queued" -> pure Queued
      "queued-deleted" -> pure QueuedDeleted
      "retired" -> pure Retired
      e ->
        fromTextError $
          "Failure parsing ReservedInstanceState from value: '" <> e
            <> "'. Accepted values: active, payment-failed, payment-pending, queued, queued-deleted, retired"

instance ToText ReservedInstanceState where
  toText = \case
    Active -> "active"
    PaymentFailed -> "payment-failed"
    PaymentPending -> "payment-pending"
    Queued -> "queued"
    QueuedDeleted -> "queued-deleted"
    Retired -> "retired"

instance Hashable ReservedInstanceState

instance NFData ReservedInstanceState

instance ToByteString ReservedInstanceState

instance ToQuery ReservedInstanceState

instance ToHeader ReservedInstanceState

instance FromXML ReservedInstanceState where
  parseXML = parseXMLText "ReservedInstanceState"
