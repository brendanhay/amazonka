{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetActivityStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetActivityStatus where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data FleetActivityStatus
  = Error'
  | Fulfilled
  | PendingFulfillment
  | PendingTermination
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

instance FromText FleetActivityStatus where
  parser =
    takeLowerText >>= \case
      "error" -> pure Error'
      "fulfilled" -> pure Fulfilled
      "pending_fulfillment" -> pure PendingFulfillment
      "pending_termination" -> pure PendingTermination
      e ->
        fromTextError $
          "Failure parsing FleetActivityStatus from value: '" <> e
            <> "'. Accepted values: error, fulfilled, pending_fulfillment, pending_termination"

instance ToText FleetActivityStatus where
  toText = \case
    Error' -> "error"
    Fulfilled -> "fulfilled"
    PendingFulfillment -> "pending_fulfillment"
    PendingTermination -> "pending_termination"

instance Hashable FleetActivityStatus

instance NFData FleetActivityStatus

instance ToByteString FleetActivityStatus

instance ToQuery FleetActivityStatus

instance ToHeader FleetActivityStatus

instance FromXML FleetActivityStatus where
  parseXML = parseXMLText "FleetActivityStatus"
