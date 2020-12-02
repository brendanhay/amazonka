{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CancelSpotInstanceRequestState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CancelSpotInstanceRequestState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data CancelSpotInstanceRequestState
  = CSIRSActive
  | CSIRSCancelled
  | CSIRSClosed
  | CSIRSCompleted
  | CSIRSOpen
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

instance FromText CancelSpotInstanceRequestState where
  parser =
    takeLowerText >>= \case
      "active" -> pure CSIRSActive
      "cancelled" -> pure CSIRSCancelled
      "closed" -> pure CSIRSClosed
      "completed" -> pure CSIRSCompleted
      "open" -> pure CSIRSOpen
      e ->
        fromTextError $
          "Failure parsing CancelSpotInstanceRequestState from value: '" <> e
            <> "'. Accepted values: active, cancelled, closed, completed, open"

instance ToText CancelSpotInstanceRequestState where
  toText = \case
    CSIRSActive -> "active"
    CSIRSCancelled -> "cancelled"
    CSIRSClosed -> "closed"
    CSIRSCompleted -> "completed"
    CSIRSOpen -> "open"

instance Hashable CancelSpotInstanceRequestState

instance NFData CancelSpotInstanceRequestState

instance ToByteString CancelSpotInstanceRequestState

instance ToQuery CancelSpotInstanceRequestState

instance ToHeader CancelSpotInstanceRequestState

instance FromXML CancelSpotInstanceRequestState where
  parseXML = parseXMLText "CancelSpotInstanceRequestState"
