{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ListingState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ListingState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ListingState
  = LAvailable
  | LCancelled
  | LPending
  | LSold
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

instance FromText ListingState where
  parser =
    takeLowerText >>= \case
      "available" -> pure LAvailable
      "cancelled" -> pure LCancelled
      "pending" -> pure LPending
      "sold" -> pure LSold
      e ->
        fromTextError $
          "Failure parsing ListingState from value: '" <> e
            <> "'. Accepted values: available, cancelled, pending, sold"

instance ToText ListingState where
  toText = \case
    LAvailable -> "available"
    LCancelled -> "cancelled"
    LPending -> "pending"
    LSold -> "sold"

instance Hashable ListingState

instance NFData ListingState

instance ToByteString ListingState

instance ToQuery ListingState

instance ToHeader ListingState

instance FromXML ListingState where
  parseXML = parseXMLText "ListingState"
