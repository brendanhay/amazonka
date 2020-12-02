{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ListingStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ListingStatus where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ListingStatus
  = LSActive
  | LSCancelled
  | LSClosed
  | LSPending
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

instance FromText ListingStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure LSActive
      "cancelled" -> pure LSCancelled
      "closed" -> pure LSClosed
      "pending" -> pure LSPending
      e ->
        fromTextError $
          "Failure parsing ListingStatus from value: '" <> e
            <> "'. Accepted values: active, cancelled, closed, pending"

instance ToText ListingStatus where
  toText = \case
    LSActive -> "active"
    LSCancelled -> "cancelled"
    LSClosed -> "closed"
    LSPending -> "pending"

instance Hashable ListingStatus

instance NFData ListingStatus

instance ToByteString ListingStatus

instance ToQuery ListingStatus

instance ToHeader ListingStatus

instance FromXML ListingStatus where
  parseXML = parseXMLText "ListingStatus"
