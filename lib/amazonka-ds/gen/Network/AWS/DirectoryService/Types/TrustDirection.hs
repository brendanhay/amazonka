{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.TrustDirection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.TrustDirection where

import Network.AWS.Prelude

data TrustDirection
  = OneWayIncoming
  | OneWayOutgoing
  | TwoWay
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

instance FromText TrustDirection where
  parser =
    takeLowerText >>= \case
      "one-way: incoming" -> pure OneWayIncoming
      "one-way: outgoing" -> pure OneWayOutgoing
      "two-way" -> pure TwoWay
      e ->
        fromTextError $
          "Failure parsing TrustDirection from value: '" <> e
            <> "'. Accepted values: one-way: incoming, one-way: outgoing, two-way"

instance ToText TrustDirection where
  toText = \case
    OneWayIncoming -> "One-Way: Incoming"
    OneWayOutgoing -> "One-Way: Outgoing"
    TwoWay -> "Two-Way"

instance Hashable TrustDirection

instance NFData TrustDirection

instance ToByteString TrustDirection

instance ToQuery TrustDirection

instance ToHeader TrustDirection

instance ToJSON TrustDirection where
  toJSON = toJSONText

instance FromJSON TrustDirection where
  parseJSON = parseJSONText "TrustDirection"
