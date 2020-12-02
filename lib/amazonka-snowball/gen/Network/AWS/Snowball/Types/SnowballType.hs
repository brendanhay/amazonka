{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.SnowballType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.SnowballType where

import Network.AWS.Prelude

data SnowballType
  = Edge
  | EdgeC
  | EdgeCg
  | EdgeS
  | SNC1Hdd
  | Standard
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

instance FromText SnowballType where
  parser =
    takeLowerText >>= \case
      "edge" -> pure Edge
      "edge_c" -> pure EdgeC
      "edge_cg" -> pure EdgeCg
      "edge_s" -> pure EdgeS
      "snc1_hdd" -> pure SNC1Hdd
      "standard" -> pure Standard
      e ->
        fromTextError $
          "Failure parsing SnowballType from value: '" <> e
            <> "'. Accepted values: edge, edge_c, edge_cg, edge_s, snc1_hdd, standard"

instance ToText SnowballType where
  toText = \case
    Edge -> "EDGE"
    EdgeC -> "EDGE_C"
    EdgeCg -> "EDGE_CG"
    EdgeS -> "EDGE_S"
    SNC1Hdd -> "SNC1_HDD"
    Standard -> "STANDARD"

instance Hashable SnowballType

instance NFData SnowballType

instance ToByteString SnowballType

instance ToQuery SnowballType

instance ToHeader SnowballType

instance ToJSON SnowballType where
  toJSON = toJSONText

instance FromJSON SnowballType where
  parseJSON = parseJSONText "SnowballType"
