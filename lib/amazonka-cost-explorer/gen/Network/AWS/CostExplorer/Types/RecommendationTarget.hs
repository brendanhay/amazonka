{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.RecommendationTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RecommendationTarget where

import Network.AWS.Prelude

data RecommendationTarget
  = CrossInstanceFamily
  | SameInstanceFamily
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

instance FromText RecommendationTarget where
  parser =
    takeLowerText >>= \case
      "cross_instance_family" -> pure CrossInstanceFamily
      "same_instance_family" -> pure SameInstanceFamily
      e ->
        fromTextError $
          "Failure parsing RecommendationTarget from value: '" <> e
            <> "'. Accepted values: cross_instance_family, same_instance_family"

instance ToText RecommendationTarget where
  toText = \case
    CrossInstanceFamily -> "CROSS_INSTANCE_FAMILY"
    SameInstanceFamily -> "SAME_INSTANCE_FAMILY"

instance Hashable RecommendationTarget

instance NFData RecommendationTarget

instance ToByteString RecommendationTarget

instance ToQuery RecommendationTarget

instance ToHeader RecommendationTarget

instance ToJSON RecommendationTarget where
  toJSON = toJSONText

instance FromJSON RecommendationTarget where
  parseJSON = parseJSONText "RecommendationTarget"
