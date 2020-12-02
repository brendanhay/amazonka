{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.PlacementStrategyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.PlacementStrategyType where

import Network.AWS.Prelude

data PlacementStrategyType
  = Binpack
  | Random
  | Spread
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

instance FromText PlacementStrategyType where
  parser =
    takeLowerText >>= \case
      "binpack" -> pure Binpack
      "random" -> pure Random
      "spread" -> pure Spread
      e ->
        fromTextError $
          "Failure parsing PlacementStrategyType from value: '" <> e
            <> "'. Accepted values: binpack, random, spread"

instance ToText PlacementStrategyType where
  toText = \case
    Binpack -> "binpack"
    Random -> "random"
    Spread -> "spread"

instance Hashable PlacementStrategyType

instance NFData PlacementStrategyType

instance ToByteString PlacementStrategyType

instance ToQuery PlacementStrategyType

instance ToHeader PlacementStrategyType

instance ToJSON PlacementStrategyType where
  toJSON = toJSONText

instance FromJSON PlacementStrategyType where
  parseJSON = parseJSONText "PlacementStrategyType"
