{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.RoutingStrategyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.RoutingStrategyType where

import Network.AWS.Prelude

data RoutingStrategyType
  = Simple
  | Terminal
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

instance FromText RoutingStrategyType where
  parser =
    takeLowerText >>= \case
      "simple" -> pure Simple
      "terminal" -> pure Terminal
      e ->
        fromTextError $
          "Failure parsing RoutingStrategyType from value: '" <> e
            <> "'. Accepted values: simple, terminal"

instance ToText RoutingStrategyType where
  toText = \case
    Simple -> "SIMPLE"
    Terminal -> "TERMINAL"

instance Hashable RoutingStrategyType

instance NFData RoutingStrategyType

instance ToByteString RoutingStrategyType

instance ToQuery RoutingStrategyType

instance ToHeader RoutingStrategyType

instance ToJSON RoutingStrategyType where
  toJSON = toJSONText

instance FromJSON RoutingStrategyType where
  parseJSON = parseJSONText "RoutingStrategyType"
