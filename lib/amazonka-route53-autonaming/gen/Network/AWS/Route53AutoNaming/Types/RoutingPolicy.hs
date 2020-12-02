{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.RoutingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.RoutingPolicy where

import Network.AWS.Prelude

data RoutingPolicy
  = Multivalue
  | Weighted
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

instance FromText RoutingPolicy where
  parser =
    takeLowerText >>= \case
      "multivalue" -> pure Multivalue
      "weighted" -> pure Weighted
      e ->
        fromTextError $
          "Failure parsing RoutingPolicy from value: '" <> e
            <> "'. Accepted values: multivalue, weighted"

instance ToText RoutingPolicy where
  toText = \case
    Multivalue -> "MULTIVALUE"
    Weighted -> "WEIGHTED"

instance Hashable RoutingPolicy

instance NFData RoutingPolicy

instance ToByteString RoutingPolicy

instance ToQuery RoutingPolicy

instance ToHeader RoutingPolicy

instance ToJSON RoutingPolicy where
  toJSON = toJSONText

instance FromJSON RoutingPolicy where
  parseJSON = parseJSONText "RoutingPolicy"
