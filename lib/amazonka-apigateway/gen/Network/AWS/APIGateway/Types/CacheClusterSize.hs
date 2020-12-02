{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.CacheClusterSize
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.CacheClusterSize where

import Network.AWS.Prelude

-- | Returns the size of the __CacheCluster__ .
data CacheClusterSize
  = D0_5
  | D118
  | D13_5
  | D1_6
  | D237
  | D28_4
  | D58_2
  | D6_1
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

instance FromText CacheClusterSize where
  parser =
    takeLowerText >>= \case
      "0.5" -> pure D0_5
      "118" -> pure D118
      "13.5" -> pure D13_5
      "1.6" -> pure D1_6
      "237" -> pure D237
      "28.4" -> pure D28_4
      "58.2" -> pure D58_2
      "6.1" -> pure D6_1
      e ->
        fromTextError $
          "Failure parsing CacheClusterSize from value: '" <> e
            <> "'. Accepted values: 0.5, 118, 13.5, 1.6, 237, 28.4, 58.2, 6.1"

instance ToText CacheClusterSize where
  toText = \case
    D0_5 -> "0.5"
    D118 -> "118"
    D13_5 -> "13.5"
    D1_6 -> "1.6"
    D237 -> "237"
    D28_4 -> "28.4"
    D58_2 -> "58.2"
    D6_1 -> "6.1"

instance Hashable CacheClusterSize

instance NFData CacheClusterSize

instance ToByteString CacheClusterSize

instance ToQuery CacheClusterSize

instance ToHeader CacheClusterSize

instance ToJSON CacheClusterSize where
  toJSON = toJSONText

instance FromJSON CacheClusterSize where
  parseJSON = parseJSONText "CacheClusterSize"
