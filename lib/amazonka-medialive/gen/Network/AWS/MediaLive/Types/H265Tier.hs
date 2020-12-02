{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265Tier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265Tier where

import Network.AWS.Prelude

-- | H265 Tier
data H265Tier
  = HTHigh
  | HTMain
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

instance FromText H265Tier where
  parser =
    takeLowerText >>= \case
      "high" -> pure HTHigh
      "main" -> pure HTMain
      e ->
        fromTextError $
          "Failure parsing H265Tier from value: '" <> e
            <> "'. Accepted values: high, main"

instance ToText H265Tier where
  toText = \case
    HTHigh -> "HIGH"
    HTMain -> "MAIN"

instance Hashable H265Tier

instance NFData H265Tier

instance ToByteString H265Tier

instance ToQuery H265Tier

instance ToHeader H265Tier

instance ToJSON H265Tier where
  toJSON = toJSONText

instance FromJSON H265Tier where
  parseJSON = parseJSONText "H265Tier"
