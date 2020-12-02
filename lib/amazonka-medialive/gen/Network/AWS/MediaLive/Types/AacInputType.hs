{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AacInputType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AacInputType where

import Network.AWS.Prelude

-- | Aac Input Type
data AacInputType
  = BroadcasterMixedAd
  | Normal
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

instance FromText AacInputType where
  parser =
    takeLowerText >>= \case
      "broadcaster_mixed_ad" -> pure BroadcasterMixedAd
      "normal" -> pure Normal
      e ->
        fromTextError $
          "Failure parsing AacInputType from value: '" <> e
            <> "'. Accepted values: broadcaster_mixed_ad, normal"

instance ToText AacInputType where
  toText = \case
    BroadcasterMixedAd -> "BROADCASTER_MIXED_AD"
    Normal -> "NORMAL"

instance Hashable AacInputType

instance NFData AacInputType

instance ToByteString AacInputType

instance ToQuery AacInputType

instance ToHeader AacInputType

instance ToJSON AacInputType where
  toJSON = toJSONText

instance FromJSON AacInputType where
  parseJSON = parseJSONText "AacInputType"
