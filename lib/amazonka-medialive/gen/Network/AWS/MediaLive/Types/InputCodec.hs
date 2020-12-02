{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputCodec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputCodec where

import Network.AWS.Prelude

-- | codec in increasing order of complexity
data InputCodec
  = Avc
  | Hevc
  | MPEG2
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

instance FromText InputCodec where
  parser =
    takeLowerText >>= \case
      "avc" -> pure Avc
      "hevc" -> pure Hevc
      "mpeg2" -> pure MPEG2
      e ->
        fromTextError $
          "Failure parsing InputCodec from value: '" <> e
            <> "'. Accepted values: avc, hevc, mpeg2"

instance ToText InputCodec where
  toText = \case
    Avc -> "AVC"
    Hevc -> "HEVC"
    MPEG2 -> "MPEG2"

instance Hashable InputCodec

instance NFData InputCodec

instance ToByteString InputCodec

instance ToQuery InputCodec

instance ToHeader InputCodec

instance ToJSON InputCodec where
  toJSON = toJSONText

instance FromJSON InputCodec where
  parseJSON = parseJSONText "InputCodec"
