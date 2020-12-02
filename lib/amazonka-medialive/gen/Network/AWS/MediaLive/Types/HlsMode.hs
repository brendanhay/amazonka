{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsMode where

import Network.AWS.Prelude

-- | Hls Mode
data HlsMode
  = Live
  | Vod
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

instance FromText HlsMode where
  parser =
    takeLowerText >>= \case
      "live" -> pure Live
      "vod" -> pure Vod
      e ->
        fromTextError $
          "Failure parsing HlsMode from value: '" <> e
            <> "'. Accepted values: live, vod"

instance ToText HlsMode where
  toText = \case
    Live -> "LIVE"
    Vod -> "VOD"

instance Hashable HlsMode

instance NFData HlsMode

instance ToByteString HlsMode

instance ToQuery HlsMode

instance ToHeader HlsMode

instance ToJSON HlsMode where
  toJSON = toJSONText

instance FromJSON HlsMode where
  parseJSON = parseJSONText "HlsMode"
