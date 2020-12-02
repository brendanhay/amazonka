{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.HLSPlaybackMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.HLSPlaybackMode where

import Network.AWS.Prelude

data HLSPlaybackMode
  = Live
  | LiveReplay
  | OnDemand
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

instance FromText HLSPlaybackMode where
  parser =
    takeLowerText >>= \case
      "live" -> pure Live
      "live_replay" -> pure LiveReplay
      "on_demand" -> pure OnDemand
      e ->
        fromTextError $
          "Failure parsing HLSPlaybackMode from value: '" <> e
            <> "'. Accepted values: live, live_replay, on_demand"

instance ToText HLSPlaybackMode where
  toText = \case
    Live -> "LIVE"
    LiveReplay -> "LIVE_REPLAY"
    OnDemand -> "ON_DEMAND"

instance Hashable HLSPlaybackMode

instance NFData HLSPlaybackMode

instance ToByteString HLSPlaybackMode

instance ToQuery HLSPlaybackMode

instance ToHeader HLSPlaybackMode

instance ToJSON HLSPlaybackMode where
  toJSON = toJSONText
