{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.InteractionMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.InteractionMode where

import Network.AWS.Prelude

data InteractionMode
  = Interactive
  | NoVideo
  | VideoOnly
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

instance FromText InteractionMode where
  parser =
    takeLowerText >>= \case
      "interactive" -> pure Interactive
      "no_video" -> pure NoVideo
      "video_only" -> pure VideoOnly
      e ->
        fromTextError $
          "Failure parsing InteractionMode from value: '" <> e
            <> "'. Accepted values: interactive, no_video, video_only"

instance ToText InteractionMode where
  toText = \case
    Interactive -> "INTERACTIVE"
    NoVideo -> "NO_VIDEO"
    VideoOnly -> "VIDEO_ONLY"

instance Hashable InteractionMode

instance NFData InteractionMode

instance ToByteString InteractionMode

instance ToQuery InteractionMode

instance ToHeader InteractionMode

instance ToJSON InteractionMode where
  toJSON = toJSONText

instance FromJSON InteractionMode where
  parseJSON = parseJSONText "InteractionMode"
