{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.StreamOrder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.StreamOrder where

import Network.AWS.Prelude

data StreamOrder
  = Original
  | VideoBitrateAscending
  | VideoBitrateDescending
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

instance FromText StreamOrder where
  parser =
    takeLowerText >>= \case
      "original" -> pure Original
      "video_bitrate_ascending" -> pure VideoBitrateAscending
      "video_bitrate_descending" -> pure VideoBitrateDescending
      e ->
        fromTextError $
          "Failure parsing StreamOrder from value: '" <> e
            <> "'. Accepted values: original, video_bitrate_ascending, video_bitrate_descending"

instance ToText StreamOrder where
  toText = \case
    Original -> "ORIGINAL"
    VideoBitrateAscending -> "VIDEO_BITRATE_ASCENDING"
    VideoBitrateDescending -> "VIDEO_BITRATE_DESCENDING"

instance Hashable StreamOrder

instance NFData StreamOrder

instance ToByteString StreamOrder

instance ToQuery StreamOrder

instance ToHeader StreamOrder

instance ToJSON StreamOrder where
  toJSON = toJSONText

instance FromJSON StreamOrder where
  parseJSON = parseJSONText "StreamOrder"
