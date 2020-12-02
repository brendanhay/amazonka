{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MxfAfdSignaling
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MxfAfdSignaling where

import Network.AWS.Prelude

-- | Optional. When you have AFD signaling set up in your output video stream, use this setting to choose whether to also include it in the MXF wrapper. Choose Don't copy (NO_COPY) to exclude AFD signaling from the MXF wrapper. Choose Copy from video stream (COPY_FROM_VIDEO) to copy the AFD values from the video stream for this output to the MXF wrapper. Regardless of which option you choose, the AFD values remain in the video stream. Related settings: To set up your output to include or exclude AFD values, see AfdSignaling, under VideoDescription. On the console, find AFD signaling under the output's video encoding settings.
data MxfAfdSignaling
  = CopyFromVideo
  | NoCopy
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

instance FromText MxfAfdSignaling where
  parser =
    takeLowerText >>= \case
      "copy_from_video" -> pure CopyFromVideo
      "no_copy" -> pure NoCopy
      e ->
        fromTextError $
          "Failure parsing MxfAfdSignaling from value: '" <> e
            <> "'. Accepted values: copy_from_video, no_copy"

instance ToText MxfAfdSignaling where
  toText = \case
    CopyFromVideo -> "COPY_FROM_VIDEO"
    NoCopy -> "NO_COPY"

instance Hashable MxfAfdSignaling

instance NFData MxfAfdSignaling

instance ToByteString MxfAfdSignaling

instance ToQuery MxfAfdSignaling

instance ToHeader MxfAfdSignaling

instance ToJSON MxfAfdSignaling where
  toJSON = toJSONText

instance FromJSON MxfAfdSignaling where
  parseJSON = parseJSONText "MxfAfdSignaling"
