{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.SccDestinationFramerate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.SccDestinationFramerate where

import Network.AWS.Prelude

-- | Set Framerate (SccDestinationFramerate) to make sure that the captions and the video are synchronized in the output. Specify a frame rate that matches the frame rate of the associated video. If the video frame rate is 29.97, choose 29.97 dropframe (FRAMERATE_29_97_DROPFRAME) only if the video has video_insertion=true and drop_frame_timecode=true; otherwise, choose 29.97 non-dropframe (FRAMERATE_29_97_NON_DROPFRAME).
data SccDestinationFramerate
  = Framerate2397
  | Framerate24
  | Framerate25
  | Framerate2997Dropframe
  | Framerate2997NonDropframe
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

instance FromText SccDestinationFramerate where
  parser =
    takeLowerText >>= \case
      "framerate_23_97" -> pure Framerate2397
      "framerate_24" -> pure Framerate24
      "framerate_25" -> pure Framerate25
      "framerate_29_97_dropframe" -> pure Framerate2997Dropframe
      "framerate_29_97_non_dropframe" -> pure Framerate2997NonDropframe
      e ->
        fromTextError $
          "Failure parsing SccDestinationFramerate from value: '" <> e
            <> "'. Accepted values: framerate_23_97, framerate_24, framerate_25, framerate_29_97_dropframe, framerate_29_97_non_dropframe"

instance ToText SccDestinationFramerate where
  toText = \case
    Framerate2397 -> "FRAMERATE_23_97"
    Framerate24 -> "FRAMERATE_24"
    Framerate25 -> "FRAMERATE_25"
    Framerate2997Dropframe -> "FRAMERATE_29_97_DROPFRAME"
    Framerate2997NonDropframe -> "FRAMERATE_29_97_NON_DROPFRAME"

instance Hashable SccDestinationFramerate

instance NFData SccDestinationFramerate

instance ToByteString SccDestinationFramerate

instance ToQuery SccDestinationFramerate

instance ToHeader SccDestinationFramerate

instance ToJSON SccDestinationFramerate where
  toJSON = toJSONText

instance FromJSON SccDestinationFramerate where
  parseJSON = parseJSONText "SccDestinationFramerate"
