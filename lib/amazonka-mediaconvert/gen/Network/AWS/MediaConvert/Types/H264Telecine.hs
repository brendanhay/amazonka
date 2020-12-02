{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264Telecine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264Telecine where

import Network.AWS.Prelude

-- | When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard or soft telecine to create a smoother picture. Hard telecine (HARD) produces a 29.97i output. Soft telecine (SOFT) produces an output with a 23.976 output that signals to the video player device to do the conversion during play back. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
data H264Telecine
  = HHard
  | HNone
  | HSoft
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

instance FromText H264Telecine where
  parser =
    takeLowerText >>= \case
      "hard" -> pure HHard
      "none" -> pure HNone
      "soft" -> pure HSoft
      e ->
        fromTextError $
          "Failure parsing H264Telecine from value: '" <> e
            <> "'. Accepted values: hard, none, soft"

instance ToText H264Telecine where
  toText = \case
    HHard -> "HARD"
    HNone -> "NONE"
    HSoft -> "SOFT"

instance Hashable H264Telecine

instance NFData H264Telecine

instance ToByteString H264Telecine

instance ToQuery H264Telecine

instance ToHeader H264Telecine

instance ToJSON H264Telecine where
  toJSON = toJSONText

instance FromJSON H264Telecine where
  parseJSON = parseJSONText "H264Telecine"
