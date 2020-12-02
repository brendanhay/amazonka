{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2Telecine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2Telecine where

import Network.AWS.Prelude

-- | When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard or soft telecine to create a smoother picture. Hard telecine (HARD) produces a 29.97i output. Soft telecine (SOFT) produces an output with a 23.976 output that signals to the video player device to do the conversion during play back. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
data Mpeg2Telecine
  = MTHard
  | MTNone
  | MTSoft
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

instance FromText Mpeg2Telecine where
  parser =
    takeLowerText >>= \case
      "hard" -> pure MTHard
      "none" -> pure MTNone
      "soft" -> pure MTSoft
      e ->
        fromTextError $
          "Failure parsing Mpeg2Telecine from value: '" <> e
            <> "'. Accepted values: hard, none, soft"

instance ToText Mpeg2Telecine where
  toText = \case
    MTHard -> "HARD"
    MTNone -> "NONE"
    MTSoft -> "SOFT"

instance Hashable Mpeg2Telecine

instance NFData Mpeg2Telecine

instance ToByteString Mpeg2Telecine

instance ToQuery Mpeg2Telecine

instance ToHeader Mpeg2Telecine

instance ToJSON Mpeg2Telecine where
  toJSON = toJSONText

instance FromJSON Mpeg2Telecine where
  parseJSON = parseJSONText "Mpeg2Telecine"
