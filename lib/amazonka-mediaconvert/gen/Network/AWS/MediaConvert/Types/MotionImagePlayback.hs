{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MotionImagePlayback
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MotionImagePlayback where

import Network.AWS.Prelude

-- | Specify whether your motion graphic overlay repeats on a loop or plays only once.
data MotionImagePlayback
  = Once
  | Repeat
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

instance FromText MotionImagePlayback where
  parser =
    takeLowerText >>= \case
      "once" -> pure Once
      "repeat" -> pure Repeat
      e ->
        fromTextError $
          "Failure parsing MotionImagePlayback from value: '" <> e
            <> "'. Accepted values: once, repeat"

instance ToText MotionImagePlayback where
  toText = \case
    Once -> "ONCE"
    Repeat -> "REPEAT"

instance Hashable MotionImagePlayback

instance NFData MotionImagePlayback

instance ToByteString MotionImagePlayback

instance ToQuery MotionImagePlayback

instance ToHeader MotionImagePlayback

instance ToJSON MotionImagePlayback where
  toJSON = toJSONText

instance FromJSON MotionImagePlayback where
  parseJSON = parseJSONText "MotionImagePlayback"
