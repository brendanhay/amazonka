{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264Level
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264Level where

import Network.AWS.Prelude

-- | H264 Level
data H264Level
  = H264Level1
  | H264Level11
  | H264Level12
  | H264Level13
  | H264Level2
  | H264Level21
  | H264Level22
  | H264Level3
  | H264Level31
  | H264Level32
  | H264Level4
  | H264Level41
  | H264Level42
  | H264Level5
  | H264Level51
  | H264Level52
  | H264LevelAuto
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

instance FromText H264Level where
  parser =
    takeLowerText >>= \case
      "h264_level_1" -> pure H264Level1
      "h264_level_1_1" -> pure H264Level11
      "h264_level_1_2" -> pure H264Level12
      "h264_level_1_3" -> pure H264Level13
      "h264_level_2" -> pure H264Level2
      "h264_level_2_1" -> pure H264Level21
      "h264_level_2_2" -> pure H264Level22
      "h264_level_3" -> pure H264Level3
      "h264_level_3_1" -> pure H264Level31
      "h264_level_3_2" -> pure H264Level32
      "h264_level_4" -> pure H264Level4
      "h264_level_4_1" -> pure H264Level41
      "h264_level_4_2" -> pure H264Level42
      "h264_level_5" -> pure H264Level5
      "h264_level_5_1" -> pure H264Level51
      "h264_level_5_2" -> pure H264Level52
      "h264_level_auto" -> pure H264LevelAuto
      e ->
        fromTextError $
          "Failure parsing H264Level from value: '" <> e
            <> "'. Accepted values: h264_level_1, h264_level_1_1, h264_level_1_2, h264_level_1_3, h264_level_2, h264_level_2_1, h264_level_2_2, h264_level_3, h264_level_3_1, h264_level_3_2, h264_level_4, h264_level_4_1, h264_level_4_2, h264_level_5, h264_level_5_1, h264_level_5_2, h264_level_auto"

instance ToText H264Level where
  toText = \case
    H264Level1 -> "H264_LEVEL_1"
    H264Level11 -> "H264_LEVEL_1_1"
    H264Level12 -> "H264_LEVEL_1_2"
    H264Level13 -> "H264_LEVEL_1_3"
    H264Level2 -> "H264_LEVEL_2"
    H264Level21 -> "H264_LEVEL_2_1"
    H264Level22 -> "H264_LEVEL_2_2"
    H264Level3 -> "H264_LEVEL_3"
    H264Level31 -> "H264_LEVEL_3_1"
    H264Level32 -> "H264_LEVEL_3_2"
    H264Level4 -> "H264_LEVEL_4"
    H264Level41 -> "H264_LEVEL_4_1"
    H264Level42 -> "H264_LEVEL_4_2"
    H264Level5 -> "H264_LEVEL_5"
    H264Level51 -> "H264_LEVEL_5_1"
    H264Level52 -> "H264_LEVEL_5_2"
    H264LevelAuto -> "H264_LEVEL_AUTO"

instance Hashable H264Level

instance NFData H264Level

instance ToByteString H264Level

instance ToQuery H264Level

instance ToHeader H264Level

instance ToJSON H264Level where
  toJSON = toJSONText

instance FromJSON H264Level where
  parseJSON = parseJSONText "H264Level"
