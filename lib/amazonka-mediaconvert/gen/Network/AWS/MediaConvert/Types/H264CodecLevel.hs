{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264CodecLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264CodecLevel where

import Network.AWS.Prelude

-- | Specify an H.264 level that is consistent with your output video settings. If you aren't sure what level to specify, choose Auto (AUTO).
data H264CodecLevel
  = HAuto
  | HLevel1
  | HLevel11
  | HLevel12
  | HLevel13
  | HLevel2
  | HLevel21
  | HLevel22
  | HLevel3
  | HLevel31
  | HLevel32
  | HLevel4
  | HLevel41
  | HLevel42
  | HLevel5
  | HLevel51
  | HLevel52
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

instance FromText H264CodecLevel where
  parser =
    takeLowerText >>= \case
      "auto" -> pure HAuto
      "level_1" -> pure HLevel1
      "level_1_1" -> pure HLevel11
      "level_1_2" -> pure HLevel12
      "level_1_3" -> pure HLevel13
      "level_2" -> pure HLevel2
      "level_2_1" -> pure HLevel21
      "level_2_2" -> pure HLevel22
      "level_3" -> pure HLevel3
      "level_3_1" -> pure HLevel31
      "level_3_2" -> pure HLevel32
      "level_4" -> pure HLevel4
      "level_4_1" -> pure HLevel41
      "level_4_2" -> pure HLevel42
      "level_5" -> pure HLevel5
      "level_5_1" -> pure HLevel51
      "level_5_2" -> pure HLevel52
      e ->
        fromTextError $
          "Failure parsing H264CodecLevel from value: '" <> e
            <> "'. Accepted values: auto, level_1, level_1_1, level_1_2, level_1_3, level_2, level_2_1, level_2_2, level_3, level_3_1, level_3_2, level_4, level_4_1, level_4_2, level_5, level_5_1, level_5_2"

instance ToText H264CodecLevel where
  toText = \case
    HAuto -> "AUTO"
    HLevel1 -> "LEVEL_1"
    HLevel11 -> "LEVEL_1_1"
    HLevel12 -> "LEVEL_1_2"
    HLevel13 -> "LEVEL_1_3"
    HLevel2 -> "LEVEL_2"
    HLevel21 -> "LEVEL_2_1"
    HLevel22 -> "LEVEL_2_2"
    HLevel3 -> "LEVEL_3"
    HLevel31 -> "LEVEL_3_1"
    HLevel32 -> "LEVEL_3_2"
    HLevel4 -> "LEVEL_4"
    HLevel41 -> "LEVEL_4_1"
    HLevel42 -> "LEVEL_4_2"
    HLevel5 -> "LEVEL_5"
    HLevel51 -> "LEVEL_5_1"
    HLevel52 -> "LEVEL_5_2"

instance Hashable H264CodecLevel

instance NFData H264CodecLevel

instance ToByteString H264CodecLevel

instance ToQuery H264CodecLevel

instance ToHeader H264CodecLevel

instance ToJSON H264CodecLevel where
  toJSON = toJSONText

instance FromJSON H264CodecLevel where
  parseJSON = parseJSONText "H264CodecLevel"
