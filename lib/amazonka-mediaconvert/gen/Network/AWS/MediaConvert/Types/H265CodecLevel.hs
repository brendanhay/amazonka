{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265CodecLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265CodecLevel where

import Network.AWS.Prelude

-- | H.265 Level.
data H265CodecLevel
  = HCLAuto
  | HCLLevel1
  | HCLLevel2
  | HCLLevel21
  | HCLLevel3
  | HCLLevel31
  | HCLLevel4
  | HCLLevel41
  | HCLLevel5
  | HCLLevel51
  | HCLLevel52
  | HCLLevel6
  | HCLLevel61
  | HCLLevel62
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

instance FromText H265CodecLevel where
  parser =
    takeLowerText >>= \case
      "auto" -> pure HCLAuto
      "level_1" -> pure HCLLevel1
      "level_2" -> pure HCLLevel2
      "level_2_1" -> pure HCLLevel21
      "level_3" -> pure HCLLevel3
      "level_3_1" -> pure HCLLevel31
      "level_4" -> pure HCLLevel4
      "level_4_1" -> pure HCLLevel41
      "level_5" -> pure HCLLevel5
      "level_5_1" -> pure HCLLevel51
      "level_5_2" -> pure HCLLevel52
      "level_6" -> pure HCLLevel6
      "level_6_1" -> pure HCLLevel61
      "level_6_2" -> pure HCLLevel62
      e ->
        fromTextError $
          "Failure parsing H265CodecLevel from value: '" <> e
            <> "'. Accepted values: auto, level_1, level_2, level_2_1, level_3, level_3_1, level_4, level_4_1, level_5, level_5_1, level_5_2, level_6, level_6_1, level_6_2"

instance ToText H265CodecLevel where
  toText = \case
    HCLAuto -> "AUTO"
    HCLLevel1 -> "LEVEL_1"
    HCLLevel2 -> "LEVEL_2"
    HCLLevel21 -> "LEVEL_2_1"
    HCLLevel3 -> "LEVEL_3"
    HCLLevel31 -> "LEVEL_3_1"
    HCLLevel4 -> "LEVEL_4"
    HCLLevel41 -> "LEVEL_4_1"
    HCLLevel5 -> "LEVEL_5"
    HCLLevel51 -> "LEVEL_5_1"
    HCLLevel52 -> "LEVEL_5_2"
    HCLLevel6 -> "LEVEL_6"
    HCLLevel61 -> "LEVEL_6_1"
    HCLLevel62 -> "LEVEL_6_2"

instance Hashable H265CodecLevel

instance NFData H265CodecLevel

instance ToByteString H265CodecLevel

instance ToQuery H265CodecLevel

instance ToHeader H265CodecLevel

instance ToJSON H265CodecLevel where
  toJSON = toJSONText

instance FromJSON H265CodecLevel where
  parseJSON = parseJSONText "H265CodecLevel"
