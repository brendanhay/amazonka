{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265Level
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265Level where

import Network.AWS.Prelude

-- | H265 Level
data H265Level
  = H265Level1
  | H265Level2
  | H265Level21
  | H265Level3
  | H265Level31
  | H265Level4
  | H265Level41
  | H265Level5
  | H265Level51
  | H265Level52
  | H265Level6
  | H265Level61
  | H265Level62
  | H265LevelAuto
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

instance FromText H265Level where
  parser =
    takeLowerText >>= \case
      "h265_level_1" -> pure H265Level1
      "h265_level_2" -> pure H265Level2
      "h265_level_2_1" -> pure H265Level21
      "h265_level_3" -> pure H265Level3
      "h265_level_3_1" -> pure H265Level31
      "h265_level_4" -> pure H265Level4
      "h265_level_4_1" -> pure H265Level41
      "h265_level_5" -> pure H265Level5
      "h265_level_5_1" -> pure H265Level51
      "h265_level_5_2" -> pure H265Level52
      "h265_level_6" -> pure H265Level6
      "h265_level_6_1" -> pure H265Level61
      "h265_level_6_2" -> pure H265Level62
      "h265_level_auto" -> pure H265LevelAuto
      e ->
        fromTextError $
          "Failure parsing H265Level from value: '" <> e
            <> "'. Accepted values: h265_level_1, h265_level_2, h265_level_2_1, h265_level_3, h265_level_3_1, h265_level_4, h265_level_4_1, h265_level_5, h265_level_5_1, h265_level_5_2, h265_level_6, h265_level_6_1, h265_level_6_2, h265_level_auto"

instance ToText H265Level where
  toText = \case
    H265Level1 -> "H265_LEVEL_1"
    H265Level2 -> "H265_LEVEL_2"
    H265Level21 -> "H265_LEVEL_2_1"
    H265Level3 -> "H265_LEVEL_3"
    H265Level31 -> "H265_LEVEL_3_1"
    H265Level4 -> "H265_LEVEL_4"
    H265Level41 -> "H265_LEVEL_4_1"
    H265Level5 -> "H265_LEVEL_5"
    H265Level51 -> "H265_LEVEL_5_1"
    H265Level52 -> "H265_LEVEL_5_2"
    H265Level6 -> "H265_LEVEL_6"
    H265Level61 -> "H265_LEVEL_6_1"
    H265Level62 -> "H265_LEVEL_6_2"
    H265LevelAuto -> "H265_LEVEL_AUTO"

instance Hashable H265Level

instance NFData H265Level

instance ToByteString H265Level

instance ToQuery H265Level

instance ToHeader H265Level

instance ToJSON H265Level where
  toJSON = toJSONText

instance FromJSON H265Level where
  parseJSON = parseJSONText "H265Level"
