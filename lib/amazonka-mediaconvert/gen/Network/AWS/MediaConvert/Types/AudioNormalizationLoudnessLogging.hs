{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioNormalizationLoudnessLogging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioNormalizationLoudnessLogging where

import Network.AWS.Prelude

-- | If set to LOG, log each output's audio track loudness to a CSV file.
data AudioNormalizationLoudnessLogging
  = DontLog
  | Log
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

instance FromText AudioNormalizationLoudnessLogging where
  parser =
    takeLowerText >>= \case
      "dont_log" -> pure DontLog
      "log" -> pure Log
      e ->
        fromTextError $
          "Failure parsing AudioNormalizationLoudnessLogging from value: '" <> e
            <> "'. Accepted values: dont_log, log"

instance ToText AudioNormalizationLoudnessLogging where
  toText = \case
    DontLog -> "DONT_LOG"
    Log -> "LOG"

instance Hashable AudioNormalizationLoudnessLogging

instance NFData AudioNormalizationLoudnessLogging

instance ToByteString AudioNormalizationLoudnessLogging

instance ToQuery AudioNormalizationLoudnessLogging

instance ToHeader AudioNormalizationLoudnessLogging

instance ToJSON AudioNormalizationLoudnessLogging where
  toJSON = toJSONText

instance FromJSON AudioNormalizationLoudnessLogging where
  parseJSON = parseJSONText "AudioNormalizationLoudnessLogging"
