{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioNormalizationAlgorithmControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioNormalizationAlgorithmControl where

import Network.AWS.Prelude

-- | Audio Normalization Algorithm Control
data AudioNormalizationAlgorithmControl = CorrectAudio
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

instance FromText AudioNormalizationAlgorithmControl where
  parser =
    takeLowerText >>= \case
      "correct_audio" -> pure CorrectAudio
      e ->
        fromTextError $
          "Failure parsing AudioNormalizationAlgorithmControl from value: '" <> e
            <> "'. Accepted values: correct_audio"

instance ToText AudioNormalizationAlgorithmControl where
  toText = \case
    CorrectAudio -> "CORRECT_AUDIO"

instance Hashable AudioNormalizationAlgorithmControl

instance NFData AudioNormalizationAlgorithmControl

instance ToByteString AudioNormalizationAlgorithmControl

instance ToQuery AudioNormalizationAlgorithmControl

instance ToHeader AudioNormalizationAlgorithmControl

instance ToJSON AudioNormalizationAlgorithmControl where
  toJSON = toJSONText

instance FromJSON AudioNormalizationAlgorithmControl where
  parseJSON = parseJSONText "AudioNormalizationAlgorithmControl"
