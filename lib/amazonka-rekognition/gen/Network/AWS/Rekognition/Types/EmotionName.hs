{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.EmotionName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.EmotionName where

import Network.AWS.Prelude

data EmotionName
  = Angry
  | Calm
  | Confused
  | Disgusted
  | Fear
  | Happy
  | Sad
  | Surprised
  | Unknown
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

instance FromText EmotionName where
  parser =
    takeLowerText >>= \case
      "angry" -> pure Angry
      "calm" -> pure Calm
      "confused" -> pure Confused
      "disgusted" -> pure Disgusted
      "fear" -> pure Fear
      "happy" -> pure Happy
      "sad" -> pure Sad
      "surprised" -> pure Surprised
      "unknown" -> pure Unknown
      e ->
        fromTextError $
          "Failure parsing EmotionName from value: '" <> e
            <> "'. Accepted values: angry, calm, confused, disgusted, fear, happy, sad, surprised, unknown"

instance ToText EmotionName where
  toText = \case
    Angry -> "ANGRY"
    Calm -> "CALM"
    Confused -> "CONFUSED"
    Disgusted -> "DISGUSTED"
    Fear -> "FEAR"
    Happy -> "HAPPY"
    Sad -> "SAD"
    Surprised -> "SURPRISED"
    Unknown -> "UNKNOWN"

instance Hashable EmotionName

instance NFData EmotionName

instance ToByteString EmotionName

instance ToQuery EmotionName

instance ToHeader EmotionName

instance FromJSON EmotionName where
  parseJSON = parseJSONText "EmotionName"
