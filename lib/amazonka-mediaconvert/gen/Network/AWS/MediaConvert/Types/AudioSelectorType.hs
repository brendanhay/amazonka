{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioSelectorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioSelectorType where

import Network.AWS.Prelude

-- | Specifies the type of the audio selector.
data AudioSelectorType
  = LanguageCode
  | Pid
  | Track
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

instance FromText AudioSelectorType where
  parser =
    takeLowerText >>= \case
      "language_code" -> pure LanguageCode
      "pid" -> pure Pid
      "track" -> pure Track
      e ->
        fromTextError $
          "Failure parsing AudioSelectorType from value: '" <> e
            <> "'. Accepted values: language_code, pid, track"

instance ToText AudioSelectorType where
  toText = \case
    LanguageCode -> "LANGUAGE_CODE"
    Pid -> "PID"
    Track -> "TRACK"

instance Hashable AudioSelectorType

instance NFData AudioSelectorType

instance ToByteString AudioSelectorType

instance ToQuery AudioSelectorType

instance ToHeader AudioSelectorType

instance ToJSON AudioSelectorType where
  toJSON = toJSONText

instance FromJSON AudioSelectorType where
  parseJSON = parseJSONText "AudioSelectorType"
