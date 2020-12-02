{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.SpeechMarkType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types.SpeechMarkType where

import Network.AWS.Prelude

data SpeechMarkType
  = Sentence
  | Ssml
  | Viseme
  | Word
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

instance FromText SpeechMarkType where
  parser =
    takeLowerText >>= \case
      "sentence" -> pure Sentence
      "ssml" -> pure Ssml
      "viseme" -> pure Viseme
      "word" -> pure Word
      e ->
        fromTextError $
          "Failure parsing SpeechMarkType from value: '" <> e
            <> "'. Accepted values: sentence, ssml, viseme, word"

instance ToText SpeechMarkType where
  toText = \case
    Sentence -> "sentence"
    Ssml -> "ssml"
    Viseme -> "viseme"
    Word -> "word"

instance Hashable SpeechMarkType

instance NFData SpeechMarkType

instance ToByteString SpeechMarkType

instance ToQuery SpeechMarkType

instance ToHeader SpeechMarkType

instance ToJSON SpeechMarkType where
  toJSON = toJSONText

instance FromJSON SpeechMarkType where
  parseJSON = parseJSONText "SpeechMarkType"
