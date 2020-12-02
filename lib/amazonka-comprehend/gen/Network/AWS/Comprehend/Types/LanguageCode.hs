{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.LanguageCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.LanguageCode where

import Network.AWS.Prelude

data LanguageCode
  = AR
  | DE
  | EN
  | ES
  | FR
  | HI
  | IT
  | JA
  | KO
  | PT
  | ZH
  | ZhTw
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

instance FromText LanguageCode where
  parser =
    takeLowerText >>= \case
      "ar" -> pure AR
      "de" -> pure DE
      "en" -> pure EN
      "es" -> pure ES
      "fr" -> pure FR
      "hi" -> pure HI
      "it" -> pure IT
      "ja" -> pure JA
      "ko" -> pure KO
      "pt" -> pure PT
      "zh" -> pure ZH
      "zh-tw" -> pure ZhTw
      e ->
        fromTextError $
          "Failure parsing LanguageCode from value: '" <> e
            <> "'. Accepted values: ar, de, en, es, fr, hi, it, ja, ko, pt, zh, zh-tw"

instance ToText LanguageCode where
  toText = \case
    AR -> "ar"
    DE -> "de"
    EN -> "en"
    ES -> "es"
    FR -> "fr"
    HI -> "hi"
    IT -> "it"
    JA -> "ja"
    KO -> "ko"
    PT -> "pt"
    ZH -> "zh"
    ZhTw -> "zh-TW"

instance Hashable LanguageCode

instance NFData LanguageCode

instance ToByteString LanguageCode

instance ToQuery LanguageCode

instance ToHeader LanguageCode

instance ToJSON LanguageCode where
  toJSON = toJSONText

instance FromJSON LanguageCode where
  parseJSON = parseJSONText "LanguageCode"
