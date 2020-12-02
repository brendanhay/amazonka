{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.LocaleType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.LocaleType where

import Network.AWS.Prelude

data LocaleType
  = DE
  | Default
  | EN
  | ES
  | FR
  | JA
  | KO
  | PtBr
  | RU
  | ZhCn
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

instance FromText LocaleType where
  parser =
    takeLowerText >>= \case
      "de" -> pure DE
      "default" -> pure Default
      "en" -> pure EN
      "es" -> pure ES
      "fr" -> pure FR
      "ja" -> pure JA
      "ko" -> pure KO
      "pt_br" -> pure PtBr
      "ru" -> pure RU
      "zh_cn" -> pure ZhCn
      "zh_tw" -> pure ZhTw
      e ->
        fromTextError $
          "Failure parsing LocaleType from value: '" <> e
            <> "'. Accepted values: de, default, en, es, fr, ja, ko, pt_br, ru, zh_cn, zh_tw"

instance ToText LocaleType where
  toText = \case
    DE -> "de"
    Default -> "default"
    EN -> "en"
    ES -> "es"
    FR -> "fr"
    JA -> "ja"
    KO -> "ko"
    PtBr -> "pt_BR"
    RU -> "ru"
    ZhCn -> "zh_CN"
    ZhTw -> "zh_TW"

instance Hashable LocaleType

instance NFData LocaleType

instance ToByteString LocaleType

instance ToQuery LocaleType

instance ToHeader LocaleType

instance ToJSON LocaleType where
  toJSON = toJSONText

instance FromJSON LocaleType where
  parseJSON = parseJSONText "LocaleType"
