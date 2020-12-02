{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.LanguageCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.LanguageCode where

import Network.AWS.Prelude

data LanguageCode
  = AfZa
  | ArAe
  | ArSa
  | CyGb
  | DaDk
  | DeCh
  | DeDe
  | EnAb
  | EnAu
  | EnGb
  | EnIe
  | EnIn
  | EnUs
  | EnWl
  | EsEs
  | EsUs
  | FaIr
  | FrCa
  | FrFr
  | GaIe
  | GdGb
  | HeIl
  | HiIn
  | IdId
  | ItIt
  | JaJp
  | KoKr
  | MsMy
  | NlNl
  | PtBr
  | PtPt
  | RuRu
  | TaIn
  | TeIn
  | TrTr
  | ZhCn
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
      "af-za" -> pure AfZa
      "ar-ae" -> pure ArAe
      "ar-sa" -> pure ArSa
      "cy-gb" -> pure CyGb
      "da-dk" -> pure DaDk
      "de-ch" -> pure DeCh
      "de-de" -> pure DeDe
      "en-ab" -> pure EnAb
      "en-au" -> pure EnAu
      "en-gb" -> pure EnGb
      "en-ie" -> pure EnIe
      "en-in" -> pure EnIn
      "en-us" -> pure EnUs
      "en-wl" -> pure EnWl
      "es-es" -> pure EsEs
      "es-us" -> pure EsUs
      "fa-ir" -> pure FaIr
      "fr-ca" -> pure FrCa
      "fr-fr" -> pure FrFr
      "ga-ie" -> pure GaIe
      "gd-gb" -> pure GdGb
      "he-il" -> pure HeIl
      "hi-in" -> pure HiIn
      "id-id" -> pure IdId
      "it-it" -> pure ItIt
      "ja-jp" -> pure JaJp
      "ko-kr" -> pure KoKr
      "ms-my" -> pure MsMy
      "nl-nl" -> pure NlNl
      "pt-br" -> pure PtBr
      "pt-pt" -> pure PtPt
      "ru-ru" -> pure RuRu
      "ta-in" -> pure TaIn
      "te-in" -> pure TeIn
      "tr-tr" -> pure TrTr
      "zh-cn" -> pure ZhCn
      e ->
        fromTextError $
          "Failure parsing LanguageCode from value: '" <> e
            <> "'. Accepted values: af-za, ar-ae, ar-sa, cy-gb, da-dk, de-ch, de-de, en-ab, en-au, en-gb, en-ie, en-in, en-us, en-wl, es-es, es-us, fa-ir, fr-ca, fr-fr, ga-ie, gd-gb, he-il, hi-in, id-id, it-it, ja-jp, ko-kr, ms-my, nl-nl, pt-br, pt-pt, ru-ru, ta-in, te-in, tr-tr, zh-cn"

instance ToText LanguageCode where
  toText = \case
    AfZa -> "af-ZA"
    ArAe -> "ar-AE"
    ArSa -> "ar-SA"
    CyGb -> "cy-GB"
    DaDk -> "da-DK"
    DeCh -> "de-CH"
    DeDe -> "de-DE"
    EnAb -> "en-AB"
    EnAu -> "en-AU"
    EnGb -> "en-GB"
    EnIe -> "en-IE"
    EnIn -> "en-IN"
    EnUs -> "en-US"
    EnWl -> "en-WL"
    EsEs -> "es-ES"
    EsUs -> "es-US"
    FaIr -> "fa-IR"
    FrCa -> "fr-CA"
    FrFr -> "fr-FR"
    GaIe -> "ga-IE"
    GdGb -> "gd-GB"
    HeIl -> "he-IL"
    HiIn -> "hi-IN"
    IdId -> "id-ID"
    ItIt -> "it-IT"
    JaJp -> "ja-JP"
    KoKr -> "ko-KR"
    MsMy -> "ms-MY"
    NlNl -> "nl-NL"
    PtBr -> "pt-BR"
    PtPt -> "pt-PT"
    RuRu -> "ru-RU"
    TaIn -> "ta-IN"
    TeIn -> "te-IN"
    TrTr -> "tr-TR"
    ZhCn -> "zh-CN"

instance Hashable LanguageCode

instance NFData LanguageCode

instance ToByteString LanguageCode

instance ToQuery LanguageCode

instance ToHeader LanguageCode

instance ToJSON LanguageCode where
  toJSON = toJSONText

instance FromJSON LanguageCode where
  parseJSON = parseJSONText "LanguageCode"
