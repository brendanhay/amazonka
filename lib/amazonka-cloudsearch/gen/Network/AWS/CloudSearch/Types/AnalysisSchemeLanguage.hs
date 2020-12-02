{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.AnalysisSchemeLanguage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.AnalysisSchemeLanguage where

import Network.AWS.Prelude

-- | An <http://tools.ietf.org/html/rfc4646 IETF RFC 4646> language code or @mul@ for multiple languages.
data AnalysisSchemeLanguage
  = AR
  | BG
  | CA
  | CS
  | DA
  | DE
  | EL
  | EN
  | ES
  | EU
  | FA
  | FI
  | FR
  | GA
  | GL
  | HE
  | HI
  | HU
  | HY
  | IT
  | Id
  | JA
  | KO
  | LV
  | Mul
  | NL
  | NO
  | PT
  | RO
  | RU
  | SV
  | TH
  | TR
  | ZhHans
  | ZhHant
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

instance FromText AnalysisSchemeLanguage where
  parser =
    takeLowerText >>= \case
      "ar" -> pure AR
      "bg" -> pure BG
      "ca" -> pure CA
      "cs" -> pure CS
      "da" -> pure DA
      "de" -> pure DE
      "el" -> pure EL
      "en" -> pure EN
      "es" -> pure ES
      "eu" -> pure EU
      "fa" -> pure FA
      "fi" -> pure FI
      "fr" -> pure FR
      "ga" -> pure GA
      "gl" -> pure GL
      "he" -> pure HE
      "hi" -> pure HI
      "hu" -> pure HU
      "hy" -> pure HY
      "it" -> pure IT
      "id" -> pure Id
      "ja" -> pure JA
      "ko" -> pure KO
      "lv" -> pure LV
      "mul" -> pure Mul
      "nl" -> pure NL
      "no" -> pure NO
      "pt" -> pure PT
      "ro" -> pure RO
      "ru" -> pure RU
      "sv" -> pure SV
      "th" -> pure TH
      "tr" -> pure TR
      "zh-hans" -> pure ZhHans
      "zh-hant" -> pure ZhHant
      e ->
        fromTextError $
          "Failure parsing AnalysisSchemeLanguage from value: '" <> e
            <> "'. Accepted values: ar, bg, ca, cs, da, de, el, en, es, eu, fa, fi, fr, ga, gl, he, hi, hu, hy, it, id, ja, ko, lv, mul, nl, no, pt, ro, ru, sv, th, tr, zh-hans, zh-hant"

instance ToText AnalysisSchemeLanguage where
  toText = \case
    AR -> "ar"
    BG -> "bg"
    CA -> "ca"
    CS -> "cs"
    DA -> "da"
    DE -> "de"
    EL -> "el"
    EN -> "en"
    ES -> "es"
    EU -> "eu"
    FA -> "fa"
    FI -> "fi"
    FR -> "fr"
    GA -> "ga"
    GL -> "gl"
    HE -> "he"
    HI -> "hi"
    HU -> "hu"
    HY -> "hy"
    IT -> "it"
    Id -> "id"
    JA -> "ja"
    KO -> "ko"
    LV -> "lv"
    Mul -> "mul"
    NL -> "nl"
    NO -> "no"
    PT -> "pt"
    RO -> "ro"
    RU -> "ru"
    SV -> "sv"
    TH -> "th"
    TR -> "tr"
    ZhHans -> "zh-Hans"
    ZhHant -> "zh-Hant"

instance Hashable AnalysisSchemeLanguage

instance NFData AnalysisSchemeLanguage

instance ToByteString AnalysisSchemeLanguage

instance ToQuery AnalysisSchemeLanguage

instance ToHeader AnalysisSchemeLanguage

instance FromXML AnalysisSchemeLanguage where
  parseXML = parseXMLText "AnalysisSchemeLanguage"
