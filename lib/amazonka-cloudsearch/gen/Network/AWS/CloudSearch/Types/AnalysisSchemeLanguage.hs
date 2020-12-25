{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.AnalysisSchemeLanguage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.AnalysisSchemeLanguage
  ( AnalysisSchemeLanguage
      ( AnalysisSchemeLanguage',
        AnalysisSchemeLanguageAR,
        AnalysisSchemeLanguageBG,
        AnalysisSchemeLanguageCA,
        AnalysisSchemeLanguageCS,
        AnalysisSchemeLanguageDA,
        AnalysisSchemeLanguageDE,
        AnalysisSchemeLanguageEL,
        AnalysisSchemeLanguageEN,
        AnalysisSchemeLanguageES,
        AnalysisSchemeLanguageEU,
        AnalysisSchemeLanguageFA,
        AnalysisSchemeLanguageFI,
        AnalysisSchemeLanguageFR,
        AnalysisSchemeLanguageGA,
        AnalysisSchemeLanguageGL,
        AnalysisSchemeLanguageHE,
        AnalysisSchemeLanguageHI,
        AnalysisSchemeLanguageHU,
        AnalysisSchemeLanguageHY,
        AnalysisSchemeLanguageID,
        AnalysisSchemeLanguageIT,
        AnalysisSchemeLanguageJA,
        AnalysisSchemeLanguageKO,
        AnalysisSchemeLanguageLV,
        AnalysisSchemeLanguageMul,
        AnalysisSchemeLanguageNL,
        AnalysisSchemeLanguageNO,
        AnalysisSchemeLanguagePT,
        AnalysisSchemeLanguageRO,
        AnalysisSchemeLanguageRU,
        AnalysisSchemeLanguageSV,
        AnalysisSchemeLanguageTH,
        AnalysisSchemeLanguageTR,
        AnalysisSchemeLanguageZhHans,
        AnalysisSchemeLanguageZhHant,
        fromAnalysisSchemeLanguage
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | An <http://tools.ietf.org/html/rfc4646 IETF RFC 4646> language code or @mul@ for multiple languages.
newtype AnalysisSchemeLanguage = AnalysisSchemeLanguage'
  { fromAnalysisSchemeLanguage ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern AnalysisSchemeLanguageAR :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageAR = AnalysisSchemeLanguage' "ar"

pattern AnalysisSchemeLanguageBG :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageBG = AnalysisSchemeLanguage' "bg"

pattern AnalysisSchemeLanguageCA :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageCA = AnalysisSchemeLanguage' "ca"

pattern AnalysisSchemeLanguageCS :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageCS = AnalysisSchemeLanguage' "cs"

pattern AnalysisSchemeLanguageDA :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageDA = AnalysisSchemeLanguage' "da"

pattern AnalysisSchemeLanguageDE :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageDE = AnalysisSchemeLanguage' "de"

pattern AnalysisSchemeLanguageEL :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageEL = AnalysisSchemeLanguage' "el"

pattern AnalysisSchemeLanguageEN :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageEN = AnalysisSchemeLanguage' "en"

pattern AnalysisSchemeLanguageES :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageES = AnalysisSchemeLanguage' "es"

pattern AnalysisSchemeLanguageEU :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageEU = AnalysisSchemeLanguage' "eu"

pattern AnalysisSchemeLanguageFA :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageFA = AnalysisSchemeLanguage' "fa"

pattern AnalysisSchemeLanguageFI :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageFI = AnalysisSchemeLanguage' "fi"

pattern AnalysisSchemeLanguageFR :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageFR = AnalysisSchemeLanguage' "fr"

pattern AnalysisSchemeLanguageGA :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageGA = AnalysisSchemeLanguage' "ga"

pattern AnalysisSchemeLanguageGL :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageGL = AnalysisSchemeLanguage' "gl"

pattern AnalysisSchemeLanguageHE :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageHE = AnalysisSchemeLanguage' "he"

pattern AnalysisSchemeLanguageHI :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageHI = AnalysisSchemeLanguage' "hi"

pattern AnalysisSchemeLanguageHU :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageHU = AnalysisSchemeLanguage' "hu"

pattern AnalysisSchemeLanguageHY :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageHY = AnalysisSchemeLanguage' "hy"

pattern AnalysisSchemeLanguageID :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageID = AnalysisSchemeLanguage' "id"

pattern AnalysisSchemeLanguageIT :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageIT = AnalysisSchemeLanguage' "it"

pattern AnalysisSchemeLanguageJA :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageJA = AnalysisSchemeLanguage' "ja"

pattern AnalysisSchemeLanguageKO :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageKO = AnalysisSchemeLanguage' "ko"

pattern AnalysisSchemeLanguageLV :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageLV = AnalysisSchemeLanguage' "lv"

pattern AnalysisSchemeLanguageMul :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageMul = AnalysisSchemeLanguage' "mul"

pattern AnalysisSchemeLanguageNL :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageNL = AnalysisSchemeLanguage' "nl"

pattern AnalysisSchemeLanguageNO :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageNO = AnalysisSchemeLanguage' "no"

pattern AnalysisSchemeLanguagePT :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguagePT = AnalysisSchemeLanguage' "pt"

pattern AnalysisSchemeLanguageRO :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageRO = AnalysisSchemeLanguage' "ro"

pattern AnalysisSchemeLanguageRU :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageRU = AnalysisSchemeLanguage' "ru"

pattern AnalysisSchemeLanguageSV :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageSV = AnalysisSchemeLanguage' "sv"

pattern AnalysisSchemeLanguageTH :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageTH = AnalysisSchemeLanguage' "th"

pattern AnalysisSchemeLanguageTR :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageTR = AnalysisSchemeLanguage' "tr"

pattern AnalysisSchemeLanguageZhHans :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageZhHans = AnalysisSchemeLanguage' "zh-Hans"

pattern AnalysisSchemeLanguageZhHant :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguageZhHant = AnalysisSchemeLanguage' "zh-Hant"

{-# COMPLETE
  AnalysisSchemeLanguageAR,
  AnalysisSchemeLanguageBG,
  AnalysisSchemeLanguageCA,
  AnalysisSchemeLanguageCS,
  AnalysisSchemeLanguageDA,
  AnalysisSchemeLanguageDE,
  AnalysisSchemeLanguageEL,
  AnalysisSchemeLanguageEN,
  AnalysisSchemeLanguageES,
  AnalysisSchemeLanguageEU,
  AnalysisSchemeLanguageFA,
  AnalysisSchemeLanguageFI,
  AnalysisSchemeLanguageFR,
  AnalysisSchemeLanguageGA,
  AnalysisSchemeLanguageGL,
  AnalysisSchemeLanguageHE,
  AnalysisSchemeLanguageHI,
  AnalysisSchemeLanguageHU,
  AnalysisSchemeLanguageHY,
  AnalysisSchemeLanguageID,
  AnalysisSchemeLanguageIT,
  AnalysisSchemeLanguageJA,
  AnalysisSchemeLanguageKO,
  AnalysisSchemeLanguageLV,
  AnalysisSchemeLanguageMul,
  AnalysisSchemeLanguageNL,
  AnalysisSchemeLanguageNO,
  AnalysisSchemeLanguagePT,
  AnalysisSchemeLanguageRO,
  AnalysisSchemeLanguageRU,
  AnalysisSchemeLanguageSV,
  AnalysisSchemeLanguageTH,
  AnalysisSchemeLanguageTR,
  AnalysisSchemeLanguageZhHans,
  AnalysisSchemeLanguageZhHant,
  AnalysisSchemeLanguage'
  #-}
