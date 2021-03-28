{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.LanguageCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Transcribe.Types.LanguageCode
  ( LanguageCode
    ( LanguageCode'
    , LanguageCodeAfZa
    , LanguageCodeArAe
    , LanguageCodeArSa
    , LanguageCodeCyGb
    , LanguageCodeDaDk
    , LanguageCodeDeCh
    , LanguageCodeDeDe
    , LanguageCodeEnAb
    , LanguageCodeEnAu
    , LanguageCodeEnGb
    , LanguageCodeEnIe
    , LanguageCodeEnIn
    , LanguageCodeEnUs
    , LanguageCodeEnWl
    , LanguageCodeEsEs
    , LanguageCodeEsUs
    , LanguageCodeFaIr
    , LanguageCodeFrCa
    , LanguageCodeFrFr
    , LanguageCodeGaIe
    , LanguageCodeGdGb
    , LanguageCodeHeIl
    , LanguageCodeHiIn
    , LanguageCodeIdId
    , LanguageCodeItIt
    , LanguageCodeJaJp
    , LanguageCodeKoKr
    , LanguageCodeMsMy
    , LanguageCodeNlNl
    , LanguageCodePtBr
    , LanguageCodePtPt
    , LanguageCodeRuRu
    , LanguageCodeTaIn
    , LanguageCodeTeIn
    , LanguageCodeTrTr
    , LanguageCodeZhCn
    , fromLanguageCode
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype LanguageCode = LanguageCode'{fromLanguageCode :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern LanguageCodeAfZa :: LanguageCode
pattern LanguageCodeAfZa = LanguageCode' "af-ZA"

pattern LanguageCodeArAe :: LanguageCode
pattern LanguageCodeArAe = LanguageCode' "ar-AE"

pattern LanguageCodeArSa :: LanguageCode
pattern LanguageCodeArSa = LanguageCode' "ar-SA"

pattern LanguageCodeCyGb :: LanguageCode
pattern LanguageCodeCyGb = LanguageCode' "cy-GB"

pattern LanguageCodeDaDk :: LanguageCode
pattern LanguageCodeDaDk = LanguageCode' "da-DK"

pattern LanguageCodeDeCh :: LanguageCode
pattern LanguageCodeDeCh = LanguageCode' "de-CH"

pattern LanguageCodeDeDe :: LanguageCode
pattern LanguageCodeDeDe = LanguageCode' "de-DE"

pattern LanguageCodeEnAb :: LanguageCode
pattern LanguageCodeEnAb = LanguageCode' "en-AB"

pattern LanguageCodeEnAu :: LanguageCode
pattern LanguageCodeEnAu = LanguageCode' "en-AU"

pattern LanguageCodeEnGb :: LanguageCode
pattern LanguageCodeEnGb = LanguageCode' "en-GB"

pattern LanguageCodeEnIe :: LanguageCode
pattern LanguageCodeEnIe = LanguageCode' "en-IE"

pattern LanguageCodeEnIn :: LanguageCode
pattern LanguageCodeEnIn = LanguageCode' "en-IN"

pattern LanguageCodeEnUs :: LanguageCode
pattern LanguageCodeEnUs = LanguageCode' "en-US"

pattern LanguageCodeEnWl :: LanguageCode
pattern LanguageCodeEnWl = LanguageCode' "en-WL"

pattern LanguageCodeEsEs :: LanguageCode
pattern LanguageCodeEsEs = LanguageCode' "es-ES"

pattern LanguageCodeEsUs :: LanguageCode
pattern LanguageCodeEsUs = LanguageCode' "es-US"

pattern LanguageCodeFaIr :: LanguageCode
pattern LanguageCodeFaIr = LanguageCode' "fa-IR"

pattern LanguageCodeFrCa :: LanguageCode
pattern LanguageCodeFrCa = LanguageCode' "fr-CA"

pattern LanguageCodeFrFr :: LanguageCode
pattern LanguageCodeFrFr = LanguageCode' "fr-FR"

pattern LanguageCodeGaIe :: LanguageCode
pattern LanguageCodeGaIe = LanguageCode' "ga-IE"

pattern LanguageCodeGdGb :: LanguageCode
pattern LanguageCodeGdGb = LanguageCode' "gd-GB"

pattern LanguageCodeHeIl :: LanguageCode
pattern LanguageCodeHeIl = LanguageCode' "he-IL"

pattern LanguageCodeHiIn :: LanguageCode
pattern LanguageCodeHiIn = LanguageCode' "hi-IN"

pattern LanguageCodeIdId :: LanguageCode
pattern LanguageCodeIdId = LanguageCode' "id-ID"

pattern LanguageCodeItIt :: LanguageCode
pattern LanguageCodeItIt = LanguageCode' "it-IT"

pattern LanguageCodeJaJp :: LanguageCode
pattern LanguageCodeJaJp = LanguageCode' "ja-JP"

pattern LanguageCodeKoKr :: LanguageCode
pattern LanguageCodeKoKr = LanguageCode' "ko-KR"

pattern LanguageCodeMsMy :: LanguageCode
pattern LanguageCodeMsMy = LanguageCode' "ms-MY"

pattern LanguageCodeNlNl :: LanguageCode
pattern LanguageCodeNlNl = LanguageCode' "nl-NL"

pattern LanguageCodePtBr :: LanguageCode
pattern LanguageCodePtBr = LanguageCode' "pt-BR"

pattern LanguageCodePtPt :: LanguageCode
pattern LanguageCodePtPt = LanguageCode' "pt-PT"

pattern LanguageCodeRuRu :: LanguageCode
pattern LanguageCodeRuRu = LanguageCode' "ru-RU"

pattern LanguageCodeTaIn :: LanguageCode
pattern LanguageCodeTaIn = LanguageCode' "ta-IN"

pattern LanguageCodeTeIn :: LanguageCode
pattern LanguageCodeTeIn = LanguageCode' "te-IN"

pattern LanguageCodeTrTr :: LanguageCode
pattern LanguageCodeTrTr = LanguageCode' "tr-TR"

pattern LanguageCodeZhCn :: LanguageCode
pattern LanguageCodeZhCn = LanguageCode' "zh-CN"

{-# COMPLETE 
  LanguageCodeAfZa,

  LanguageCodeArAe,

  LanguageCodeArSa,

  LanguageCodeCyGb,

  LanguageCodeDaDk,

  LanguageCodeDeCh,

  LanguageCodeDeDe,

  LanguageCodeEnAb,

  LanguageCodeEnAu,

  LanguageCodeEnGb,

  LanguageCodeEnIe,

  LanguageCodeEnIn,

  LanguageCodeEnUs,

  LanguageCodeEnWl,

  LanguageCodeEsEs,

  LanguageCodeEsUs,

  LanguageCodeFaIr,

  LanguageCodeFrCa,

  LanguageCodeFrFr,

  LanguageCodeGaIe,

  LanguageCodeGdGb,

  LanguageCodeHeIl,

  LanguageCodeHiIn,

  LanguageCodeIdId,

  LanguageCodeItIt,

  LanguageCodeJaJp,

  LanguageCodeKoKr,

  LanguageCodeMsMy,

  LanguageCodeNlNl,

  LanguageCodePtBr,

  LanguageCodePtPt,

  LanguageCodeRuRu,

  LanguageCodeTaIn,

  LanguageCodeTeIn,

  LanguageCodeTrTr,

  LanguageCodeZhCn,
  LanguageCode'
  #-}
