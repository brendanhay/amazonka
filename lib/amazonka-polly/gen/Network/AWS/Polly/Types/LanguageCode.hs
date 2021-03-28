{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.LanguageCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Polly.Types.LanguageCode
  ( LanguageCode
    ( LanguageCode'
    , LanguageCodeArb
    , LanguageCodeCmnCn
    , LanguageCodeCyGb
    , LanguageCodeDaDk
    , LanguageCodeDeDe
    , LanguageCodeEnAu
    , LanguageCodeEnGb
    , LanguageCodeEnGbWls
    , LanguageCodeEnIn
    , LanguageCodeEnUs
    , LanguageCodeEsEs
    , LanguageCodeEsMx
    , LanguageCodeEsUs
    , LanguageCodeFrCa
    , LanguageCodeFrFr
    , LanguageCodeIsIs
    , LanguageCodeItIt
    , LanguageCodeJaJp
    , LanguageCodeHiIn
    , LanguageCodeKoKr
    , LanguageCodeNbNo
    , LanguageCodeNlNl
    , LanguageCodePlPl
    , LanguageCodePtBr
    , LanguageCodePtPt
    , LanguageCodeRoRo
    , LanguageCodeRuRu
    , LanguageCodeSvSe
    , LanguageCodeTrTr
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

pattern LanguageCodeArb :: LanguageCode
pattern LanguageCodeArb = LanguageCode' "arb"

pattern LanguageCodeCmnCn :: LanguageCode
pattern LanguageCodeCmnCn = LanguageCode' "cmn-CN"

pattern LanguageCodeCyGb :: LanguageCode
pattern LanguageCodeCyGb = LanguageCode' "cy-GB"

pattern LanguageCodeDaDk :: LanguageCode
pattern LanguageCodeDaDk = LanguageCode' "da-DK"

pattern LanguageCodeDeDe :: LanguageCode
pattern LanguageCodeDeDe = LanguageCode' "de-DE"

pattern LanguageCodeEnAu :: LanguageCode
pattern LanguageCodeEnAu = LanguageCode' "en-AU"

pattern LanguageCodeEnGb :: LanguageCode
pattern LanguageCodeEnGb = LanguageCode' "en-GB"

pattern LanguageCodeEnGbWls :: LanguageCode
pattern LanguageCodeEnGbWls = LanguageCode' "en-GB-WLS"

pattern LanguageCodeEnIn :: LanguageCode
pattern LanguageCodeEnIn = LanguageCode' "en-IN"

pattern LanguageCodeEnUs :: LanguageCode
pattern LanguageCodeEnUs = LanguageCode' "en-US"

pattern LanguageCodeEsEs :: LanguageCode
pattern LanguageCodeEsEs = LanguageCode' "es-ES"

pattern LanguageCodeEsMx :: LanguageCode
pattern LanguageCodeEsMx = LanguageCode' "es-MX"

pattern LanguageCodeEsUs :: LanguageCode
pattern LanguageCodeEsUs = LanguageCode' "es-US"

pattern LanguageCodeFrCa :: LanguageCode
pattern LanguageCodeFrCa = LanguageCode' "fr-CA"

pattern LanguageCodeFrFr :: LanguageCode
pattern LanguageCodeFrFr = LanguageCode' "fr-FR"

pattern LanguageCodeIsIs :: LanguageCode
pattern LanguageCodeIsIs = LanguageCode' "is-IS"

pattern LanguageCodeItIt :: LanguageCode
pattern LanguageCodeItIt = LanguageCode' "it-IT"

pattern LanguageCodeJaJp :: LanguageCode
pattern LanguageCodeJaJp = LanguageCode' "ja-JP"

pattern LanguageCodeHiIn :: LanguageCode
pattern LanguageCodeHiIn = LanguageCode' "hi-IN"

pattern LanguageCodeKoKr :: LanguageCode
pattern LanguageCodeKoKr = LanguageCode' "ko-KR"

pattern LanguageCodeNbNo :: LanguageCode
pattern LanguageCodeNbNo = LanguageCode' "nb-NO"

pattern LanguageCodeNlNl :: LanguageCode
pattern LanguageCodeNlNl = LanguageCode' "nl-NL"

pattern LanguageCodePlPl :: LanguageCode
pattern LanguageCodePlPl = LanguageCode' "pl-PL"

pattern LanguageCodePtBr :: LanguageCode
pattern LanguageCodePtBr = LanguageCode' "pt-BR"

pattern LanguageCodePtPt :: LanguageCode
pattern LanguageCodePtPt = LanguageCode' "pt-PT"

pattern LanguageCodeRoRo :: LanguageCode
pattern LanguageCodeRoRo = LanguageCode' "ro-RO"

pattern LanguageCodeRuRu :: LanguageCode
pattern LanguageCodeRuRu = LanguageCode' "ru-RU"

pattern LanguageCodeSvSe :: LanguageCode
pattern LanguageCodeSvSe = LanguageCode' "sv-SE"

pattern LanguageCodeTrTr :: LanguageCode
pattern LanguageCodeTrTr = LanguageCode' "tr-TR"

{-# COMPLETE 
  LanguageCodeArb,

  LanguageCodeCmnCn,

  LanguageCodeCyGb,

  LanguageCodeDaDk,

  LanguageCodeDeDe,

  LanguageCodeEnAu,

  LanguageCodeEnGb,

  LanguageCodeEnGbWls,

  LanguageCodeEnIn,

  LanguageCodeEnUs,

  LanguageCodeEsEs,

  LanguageCodeEsMx,

  LanguageCodeEsUs,

  LanguageCodeFrCa,

  LanguageCodeFrFr,

  LanguageCodeIsIs,

  LanguageCodeItIt,

  LanguageCodeJaJp,

  LanguageCodeHiIn,

  LanguageCodeKoKr,

  LanguageCodeNbNo,

  LanguageCodeNlNl,

  LanguageCodePlPl,

  LanguageCodePtBr,

  LanguageCodePtPt,

  LanguageCodeRoRo,

  LanguageCodeRuRu,

  LanguageCodeSvSe,

  LanguageCodeTrTr,
  LanguageCode'
  #-}
