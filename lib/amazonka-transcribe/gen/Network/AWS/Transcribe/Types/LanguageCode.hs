-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.LanguageCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.LanguageCode
  ( LanguageCode
      ( LanguageCode',
        AfZa,
        ArAe,
        ArSa,
        CyGb,
        DaDk,
        DeCh,
        DeDe,
        EnAb,
        EnAu,
        EnGb,
        EnIe,
        EnIn,
        EnUs,
        EnWl,
        EsEs,
        EsUs,
        FaIr,
        FrCa,
        FrFr,
        GaIe,
        GdGb,
        HeIl,
        HiIn,
        IdId,
        ItIt,
        JaJp,
        KoKr,
        MsMy,
        NlNl,
        PtBr,
        PtPt,
        RuRu,
        TaIn,
        TeIn,
        TrTr,
        ZhCn
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LanguageCode = LanguageCode' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AfZa :: LanguageCode
pattern AfZa = LanguageCode' "af-ZA"

pattern ArAe :: LanguageCode
pattern ArAe = LanguageCode' "ar-AE"

pattern ArSa :: LanguageCode
pattern ArSa = LanguageCode' "ar-SA"

pattern CyGb :: LanguageCode
pattern CyGb = LanguageCode' "cy-GB"

pattern DaDk :: LanguageCode
pattern DaDk = LanguageCode' "da-DK"

pattern DeCh :: LanguageCode
pattern DeCh = LanguageCode' "de-CH"

pattern DeDe :: LanguageCode
pattern DeDe = LanguageCode' "de-DE"

pattern EnAb :: LanguageCode
pattern EnAb = LanguageCode' "en-AB"

pattern EnAu :: LanguageCode
pattern EnAu = LanguageCode' "en-AU"

pattern EnGb :: LanguageCode
pattern EnGb = LanguageCode' "en-GB"

pattern EnIe :: LanguageCode
pattern EnIe = LanguageCode' "en-IE"

pattern EnIn :: LanguageCode
pattern EnIn = LanguageCode' "en-IN"

pattern EnUs :: LanguageCode
pattern EnUs = LanguageCode' "en-US"

pattern EnWl :: LanguageCode
pattern EnWl = LanguageCode' "en-WL"

pattern EsEs :: LanguageCode
pattern EsEs = LanguageCode' "es-ES"

pattern EsUs :: LanguageCode
pattern EsUs = LanguageCode' "es-US"

pattern FaIr :: LanguageCode
pattern FaIr = LanguageCode' "fa-IR"

pattern FrCa :: LanguageCode
pattern FrCa = LanguageCode' "fr-CA"

pattern FrFr :: LanguageCode
pattern FrFr = LanguageCode' "fr-FR"

pattern GaIe :: LanguageCode
pattern GaIe = LanguageCode' "ga-IE"

pattern GdGb :: LanguageCode
pattern GdGb = LanguageCode' "gd-GB"

pattern HeIl :: LanguageCode
pattern HeIl = LanguageCode' "he-IL"

pattern HiIn :: LanguageCode
pattern HiIn = LanguageCode' "hi-IN"

pattern IdId :: LanguageCode
pattern IdId = LanguageCode' "id-ID"

pattern ItIt :: LanguageCode
pattern ItIt = LanguageCode' "it-IT"

pattern JaJp :: LanguageCode
pattern JaJp = LanguageCode' "ja-JP"

pattern KoKr :: LanguageCode
pattern KoKr = LanguageCode' "ko-KR"

pattern MsMy :: LanguageCode
pattern MsMy = LanguageCode' "ms-MY"

pattern NlNl :: LanguageCode
pattern NlNl = LanguageCode' "nl-NL"

pattern PtBr :: LanguageCode
pattern PtBr = LanguageCode' "pt-BR"

pattern PtPt :: LanguageCode
pattern PtPt = LanguageCode' "pt-PT"

pattern RuRu :: LanguageCode
pattern RuRu = LanguageCode' "ru-RU"

pattern TaIn :: LanguageCode
pattern TaIn = LanguageCode' "ta-IN"

pattern TeIn :: LanguageCode
pattern TeIn = LanguageCode' "te-IN"

pattern TrTr :: LanguageCode
pattern TrTr = LanguageCode' "tr-TR"

pattern ZhCn :: LanguageCode
pattern ZhCn = LanguageCode' "zh-CN"

{-# COMPLETE
  AfZa,
  ArAe,
  ArSa,
  CyGb,
  DaDk,
  DeCh,
  DeDe,
  EnAb,
  EnAu,
  EnGb,
  EnIe,
  EnIn,
  EnUs,
  EnWl,
  EsEs,
  EsUs,
  FaIr,
  FrCa,
  FrFr,
  GaIe,
  GdGb,
  HeIl,
  HiIn,
  IdId,
  ItIt,
  JaJp,
  KoKr,
  MsMy,
  NlNl,
  PtBr,
  PtPt,
  RuRu,
  TaIn,
  TeIn,
  TrTr,
  ZhCn,
  LanguageCode'
  #-}
