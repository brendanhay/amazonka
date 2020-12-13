{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.LanguageCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types.LanguageCode
  ( LanguageCode
      ( LanguageCode',
        Arb,
        CmnCn,
        CyGb,
        DaDk,
        DeDe,
        EnAu,
        EnGb,
        EnGbWls,
        EnIn,
        EnUs,
        EsEs,
        EsMx,
        EsUs,
        FrCa,
        FrFr,
        IsIs,
        ItIt,
        JaJp,
        HiIn,
        KoKr,
        NbNo,
        NlNl,
        PlPl,
        PtBr,
        PtPt,
        RoRo,
        RuRu,
        SvSe,
        TrTr
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

pattern Arb :: LanguageCode
pattern Arb = LanguageCode' "arb"

pattern CmnCn :: LanguageCode
pattern CmnCn = LanguageCode' "cmn-CN"

pattern CyGb :: LanguageCode
pattern CyGb = LanguageCode' "cy-GB"

pattern DaDk :: LanguageCode
pattern DaDk = LanguageCode' "da-DK"

pattern DeDe :: LanguageCode
pattern DeDe = LanguageCode' "de-DE"

pattern EnAu :: LanguageCode
pattern EnAu = LanguageCode' "en-AU"

pattern EnGb :: LanguageCode
pattern EnGb = LanguageCode' "en-GB"

pattern EnGbWls :: LanguageCode
pattern EnGbWls = LanguageCode' "en-GB-WLS"

pattern EnIn :: LanguageCode
pattern EnIn = LanguageCode' "en-IN"

pattern EnUs :: LanguageCode
pattern EnUs = LanguageCode' "en-US"

pattern EsEs :: LanguageCode
pattern EsEs = LanguageCode' "es-ES"

pattern EsMx :: LanguageCode
pattern EsMx = LanguageCode' "es-MX"

pattern EsUs :: LanguageCode
pattern EsUs = LanguageCode' "es-US"

pattern FrCa :: LanguageCode
pattern FrCa = LanguageCode' "fr-CA"

pattern FrFr :: LanguageCode
pattern FrFr = LanguageCode' "fr-FR"

pattern IsIs :: LanguageCode
pattern IsIs = LanguageCode' "is-IS"

pattern ItIt :: LanguageCode
pattern ItIt = LanguageCode' "it-IT"

pattern JaJp :: LanguageCode
pattern JaJp = LanguageCode' "ja-JP"

pattern HiIn :: LanguageCode
pattern HiIn = LanguageCode' "hi-IN"

pattern KoKr :: LanguageCode
pattern KoKr = LanguageCode' "ko-KR"

pattern NbNo :: LanguageCode
pattern NbNo = LanguageCode' "nb-NO"

pattern NlNl :: LanguageCode
pattern NlNl = LanguageCode' "nl-NL"

pattern PlPl :: LanguageCode
pattern PlPl = LanguageCode' "pl-PL"

pattern PtBr :: LanguageCode
pattern PtBr = LanguageCode' "pt-BR"

pattern PtPt :: LanguageCode
pattern PtPt = LanguageCode' "pt-PT"

pattern RoRo :: LanguageCode
pattern RoRo = LanguageCode' "ro-RO"

pattern RuRu :: LanguageCode
pattern RuRu = LanguageCode' "ru-RU"

pattern SvSe :: LanguageCode
pattern SvSe = LanguageCode' "sv-SE"

pattern TrTr :: LanguageCode
pattern TrTr = LanguageCode' "tr-TR"

{-# COMPLETE
  Arb,
  CmnCn,
  CyGb,
  DaDk,
  DeDe,
  EnAu,
  EnGb,
  EnGbWls,
  EnIn,
  EnUs,
  EsEs,
  EsMx,
  EsUs,
  FrCa,
  FrFr,
  IsIs,
  ItIt,
  JaJp,
  HiIn,
  KoKr,
  NbNo,
  NlNl,
  PlPl,
  PtBr,
  PtPt,
  RoRo,
  RuRu,
  SvSe,
  TrTr,
  LanguageCode'
  #-}
