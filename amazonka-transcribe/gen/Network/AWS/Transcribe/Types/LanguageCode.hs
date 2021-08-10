{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.LanguageCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.LanguageCode
  ( LanguageCode
      ( ..,
        LanguageCode_Af_ZA,
        LanguageCode_Ar_AE,
        LanguageCode_Ar_SA,
        LanguageCode_Cy_GB,
        LanguageCode_Da_DK,
        LanguageCode_De_CH,
        LanguageCode_De_DE,
        LanguageCode_En_AB,
        LanguageCode_En_AU,
        LanguageCode_En_GB,
        LanguageCode_En_IE,
        LanguageCode_En_IN,
        LanguageCode_En_US,
        LanguageCode_En_WL,
        LanguageCode_Es_ES,
        LanguageCode_Es_US,
        LanguageCode_Fa_IR,
        LanguageCode_Fr_CA,
        LanguageCode_Fr_FR,
        LanguageCode_Ga_IE,
        LanguageCode_Gd_GB,
        LanguageCode_He_IL,
        LanguageCode_Hi_IN,
        LanguageCode_Id_ID,
        LanguageCode_It_IT,
        LanguageCode_Ja_JP,
        LanguageCode_Ko_KR,
        LanguageCode_Ms_MY,
        LanguageCode_Nl_NL,
        LanguageCode_Pt_BR,
        LanguageCode_Pt_PT,
        LanguageCode_Ru_RU,
        LanguageCode_Ta_IN,
        LanguageCode_Te_IN,
        LanguageCode_Tr_TR,
        LanguageCode_Zh_CN
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype LanguageCode = LanguageCode'
  { fromLanguageCode ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern LanguageCode_Af_ZA :: LanguageCode
pattern LanguageCode_Af_ZA = LanguageCode' "af-ZA"

pattern LanguageCode_Ar_AE :: LanguageCode
pattern LanguageCode_Ar_AE = LanguageCode' "ar-AE"

pattern LanguageCode_Ar_SA :: LanguageCode
pattern LanguageCode_Ar_SA = LanguageCode' "ar-SA"

pattern LanguageCode_Cy_GB :: LanguageCode
pattern LanguageCode_Cy_GB = LanguageCode' "cy-GB"

pattern LanguageCode_Da_DK :: LanguageCode
pattern LanguageCode_Da_DK = LanguageCode' "da-DK"

pattern LanguageCode_De_CH :: LanguageCode
pattern LanguageCode_De_CH = LanguageCode' "de-CH"

pattern LanguageCode_De_DE :: LanguageCode
pattern LanguageCode_De_DE = LanguageCode' "de-DE"

pattern LanguageCode_En_AB :: LanguageCode
pattern LanguageCode_En_AB = LanguageCode' "en-AB"

pattern LanguageCode_En_AU :: LanguageCode
pattern LanguageCode_En_AU = LanguageCode' "en-AU"

pattern LanguageCode_En_GB :: LanguageCode
pattern LanguageCode_En_GB = LanguageCode' "en-GB"

pattern LanguageCode_En_IE :: LanguageCode
pattern LanguageCode_En_IE = LanguageCode' "en-IE"

pattern LanguageCode_En_IN :: LanguageCode
pattern LanguageCode_En_IN = LanguageCode' "en-IN"

pattern LanguageCode_En_US :: LanguageCode
pattern LanguageCode_En_US = LanguageCode' "en-US"

pattern LanguageCode_En_WL :: LanguageCode
pattern LanguageCode_En_WL = LanguageCode' "en-WL"

pattern LanguageCode_Es_ES :: LanguageCode
pattern LanguageCode_Es_ES = LanguageCode' "es-ES"

pattern LanguageCode_Es_US :: LanguageCode
pattern LanguageCode_Es_US = LanguageCode' "es-US"

pattern LanguageCode_Fa_IR :: LanguageCode
pattern LanguageCode_Fa_IR = LanguageCode' "fa-IR"

pattern LanguageCode_Fr_CA :: LanguageCode
pattern LanguageCode_Fr_CA = LanguageCode' "fr-CA"

pattern LanguageCode_Fr_FR :: LanguageCode
pattern LanguageCode_Fr_FR = LanguageCode' "fr-FR"

pattern LanguageCode_Ga_IE :: LanguageCode
pattern LanguageCode_Ga_IE = LanguageCode' "ga-IE"

pattern LanguageCode_Gd_GB :: LanguageCode
pattern LanguageCode_Gd_GB = LanguageCode' "gd-GB"

pattern LanguageCode_He_IL :: LanguageCode
pattern LanguageCode_He_IL = LanguageCode' "he-IL"

pattern LanguageCode_Hi_IN :: LanguageCode
pattern LanguageCode_Hi_IN = LanguageCode' "hi-IN"

pattern LanguageCode_Id_ID :: LanguageCode
pattern LanguageCode_Id_ID = LanguageCode' "id-ID"

pattern LanguageCode_It_IT :: LanguageCode
pattern LanguageCode_It_IT = LanguageCode' "it-IT"

pattern LanguageCode_Ja_JP :: LanguageCode
pattern LanguageCode_Ja_JP = LanguageCode' "ja-JP"

pattern LanguageCode_Ko_KR :: LanguageCode
pattern LanguageCode_Ko_KR = LanguageCode' "ko-KR"

pattern LanguageCode_Ms_MY :: LanguageCode
pattern LanguageCode_Ms_MY = LanguageCode' "ms-MY"

pattern LanguageCode_Nl_NL :: LanguageCode
pattern LanguageCode_Nl_NL = LanguageCode' "nl-NL"

pattern LanguageCode_Pt_BR :: LanguageCode
pattern LanguageCode_Pt_BR = LanguageCode' "pt-BR"

pattern LanguageCode_Pt_PT :: LanguageCode
pattern LanguageCode_Pt_PT = LanguageCode' "pt-PT"

pattern LanguageCode_Ru_RU :: LanguageCode
pattern LanguageCode_Ru_RU = LanguageCode' "ru-RU"

pattern LanguageCode_Ta_IN :: LanguageCode
pattern LanguageCode_Ta_IN = LanguageCode' "ta-IN"

pattern LanguageCode_Te_IN :: LanguageCode
pattern LanguageCode_Te_IN = LanguageCode' "te-IN"

pattern LanguageCode_Tr_TR :: LanguageCode
pattern LanguageCode_Tr_TR = LanguageCode' "tr-TR"

pattern LanguageCode_Zh_CN :: LanguageCode
pattern LanguageCode_Zh_CN = LanguageCode' "zh-CN"

{-# COMPLETE
  LanguageCode_Af_ZA,
  LanguageCode_Ar_AE,
  LanguageCode_Ar_SA,
  LanguageCode_Cy_GB,
  LanguageCode_Da_DK,
  LanguageCode_De_CH,
  LanguageCode_De_DE,
  LanguageCode_En_AB,
  LanguageCode_En_AU,
  LanguageCode_En_GB,
  LanguageCode_En_IE,
  LanguageCode_En_IN,
  LanguageCode_En_US,
  LanguageCode_En_WL,
  LanguageCode_Es_ES,
  LanguageCode_Es_US,
  LanguageCode_Fa_IR,
  LanguageCode_Fr_CA,
  LanguageCode_Fr_FR,
  LanguageCode_Ga_IE,
  LanguageCode_Gd_GB,
  LanguageCode_He_IL,
  LanguageCode_Hi_IN,
  LanguageCode_Id_ID,
  LanguageCode_It_IT,
  LanguageCode_Ja_JP,
  LanguageCode_Ko_KR,
  LanguageCode_Ms_MY,
  LanguageCode_Nl_NL,
  LanguageCode_Pt_BR,
  LanguageCode_Pt_PT,
  LanguageCode_Ru_RU,
  LanguageCode_Ta_IN,
  LanguageCode_Te_IN,
  LanguageCode_Tr_TR,
  LanguageCode_Zh_CN,
  LanguageCode'
  #-}
