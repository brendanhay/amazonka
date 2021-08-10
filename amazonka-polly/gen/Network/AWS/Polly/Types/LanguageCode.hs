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
-- Module      : Network.AWS.Polly.Types.LanguageCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types.LanguageCode
  ( LanguageCode
      ( ..,
        LanguageCode_Arb,
        LanguageCode_Cmn_CN,
        LanguageCode_Cy_GB,
        LanguageCode_Da_DK,
        LanguageCode_De_DE,
        LanguageCode_En_AU,
        LanguageCode_En_GB,
        LanguageCode_En_GB_WLS,
        LanguageCode_En_IN,
        LanguageCode_En_US,
        LanguageCode_Es_ES,
        LanguageCode_Es_MX,
        LanguageCode_Es_US,
        LanguageCode_Fr_CA,
        LanguageCode_Fr_FR,
        LanguageCode_Hi_IN,
        LanguageCode_Is_IS,
        LanguageCode_It_IT,
        LanguageCode_Ja_JP,
        LanguageCode_Ko_KR,
        LanguageCode_Nb_NO,
        LanguageCode_Nl_NL,
        LanguageCode_Pl_PL,
        LanguageCode_Pt_BR,
        LanguageCode_Pt_PT,
        LanguageCode_Ro_RO,
        LanguageCode_Ru_RU,
        LanguageCode_Sv_SE,
        LanguageCode_Tr_TR
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

pattern LanguageCode_Arb :: LanguageCode
pattern LanguageCode_Arb = LanguageCode' "arb"

pattern LanguageCode_Cmn_CN :: LanguageCode
pattern LanguageCode_Cmn_CN = LanguageCode' "cmn-CN"

pattern LanguageCode_Cy_GB :: LanguageCode
pattern LanguageCode_Cy_GB = LanguageCode' "cy-GB"

pattern LanguageCode_Da_DK :: LanguageCode
pattern LanguageCode_Da_DK = LanguageCode' "da-DK"

pattern LanguageCode_De_DE :: LanguageCode
pattern LanguageCode_De_DE = LanguageCode' "de-DE"

pattern LanguageCode_En_AU :: LanguageCode
pattern LanguageCode_En_AU = LanguageCode' "en-AU"

pattern LanguageCode_En_GB :: LanguageCode
pattern LanguageCode_En_GB = LanguageCode' "en-GB"

pattern LanguageCode_En_GB_WLS :: LanguageCode
pattern LanguageCode_En_GB_WLS = LanguageCode' "en-GB-WLS"

pattern LanguageCode_En_IN :: LanguageCode
pattern LanguageCode_En_IN = LanguageCode' "en-IN"

pattern LanguageCode_En_US :: LanguageCode
pattern LanguageCode_En_US = LanguageCode' "en-US"

pattern LanguageCode_Es_ES :: LanguageCode
pattern LanguageCode_Es_ES = LanguageCode' "es-ES"

pattern LanguageCode_Es_MX :: LanguageCode
pattern LanguageCode_Es_MX = LanguageCode' "es-MX"

pattern LanguageCode_Es_US :: LanguageCode
pattern LanguageCode_Es_US = LanguageCode' "es-US"

pattern LanguageCode_Fr_CA :: LanguageCode
pattern LanguageCode_Fr_CA = LanguageCode' "fr-CA"

pattern LanguageCode_Fr_FR :: LanguageCode
pattern LanguageCode_Fr_FR = LanguageCode' "fr-FR"

pattern LanguageCode_Hi_IN :: LanguageCode
pattern LanguageCode_Hi_IN = LanguageCode' "hi-IN"

pattern LanguageCode_Is_IS :: LanguageCode
pattern LanguageCode_Is_IS = LanguageCode' "is-IS"

pattern LanguageCode_It_IT :: LanguageCode
pattern LanguageCode_It_IT = LanguageCode' "it-IT"

pattern LanguageCode_Ja_JP :: LanguageCode
pattern LanguageCode_Ja_JP = LanguageCode' "ja-JP"

pattern LanguageCode_Ko_KR :: LanguageCode
pattern LanguageCode_Ko_KR = LanguageCode' "ko-KR"

pattern LanguageCode_Nb_NO :: LanguageCode
pattern LanguageCode_Nb_NO = LanguageCode' "nb-NO"

pattern LanguageCode_Nl_NL :: LanguageCode
pattern LanguageCode_Nl_NL = LanguageCode' "nl-NL"

pattern LanguageCode_Pl_PL :: LanguageCode
pattern LanguageCode_Pl_PL = LanguageCode' "pl-PL"

pattern LanguageCode_Pt_BR :: LanguageCode
pattern LanguageCode_Pt_BR = LanguageCode' "pt-BR"

pattern LanguageCode_Pt_PT :: LanguageCode
pattern LanguageCode_Pt_PT = LanguageCode' "pt-PT"

pattern LanguageCode_Ro_RO :: LanguageCode
pattern LanguageCode_Ro_RO = LanguageCode' "ro-RO"

pattern LanguageCode_Ru_RU :: LanguageCode
pattern LanguageCode_Ru_RU = LanguageCode' "ru-RU"

pattern LanguageCode_Sv_SE :: LanguageCode
pattern LanguageCode_Sv_SE = LanguageCode' "sv-SE"

pattern LanguageCode_Tr_TR :: LanguageCode
pattern LanguageCode_Tr_TR = LanguageCode' "tr-TR"

{-# COMPLETE
  LanguageCode_Arb,
  LanguageCode_Cmn_CN,
  LanguageCode_Cy_GB,
  LanguageCode_Da_DK,
  LanguageCode_De_DE,
  LanguageCode_En_AU,
  LanguageCode_En_GB,
  LanguageCode_En_GB_WLS,
  LanguageCode_En_IN,
  LanguageCode_En_US,
  LanguageCode_Es_ES,
  LanguageCode_Es_MX,
  LanguageCode_Es_US,
  LanguageCode_Fr_CA,
  LanguageCode_Fr_FR,
  LanguageCode_Hi_IN,
  LanguageCode_Is_IS,
  LanguageCode_It_IT,
  LanguageCode_Ja_JP,
  LanguageCode_Ko_KR,
  LanguageCode_Nb_NO,
  LanguageCode_Nl_NL,
  LanguageCode_Pl_PL,
  LanguageCode_Pt_BR,
  LanguageCode_Pt_PT,
  LanguageCode_Ro_RO,
  LanguageCode_Ru_RU,
  LanguageCode_Sv_SE,
  LanguageCode_Tr_TR,
  LanguageCode'
  #-}
