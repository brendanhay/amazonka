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
-- Module      : Amazonka.Connect.Types.VocabularyLanguageCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.VocabularyLanguageCode
  ( VocabularyLanguageCode
      ( ..,
        VocabularyLanguageCode_Ar_AE,
        VocabularyLanguageCode_De_CH,
        VocabularyLanguageCode_De_DE,
        VocabularyLanguageCode_En_AB,
        VocabularyLanguageCode_En_AU,
        VocabularyLanguageCode_En_GB,
        VocabularyLanguageCode_En_IE,
        VocabularyLanguageCode_En_IN,
        VocabularyLanguageCode_En_NZ,
        VocabularyLanguageCode_En_US,
        VocabularyLanguageCode_En_WL,
        VocabularyLanguageCode_En_ZA,
        VocabularyLanguageCode_Es_ES,
        VocabularyLanguageCode_Es_US,
        VocabularyLanguageCode_Fr_CA,
        VocabularyLanguageCode_Fr_FR,
        VocabularyLanguageCode_Hi_IN,
        VocabularyLanguageCode_It_IT,
        VocabularyLanguageCode_Ja_JP,
        VocabularyLanguageCode_Ko_KR,
        VocabularyLanguageCode_Pt_BR,
        VocabularyLanguageCode_Pt_PT,
        VocabularyLanguageCode_Zh_CN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VocabularyLanguageCode = VocabularyLanguageCode'
  { fromVocabularyLanguageCode ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern VocabularyLanguageCode_Ar_AE :: VocabularyLanguageCode
pattern VocabularyLanguageCode_Ar_AE = VocabularyLanguageCode' "ar-AE"

pattern VocabularyLanguageCode_De_CH :: VocabularyLanguageCode
pattern VocabularyLanguageCode_De_CH = VocabularyLanguageCode' "de-CH"

pattern VocabularyLanguageCode_De_DE :: VocabularyLanguageCode
pattern VocabularyLanguageCode_De_DE = VocabularyLanguageCode' "de-DE"

pattern VocabularyLanguageCode_En_AB :: VocabularyLanguageCode
pattern VocabularyLanguageCode_En_AB = VocabularyLanguageCode' "en-AB"

pattern VocabularyLanguageCode_En_AU :: VocabularyLanguageCode
pattern VocabularyLanguageCode_En_AU = VocabularyLanguageCode' "en-AU"

pattern VocabularyLanguageCode_En_GB :: VocabularyLanguageCode
pattern VocabularyLanguageCode_En_GB = VocabularyLanguageCode' "en-GB"

pattern VocabularyLanguageCode_En_IE :: VocabularyLanguageCode
pattern VocabularyLanguageCode_En_IE = VocabularyLanguageCode' "en-IE"

pattern VocabularyLanguageCode_En_IN :: VocabularyLanguageCode
pattern VocabularyLanguageCode_En_IN = VocabularyLanguageCode' "en-IN"

pattern VocabularyLanguageCode_En_NZ :: VocabularyLanguageCode
pattern VocabularyLanguageCode_En_NZ = VocabularyLanguageCode' "en-NZ"

pattern VocabularyLanguageCode_En_US :: VocabularyLanguageCode
pattern VocabularyLanguageCode_En_US = VocabularyLanguageCode' "en-US"

pattern VocabularyLanguageCode_En_WL :: VocabularyLanguageCode
pattern VocabularyLanguageCode_En_WL = VocabularyLanguageCode' "en-WL"

pattern VocabularyLanguageCode_En_ZA :: VocabularyLanguageCode
pattern VocabularyLanguageCode_En_ZA = VocabularyLanguageCode' "en-ZA"

pattern VocabularyLanguageCode_Es_ES :: VocabularyLanguageCode
pattern VocabularyLanguageCode_Es_ES = VocabularyLanguageCode' "es-ES"

pattern VocabularyLanguageCode_Es_US :: VocabularyLanguageCode
pattern VocabularyLanguageCode_Es_US = VocabularyLanguageCode' "es-US"

pattern VocabularyLanguageCode_Fr_CA :: VocabularyLanguageCode
pattern VocabularyLanguageCode_Fr_CA = VocabularyLanguageCode' "fr-CA"

pattern VocabularyLanguageCode_Fr_FR :: VocabularyLanguageCode
pattern VocabularyLanguageCode_Fr_FR = VocabularyLanguageCode' "fr-FR"

pattern VocabularyLanguageCode_Hi_IN :: VocabularyLanguageCode
pattern VocabularyLanguageCode_Hi_IN = VocabularyLanguageCode' "hi-IN"

pattern VocabularyLanguageCode_It_IT :: VocabularyLanguageCode
pattern VocabularyLanguageCode_It_IT = VocabularyLanguageCode' "it-IT"

pattern VocabularyLanguageCode_Ja_JP :: VocabularyLanguageCode
pattern VocabularyLanguageCode_Ja_JP = VocabularyLanguageCode' "ja-JP"

pattern VocabularyLanguageCode_Ko_KR :: VocabularyLanguageCode
pattern VocabularyLanguageCode_Ko_KR = VocabularyLanguageCode' "ko-KR"

pattern VocabularyLanguageCode_Pt_BR :: VocabularyLanguageCode
pattern VocabularyLanguageCode_Pt_BR = VocabularyLanguageCode' "pt-BR"

pattern VocabularyLanguageCode_Pt_PT :: VocabularyLanguageCode
pattern VocabularyLanguageCode_Pt_PT = VocabularyLanguageCode' "pt-PT"

pattern VocabularyLanguageCode_Zh_CN :: VocabularyLanguageCode
pattern VocabularyLanguageCode_Zh_CN = VocabularyLanguageCode' "zh-CN"

{-# COMPLETE
  VocabularyLanguageCode_Ar_AE,
  VocabularyLanguageCode_De_CH,
  VocabularyLanguageCode_De_DE,
  VocabularyLanguageCode_En_AB,
  VocabularyLanguageCode_En_AU,
  VocabularyLanguageCode_En_GB,
  VocabularyLanguageCode_En_IE,
  VocabularyLanguageCode_En_IN,
  VocabularyLanguageCode_En_NZ,
  VocabularyLanguageCode_En_US,
  VocabularyLanguageCode_En_WL,
  VocabularyLanguageCode_En_ZA,
  VocabularyLanguageCode_Es_ES,
  VocabularyLanguageCode_Es_US,
  VocabularyLanguageCode_Fr_CA,
  VocabularyLanguageCode_Fr_FR,
  VocabularyLanguageCode_Hi_IN,
  VocabularyLanguageCode_It_IT,
  VocabularyLanguageCode_Ja_JP,
  VocabularyLanguageCode_Ko_KR,
  VocabularyLanguageCode_Pt_BR,
  VocabularyLanguageCode_Pt_PT,
  VocabularyLanguageCode_Zh_CN,
  VocabularyLanguageCode'
  #-}
