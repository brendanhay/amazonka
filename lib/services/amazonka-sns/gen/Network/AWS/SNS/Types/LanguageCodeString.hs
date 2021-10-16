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
-- Module      : Network.AWS.SNS.Types.LanguageCodeString
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SNS.Types.LanguageCodeString
  ( LanguageCodeString
      ( ..,
        LanguageCodeString_De_DE,
        LanguageCodeString_En_GB,
        LanguageCodeString_En_US,
        LanguageCodeString_Es_419,
        LanguageCodeString_Es_ES,
        LanguageCodeString_Fr_CA,
        LanguageCodeString_Fr_FR,
        LanguageCodeString_It_IT,
        LanguageCodeString_Ja_JP,
        LanguageCodeString_Kr_KR,
        LanguageCodeString_Pt_BR,
        LanguageCodeString_Zh_CN,
        LanguageCodeString_Zh_TW
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Supported language code for sending OTP message
newtype LanguageCodeString = LanguageCodeString'
  { fromLanguageCodeString ::
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

pattern LanguageCodeString_De_DE :: LanguageCodeString
pattern LanguageCodeString_De_DE = LanguageCodeString' "de-DE"

pattern LanguageCodeString_En_GB :: LanguageCodeString
pattern LanguageCodeString_En_GB = LanguageCodeString' "en-GB"

pattern LanguageCodeString_En_US :: LanguageCodeString
pattern LanguageCodeString_En_US = LanguageCodeString' "en-US"

pattern LanguageCodeString_Es_419 :: LanguageCodeString
pattern LanguageCodeString_Es_419 = LanguageCodeString' "es-419"

pattern LanguageCodeString_Es_ES :: LanguageCodeString
pattern LanguageCodeString_Es_ES = LanguageCodeString' "es-ES"

pattern LanguageCodeString_Fr_CA :: LanguageCodeString
pattern LanguageCodeString_Fr_CA = LanguageCodeString' "fr-CA"

pattern LanguageCodeString_Fr_FR :: LanguageCodeString
pattern LanguageCodeString_Fr_FR = LanguageCodeString' "fr-FR"

pattern LanguageCodeString_It_IT :: LanguageCodeString
pattern LanguageCodeString_It_IT = LanguageCodeString' "it-IT"

pattern LanguageCodeString_Ja_JP :: LanguageCodeString
pattern LanguageCodeString_Ja_JP = LanguageCodeString' "ja-JP"

pattern LanguageCodeString_Kr_KR :: LanguageCodeString
pattern LanguageCodeString_Kr_KR = LanguageCodeString' "kr-KR"

pattern LanguageCodeString_Pt_BR :: LanguageCodeString
pattern LanguageCodeString_Pt_BR = LanguageCodeString' "pt-BR"

pattern LanguageCodeString_Zh_CN :: LanguageCodeString
pattern LanguageCodeString_Zh_CN = LanguageCodeString' "zh-CN"

pattern LanguageCodeString_Zh_TW :: LanguageCodeString
pattern LanguageCodeString_Zh_TW = LanguageCodeString' "zh-TW"

{-# COMPLETE
  LanguageCodeString_De_DE,
  LanguageCodeString_En_GB,
  LanguageCodeString_En_US,
  LanguageCodeString_Es_419,
  LanguageCodeString_Es_ES,
  LanguageCodeString_Fr_CA,
  LanguageCodeString_Fr_FR,
  LanguageCodeString_It_IT,
  LanguageCodeString_Ja_JP,
  LanguageCodeString_Kr_KR,
  LanguageCodeString_Pt_BR,
  LanguageCodeString_Zh_CN,
  LanguageCodeString_Zh_TW,
  LanguageCodeString'
  #-}
