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
-- Module      : Amazonka.DevOpsGuru.Types.Locale
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.Locale
  ( Locale
      ( ..,
        Locale_DE_DE,
        Locale_EN_GB,
        Locale_EN_US,
        Locale_ES_ES,
        Locale_FR_FR,
        Locale_IT_IT,
        Locale_JA_JP,
        Locale_KO_KR,
        Locale_PT_BR,
        Locale_ZH_CN,
        Locale_ZH_TW
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Locale = Locale' {fromLocale :: Data.Text}
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

pattern Locale_DE_DE :: Locale
pattern Locale_DE_DE = Locale' "DE_DE"

pattern Locale_EN_GB :: Locale
pattern Locale_EN_GB = Locale' "EN_GB"

pattern Locale_EN_US :: Locale
pattern Locale_EN_US = Locale' "EN_US"

pattern Locale_ES_ES :: Locale
pattern Locale_ES_ES = Locale' "ES_ES"

pattern Locale_FR_FR :: Locale
pattern Locale_FR_FR = Locale' "FR_FR"

pattern Locale_IT_IT :: Locale
pattern Locale_IT_IT = Locale' "IT_IT"

pattern Locale_JA_JP :: Locale
pattern Locale_JA_JP = Locale' "JA_JP"

pattern Locale_KO_KR :: Locale
pattern Locale_KO_KR = Locale' "KO_KR"

pattern Locale_PT_BR :: Locale
pattern Locale_PT_BR = Locale' "PT_BR"

pattern Locale_ZH_CN :: Locale
pattern Locale_ZH_CN = Locale' "ZH_CN"

pattern Locale_ZH_TW :: Locale
pattern Locale_ZH_TW = Locale' "ZH_TW"

{-# COMPLETE
  Locale_DE_DE,
  Locale_EN_GB,
  Locale_EN_US,
  Locale_ES_ES,
  Locale_FR_FR,
  Locale_IT_IT,
  Locale_JA_JP,
  Locale_KO_KR,
  Locale_PT_BR,
  Locale_ZH_CN,
  Locale_ZH_TW,
  Locale'
  #-}
