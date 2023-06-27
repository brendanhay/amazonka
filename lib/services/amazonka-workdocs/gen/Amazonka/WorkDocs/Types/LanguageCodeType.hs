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
-- Module      : Amazonka.WorkDocs.Types.LanguageCodeType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.LanguageCodeType
  ( LanguageCodeType
      ( ..,
        LanguageCodeType_AR,
        LanguageCodeType_BG,
        LanguageCodeType_BN,
        LanguageCodeType_CS,
        LanguageCodeType_DA,
        LanguageCodeType_DE,
        LanguageCodeType_DEFAULT,
        LanguageCodeType_EL,
        LanguageCodeType_EN,
        LanguageCodeType_ES,
        LanguageCodeType_FA,
        LanguageCodeType_FI,
        LanguageCodeType_FR,
        LanguageCodeType_HI,
        LanguageCodeType_HU,
        LanguageCodeType_ID,
        LanguageCodeType_IT,
        LanguageCodeType_JA,
        LanguageCodeType_KO,
        LanguageCodeType_LT,
        LanguageCodeType_LV,
        LanguageCodeType_NL,
        LanguageCodeType_NO,
        LanguageCodeType_PT,
        LanguageCodeType_RO,
        LanguageCodeType_RU,
        LanguageCodeType_SV,
        LanguageCodeType_SW,
        LanguageCodeType_TH,
        LanguageCodeType_TR,
        LanguageCodeType_ZH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LanguageCodeType = LanguageCodeType'
  { fromLanguageCodeType ::
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

pattern LanguageCodeType_AR :: LanguageCodeType
pattern LanguageCodeType_AR = LanguageCodeType' "AR"

pattern LanguageCodeType_BG :: LanguageCodeType
pattern LanguageCodeType_BG = LanguageCodeType' "BG"

pattern LanguageCodeType_BN :: LanguageCodeType
pattern LanguageCodeType_BN = LanguageCodeType' "BN"

pattern LanguageCodeType_CS :: LanguageCodeType
pattern LanguageCodeType_CS = LanguageCodeType' "CS"

pattern LanguageCodeType_DA :: LanguageCodeType
pattern LanguageCodeType_DA = LanguageCodeType' "DA"

pattern LanguageCodeType_DE :: LanguageCodeType
pattern LanguageCodeType_DE = LanguageCodeType' "DE"

pattern LanguageCodeType_DEFAULT :: LanguageCodeType
pattern LanguageCodeType_DEFAULT = LanguageCodeType' "DEFAULT"

pattern LanguageCodeType_EL :: LanguageCodeType
pattern LanguageCodeType_EL = LanguageCodeType' "EL"

pattern LanguageCodeType_EN :: LanguageCodeType
pattern LanguageCodeType_EN = LanguageCodeType' "EN"

pattern LanguageCodeType_ES :: LanguageCodeType
pattern LanguageCodeType_ES = LanguageCodeType' "ES"

pattern LanguageCodeType_FA :: LanguageCodeType
pattern LanguageCodeType_FA = LanguageCodeType' "FA"

pattern LanguageCodeType_FI :: LanguageCodeType
pattern LanguageCodeType_FI = LanguageCodeType' "FI"

pattern LanguageCodeType_FR :: LanguageCodeType
pattern LanguageCodeType_FR = LanguageCodeType' "FR"

pattern LanguageCodeType_HI :: LanguageCodeType
pattern LanguageCodeType_HI = LanguageCodeType' "HI"

pattern LanguageCodeType_HU :: LanguageCodeType
pattern LanguageCodeType_HU = LanguageCodeType' "HU"

pattern LanguageCodeType_ID :: LanguageCodeType
pattern LanguageCodeType_ID = LanguageCodeType' "ID"

pattern LanguageCodeType_IT :: LanguageCodeType
pattern LanguageCodeType_IT = LanguageCodeType' "IT"

pattern LanguageCodeType_JA :: LanguageCodeType
pattern LanguageCodeType_JA = LanguageCodeType' "JA"

pattern LanguageCodeType_KO :: LanguageCodeType
pattern LanguageCodeType_KO = LanguageCodeType' "KO"

pattern LanguageCodeType_LT :: LanguageCodeType
pattern LanguageCodeType_LT = LanguageCodeType' "LT"

pattern LanguageCodeType_LV :: LanguageCodeType
pattern LanguageCodeType_LV = LanguageCodeType' "LV"

pattern LanguageCodeType_NL :: LanguageCodeType
pattern LanguageCodeType_NL = LanguageCodeType' "NL"

pattern LanguageCodeType_NO :: LanguageCodeType
pattern LanguageCodeType_NO = LanguageCodeType' "NO"

pattern LanguageCodeType_PT :: LanguageCodeType
pattern LanguageCodeType_PT = LanguageCodeType' "PT"

pattern LanguageCodeType_RO :: LanguageCodeType
pattern LanguageCodeType_RO = LanguageCodeType' "RO"

pattern LanguageCodeType_RU :: LanguageCodeType
pattern LanguageCodeType_RU = LanguageCodeType' "RU"

pattern LanguageCodeType_SV :: LanguageCodeType
pattern LanguageCodeType_SV = LanguageCodeType' "SV"

pattern LanguageCodeType_SW :: LanguageCodeType
pattern LanguageCodeType_SW = LanguageCodeType' "SW"

pattern LanguageCodeType_TH :: LanguageCodeType
pattern LanguageCodeType_TH = LanguageCodeType' "TH"

pattern LanguageCodeType_TR :: LanguageCodeType
pattern LanguageCodeType_TR = LanguageCodeType' "TR"

pattern LanguageCodeType_ZH :: LanguageCodeType
pattern LanguageCodeType_ZH = LanguageCodeType' "ZH"

{-# COMPLETE
  LanguageCodeType_AR,
  LanguageCodeType_BG,
  LanguageCodeType_BN,
  LanguageCodeType_CS,
  LanguageCodeType_DA,
  LanguageCodeType_DE,
  LanguageCodeType_DEFAULT,
  LanguageCodeType_EL,
  LanguageCodeType_EN,
  LanguageCodeType_ES,
  LanguageCodeType_FA,
  LanguageCodeType_FI,
  LanguageCodeType_FR,
  LanguageCodeType_HI,
  LanguageCodeType_HU,
  LanguageCodeType_ID,
  LanguageCodeType_IT,
  LanguageCodeType_JA,
  LanguageCodeType_KO,
  LanguageCodeType_LT,
  LanguageCodeType_LV,
  LanguageCodeType_NL,
  LanguageCodeType_NO,
  LanguageCodeType_PT,
  LanguageCodeType_RO,
  LanguageCodeType_RU,
  LanguageCodeType_SV,
  LanguageCodeType_SW,
  LanguageCodeType_TH,
  LanguageCodeType_TR,
  LanguageCodeType_ZH,
  LanguageCodeType'
  #-}
