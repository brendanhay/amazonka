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
-- Module      : Amazonka.CloudSearch.Types.AnalysisSchemeLanguage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.AnalysisSchemeLanguage
  ( AnalysisSchemeLanguage
      ( ..,
        AnalysisSchemeLanguage_Ar,
        AnalysisSchemeLanguage_Bg,
        AnalysisSchemeLanguage_Ca,
        AnalysisSchemeLanguage_Cs,
        AnalysisSchemeLanguage_Da,
        AnalysisSchemeLanguage_De,
        AnalysisSchemeLanguage_El,
        AnalysisSchemeLanguage_En,
        AnalysisSchemeLanguage_Es,
        AnalysisSchemeLanguage_Eu,
        AnalysisSchemeLanguage_Fa,
        AnalysisSchemeLanguage_Fi,
        AnalysisSchemeLanguage_Fr,
        AnalysisSchemeLanguage_Ga,
        AnalysisSchemeLanguage_Gl,
        AnalysisSchemeLanguage_He,
        AnalysisSchemeLanguage_Hi,
        AnalysisSchemeLanguage_Hu,
        AnalysisSchemeLanguage_Hy,
        AnalysisSchemeLanguage_Id,
        AnalysisSchemeLanguage_It,
        AnalysisSchemeLanguage_Ja,
        AnalysisSchemeLanguage_Ko,
        AnalysisSchemeLanguage_Lv,
        AnalysisSchemeLanguage_Mul,
        AnalysisSchemeLanguage_Nl,
        AnalysisSchemeLanguage_No,
        AnalysisSchemeLanguage_Pt,
        AnalysisSchemeLanguage_Ro,
        AnalysisSchemeLanguage_Ru,
        AnalysisSchemeLanguage_Sv,
        AnalysisSchemeLanguage_Th,
        AnalysisSchemeLanguage_Tr,
        AnalysisSchemeLanguage_Zh_Hans,
        AnalysisSchemeLanguage_Zh_Hant
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An <http://tools.ietf.org/html/rfc4646 IETF RFC 4646> language code or
-- @mul@ for multiple languages.
newtype AnalysisSchemeLanguage = AnalysisSchemeLanguage'
  { fromAnalysisSchemeLanguage ::
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

pattern AnalysisSchemeLanguage_Ar :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Ar = AnalysisSchemeLanguage' "ar"

pattern AnalysisSchemeLanguage_Bg :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Bg = AnalysisSchemeLanguage' "bg"

pattern AnalysisSchemeLanguage_Ca :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Ca = AnalysisSchemeLanguage' "ca"

pattern AnalysisSchemeLanguage_Cs :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Cs = AnalysisSchemeLanguage' "cs"

pattern AnalysisSchemeLanguage_Da :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Da = AnalysisSchemeLanguage' "da"

pattern AnalysisSchemeLanguage_De :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_De = AnalysisSchemeLanguage' "de"

pattern AnalysisSchemeLanguage_El :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_El = AnalysisSchemeLanguage' "el"

pattern AnalysisSchemeLanguage_En :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_En = AnalysisSchemeLanguage' "en"

pattern AnalysisSchemeLanguage_Es :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Es = AnalysisSchemeLanguage' "es"

pattern AnalysisSchemeLanguage_Eu :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Eu = AnalysisSchemeLanguage' "eu"

pattern AnalysisSchemeLanguage_Fa :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Fa = AnalysisSchemeLanguage' "fa"

pattern AnalysisSchemeLanguage_Fi :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Fi = AnalysisSchemeLanguage' "fi"

pattern AnalysisSchemeLanguage_Fr :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Fr = AnalysisSchemeLanguage' "fr"

pattern AnalysisSchemeLanguage_Ga :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Ga = AnalysisSchemeLanguage' "ga"

pattern AnalysisSchemeLanguage_Gl :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Gl = AnalysisSchemeLanguage' "gl"

pattern AnalysisSchemeLanguage_He :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_He = AnalysisSchemeLanguage' "he"

pattern AnalysisSchemeLanguage_Hi :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Hi = AnalysisSchemeLanguage' "hi"

pattern AnalysisSchemeLanguage_Hu :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Hu = AnalysisSchemeLanguage' "hu"

pattern AnalysisSchemeLanguage_Hy :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Hy = AnalysisSchemeLanguage' "hy"

pattern AnalysisSchemeLanguage_Id :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Id = AnalysisSchemeLanguage' "id"

pattern AnalysisSchemeLanguage_It :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_It = AnalysisSchemeLanguage' "it"

pattern AnalysisSchemeLanguage_Ja :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Ja = AnalysisSchemeLanguage' "ja"

pattern AnalysisSchemeLanguage_Ko :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Ko = AnalysisSchemeLanguage' "ko"

pattern AnalysisSchemeLanguage_Lv :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Lv = AnalysisSchemeLanguage' "lv"

pattern AnalysisSchemeLanguage_Mul :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Mul = AnalysisSchemeLanguage' "mul"

pattern AnalysisSchemeLanguage_Nl :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Nl = AnalysisSchemeLanguage' "nl"

pattern AnalysisSchemeLanguage_No :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_No = AnalysisSchemeLanguage' "no"

pattern AnalysisSchemeLanguage_Pt :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Pt = AnalysisSchemeLanguage' "pt"

pattern AnalysisSchemeLanguage_Ro :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Ro = AnalysisSchemeLanguage' "ro"

pattern AnalysisSchemeLanguage_Ru :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Ru = AnalysisSchemeLanguage' "ru"

pattern AnalysisSchemeLanguage_Sv :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Sv = AnalysisSchemeLanguage' "sv"

pattern AnalysisSchemeLanguage_Th :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Th = AnalysisSchemeLanguage' "th"

pattern AnalysisSchemeLanguage_Tr :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Tr = AnalysisSchemeLanguage' "tr"

pattern AnalysisSchemeLanguage_Zh_Hans :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Zh_Hans = AnalysisSchemeLanguage' "zh-Hans"

pattern AnalysisSchemeLanguage_Zh_Hant :: AnalysisSchemeLanguage
pattern AnalysisSchemeLanguage_Zh_Hant = AnalysisSchemeLanguage' "zh-Hant"

{-# COMPLETE
  AnalysisSchemeLanguage_Ar,
  AnalysisSchemeLanguage_Bg,
  AnalysisSchemeLanguage_Ca,
  AnalysisSchemeLanguage_Cs,
  AnalysisSchemeLanguage_Da,
  AnalysisSchemeLanguage_De,
  AnalysisSchemeLanguage_El,
  AnalysisSchemeLanguage_En,
  AnalysisSchemeLanguage_Es,
  AnalysisSchemeLanguage_Eu,
  AnalysisSchemeLanguage_Fa,
  AnalysisSchemeLanguage_Fi,
  AnalysisSchemeLanguage_Fr,
  AnalysisSchemeLanguage_Ga,
  AnalysisSchemeLanguage_Gl,
  AnalysisSchemeLanguage_He,
  AnalysisSchemeLanguage_Hi,
  AnalysisSchemeLanguage_Hu,
  AnalysisSchemeLanguage_Hy,
  AnalysisSchemeLanguage_Id,
  AnalysisSchemeLanguage_It,
  AnalysisSchemeLanguage_Ja,
  AnalysisSchemeLanguage_Ko,
  AnalysisSchemeLanguage_Lv,
  AnalysisSchemeLanguage_Mul,
  AnalysisSchemeLanguage_Nl,
  AnalysisSchemeLanguage_No,
  AnalysisSchemeLanguage_Pt,
  AnalysisSchemeLanguage_Ro,
  AnalysisSchemeLanguage_Ru,
  AnalysisSchemeLanguage_Sv,
  AnalysisSchemeLanguage_Th,
  AnalysisSchemeLanguage_Tr,
  AnalysisSchemeLanguage_Zh_Hans,
  AnalysisSchemeLanguage_Zh_Hant,
  AnalysisSchemeLanguage'
  #-}
