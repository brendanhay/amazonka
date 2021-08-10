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
-- Module      : Network.AWS.Comprehend.Types.LanguageCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.LanguageCode
  ( LanguageCode
      ( ..,
        LanguageCode_Ar,
        LanguageCode_De,
        LanguageCode_En,
        LanguageCode_Es,
        LanguageCode_Fr,
        LanguageCode_Hi,
        LanguageCode_It,
        LanguageCode_Ja,
        LanguageCode_Ko,
        LanguageCode_Pt,
        LanguageCode_Zh,
        LanguageCode_Zh_TW
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

pattern LanguageCode_Ar :: LanguageCode
pattern LanguageCode_Ar = LanguageCode' "ar"

pattern LanguageCode_De :: LanguageCode
pattern LanguageCode_De = LanguageCode' "de"

pattern LanguageCode_En :: LanguageCode
pattern LanguageCode_En = LanguageCode' "en"

pattern LanguageCode_Es :: LanguageCode
pattern LanguageCode_Es = LanguageCode' "es"

pattern LanguageCode_Fr :: LanguageCode
pattern LanguageCode_Fr = LanguageCode' "fr"

pattern LanguageCode_Hi :: LanguageCode
pattern LanguageCode_Hi = LanguageCode' "hi"

pattern LanguageCode_It :: LanguageCode
pattern LanguageCode_It = LanguageCode' "it"

pattern LanguageCode_Ja :: LanguageCode
pattern LanguageCode_Ja = LanguageCode' "ja"

pattern LanguageCode_Ko :: LanguageCode
pattern LanguageCode_Ko = LanguageCode' "ko"

pattern LanguageCode_Pt :: LanguageCode
pattern LanguageCode_Pt = LanguageCode' "pt"

pattern LanguageCode_Zh :: LanguageCode
pattern LanguageCode_Zh = LanguageCode' "zh"

pattern LanguageCode_Zh_TW :: LanguageCode
pattern LanguageCode_Zh_TW = LanguageCode' "zh-TW"

{-# COMPLETE
  LanguageCode_Ar,
  LanguageCode_De,
  LanguageCode_En,
  LanguageCode_Es,
  LanguageCode_Fr,
  LanguageCode_Hi,
  LanguageCode_It,
  LanguageCode_Ja,
  LanguageCode_Ko,
  LanguageCode_Pt,
  LanguageCode_Zh,
  LanguageCode_Zh_TW,
  LanguageCode'
  #-}
