{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.LocaleType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.LocaleType
  ( LocaleType
      ( LocaleType',
        EN,
        FR,
        KO,
        DE,
        ES,
        JA,
        RU,
        ZhCn,
        ZhTw,
        PtBr,
        Default
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LocaleType = LocaleType' Lude.Text
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

pattern EN :: LocaleType
pattern EN = LocaleType' "en"

pattern FR :: LocaleType
pattern FR = LocaleType' "fr"

pattern KO :: LocaleType
pattern KO = LocaleType' "ko"

pattern DE :: LocaleType
pattern DE = LocaleType' "de"

pattern ES :: LocaleType
pattern ES = LocaleType' "es"

pattern JA :: LocaleType
pattern JA = LocaleType' "ja"

pattern RU :: LocaleType
pattern RU = LocaleType' "ru"

pattern ZhCn :: LocaleType
pattern ZhCn = LocaleType' "zh_CN"

pattern ZhTw :: LocaleType
pattern ZhTw = LocaleType' "zh_TW"

pattern PtBr :: LocaleType
pattern PtBr = LocaleType' "pt_BR"

pattern Default :: LocaleType
pattern Default = LocaleType' "default"

{-# COMPLETE
  EN,
  FR,
  KO,
  DE,
  ES,
  JA,
  RU,
  ZhCn,
  ZhTw,
  PtBr,
  Default,
  LocaleType'
  #-}
