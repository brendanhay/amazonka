-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.LanguageCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.LanguageCode
  ( LanguageCode
      ( LanguageCode',
        AR,
        DE,
        EN,
        ES,
        FR,
        HI,
        IT,
        JA,
        KO,
        PT,
        ZH,
        ZhTw
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

pattern AR :: LanguageCode
pattern AR = LanguageCode' "ar"

pattern DE :: LanguageCode
pattern DE = LanguageCode' "de"

pattern EN :: LanguageCode
pattern EN = LanguageCode' "en"

pattern ES :: LanguageCode
pattern ES = LanguageCode' "es"

pattern FR :: LanguageCode
pattern FR = LanguageCode' "fr"

pattern HI :: LanguageCode
pattern HI = LanguageCode' "hi"

pattern IT :: LanguageCode
pattern IT = LanguageCode' "it"

pattern JA :: LanguageCode
pattern JA = LanguageCode' "ja"

pattern KO :: LanguageCode
pattern KO = LanguageCode' "ko"

pattern PT :: LanguageCode
pattern PT = LanguageCode' "pt"

pattern ZH :: LanguageCode
pattern ZH = LanguageCode' "zh"

pattern ZhTw :: LanguageCode
pattern ZhTw = LanguageCode' "zh-TW"

{-# COMPLETE
  AR,
  DE,
  EN,
  ES,
  FR,
  HI,
  IT,
  JA,
  KO,
  PT,
  ZH,
  ZhTw,
  LanguageCode'
  #-}
