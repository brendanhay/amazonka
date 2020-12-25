{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        LanguageCodeEN,
        LanguageCodeES,
        LanguageCodeFR,
        LanguageCodeDE,
        LanguageCodeIT,
        LanguageCodePT,
        LanguageCodeAR,
        LanguageCodeHI,
        LanguageCodeJA,
        LanguageCodeKO,
        LanguageCodeZH,
        LanguageCodeZhTw,
        fromLanguageCode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype LanguageCode = LanguageCode' {fromLanguageCode :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern LanguageCodeEN :: LanguageCode
pattern LanguageCodeEN = LanguageCode' "en"

pattern LanguageCodeES :: LanguageCode
pattern LanguageCodeES = LanguageCode' "es"

pattern LanguageCodeFR :: LanguageCode
pattern LanguageCodeFR = LanguageCode' "fr"

pattern LanguageCodeDE :: LanguageCode
pattern LanguageCodeDE = LanguageCode' "de"

pattern LanguageCodeIT :: LanguageCode
pattern LanguageCodeIT = LanguageCode' "it"

pattern LanguageCodePT :: LanguageCode
pattern LanguageCodePT = LanguageCode' "pt"

pattern LanguageCodeAR :: LanguageCode
pattern LanguageCodeAR = LanguageCode' "ar"

pattern LanguageCodeHI :: LanguageCode
pattern LanguageCodeHI = LanguageCode' "hi"

pattern LanguageCodeJA :: LanguageCode
pattern LanguageCodeJA = LanguageCode' "ja"

pattern LanguageCodeKO :: LanguageCode
pattern LanguageCodeKO = LanguageCode' "ko"

pattern LanguageCodeZH :: LanguageCode
pattern LanguageCodeZH = LanguageCode' "zh"

pattern LanguageCodeZhTw :: LanguageCode
pattern LanguageCodeZhTw = LanguageCode' "zh-TW"

{-# COMPLETE
  LanguageCodeEN,
  LanguageCodeES,
  LanguageCodeFR,
  LanguageCodeDE,
  LanguageCodeIT,
  LanguageCodePT,
  LanguageCodeAR,
  LanguageCodeHI,
  LanguageCodeJA,
  LanguageCodeKO,
  LanguageCodeZH,
  LanguageCodeZhTw,
  LanguageCode'
  #-}
