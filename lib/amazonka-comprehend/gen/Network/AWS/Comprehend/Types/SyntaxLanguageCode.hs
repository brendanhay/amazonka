{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.SyntaxLanguageCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.SyntaxLanguageCode
  ( SyntaxLanguageCode
      ( SyntaxLanguageCode',
        SyntaxLanguageCodeEN,
        SyntaxLanguageCodeES,
        SyntaxLanguageCodeFR,
        SyntaxLanguageCodeDE,
        SyntaxLanguageCodeIT,
        SyntaxLanguageCodePT,
        fromSyntaxLanguageCode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype SyntaxLanguageCode = SyntaxLanguageCode'
  { fromSyntaxLanguageCode ::
      Core.Text
  }
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

pattern SyntaxLanguageCodeEN :: SyntaxLanguageCode
pattern SyntaxLanguageCodeEN = SyntaxLanguageCode' "en"

pattern SyntaxLanguageCodeES :: SyntaxLanguageCode
pattern SyntaxLanguageCodeES = SyntaxLanguageCode' "es"

pattern SyntaxLanguageCodeFR :: SyntaxLanguageCode
pattern SyntaxLanguageCodeFR = SyntaxLanguageCode' "fr"

pattern SyntaxLanguageCodeDE :: SyntaxLanguageCode
pattern SyntaxLanguageCodeDE = SyntaxLanguageCode' "de"

pattern SyntaxLanguageCodeIT :: SyntaxLanguageCode
pattern SyntaxLanguageCodeIT = SyntaxLanguageCode' "it"

pattern SyntaxLanguageCodePT :: SyntaxLanguageCode
pattern SyntaxLanguageCodePT = SyntaxLanguageCode' "pt"

{-# COMPLETE
  SyntaxLanguageCodeEN,
  SyntaxLanguageCodeES,
  SyntaxLanguageCodeFR,
  SyntaxLanguageCodeDE,
  SyntaxLanguageCodeIT,
  SyntaxLanguageCodePT,
  SyntaxLanguageCode'
  #-}
