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
        SLCEN,
        SLCES,
        SLCFR,
        SLCDE,
        SLCIT,
        SLCPT
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SyntaxLanguageCode = SyntaxLanguageCode' Lude.Text
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

pattern SLCEN :: SyntaxLanguageCode
pattern SLCEN = SyntaxLanguageCode' "en"

pattern SLCES :: SyntaxLanguageCode
pattern SLCES = SyntaxLanguageCode' "es"

pattern SLCFR :: SyntaxLanguageCode
pattern SLCFR = SyntaxLanguageCode' "fr"

pattern SLCDE :: SyntaxLanguageCode
pattern SLCDE = SyntaxLanguageCode' "de"

pattern SLCIT :: SyntaxLanguageCode
pattern SLCIT = SyntaxLanguageCode' "it"

pattern SLCPT :: SyntaxLanguageCode
pattern SLCPT = SyntaxLanguageCode' "pt"

{-# COMPLETE
  SLCEN,
  SLCES,
  SLCFR,
  SLCDE,
  SLCIT,
  SLCPT,
  SyntaxLanguageCode'
  #-}
