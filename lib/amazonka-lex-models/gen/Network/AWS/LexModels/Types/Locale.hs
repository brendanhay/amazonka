{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.Locale
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.Locale
  ( Locale
      ( Locale',
        DeDe,
        EnAu,
        EnGb,
        EnUs,
        Es419,
        EsEs,
        EsUs,
        FrCa,
        FrFr,
        ItIt
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype Locale = Locale' Lude.Text
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

pattern DeDe :: Locale
pattern DeDe = Locale' "de-DE"

pattern EnAu :: Locale
pattern EnAu = Locale' "en-AU"

pattern EnGb :: Locale
pattern EnGb = Locale' "en-GB"

pattern EnUs :: Locale
pattern EnUs = Locale' "en-US"

pattern Es419 :: Locale
pattern Es419 = Locale' "es-419"

pattern EsEs :: Locale
pattern EsEs = Locale' "es-ES"

pattern EsUs :: Locale
pattern EsUs = Locale' "es-US"

pattern FrCa :: Locale
pattern FrCa = Locale' "fr-CA"

pattern FrFr :: Locale
pattern FrFr = Locale' "fr-FR"

pattern ItIt :: Locale
pattern ItIt = Locale' "it-IT"

{-# COMPLETE
  DeDe,
  EnAu,
  EnGb,
  EnUs,
  Es419,
  EsEs,
  EsUs,
  FrCa,
  FrFr,
  ItIt,
  Locale'
  #-}
