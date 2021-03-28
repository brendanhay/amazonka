{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.Locale
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.Locale
  ( Locale
    ( Locale'
    , LocaleDeDe
    , LocaleEnAu
    , LocaleEnGb
    , LocaleEnUs
    , LocaleEs419
    , LocaleEsEs
    , LocaleEsUs
    , LocaleFrFr
    , LocaleFrCa
    , LocaleItIt
    , fromLocale
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype Locale = Locale'{fromLocale :: Core.Text}
                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                   Core.Generic)
                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON, Core.FromJSON,
                                     Core.ToXML, Core.FromXML, Core.ToText, Core.FromText,
                                     Core.ToByteString, Core.ToQuery, Core.ToHeader)

pattern LocaleDeDe :: Locale
pattern LocaleDeDe = Locale' "de-DE"

pattern LocaleEnAu :: Locale
pattern LocaleEnAu = Locale' "en-AU"

pattern LocaleEnGb :: Locale
pattern LocaleEnGb = Locale' "en-GB"

pattern LocaleEnUs :: Locale
pattern LocaleEnUs = Locale' "en-US"

pattern LocaleEs419 :: Locale
pattern LocaleEs419 = Locale' "es-419"

pattern LocaleEsEs :: Locale
pattern LocaleEsEs = Locale' "es-ES"

pattern LocaleEsUs :: Locale
pattern LocaleEsUs = Locale' "es-US"

pattern LocaleFrFr :: Locale
pattern LocaleFrFr = Locale' "fr-FR"

pattern LocaleFrCa :: Locale
pattern LocaleFrCa = Locale' "fr-CA"

pattern LocaleItIt :: Locale
pattern LocaleItIt = Locale' "it-IT"

{-# COMPLETE 
  LocaleDeDe,

  LocaleEnAu,

  LocaleEnGb,

  LocaleEnUs,

  LocaleEs419,

  LocaleEsEs,

  LocaleEsUs,

  LocaleFrFr,

  LocaleFrCa,

  LocaleItIt,
  Locale'
  #-}
