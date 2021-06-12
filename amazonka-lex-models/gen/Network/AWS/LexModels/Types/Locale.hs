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
-- Module      : Network.AWS.LexModels.Types.Locale
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.Locale
  ( Locale
      ( ..,
        Locale_De_DE,
        Locale_En_AU,
        Locale_En_GB,
        Locale_En_US,
        Locale_Es_419,
        Locale_Es_ES,
        Locale_Es_US,
        Locale_Fr_CA,
        Locale_Fr_FR,
        Locale_It_IT
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype Locale = Locale' {fromLocale :: Core.Text}
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern Locale_De_DE :: Locale
pattern Locale_De_DE = Locale' "de-DE"

pattern Locale_En_AU :: Locale
pattern Locale_En_AU = Locale' "en-AU"

pattern Locale_En_GB :: Locale
pattern Locale_En_GB = Locale' "en-GB"

pattern Locale_En_US :: Locale
pattern Locale_En_US = Locale' "en-US"

pattern Locale_Es_419 :: Locale
pattern Locale_Es_419 = Locale' "es-419"

pattern Locale_Es_ES :: Locale
pattern Locale_Es_ES = Locale' "es-ES"

pattern Locale_Es_US :: Locale
pattern Locale_Es_US = Locale' "es-US"

pattern Locale_Fr_CA :: Locale
pattern Locale_Fr_CA = Locale' "fr-CA"

pattern Locale_Fr_FR :: Locale
pattern Locale_Fr_FR = Locale' "fr-FR"

pattern Locale_It_IT :: Locale
pattern Locale_It_IT = Locale' "it-IT"

{-# COMPLETE
  Locale_De_DE,
  Locale_En_AU,
  Locale_En_GB,
  Locale_En_US,
  Locale_Es_419,
  Locale_Es_ES,
  Locale_Es_US,
  Locale_Fr_CA,
  Locale_Fr_FR,
  Locale_It_IT,
  Locale'
  #-}
