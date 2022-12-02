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
-- Module      : Amazonka.LexModels.Types.Locale
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.Locale
  ( Locale
      ( ..,
        Locale_De_DE,
        Locale_En_AU,
        Locale_En_GB,
        Locale_En_IN,
        Locale_En_US,
        Locale_Es_419,
        Locale_Es_ES,
        Locale_Es_US,
        Locale_Fr_CA,
        Locale_Fr_FR,
        Locale_It_IT,
        Locale_Ja_JP,
        Locale_Ko_KR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Locale = Locale' {fromLocale :: Data.Text}
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

pattern Locale_De_DE :: Locale
pattern Locale_De_DE = Locale' "de-DE"

pattern Locale_En_AU :: Locale
pattern Locale_En_AU = Locale' "en-AU"

pattern Locale_En_GB :: Locale
pattern Locale_En_GB = Locale' "en-GB"

pattern Locale_En_IN :: Locale
pattern Locale_En_IN = Locale' "en-IN"

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

pattern Locale_Ja_JP :: Locale
pattern Locale_Ja_JP = Locale' "ja-JP"

pattern Locale_Ko_KR :: Locale
pattern Locale_Ko_KR = Locale' "ko-KR"

{-# COMPLETE
  Locale_De_DE,
  Locale_En_AU,
  Locale_En_GB,
  Locale_En_IN,
  Locale_En_US,
  Locale_Es_419,
  Locale_Es_ES,
  Locale_Es_US,
  Locale_Fr_CA,
  Locale_Fr_FR,
  Locale_It_IT,
  Locale_Ja_JP,
  Locale_Ko_KR,
  Locale'
  #-}
