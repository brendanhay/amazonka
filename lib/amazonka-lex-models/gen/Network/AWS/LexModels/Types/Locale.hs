{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.Locale
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.Locale where

import Network.AWS.Prelude

data Locale
  = DeDe
  | EnAu
  | EnGb
  | EnUs
  | Es419
  | EsEs
  | EsUs
  | FrCa
  | FrFr
  | ItIt
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText Locale where
  parser =
    takeLowerText >>= \case
      "de-de" -> pure DeDe
      "en-au" -> pure EnAu
      "en-gb" -> pure EnGb
      "en-us" -> pure EnUs
      "es-419" -> pure Es419
      "es-es" -> pure EsEs
      "es-us" -> pure EsUs
      "fr-ca" -> pure FrCa
      "fr-fr" -> pure FrFr
      "it-it" -> pure ItIt
      e ->
        fromTextError $
          "Failure parsing Locale from value: '" <> e
            <> "'. Accepted values: de-de, en-au, en-gb, en-us, es-419, es-es, es-us, fr-ca, fr-fr, it-it"

instance ToText Locale where
  toText = \case
    DeDe -> "de-DE"
    EnAu -> "en-AU"
    EnGb -> "en-GB"
    EnUs -> "en-US"
    Es419 -> "es-419"
    EsEs -> "es-ES"
    EsUs -> "es-US"
    FrCa -> "fr-CA"
    FrFr -> "fr-FR"
    ItIt -> "it-IT"

instance Hashable Locale

instance NFData Locale

instance ToByteString Locale

instance ToQuery Locale

instance ToHeader Locale

instance ToJSON Locale where
  toJSON = toJSONText

instance FromJSON Locale where
  parseJSON = parseJSONText "Locale"
