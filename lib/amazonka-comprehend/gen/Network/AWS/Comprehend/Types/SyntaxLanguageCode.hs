{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.SyntaxLanguageCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.SyntaxLanguageCode where

import Network.AWS.Prelude

data SyntaxLanguageCode
  = SLCDE
  | SLCEN
  | SLCES
  | SLCFR
  | SLCIT
  | SLCPT
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

instance FromText SyntaxLanguageCode where
  parser =
    takeLowerText >>= \case
      "de" -> pure SLCDE
      "en" -> pure SLCEN
      "es" -> pure SLCES
      "fr" -> pure SLCFR
      "it" -> pure SLCIT
      "pt" -> pure SLCPT
      e ->
        fromTextError $
          "Failure parsing SyntaxLanguageCode from value: '" <> e
            <> "'. Accepted values: de, en, es, fr, it, pt"

instance ToText SyntaxLanguageCode where
  toText = \case
    SLCDE -> "de"
    SLCEN -> "en"
    SLCES -> "es"
    SLCFR -> "fr"
    SLCIT -> "it"
    SLCPT -> "pt"

instance Hashable SyntaxLanguageCode

instance NFData SyntaxLanguageCode

instance ToByteString SyntaxLanguageCode

instance ToQuery SyntaxLanguageCode

instance ToHeader SyntaxLanguageCode

instance ToJSON SyntaxLanguageCode where
  toJSON = toJSONText
