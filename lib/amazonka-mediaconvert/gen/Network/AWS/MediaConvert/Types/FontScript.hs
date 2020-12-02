{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.FontScript
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.FontScript where

import Network.AWS.Prelude

-- | Provide the font script, using an ISO 15924 script code, if the LanguageCode is not sufficient for determining the script type. Where LanguageCode or CustomLanguageCode is sufficient, use "AUTOMATIC" or leave unset.
data FontScript
  = Automatic
  | Hans
  | Hant
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

instance FromText FontScript where
  parser =
    takeLowerText >>= \case
      "automatic" -> pure Automatic
      "hans" -> pure Hans
      "hant" -> pure Hant
      e ->
        fromTextError $
          "Failure parsing FontScript from value: '" <> e
            <> "'. Accepted values: automatic, hans, hant"

instance ToText FontScript where
  toText = \case
    Automatic -> "AUTOMATIC"
    Hans -> "HANS"
    Hant -> "HANT"

instance Hashable FontScript

instance NFData FontScript

instance ToByteString FontScript

instance ToQuery FontScript

instance ToHeader FontScript

instance ToJSON FontScript where
  toJSON = toJSONText

instance FromJSON FontScript where
  parseJSON = parseJSONText "FontScript"
