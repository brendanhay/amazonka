{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioLanguageSelectionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioLanguageSelectionPolicy where

import Network.AWS.Prelude

-- | Audio Language Selection Policy
data AudioLanguageSelectionPolicy
  = Loose
  | Strict
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

instance FromText AudioLanguageSelectionPolicy where
  parser =
    takeLowerText >>= \case
      "loose" -> pure Loose
      "strict" -> pure Strict
      e ->
        fromTextError $
          "Failure parsing AudioLanguageSelectionPolicy from value: '" <> e
            <> "'. Accepted values: loose, strict"

instance ToText AudioLanguageSelectionPolicy where
  toText = \case
    Loose -> "LOOSE"
    Strict -> "STRICT"

instance Hashable AudioLanguageSelectionPolicy

instance NFData AudioLanguageSelectionPolicy

instance ToByteString AudioLanguageSelectionPolicy

instance ToQuery AudioLanguageSelectionPolicy

instance ToHeader AudioLanguageSelectionPolicy

instance ToJSON AudioLanguageSelectionPolicy where
  toJSON = toJSONText

instance FromJSON AudioLanguageSelectionPolicy where
  parseJSON = parseJSONText "AudioLanguageSelectionPolicy"
