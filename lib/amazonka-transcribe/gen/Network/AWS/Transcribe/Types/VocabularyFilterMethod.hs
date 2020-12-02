{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.VocabularyFilterMethod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.VocabularyFilterMethod where

import Network.AWS.Prelude

data VocabularyFilterMethod
  = Mask
  | Remove
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

instance FromText VocabularyFilterMethod where
  parser =
    takeLowerText >>= \case
      "mask" -> pure Mask
      "remove" -> pure Remove
      e ->
        fromTextError $
          "Failure parsing VocabularyFilterMethod from value: '" <> e
            <> "'. Accepted values: mask, remove"

instance ToText VocabularyFilterMethod where
  toText = \case
    Mask -> "mask"
    Remove -> "remove"

instance Hashable VocabularyFilterMethod

instance NFData VocabularyFilterMethod

instance ToByteString VocabularyFilterMethod

instance ToQuery VocabularyFilterMethod

instance ToHeader VocabularyFilterMethod

instance ToJSON VocabularyFilterMethod where
  toJSON = toJSONText

instance FromJSON VocabularyFilterMethod where
  parseJSON = parseJSONText "VocabularyFilterMethod"
