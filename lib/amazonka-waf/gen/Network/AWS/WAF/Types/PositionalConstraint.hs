{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.PositionalConstraint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.PositionalConstraint where

import Network.AWS.Prelude

data PositionalConstraint
  = Contains
  | ContainsWord
  | EndsWith
  | Exactly
  | StartsWith
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

instance FromText PositionalConstraint where
  parser =
    takeLowerText >>= \case
      "contains" -> pure Contains
      "contains_word" -> pure ContainsWord
      "ends_with" -> pure EndsWith
      "exactly" -> pure Exactly
      "starts_with" -> pure StartsWith
      e ->
        fromTextError $
          "Failure parsing PositionalConstraint from value: '" <> e
            <> "'. Accepted values: contains, contains_word, ends_with, exactly, starts_with"

instance ToText PositionalConstraint where
  toText = \case
    Contains -> "CONTAINS"
    ContainsWord -> "CONTAINS_WORD"
    EndsWith -> "ENDS_WITH"
    Exactly -> "EXACTLY"
    StartsWith -> "STARTS_WITH"

instance Hashable PositionalConstraint

instance NFData PositionalConstraint

instance ToByteString PositionalConstraint

instance ToQuery PositionalConstraint

instance ToHeader PositionalConstraint

instance ToJSON PositionalConstraint where
  toJSON = toJSONText

instance FromJSON PositionalConstraint where
  parseJSON = parseJSONText "PositionalConstraint"
