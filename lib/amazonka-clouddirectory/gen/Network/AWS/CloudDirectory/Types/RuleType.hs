{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.RuleType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.RuleType where

import Network.AWS.Prelude

data RuleType
  = BinaryLength
  | NumberComparison
  | StringFromSet
  | StringLength
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

instance FromText RuleType where
  parser =
    takeLowerText >>= \case
      "binary_length" -> pure BinaryLength
      "number_comparison" -> pure NumberComparison
      "string_from_set" -> pure StringFromSet
      "string_length" -> pure StringLength
      e ->
        fromTextError $
          "Failure parsing RuleType from value: '" <> e
            <> "'. Accepted values: binary_length, number_comparison, string_from_set, string_length"

instance ToText RuleType where
  toText = \case
    BinaryLength -> "BINARY_LENGTH"
    NumberComparison -> "NUMBER_COMPARISON"
    StringFromSet -> "STRING_FROM_SET"
    StringLength -> "STRING_LENGTH"

instance Hashable RuleType

instance NFData RuleType

instance ToByteString RuleType

instance ToQuery RuleType

instance ToHeader RuleType

instance ToJSON RuleType where
  toJSON = toJSONText

instance FromJSON RuleType where
  parseJSON = parseJSONText "RuleType"
