{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ComparisonOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ComparisonOperator where

import Network.AWS.Prelude

data ComparisonOperator
  = BeginsWith
  | Between
  | Contains
  | EQ'
  | GE
  | GT'
  | IN
  | LE
  | LT'
  | NE
  | NotContains
  | NotNull
  | Null
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

instance FromText ComparisonOperator where
  parser =
    takeLowerText >>= \case
      "begins_with" -> pure BeginsWith
      "between" -> pure Between
      "contains" -> pure Contains
      "eq" -> pure EQ'
      "ge" -> pure GE
      "gt" -> pure GT'
      "in" -> pure IN
      "le" -> pure LE
      "lt" -> pure LT'
      "ne" -> pure NE
      "not_contains" -> pure NotContains
      "not_null" -> pure NotNull
      "null" -> pure Null
      e ->
        fromTextError $
          "Failure parsing ComparisonOperator from value: '" <> e
            <> "'. Accepted values: begins_with, between, contains, eq, ge, gt, in, le, lt, ne, not_contains, not_null, null"

instance ToText ComparisonOperator where
  toText = \case
    BeginsWith -> "BEGINS_WITH"
    Between -> "BETWEEN"
    Contains -> "CONTAINS"
    EQ' -> "EQ"
    GE -> "GE"
    GT' -> "GT"
    IN -> "IN"
    LE -> "LE"
    LT' -> "LT"
    NE -> "NE"
    NotContains -> "NOT_CONTAINS"
    NotNull -> "NOT_NULL"
    Null -> "NULL"

instance Hashable ComparisonOperator

instance NFData ComparisonOperator

instance ToByteString ComparisonOperator

instance ToQuery ComparisonOperator

instance ToHeader ComparisonOperator

instance ToJSON ComparisonOperator where
  toJSON = toJSONText
