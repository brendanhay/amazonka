{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.RuleOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.RuleOperator where

import Network.AWS.Prelude

data RuleOperator
  = Contains
  | Equals
  | GreaterThan
  | GreaterThanOrEquals
  | IN
  | LessThan
  | LessThanOrEquals
  | NotIn
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

instance FromText RuleOperator where
  parser =
    takeLowerText >>= \case
      "contains" -> pure Contains
      "equals" -> pure Equals
      "greater_than" -> pure GreaterThan
      "greater_than_or_equals" -> pure GreaterThanOrEquals
      "in" -> pure IN
      "less_than" -> pure LessThan
      "less_than_or_equals" -> pure LessThanOrEquals
      "not_in" -> pure NotIn
      e ->
        fromTextError $
          "Failure parsing RuleOperator from value: '" <> e
            <> "'. Accepted values: contains, equals, greater_than, greater_than_or_equals, in, less_than, less_than_or_equals, not_in"

instance ToText RuleOperator where
  toText = \case
    Contains -> "CONTAINS"
    Equals -> "EQUALS"
    GreaterThan -> "GREATER_THAN"
    GreaterThanOrEquals -> "GREATER_THAN_OR_EQUALS"
    IN -> "IN"
    LessThan -> "LESS_THAN"
    LessThanOrEquals -> "LESS_THAN_OR_EQUALS"
    NotIn -> "NOT_IN"

instance Hashable RuleOperator

instance NFData RuleOperator

instance ToByteString RuleOperator

instance ToQuery RuleOperator

instance ToHeader RuleOperator

instance ToJSON RuleOperator where
  toJSON = toJSONText

instance FromJSON RuleOperator where
  parseJSON = parseJSONText "RuleOperator"
