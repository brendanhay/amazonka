{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.NumericOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.NumericOperator where

import Network.AWS.Prelude

data NumericOperator
  = Between
  | Equal
  | GreaterThan
  | GreaterThanOrEqual
  | LessThan
  | LessThanOrEqual
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

instance FromText NumericOperator where
  parser =
    takeLowerText >>= \case
      "between" -> pure Between
      "equal" -> pure Equal
      "greater_than" -> pure GreaterThan
      "greater_than_or_equal" -> pure GreaterThanOrEqual
      "less_than" -> pure LessThan
      "less_than_or_equal" -> pure LessThanOrEqual
      e ->
        fromTextError $
          "Failure parsing NumericOperator from value: '" <> e
            <> "'. Accepted values: between, equal, greater_than, greater_than_or_equal, less_than, less_than_or_equal"

instance ToText NumericOperator where
  toText = \case
    Between -> "BETWEEN"
    Equal -> "EQUAL"
    GreaterThan -> "GREATER_THAN"
    GreaterThanOrEqual -> "GREATER_THAN_OR_EQUAL"
    LessThan -> "LESS_THAN"
    LessThanOrEqual -> "LESS_THAN_OR_EQUAL"

instance Hashable NumericOperator

instance NFData NumericOperator

instance ToByteString NumericOperator

instance ToQuery NumericOperator

instance ToHeader NumericOperator

instance ToJSON NumericOperator where
  toJSON = toJSONText
