{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ComparisonOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ComparisonOperator where

import Network.AWS.Prelude

data ComparisonOperator
  = GreaterThan
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

instance FromText ComparisonOperator where
  parser =
    takeLowerText >>= \case
      "greater_than" -> pure GreaterThan
      "greater_than_or_equal" -> pure GreaterThanOrEqual
      "less_than" -> pure LessThan
      "less_than_or_equal" -> pure LessThanOrEqual
      e ->
        fromTextError $
          "Failure parsing ComparisonOperator from value: '" <> e
            <> "'. Accepted values: greater_than, greater_than_or_equal, less_than, less_than_or_equal"

instance ToText ComparisonOperator where
  toText = \case
    GreaterThan -> "GREATER_THAN"
    GreaterThanOrEqual -> "GREATER_THAN_OR_EQUAL"
    LessThan -> "LESS_THAN"
    LessThanOrEqual -> "LESS_THAN_OR_EQUAL"

instance Hashable ComparisonOperator

instance NFData ComparisonOperator

instance ToByteString ComparisonOperator

instance ToQuery ComparisonOperator

instance ToHeader ComparisonOperator

instance ToJSON ComparisonOperator where
  toJSON = toJSONText

instance FromJSON ComparisonOperator where
  parseJSON = parseJSONText "ComparisonOperator"
