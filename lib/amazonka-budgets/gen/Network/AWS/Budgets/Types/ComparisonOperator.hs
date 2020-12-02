{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.ComparisonOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.ComparisonOperator where

import Network.AWS.Prelude

-- | The comparison operator of a notification. Currently the service supports the following operators:
--
--
-- @GREATER_THAN@ , @LESS_THAN@ , @EQUAL_TO@
data ComparisonOperator
  = EqualTo
  | GreaterThan
  | LessThan
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
      "equal_to" -> pure EqualTo
      "greater_than" -> pure GreaterThan
      "less_than" -> pure LessThan
      e ->
        fromTextError $
          "Failure parsing ComparisonOperator from value: '" <> e
            <> "'. Accepted values: equal_to, greater_than, less_than"

instance ToText ComparisonOperator where
  toText = \case
    EqualTo -> "EQUAL_TO"
    GreaterThan -> "GREATER_THAN"
    LessThan -> "LESS_THAN"

instance Hashable ComparisonOperator

instance NFData ComparisonOperator

instance ToByteString ComparisonOperator

instance ToQuery ComparisonOperator

instance ToHeader ComparisonOperator

instance ToJSON ComparisonOperator where
  toJSON = toJSONText

instance FromJSON ComparisonOperator where
  parseJSON = parseJSONText "ComparisonOperator"
