{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.LogicalOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.LogicalOperator where

import Network.AWS.Prelude

data LogicalOperator = Equals
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

instance FromText LogicalOperator where
  parser =
    takeLowerText >>= \case
      "equals" -> pure Equals
      e ->
        fromTextError $
          "Failure parsing LogicalOperator from value: '" <> e
            <> "'. Accepted values: equals"

instance ToText LogicalOperator where
  toText = \case
    Equals -> "EQUALS"

instance Hashable LogicalOperator

instance NFData LogicalOperator

instance ToByteString LogicalOperator

instance ToQuery LogicalOperator

instance ToHeader LogicalOperator

instance ToJSON LogicalOperator where
  toJSON = toJSONText

instance FromJSON LogicalOperator where
  parseJSON = parseJSONText "LogicalOperator"
