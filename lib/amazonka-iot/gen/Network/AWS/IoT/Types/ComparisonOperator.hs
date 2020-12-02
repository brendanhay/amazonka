{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ComparisonOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ComparisonOperator where

import Network.AWS.Prelude

data ComparisonOperator
  = GreaterThan
  | GreaterThanEquals
  | InCidrSet
  | InPortSet
  | LessThan
  | LessThanEquals
  | NotInCidrSet
  | NotInPortSet
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
      "greater-than" -> pure GreaterThan
      "greater-than-equals" -> pure GreaterThanEquals
      "in-cidr-set" -> pure InCidrSet
      "in-port-set" -> pure InPortSet
      "less-than" -> pure LessThan
      "less-than-equals" -> pure LessThanEquals
      "not-in-cidr-set" -> pure NotInCidrSet
      "not-in-port-set" -> pure NotInPortSet
      e ->
        fromTextError $
          "Failure parsing ComparisonOperator from value: '" <> e
            <> "'. Accepted values: greater-than, greater-than-equals, in-cidr-set, in-port-set, less-than, less-than-equals, not-in-cidr-set, not-in-port-set"

instance ToText ComparisonOperator where
  toText = \case
    GreaterThan -> "greater-than"
    GreaterThanEquals -> "greater-than-equals"
    InCidrSet -> "in-cidr-set"
    InPortSet -> "in-port-set"
    LessThan -> "less-than"
    LessThanEquals -> "less-than-equals"
    NotInCidrSet -> "not-in-cidr-set"
    NotInPortSet -> "not-in-port-set"

instance Hashable ComparisonOperator

instance NFData ComparisonOperator

instance ToByteString ComparisonOperator

instance ToQuery ComparisonOperator

instance ToHeader ComparisonOperator

instance ToJSON ComparisonOperator where
  toJSON = toJSONText

instance FromJSON ComparisonOperator where
  parseJSON = parseJSONText "ComparisonOperator"
