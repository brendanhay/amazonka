{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Comparator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Comparator where

import Network.AWS.Prelude

data Comparator
  = CEquals
  | CGreaterThan
  | CGreaterThanEquals
  | CLessThan
  | CLessThanEquals
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

instance FromText Comparator where
  parser =
    takeLowerText >>= \case
      "equals" -> pure CEquals
      "greater_than" -> pure CGreaterThan
      "greater_than_equals" -> pure CGreaterThanEquals
      "less_than" -> pure CLessThan
      "less_than_equals" -> pure CLessThanEquals
      e ->
        fromTextError $
          "Failure parsing Comparator from value: '" <> e
            <> "'. Accepted values: equals, greater_than, greater_than_equals, less_than, less_than_equals"

instance ToText Comparator where
  toText = \case
    CEquals -> "EQUALS"
    CGreaterThan -> "GREATER_THAN"
    CGreaterThanEquals -> "GREATER_THAN_EQUALS"
    CLessThan -> "LESS_THAN"
    CLessThanEquals -> "LESS_THAN_EQUALS"

instance Hashable Comparator

instance NFData Comparator

instance ToByteString Comparator

instance ToQuery Comparator

instance ToHeader Comparator

instance ToJSON Comparator where
  toJSON = toJSONText
