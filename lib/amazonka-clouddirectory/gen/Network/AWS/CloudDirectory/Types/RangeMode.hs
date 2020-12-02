{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.RangeMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.RangeMode where

import Network.AWS.Prelude

data RangeMode
  = Exclusive
  | First
  | Inclusive
  | Last
  | LastBeforeMissingValues
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

instance FromText RangeMode where
  parser =
    takeLowerText >>= \case
      "exclusive" -> pure Exclusive
      "first" -> pure First
      "inclusive" -> pure Inclusive
      "last" -> pure Last
      "last_before_missing_values" -> pure LastBeforeMissingValues
      e ->
        fromTextError $
          "Failure parsing RangeMode from value: '" <> e
            <> "'. Accepted values: exclusive, first, inclusive, last, last_before_missing_values"

instance ToText RangeMode where
  toText = \case
    Exclusive -> "EXCLUSIVE"
    First -> "FIRST"
    Inclusive -> "INCLUSIVE"
    Last -> "LAST"
    LastBeforeMissingValues -> "LAST_BEFORE_MISSING_VALUES"

instance Hashable RangeMode

instance NFData RangeMode

instance ToByteString RangeMode

instance ToQuery RangeMode

instance ToHeader RangeMode

instance ToJSON RangeMode where
  toJSON = toJSONText
