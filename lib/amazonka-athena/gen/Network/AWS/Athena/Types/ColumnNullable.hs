{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.ColumnNullable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.ColumnNullable where

import Network.AWS.Prelude

data ColumnNullable
  = NotNull
  | Nullable
  | Unknown
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

instance FromText ColumnNullable where
  parser =
    takeLowerText >>= \case
      "not_null" -> pure NotNull
      "nullable" -> pure Nullable
      "unknown" -> pure Unknown
      e ->
        fromTextError $
          "Failure parsing ColumnNullable from value: '" <> e
            <> "'. Accepted values: not_null, nullable, unknown"

instance ToText ColumnNullable where
  toText = \case
    NotNull -> "NOT_NULL"
    Nullable -> "NULLABLE"
    Unknown -> "UNKNOWN"

instance Hashable ColumnNullable

instance NFData ColumnNullable

instance ToByteString ColumnNullable

instance ToQuery ColumnNullable

instance ToHeader ColumnNullable

instance FromJSON ColumnNullable where
  parseJSON = parseJSONText "ColumnNullable"
