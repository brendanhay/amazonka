{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Permission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Permission where

import Network.AWS.Prelude

data Permission
  = PAll
  | PAlter
  | PCreateDatabase
  | PCreateTable
  | PDataLocationAccess
  | PDelete
  | PDrop
  | PInsert
  | PSelect
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

instance FromText Permission where
  parser =
    takeLowerText >>= \case
      "all" -> pure PAll
      "alter" -> pure PAlter
      "create_database" -> pure PCreateDatabase
      "create_table" -> pure PCreateTable
      "data_location_access" -> pure PDataLocationAccess
      "delete" -> pure PDelete
      "drop" -> pure PDrop
      "insert" -> pure PInsert
      "select" -> pure PSelect
      e ->
        fromTextError $
          "Failure parsing Permission from value: '" <> e
            <> "'. Accepted values: all, alter, create_database, create_table, data_location_access, delete, drop, insert, select"

instance ToText Permission where
  toText = \case
    PAll -> "ALL"
    PAlter -> "ALTER"
    PCreateDatabase -> "CREATE_DATABASE"
    PCreateTable -> "CREATE_TABLE"
    PDataLocationAccess -> "DATA_LOCATION_ACCESS"
    PDelete -> "DELETE"
    PDrop -> "DROP"
    PInsert -> "INSERT"
    PSelect -> "SELECT"

instance Hashable Permission

instance NFData Permission

instance ToByteString Permission

instance ToQuery Permission

instance ToHeader Permission

instance ToJSON Permission where
  toJSON = toJSONText

instance FromJSON Permission where
  parseJSON = parseJSONText "Permission"
