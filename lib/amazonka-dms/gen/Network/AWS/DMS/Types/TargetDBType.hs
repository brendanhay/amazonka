{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.TargetDBType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.TargetDBType where

import Network.AWS.Prelude

data TargetDBType
  = MultipleDatabases
  | SpecificDatabase
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

instance FromText TargetDBType where
  parser =
    takeLowerText >>= \case
      "multiple-databases" -> pure MultipleDatabases
      "specific-database" -> pure SpecificDatabase
      e ->
        fromTextError $
          "Failure parsing TargetDBType from value: '" <> e
            <> "'. Accepted values: multiple-databases, specific-database"

instance ToText TargetDBType where
  toText = \case
    MultipleDatabases -> "multiple-databases"
    SpecificDatabase -> "specific-database"

instance Hashable TargetDBType

instance NFData TargetDBType

instance ToByteString TargetDBType

instance ToQuery TargetDBType

instance ToHeader TargetDBType

instance ToJSON TargetDBType where
  toJSON = toJSONText

instance FromJSON TargetDBType where
  parseJSON = parseJSONText "TargetDBType"
