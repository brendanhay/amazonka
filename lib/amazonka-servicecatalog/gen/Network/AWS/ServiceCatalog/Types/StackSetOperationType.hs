{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.StackSetOperationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.StackSetOperationType where

import Network.AWS.Prelude

data StackSetOperationType
  = Create
  | Delete
  | Update
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

instance FromText StackSetOperationType where
  parser =
    takeLowerText >>= \case
      "create" -> pure Create
      "delete" -> pure Delete
      "update" -> pure Update
      e ->
        fromTextError $
          "Failure parsing StackSetOperationType from value: '" <> e
            <> "'. Accepted values: create, delete, update"

instance ToText StackSetOperationType where
  toText = \case
    Create -> "CREATE"
    Delete -> "DELETE"
    Update -> "UPDATE"

instance Hashable StackSetOperationType

instance NFData StackSetOperationType

instance ToByteString StackSetOperationType

instance ToQuery StackSetOperationType

instance ToHeader StackSetOperationType

instance ToJSON StackSetOperationType where
  toJSON = toJSONText
