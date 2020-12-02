{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.EventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.EventType where

import Network.AWS.Prelude

data EventType
  = CreateAction
  | DeleteAction
  | ExecuteAction
  | System
  | UpdateAction
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

instance FromText EventType where
  parser =
    takeLowerText >>= \case
      "create_action" -> pure CreateAction
      "delete_action" -> pure DeleteAction
      "execute_action" -> pure ExecuteAction
      "system" -> pure System
      "update_action" -> pure UpdateAction
      e ->
        fromTextError $
          "Failure parsing EventType from value: '" <> e
            <> "'. Accepted values: create_action, delete_action, execute_action, system, update_action"

instance ToText EventType where
  toText = \case
    CreateAction -> "CREATE_ACTION"
    DeleteAction -> "DELETE_ACTION"
    ExecuteAction -> "EXECUTE_ACTION"
    System -> "SYSTEM"
    UpdateAction -> "UPDATE_ACTION"

instance Hashable EventType

instance NFData EventType

instance ToByteString EventType

instance ToQuery EventType

instance ToHeader EventType

instance FromJSON EventType where
  parseJSON = parseJSONText "EventType"
