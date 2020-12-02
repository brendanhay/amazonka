{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.ScalingStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.ScalingStatusType where

import Network.AWS.Prelude

data ScalingStatusType
  = Active
  | DeleteRequested
  | Deleted
  | Deleting
  | Error'
  | UpdateRequested
  | Updating
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

instance FromText ScalingStatusType where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "delete_requested" -> pure DeleteRequested
      "deleted" -> pure Deleted
      "deleting" -> pure Deleting
      "error" -> pure Error'
      "update_requested" -> pure UpdateRequested
      "updating" -> pure Updating
      e ->
        fromTextError $
          "Failure parsing ScalingStatusType from value: '" <> e
            <> "'. Accepted values: active, delete_requested, deleted, deleting, error, update_requested, updating"

instance ToText ScalingStatusType where
  toText = \case
    Active -> "ACTIVE"
    DeleteRequested -> "DELETE_REQUESTED"
    Deleted -> "DELETED"
    Deleting -> "DELETING"
    Error' -> "ERROR"
    UpdateRequested -> "UPDATE_REQUESTED"
    Updating -> "UPDATING"

instance Hashable ScalingStatusType

instance NFData ScalingStatusType

instance ToByteString ScalingStatusType

instance ToQuery ScalingStatusType

instance ToHeader ScalingStatusType

instance ToJSON ScalingStatusType where
  toJSON = toJSONText

instance FromJSON ScalingStatusType where
  parseJSON = parseJSONText "ScalingStatusType"
