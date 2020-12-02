{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.AppStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.AppStatus where

import Network.AWS.Prelude

data AppStatus
  = Active
  | Creating
  | DeleteFailed
  | Deleted
  | Deleting
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

instance FromText AppStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "creating" -> pure Creating
      "delete_failed" -> pure DeleteFailed
      "deleted" -> pure Deleted
      "deleting" -> pure Deleting
      "updating" -> pure Updating
      e ->
        fromTextError $
          "Failure parsing AppStatus from value: '" <> e
            <> "'. Accepted values: active, creating, delete_failed, deleted, deleting, updating"

instance ToText AppStatus where
  toText = \case
    Active -> "ACTIVE"
    Creating -> "CREATING"
    DeleteFailed -> "DELETE_FAILED"
    Deleted -> "DELETED"
    Deleting -> "DELETING"
    Updating -> "UPDATING"

instance Hashable AppStatus

instance NFData AppStatus

instance ToByteString AppStatus

instance ToQuery AppStatus

instance ToHeader AppStatus

instance FromJSON AppStatus where
  parseJSON = parseJSONText "AppStatus"
