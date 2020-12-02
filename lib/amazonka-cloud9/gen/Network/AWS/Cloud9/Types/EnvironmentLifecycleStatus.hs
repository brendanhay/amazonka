{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.Types.EnvironmentLifecycleStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Cloud9.Types.EnvironmentLifecycleStatus where

import Network.AWS.Prelude

data EnvironmentLifecycleStatus
  = CreateFailed
  | Created
  | Creating
  | DeleteFailed
  | Deleting
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

instance FromText EnvironmentLifecycleStatus where
  parser =
    takeLowerText >>= \case
      "create_failed" -> pure CreateFailed
      "created" -> pure Created
      "creating" -> pure Creating
      "delete_failed" -> pure DeleteFailed
      "deleting" -> pure Deleting
      e ->
        fromTextError $
          "Failure parsing EnvironmentLifecycleStatus from value: '" <> e
            <> "'. Accepted values: create_failed, created, creating, delete_failed, deleting"

instance ToText EnvironmentLifecycleStatus where
  toText = \case
    CreateFailed -> "CREATE_FAILED"
    Created -> "CREATED"
    Creating -> "CREATING"
    DeleteFailed -> "DELETE_FAILED"
    Deleting -> "DELETING"

instance Hashable EnvironmentLifecycleStatus

instance NFData EnvironmentLifecycleStatus

instance ToByteString EnvironmentLifecycleStatus

instance ToQuery EnvironmentLifecycleStatus

instance ToHeader EnvironmentLifecycleStatus

instance FromJSON EnvironmentLifecycleStatus where
  parseJSON = parseJSONText "EnvironmentLifecycleStatus"
