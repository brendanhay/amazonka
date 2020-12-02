{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.ClusterState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types.ClusterState where

import Network.AWS.Prelude

data ClusterState
  = CSActive
  | CSCreateInProgress
  | CSDegraded
  | CSDeleteInProgress
  | CSDeleted
  | CSInitializeInProgress
  | CSInitialized
  | CSUninitialized
  | CSUpdateInProgress
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

instance FromText ClusterState where
  parser =
    takeLowerText >>= \case
      "active" -> pure CSActive
      "create_in_progress" -> pure CSCreateInProgress
      "degraded" -> pure CSDegraded
      "delete_in_progress" -> pure CSDeleteInProgress
      "deleted" -> pure CSDeleted
      "initialize_in_progress" -> pure CSInitializeInProgress
      "initialized" -> pure CSInitialized
      "uninitialized" -> pure CSUninitialized
      "update_in_progress" -> pure CSUpdateInProgress
      e ->
        fromTextError $
          "Failure parsing ClusterState from value: '" <> e
            <> "'. Accepted values: active, create_in_progress, degraded, delete_in_progress, deleted, initialize_in_progress, initialized, uninitialized, update_in_progress"

instance ToText ClusterState where
  toText = \case
    CSActive -> "ACTIVE"
    CSCreateInProgress -> "CREATE_IN_PROGRESS"
    CSDegraded -> "DEGRADED"
    CSDeleteInProgress -> "DELETE_IN_PROGRESS"
    CSDeleted -> "DELETED"
    CSInitializeInProgress -> "INITIALIZE_IN_PROGRESS"
    CSInitialized -> "INITIALIZED"
    CSUninitialized -> "UNINITIALIZED"
    CSUpdateInProgress -> "UPDATE_IN_PROGRESS"

instance Hashable ClusterState

instance NFData ClusterState

instance ToByteString ClusterState

instance ToQuery ClusterState

instance ToHeader ClusterState

instance FromJSON ClusterState where
  parseJSON = parseJSONText "ClusterState"
