{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudHSMv2.Types.Sum where

import Network.AWS.Prelude

data BackupPolicy =
  Default
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BackupPolicy where
    parser = takeLowerText >>= \case
        "default" -> pure Default
        e -> fromTextError $ "Failure parsing BackupPolicy from value: '" <> e
           <> "'. Accepted values: default"

instance ToText BackupPolicy where
    toText = \case
        Default -> "DEFAULT"

instance Hashable     BackupPolicy
instance NFData       BackupPolicy
instance ToByteString BackupPolicy
instance ToQuery      BackupPolicy
instance ToHeader     BackupPolicy

instance FromJSON BackupPolicy where
    parseJSON = parseJSONText "BackupPolicy"

data BackupState
  = CreateInProgress
  | Deleted
  | PendingDeletion
  | Ready
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BackupState where
    parser = takeLowerText >>= \case
        "create_in_progress" -> pure CreateInProgress
        "deleted" -> pure Deleted
        "pending_deletion" -> pure PendingDeletion
        "ready" -> pure Ready
        e -> fromTextError $ "Failure parsing BackupState from value: '" <> e
           <> "'. Accepted values: create_in_progress, deleted, pending_deletion, ready"

instance ToText BackupState where
    toText = \case
        CreateInProgress -> "CREATE_IN_PROGRESS"
        Deleted -> "DELETED"
        PendingDeletion -> "PENDING_DELETION"
        Ready -> "READY"

instance Hashable     BackupState
instance NFData       BackupState
instance ToByteString BackupState
instance ToQuery      BackupState
instance ToHeader     BackupState

instance FromJSON BackupState where
    parseJSON = parseJSONText "BackupState"

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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ClusterState where
    parser = takeLowerText >>= \case
        "active" -> pure CSActive
        "create_in_progress" -> pure CSCreateInProgress
        "degraded" -> pure CSDegraded
        "delete_in_progress" -> pure CSDeleteInProgress
        "deleted" -> pure CSDeleted
        "initialize_in_progress" -> pure CSInitializeInProgress
        "initialized" -> pure CSInitialized
        "uninitialized" -> pure CSUninitialized
        "update_in_progress" -> pure CSUpdateInProgress
        e -> fromTextError $ "Failure parsing ClusterState from value: '" <> e
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

instance Hashable     ClusterState
instance NFData       ClusterState
instance ToByteString ClusterState
instance ToQuery      ClusterState
instance ToHeader     ClusterState

instance FromJSON ClusterState where
    parseJSON = parseJSONText "ClusterState"

data HSMState
  = HSActive
  | HSCreateInProgress
  | HSDegraded
  | HSDeleteInProgress
  | HSDeleted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HSMState where
    parser = takeLowerText >>= \case
        "active" -> pure HSActive
        "create_in_progress" -> pure HSCreateInProgress
        "degraded" -> pure HSDegraded
        "delete_in_progress" -> pure HSDeleteInProgress
        "deleted" -> pure HSDeleted
        e -> fromTextError $ "Failure parsing HSMState from value: '" <> e
           <> "'. Accepted values: active, create_in_progress, degraded, delete_in_progress, deleted"

instance ToText HSMState where
    toText = \case
        HSActive -> "ACTIVE"
        HSCreateInProgress -> "CREATE_IN_PROGRESS"
        HSDegraded -> "DEGRADED"
        HSDeleteInProgress -> "DELETE_IN_PROGRESS"
        HSDeleted -> "DELETED"

instance Hashable     HSMState
instance NFData       HSMState
instance ToByteString HSMState
instance ToQuery      HSMState
instance ToHeader     HSMState

instance FromJSON HSMState where
    parseJSON = parseJSONText "HSMState"
