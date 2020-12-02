{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.AppReplicationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.AppReplicationStatus where

import Network.AWS.Prelude

data AppReplicationStatus
  = ARSConfigurationInProgress
  | ARSConfigurationInvalid
  | ARSDeltaReplicated
  | ARSDeltaReplicationFailed
  | ARSDeltaReplicationInProgress
  | ARSPartiallyReplicated
  | ARSReadyForConfiguration
  | ARSReadyForReplication
  | ARSReplicated
  | ARSReplicationFailed
  | ARSReplicationInProgress
  | ARSReplicationPending
  | ARSReplicationStopFailed
  | ARSReplicationStopped
  | ARSReplicationStopping
  | ARSValidationInProgress
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

instance FromText AppReplicationStatus where
  parser =
    takeLowerText >>= \case
      "configuration_in_progress" -> pure ARSConfigurationInProgress
      "configuration_invalid" -> pure ARSConfigurationInvalid
      "delta_replicated" -> pure ARSDeltaReplicated
      "delta_replication_failed" -> pure ARSDeltaReplicationFailed
      "delta_replication_in_progress" -> pure ARSDeltaReplicationInProgress
      "partially_replicated" -> pure ARSPartiallyReplicated
      "ready_for_configuration" -> pure ARSReadyForConfiguration
      "ready_for_replication" -> pure ARSReadyForReplication
      "replicated" -> pure ARSReplicated
      "replication_failed" -> pure ARSReplicationFailed
      "replication_in_progress" -> pure ARSReplicationInProgress
      "replication_pending" -> pure ARSReplicationPending
      "replication_stop_failed" -> pure ARSReplicationStopFailed
      "replication_stopped" -> pure ARSReplicationStopped
      "replication_stopping" -> pure ARSReplicationStopping
      "validation_in_progress" -> pure ARSValidationInProgress
      e ->
        fromTextError $
          "Failure parsing AppReplicationStatus from value: '" <> e
            <> "'. Accepted values: configuration_in_progress, configuration_invalid, delta_replicated, delta_replication_failed, delta_replication_in_progress, partially_replicated, ready_for_configuration, ready_for_replication, replicated, replication_failed, replication_in_progress, replication_pending, replication_stop_failed, replication_stopped, replication_stopping, validation_in_progress"

instance ToText AppReplicationStatus where
  toText = \case
    ARSConfigurationInProgress -> "CONFIGURATION_IN_PROGRESS"
    ARSConfigurationInvalid -> "CONFIGURATION_INVALID"
    ARSDeltaReplicated -> "DELTA_REPLICATED"
    ARSDeltaReplicationFailed -> "DELTA_REPLICATION_FAILED"
    ARSDeltaReplicationInProgress -> "DELTA_REPLICATION_IN_PROGRESS"
    ARSPartiallyReplicated -> "PARTIALLY_REPLICATED"
    ARSReadyForConfiguration -> "READY_FOR_CONFIGURATION"
    ARSReadyForReplication -> "READY_FOR_REPLICATION"
    ARSReplicated -> "REPLICATED"
    ARSReplicationFailed -> "REPLICATION_FAILED"
    ARSReplicationInProgress -> "REPLICATION_IN_PROGRESS"
    ARSReplicationPending -> "REPLICATION_PENDING"
    ARSReplicationStopFailed -> "REPLICATION_STOP_FAILED"
    ARSReplicationStopped -> "REPLICATION_STOPPED"
    ARSReplicationStopping -> "REPLICATION_STOPPING"
    ARSValidationInProgress -> "VALIDATION_IN_PROGRESS"

instance Hashable AppReplicationStatus

instance NFData AppReplicationStatus

instance ToByteString AppReplicationStatus

instance ToQuery AppReplicationStatus

instance ToHeader AppReplicationStatus

instance FromJSON AppReplicationStatus where
  parseJSON = parseJSONText "AppReplicationStatus"
