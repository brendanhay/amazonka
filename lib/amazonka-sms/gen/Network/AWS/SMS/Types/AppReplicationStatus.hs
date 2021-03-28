{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.AppReplicationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.AppReplicationStatus
  ( AppReplicationStatus
    ( AppReplicationStatus'
    , AppReplicationStatusReadyForConfiguration
    , AppReplicationStatusConfigurationInProgress
    , AppReplicationStatusConfigurationInvalid
    , AppReplicationStatusReadyForReplication
    , AppReplicationStatusValidationInProgress
    , AppReplicationStatusReplicationPending
    , AppReplicationStatusReplicationInProgress
    , AppReplicationStatusReplicated
    , AppReplicationStatusPartiallyReplicated
    , AppReplicationStatusDeltaReplicationInProgress
    , AppReplicationStatusDeltaReplicated
    , AppReplicationStatusDeltaReplicationFailed
    , AppReplicationStatusReplicationFailed
    , AppReplicationStatusReplicationStopping
    , AppReplicationStatusReplicationStopFailed
    , AppReplicationStatusReplicationStopped
    , fromAppReplicationStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype AppReplicationStatus = AppReplicationStatus'{fromAppReplicationStatus
                                                     :: Core.Text}
                                 deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                 Core.Generic)
                                 deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                   Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                   Core.FromJSON, Core.ToXML, Core.FromXML,
                                                   Core.ToText, Core.FromText, Core.ToByteString,
                                                   Core.ToQuery, Core.ToHeader)

pattern AppReplicationStatusReadyForConfiguration :: AppReplicationStatus
pattern AppReplicationStatusReadyForConfiguration = AppReplicationStatus' "READY_FOR_CONFIGURATION"

pattern AppReplicationStatusConfigurationInProgress :: AppReplicationStatus
pattern AppReplicationStatusConfigurationInProgress = AppReplicationStatus' "CONFIGURATION_IN_PROGRESS"

pattern AppReplicationStatusConfigurationInvalid :: AppReplicationStatus
pattern AppReplicationStatusConfigurationInvalid = AppReplicationStatus' "CONFIGURATION_INVALID"

pattern AppReplicationStatusReadyForReplication :: AppReplicationStatus
pattern AppReplicationStatusReadyForReplication = AppReplicationStatus' "READY_FOR_REPLICATION"

pattern AppReplicationStatusValidationInProgress :: AppReplicationStatus
pattern AppReplicationStatusValidationInProgress = AppReplicationStatus' "VALIDATION_IN_PROGRESS"

pattern AppReplicationStatusReplicationPending :: AppReplicationStatus
pattern AppReplicationStatusReplicationPending = AppReplicationStatus' "REPLICATION_PENDING"

pattern AppReplicationStatusReplicationInProgress :: AppReplicationStatus
pattern AppReplicationStatusReplicationInProgress = AppReplicationStatus' "REPLICATION_IN_PROGRESS"

pattern AppReplicationStatusReplicated :: AppReplicationStatus
pattern AppReplicationStatusReplicated = AppReplicationStatus' "REPLICATED"

pattern AppReplicationStatusPartiallyReplicated :: AppReplicationStatus
pattern AppReplicationStatusPartiallyReplicated = AppReplicationStatus' "PARTIALLY_REPLICATED"

pattern AppReplicationStatusDeltaReplicationInProgress :: AppReplicationStatus
pattern AppReplicationStatusDeltaReplicationInProgress = AppReplicationStatus' "DELTA_REPLICATION_IN_PROGRESS"

pattern AppReplicationStatusDeltaReplicated :: AppReplicationStatus
pattern AppReplicationStatusDeltaReplicated = AppReplicationStatus' "DELTA_REPLICATED"

pattern AppReplicationStatusDeltaReplicationFailed :: AppReplicationStatus
pattern AppReplicationStatusDeltaReplicationFailed = AppReplicationStatus' "DELTA_REPLICATION_FAILED"

pattern AppReplicationStatusReplicationFailed :: AppReplicationStatus
pattern AppReplicationStatusReplicationFailed = AppReplicationStatus' "REPLICATION_FAILED"

pattern AppReplicationStatusReplicationStopping :: AppReplicationStatus
pattern AppReplicationStatusReplicationStopping = AppReplicationStatus' "REPLICATION_STOPPING"

pattern AppReplicationStatusReplicationStopFailed :: AppReplicationStatus
pattern AppReplicationStatusReplicationStopFailed = AppReplicationStatus' "REPLICATION_STOP_FAILED"

pattern AppReplicationStatusReplicationStopped :: AppReplicationStatus
pattern AppReplicationStatusReplicationStopped = AppReplicationStatus' "REPLICATION_STOPPED"

{-# COMPLETE 
  AppReplicationStatusReadyForConfiguration,

  AppReplicationStatusConfigurationInProgress,

  AppReplicationStatusConfigurationInvalid,

  AppReplicationStatusReadyForReplication,

  AppReplicationStatusValidationInProgress,

  AppReplicationStatusReplicationPending,

  AppReplicationStatusReplicationInProgress,

  AppReplicationStatusReplicated,

  AppReplicationStatusPartiallyReplicated,

  AppReplicationStatusDeltaReplicationInProgress,

  AppReplicationStatusDeltaReplicated,

  AppReplicationStatusDeltaReplicationFailed,

  AppReplicationStatusReplicationFailed,

  AppReplicationStatusReplicationStopping,

  AppReplicationStatusReplicationStopFailed,

  AppReplicationStatusReplicationStopped,
  AppReplicationStatus'
  #-}
