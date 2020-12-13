{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.AppReplicationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.AppReplicationStatus
  ( AppReplicationStatus
      ( AppReplicationStatus',
        ARSReadyForConfiguration,
        ARSConfigurationInProgress,
        ARSConfigurationInvalid,
        ARSReadyForReplication,
        ARSValidationInProgress,
        ARSReplicationPending,
        ARSReplicationInProgress,
        ARSReplicated,
        ARSPartiallyReplicated,
        ARSDeltaReplicationInProgress,
        ARSDeltaReplicated,
        ARSDeltaReplicationFailed,
        ARSReplicationFailed,
        ARSReplicationStopping,
        ARSReplicationStopFailed,
        ARSReplicationStopped
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AppReplicationStatus = AppReplicationStatus' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern ARSReadyForConfiguration :: AppReplicationStatus
pattern ARSReadyForConfiguration = AppReplicationStatus' "READY_FOR_CONFIGURATION"

pattern ARSConfigurationInProgress :: AppReplicationStatus
pattern ARSConfigurationInProgress = AppReplicationStatus' "CONFIGURATION_IN_PROGRESS"

pattern ARSConfigurationInvalid :: AppReplicationStatus
pattern ARSConfigurationInvalid = AppReplicationStatus' "CONFIGURATION_INVALID"

pattern ARSReadyForReplication :: AppReplicationStatus
pattern ARSReadyForReplication = AppReplicationStatus' "READY_FOR_REPLICATION"

pattern ARSValidationInProgress :: AppReplicationStatus
pattern ARSValidationInProgress = AppReplicationStatus' "VALIDATION_IN_PROGRESS"

pattern ARSReplicationPending :: AppReplicationStatus
pattern ARSReplicationPending = AppReplicationStatus' "REPLICATION_PENDING"

pattern ARSReplicationInProgress :: AppReplicationStatus
pattern ARSReplicationInProgress = AppReplicationStatus' "REPLICATION_IN_PROGRESS"

pattern ARSReplicated :: AppReplicationStatus
pattern ARSReplicated = AppReplicationStatus' "REPLICATED"

pattern ARSPartiallyReplicated :: AppReplicationStatus
pattern ARSPartiallyReplicated = AppReplicationStatus' "PARTIALLY_REPLICATED"

pattern ARSDeltaReplicationInProgress :: AppReplicationStatus
pattern ARSDeltaReplicationInProgress = AppReplicationStatus' "DELTA_REPLICATION_IN_PROGRESS"

pattern ARSDeltaReplicated :: AppReplicationStatus
pattern ARSDeltaReplicated = AppReplicationStatus' "DELTA_REPLICATED"

pattern ARSDeltaReplicationFailed :: AppReplicationStatus
pattern ARSDeltaReplicationFailed = AppReplicationStatus' "DELTA_REPLICATION_FAILED"

pattern ARSReplicationFailed :: AppReplicationStatus
pattern ARSReplicationFailed = AppReplicationStatus' "REPLICATION_FAILED"

pattern ARSReplicationStopping :: AppReplicationStatus
pattern ARSReplicationStopping = AppReplicationStatus' "REPLICATION_STOPPING"

pattern ARSReplicationStopFailed :: AppReplicationStatus
pattern ARSReplicationStopFailed = AppReplicationStatus' "REPLICATION_STOP_FAILED"

pattern ARSReplicationStopped :: AppReplicationStatus
pattern ARSReplicationStopped = AppReplicationStatus' "REPLICATION_STOPPED"

{-# COMPLETE
  ARSReadyForConfiguration,
  ARSConfigurationInProgress,
  ARSConfigurationInvalid,
  ARSReadyForReplication,
  ARSValidationInProgress,
  ARSReplicationPending,
  ARSReplicationInProgress,
  ARSReplicated,
  ARSPartiallyReplicated,
  ARSDeltaReplicationInProgress,
  ARSDeltaReplicated,
  ARSDeltaReplicationFailed,
  ARSReplicationFailed,
  ARSReplicationStopping,
  ARSReplicationStopFailed,
  ARSReplicationStopped,
  AppReplicationStatus'
  #-}
