{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.AppReplicationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.AppReplicationStatus
  ( AppReplicationStatus
      ( ..,
        AppReplicationStatus_CONFIGURATION_INVALID,
        AppReplicationStatus_CONFIGURATION_IN_PROGRESS,
        AppReplicationStatus_DELTA_REPLICATED,
        AppReplicationStatus_DELTA_REPLICATION_FAILED,
        AppReplicationStatus_DELTA_REPLICATION_IN_PROGRESS,
        AppReplicationStatus_PARTIALLY_REPLICATED,
        AppReplicationStatus_READY_FOR_CONFIGURATION,
        AppReplicationStatus_READY_FOR_REPLICATION,
        AppReplicationStatus_REPLICATED,
        AppReplicationStatus_REPLICATION_FAILED,
        AppReplicationStatus_REPLICATION_IN_PROGRESS,
        AppReplicationStatus_REPLICATION_PENDING,
        AppReplicationStatus_REPLICATION_STOPPED,
        AppReplicationStatus_REPLICATION_STOPPING,
        AppReplicationStatus_REPLICATION_STOP_FAILED,
        AppReplicationStatus_VALIDATION_IN_PROGRESS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AppReplicationStatus = AppReplicationStatus'
  { fromAppReplicationStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern AppReplicationStatus_CONFIGURATION_INVALID :: AppReplicationStatus
pattern AppReplicationStatus_CONFIGURATION_INVALID = AppReplicationStatus' "CONFIGURATION_INVALID"

pattern AppReplicationStatus_CONFIGURATION_IN_PROGRESS :: AppReplicationStatus
pattern AppReplicationStatus_CONFIGURATION_IN_PROGRESS = AppReplicationStatus' "CONFIGURATION_IN_PROGRESS"

pattern AppReplicationStatus_DELTA_REPLICATED :: AppReplicationStatus
pattern AppReplicationStatus_DELTA_REPLICATED = AppReplicationStatus' "DELTA_REPLICATED"

pattern AppReplicationStatus_DELTA_REPLICATION_FAILED :: AppReplicationStatus
pattern AppReplicationStatus_DELTA_REPLICATION_FAILED = AppReplicationStatus' "DELTA_REPLICATION_FAILED"

pattern AppReplicationStatus_DELTA_REPLICATION_IN_PROGRESS :: AppReplicationStatus
pattern AppReplicationStatus_DELTA_REPLICATION_IN_PROGRESS = AppReplicationStatus' "DELTA_REPLICATION_IN_PROGRESS"

pattern AppReplicationStatus_PARTIALLY_REPLICATED :: AppReplicationStatus
pattern AppReplicationStatus_PARTIALLY_REPLICATED = AppReplicationStatus' "PARTIALLY_REPLICATED"

pattern AppReplicationStatus_READY_FOR_CONFIGURATION :: AppReplicationStatus
pattern AppReplicationStatus_READY_FOR_CONFIGURATION = AppReplicationStatus' "READY_FOR_CONFIGURATION"

pattern AppReplicationStatus_READY_FOR_REPLICATION :: AppReplicationStatus
pattern AppReplicationStatus_READY_FOR_REPLICATION = AppReplicationStatus' "READY_FOR_REPLICATION"

pattern AppReplicationStatus_REPLICATED :: AppReplicationStatus
pattern AppReplicationStatus_REPLICATED = AppReplicationStatus' "REPLICATED"

pattern AppReplicationStatus_REPLICATION_FAILED :: AppReplicationStatus
pattern AppReplicationStatus_REPLICATION_FAILED = AppReplicationStatus' "REPLICATION_FAILED"

pattern AppReplicationStatus_REPLICATION_IN_PROGRESS :: AppReplicationStatus
pattern AppReplicationStatus_REPLICATION_IN_PROGRESS = AppReplicationStatus' "REPLICATION_IN_PROGRESS"

pattern AppReplicationStatus_REPLICATION_PENDING :: AppReplicationStatus
pattern AppReplicationStatus_REPLICATION_PENDING = AppReplicationStatus' "REPLICATION_PENDING"

pattern AppReplicationStatus_REPLICATION_STOPPED :: AppReplicationStatus
pattern AppReplicationStatus_REPLICATION_STOPPED = AppReplicationStatus' "REPLICATION_STOPPED"

pattern AppReplicationStatus_REPLICATION_STOPPING :: AppReplicationStatus
pattern AppReplicationStatus_REPLICATION_STOPPING = AppReplicationStatus' "REPLICATION_STOPPING"

pattern AppReplicationStatus_REPLICATION_STOP_FAILED :: AppReplicationStatus
pattern AppReplicationStatus_REPLICATION_STOP_FAILED = AppReplicationStatus' "REPLICATION_STOP_FAILED"

pattern AppReplicationStatus_VALIDATION_IN_PROGRESS :: AppReplicationStatus
pattern AppReplicationStatus_VALIDATION_IN_PROGRESS = AppReplicationStatus' "VALIDATION_IN_PROGRESS"

{-# COMPLETE
  AppReplicationStatus_CONFIGURATION_INVALID,
  AppReplicationStatus_CONFIGURATION_IN_PROGRESS,
  AppReplicationStatus_DELTA_REPLICATED,
  AppReplicationStatus_DELTA_REPLICATION_FAILED,
  AppReplicationStatus_DELTA_REPLICATION_IN_PROGRESS,
  AppReplicationStatus_PARTIALLY_REPLICATED,
  AppReplicationStatus_READY_FOR_CONFIGURATION,
  AppReplicationStatus_READY_FOR_REPLICATION,
  AppReplicationStatus_REPLICATED,
  AppReplicationStatus_REPLICATION_FAILED,
  AppReplicationStatus_REPLICATION_IN_PROGRESS,
  AppReplicationStatus_REPLICATION_PENDING,
  AppReplicationStatus_REPLICATION_STOPPED,
  AppReplicationStatus_REPLICATION_STOPPING,
  AppReplicationStatus_REPLICATION_STOP_FAILED,
  AppReplicationStatus_VALIDATION_IN_PROGRESS,
  AppReplicationStatus'
  #-}
