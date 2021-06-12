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
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowExecutionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowExecutionStatus
  ( MaintenanceWindowExecutionStatus
      ( ..,
        MaintenanceWindowExecutionStatus_CANCELLED,
        MaintenanceWindowExecutionStatus_CANCELLING,
        MaintenanceWindowExecutionStatus_FAILED,
        MaintenanceWindowExecutionStatus_IN_PROGRESS,
        MaintenanceWindowExecutionStatus_PENDING,
        MaintenanceWindowExecutionStatus_SKIPPED_OVERLAPPING,
        MaintenanceWindowExecutionStatus_SUCCESS,
        MaintenanceWindowExecutionStatus_TIMED_OUT
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype MaintenanceWindowExecutionStatus = MaintenanceWindowExecutionStatus'
  { fromMaintenanceWindowExecutionStatus ::
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

pattern MaintenanceWindowExecutionStatus_CANCELLED :: MaintenanceWindowExecutionStatus
pattern MaintenanceWindowExecutionStatus_CANCELLED = MaintenanceWindowExecutionStatus' "CANCELLED"

pattern MaintenanceWindowExecutionStatus_CANCELLING :: MaintenanceWindowExecutionStatus
pattern MaintenanceWindowExecutionStatus_CANCELLING = MaintenanceWindowExecutionStatus' "CANCELLING"

pattern MaintenanceWindowExecutionStatus_FAILED :: MaintenanceWindowExecutionStatus
pattern MaintenanceWindowExecutionStatus_FAILED = MaintenanceWindowExecutionStatus' "FAILED"

pattern MaintenanceWindowExecutionStatus_IN_PROGRESS :: MaintenanceWindowExecutionStatus
pattern MaintenanceWindowExecutionStatus_IN_PROGRESS = MaintenanceWindowExecutionStatus' "IN_PROGRESS"

pattern MaintenanceWindowExecutionStatus_PENDING :: MaintenanceWindowExecutionStatus
pattern MaintenanceWindowExecutionStatus_PENDING = MaintenanceWindowExecutionStatus' "PENDING"

pattern MaintenanceWindowExecutionStatus_SKIPPED_OVERLAPPING :: MaintenanceWindowExecutionStatus
pattern MaintenanceWindowExecutionStatus_SKIPPED_OVERLAPPING = MaintenanceWindowExecutionStatus' "SKIPPED_OVERLAPPING"

pattern MaintenanceWindowExecutionStatus_SUCCESS :: MaintenanceWindowExecutionStatus
pattern MaintenanceWindowExecutionStatus_SUCCESS = MaintenanceWindowExecutionStatus' "SUCCESS"

pattern MaintenanceWindowExecutionStatus_TIMED_OUT :: MaintenanceWindowExecutionStatus
pattern MaintenanceWindowExecutionStatus_TIMED_OUT = MaintenanceWindowExecutionStatus' "TIMED_OUT"

{-# COMPLETE
  MaintenanceWindowExecutionStatus_CANCELLED,
  MaintenanceWindowExecutionStatus_CANCELLING,
  MaintenanceWindowExecutionStatus_FAILED,
  MaintenanceWindowExecutionStatus_IN_PROGRESS,
  MaintenanceWindowExecutionStatus_PENDING,
  MaintenanceWindowExecutionStatus_SKIPPED_OVERLAPPING,
  MaintenanceWindowExecutionStatus_SUCCESS,
  MaintenanceWindowExecutionStatus_TIMED_OUT,
  MaintenanceWindowExecutionStatus'
  #-}
