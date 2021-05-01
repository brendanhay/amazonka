{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype MaintenanceWindowExecutionStatus = MaintenanceWindowExecutionStatus'
  { fromMaintenanceWindowExecutionStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
