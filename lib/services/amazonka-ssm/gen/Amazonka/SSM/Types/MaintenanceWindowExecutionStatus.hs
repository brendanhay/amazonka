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
-- Module      : Amazonka.SSM.Types.MaintenanceWindowExecutionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.MaintenanceWindowExecutionStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MaintenanceWindowExecutionStatus = MaintenanceWindowExecutionStatus'
  { fromMaintenanceWindowExecutionStatus ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
