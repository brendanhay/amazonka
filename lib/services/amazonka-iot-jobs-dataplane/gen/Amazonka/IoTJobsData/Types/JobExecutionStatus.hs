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
-- Module      : Amazonka.IoTJobsData.Types.JobExecutionStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTJobsData.Types.JobExecutionStatus
  ( JobExecutionStatus
      ( ..,
        JobExecutionStatus_CANCELED,
        JobExecutionStatus_FAILED,
        JobExecutionStatus_IN_PROGRESS,
        JobExecutionStatus_QUEUED,
        JobExecutionStatus_REJECTED,
        JobExecutionStatus_REMOVED,
        JobExecutionStatus_SUCCEEDED,
        JobExecutionStatus_TIMED_OUT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype JobExecutionStatus = JobExecutionStatus'
  { fromJobExecutionStatus ::
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

pattern JobExecutionStatus_CANCELED :: JobExecutionStatus
pattern JobExecutionStatus_CANCELED = JobExecutionStatus' "CANCELED"

pattern JobExecutionStatus_FAILED :: JobExecutionStatus
pattern JobExecutionStatus_FAILED = JobExecutionStatus' "FAILED"

pattern JobExecutionStatus_IN_PROGRESS :: JobExecutionStatus
pattern JobExecutionStatus_IN_PROGRESS = JobExecutionStatus' "IN_PROGRESS"

pattern JobExecutionStatus_QUEUED :: JobExecutionStatus
pattern JobExecutionStatus_QUEUED = JobExecutionStatus' "QUEUED"

pattern JobExecutionStatus_REJECTED :: JobExecutionStatus
pattern JobExecutionStatus_REJECTED = JobExecutionStatus' "REJECTED"

pattern JobExecutionStatus_REMOVED :: JobExecutionStatus
pattern JobExecutionStatus_REMOVED = JobExecutionStatus' "REMOVED"

pattern JobExecutionStatus_SUCCEEDED :: JobExecutionStatus
pattern JobExecutionStatus_SUCCEEDED = JobExecutionStatus' "SUCCEEDED"

pattern JobExecutionStatus_TIMED_OUT :: JobExecutionStatus
pattern JobExecutionStatus_TIMED_OUT = JobExecutionStatus' "TIMED_OUT"

{-# COMPLETE
  JobExecutionStatus_CANCELED,
  JobExecutionStatus_FAILED,
  JobExecutionStatus_IN_PROGRESS,
  JobExecutionStatus_QUEUED,
  JobExecutionStatus_REJECTED,
  JobExecutionStatus_REMOVED,
  JobExecutionStatus_SUCCEEDED,
  JobExecutionStatus_TIMED_OUT,
  JobExecutionStatus'
  #-}
