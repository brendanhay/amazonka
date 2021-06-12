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
-- Module      : Network.AWS.IoT.Types.JobExecutionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.JobExecutionStatus
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

import qualified Network.AWS.Core as Core

newtype JobExecutionStatus = JobExecutionStatus'
  { fromJobExecutionStatus ::
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
