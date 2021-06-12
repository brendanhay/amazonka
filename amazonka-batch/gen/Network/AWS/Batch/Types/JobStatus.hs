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
-- Module      : Network.AWS.Batch.Types.JobStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JobStatus
  ( JobStatus
      ( ..,
        JobStatus_FAILED,
        JobStatus_PENDING,
        JobStatus_RUNNABLE,
        JobStatus_RUNNING,
        JobStatus_STARTING,
        JobStatus_SUBMITTED,
        JobStatus_SUCCEEDED
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype JobStatus = JobStatus'
  { fromJobStatus ::
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

pattern JobStatus_FAILED :: JobStatus
pattern JobStatus_FAILED = JobStatus' "FAILED"

pattern JobStatus_PENDING :: JobStatus
pattern JobStatus_PENDING = JobStatus' "PENDING"

pattern JobStatus_RUNNABLE :: JobStatus
pattern JobStatus_RUNNABLE = JobStatus' "RUNNABLE"

pattern JobStatus_RUNNING :: JobStatus
pattern JobStatus_RUNNING = JobStatus' "RUNNING"

pattern JobStatus_STARTING :: JobStatus
pattern JobStatus_STARTING = JobStatus' "STARTING"

pattern JobStatus_SUBMITTED :: JobStatus
pattern JobStatus_SUBMITTED = JobStatus' "SUBMITTED"

pattern JobStatus_SUCCEEDED :: JobStatus
pattern JobStatus_SUCCEEDED = JobStatus' "SUCCEEDED"

{-# COMPLETE
  JobStatus_FAILED,
  JobStatus_PENDING,
  JobStatus_RUNNABLE,
  JobStatus_RUNNING,
  JobStatus_STARTING,
  JobStatus_SUBMITTED,
  JobStatus_SUCCEEDED,
  JobStatus'
  #-}
