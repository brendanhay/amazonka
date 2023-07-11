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
-- Module      : Amazonka.Batch.Types.JobStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.JobStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype JobStatus = JobStatus'
  { fromJobStatus ::
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
