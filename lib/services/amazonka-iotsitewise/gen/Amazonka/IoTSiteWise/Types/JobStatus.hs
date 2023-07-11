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
-- Module      : Amazonka.IoTSiteWise.Types.JobStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.JobStatus
  ( JobStatus
      ( ..,
        JobStatus_CANCELLED,
        JobStatus_COMPLETED,
        JobStatus_COMPLETED_WITH_FAILURES,
        JobStatus_FAILED,
        JobStatus_PENDING,
        JobStatus_RUNNING
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

pattern JobStatus_CANCELLED :: JobStatus
pattern JobStatus_CANCELLED = JobStatus' "CANCELLED"

pattern JobStatus_COMPLETED :: JobStatus
pattern JobStatus_COMPLETED = JobStatus' "COMPLETED"

pattern JobStatus_COMPLETED_WITH_FAILURES :: JobStatus
pattern JobStatus_COMPLETED_WITH_FAILURES = JobStatus' "COMPLETED_WITH_FAILURES"

pattern JobStatus_FAILED :: JobStatus
pattern JobStatus_FAILED = JobStatus' "FAILED"

pattern JobStatus_PENDING :: JobStatus
pattern JobStatus_PENDING = JobStatus' "PENDING"

pattern JobStatus_RUNNING :: JobStatus
pattern JobStatus_RUNNING = JobStatus' "RUNNING"

{-# COMPLETE
  JobStatus_CANCELLED,
  JobStatus_COMPLETED,
  JobStatus_COMPLETED_WITH_FAILURES,
  JobStatus_FAILED,
  JobStatus_PENDING,
  JobStatus_RUNNING,
  JobStatus'
  #-}
