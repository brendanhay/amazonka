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
-- Module      : Amazonka.DrS.Types.JobLogEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.JobLogEvent
  ( JobLogEvent
      ( ..,
        JobLogEvent_CLEANUP_END,
        JobLogEvent_CLEANUP_FAIL,
        JobLogEvent_CLEANUP_START,
        JobLogEvent_CONVERSION_END,
        JobLogEvent_CONVERSION_FAIL,
        JobLogEvent_CONVERSION_START,
        JobLogEvent_JOB_CANCEL,
        JobLogEvent_JOB_END,
        JobLogEvent_JOB_START,
        JobLogEvent_LAUNCH_FAILED,
        JobLogEvent_LAUNCH_START,
        JobLogEvent_SERVER_SKIPPED,
        JobLogEvent_SNAPSHOT_END,
        JobLogEvent_SNAPSHOT_FAIL,
        JobLogEvent_SNAPSHOT_START,
        JobLogEvent_USING_PREVIOUS_SNAPSHOT,
        JobLogEvent_USING_PREVIOUS_SNAPSHOT_FAILED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype JobLogEvent = JobLogEvent'
  { fromJobLogEvent ::
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

pattern JobLogEvent_CLEANUP_END :: JobLogEvent
pattern JobLogEvent_CLEANUP_END = JobLogEvent' "CLEANUP_END"

pattern JobLogEvent_CLEANUP_FAIL :: JobLogEvent
pattern JobLogEvent_CLEANUP_FAIL = JobLogEvent' "CLEANUP_FAIL"

pattern JobLogEvent_CLEANUP_START :: JobLogEvent
pattern JobLogEvent_CLEANUP_START = JobLogEvent' "CLEANUP_START"

pattern JobLogEvent_CONVERSION_END :: JobLogEvent
pattern JobLogEvent_CONVERSION_END = JobLogEvent' "CONVERSION_END"

pattern JobLogEvent_CONVERSION_FAIL :: JobLogEvent
pattern JobLogEvent_CONVERSION_FAIL = JobLogEvent' "CONVERSION_FAIL"

pattern JobLogEvent_CONVERSION_START :: JobLogEvent
pattern JobLogEvent_CONVERSION_START = JobLogEvent' "CONVERSION_START"

pattern JobLogEvent_JOB_CANCEL :: JobLogEvent
pattern JobLogEvent_JOB_CANCEL = JobLogEvent' "JOB_CANCEL"

pattern JobLogEvent_JOB_END :: JobLogEvent
pattern JobLogEvent_JOB_END = JobLogEvent' "JOB_END"

pattern JobLogEvent_JOB_START :: JobLogEvent
pattern JobLogEvent_JOB_START = JobLogEvent' "JOB_START"

pattern JobLogEvent_LAUNCH_FAILED :: JobLogEvent
pattern JobLogEvent_LAUNCH_FAILED = JobLogEvent' "LAUNCH_FAILED"

pattern JobLogEvent_LAUNCH_START :: JobLogEvent
pattern JobLogEvent_LAUNCH_START = JobLogEvent' "LAUNCH_START"

pattern JobLogEvent_SERVER_SKIPPED :: JobLogEvent
pattern JobLogEvent_SERVER_SKIPPED = JobLogEvent' "SERVER_SKIPPED"

pattern JobLogEvent_SNAPSHOT_END :: JobLogEvent
pattern JobLogEvent_SNAPSHOT_END = JobLogEvent' "SNAPSHOT_END"

pattern JobLogEvent_SNAPSHOT_FAIL :: JobLogEvent
pattern JobLogEvent_SNAPSHOT_FAIL = JobLogEvent' "SNAPSHOT_FAIL"

pattern JobLogEvent_SNAPSHOT_START :: JobLogEvent
pattern JobLogEvent_SNAPSHOT_START = JobLogEvent' "SNAPSHOT_START"

pattern JobLogEvent_USING_PREVIOUS_SNAPSHOT :: JobLogEvent
pattern JobLogEvent_USING_PREVIOUS_SNAPSHOT = JobLogEvent' "USING_PREVIOUS_SNAPSHOT"

pattern JobLogEvent_USING_PREVIOUS_SNAPSHOT_FAILED :: JobLogEvent
pattern JobLogEvent_USING_PREVIOUS_SNAPSHOT_FAILED = JobLogEvent' "USING_PREVIOUS_SNAPSHOT_FAILED"

{-# COMPLETE
  JobLogEvent_CLEANUP_END,
  JobLogEvent_CLEANUP_FAIL,
  JobLogEvent_CLEANUP_START,
  JobLogEvent_CONVERSION_END,
  JobLogEvent_CONVERSION_FAIL,
  JobLogEvent_CONVERSION_START,
  JobLogEvent_JOB_CANCEL,
  JobLogEvent_JOB_END,
  JobLogEvent_JOB_START,
  JobLogEvent_LAUNCH_FAILED,
  JobLogEvent_LAUNCH_START,
  JobLogEvent_SERVER_SKIPPED,
  JobLogEvent_SNAPSHOT_END,
  JobLogEvent_SNAPSHOT_FAIL,
  JobLogEvent_SNAPSHOT_START,
  JobLogEvent_USING_PREVIOUS_SNAPSHOT,
  JobLogEvent_USING_PREVIOUS_SNAPSHOT_FAILED,
  JobLogEvent'
  #-}
