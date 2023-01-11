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
-- Module      : Amazonka.Braket.Types.JobEventType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Braket.Types.JobEventType
  ( JobEventType
      ( ..,
        JobEventType_CANCELLED,
        JobEventType_COMPLETED,
        JobEventType_DEPRIORITIZED_DUE_TO_INACTIVITY,
        JobEventType_DOWNLOADING_DATA,
        JobEventType_FAILED,
        JobEventType_MAX_RUNTIME_EXCEEDED,
        JobEventType_QUEUED_FOR_EXECUTION,
        JobEventType_RUNNING,
        JobEventType_STARTING_INSTANCE,
        JobEventType_UPLOADING_RESULTS,
        JobEventType_WAITING_FOR_PRIORITY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype JobEventType = JobEventType'
  { fromJobEventType ::
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

pattern JobEventType_CANCELLED :: JobEventType
pattern JobEventType_CANCELLED = JobEventType' "CANCELLED"

pattern JobEventType_COMPLETED :: JobEventType
pattern JobEventType_COMPLETED = JobEventType' "COMPLETED"

pattern JobEventType_DEPRIORITIZED_DUE_TO_INACTIVITY :: JobEventType
pattern JobEventType_DEPRIORITIZED_DUE_TO_INACTIVITY = JobEventType' "DEPRIORITIZED_DUE_TO_INACTIVITY"

pattern JobEventType_DOWNLOADING_DATA :: JobEventType
pattern JobEventType_DOWNLOADING_DATA = JobEventType' "DOWNLOADING_DATA"

pattern JobEventType_FAILED :: JobEventType
pattern JobEventType_FAILED = JobEventType' "FAILED"

pattern JobEventType_MAX_RUNTIME_EXCEEDED :: JobEventType
pattern JobEventType_MAX_RUNTIME_EXCEEDED = JobEventType' "MAX_RUNTIME_EXCEEDED"

pattern JobEventType_QUEUED_FOR_EXECUTION :: JobEventType
pattern JobEventType_QUEUED_FOR_EXECUTION = JobEventType' "QUEUED_FOR_EXECUTION"

pattern JobEventType_RUNNING :: JobEventType
pattern JobEventType_RUNNING = JobEventType' "RUNNING"

pattern JobEventType_STARTING_INSTANCE :: JobEventType
pattern JobEventType_STARTING_INSTANCE = JobEventType' "STARTING_INSTANCE"

pattern JobEventType_UPLOADING_RESULTS :: JobEventType
pattern JobEventType_UPLOADING_RESULTS = JobEventType' "UPLOADING_RESULTS"

pattern JobEventType_WAITING_FOR_PRIORITY :: JobEventType
pattern JobEventType_WAITING_FOR_PRIORITY = JobEventType' "WAITING_FOR_PRIORITY"

{-# COMPLETE
  JobEventType_CANCELLED,
  JobEventType_COMPLETED,
  JobEventType_DEPRIORITIZED_DUE_TO_INACTIVITY,
  JobEventType_DOWNLOADING_DATA,
  JobEventType_FAILED,
  JobEventType_MAX_RUNTIME_EXCEEDED,
  JobEventType_QUEUED_FOR_EXECUTION,
  JobEventType_RUNNING,
  JobEventType_STARTING_INSTANCE,
  JobEventType_UPLOADING_RESULTS,
  JobEventType_WAITING_FOR_PRIORITY,
  JobEventType'
  #-}
