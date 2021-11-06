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
-- Module      : Amazonka.IoTThingsGraph.Types.FlowExecutionEventType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTThingsGraph.Types.FlowExecutionEventType
  ( FlowExecutionEventType
      ( ..,
        FlowExecutionEventType_ACKNOWLEDGE_TASK_MESSAGE,
        FlowExecutionEventType_ACTIVITY_FAILED,
        FlowExecutionEventType_ACTIVITY_SCHEDULED,
        FlowExecutionEventType_ACTIVITY_STARTED,
        FlowExecutionEventType_ACTIVITY_SUCCEEDED,
        FlowExecutionEventType_EXECUTION_ABORTED,
        FlowExecutionEventType_EXECUTION_FAILED,
        FlowExecutionEventType_EXECUTION_STARTED,
        FlowExecutionEventType_EXECUTION_SUCCEEDED,
        FlowExecutionEventType_SCHEDULE_NEXT_READY_STEPS_TASK,
        FlowExecutionEventType_START_FLOW_EXECUTION_TASK,
        FlowExecutionEventType_STEP_FAILED,
        FlowExecutionEventType_STEP_STARTED,
        FlowExecutionEventType_STEP_SUCCEEDED,
        FlowExecutionEventType_THING_ACTION_TASK,
        FlowExecutionEventType_THING_ACTION_TASK_FAILED,
        FlowExecutionEventType_THING_ACTION_TASK_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype FlowExecutionEventType = FlowExecutionEventType'
  { fromFlowExecutionEventType ::
      Core.Text
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

pattern FlowExecutionEventType_ACKNOWLEDGE_TASK_MESSAGE :: FlowExecutionEventType
pattern FlowExecutionEventType_ACKNOWLEDGE_TASK_MESSAGE = FlowExecutionEventType' "ACKNOWLEDGE_TASK_MESSAGE"

pattern FlowExecutionEventType_ACTIVITY_FAILED :: FlowExecutionEventType
pattern FlowExecutionEventType_ACTIVITY_FAILED = FlowExecutionEventType' "ACTIVITY_FAILED"

pattern FlowExecutionEventType_ACTIVITY_SCHEDULED :: FlowExecutionEventType
pattern FlowExecutionEventType_ACTIVITY_SCHEDULED = FlowExecutionEventType' "ACTIVITY_SCHEDULED"

pattern FlowExecutionEventType_ACTIVITY_STARTED :: FlowExecutionEventType
pattern FlowExecutionEventType_ACTIVITY_STARTED = FlowExecutionEventType' "ACTIVITY_STARTED"

pattern FlowExecutionEventType_ACTIVITY_SUCCEEDED :: FlowExecutionEventType
pattern FlowExecutionEventType_ACTIVITY_SUCCEEDED = FlowExecutionEventType' "ACTIVITY_SUCCEEDED"

pattern FlowExecutionEventType_EXECUTION_ABORTED :: FlowExecutionEventType
pattern FlowExecutionEventType_EXECUTION_ABORTED = FlowExecutionEventType' "EXECUTION_ABORTED"

pattern FlowExecutionEventType_EXECUTION_FAILED :: FlowExecutionEventType
pattern FlowExecutionEventType_EXECUTION_FAILED = FlowExecutionEventType' "EXECUTION_FAILED"

pattern FlowExecutionEventType_EXECUTION_STARTED :: FlowExecutionEventType
pattern FlowExecutionEventType_EXECUTION_STARTED = FlowExecutionEventType' "EXECUTION_STARTED"

pattern FlowExecutionEventType_EXECUTION_SUCCEEDED :: FlowExecutionEventType
pattern FlowExecutionEventType_EXECUTION_SUCCEEDED = FlowExecutionEventType' "EXECUTION_SUCCEEDED"

pattern FlowExecutionEventType_SCHEDULE_NEXT_READY_STEPS_TASK :: FlowExecutionEventType
pattern FlowExecutionEventType_SCHEDULE_NEXT_READY_STEPS_TASK = FlowExecutionEventType' "SCHEDULE_NEXT_READY_STEPS_TASK"

pattern FlowExecutionEventType_START_FLOW_EXECUTION_TASK :: FlowExecutionEventType
pattern FlowExecutionEventType_START_FLOW_EXECUTION_TASK = FlowExecutionEventType' "START_FLOW_EXECUTION_TASK"

pattern FlowExecutionEventType_STEP_FAILED :: FlowExecutionEventType
pattern FlowExecutionEventType_STEP_FAILED = FlowExecutionEventType' "STEP_FAILED"

pattern FlowExecutionEventType_STEP_STARTED :: FlowExecutionEventType
pattern FlowExecutionEventType_STEP_STARTED = FlowExecutionEventType' "STEP_STARTED"

pattern FlowExecutionEventType_STEP_SUCCEEDED :: FlowExecutionEventType
pattern FlowExecutionEventType_STEP_SUCCEEDED = FlowExecutionEventType' "STEP_SUCCEEDED"

pattern FlowExecutionEventType_THING_ACTION_TASK :: FlowExecutionEventType
pattern FlowExecutionEventType_THING_ACTION_TASK = FlowExecutionEventType' "THING_ACTION_TASK"

pattern FlowExecutionEventType_THING_ACTION_TASK_FAILED :: FlowExecutionEventType
pattern FlowExecutionEventType_THING_ACTION_TASK_FAILED = FlowExecutionEventType' "THING_ACTION_TASK_FAILED"

pattern FlowExecutionEventType_THING_ACTION_TASK_SUCCEEDED :: FlowExecutionEventType
pattern FlowExecutionEventType_THING_ACTION_TASK_SUCCEEDED = FlowExecutionEventType' "THING_ACTION_TASK_SUCCEEDED"

{-# COMPLETE
  FlowExecutionEventType_ACKNOWLEDGE_TASK_MESSAGE,
  FlowExecutionEventType_ACTIVITY_FAILED,
  FlowExecutionEventType_ACTIVITY_SCHEDULED,
  FlowExecutionEventType_ACTIVITY_STARTED,
  FlowExecutionEventType_ACTIVITY_SUCCEEDED,
  FlowExecutionEventType_EXECUTION_ABORTED,
  FlowExecutionEventType_EXECUTION_FAILED,
  FlowExecutionEventType_EXECUTION_STARTED,
  FlowExecutionEventType_EXECUTION_SUCCEEDED,
  FlowExecutionEventType_SCHEDULE_NEXT_READY_STEPS_TASK,
  FlowExecutionEventType_START_FLOW_EXECUTION_TASK,
  FlowExecutionEventType_STEP_FAILED,
  FlowExecutionEventType_STEP_STARTED,
  FlowExecutionEventType_STEP_SUCCEEDED,
  FlowExecutionEventType_THING_ACTION_TASK,
  FlowExecutionEventType_THING_ACTION_TASK_FAILED,
  FlowExecutionEventType_THING_ACTION_TASK_SUCCEEDED,
  FlowExecutionEventType'
  #-}
