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
-- Module      : Amazonka.SWF.Types.StartChildWorkflowExecutionFailedCause
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.StartChildWorkflowExecutionFailedCause
  ( StartChildWorkflowExecutionFailedCause
      ( ..,
        StartChildWorkflowExecutionFailedCause_CHILD_CREATION_RATE_EXCEEDED,
        StartChildWorkflowExecutionFailedCause_DEFAULT_CHILD_POLICY_UNDEFINED,
        StartChildWorkflowExecutionFailedCause_DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED,
        StartChildWorkflowExecutionFailedCause_DEFAULT_TASK_LIST_UNDEFINED,
        StartChildWorkflowExecutionFailedCause_DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED,
        StartChildWorkflowExecutionFailedCause_OPEN_CHILDREN_LIMIT_EXCEEDED,
        StartChildWorkflowExecutionFailedCause_OPEN_WORKFLOWS_LIMIT_EXCEEDED,
        StartChildWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED,
        StartChildWorkflowExecutionFailedCause_WORKFLOW_ALREADY_RUNNING,
        StartChildWorkflowExecutionFailedCause_WORKFLOW_TYPE_DEPRECATED,
        StartChildWorkflowExecutionFailedCause_WORKFLOW_TYPE_DOES_NOT_EXIST
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StartChildWorkflowExecutionFailedCause = StartChildWorkflowExecutionFailedCause'
  { fromStartChildWorkflowExecutionFailedCause ::
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

pattern StartChildWorkflowExecutionFailedCause_CHILD_CREATION_RATE_EXCEEDED :: StartChildWorkflowExecutionFailedCause
pattern StartChildWorkflowExecutionFailedCause_CHILD_CREATION_RATE_EXCEEDED = StartChildWorkflowExecutionFailedCause' "CHILD_CREATION_RATE_EXCEEDED"

pattern StartChildWorkflowExecutionFailedCause_DEFAULT_CHILD_POLICY_UNDEFINED :: StartChildWorkflowExecutionFailedCause
pattern StartChildWorkflowExecutionFailedCause_DEFAULT_CHILD_POLICY_UNDEFINED = StartChildWorkflowExecutionFailedCause' "DEFAULT_CHILD_POLICY_UNDEFINED"

pattern StartChildWorkflowExecutionFailedCause_DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED :: StartChildWorkflowExecutionFailedCause
pattern StartChildWorkflowExecutionFailedCause_DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED = StartChildWorkflowExecutionFailedCause' "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"

pattern StartChildWorkflowExecutionFailedCause_DEFAULT_TASK_LIST_UNDEFINED :: StartChildWorkflowExecutionFailedCause
pattern StartChildWorkflowExecutionFailedCause_DEFAULT_TASK_LIST_UNDEFINED = StartChildWorkflowExecutionFailedCause' "DEFAULT_TASK_LIST_UNDEFINED"

pattern StartChildWorkflowExecutionFailedCause_DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED :: StartChildWorkflowExecutionFailedCause
pattern StartChildWorkflowExecutionFailedCause_DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED = StartChildWorkflowExecutionFailedCause' "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"

pattern StartChildWorkflowExecutionFailedCause_OPEN_CHILDREN_LIMIT_EXCEEDED :: StartChildWorkflowExecutionFailedCause
pattern StartChildWorkflowExecutionFailedCause_OPEN_CHILDREN_LIMIT_EXCEEDED = StartChildWorkflowExecutionFailedCause' "OPEN_CHILDREN_LIMIT_EXCEEDED"

pattern StartChildWorkflowExecutionFailedCause_OPEN_WORKFLOWS_LIMIT_EXCEEDED :: StartChildWorkflowExecutionFailedCause
pattern StartChildWorkflowExecutionFailedCause_OPEN_WORKFLOWS_LIMIT_EXCEEDED = StartChildWorkflowExecutionFailedCause' "OPEN_WORKFLOWS_LIMIT_EXCEEDED"

pattern StartChildWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED :: StartChildWorkflowExecutionFailedCause
pattern StartChildWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED = StartChildWorkflowExecutionFailedCause' "OPERATION_NOT_PERMITTED"

pattern StartChildWorkflowExecutionFailedCause_WORKFLOW_ALREADY_RUNNING :: StartChildWorkflowExecutionFailedCause
pattern StartChildWorkflowExecutionFailedCause_WORKFLOW_ALREADY_RUNNING = StartChildWorkflowExecutionFailedCause' "WORKFLOW_ALREADY_RUNNING"

pattern StartChildWorkflowExecutionFailedCause_WORKFLOW_TYPE_DEPRECATED :: StartChildWorkflowExecutionFailedCause
pattern StartChildWorkflowExecutionFailedCause_WORKFLOW_TYPE_DEPRECATED = StartChildWorkflowExecutionFailedCause' "WORKFLOW_TYPE_DEPRECATED"

pattern StartChildWorkflowExecutionFailedCause_WORKFLOW_TYPE_DOES_NOT_EXIST :: StartChildWorkflowExecutionFailedCause
pattern StartChildWorkflowExecutionFailedCause_WORKFLOW_TYPE_DOES_NOT_EXIST = StartChildWorkflowExecutionFailedCause' "WORKFLOW_TYPE_DOES_NOT_EXIST"

{-# COMPLETE
  StartChildWorkflowExecutionFailedCause_CHILD_CREATION_RATE_EXCEEDED,
  StartChildWorkflowExecutionFailedCause_DEFAULT_CHILD_POLICY_UNDEFINED,
  StartChildWorkflowExecutionFailedCause_DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED,
  StartChildWorkflowExecutionFailedCause_DEFAULT_TASK_LIST_UNDEFINED,
  StartChildWorkflowExecutionFailedCause_DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED,
  StartChildWorkflowExecutionFailedCause_OPEN_CHILDREN_LIMIT_EXCEEDED,
  StartChildWorkflowExecutionFailedCause_OPEN_WORKFLOWS_LIMIT_EXCEEDED,
  StartChildWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED,
  StartChildWorkflowExecutionFailedCause_WORKFLOW_ALREADY_RUNNING,
  StartChildWorkflowExecutionFailedCause_WORKFLOW_TYPE_DEPRECATED,
  StartChildWorkflowExecutionFailedCause_WORKFLOW_TYPE_DOES_NOT_EXIST,
  StartChildWorkflowExecutionFailedCause'
  #-}
