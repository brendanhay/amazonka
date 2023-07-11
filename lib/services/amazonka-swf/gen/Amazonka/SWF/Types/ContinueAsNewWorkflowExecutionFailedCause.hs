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
-- Module      : Amazonka.SWF.Types.ContinueAsNewWorkflowExecutionFailedCause
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.ContinueAsNewWorkflowExecutionFailedCause
  ( ContinueAsNewWorkflowExecutionFailedCause
      ( ..,
        ContinueAsNewWorkflowExecutionFailedCause_CONTINUE_AS_NEW_WORKFLOW_EXECUTION_RATE_EXCEEDED,
        ContinueAsNewWorkflowExecutionFailedCause_DEFAULT_CHILD_POLICY_UNDEFINED,
        ContinueAsNewWorkflowExecutionFailedCause_DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED,
        ContinueAsNewWorkflowExecutionFailedCause_DEFAULT_TASK_LIST_UNDEFINED,
        ContinueAsNewWorkflowExecutionFailedCause_DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED,
        ContinueAsNewWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED,
        ContinueAsNewWorkflowExecutionFailedCause_UNHANDLED_DECISION,
        ContinueAsNewWorkflowExecutionFailedCause_WORKFLOW_TYPE_DEPRECATED,
        ContinueAsNewWorkflowExecutionFailedCause_WORKFLOW_TYPE_DOES_NOT_EXIST
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ContinueAsNewWorkflowExecutionFailedCause = ContinueAsNewWorkflowExecutionFailedCause'
  { fromContinueAsNewWorkflowExecutionFailedCause ::
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

pattern ContinueAsNewWorkflowExecutionFailedCause_CONTINUE_AS_NEW_WORKFLOW_EXECUTION_RATE_EXCEEDED :: ContinueAsNewWorkflowExecutionFailedCause
pattern ContinueAsNewWorkflowExecutionFailedCause_CONTINUE_AS_NEW_WORKFLOW_EXECUTION_RATE_EXCEEDED = ContinueAsNewWorkflowExecutionFailedCause' "CONTINUE_AS_NEW_WORKFLOW_EXECUTION_RATE_EXCEEDED"

pattern ContinueAsNewWorkflowExecutionFailedCause_DEFAULT_CHILD_POLICY_UNDEFINED :: ContinueAsNewWorkflowExecutionFailedCause
pattern ContinueAsNewWorkflowExecutionFailedCause_DEFAULT_CHILD_POLICY_UNDEFINED = ContinueAsNewWorkflowExecutionFailedCause' "DEFAULT_CHILD_POLICY_UNDEFINED"

pattern ContinueAsNewWorkflowExecutionFailedCause_DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED :: ContinueAsNewWorkflowExecutionFailedCause
pattern ContinueAsNewWorkflowExecutionFailedCause_DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED = ContinueAsNewWorkflowExecutionFailedCause' "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"

pattern ContinueAsNewWorkflowExecutionFailedCause_DEFAULT_TASK_LIST_UNDEFINED :: ContinueAsNewWorkflowExecutionFailedCause
pattern ContinueAsNewWorkflowExecutionFailedCause_DEFAULT_TASK_LIST_UNDEFINED = ContinueAsNewWorkflowExecutionFailedCause' "DEFAULT_TASK_LIST_UNDEFINED"

pattern ContinueAsNewWorkflowExecutionFailedCause_DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED :: ContinueAsNewWorkflowExecutionFailedCause
pattern ContinueAsNewWorkflowExecutionFailedCause_DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED = ContinueAsNewWorkflowExecutionFailedCause' "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"

pattern ContinueAsNewWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED :: ContinueAsNewWorkflowExecutionFailedCause
pattern ContinueAsNewWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED = ContinueAsNewWorkflowExecutionFailedCause' "OPERATION_NOT_PERMITTED"

pattern ContinueAsNewWorkflowExecutionFailedCause_UNHANDLED_DECISION :: ContinueAsNewWorkflowExecutionFailedCause
pattern ContinueAsNewWorkflowExecutionFailedCause_UNHANDLED_DECISION = ContinueAsNewWorkflowExecutionFailedCause' "UNHANDLED_DECISION"

pattern ContinueAsNewWorkflowExecutionFailedCause_WORKFLOW_TYPE_DEPRECATED :: ContinueAsNewWorkflowExecutionFailedCause
pattern ContinueAsNewWorkflowExecutionFailedCause_WORKFLOW_TYPE_DEPRECATED = ContinueAsNewWorkflowExecutionFailedCause' "WORKFLOW_TYPE_DEPRECATED"

pattern ContinueAsNewWorkflowExecutionFailedCause_WORKFLOW_TYPE_DOES_NOT_EXIST :: ContinueAsNewWorkflowExecutionFailedCause
pattern ContinueAsNewWorkflowExecutionFailedCause_WORKFLOW_TYPE_DOES_NOT_EXIST = ContinueAsNewWorkflowExecutionFailedCause' "WORKFLOW_TYPE_DOES_NOT_EXIST"

{-# COMPLETE
  ContinueAsNewWorkflowExecutionFailedCause_CONTINUE_AS_NEW_WORKFLOW_EXECUTION_RATE_EXCEEDED,
  ContinueAsNewWorkflowExecutionFailedCause_DEFAULT_CHILD_POLICY_UNDEFINED,
  ContinueAsNewWorkflowExecutionFailedCause_DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED,
  ContinueAsNewWorkflowExecutionFailedCause_DEFAULT_TASK_LIST_UNDEFINED,
  ContinueAsNewWorkflowExecutionFailedCause_DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED,
  ContinueAsNewWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED,
  ContinueAsNewWorkflowExecutionFailedCause_UNHANDLED_DECISION,
  ContinueAsNewWorkflowExecutionFailedCause_WORKFLOW_TYPE_DEPRECATED,
  ContinueAsNewWorkflowExecutionFailedCause_WORKFLOW_TYPE_DOES_NOT_EXIST,
  ContinueAsNewWorkflowExecutionFailedCause'
  #-}
