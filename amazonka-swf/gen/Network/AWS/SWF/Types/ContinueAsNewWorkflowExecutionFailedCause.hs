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
-- Module      : Network.AWS.SWF.Types.ContinueAsNewWorkflowExecutionFailedCause
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ContinueAsNewWorkflowExecutionFailedCause
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

import qualified Network.AWS.Core as Core

newtype ContinueAsNewWorkflowExecutionFailedCause = ContinueAsNewWorkflowExecutionFailedCause'
  { fromContinueAsNewWorkflowExecutionFailedCause ::
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
