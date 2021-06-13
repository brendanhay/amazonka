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
-- Module      : Network.AWS.SWF.Types.SignalExternalWorkflowExecutionFailedCause
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.SignalExternalWorkflowExecutionFailedCause
  ( SignalExternalWorkflowExecutionFailedCause
      ( ..,
        SignalExternalWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED,
        SignalExternalWorkflowExecutionFailedCause_SIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED,
        SignalExternalWorkflowExecutionFailedCause_UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype SignalExternalWorkflowExecutionFailedCause = SignalExternalWorkflowExecutionFailedCause'
  { fromSignalExternalWorkflowExecutionFailedCause ::
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

pattern SignalExternalWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED :: SignalExternalWorkflowExecutionFailedCause
pattern SignalExternalWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED = SignalExternalWorkflowExecutionFailedCause' "OPERATION_NOT_PERMITTED"

pattern SignalExternalWorkflowExecutionFailedCause_SIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED :: SignalExternalWorkflowExecutionFailedCause
pattern SignalExternalWorkflowExecutionFailedCause_SIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED = SignalExternalWorkflowExecutionFailedCause' "SIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED"

pattern SignalExternalWorkflowExecutionFailedCause_UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION :: SignalExternalWorkflowExecutionFailedCause
pattern SignalExternalWorkflowExecutionFailedCause_UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION = SignalExternalWorkflowExecutionFailedCause' "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION"

{-# COMPLETE
  SignalExternalWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED,
  SignalExternalWorkflowExecutionFailedCause_SIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED,
  SignalExternalWorkflowExecutionFailedCause_UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION,
  SignalExternalWorkflowExecutionFailedCause'
  #-}
