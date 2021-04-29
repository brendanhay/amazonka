{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionFailedCause
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionFailedCause
  ( RequestCancelExternalWorkflowExecutionFailedCause
      ( ..,
        RequestCancelExternalWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED,
        RequestCancelExternalWorkflowExecutionFailedCause_REQUEST_CANCEL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED,
        RequestCancelExternalWorkflowExecutionFailedCause_UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype RequestCancelExternalWorkflowExecutionFailedCause = RequestCancelExternalWorkflowExecutionFailedCause'
  { fromRequestCancelExternalWorkflowExecutionFailedCause ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern RequestCancelExternalWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED :: RequestCancelExternalWorkflowExecutionFailedCause
pattern RequestCancelExternalWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED = RequestCancelExternalWorkflowExecutionFailedCause' "OPERATION_NOT_PERMITTED"

pattern RequestCancelExternalWorkflowExecutionFailedCause_REQUEST_CANCEL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED :: RequestCancelExternalWorkflowExecutionFailedCause
pattern RequestCancelExternalWorkflowExecutionFailedCause_REQUEST_CANCEL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED = RequestCancelExternalWorkflowExecutionFailedCause' "REQUEST_CANCEL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED"

pattern RequestCancelExternalWorkflowExecutionFailedCause_UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION :: RequestCancelExternalWorkflowExecutionFailedCause
pattern RequestCancelExternalWorkflowExecutionFailedCause_UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION = RequestCancelExternalWorkflowExecutionFailedCause' "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION"

{-# COMPLETE
  RequestCancelExternalWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED,
  RequestCancelExternalWorkflowExecutionFailedCause_REQUEST_CANCEL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED,
  RequestCancelExternalWorkflowExecutionFailedCause_UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION,
  RequestCancelExternalWorkflowExecutionFailedCause'
  #-}
