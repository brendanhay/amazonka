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
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionTerminatedCause
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionTerminatedCause
  ( WorkflowExecutionTerminatedCause
      ( ..,
        WorkflowExecutionTerminatedCause_CHILD_POLICY_APPLIED,
        WorkflowExecutionTerminatedCause_EVENT_LIMIT_EXCEEDED,
        WorkflowExecutionTerminatedCause_OPERATOR_INITIATED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype WorkflowExecutionTerminatedCause = WorkflowExecutionTerminatedCause'
  { fromWorkflowExecutionTerminatedCause ::
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

pattern WorkflowExecutionTerminatedCause_CHILD_POLICY_APPLIED :: WorkflowExecutionTerminatedCause
pattern WorkflowExecutionTerminatedCause_CHILD_POLICY_APPLIED = WorkflowExecutionTerminatedCause' "CHILD_POLICY_APPLIED"

pattern WorkflowExecutionTerminatedCause_EVENT_LIMIT_EXCEEDED :: WorkflowExecutionTerminatedCause
pattern WorkflowExecutionTerminatedCause_EVENT_LIMIT_EXCEEDED = WorkflowExecutionTerminatedCause' "EVENT_LIMIT_EXCEEDED"

pattern WorkflowExecutionTerminatedCause_OPERATOR_INITIATED :: WorkflowExecutionTerminatedCause
pattern WorkflowExecutionTerminatedCause_OPERATOR_INITIATED = WorkflowExecutionTerminatedCause' "OPERATOR_INITIATED"

{-# COMPLETE
  WorkflowExecutionTerminatedCause_CHILD_POLICY_APPLIED,
  WorkflowExecutionTerminatedCause_EVENT_LIMIT_EXCEEDED,
  WorkflowExecutionTerminatedCause_OPERATOR_INITIATED,
  WorkflowExecutionTerminatedCause'
  #-}
