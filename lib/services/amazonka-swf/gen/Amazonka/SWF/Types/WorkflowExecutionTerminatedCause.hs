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
-- Module      : Amazonka.SWF.Types.WorkflowExecutionTerminatedCause
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.WorkflowExecutionTerminatedCause
  ( WorkflowExecutionTerminatedCause
      ( ..,
        WorkflowExecutionTerminatedCause_CHILD_POLICY_APPLIED,
        WorkflowExecutionTerminatedCause_EVENT_LIMIT_EXCEEDED,
        WorkflowExecutionTerminatedCause_OPERATOR_INITIATED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WorkflowExecutionTerminatedCause = WorkflowExecutionTerminatedCause'
  { fromWorkflowExecutionTerminatedCause ::
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
