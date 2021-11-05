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
-- Module      : Amazonka.SWF.Types.WorkflowExecutionCancelRequestedCause
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.WorkflowExecutionCancelRequestedCause
  ( WorkflowExecutionCancelRequestedCause
      ( ..,
        WorkflowExecutionCancelRequestedCause_CHILD_POLICY_APPLIED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype WorkflowExecutionCancelRequestedCause = WorkflowExecutionCancelRequestedCause'
  { fromWorkflowExecutionCancelRequestedCause ::
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

pattern WorkflowExecutionCancelRequestedCause_CHILD_POLICY_APPLIED :: WorkflowExecutionCancelRequestedCause
pattern WorkflowExecutionCancelRequestedCause_CHILD_POLICY_APPLIED = WorkflowExecutionCancelRequestedCause' "CHILD_POLICY_APPLIED"

{-# COMPLETE
  WorkflowExecutionCancelRequestedCause_CHILD_POLICY_APPLIED,
  WorkflowExecutionCancelRequestedCause'
  #-}
