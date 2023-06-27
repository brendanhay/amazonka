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
-- Module      : Amazonka.ImageBuilder.Types.WorkflowStepExecutionRollbackStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.WorkflowStepExecutionRollbackStatus
  ( WorkflowStepExecutionRollbackStatus
      ( ..,
        WorkflowStepExecutionRollbackStatus_COMPLETED,
        WorkflowStepExecutionRollbackStatus_FAILED,
        WorkflowStepExecutionRollbackStatus_RUNNING,
        WorkflowStepExecutionRollbackStatus_SKIPPED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WorkflowStepExecutionRollbackStatus = WorkflowStepExecutionRollbackStatus'
  { fromWorkflowStepExecutionRollbackStatus ::
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

pattern WorkflowStepExecutionRollbackStatus_COMPLETED :: WorkflowStepExecutionRollbackStatus
pattern WorkflowStepExecutionRollbackStatus_COMPLETED = WorkflowStepExecutionRollbackStatus' "COMPLETED"

pattern WorkflowStepExecutionRollbackStatus_FAILED :: WorkflowStepExecutionRollbackStatus
pattern WorkflowStepExecutionRollbackStatus_FAILED = WorkflowStepExecutionRollbackStatus' "FAILED"

pattern WorkflowStepExecutionRollbackStatus_RUNNING :: WorkflowStepExecutionRollbackStatus
pattern WorkflowStepExecutionRollbackStatus_RUNNING = WorkflowStepExecutionRollbackStatus' "RUNNING"

pattern WorkflowStepExecutionRollbackStatus_SKIPPED :: WorkflowStepExecutionRollbackStatus
pattern WorkflowStepExecutionRollbackStatus_SKIPPED = WorkflowStepExecutionRollbackStatus' "SKIPPED"

{-# COMPLETE
  WorkflowStepExecutionRollbackStatus_COMPLETED,
  WorkflowStepExecutionRollbackStatus_FAILED,
  WorkflowStepExecutionRollbackStatus_RUNNING,
  WorkflowStepExecutionRollbackStatus_SKIPPED,
  WorkflowStepExecutionRollbackStatus'
  #-}
