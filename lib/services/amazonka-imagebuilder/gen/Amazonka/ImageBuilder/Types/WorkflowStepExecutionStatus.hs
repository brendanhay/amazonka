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
-- Module      : Amazonka.ImageBuilder.Types.WorkflowStepExecutionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.WorkflowStepExecutionStatus
  ( WorkflowStepExecutionStatus
      ( ..,
        WorkflowStepExecutionStatus_COMPLETED,
        WorkflowStepExecutionStatus_FAILED,
        WorkflowStepExecutionStatus_PENDING,
        WorkflowStepExecutionStatus_RUNNING,
        WorkflowStepExecutionStatus_SKIPPED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WorkflowStepExecutionStatus = WorkflowStepExecutionStatus'
  { fromWorkflowStepExecutionStatus ::
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

pattern WorkflowStepExecutionStatus_COMPLETED :: WorkflowStepExecutionStatus
pattern WorkflowStepExecutionStatus_COMPLETED = WorkflowStepExecutionStatus' "COMPLETED"

pattern WorkflowStepExecutionStatus_FAILED :: WorkflowStepExecutionStatus
pattern WorkflowStepExecutionStatus_FAILED = WorkflowStepExecutionStatus' "FAILED"

pattern WorkflowStepExecutionStatus_PENDING :: WorkflowStepExecutionStatus
pattern WorkflowStepExecutionStatus_PENDING = WorkflowStepExecutionStatus' "PENDING"

pattern WorkflowStepExecutionStatus_RUNNING :: WorkflowStepExecutionStatus
pattern WorkflowStepExecutionStatus_RUNNING = WorkflowStepExecutionStatus' "RUNNING"

pattern WorkflowStepExecutionStatus_SKIPPED :: WorkflowStepExecutionStatus
pattern WorkflowStepExecutionStatus_SKIPPED = WorkflowStepExecutionStatus' "SKIPPED"

{-# COMPLETE
  WorkflowStepExecutionStatus_COMPLETED,
  WorkflowStepExecutionStatus_FAILED,
  WorkflowStepExecutionStatus_PENDING,
  WorkflowStepExecutionStatus_RUNNING,
  WorkflowStepExecutionStatus_SKIPPED,
  WorkflowStepExecutionStatus'
  #-}
