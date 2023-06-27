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
-- Module      : Amazonka.ImageBuilder.Types.WorkflowExecutionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.WorkflowExecutionStatus
  ( WorkflowExecutionStatus
      ( ..,
        WorkflowExecutionStatus_COMPLETED,
        WorkflowExecutionStatus_FAILED,
        WorkflowExecutionStatus_PENDING,
        WorkflowExecutionStatus_ROLLBACK_COMPLETED,
        WorkflowExecutionStatus_ROLLBACK_IN_PROGRESS,
        WorkflowExecutionStatus_RUNNING,
        WorkflowExecutionStatus_SKIPPED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WorkflowExecutionStatus = WorkflowExecutionStatus'
  { fromWorkflowExecutionStatus ::
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

pattern WorkflowExecutionStatus_COMPLETED :: WorkflowExecutionStatus
pattern WorkflowExecutionStatus_COMPLETED = WorkflowExecutionStatus' "COMPLETED"

pattern WorkflowExecutionStatus_FAILED :: WorkflowExecutionStatus
pattern WorkflowExecutionStatus_FAILED = WorkflowExecutionStatus' "FAILED"

pattern WorkflowExecutionStatus_PENDING :: WorkflowExecutionStatus
pattern WorkflowExecutionStatus_PENDING = WorkflowExecutionStatus' "PENDING"

pattern WorkflowExecutionStatus_ROLLBACK_COMPLETED :: WorkflowExecutionStatus
pattern WorkflowExecutionStatus_ROLLBACK_COMPLETED = WorkflowExecutionStatus' "ROLLBACK_COMPLETED"

pattern WorkflowExecutionStatus_ROLLBACK_IN_PROGRESS :: WorkflowExecutionStatus
pattern WorkflowExecutionStatus_ROLLBACK_IN_PROGRESS = WorkflowExecutionStatus' "ROLLBACK_IN_PROGRESS"

pattern WorkflowExecutionStatus_RUNNING :: WorkflowExecutionStatus
pattern WorkflowExecutionStatus_RUNNING = WorkflowExecutionStatus' "RUNNING"

pattern WorkflowExecutionStatus_SKIPPED :: WorkflowExecutionStatus
pattern WorkflowExecutionStatus_SKIPPED = WorkflowExecutionStatus' "SKIPPED"

{-# COMPLETE
  WorkflowExecutionStatus_COMPLETED,
  WorkflowExecutionStatus_FAILED,
  WorkflowExecutionStatus_PENDING,
  WorkflowExecutionStatus_ROLLBACK_COMPLETED,
  WorkflowExecutionStatus_ROLLBACK_IN_PROGRESS,
  WorkflowExecutionStatus_RUNNING,
  WorkflowExecutionStatus_SKIPPED,
  WorkflowExecutionStatus'
  #-}
