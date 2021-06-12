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
-- Module      : Network.AWS.Glue.Types.WorkflowRunStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.WorkflowRunStatus
  ( WorkflowRunStatus
      ( ..,
        WorkflowRunStatus_COMPLETED,
        WorkflowRunStatus_ERROR,
        WorkflowRunStatus_RUNNING,
        WorkflowRunStatus_STOPPED,
        WorkflowRunStatus_STOPPING
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype WorkflowRunStatus = WorkflowRunStatus'
  { fromWorkflowRunStatus ::
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

pattern WorkflowRunStatus_COMPLETED :: WorkflowRunStatus
pattern WorkflowRunStatus_COMPLETED = WorkflowRunStatus' "COMPLETED"

pattern WorkflowRunStatus_ERROR :: WorkflowRunStatus
pattern WorkflowRunStatus_ERROR = WorkflowRunStatus' "ERROR"

pattern WorkflowRunStatus_RUNNING :: WorkflowRunStatus
pattern WorkflowRunStatus_RUNNING = WorkflowRunStatus' "RUNNING"

pattern WorkflowRunStatus_STOPPED :: WorkflowRunStatus
pattern WorkflowRunStatus_STOPPED = WorkflowRunStatus' "STOPPED"

pattern WorkflowRunStatus_STOPPING :: WorkflowRunStatus
pattern WorkflowRunStatus_STOPPING = WorkflowRunStatus' "STOPPING"

{-# COMPLETE
  WorkflowRunStatus_COMPLETED,
  WorkflowRunStatus_ERROR,
  WorkflowRunStatus_RUNNING,
  WorkflowRunStatus_STOPPED,
  WorkflowRunStatus_STOPPING,
  WorkflowRunStatus'
  #-}
