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
-- Module      : Amazonka.MigrationHubOrchestrator.Types.MigrationWorkflowStatusEnum
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubOrchestrator.Types.MigrationWorkflowStatusEnum
  ( MigrationWorkflowStatusEnum
      ( ..,
        MigrationWorkflowStatusEnum_COMPLETED,
        MigrationWorkflowStatusEnum_CREATING,
        MigrationWorkflowStatusEnum_CREATION_FAILED,
        MigrationWorkflowStatusEnum_DELETED,
        MigrationWorkflowStatusEnum_DELETING,
        MigrationWorkflowStatusEnum_DELETION_FAILED,
        MigrationWorkflowStatusEnum_IN_PROGRESS,
        MigrationWorkflowStatusEnum_NOT_STARTED,
        MigrationWorkflowStatusEnum_PAUSED,
        MigrationWorkflowStatusEnum_PAUSING,
        MigrationWorkflowStatusEnum_PAUSING_FAILED,
        MigrationWorkflowStatusEnum_STARTING,
        MigrationWorkflowStatusEnum_USER_ATTENTION_REQUIRED,
        MigrationWorkflowStatusEnum_WORKFLOW_FAILED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MigrationWorkflowStatusEnum = MigrationWorkflowStatusEnum'
  { fromMigrationWorkflowStatusEnum ::
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

pattern MigrationWorkflowStatusEnum_COMPLETED :: MigrationWorkflowStatusEnum
pattern MigrationWorkflowStatusEnum_COMPLETED = MigrationWorkflowStatusEnum' "COMPLETED"

pattern MigrationWorkflowStatusEnum_CREATING :: MigrationWorkflowStatusEnum
pattern MigrationWorkflowStatusEnum_CREATING = MigrationWorkflowStatusEnum' "CREATING"

pattern MigrationWorkflowStatusEnum_CREATION_FAILED :: MigrationWorkflowStatusEnum
pattern MigrationWorkflowStatusEnum_CREATION_FAILED = MigrationWorkflowStatusEnum' "CREATION_FAILED"

pattern MigrationWorkflowStatusEnum_DELETED :: MigrationWorkflowStatusEnum
pattern MigrationWorkflowStatusEnum_DELETED = MigrationWorkflowStatusEnum' "DELETED"

pattern MigrationWorkflowStatusEnum_DELETING :: MigrationWorkflowStatusEnum
pattern MigrationWorkflowStatusEnum_DELETING = MigrationWorkflowStatusEnum' "DELETING"

pattern MigrationWorkflowStatusEnum_DELETION_FAILED :: MigrationWorkflowStatusEnum
pattern MigrationWorkflowStatusEnum_DELETION_FAILED = MigrationWorkflowStatusEnum' "DELETION_FAILED"

pattern MigrationWorkflowStatusEnum_IN_PROGRESS :: MigrationWorkflowStatusEnum
pattern MigrationWorkflowStatusEnum_IN_PROGRESS = MigrationWorkflowStatusEnum' "IN_PROGRESS"

pattern MigrationWorkflowStatusEnum_NOT_STARTED :: MigrationWorkflowStatusEnum
pattern MigrationWorkflowStatusEnum_NOT_STARTED = MigrationWorkflowStatusEnum' "NOT_STARTED"

pattern MigrationWorkflowStatusEnum_PAUSED :: MigrationWorkflowStatusEnum
pattern MigrationWorkflowStatusEnum_PAUSED = MigrationWorkflowStatusEnum' "PAUSED"

pattern MigrationWorkflowStatusEnum_PAUSING :: MigrationWorkflowStatusEnum
pattern MigrationWorkflowStatusEnum_PAUSING = MigrationWorkflowStatusEnum' "PAUSING"

pattern MigrationWorkflowStatusEnum_PAUSING_FAILED :: MigrationWorkflowStatusEnum
pattern MigrationWorkflowStatusEnum_PAUSING_FAILED = MigrationWorkflowStatusEnum' "PAUSING_FAILED"

pattern MigrationWorkflowStatusEnum_STARTING :: MigrationWorkflowStatusEnum
pattern MigrationWorkflowStatusEnum_STARTING = MigrationWorkflowStatusEnum' "STARTING"

pattern MigrationWorkflowStatusEnum_USER_ATTENTION_REQUIRED :: MigrationWorkflowStatusEnum
pattern MigrationWorkflowStatusEnum_USER_ATTENTION_REQUIRED = MigrationWorkflowStatusEnum' "USER_ATTENTION_REQUIRED"

pattern MigrationWorkflowStatusEnum_WORKFLOW_FAILED :: MigrationWorkflowStatusEnum
pattern MigrationWorkflowStatusEnum_WORKFLOW_FAILED = MigrationWorkflowStatusEnum' "WORKFLOW_FAILED"

{-# COMPLETE
  MigrationWorkflowStatusEnum_COMPLETED,
  MigrationWorkflowStatusEnum_CREATING,
  MigrationWorkflowStatusEnum_CREATION_FAILED,
  MigrationWorkflowStatusEnum_DELETED,
  MigrationWorkflowStatusEnum_DELETING,
  MigrationWorkflowStatusEnum_DELETION_FAILED,
  MigrationWorkflowStatusEnum_IN_PROGRESS,
  MigrationWorkflowStatusEnum_NOT_STARTED,
  MigrationWorkflowStatusEnum_PAUSED,
  MigrationWorkflowStatusEnum_PAUSING,
  MigrationWorkflowStatusEnum_PAUSING_FAILED,
  MigrationWorkflowStatusEnum_STARTING,
  MigrationWorkflowStatusEnum_USER_ATTENTION_REQUIRED,
  MigrationWorkflowStatusEnum_WORKFLOW_FAILED,
  MigrationWorkflowStatusEnum'
  #-}
