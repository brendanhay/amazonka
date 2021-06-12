{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.QueryExecutionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.QueryExecutionStatus where

import Network.AWS.Athena.Types.QueryExecutionState
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The completion date, current state, submission time, and state change
-- reason (if applicable) for the query execution.
--
-- /See:/ 'newQueryExecutionStatus' smart constructor.
data QueryExecutionStatus = QueryExecutionStatus'
  { -- | The date and time that the query was submitted.
    submissionDateTime :: Core.Maybe Core.POSIX,
    -- | Further detail about the status of the query.
    stateChangeReason :: Core.Maybe Core.Text,
    -- | The date and time that the query completed.
    completionDateTime :: Core.Maybe Core.POSIX,
    -- | The state of query execution. @QUEUED@ indicates that the query has been
    -- submitted to the service, and Athena will execute the query as soon as
    -- resources are available. @RUNNING@ indicates that the query is in
    -- execution phase. @SUCCEEDED@ indicates that the query completed without
    -- errors. @FAILED@ indicates that the query experienced an error and did
    -- not complete processing. @CANCELLED@ indicates that a user input
    -- interrupted query execution.
    --
    -- Athena automatically retries your queries in cases of certain transient
    -- errors. As a result, you may see the query state transition from
    -- @RUNNING@ or @FAILED@ to @QUEUED@.
    state :: Core.Maybe QueryExecutionState
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'QueryExecutionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'submissionDateTime', 'queryExecutionStatus_submissionDateTime' - The date and time that the query was submitted.
--
-- 'stateChangeReason', 'queryExecutionStatus_stateChangeReason' - Further detail about the status of the query.
--
-- 'completionDateTime', 'queryExecutionStatus_completionDateTime' - The date and time that the query completed.
--
-- 'state', 'queryExecutionStatus_state' - The state of query execution. @QUEUED@ indicates that the query has been
-- submitted to the service, and Athena will execute the query as soon as
-- resources are available. @RUNNING@ indicates that the query is in
-- execution phase. @SUCCEEDED@ indicates that the query completed without
-- errors. @FAILED@ indicates that the query experienced an error and did
-- not complete processing. @CANCELLED@ indicates that a user input
-- interrupted query execution.
--
-- Athena automatically retries your queries in cases of certain transient
-- errors. As a result, you may see the query state transition from
-- @RUNNING@ or @FAILED@ to @QUEUED@.
newQueryExecutionStatus ::
  QueryExecutionStatus
newQueryExecutionStatus =
  QueryExecutionStatus'
    { submissionDateTime =
        Core.Nothing,
      stateChangeReason = Core.Nothing,
      completionDateTime = Core.Nothing,
      state = Core.Nothing
    }

-- | The date and time that the query was submitted.
queryExecutionStatus_submissionDateTime :: Lens.Lens' QueryExecutionStatus (Core.Maybe Core.UTCTime)
queryExecutionStatus_submissionDateTime = Lens.lens (\QueryExecutionStatus' {submissionDateTime} -> submissionDateTime) (\s@QueryExecutionStatus' {} a -> s {submissionDateTime = a} :: QueryExecutionStatus) Core.. Lens.mapping Core._Time

-- | Further detail about the status of the query.
queryExecutionStatus_stateChangeReason :: Lens.Lens' QueryExecutionStatus (Core.Maybe Core.Text)
queryExecutionStatus_stateChangeReason = Lens.lens (\QueryExecutionStatus' {stateChangeReason} -> stateChangeReason) (\s@QueryExecutionStatus' {} a -> s {stateChangeReason = a} :: QueryExecutionStatus)

-- | The date and time that the query completed.
queryExecutionStatus_completionDateTime :: Lens.Lens' QueryExecutionStatus (Core.Maybe Core.UTCTime)
queryExecutionStatus_completionDateTime = Lens.lens (\QueryExecutionStatus' {completionDateTime} -> completionDateTime) (\s@QueryExecutionStatus' {} a -> s {completionDateTime = a} :: QueryExecutionStatus) Core.. Lens.mapping Core._Time

-- | The state of query execution. @QUEUED@ indicates that the query has been
-- submitted to the service, and Athena will execute the query as soon as
-- resources are available. @RUNNING@ indicates that the query is in
-- execution phase. @SUCCEEDED@ indicates that the query completed without
-- errors. @FAILED@ indicates that the query experienced an error and did
-- not complete processing. @CANCELLED@ indicates that a user input
-- interrupted query execution.
--
-- Athena automatically retries your queries in cases of certain transient
-- errors. As a result, you may see the query state transition from
-- @RUNNING@ or @FAILED@ to @QUEUED@.
queryExecutionStatus_state :: Lens.Lens' QueryExecutionStatus (Core.Maybe QueryExecutionState)
queryExecutionStatus_state = Lens.lens (\QueryExecutionStatus' {state} -> state) (\s@QueryExecutionStatus' {} a -> s {state = a} :: QueryExecutionStatus)

instance Core.FromJSON QueryExecutionStatus where
  parseJSON =
    Core.withObject
      "QueryExecutionStatus"
      ( \x ->
          QueryExecutionStatus'
            Core.<$> (x Core..:? "SubmissionDateTime")
            Core.<*> (x Core..:? "StateChangeReason")
            Core.<*> (x Core..:? "CompletionDateTime")
            Core.<*> (x Core..:? "State")
      )

instance Core.Hashable QueryExecutionStatus

instance Core.NFData QueryExecutionStatus
