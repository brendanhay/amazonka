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
-- Module      : Amazonka.Athena.Types.QueryExecutionStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.QueryExecutionStatus where

import Amazonka.Athena.Types.AthenaError
import Amazonka.Athena.Types.QueryExecutionState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The completion date, current state, submission time, and state change
-- reason (if applicable) for the query execution.
--
-- /See:/ 'newQueryExecutionStatus' smart constructor.
data QueryExecutionStatus = QueryExecutionStatus'
  { -- | Further detail about the status of the query.
    stateChangeReason :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the query was submitted.
    submissionDateTime :: Prelude.Maybe Data.POSIX,
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
    state :: Prelude.Maybe QueryExecutionState,
    -- | Provides information about an Athena query error.
    athenaError :: Prelude.Maybe AthenaError,
    -- | The date and time that the query completed.
    completionDateTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryExecutionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stateChangeReason', 'queryExecutionStatus_stateChangeReason' - Further detail about the status of the query.
--
-- 'submissionDateTime', 'queryExecutionStatus_submissionDateTime' - The date and time that the query was submitted.
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
--
-- 'athenaError', 'queryExecutionStatus_athenaError' - Provides information about an Athena query error.
--
-- 'completionDateTime', 'queryExecutionStatus_completionDateTime' - The date and time that the query completed.
newQueryExecutionStatus ::
  QueryExecutionStatus
newQueryExecutionStatus =
  QueryExecutionStatus'
    { stateChangeReason =
        Prelude.Nothing,
      submissionDateTime = Prelude.Nothing,
      state = Prelude.Nothing,
      athenaError = Prelude.Nothing,
      completionDateTime = Prelude.Nothing
    }

-- | Further detail about the status of the query.
queryExecutionStatus_stateChangeReason :: Lens.Lens' QueryExecutionStatus (Prelude.Maybe Prelude.Text)
queryExecutionStatus_stateChangeReason = Lens.lens (\QueryExecutionStatus' {stateChangeReason} -> stateChangeReason) (\s@QueryExecutionStatus' {} a -> s {stateChangeReason = a} :: QueryExecutionStatus)

-- | The date and time that the query was submitted.
queryExecutionStatus_submissionDateTime :: Lens.Lens' QueryExecutionStatus (Prelude.Maybe Prelude.UTCTime)
queryExecutionStatus_submissionDateTime = Lens.lens (\QueryExecutionStatus' {submissionDateTime} -> submissionDateTime) (\s@QueryExecutionStatus' {} a -> s {submissionDateTime = a} :: QueryExecutionStatus) Prelude.. Lens.mapping Data._Time

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
queryExecutionStatus_state :: Lens.Lens' QueryExecutionStatus (Prelude.Maybe QueryExecutionState)
queryExecutionStatus_state = Lens.lens (\QueryExecutionStatus' {state} -> state) (\s@QueryExecutionStatus' {} a -> s {state = a} :: QueryExecutionStatus)

-- | Provides information about an Athena query error.
queryExecutionStatus_athenaError :: Lens.Lens' QueryExecutionStatus (Prelude.Maybe AthenaError)
queryExecutionStatus_athenaError = Lens.lens (\QueryExecutionStatus' {athenaError} -> athenaError) (\s@QueryExecutionStatus' {} a -> s {athenaError = a} :: QueryExecutionStatus)

-- | The date and time that the query completed.
queryExecutionStatus_completionDateTime :: Lens.Lens' QueryExecutionStatus (Prelude.Maybe Prelude.UTCTime)
queryExecutionStatus_completionDateTime = Lens.lens (\QueryExecutionStatus' {completionDateTime} -> completionDateTime) (\s@QueryExecutionStatus' {} a -> s {completionDateTime = a} :: QueryExecutionStatus) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON QueryExecutionStatus where
  parseJSON =
    Data.withObject
      "QueryExecutionStatus"
      ( \x ->
          QueryExecutionStatus'
            Prelude.<$> (x Data..:? "StateChangeReason")
            Prelude.<*> (x Data..:? "SubmissionDateTime")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "AthenaError")
            Prelude.<*> (x Data..:? "CompletionDateTime")
      )

instance Prelude.Hashable QueryExecutionStatus where
  hashWithSalt _salt QueryExecutionStatus' {..} =
    _salt `Prelude.hashWithSalt` stateChangeReason
      `Prelude.hashWithSalt` submissionDateTime
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` athenaError
      `Prelude.hashWithSalt` completionDateTime

instance Prelude.NFData QueryExecutionStatus where
  rnf QueryExecutionStatus' {..} =
    Prelude.rnf stateChangeReason
      `Prelude.seq` Prelude.rnf submissionDateTime
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf athenaError
      `Prelude.seq` Prelude.rnf completionDateTime
