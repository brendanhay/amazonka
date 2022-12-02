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
-- Module      : Amazonka.TimeStreamQuery.Types.ScheduledQuery
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.ScheduledQuery where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamQuery.Types.ErrorReportConfiguration
import Amazonka.TimeStreamQuery.Types.ScheduledQueryRunStatus
import Amazonka.TimeStreamQuery.Types.ScheduledQueryState
import Amazonka.TimeStreamQuery.Types.TargetDestination

-- | Scheduled Query
--
-- /See:/ 'newScheduledQuery' smart constructor.
data ScheduledQuery = ScheduledQuery'
  { -- | Target data source where final scheduled query result will be written.
    targetDestination :: Prelude.Maybe TargetDestination,
    -- | The last time the scheduled query was run.
    previousInvocationTime :: Prelude.Maybe Data.POSIX,
    -- | Configuration for scheduled query error reporting.
    errorReportConfiguration :: Prelude.Maybe ErrorReportConfiguration,
    -- | The next time the scheduled query is to be run.
    nextInvocationTime :: Prelude.Maybe Data.POSIX,
    -- | The creation time of the scheduled query.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | Status of the last scheduled query run.
    lastRunStatus :: Prelude.Maybe ScheduledQueryRunStatus,
    -- | The Amazon Resource Name.
    arn :: Prelude.Text,
    -- | The name of the scheduled query.
    name :: Prelude.Text,
    -- | State of scheduled query.
    state :: ScheduledQueryState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduledQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetDestination', 'scheduledQuery_targetDestination' - Target data source where final scheduled query result will be written.
--
-- 'previousInvocationTime', 'scheduledQuery_previousInvocationTime' - The last time the scheduled query was run.
--
-- 'errorReportConfiguration', 'scheduledQuery_errorReportConfiguration' - Configuration for scheduled query error reporting.
--
-- 'nextInvocationTime', 'scheduledQuery_nextInvocationTime' - The next time the scheduled query is to be run.
--
-- 'creationTime', 'scheduledQuery_creationTime' - The creation time of the scheduled query.
--
-- 'lastRunStatus', 'scheduledQuery_lastRunStatus' - Status of the last scheduled query run.
--
-- 'arn', 'scheduledQuery_arn' - The Amazon Resource Name.
--
-- 'name', 'scheduledQuery_name' - The name of the scheduled query.
--
-- 'state', 'scheduledQuery_state' - State of scheduled query.
newScheduledQuery ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'state'
  ScheduledQueryState ->
  ScheduledQuery
newScheduledQuery pArn_ pName_ pState_ =
  ScheduledQuery'
    { targetDestination =
        Prelude.Nothing,
      previousInvocationTime = Prelude.Nothing,
      errorReportConfiguration = Prelude.Nothing,
      nextInvocationTime = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastRunStatus = Prelude.Nothing,
      arn = pArn_,
      name = pName_,
      state = pState_
    }

-- | Target data source where final scheduled query result will be written.
scheduledQuery_targetDestination :: Lens.Lens' ScheduledQuery (Prelude.Maybe TargetDestination)
scheduledQuery_targetDestination = Lens.lens (\ScheduledQuery' {targetDestination} -> targetDestination) (\s@ScheduledQuery' {} a -> s {targetDestination = a} :: ScheduledQuery)

-- | The last time the scheduled query was run.
scheduledQuery_previousInvocationTime :: Lens.Lens' ScheduledQuery (Prelude.Maybe Prelude.UTCTime)
scheduledQuery_previousInvocationTime = Lens.lens (\ScheduledQuery' {previousInvocationTime} -> previousInvocationTime) (\s@ScheduledQuery' {} a -> s {previousInvocationTime = a} :: ScheduledQuery) Prelude.. Lens.mapping Data._Time

-- | Configuration for scheduled query error reporting.
scheduledQuery_errorReportConfiguration :: Lens.Lens' ScheduledQuery (Prelude.Maybe ErrorReportConfiguration)
scheduledQuery_errorReportConfiguration = Lens.lens (\ScheduledQuery' {errorReportConfiguration} -> errorReportConfiguration) (\s@ScheduledQuery' {} a -> s {errorReportConfiguration = a} :: ScheduledQuery)

-- | The next time the scheduled query is to be run.
scheduledQuery_nextInvocationTime :: Lens.Lens' ScheduledQuery (Prelude.Maybe Prelude.UTCTime)
scheduledQuery_nextInvocationTime = Lens.lens (\ScheduledQuery' {nextInvocationTime} -> nextInvocationTime) (\s@ScheduledQuery' {} a -> s {nextInvocationTime = a} :: ScheduledQuery) Prelude.. Lens.mapping Data._Time

-- | The creation time of the scheduled query.
scheduledQuery_creationTime :: Lens.Lens' ScheduledQuery (Prelude.Maybe Prelude.UTCTime)
scheduledQuery_creationTime = Lens.lens (\ScheduledQuery' {creationTime} -> creationTime) (\s@ScheduledQuery' {} a -> s {creationTime = a} :: ScheduledQuery) Prelude.. Lens.mapping Data._Time

-- | Status of the last scheduled query run.
scheduledQuery_lastRunStatus :: Lens.Lens' ScheduledQuery (Prelude.Maybe ScheduledQueryRunStatus)
scheduledQuery_lastRunStatus = Lens.lens (\ScheduledQuery' {lastRunStatus} -> lastRunStatus) (\s@ScheduledQuery' {} a -> s {lastRunStatus = a} :: ScheduledQuery)

-- | The Amazon Resource Name.
scheduledQuery_arn :: Lens.Lens' ScheduledQuery Prelude.Text
scheduledQuery_arn = Lens.lens (\ScheduledQuery' {arn} -> arn) (\s@ScheduledQuery' {} a -> s {arn = a} :: ScheduledQuery)

-- | The name of the scheduled query.
scheduledQuery_name :: Lens.Lens' ScheduledQuery Prelude.Text
scheduledQuery_name = Lens.lens (\ScheduledQuery' {name} -> name) (\s@ScheduledQuery' {} a -> s {name = a} :: ScheduledQuery)

-- | State of scheduled query.
scheduledQuery_state :: Lens.Lens' ScheduledQuery ScheduledQueryState
scheduledQuery_state = Lens.lens (\ScheduledQuery' {state} -> state) (\s@ScheduledQuery' {} a -> s {state = a} :: ScheduledQuery)

instance Data.FromJSON ScheduledQuery where
  parseJSON =
    Data.withObject
      "ScheduledQuery"
      ( \x ->
          ScheduledQuery'
            Prelude.<$> (x Data..:? "TargetDestination")
            Prelude.<*> (x Data..:? "PreviousInvocationTime")
            Prelude.<*> (x Data..:? "ErrorReportConfiguration")
            Prelude.<*> (x Data..:? "NextInvocationTime")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "LastRunStatus")
            Prelude.<*> (x Data..: "Arn")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "State")
      )

instance Prelude.Hashable ScheduledQuery where
  hashWithSalt _salt ScheduledQuery' {..} =
    _salt `Prelude.hashWithSalt` targetDestination
      `Prelude.hashWithSalt` previousInvocationTime
      `Prelude.hashWithSalt` errorReportConfiguration
      `Prelude.hashWithSalt` nextInvocationTime
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastRunStatus
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` state

instance Prelude.NFData ScheduledQuery where
  rnf ScheduledQuery' {..} =
    Prelude.rnf targetDestination
      `Prelude.seq` Prelude.rnf previousInvocationTime
      `Prelude.seq` Prelude.rnf errorReportConfiguration
      `Prelude.seq` Prelude.rnf nextInvocationTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastRunStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf state
