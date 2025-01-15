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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { -- | The creation time of the scheduled query.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | Configuration for scheduled query error reporting.
    errorReportConfiguration :: Prelude.Maybe ErrorReportConfiguration,
    -- | Status of the last scheduled query run.
    lastRunStatus :: Prelude.Maybe ScheduledQueryRunStatus,
    -- | The next time the scheduled query is to be run.
    nextInvocationTime :: Prelude.Maybe Data.POSIX,
    -- | The last time the scheduled query was run.
    previousInvocationTime :: Prelude.Maybe Data.POSIX,
    -- | Target data source where final scheduled query result will be written.
    targetDestination :: Prelude.Maybe TargetDestination,
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
-- 'creationTime', 'scheduledQuery_creationTime' - The creation time of the scheduled query.
--
-- 'errorReportConfiguration', 'scheduledQuery_errorReportConfiguration' - Configuration for scheduled query error reporting.
--
-- 'lastRunStatus', 'scheduledQuery_lastRunStatus' - Status of the last scheduled query run.
--
-- 'nextInvocationTime', 'scheduledQuery_nextInvocationTime' - The next time the scheduled query is to be run.
--
-- 'previousInvocationTime', 'scheduledQuery_previousInvocationTime' - The last time the scheduled query was run.
--
-- 'targetDestination', 'scheduledQuery_targetDestination' - Target data source where final scheduled query result will be written.
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
    { creationTime = Prelude.Nothing,
      errorReportConfiguration = Prelude.Nothing,
      lastRunStatus = Prelude.Nothing,
      nextInvocationTime = Prelude.Nothing,
      previousInvocationTime = Prelude.Nothing,
      targetDestination = Prelude.Nothing,
      arn = pArn_,
      name = pName_,
      state = pState_
    }

-- | The creation time of the scheduled query.
scheduledQuery_creationTime :: Lens.Lens' ScheduledQuery (Prelude.Maybe Prelude.UTCTime)
scheduledQuery_creationTime = Lens.lens (\ScheduledQuery' {creationTime} -> creationTime) (\s@ScheduledQuery' {} a -> s {creationTime = a} :: ScheduledQuery) Prelude.. Lens.mapping Data._Time

-- | Configuration for scheduled query error reporting.
scheduledQuery_errorReportConfiguration :: Lens.Lens' ScheduledQuery (Prelude.Maybe ErrorReportConfiguration)
scheduledQuery_errorReportConfiguration = Lens.lens (\ScheduledQuery' {errorReportConfiguration} -> errorReportConfiguration) (\s@ScheduledQuery' {} a -> s {errorReportConfiguration = a} :: ScheduledQuery)

-- | Status of the last scheduled query run.
scheduledQuery_lastRunStatus :: Lens.Lens' ScheduledQuery (Prelude.Maybe ScheduledQueryRunStatus)
scheduledQuery_lastRunStatus = Lens.lens (\ScheduledQuery' {lastRunStatus} -> lastRunStatus) (\s@ScheduledQuery' {} a -> s {lastRunStatus = a} :: ScheduledQuery)

-- | The next time the scheduled query is to be run.
scheduledQuery_nextInvocationTime :: Lens.Lens' ScheduledQuery (Prelude.Maybe Prelude.UTCTime)
scheduledQuery_nextInvocationTime = Lens.lens (\ScheduledQuery' {nextInvocationTime} -> nextInvocationTime) (\s@ScheduledQuery' {} a -> s {nextInvocationTime = a} :: ScheduledQuery) Prelude.. Lens.mapping Data._Time

-- | The last time the scheduled query was run.
scheduledQuery_previousInvocationTime :: Lens.Lens' ScheduledQuery (Prelude.Maybe Prelude.UTCTime)
scheduledQuery_previousInvocationTime = Lens.lens (\ScheduledQuery' {previousInvocationTime} -> previousInvocationTime) (\s@ScheduledQuery' {} a -> s {previousInvocationTime = a} :: ScheduledQuery) Prelude.. Lens.mapping Data._Time

-- | Target data source where final scheduled query result will be written.
scheduledQuery_targetDestination :: Lens.Lens' ScheduledQuery (Prelude.Maybe TargetDestination)
scheduledQuery_targetDestination = Lens.lens (\ScheduledQuery' {targetDestination} -> targetDestination) (\s@ScheduledQuery' {} a -> s {targetDestination = a} :: ScheduledQuery)

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
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "ErrorReportConfiguration")
            Prelude.<*> (x Data..:? "LastRunStatus")
            Prelude.<*> (x Data..:? "NextInvocationTime")
            Prelude.<*> (x Data..:? "PreviousInvocationTime")
            Prelude.<*> (x Data..:? "TargetDestination")
            Prelude.<*> (x Data..: "Arn")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "State")
      )

instance Prelude.Hashable ScheduledQuery where
  hashWithSalt _salt ScheduledQuery' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` errorReportConfiguration
      `Prelude.hashWithSalt` lastRunStatus
      `Prelude.hashWithSalt` nextInvocationTime
      `Prelude.hashWithSalt` previousInvocationTime
      `Prelude.hashWithSalt` targetDestination
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` state

instance Prelude.NFData ScheduledQuery where
  rnf ScheduledQuery' {..} =
    Prelude.rnf creationTime `Prelude.seq`
      Prelude.rnf errorReportConfiguration `Prelude.seq`
        Prelude.rnf lastRunStatus `Prelude.seq`
          Prelude.rnf nextInvocationTime `Prelude.seq`
            Prelude.rnf previousInvocationTime `Prelude.seq`
              Prelude.rnf targetDestination `Prelude.seq`
                Prelude.rnf arn `Prelude.seq`
                  Prelude.rnf name `Prelude.seq`
                    Prelude.rnf state
