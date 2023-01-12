{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MigrationHub.NotifyMigrationTaskState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Notifies Migration Hub of the current status, progress, or other detail
-- regarding a migration task. This API has the following traits:
--
-- -   Migration tools will call the @NotifyMigrationTaskState@ API to
--     share the latest progress and status.
--
-- -   @MigrationTaskName@ is used for addressing updates to the correct
--     target.
--
-- -   @ProgressUpdateStream@ is used for access control and to provide a
--     namespace for each migration tool.
module Amazonka.MigrationHub.NotifyMigrationTaskState
  ( -- * Creating a Request
    NotifyMigrationTaskState (..),
    newNotifyMigrationTaskState,

    -- * Request Lenses
    notifyMigrationTaskState_dryRun,
    notifyMigrationTaskState_progressUpdateStream,
    notifyMigrationTaskState_migrationTaskName,
    notifyMigrationTaskState_task,
    notifyMigrationTaskState_updateDateTime,
    notifyMigrationTaskState_nextUpdateSeconds,

    -- * Destructuring the Response
    NotifyMigrationTaskStateResponse (..),
    newNotifyMigrationTaskStateResponse,

    -- * Response Lenses
    notifyMigrationTaskStateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHub.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newNotifyMigrationTaskState' smart constructor.
data NotifyMigrationTaskState = NotifyMigrationTaskState'
  { -- | Optional boolean flag to indicate whether any effect should take place.
    -- Used to test if the caller has permission to make the call.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Prelude.Text,
    -- | Unique identifier that references the migration task. /Do not store
    -- personal data in this field./
    migrationTaskName :: Prelude.Text,
    -- | Information about the task\'s progress and status.
    task :: Task,
    -- | The timestamp when the task was gathered.
    updateDateTime :: Data.POSIX,
    -- | Number of seconds after the UpdateDateTime within which the Migration
    -- Hub can expect an update. If Migration Hub does not receive an update
    -- within the specified interval, then the migration task will be
    -- considered stale.
    nextUpdateSeconds :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotifyMigrationTaskState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'notifyMigrationTaskState_dryRun' - Optional boolean flag to indicate whether any effect should take place.
-- Used to test if the caller has permission to make the call.
--
-- 'progressUpdateStream', 'notifyMigrationTaskState_progressUpdateStream' - The name of the ProgressUpdateStream.
--
-- 'migrationTaskName', 'notifyMigrationTaskState_migrationTaskName' - Unique identifier that references the migration task. /Do not store
-- personal data in this field./
--
-- 'task', 'notifyMigrationTaskState_task' - Information about the task\'s progress and status.
--
-- 'updateDateTime', 'notifyMigrationTaskState_updateDateTime' - The timestamp when the task was gathered.
--
-- 'nextUpdateSeconds', 'notifyMigrationTaskState_nextUpdateSeconds' - Number of seconds after the UpdateDateTime within which the Migration
-- Hub can expect an update. If Migration Hub does not receive an update
-- within the specified interval, then the migration task will be
-- considered stale.
newNotifyMigrationTaskState ::
  -- | 'progressUpdateStream'
  Prelude.Text ->
  -- | 'migrationTaskName'
  Prelude.Text ->
  -- | 'task'
  Task ->
  -- | 'updateDateTime'
  Prelude.UTCTime ->
  -- | 'nextUpdateSeconds'
  Prelude.Natural ->
  NotifyMigrationTaskState
newNotifyMigrationTaskState
  pProgressUpdateStream_
  pMigrationTaskName_
  pTask_
  pUpdateDateTime_
  pNextUpdateSeconds_ =
    NotifyMigrationTaskState'
      { dryRun = Prelude.Nothing,
        progressUpdateStream = pProgressUpdateStream_,
        migrationTaskName = pMigrationTaskName_,
        task = pTask_,
        updateDateTime =
          Data._Time Lens.# pUpdateDateTime_,
        nextUpdateSeconds = pNextUpdateSeconds_
      }

-- | Optional boolean flag to indicate whether any effect should take place.
-- Used to test if the caller has permission to make the call.
notifyMigrationTaskState_dryRun :: Lens.Lens' NotifyMigrationTaskState (Prelude.Maybe Prelude.Bool)
notifyMigrationTaskState_dryRun = Lens.lens (\NotifyMigrationTaskState' {dryRun} -> dryRun) (\s@NotifyMigrationTaskState' {} a -> s {dryRun = a} :: NotifyMigrationTaskState)

-- | The name of the ProgressUpdateStream.
notifyMigrationTaskState_progressUpdateStream :: Lens.Lens' NotifyMigrationTaskState Prelude.Text
notifyMigrationTaskState_progressUpdateStream = Lens.lens (\NotifyMigrationTaskState' {progressUpdateStream} -> progressUpdateStream) (\s@NotifyMigrationTaskState' {} a -> s {progressUpdateStream = a} :: NotifyMigrationTaskState)

-- | Unique identifier that references the migration task. /Do not store
-- personal data in this field./
notifyMigrationTaskState_migrationTaskName :: Lens.Lens' NotifyMigrationTaskState Prelude.Text
notifyMigrationTaskState_migrationTaskName = Lens.lens (\NotifyMigrationTaskState' {migrationTaskName} -> migrationTaskName) (\s@NotifyMigrationTaskState' {} a -> s {migrationTaskName = a} :: NotifyMigrationTaskState)

-- | Information about the task\'s progress and status.
notifyMigrationTaskState_task :: Lens.Lens' NotifyMigrationTaskState Task
notifyMigrationTaskState_task = Lens.lens (\NotifyMigrationTaskState' {task} -> task) (\s@NotifyMigrationTaskState' {} a -> s {task = a} :: NotifyMigrationTaskState)

-- | The timestamp when the task was gathered.
notifyMigrationTaskState_updateDateTime :: Lens.Lens' NotifyMigrationTaskState Prelude.UTCTime
notifyMigrationTaskState_updateDateTime = Lens.lens (\NotifyMigrationTaskState' {updateDateTime} -> updateDateTime) (\s@NotifyMigrationTaskState' {} a -> s {updateDateTime = a} :: NotifyMigrationTaskState) Prelude.. Data._Time

-- | Number of seconds after the UpdateDateTime within which the Migration
-- Hub can expect an update. If Migration Hub does not receive an update
-- within the specified interval, then the migration task will be
-- considered stale.
notifyMigrationTaskState_nextUpdateSeconds :: Lens.Lens' NotifyMigrationTaskState Prelude.Natural
notifyMigrationTaskState_nextUpdateSeconds = Lens.lens (\NotifyMigrationTaskState' {nextUpdateSeconds} -> nextUpdateSeconds) (\s@NotifyMigrationTaskState' {} a -> s {nextUpdateSeconds = a} :: NotifyMigrationTaskState)

instance Core.AWSRequest NotifyMigrationTaskState where
  type
    AWSResponse NotifyMigrationTaskState =
      NotifyMigrationTaskStateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          NotifyMigrationTaskStateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable NotifyMigrationTaskState where
  hashWithSalt _salt NotifyMigrationTaskState' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` progressUpdateStream
      `Prelude.hashWithSalt` migrationTaskName
      `Prelude.hashWithSalt` task
      `Prelude.hashWithSalt` updateDateTime
      `Prelude.hashWithSalt` nextUpdateSeconds

instance Prelude.NFData NotifyMigrationTaskState where
  rnf NotifyMigrationTaskState' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf progressUpdateStream
      `Prelude.seq` Prelude.rnf migrationTaskName
      `Prelude.seq` Prelude.rnf task
      `Prelude.seq` Prelude.rnf updateDateTime
      `Prelude.seq` Prelude.rnf nextUpdateSeconds

instance Data.ToHeaders NotifyMigrationTaskState where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSMigrationHub.NotifyMigrationTaskState" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON NotifyMigrationTaskState where
  toJSON NotifyMigrationTaskState' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DryRun" Data..=) Prelude.<$> dryRun,
            Prelude.Just
              ( "ProgressUpdateStream"
                  Data..= progressUpdateStream
              ),
            Prelude.Just
              ("MigrationTaskName" Data..= migrationTaskName),
            Prelude.Just ("Task" Data..= task),
            Prelude.Just
              ("UpdateDateTime" Data..= updateDateTime),
            Prelude.Just
              ("NextUpdateSeconds" Data..= nextUpdateSeconds)
          ]
      )

instance Data.ToPath NotifyMigrationTaskState where
  toPath = Prelude.const "/"

instance Data.ToQuery NotifyMigrationTaskState where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newNotifyMigrationTaskStateResponse' smart constructor.
data NotifyMigrationTaskStateResponse = NotifyMigrationTaskStateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotifyMigrationTaskStateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'notifyMigrationTaskStateResponse_httpStatus' - The response's http status code.
newNotifyMigrationTaskStateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  NotifyMigrationTaskStateResponse
newNotifyMigrationTaskStateResponse pHttpStatus_ =
  NotifyMigrationTaskStateResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
notifyMigrationTaskStateResponse_httpStatus :: Lens.Lens' NotifyMigrationTaskStateResponse Prelude.Int
notifyMigrationTaskStateResponse_httpStatus = Lens.lens (\NotifyMigrationTaskStateResponse' {httpStatus} -> httpStatus) (\s@NotifyMigrationTaskStateResponse' {} a -> s {httpStatus = a} :: NotifyMigrationTaskStateResponse)

instance
  Prelude.NFData
    NotifyMigrationTaskStateResponse
  where
  rnf NotifyMigrationTaskStateResponse' {..} =
    Prelude.rnf httpStatus
