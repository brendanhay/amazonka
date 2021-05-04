{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MigrationHub.ImportMigrationTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a new migration task which represents a server, database,
-- etc., being migrated to AWS by a migration tool.
--
-- This API is a prerequisite to calling the @NotifyMigrationTaskState@ API
-- as the migration tool must first register the migration task with
-- Migration Hub.
module Network.AWS.MigrationHub.ImportMigrationTask
  ( -- * Creating a Request
    ImportMigrationTask (..),
    newImportMigrationTask,

    -- * Request Lenses
    importMigrationTask_dryRun,
    importMigrationTask_progressUpdateStream,
    importMigrationTask_migrationTaskName,

    -- * Destructuring the Response
    ImportMigrationTaskResponse (..),
    newImportMigrationTaskResponse,

    -- * Response Lenses
    importMigrationTaskResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newImportMigrationTask' smart constructor.
data ImportMigrationTask = ImportMigrationTask'
  { -- | Optional boolean flag to indicate whether any effect should take place.
    -- Used to test if the caller has permission to make the call.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The name of the ProgressUpdateStream. >
    progressUpdateStream :: Prelude.Text,
    -- | Unique identifier that references the migration task. /Do not store
    -- personal data in this field./
    migrationTaskName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ImportMigrationTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'importMigrationTask_dryRun' - Optional boolean flag to indicate whether any effect should take place.
-- Used to test if the caller has permission to make the call.
--
-- 'progressUpdateStream', 'importMigrationTask_progressUpdateStream' - The name of the ProgressUpdateStream. >
--
-- 'migrationTaskName', 'importMigrationTask_migrationTaskName' - Unique identifier that references the migration task. /Do not store
-- personal data in this field./
newImportMigrationTask ::
  -- | 'progressUpdateStream'
  Prelude.Text ->
  -- | 'migrationTaskName'
  Prelude.Text ->
  ImportMigrationTask
newImportMigrationTask
  pProgressUpdateStream_
  pMigrationTaskName_ =
    ImportMigrationTask'
      { dryRun = Prelude.Nothing,
        progressUpdateStream = pProgressUpdateStream_,
        migrationTaskName = pMigrationTaskName_
      }

-- | Optional boolean flag to indicate whether any effect should take place.
-- Used to test if the caller has permission to make the call.
importMigrationTask_dryRun :: Lens.Lens' ImportMigrationTask (Prelude.Maybe Prelude.Bool)
importMigrationTask_dryRun = Lens.lens (\ImportMigrationTask' {dryRun} -> dryRun) (\s@ImportMigrationTask' {} a -> s {dryRun = a} :: ImportMigrationTask)

-- | The name of the ProgressUpdateStream. >
importMigrationTask_progressUpdateStream :: Lens.Lens' ImportMigrationTask Prelude.Text
importMigrationTask_progressUpdateStream = Lens.lens (\ImportMigrationTask' {progressUpdateStream} -> progressUpdateStream) (\s@ImportMigrationTask' {} a -> s {progressUpdateStream = a} :: ImportMigrationTask)

-- | Unique identifier that references the migration task. /Do not store
-- personal data in this field./
importMigrationTask_migrationTaskName :: Lens.Lens' ImportMigrationTask Prelude.Text
importMigrationTask_migrationTaskName = Lens.lens (\ImportMigrationTask' {migrationTaskName} -> migrationTaskName) (\s@ImportMigrationTask' {} a -> s {migrationTaskName = a} :: ImportMigrationTask)

instance Prelude.AWSRequest ImportMigrationTask where
  type
    Rs ImportMigrationTask =
      ImportMigrationTaskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ImportMigrationTaskResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportMigrationTask

instance Prelude.NFData ImportMigrationTask

instance Prelude.ToHeaders ImportMigrationTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSMigrationHub.ImportMigrationTask" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ImportMigrationTask where
  toJSON ImportMigrationTask' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DryRun" Prelude..=) Prelude.<$> dryRun,
            Prelude.Just
              ( "ProgressUpdateStream"
                  Prelude..= progressUpdateStream
              ),
            Prelude.Just
              ("MigrationTaskName" Prelude..= migrationTaskName)
          ]
      )

instance Prelude.ToPath ImportMigrationTask where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ImportMigrationTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportMigrationTaskResponse' smart constructor.
data ImportMigrationTaskResponse = ImportMigrationTaskResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ImportMigrationTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'importMigrationTaskResponse_httpStatus' - The response's http status code.
newImportMigrationTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportMigrationTaskResponse
newImportMigrationTaskResponse pHttpStatus_ =
  ImportMigrationTaskResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
importMigrationTaskResponse_httpStatus :: Lens.Lens' ImportMigrationTaskResponse Prelude.Int
importMigrationTaskResponse_httpStatus = Lens.lens (\ImportMigrationTaskResponse' {httpStatus} -> httpStatus) (\s@ImportMigrationTaskResponse' {} a -> s {httpStatus = a} :: ImportMigrationTaskResponse)

instance Prelude.NFData ImportMigrationTaskResponse
