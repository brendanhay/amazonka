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
-- Module      : Network.AWS.MigrationHub.DescribeMigrationTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of all attributes associated with a specific migration
-- task.
module Network.AWS.MigrationHub.DescribeMigrationTask
  ( -- * Creating a Request
    DescribeMigrationTask (..),
    newDescribeMigrationTask,

    -- * Request Lenses
    describeMigrationTask_progressUpdateStream,
    describeMigrationTask_migrationTaskName,

    -- * Destructuring the Response
    DescribeMigrationTaskResponse (..),
    newDescribeMigrationTaskResponse,

    -- * Response Lenses
    describeMigrationTaskResponse_migrationTask,
    describeMigrationTaskResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeMigrationTask' smart constructor.
data DescribeMigrationTask = DescribeMigrationTask'
  { -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Prelude.Text,
    -- | The identifier given to the MigrationTask. /Do not store personal data
    -- in this field./
    migrationTaskName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeMigrationTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'progressUpdateStream', 'describeMigrationTask_progressUpdateStream' - The name of the ProgressUpdateStream.
--
-- 'migrationTaskName', 'describeMigrationTask_migrationTaskName' - The identifier given to the MigrationTask. /Do not store personal data
-- in this field./
newDescribeMigrationTask ::
  -- | 'progressUpdateStream'
  Prelude.Text ->
  -- | 'migrationTaskName'
  Prelude.Text ->
  DescribeMigrationTask
newDescribeMigrationTask
  pProgressUpdateStream_
  pMigrationTaskName_ =
    DescribeMigrationTask'
      { progressUpdateStream =
          pProgressUpdateStream_,
        migrationTaskName = pMigrationTaskName_
      }

-- | The name of the ProgressUpdateStream.
describeMigrationTask_progressUpdateStream :: Lens.Lens' DescribeMigrationTask Prelude.Text
describeMigrationTask_progressUpdateStream = Lens.lens (\DescribeMigrationTask' {progressUpdateStream} -> progressUpdateStream) (\s@DescribeMigrationTask' {} a -> s {progressUpdateStream = a} :: DescribeMigrationTask)

-- | The identifier given to the MigrationTask. /Do not store personal data
-- in this field./
describeMigrationTask_migrationTaskName :: Lens.Lens' DescribeMigrationTask Prelude.Text
describeMigrationTask_migrationTaskName = Lens.lens (\DescribeMigrationTask' {migrationTaskName} -> migrationTaskName) (\s@DescribeMigrationTask' {} a -> s {migrationTaskName = a} :: DescribeMigrationTask)

instance Prelude.AWSRequest DescribeMigrationTask where
  type
    Rs DescribeMigrationTask =
      DescribeMigrationTaskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMigrationTaskResponse'
            Prelude.<$> (x Prelude..?> "MigrationTask")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeMigrationTask

instance Prelude.NFData DescribeMigrationTask

instance Prelude.ToHeaders DescribeMigrationTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSMigrationHub.DescribeMigrationTask" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeMigrationTask where
  toJSON DescribeMigrationTask' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ProgressUpdateStream"
                  Prelude..= progressUpdateStream
              ),
            Prelude.Just
              ("MigrationTaskName" Prelude..= migrationTaskName)
          ]
      )

instance Prelude.ToPath DescribeMigrationTask where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeMigrationTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMigrationTaskResponse' smart constructor.
data DescribeMigrationTaskResponse = DescribeMigrationTaskResponse'
  { -- | Object encapsulating information about the migration task.
    migrationTask :: Prelude.Maybe MigrationTask,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeMigrationTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'migrationTask', 'describeMigrationTaskResponse_migrationTask' - Object encapsulating information about the migration task.
--
-- 'httpStatus', 'describeMigrationTaskResponse_httpStatus' - The response's http status code.
newDescribeMigrationTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeMigrationTaskResponse
newDescribeMigrationTaskResponse pHttpStatus_ =
  DescribeMigrationTaskResponse'
    { migrationTask =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Object encapsulating information about the migration task.
describeMigrationTaskResponse_migrationTask :: Lens.Lens' DescribeMigrationTaskResponse (Prelude.Maybe MigrationTask)
describeMigrationTaskResponse_migrationTask = Lens.lens (\DescribeMigrationTaskResponse' {migrationTask} -> migrationTask) (\s@DescribeMigrationTaskResponse' {} a -> s {migrationTask = a} :: DescribeMigrationTaskResponse)

-- | The response's http status code.
describeMigrationTaskResponse_httpStatus :: Lens.Lens' DescribeMigrationTaskResponse Prelude.Int
describeMigrationTaskResponse_httpStatus = Lens.lens (\DescribeMigrationTaskResponse' {httpStatus} -> httpStatus) (\s@DescribeMigrationTaskResponse' {} a -> s {httpStatus = a} :: DescribeMigrationTaskResponse)

instance Prelude.NFData DescribeMigrationTaskResponse
