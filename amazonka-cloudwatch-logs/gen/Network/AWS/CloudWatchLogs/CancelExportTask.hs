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
-- Module      : Network.AWS.CloudWatchLogs.CancelExportTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified export task.
--
-- The task must be in the @PENDING@ or @RUNNING@ state.
module Network.AWS.CloudWatchLogs.CancelExportTask
  ( -- * Creating a Request
    CancelExportTask (..),
    newCancelExportTask,

    -- * Request Lenses
    cancelExportTask_taskId,

    -- * Destructuring the Response
    CancelExportTaskResponse (..),
    newCancelExportTaskResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCancelExportTask' smart constructor.
data CancelExportTask = CancelExportTask'
  { -- | The ID of the export task.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelExportTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskId', 'cancelExportTask_taskId' - The ID of the export task.
newCancelExportTask ::
  -- | 'taskId'
  Prelude.Text ->
  CancelExportTask
newCancelExportTask pTaskId_ =
  CancelExportTask' {taskId = pTaskId_}

-- | The ID of the export task.
cancelExportTask_taskId :: Lens.Lens' CancelExportTask Prelude.Text
cancelExportTask_taskId = Lens.lens (\CancelExportTask' {taskId} -> taskId) (\s@CancelExportTask' {} a -> s {taskId = a} :: CancelExportTask)

instance Prelude.AWSRequest CancelExportTask where
  type Rs CancelExportTask = CancelExportTaskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull CancelExportTaskResponse'

instance Prelude.Hashable CancelExportTask

instance Prelude.NFData CancelExportTask

instance Prelude.ToHeaders CancelExportTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Logs_20140328.CancelExportTask" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CancelExportTask where
  toJSON CancelExportTask' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("taskId" Prelude..= taskId)]
      )

instance Prelude.ToPath CancelExportTask where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CancelExportTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelExportTaskResponse' smart constructor.
data CancelExportTaskResponse = CancelExportTaskResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelExportTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCancelExportTaskResponse ::
  CancelExportTaskResponse
newCancelExportTaskResponse =
  CancelExportTaskResponse'

instance Prelude.NFData CancelExportTaskResponse
