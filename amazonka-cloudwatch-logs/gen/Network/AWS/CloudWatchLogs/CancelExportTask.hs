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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCancelExportTask' smart constructor.
data CancelExportTask = CancelExportTask'
  { -- | The ID of the export task.
    taskId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  CancelExportTask
newCancelExportTask pTaskId_ =
  CancelExportTask' {taskId = pTaskId_}

-- | The ID of the export task.
cancelExportTask_taskId :: Lens.Lens' CancelExportTask Core.Text
cancelExportTask_taskId = Lens.lens (\CancelExportTask' {taskId} -> taskId) (\s@CancelExportTask' {} a -> s {taskId = a} :: CancelExportTask)

instance Core.AWSRequest CancelExportTask where
  type
    AWSResponse CancelExportTask =
      CancelExportTaskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull CancelExportTaskResponse'

instance Core.Hashable CancelExportTask

instance Core.NFData CancelExportTask

instance Core.ToHeaders CancelExportTask where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Logs_20140328.CancelExportTask" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CancelExportTask where
  toJSON CancelExportTask' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("taskId" Core..= taskId)]
      )

instance Core.ToPath CancelExportTask where
  toPath = Core.const "/"

instance Core.ToQuery CancelExportTask where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCancelExportTaskResponse' smart constructor.
data CancelExportTaskResponse = CancelExportTaskResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CancelExportTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCancelExportTaskResponse ::
  CancelExportTaskResponse
newCancelExportTaskResponse =
  CancelExportTaskResponse'

instance Core.NFData CancelExportTaskResponse
