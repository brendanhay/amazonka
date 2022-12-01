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
-- Module      : Amazonka.CloudWatchLogs.CancelExportTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified export task.
--
-- The task must be in the @PENDING@ or @RUNNING@ state.
module Amazonka.CloudWatchLogs.CancelExportTask
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

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelExportTask' smart constructor.
data CancelExportTask = CancelExportTask'
  { -- | The ID of the export task.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest CancelExportTask where
  type
    AWSResponse CancelExportTask =
      CancelExportTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull CancelExportTaskResponse'

instance Prelude.Hashable CancelExportTask where
  hashWithSalt _salt CancelExportTask' {..} =
    _salt `Prelude.hashWithSalt` taskId

instance Prelude.NFData CancelExportTask where
  rnf CancelExportTask' {..} = Prelude.rnf taskId

instance Core.ToHeaders CancelExportTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Logs_20140328.CancelExportTask" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CancelExportTask where
  toJSON CancelExportTask' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("taskId" Core..= taskId)]
      )

instance Core.ToPath CancelExportTask where
  toPath = Prelude.const "/"

instance Core.ToQuery CancelExportTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelExportTaskResponse' smart constructor.
data CancelExportTaskResponse = CancelExportTaskResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelExportTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCancelExportTaskResponse ::
  CancelExportTaskResponse
newCancelExportTaskResponse =
  CancelExportTaskResponse'

instance Prelude.NFData CancelExportTaskResponse where
  rnf _ = ()
