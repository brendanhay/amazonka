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
-- Module      : Network.AWS.EC2.CancelExportTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an active export task. The request removes all artifacts of the
-- export, including any partially-created Amazon S3 objects. If the export
-- task is complete or is in the process of transferring the final disk
-- image, the command fails and returns an error.
module Network.AWS.EC2.CancelExportTask
  ( -- * Creating a Request
    CancelExportTask (..),
    newCancelExportTask,

    -- * Request Lenses
    cancelExportTask_exportTaskId,

    -- * Destructuring the Response
    CancelExportTaskResponse (..),
    newCancelExportTaskResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCancelExportTask' smart constructor.
data CancelExportTask = CancelExportTask'
  { -- | The ID of the export task. This is the ID returned by
    -- @CreateInstanceExportTask@.
    exportTaskId :: Core.Text
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
-- 'exportTaskId', 'cancelExportTask_exportTaskId' - The ID of the export task. This is the ID returned by
-- @CreateInstanceExportTask@.
newCancelExportTask ::
  -- | 'exportTaskId'
  Core.Text ->
  CancelExportTask
newCancelExportTask pExportTaskId_ =
  CancelExportTask' {exportTaskId = pExportTaskId_}

-- | The ID of the export task. This is the ID returned by
-- @CreateInstanceExportTask@.
cancelExportTask_exportTaskId :: Lens.Lens' CancelExportTask Core.Text
cancelExportTask_exportTaskId = Lens.lens (\CancelExportTask' {exportTaskId} -> exportTaskId) (\s@CancelExportTask' {} a -> s {exportTaskId = a} :: CancelExportTask)

instance Core.AWSRequest CancelExportTask where
  type
    AWSResponse CancelExportTask =
      CancelExportTaskResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull CancelExportTaskResponse'

instance Core.Hashable CancelExportTask

instance Core.NFData CancelExportTask

instance Core.ToHeaders CancelExportTask where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CancelExportTask where
  toPath = Core.const "/"

instance Core.ToQuery CancelExportTask where
  toQuery CancelExportTask' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CancelExportTask" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "ExportTaskId" Core.=: exportTaskId
      ]

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
