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
-- Module      : Amazonka.DataPipeline.ReportTaskProgress
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Task runners call @ReportTaskProgress@ when assigned a task to
-- acknowledge that it has the task. If the web service does not receive
-- this acknowledgement within 2 minutes, it assigns the task in a
-- subsequent PollForTask call. After this initial acknowledgement, the
-- task runner only needs to report progress every 15 minutes to maintain
-- its ownership of the task. You can change this reporting time from 15
-- minutes by specifying a @reportProgressTimeout@ field in your pipeline.
--
-- If a task runner does not report its status after 5 minutes, AWS Data
-- Pipeline assumes that the task runner is unable to process the task and
-- reassigns the task in a subsequent response to PollForTask. Task runners
-- should call @ReportTaskProgress@ every 60 seconds.
module Amazonka.DataPipeline.ReportTaskProgress
  ( -- * Creating a Request
    ReportTaskProgress (..),
    newReportTaskProgress,

    -- * Request Lenses
    reportTaskProgress_fields,
    reportTaskProgress_taskId,

    -- * Destructuring the Response
    ReportTaskProgressResponse (..),
    newReportTaskProgressResponse,

    -- * Response Lenses
    reportTaskProgressResponse_httpStatus,
    reportTaskProgressResponse_canceled,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataPipeline.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for ReportTaskProgress.
--
-- /See:/ 'newReportTaskProgress' smart constructor.
data ReportTaskProgress = ReportTaskProgress'
  { -- | Key-value pairs that define the properties of the
    -- ReportTaskProgressInput object.
    fields :: Prelude.Maybe [Field],
    -- | The ID of the task assigned to the task runner. This value is provided
    -- in the response for PollForTask.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReportTaskProgress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fields', 'reportTaskProgress_fields' - Key-value pairs that define the properties of the
-- ReportTaskProgressInput object.
--
-- 'taskId', 'reportTaskProgress_taskId' - The ID of the task assigned to the task runner. This value is provided
-- in the response for PollForTask.
newReportTaskProgress ::
  -- | 'taskId'
  Prelude.Text ->
  ReportTaskProgress
newReportTaskProgress pTaskId_ =
  ReportTaskProgress'
    { fields = Prelude.Nothing,
      taskId = pTaskId_
    }

-- | Key-value pairs that define the properties of the
-- ReportTaskProgressInput object.
reportTaskProgress_fields :: Lens.Lens' ReportTaskProgress (Prelude.Maybe [Field])
reportTaskProgress_fields = Lens.lens (\ReportTaskProgress' {fields} -> fields) (\s@ReportTaskProgress' {} a -> s {fields = a} :: ReportTaskProgress) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the task assigned to the task runner. This value is provided
-- in the response for PollForTask.
reportTaskProgress_taskId :: Lens.Lens' ReportTaskProgress Prelude.Text
reportTaskProgress_taskId = Lens.lens (\ReportTaskProgress' {taskId} -> taskId) (\s@ReportTaskProgress' {} a -> s {taskId = a} :: ReportTaskProgress)

instance Core.AWSRequest ReportTaskProgress where
  type
    AWSResponse ReportTaskProgress =
      ReportTaskProgressResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ReportTaskProgressResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "canceled")
      )

instance Prelude.Hashable ReportTaskProgress where
  hashWithSalt _salt ReportTaskProgress' {..} =
    _salt `Prelude.hashWithSalt` fields
      `Prelude.hashWithSalt` taskId

instance Prelude.NFData ReportTaskProgress where
  rnf ReportTaskProgress' {..} =
    Prelude.rnf fields `Prelude.seq` Prelude.rnf taskId

instance Data.ToHeaders ReportTaskProgress where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DataPipeline.ReportTaskProgress" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ReportTaskProgress where
  toJSON ReportTaskProgress' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("fields" Data..=) Prelude.<$> fields,
            Prelude.Just ("taskId" Data..= taskId)
          ]
      )

instance Data.ToPath ReportTaskProgress where
  toPath = Prelude.const "/"

instance Data.ToQuery ReportTaskProgress where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of ReportTaskProgress.
--
-- /See:/ 'newReportTaskProgressResponse' smart constructor.
data ReportTaskProgressResponse = ReportTaskProgressResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | If true, the calling task runner should cancel processing of the task.
    -- The task runner does not need to call SetTaskStatus for canceled tasks.
    canceled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReportTaskProgressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'reportTaskProgressResponse_httpStatus' - The response's http status code.
--
-- 'canceled', 'reportTaskProgressResponse_canceled' - If true, the calling task runner should cancel processing of the task.
-- The task runner does not need to call SetTaskStatus for canceled tasks.
newReportTaskProgressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'canceled'
  Prelude.Bool ->
  ReportTaskProgressResponse
newReportTaskProgressResponse pHttpStatus_ pCanceled_ =
  ReportTaskProgressResponse'
    { httpStatus =
        pHttpStatus_,
      canceled = pCanceled_
    }

-- | The response's http status code.
reportTaskProgressResponse_httpStatus :: Lens.Lens' ReportTaskProgressResponse Prelude.Int
reportTaskProgressResponse_httpStatus = Lens.lens (\ReportTaskProgressResponse' {httpStatus} -> httpStatus) (\s@ReportTaskProgressResponse' {} a -> s {httpStatus = a} :: ReportTaskProgressResponse)

-- | If true, the calling task runner should cancel processing of the task.
-- The task runner does not need to call SetTaskStatus for canceled tasks.
reportTaskProgressResponse_canceled :: Lens.Lens' ReportTaskProgressResponse Prelude.Bool
reportTaskProgressResponse_canceled = Lens.lens (\ReportTaskProgressResponse' {canceled} -> canceled) (\s@ReportTaskProgressResponse' {} a -> s {canceled = a} :: ReportTaskProgressResponse)

instance Prelude.NFData ReportTaskProgressResponse where
  rnf ReportTaskProgressResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf canceled
