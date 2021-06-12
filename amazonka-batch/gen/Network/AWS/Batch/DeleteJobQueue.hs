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
-- Module      : Network.AWS.Batch.DeleteJobQueue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified job queue. You must first disable submissions for
-- a queue with the UpdateJobQueue operation. All jobs in the queue are
-- eventually terminated when you delete a job queue. The jobs are
-- terminated at a rate of about 16 jobs each second.
--
-- It\'s not necessary to disassociate compute environments from a queue
-- before submitting a @DeleteJobQueue@ request.
module Network.AWS.Batch.DeleteJobQueue
  ( -- * Creating a Request
    DeleteJobQueue (..),
    newDeleteJobQueue,

    -- * Request Lenses
    deleteJobQueue_jobQueue,

    -- * Destructuring the Response
    DeleteJobQueueResponse (..),
    newDeleteJobQueueResponse,

    -- * Response Lenses
    deleteJobQueueResponse_httpStatus,
  )
where

import Network.AWS.Batch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for @DeleteJobQueue@.
--
-- /See:/ 'newDeleteJobQueue' smart constructor.
data DeleteJobQueue = DeleteJobQueue'
  { -- | The short name or full Amazon Resource Name (ARN) of the queue to
    -- delete.
    jobQueue :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteJobQueue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobQueue', 'deleteJobQueue_jobQueue' - The short name or full Amazon Resource Name (ARN) of the queue to
-- delete.
newDeleteJobQueue ::
  -- | 'jobQueue'
  Core.Text ->
  DeleteJobQueue
newDeleteJobQueue pJobQueue_ =
  DeleteJobQueue' {jobQueue = pJobQueue_}

-- | The short name or full Amazon Resource Name (ARN) of the queue to
-- delete.
deleteJobQueue_jobQueue :: Lens.Lens' DeleteJobQueue Core.Text
deleteJobQueue_jobQueue = Lens.lens (\DeleteJobQueue' {jobQueue} -> jobQueue) (\s@DeleteJobQueue' {} a -> s {jobQueue = a} :: DeleteJobQueue)

instance Core.AWSRequest DeleteJobQueue where
  type
    AWSResponse DeleteJobQueue =
      DeleteJobQueueResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteJobQueueResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteJobQueue

instance Core.NFData DeleteJobQueue

instance Core.ToHeaders DeleteJobQueue where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteJobQueue where
  toJSON DeleteJobQueue' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("jobQueue" Core..= jobQueue)]
      )

instance Core.ToPath DeleteJobQueue where
  toPath = Core.const "/v1/deletejobqueue"

instance Core.ToQuery DeleteJobQueue where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteJobQueueResponse' smart constructor.
data DeleteJobQueueResponse = DeleteJobQueueResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteJobQueueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteJobQueueResponse_httpStatus' - The response's http status code.
newDeleteJobQueueResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteJobQueueResponse
newDeleteJobQueueResponse pHttpStatus_ =
  DeleteJobQueueResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteJobQueueResponse_httpStatus :: Lens.Lens' DeleteJobQueueResponse Core.Int
deleteJobQueueResponse_httpStatus = Lens.lens (\DeleteJobQueueResponse' {httpStatus} -> httpStatus) (\s@DeleteJobQueueResponse' {} a -> s {httpStatus = a} :: DeleteJobQueueResponse)

instance Core.NFData DeleteJobQueueResponse
