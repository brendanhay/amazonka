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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for @DeleteJobQueue@.
--
-- /See:/ 'newDeleteJobQueue' smart constructor.
data DeleteJobQueue = DeleteJobQueue'
  { -- | The short name or full Amazon Resource Name (ARN) of the queue to
    -- delete.
    jobQueue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteJobQueue
newDeleteJobQueue pJobQueue_ =
  DeleteJobQueue' {jobQueue = pJobQueue_}

-- | The short name or full Amazon Resource Name (ARN) of the queue to
-- delete.
deleteJobQueue_jobQueue :: Lens.Lens' DeleteJobQueue Prelude.Text
deleteJobQueue_jobQueue = Lens.lens (\DeleteJobQueue' {jobQueue} -> jobQueue) (\s@DeleteJobQueue' {} a -> s {jobQueue = a} :: DeleteJobQueue)

instance Prelude.AWSRequest DeleteJobQueue where
  type Rs DeleteJobQueue = DeleteJobQueueResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteJobQueueResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteJobQueue

instance Prelude.NFData DeleteJobQueue

instance Prelude.ToHeaders DeleteJobQueue where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteJobQueue where
  toJSON DeleteJobQueue' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("jobQueue" Prelude..= jobQueue)]
      )

instance Prelude.ToPath DeleteJobQueue where
  toPath = Prelude.const "/v1/deletejobqueue"

instance Prelude.ToQuery DeleteJobQueue where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteJobQueueResponse' smart constructor.
data DeleteJobQueueResponse = DeleteJobQueueResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteJobQueueResponse
newDeleteJobQueueResponse pHttpStatus_ =
  DeleteJobQueueResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteJobQueueResponse_httpStatus :: Lens.Lens' DeleteJobQueueResponse Prelude.Int
deleteJobQueueResponse_httpStatus = Lens.lens (\DeleteJobQueueResponse' {httpStatus} -> httpStatus) (\s@DeleteJobQueueResponse' {} a -> s {httpStatus = a} :: DeleteJobQueueResponse)

instance Prelude.NFData DeleteJobQueueResponse
