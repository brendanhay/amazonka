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
-- Module      : Amazonka.Batch.DeleteJobQueue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.Batch.DeleteJobQueue
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

import Amazonka.Batch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for @DeleteJobQueue@.
--
-- /See:/ 'newDeleteJobQueue' smart constructor.
data DeleteJobQueue = DeleteJobQueue'
  { -- | The short name or full Amazon Resource Name (ARN) of the queue to
    -- delete.
    jobQueue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteJobQueue where
  type
    AWSResponse DeleteJobQueue =
      DeleteJobQueueResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteJobQueueResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteJobQueue where
  hashWithSalt _salt DeleteJobQueue' {..} =
    _salt `Prelude.hashWithSalt` jobQueue

instance Prelude.NFData DeleteJobQueue where
  rnf DeleteJobQueue' {..} = Prelude.rnf jobQueue

instance Data.ToHeaders DeleteJobQueue where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteJobQueue where
  toJSON DeleteJobQueue' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("jobQueue" Data..= jobQueue)]
      )

instance Data.ToPath DeleteJobQueue where
  toPath = Prelude.const "/v1/deletejobqueue"

instance Data.ToQuery DeleteJobQueue where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteJobQueueResponse' smart constructor.
data DeleteJobQueueResponse = DeleteJobQueueResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData DeleteJobQueueResponse where
  rnf DeleteJobQueueResponse' {..} =
    Prelude.rnf httpStatus
