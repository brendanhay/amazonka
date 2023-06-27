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
-- Module      : Amazonka.TimeStreamWrite.DescribeBatchLoadTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the batch load task, including configurations,
-- mappings, progress, and other details.
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/ts-limits.html Service quotas apply>.
-- See
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/code-samples.describe-batch-load.html code sample>
-- for details.
module Amazonka.TimeStreamWrite.DescribeBatchLoadTask
  ( -- * Creating a Request
    DescribeBatchLoadTask (..),
    newDescribeBatchLoadTask,

    -- * Request Lenses
    describeBatchLoadTask_taskId,

    -- * Destructuring the Response
    DescribeBatchLoadTaskResponse (..),
    newDescribeBatchLoadTaskResponse,

    -- * Response Lenses
    describeBatchLoadTaskResponse_httpStatus,
    describeBatchLoadTaskResponse_batchLoadTaskDescription,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TimeStreamWrite.Types

-- | /See:/ 'newDescribeBatchLoadTask' smart constructor.
data DescribeBatchLoadTask = DescribeBatchLoadTask'
  { -- | The ID of the batch load task.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBatchLoadTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskId', 'describeBatchLoadTask_taskId' - The ID of the batch load task.
newDescribeBatchLoadTask ::
  -- | 'taskId'
  Prelude.Text ->
  DescribeBatchLoadTask
newDescribeBatchLoadTask pTaskId_ =
  DescribeBatchLoadTask' {taskId = pTaskId_}

-- | The ID of the batch load task.
describeBatchLoadTask_taskId :: Lens.Lens' DescribeBatchLoadTask Prelude.Text
describeBatchLoadTask_taskId = Lens.lens (\DescribeBatchLoadTask' {taskId} -> taskId) (\s@DescribeBatchLoadTask' {} a -> s {taskId = a} :: DescribeBatchLoadTask)

instance Core.AWSRequest DescribeBatchLoadTask where
  type
    AWSResponse DescribeBatchLoadTask =
      DescribeBatchLoadTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBatchLoadTaskResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "BatchLoadTaskDescription")
      )

instance Prelude.Hashable DescribeBatchLoadTask where
  hashWithSalt _salt DescribeBatchLoadTask' {..} =
    _salt `Prelude.hashWithSalt` taskId

instance Prelude.NFData DescribeBatchLoadTask where
  rnf DescribeBatchLoadTask' {..} = Prelude.rnf taskId

instance Data.ToHeaders DescribeBatchLoadTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Timestream_20181101.DescribeBatchLoadTask" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeBatchLoadTask where
  toJSON DescribeBatchLoadTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("TaskId" Data..= taskId)]
      )

instance Data.ToPath DescribeBatchLoadTask where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeBatchLoadTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeBatchLoadTaskResponse' smart constructor.
data DescribeBatchLoadTaskResponse = DescribeBatchLoadTaskResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Description of the batch load task.
    batchLoadTaskDescription :: BatchLoadTaskDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBatchLoadTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeBatchLoadTaskResponse_httpStatus' - The response's http status code.
--
-- 'batchLoadTaskDescription', 'describeBatchLoadTaskResponse_batchLoadTaskDescription' - Description of the batch load task.
newDescribeBatchLoadTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'batchLoadTaskDescription'
  BatchLoadTaskDescription ->
  DescribeBatchLoadTaskResponse
newDescribeBatchLoadTaskResponse
  pHttpStatus_
  pBatchLoadTaskDescription_ =
    DescribeBatchLoadTaskResponse'
      { httpStatus =
          pHttpStatus_,
        batchLoadTaskDescription =
          pBatchLoadTaskDescription_
      }

-- | The response's http status code.
describeBatchLoadTaskResponse_httpStatus :: Lens.Lens' DescribeBatchLoadTaskResponse Prelude.Int
describeBatchLoadTaskResponse_httpStatus = Lens.lens (\DescribeBatchLoadTaskResponse' {httpStatus} -> httpStatus) (\s@DescribeBatchLoadTaskResponse' {} a -> s {httpStatus = a} :: DescribeBatchLoadTaskResponse)

-- | Description of the batch load task.
describeBatchLoadTaskResponse_batchLoadTaskDescription :: Lens.Lens' DescribeBatchLoadTaskResponse BatchLoadTaskDescription
describeBatchLoadTaskResponse_batchLoadTaskDescription = Lens.lens (\DescribeBatchLoadTaskResponse' {batchLoadTaskDescription} -> batchLoadTaskDescription) (\s@DescribeBatchLoadTaskResponse' {} a -> s {batchLoadTaskDescription = a} :: DescribeBatchLoadTaskResponse)

instance Prelude.NFData DescribeBatchLoadTaskResponse where
  rnf DescribeBatchLoadTaskResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf batchLoadTaskDescription
