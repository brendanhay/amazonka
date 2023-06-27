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
-- Module      : Amazonka.TimeStreamWrite.ResumeBatchLoadTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.ResumeBatchLoadTask
  ( -- * Creating a Request
    ResumeBatchLoadTask (..),
    newResumeBatchLoadTask,

    -- * Request Lenses
    resumeBatchLoadTask_taskId,

    -- * Destructuring the Response
    ResumeBatchLoadTaskResponse (..),
    newResumeBatchLoadTaskResponse,

    -- * Response Lenses
    resumeBatchLoadTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TimeStreamWrite.Types

-- | /See:/ 'newResumeBatchLoadTask' smart constructor.
data ResumeBatchLoadTask = ResumeBatchLoadTask'
  { -- | The ID of the batch load task to resume.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResumeBatchLoadTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskId', 'resumeBatchLoadTask_taskId' - The ID of the batch load task to resume.
newResumeBatchLoadTask ::
  -- | 'taskId'
  Prelude.Text ->
  ResumeBatchLoadTask
newResumeBatchLoadTask pTaskId_ =
  ResumeBatchLoadTask' {taskId = pTaskId_}

-- | The ID of the batch load task to resume.
resumeBatchLoadTask_taskId :: Lens.Lens' ResumeBatchLoadTask Prelude.Text
resumeBatchLoadTask_taskId = Lens.lens (\ResumeBatchLoadTask' {taskId} -> taskId) (\s@ResumeBatchLoadTask' {} a -> s {taskId = a} :: ResumeBatchLoadTask)

instance Core.AWSRequest ResumeBatchLoadTask where
  type
    AWSResponse ResumeBatchLoadTask =
      ResumeBatchLoadTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ResumeBatchLoadTaskResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResumeBatchLoadTask where
  hashWithSalt _salt ResumeBatchLoadTask' {..} =
    _salt `Prelude.hashWithSalt` taskId

instance Prelude.NFData ResumeBatchLoadTask where
  rnf ResumeBatchLoadTask' {..} = Prelude.rnf taskId

instance Data.ToHeaders ResumeBatchLoadTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Timestream_20181101.ResumeBatchLoadTask" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ResumeBatchLoadTask where
  toJSON ResumeBatchLoadTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("TaskId" Data..= taskId)]
      )

instance Data.ToPath ResumeBatchLoadTask where
  toPath = Prelude.const "/"

instance Data.ToQuery ResumeBatchLoadTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResumeBatchLoadTaskResponse' smart constructor.
data ResumeBatchLoadTaskResponse = ResumeBatchLoadTaskResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResumeBatchLoadTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'resumeBatchLoadTaskResponse_httpStatus' - The response's http status code.
newResumeBatchLoadTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResumeBatchLoadTaskResponse
newResumeBatchLoadTaskResponse pHttpStatus_ =
  ResumeBatchLoadTaskResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
resumeBatchLoadTaskResponse_httpStatus :: Lens.Lens' ResumeBatchLoadTaskResponse Prelude.Int
resumeBatchLoadTaskResponse_httpStatus = Lens.lens (\ResumeBatchLoadTaskResponse' {httpStatus} -> httpStatus) (\s@ResumeBatchLoadTaskResponse' {} a -> s {httpStatus = a} :: ResumeBatchLoadTaskResponse)

instance Prelude.NFData ResumeBatchLoadTaskResponse where
  rnf ResumeBatchLoadTaskResponse' {..} =
    Prelude.rnf httpStatus
