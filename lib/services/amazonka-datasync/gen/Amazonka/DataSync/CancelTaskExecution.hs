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
-- Module      : Amazonka.DataSync.CancelTaskExecution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an DataSync task execution that\'s in progress. The transfer of
-- some files are abruptly interrupted. File contents that\'re transferred
-- to the destination might be incomplete or inconsistent with the source
-- files.
--
-- However, if you start a new task execution using the same task and allow
-- it to finish, file content on the destination will be complete and
-- consistent. This applies to other unexpected failures that interrupt a
-- task execution. In all of these cases, DataSync successfully completes
-- the transfer when you start the next task execution.
module Amazonka.DataSync.CancelTaskExecution
  ( -- * Creating a Request
    CancelTaskExecution (..),
    newCancelTaskExecution,

    -- * Request Lenses
    cancelTaskExecution_taskExecutionArn,

    -- * Destructuring the Response
    CancelTaskExecutionResponse (..),
    newCancelTaskExecutionResponse,

    -- * Response Lenses
    cancelTaskExecutionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | CancelTaskExecutionRequest
--
-- /See:/ 'newCancelTaskExecution' smart constructor.
data CancelTaskExecution = CancelTaskExecution'
  { -- | The Amazon Resource Name (ARN) of the task execution to stop.
    taskExecutionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelTaskExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskExecutionArn', 'cancelTaskExecution_taskExecutionArn' - The Amazon Resource Name (ARN) of the task execution to stop.
newCancelTaskExecution ::
  -- | 'taskExecutionArn'
  Prelude.Text ->
  CancelTaskExecution
newCancelTaskExecution pTaskExecutionArn_ =
  CancelTaskExecution'
    { taskExecutionArn =
        pTaskExecutionArn_
    }

-- | The Amazon Resource Name (ARN) of the task execution to stop.
cancelTaskExecution_taskExecutionArn :: Lens.Lens' CancelTaskExecution Prelude.Text
cancelTaskExecution_taskExecutionArn = Lens.lens (\CancelTaskExecution' {taskExecutionArn} -> taskExecutionArn) (\s@CancelTaskExecution' {} a -> s {taskExecutionArn = a} :: CancelTaskExecution)

instance Core.AWSRequest CancelTaskExecution where
  type
    AWSResponse CancelTaskExecution =
      CancelTaskExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelTaskExecutionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelTaskExecution where
  hashWithSalt _salt CancelTaskExecution' {..} =
    _salt `Prelude.hashWithSalt` taskExecutionArn

instance Prelude.NFData CancelTaskExecution where
  rnf CancelTaskExecution' {..} =
    Prelude.rnf taskExecutionArn

instance Data.ToHeaders CancelTaskExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.CancelTaskExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelTaskExecution where
  toJSON CancelTaskExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TaskExecutionArn" Data..= taskExecutionArn)
          ]
      )

instance Data.ToPath CancelTaskExecution where
  toPath = Prelude.const "/"

instance Data.ToQuery CancelTaskExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelTaskExecutionResponse' smart constructor.
data CancelTaskExecutionResponse = CancelTaskExecutionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelTaskExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelTaskExecutionResponse_httpStatus' - The response's http status code.
newCancelTaskExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelTaskExecutionResponse
newCancelTaskExecutionResponse pHttpStatus_ =
  CancelTaskExecutionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
cancelTaskExecutionResponse_httpStatus :: Lens.Lens' CancelTaskExecutionResponse Prelude.Int
cancelTaskExecutionResponse_httpStatus = Lens.lens (\CancelTaskExecutionResponse' {httpStatus} -> httpStatus) (\s@CancelTaskExecutionResponse' {} a -> s {httpStatus = a} :: CancelTaskExecutionResponse)

instance Prelude.NFData CancelTaskExecutionResponse where
  rnf CancelTaskExecutionResponse' {..} =
    Prelude.rnf httpStatus
