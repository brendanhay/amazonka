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
-- Module      : Amazonka.M2.CancelBatchJobExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the running of a specific batch job execution.
module Amazonka.M2.CancelBatchJobExecution
  ( -- * Creating a Request
    CancelBatchJobExecution (..),
    newCancelBatchJobExecution,

    -- * Request Lenses
    cancelBatchJobExecution_applicationId,
    cancelBatchJobExecution_executionId,

    -- * Destructuring the Response
    CancelBatchJobExecutionResponse (..),
    newCancelBatchJobExecutionResponse,

    -- * Response Lenses
    cancelBatchJobExecutionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelBatchJobExecution' smart constructor.
data CancelBatchJobExecution = CancelBatchJobExecution'
  { -- | The unique identifier of the application.
    applicationId :: Prelude.Text,
    -- | The unique identifier of the batch job execution.
    executionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelBatchJobExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'cancelBatchJobExecution_applicationId' - The unique identifier of the application.
--
-- 'executionId', 'cancelBatchJobExecution_executionId' - The unique identifier of the batch job execution.
newCancelBatchJobExecution ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'executionId'
  Prelude.Text ->
  CancelBatchJobExecution
newCancelBatchJobExecution
  pApplicationId_
  pExecutionId_ =
    CancelBatchJobExecution'
      { applicationId =
          pApplicationId_,
        executionId = pExecutionId_
      }

-- | The unique identifier of the application.
cancelBatchJobExecution_applicationId :: Lens.Lens' CancelBatchJobExecution Prelude.Text
cancelBatchJobExecution_applicationId = Lens.lens (\CancelBatchJobExecution' {applicationId} -> applicationId) (\s@CancelBatchJobExecution' {} a -> s {applicationId = a} :: CancelBatchJobExecution)

-- | The unique identifier of the batch job execution.
cancelBatchJobExecution_executionId :: Lens.Lens' CancelBatchJobExecution Prelude.Text
cancelBatchJobExecution_executionId = Lens.lens (\CancelBatchJobExecution' {executionId} -> executionId) (\s@CancelBatchJobExecution' {} a -> s {executionId = a} :: CancelBatchJobExecution)

instance Core.AWSRequest CancelBatchJobExecution where
  type
    AWSResponse CancelBatchJobExecution =
      CancelBatchJobExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelBatchJobExecutionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelBatchJobExecution where
  hashWithSalt _salt CancelBatchJobExecution' {..} =
    _salt
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` executionId

instance Prelude.NFData CancelBatchJobExecution where
  rnf CancelBatchJobExecution' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf executionId

instance Data.ToHeaders CancelBatchJobExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelBatchJobExecution where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath CancelBatchJobExecution where
  toPath CancelBatchJobExecution' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/batch-job-executions/",
        Data.toBS executionId,
        "/cancel"
      ]

instance Data.ToQuery CancelBatchJobExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelBatchJobExecutionResponse' smart constructor.
data CancelBatchJobExecutionResponse = CancelBatchJobExecutionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelBatchJobExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelBatchJobExecutionResponse_httpStatus' - The response's http status code.
newCancelBatchJobExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelBatchJobExecutionResponse
newCancelBatchJobExecutionResponse pHttpStatus_ =
  CancelBatchJobExecutionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
cancelBatchJobExecutionResponse_httpStatus :: Lens.Lens' CancelBatchJobExecutionResponse Prelude.Int
cancelBatchJobExecutionResponse_httpStatus = Lens.lens (\CancelBatchJobExecutionResponse' {httpStatus} -> httpStatus) (\s@CancelBatchJobExecutionResponse' {} a -> s {httpStatus = a} :: CancelBatchJobExecutionResponse)

instance
  Prelude.NFData
    CancelBatchJobExecutionResponse
  where
  rnf CancelBatchJobExecutionResponse' {..} =
    Prelude.rnf httpStatus
