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
-- Module      : Amazonka.M2.StartBatchJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a batch job and returns the unique identifier of this execution
-- of the batch job. The associated application must be running in order to
-- start the batch job.
module Amazonka.M2.StartBatchJob
  ( -- * Creating a Request
    StartBatchJob (..),
    newStartBatchJob,

    -- * Request Lenses
    startBatchJob_jobParams,
    startBatchJob_applicationId,
    startBatchJob_batchJobIdentifier,

    -- * Destructuring the Response
    StartBatchJobResponse (..),
    newStartBatchJobResponse,

    -- * Response Lenses
    startBatchJobResponse_httpStatus,
    startBatchJobResponse_executionId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartBatchJob' smart constructor.
data StartBatchJob = StartBatchJob'
  { -- | The collection of batch job parameters. For details about limits for
    -- keys and values, see
    -- <https://www.ibm.com/docs/en/workload-automation/9.3.0?topic=zos-coding-variables-in-jcl Coding variables in JCL>.
    jobParams :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The unique identifier of the application associated with this batch job.
    applicationId :: Prelude.Text,
    -- | The unique identifier of the batch job.
    batchJobIdentifier :: BatchJobIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartBatchJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobParams', 'startBatchJob_jobParams' - The collection of batch job parameters. For details about limits for
-- keys and values, see
-- <https://www.ibm.com/docs/en/workload-automation/9.3.0?topic=zos-coding-variables-in-jcl Coding variables in JCL>.
--
-- 'applicationId', 'startBatchJob_applicationId' - The unique identifier of the application associated with this batch job.
--
-- 'batchJobIdentifier', 'startBatchJob_batchJobIdentifier' - The unique identifier of the batch job.
newStartBatchJob ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'batchJobIdentifier'
  BatchJobIdentifier ->
  StartBatchJob
newStartBatchJob pApplicationId_ pBatchJobIdentifier_ =
  StartBatchJob'
    { jobParams = Prelude.Nothing,
      applicationId = pApplicationId_,
      batchJobIdentifier = pBatchJobIdentifier_
    }

-- | The collection of batch job parameters. For details about limits for
-- keys and values, see
-- <https://www.ibm.com/docs/en/workload-automation/9.3.0?topic=zos-coding-variables-in-jcl Coding variables in JCL>.
startBatchJob_jobParams :: Lens.Lens' StartBatchJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startBatchJob_jobParams = Lens.lens (\StartBatchJob' {jobParams} -> jobParams) (\s@StartBatchJob' {} a -> s {jobParams = a} :: StartBatchJob) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier of the application associated with this batch job.
startBatchJob_applicationId :: Lens.Lens' StartBatchJob Prelude.Text
startBatchJob_applicationId = Lens.lens (\StartBatchJob' {applicationId} -> applicationId) (\s@StartBatchJob' {} a -> s {applicationId = a} :: StartBatchJob)

-- | The unique identifier of the batch job.
startBatchJob_batchJobIdentifier :: Lens.Lens' StartBatchJob BatchJobIdentifier
startBatchJob_batchJobIdentifier = Lens.lens (\StartBatchJob' {batchJobIdentifier} -> batchJobIdentifier) (\s@StartBatchJob' {} a -> s {batchJobIdentifier = a} :: StartBatchJob)

instance Core.AWSRequest StartBatchJob where
  type
    AWSResponse StartBatchJob =
      StartBatchJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartBatchJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "executionId")
      )

instance Prelude.Hashable StartBatchJob where
  hashWithSalt _salt StartBatchJob' {..} =
    _salt `Prelude.hashWithSalt` jobParams
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` batchJobIdentifier

instance Prelude.NFData StartBatchJob where
  rnf StartBatchJob' {..} =
    Prelude.rnf jobParams
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf batchJobIdentifier

instance Data.ToHeaders StartBatchJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartBatchJob where
  toJSON StartBatchJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("jobParams" Data..=) Prelude.<$> jobParams,
            Prelude.Just
              ("batchJobIdentifier" Data..= batchJobIdentifier)
          ]
      )

instance Data.ToPath StartBatchJob where
  toPath StartBatchJob' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/batch-job"
      ]

instance Data.ToQuery StartBatchJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartBatchJobResponse' smart constructor.
data StartBatchJobResponse = StartBatchJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique identifier of this execution of the batch job.
    executionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartBatchJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startBatchJobResponse_httpStatus' - The response's http status code.
--
-- 'executionId', 'startBatchJobResponse_executionId' - The unique identifier of this execution of the batch job.
newStartBatchJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'executionId'
  Prelude.Text ->
  StartBatchJobResponse
newStartBatchJobResponse pHttpStatus_ pExecutionId_ =
  StartBatchJobResponse'
    { httpStatus = pHttpStatus_,
      executionId = pExecutionId_
    }

-- | The response's http status code.
startBatchJobResponse_httpStatus :: Lens.Lens' StartBatchJobResponse Prelude.Int
startBatchJobResponse_httpStatus = Lens.lens (\StartBatchJobResponse' {httpStatus} -> httpStatus) (\s@StartBatchJobResponse' {} a -> s {httpStatus = a} :: StartBatchJobResponse)

-- | The unique identifier of this execution of the batch job.
startBatchJobResponse_executionId :: Lens.Lens' StartBatchJobResponse Prelude.Text
startBatchJobResponse_executionId = Lens.lens (\StartBatchJobResponse' {executionId} -> executionId) (\s@StartBatchJobResponse' {} a -> s {executionId = a} :: StartBatchJobResponse)

instance Prelude.NFData StartBatchJobResponse where
  rnf StartBatchJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf executionId
