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
-- Module      : Amazonka.EMRServerless.CancelJobRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a job run.
module Amazonka.EMRServerless.CancelJobRun
  ( -- * Creating a Request
    CancelJobRun (..),
    newCancelJobRun,

    -- * Request Lenses
    cancelJobRun_applicationId,
    cancelJobRun_jobRunId,

    -- * Destructuring the Response
    CancelJobRunResponse (..),
    newCancelJobRunResponse,

    -- * Response Lenses
    cancelJobRunResponse_httpStatus,
    cancelJobRunResponse_applicationId,
    cancelJobRunResponse_jobRunId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelJobRun' smart constructor.
data CancelJobRun = CancelJobRun'
  { -- | The ID of the application on which the job run will be canceled.
    applicationId :: Prelude.Text,
    -- | The ID of the job run to cancel.
    jobRunId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelJobRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'cancelJobRun_applicationId' - The ID of the application on which the job run will be canceled.
--
-- 'jobRunId', 'cancelJobRun_jobRunId' - The ID of the job run to cancel.
newCancelJobRun ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'jobRunId'
  Prelude.Text ->
  CancelJobRun
newCancelJobRun pApplicationId_ pJobRunId_ =
  CancelJobRun'
    { applicationId = pApplicationId_,
      jobRunId = pJobRunId_
    }

-- | The ID of the application on which the job run will be canceled.
cancelJobRun_applicationId :: Lens.Lens' CancelJobRun Prelude.Text
cancelJobRun_applicationId = Lens.lens (\CancelJobRun' {applicationId} -> applicationId) (\s@CancelJobRun' {} a -> s {applicationId = a} :: CancelJobRun)

-- | The ID of the job run to cancel.
cancelJobRun_jobRunId :: Lens.Lens' CancelJobRun Prelude.Text
cancelJobRun_jobRunId = Lens.lens (\CancelJobRun' {jobRunId} -> jobRunId) (\s@CancelJobRun' {} a -> s {jobRunId = a} :: CancelJobRun)

instance Core.AWSRequest CancelJobRun where
  type AWSResponse CancelJobRun = CancelJobRunResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelJobRunResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "applicationId")
            Prelude.<*> (x Data..:> "jobRunId")
      )

instance Prelude.Hashable CancelJobRun where
  hashWithSalt _salt CancelJobRun' {..} =
    _salt
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` jobRunId

instance Prelude.NFData CancelJobRun where
  rnf CancelJobRun' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf jobRunId

instance Data.ToHeaders CancelJobRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath CancelJobRun where
  toPath CancelJobRun' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/jobruns/",
        Data.toBS jobRunId
      ]

instance Data.ToQuery CancelJobRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelJobRunResponse' smart constructor.
data CancelJobRunResponse = CancelJobRunResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The output contains the application ID on which the job run is
    -- cancelled.
    applicationId :: Prelude.Text,
    -- | The output contains the ID of the cancelled job run.
    jobRunId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelJobRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelJobRunResponse_httpStatus' - The response's http status code.
--
-- 'applicationId', 'cancelJobRunResponse_applicationId' - The output contains the application ID on which the job run is
-- cancelled.
--
-- 'jobRunId', 'cancelJobRunResponse_jobRunId' - The output contains the ID of the cancelled job run.
newCancelJobRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'jobRunId'
  Prelude.Text ->
  CancelJobRunResponse
newCancelJobRunResponse
  pHttpStatus_
  pApplicationId_
  pJobRunId_ =
    CancelJobRunResponse'
      { httpStatus = pHttpStatus_,
        applicationId = pApplicationId_,
        jobRunId = pJobRunId_
      }

-- | The response's http status code.
cancelJobRunResponse_httpStatus :: Lens.Lens' CancelJobRunResponse Prelude.Int
cancelJobRunResponse_httpStatus = Lens.lens (\CancelJobRunResponse' {httpStatus} -> httpStatus) (\s@CancelJobRunResponse' {} a -> s {httpStatus = a} :: CancelJobRunResponse)

-- | The output contains the application ID on which the job run is
-- cancelled.
cancelJobRunResponse_applicationId :: Lens.Lens' CancelJobRunResponse Prelude.Text
cancelJobRunResponse_applicationId = Lens.lens (\CancelJobRunResponse' {applicationId} -> applicationId) (\s@CancelJobRunResponse' {} a -> s {applicationId = a} :: CancelJobRunResponse)

-- | The output contains the ID of the cancelled job run.
cancelJobRunResponse_jobRunId :: Lens.Lens' CancelJobRunResponse Prelude.Text
cancelJobRunResponse_jobRunId = Lens.lens (\CancelJobRunResponse' {jobRunId} -> jobRunId) (\s@CancelJobRunResponse' {} a -> s {jobRunId = a} :: CancelJobRunResponse)

instance Prelude.NFData CancelJobRunResponse where
  rnf CancelJobRunResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf jobRunId
