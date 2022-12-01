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
-- Module      : Amazonka.CodePipeline.AcknowledgeJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specified job and whether that job has been
-- received by the job worker. Used for custom actions only.
module Amazonka.CodePipeline.AcknowledgeJob
  ( -- * Creating a Request
    AcknowledgeJob (..),
    newAcknowledgeJob,

    -- * Request Lenses
    acknowledgeJob_jobId,
    acknowledgeJob_nonce,

    -- * Destructuring the Response
    AcknowledgeJobResponse (..),
    newAcknowledgeJobResponse,

    -- * Response Lenses
    acknowledgeJobResponse_status,
    acknowledgeJobResponse_httpStatus,
  )
where

import Amazonka.CodePipeline.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of an AcknowledgeJob action.
--
-- /See:/ 'newAcknowledgeJob' smart constructor.
data AcknowledgeJob = AcknowledgeJob'
  { -- | The unique system-generated ID of the job for which you want to confirm
    -- receipt.
    jobId :: Prelude.Text,
    -- | A system-generated random number that AWS CodePipeline uses to ensure
    -- that the job is being worked on by only one job worker. Get this number
    -- from the response of the PollForJobs request that returned this job.
    nonce :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcknowledgeJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'acknowledgeJob_jobId' - The unique system-generated ID of the job for which you want to confirm
-- receipt.
--
-- 'nonce', 'acknowledgeJob_nonce' - A system-generated random number that AWS CodePipeline uses to ensure
-- that the job is being worked on by only one job worker. Get this number
-- from the response of the PollForJobs request that returned this job.
newAcknowledgeJob ::
  -- | 'jobId'
  Prelude.Text ->
  -- | 'nonce'
  Prelude.Text ->
  AcknowledgeJob
newAcknowledgeJob pJobId_ pNonce_ =
  AcknowledgeJob' {jobId = pJobId_, nonce = pNonce_}

-- | The unique system-generated ID of the job for which you want to confirm
-- receipt.
acknowledgeJob_jobId :: Lens.Lens' AcknowledgeJob Prelude.Text
acknowledgeJob_jobId = Lens.lens (\AcknowledgeJob' {jobId} -> jobId) (\s@AcknowledgeJob' {} a -> s {jobId = a} :: AcknowledgeJob)

-- | A system-generated random number that AWS CodePipeline uses to ensure
-- that the job is being worked on by only one job worker. Get this number
-- from the response of the PollForJobs request that returned this job.
acknowledgeJob_nonce :: Lens.Lens' AcknowledgeJob Prelude.Text
acknowledgeJob_nonce = Lens.lens (\AcknowledgeJob' {nonce} -> nonce) (\s@AcknowledgeJob' {} a -> s {nonce = a} :: AcknowledgeJob)

instance Core.AWSRequest AcknowledgeJob where
  type
    AWSResponse AcknowledgeJob =
      AcknowledgeJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AcknowledgeJobResponse'
            Prelude.<$> (x Core..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AcknowledgeJob where
  hashWithSalt _salt AcknowledgeJob' {..} =
    _salt `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` nonce

instance Prelude.NFData AcknowledgeJob where
  rnf AcknowledgeJob' {..} =
    Prelude.rnf jobId `Prelude.seq` Prelude.rnf nonce

instance Core.ToHeaders AcknowledgeJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodePipeline_20150709.AcknowledgeJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AcknowledgeJob where
  toJSON AcknowledgeJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("jobId" Core..= jobId),
            Prelude.Just ("nonce" Core..= nonce)
          ]
      )

instance Core.ToPath AcknowledgeJob where
  toPath = Prelude.const "/"

instance Core.ToQuery AcknowledgeJob where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of an AcknowledgeJob action.
--
-- /See:/ 'newAcknowledgeJobResponse' smart constructor.
data AcknowledgeJobResponse = AcknowledgeJobResponse'
  { -- | Whether the job worker has received the specified job.
    status :: Prelude.Maybe JobStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcknowledgeJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'acknowledgeJobResponse_status' - Whether the job worker has received the specified job.
--
-- 'httpStatus', 'acknowledgeJobResponse_httpStatus' - The response's http status code.
newAcknowledgeJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AcknowledgeJobResponse
newAcknowledgeJobResponse pHttpStatus_ =
  AcknowledgeJobResponse'
    { status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Whether the job worker has received the specified job.
acknowledgeJobResponse_status :: Lens.Lens' AcknowledgeJobResponse (Prelude.Maybe JobStatus)
acknowledgeJobResponse_status = Lens.lens (\AcknowledgeJobResponse' {status} -> status) (\s@AcknowledgeJobResponse' {} a -> s {status = a} :: AcknowledgeJobResponse)

-- | The response's http status code.
acknowledgeJobResponse_httpStatus :: Lens.Lens' AcknowledgeJobResponse Prelude.Int
acknowledgeJobResponse_httpStatus = Lens.lens (\AcknowledgeJobResponse' {httpStatus} -> httpStatus) (\s@AcknowledgeJobResponse' {} a -> s {httpStatus = a} :: AcknowledgeJobResponse)

instance Prelude.NFData AcknowledgeJobResponse where
  rnf AcknowledgeJobResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
