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
-- Module      : Network.AWS.CodePipeline.AcknowledgeThirdPartyJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Confirms a job worker has received the specified job. Used for partner
-- actions only.
module Network.AWS.CodePipeline.AcknowledgeThirdPartyJob
  ( -- * Creating a Request
    AcknowledgeThirdPartyJob (..),
    newAcknowledgeThirdPartyJob,

    -- * Request Lenses
    acknowledgeThirdPartyJob_jobId,
    acknowledgeThirdPartyJob_nonce,
    acknowledgeThirdPartyJob_clientToken,

    -- * Destructuring the Response
    AcknowledgeThirdPartyJobResponse (..),
    newAcknowledgeThirdPartyJobResponse,

    -- * Response Lenses
    acknowledgeThirdPartyJobResponse_status,
    acknowledgeThirdPartyJobResponse_httpStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of an AcknowledgeThirdPartyJob action.
--
-- /See:/ 'newAcknowledgeThirdPartyJob' smart constructor.
data AcknowledgeThirdPartyJob = AcknowledgeThirdPartyJob'
  { -- | The unique system-generated ID of the job.
    jobId :: Prelude.Text,
    -- | A system-generated random number that AWS CodePipeline uses to ensure
    -- that the job is being worked on by only one job worker. Get this number
    -- from the response to a GetThirdPartyJobDetails request.
    nonce :: Prelude.Text,
    -- | The clientToken portion of the clientId and clientToken pair used to
    -- verify that the calling entity is allowed access to the job and its
    -- details.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcknowledgeThirdPartyJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'acknowledgeThirdPartyJob_jobId' - The unique system-generated ID of the job.
--
-- 'nonce', 'acknowledgeThirdPartyJob_nonce' - A system-generated random number that AWS CodePipeline uses to ensure
-- that the job is being worked on by only one job worker. Get this number
-- from the response to a GetThirdPartyJobDetails request.
--
-- 'clientToken', 'acknowledgeThirdPartyJob_clientToken' - The clientToken portion of the clientId and clientToken pair used to
-- verify that the calling entity is allowed access to the job and its
-- details.
newAcknowledgeThirdPartyJob ::
  -- | 'jobId'
  Prelude.Text ->
  -- | 'nonce'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  AcknowledgeThirdPartyJob
newAcknowledgeThirdPartyJob
  pJobId_
  pNonce_
  pClientToken_ =
    AcknowledgeThirdPartyJob'
      { jobId = pJobId_,
        nonce = pNonce_,
        clientToken = pClientToken_
      }

-- | The unique system-generated ID of the job.
acknowledgeThirdPartyJob_jobId :: Lens.Lens' AcknowledgeThirdPartyJob Prelude.Text
acknowledgeThirdPartyJob_jobId = Lens.lens (\AcknowledgeThirdPartyJob' {jobId} -> jobId) (\s@AcknowledgeThirdPartyJob' {} a -> s {jobId = a} :: AcknowledgeThirdPartyJob)

-- | A system-generated random number that AWS CodePipeline uses to ensure
-- that the job is being worked on by only one job worker. Get this number
-- from the response to a GetThirdPartyJobDetails request.
acknowledgeThirdPartyJob_nonce :: Lens.Lens' AcknowledgeThirdPartyJob Prelude.Text
acknowledgeThirdPartyJob_nonce = Lens.lens (\AcknowledgeThirdPartyJob' {nonce} -> nonce) (\s@AcknowledgeThirdPartyJob' {} a -> s {nonce = a} :: AcknowledgeThirdPartyJob)

-- | The clientToken portion of the clientId and clientToken pair used to
-- verify that the calling entity is allowed access to the job and its
-- details.
acknowledgeThirdPartyJob_clientToken :: Lens.Lens' AcknowledgeThirdPartyJob Prelude.Text
acknowledgeThirdPartyJob_clientToken = Lens.lens (\AcknowledgeThirdPartyJob' {clientToken} -> clientToken) (\s@AcknowledgeThirdPartyJob' {} a -> s {clientToken = a} :: AcknowledgeThirdPartyJob)

instance Core.AWSRequest AcknowledgeThirdPartyJob where
  type
    AWSResponse AcknowledgeThirdPartyJob =
      AcknowledgeThirdPartyJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AcknowledgeThirdPartyJobResponse'
            Prelude.<$> (x Core..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AcknowledgeThirdPartyJob

instance Prelude.NFData AcknowledgeThirdPartyJob

instance Core.ToHeaders AcknowledgeThirdPartyJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodePipeline_20150709.AcknowledgeThirdPartyJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AcknowledgeThirdPartyJob where
  toJSON AcknowledgeThirdPartyJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("jobId" Core..= jobId),
            Prelude.Just ("nonce" Core..= nonce),
            Prelude.Just ("clientToken" Core..= clientToken)
          ]
      )

instance Core.ToPath AcknowledgeThirdPartyJob where
  toPath = Prelude.const "/"

instance Core.ToQuery AcknowledgeThirdPartyJob where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of an AcknowledgeThirdPartyJob action.
--
-- /See:/ 'newAcknowledgeThirdPartyJobResponse' smart constructor.
data AcknowledgeThirdPartyJobResponse = AcknowledgeThirdPartyJobResponse'
  { -- | The status information for the third party job, if any.
    status :: Prelude.Maybe JobStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcknowledgeThirdPartyJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'acknowledgeThirdPartyJobResponse_status' - The status information for the third party job, if any.
--
-- 'httpStatus', 'acknowledgeThirdPartyJobResponse_httpStatus' - The response's http status code.
newAcknowledgeThirdPartyJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AcknowledgeThirdPartyJobResponse
newAcknowledgeThirdPartyJobResponse pHttpStatus_ =
  AcknowledgeThirdPartyJobResponse'
    { status =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status information for the third party job, if any.
acknowledgeThirdPartyJobResponse_status :: Lens.Lens' AcknowledgeThirdPartyJobResponse (Prelude.Maybe JobStatus)
acknowledgeThirdPartyJobResponse_status = Lens.lens (\AcknowledgeThirdPartyJobResponse' {status} -> status) (\s@AcknowledgeThirdPartyJobResponse' {} a -> s {status = a} :: AcknowledgeThirdPartyJobResponse)

-- | The response's http status code.
acknowledgeThirdPartyJobResponse_httpStatus :: Lens.Lens' AcknowledgeThirdPartyJobResponse Prelude.Int
acknowledgeThirdPartyJobResponse_httpStatus = Lens.lens (\AcknowledgeThirdPartyJobResponse' {httpStatus} -> httpStatus) (\s@AcknowledgeThirdPartyJobResponse' {} a -> s {httpStatus = a} :: AcknowledgeThirdPartyJobResponse)

instance
  Prelude.NFData
    AcknowledgeThirdPartyJobResponse
