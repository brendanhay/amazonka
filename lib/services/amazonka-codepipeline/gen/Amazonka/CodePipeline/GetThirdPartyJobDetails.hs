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
-- Module      : Amazonka.CodePipeline.GetThirdPartyJobDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests the details of a job for a third party action. Used for partner
-- actions only.
--
-- When this API is called, AWS CodePipeline returns temporary credentials
-- for the S3 bucket used to store artifacts for the pipeline, if the
-- action requires access to that S3 bucket for input or output artifacts.
-- This API also returns any secret values defined for the action.
module Amazonka.CodePipeline.GetThirdPartyJobDetails
  ( -- * Creating a Request
    GetThirdPartyJobDetails (..),
    newGetThirdPartyJobDetails,

    -- * Request Lenses
    getThirdPartyJobDetails_jobId,
    getThirdPartyJobDetails_clientToken,

    -- * Destructuring the Response
    GetThirdPartyJobDetailsResponse (..),
    newGetThirdPartyJobDetailsResponse,

    -- * Response Lenses
    getThirdPartyJobDetailsResponse_jobDetails,
    getThirdPartyJobDetailsResponse_httpStatus,
  )
where

import Amazonka.CodePipeline.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @GetThirdPartyJobDetails@ action.
--
-- /See:/ 'newGetThirdPartyJobDetails' smart constructor.
data GetThirdPartyJobDetails = GetThirdPartyJobDetails'
  { -- | The unique system-generated ID used for identifying the job.
    jobId :: Prelude.Text,
    -- | The clientToken portion of the clientId and clientToken pair used to
    -- verify that the calling entity is allowed access to the job and its
    -- details.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetThirdPartyJobDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'getThirdPartyJobDetails_jobId' - The unique system-generated ID used for identifying the job.
--
-- 'clientToken', 'getThirdPartyJobDetails_clientToken' - The clientToken portion of the clientId and clientToken pair used to
-- verify that the calling entity is allowed access to the job and its
-- details.
newGetThirdPartyJobDetails ::
  -- | 'jobId'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  GetThirdPartyJobDetails
newGetThirdPartyJobDetails pJobId_ pClientToken_ =
  GetThirdPartyJobDetails'
    { jobId = pJobId_,
      clientToken = pClientToken_
    }

-- | The unique system-generated ID used for identifying the job.
getThirdPartyJobDetails_jobId :: Lens.Lens' GetThirdPartyJobDetails Prelude.Text
getThirdPartyJobDetails_jobId = Lens.lens (\GetThirdPartyJobDetails' {jobId} -> jobId) (\s@GetThirdPartyJobDetails' {} a -> s {jobId = a} :: GetThirdPartyJobDetails)

-- | The clientToken portion of the clientId and clientToken pair used to
-- verify that the calling entity is allowed access to the job and its
-- details.
getThirdPartyJobDetails_clientToken :: Lens.Lens' GetThirdPartyJobDetails Prelude.Text
getThirdPartyJobDetails_clientToken = Lens.lens (\GetThirdPartyJobDetails' {clientToken} -> clientToken) (\s@GetThirdPartyJobDetails' {} a -> s {clientToken = a} :: GetThirdPartyJobDetails)

instance Core.AWSRequest GetThirdPartyJobDetails where
  type
    AWSResponse GetThirdPartyJobDetails =
      GetThirdPartyJobDetailsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetThirdPartyJobDetailsResponse'
            Prelude.<$> (x Data..?> "jobDetails")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetThirdPartyJobDetails where
  hashWithSalt _salt GetThirdPartyJobDetails' {..} =
    _salt `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData GetThirdPartyJobDetails where
  rnf GetThirdPartyJobDetails' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders GetThirdPartyJobDetails where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodePipeline_20150709.GetThirdPartyJobDetails" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetThirdPartyJobDetails where
  toJSON GetThirdPartyJobDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("jobId" Data..= jobId),
            Prelude.Just ("clientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath GetThirdPartyJobDetails where
  toPath = Prelude.const "/"

instance Data.ToQuery GetThirdPartyJobDetails where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @GetThirdPartyJobDetails@ action.
--
-- /See:/ 'newGetThirdPartyJobDetailsResponse' smart constructor.
data GetThirdPartyJobDetailsResponse = GetThirdPartyJobDetailsResponse'
  { -- | The details of the job, including any protected values defined for the
    -- job.
    jobDetails :: Prelude.Maybe ThirdPartyJobDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetThirdPartyJobDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobDetails', 'getThirdPartyJobDetailsResponse_jobDetails' - The details of the job, including any protected values defined for the
-- job.
--
-- 'httpStatus', 'getThirdPartyJobDetailsResponse_httpStatus' - The response's http status code.
newGetThirdPartyJobDetailsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetThirdPartyJobDetailsResponse
newGetThirdPartyJobDetailsResponse pHttpStatus_ =
  GetThirdPartyJobDetailsResponse'
    { jobDetails =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the job, including any protected values defined for the
-- job.
getThirdPartyJobDetailsResponse_jobDetails :: Lens.Lens' GetThirdPartyJobDetailsResponse (Prelude.Maybe ThirdPartyJobDetails)
getThirdPartyJobDetailsResponse_jobDetails = Lens.lens (\GetThirdPartyJobDetailsResponse' {jobDetails} -> jobDetails) (\s@GetThirdPartyJobDetailsResponse' {} a -> s {jobDetails = a} :: GetThirdPartyJobDetailsResponse)

-- | The response's http status code.
getThirdPartyJobDetailsResponse_httpStatus :: Lens.Lens' GetThirdPartyJobDetailsResponse Prelude.Int
getThirdPartyJobDetailsResponse_httpStatus = Lens.lens (\GetThirdPartyJobDetailsResponse' {httpStatus} -> httpStatus) (\s@GetThirdPartyJobDetailsResponse' {} a -> s {httpStatus = a} :: GetThirdPartyJobDetailsResponse)

instance
  Prelude.NFData
    GetThirdPartyJobDetailsResponse
  where
  rnf GetThirdPartyJobDetailsResponse' {..} =
    Prelude.rnf jobDetails
      `Prelude.seq` Prelude.rnf httpStatus
