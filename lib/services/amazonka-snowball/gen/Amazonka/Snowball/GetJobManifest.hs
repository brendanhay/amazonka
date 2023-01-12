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
-- Module      : Amazonka.Snowball.GetJobManifest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a link to an Amazon S3 presigned URL for the manifest file
-- associated with the specified @JobId@ value. You can access the manifest
-- file for up to 60 minutes after this request has been made. To access
-- the manifest file after 60 minutes have passed, you\'ll have to make
-- another call to the @GetJobManifest@ action.
--
-- The manifest is an encrypted file that you can download after your job
-- enters the @WithCustomer@ status. This is the only valid status for
-- calling this API as the manifest and @UnlockCode@ code value are used
-- for securing your device and should only be used when you have the
-- device. The manifest is decrypted by using the @UnlockCode@ code value,
-- when you pass both values to the Snow device through the Snowball client
-- when the client is started for the first time.
--
-- As a best practice, we recommend that you don\'t save a copy of an
-- @UnlockCode@ value in the same location as the manifest file for that
-- job. Saving these separately helps prevent unauthorized parties from
-- gaining access to the Snow device associated with that job.
--
-- The credentials of a given job, including its manifest file and unlock
-- code, expire 360 days after the job is created.
module Amazonka.Snowball.GetJobManifest
  ( -- * Creating a Request
    GetJobManifest (..),
    newGetJobManifest,

    -- * Request Lenses
    getJobManifest_jobId,

    -- * Destructuring the Response
    GetJobManifestResponse (..),
    newGetJobManifestResponse,

    -- * Response Lenses
    getJobManifestResponse_manifestURI,
    getJobManifestResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Snowball.Types

-- | /See:/ 'newGetJobManifest' smart constructor.
data GetJobManifest = GetJobManifest'
  { -- | The ID for a job that you want to get the manifest file for, for example
    -- @JID123e4567-e89b-12d3-a456-426655440000@.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJobManifest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'getJobManifest_jobId' - The ID for a job that you want to get the manifest file for, for example
-- @JID123e4567-e89b-12d3-a456-426655440000@.
newGetJobManifest ::
  -- | 'jobId'
  Prelude.Text ->
  GetJobManifest
newGetJobManifest pJobId_ =
  GetJobManifest' {jobId = pJobId_}

-- | The ID for a job that you want to get the manifest file for, for example
-- @JID123e4567-e89b-12d3-a456-426655440000@.
getJobManifest_jobId :: Lens.Lens' GetJobManifest Prelude.Text
getJobManifest_jobId = Lens.lens (\GetJobManifest' {jobId} -> jobId) (\s@GetJobManifest' {} a -> s {jobId = a} :: GetJobManifest)

instance Core.AWSRequest GetJobManifest where
  type
    AWSResponse GetJobManifest =
      GetJobManifestResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobManifestResponse'
            Prelude.<$> (x Data..?> "ManifestURI")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetJobManifest where
  hashWithSalt _salt GetJobManifest' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData GetJobManifest where
  rnf GetJobManifest' {..} = Prelude.rnf jobId

instance Data.ToHeaders GetJobManifest where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIESnowballJobManagementService.GetJobManifest" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetJobManifest where
  toJSON GetJobManifest' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Data..= jobId)]
      )

instance Data.ToPath GetJobManifest where
  toPath = Prelude.const "/"

instance Data.ToQuery GetJobManifest where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetJobManifestResponse' smart constructor.
data GetJobManifestResponse = GetJobManifestResponse'
  { -- | The Amazon S3 presigned URL for the manifest file associated with the
    -- specified @JobId@ value.
    manifestURI :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJobManifestResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'manifestURI', 'getJobManifestResponse_manifestURI' - The Amazon S3 presigned URL for the manifest file associated with the
-- specified @JobId@ value.
--
-- 'httpStatus', 'getJobManifestResponse_httpStatus' - The response's http status code.
newGetJobManifestResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetJobManifestResponse
newGetJobManifestResponse pHttpStatus_ =
  GetJobManifestResponse'
    { manifestURI =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon S3 presigned URL for the manifest file associated with the
-- specified @JobId@ value.
getJobManifestResponse_manifestURI :: Lens.Lens' GetJobManifestResponse (Prelude.Maybe Prelude.Text)
getJobManifestResponse_manifestURI = Lens.lens (\GetJobManifestResponse' {manifestURI} -> manifestURI) (\s@GetJobManifestResponse' {} a -> s {manifestURI = a} :: GetJobManifestResponse)

-- | The response's http status code.
getJobManifestResponse_httpStatus :: Lens.Lens' GetJobManifestResponse Prelude.Int
getJobManifestResponse_httpStatus = Lens.lens (\GetJobManifestResponse' {httpStatus} -> httpStatus) (\s@GetJobManifestResponse' {} a -> s {httpStatus = a} :: GetJobManifestResponse)

instance Prelude.NFData GetJobManifestResponse where
  rnf GetJobManifestResponse' {..} =
    Prelude.rnf manifestURI
      `Prelude.seq` Prelude.rnf httpStatus
