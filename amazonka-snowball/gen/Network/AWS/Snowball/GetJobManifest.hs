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
-- Module      : Network.AWS.Snowball.GetJobManifest
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- enters the @WithCustomer@ status. The manifest is decrypted by using the
-- @UnlockCode@ code value, when you pass both values to the Snow device
-- through the Snowball client when the client is started for the first
-- time.
--
-- As a best practice, we recommend that you don\'t save a copy of an
-- @UnlockCode@ value in the same location as the manifest file for that
-- job. Saving these separately helps prevent unauthorized parties from
-- gaining access to the Snow device associated with that job.
--
-- The credentials of a given job, including its manifest file and unlock
-- code, expire 90 days after the job is created.
module Network.AWS.Snowball.GetJobManifest
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Snowball.Types

-- | /See:/ 'newGetJobManifest' smart constructor.
data GetJobManifest = GetJobManifest'
  { -- | The ID for a job that you want to get the manifest file for, for example
    -- @JID123e4567-e89b-12d3-a456-426655440000@.
    jobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetJobManifest
newGetJobManifest pJobId_ =
  GetJobManifest' {jobId = pJobId_}

-- | The ID for a job that you want to get the manifest file for, for example
-- @JID123e4567-e89b-12d3-a456-426655440000@.
getJobManifest_jobId :: Lens.Lens' GetJobManifest Core.Text
getJobManifest_jobId = Lens.lens (\GetJobManifest' {jobId} -> jobId) (\s@GetJobManifest' {} a -> s {jobId = a} :: GetJobManifest)

instance Core.AWSRequest GetJobManifest where
  type
    AWSResponse GetJobManifest =
      GetJobManifestResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobManifestResponse'
            Core.<$> (x Core..?> "ManifestURI")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetJobManifest

instance Core.NFData GetJobManifest

instance Core.ToHeaders GetJobManifest where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSIESnowballJobManagementService.GetJobManifest" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetJobManifest where
  toJSON GetJobManifest' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.ToPath GetJobManifest where
  toPath = Core.const "/"

instance Core.ToQuery GetJobManifest where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetJobManifestResponse' smart constructor.
data GetJobManifestResponse = GetJobManifestResponse'
  { -- | The Amazon S3 presigned URL for the manifest file associated with the
    -- specified @JobId@ value.
    manifestURI :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetJobManifestResponse
newGetJobManifestResponse pHttpStatus_ =
  GetJobManifestResponse'
    { manifestURI = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon S3 presigned URL for the manifest file associated with the
-- specified @JobId@ value.
getJobManifestResponse_manifestURI :: Lens.Lens' GetJobManifestResponse (Core.Maybe Core.Text)
getJobManifestResponse_manifestURI = Lens.lens (\GetJobManifestResponse' {manifestURI} -> manifestURI) (\s@GetJobManifestResponse' {} a -> s {manifestURI = a} :: GetJobManifestResponse)

-- | The response's http status code.
getJobManifestResponse_httpStatus :: Lens.Lens' GetJobManifestResponse Core.Int
getJobManifestResponse_httpStatus = Lens.lens (\GetJobManifestResponse' {httpStatus} -> httpStatus) (\s@GetJobManifestResponse' {} a -> s {httpStatus = a} :: GetJobManifestResponse)

instance Core.NFData GetJobManifestResponse
