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
-- Module      : Network.AWS.Snowball.GetJobUnlockCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the @UnlockCode@ code value for the specified job. A particular
-- @UnlockCode@ value can be accessed for up to 90 days after the
-- associated job has been created.
--
-- The @UnlockCode@ value is a 29-character code with 25 alphanumeric
-- characters and 4 hyphens. This code is used to decrypt the manifest file
-- when it is passed along with the manifest to the Snow device through the
-- Snowball client when the client is started for the first time.
--
-- As a best practice, we recommend that you don\'t save a copy of the
-- @UnlockCode@ in the same location as the manifest file for that job.
-- Saving these separately helps prevent unauthorized parties from gaining
-- access to the Snow device associated with that job.
module Network.AWS.Snowball.GetJobUnlockCode
  ( -- * Creating a Request
    GetJobUnlockCode (..),
    newGetJobUnlockCode,

    -- * Request Lenses
    getJobUnlockCode_jobId,

    -- * Destructuring the Response
    GetJobUnlockCodeResponse (..),
    newGetJobUnlockCodeResponse,

    -- * Response Lenses
    getJobUnlockCodeResponse_unlockCode,
    getJobUnlockCodeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Snowball.Types

-- | /See:/ 'newGetJobUnlockCode' smart constructor.
data GetJobUnlockCode = GetJobUnlockCode'
  { -- | The ID for the job that you want to get the @UnlockCode@ value for, for
    -- example @JID123e4567-e89b-12d3-a456-426655440000@.
    jobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetJobUnlockCode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'getJobUnlockCode_jobId' - The ID for the job that you want to get the @UnlockCode@ value for, for
-- example @JID123e4567-e89b-12d3-a456-426655440000@.
newGetJobUnlockCode ::
  -- | 'jobId'
  Core.Text ->
  GetJobUnlockCode
newGetJobUnlockCode pJobId_ =
  GetJobUnlockCode' {jobId = pJobId_}

-- | The ID for the job that you want to get the @UnlockCode@ value for, for
-- example @JID123e4567-e89b-12d3-a456-426655440000@.
getJobUnlockCode_jobId :: Lens.Lens' GetJobUnlockCode Core.Text
getJobUnlockCode_jobId = Lens.lens (\GetJobUnlockCode' {jobId} -> jobId) (\s@GetJobUnlockCode' {} a -> s {jobId = a} :: GetJobUnlockCode)

instance Core.AWSRequest GetJobUnlockCode where
  type
    AWSResponse GetJobUnlockCode =
      GetJobUnlockCodeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobUnlockCodeResponse'
            Core.<$> (x Core..?> "UnlockCode")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetJobUnlockCode

instance Core.NFData GetJobUnlockCode

instance Core.ToHeaders GetJobUnlockCode where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSIESnowballJobManagementService.GetJobUnlockCode" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetJobUnlockCode where
  toJSON GetJobUnlockCode' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.ToPath GetJobUnlockCode where
  toPath = Core.const "/"

instance Core.ToQuery GetJobUnlockCode where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetJobUnlockCodeResponse' smart constructor.
data GetJobUnlockCodeResponse = GetJobUnlockCodeResponse'
  { -- | The @UnlockCode@ value for the specified job. The @UnlockCode@ value can
    -- be accessed for up to 90 days after the job has been created.
    unlockCode :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetJobUnlockCodeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unlockCode', 'getJobUnlockCodeResponse_unlockCode' - The @UnlockCode@ value for the specified job. The @UnlockCode@ value can
-- be accessed for up to 90 days after the job has been created.
--
-- 'httpStatus', 'getJobUnlockCodeResponse_httpStatus' - The response's http status code.
newGetJobUnlockCodeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetJobUnlockCodeResponse
newGetJobUnlockCodeResponse pHttpStatus_ =
  GetJobUnlockCodeResponse'
    { unlockCode =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @UnlockCode@ value for the specified job. The @UnlockCode@ value can
-- be accessed for up to 90 days after the job has been created.
getJobUnlockCodeResponse_unlockCode :: Lens.Lens' GetJobUnlockCodeResponse (Core.Maybe Core.Text)
getJobUnlockCodeResponse_unlockCode = Lens.lens (\GetJobUnlockCodeResponse' {unlockCode} -> unlockCode) (\s@GetJobUnlockCodeResponse' {} a -> s {unlockCode = a} :: GetJobUnlockCodeResponse)

-- | The response's http status code.
getJobUnlockCodeResponse_httpStatus :: Lens.Lens' GetJobUnlockCodeResponse Core.Int
getJobUnlockCodeResponse_httpStatus = Lens.lens (\GetJobUnlockCodeResponse' {httpStatus} -> httpStatus) (\s@GetJobUnlockCodeResponse' {} a -> s {httpStatus = a} :: GetJobUnlockCodeResponse)

instance Core.NFData GetJobUnlockCodeResponse
