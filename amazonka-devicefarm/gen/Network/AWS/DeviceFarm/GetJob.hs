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
-- Module      : Network.AWS.DeviceFarm.GetJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a job.
module Network.AWS.DeviceFarm.GetJob
  ( -- * Creating a Request
    GetJob (..),
    newGetJob,

    -- * Request Lenses
    getJob_arn,

    -- * Destructuring the Response
    GetJobResponse (..),
    newGetJobResponse,

    -- * Response Lenses
    getJobResponse_job,
    getJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the get job operation.
--
-- /See:/ 'newGetJob' smart constructor.
data GetJob = GetJob'
  { -- | The job\'s ARN.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getJob_arn' - The job\'s ARN.
newGetJob ::
  -- | 'arn'
  Core.Text ->
  GetJob
newGetJob pArn_ = GetJob' {arn = pArn_}

-- | The job\'s ARN.
getJob_arn :: Lens.Lens' GetJob Core.Text
getJob_arn = Lens.lens (\GetJob' {arn} -> arn) (\s@GetJob' {} a -> s {arn = a} :: GetJob)

instance Core.AWSRequest GetJob where
  type AWSResponse GetJob = GetJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobResponse'
            Core.<$> (x Core..?> "job")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetJob

instance Core.NFData GetJob

instance Core.ToHeaders GetJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("DeviceFarm_20150623.GetJob" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetJob where
  toJSON GetJob' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.ToPath GetJob where
  toPath = Core.const "/"

instance Core.ToQuery GetJob where
  toQuery = Core.const Core.mempty

-- | Represents the result of a get job request.
--
-- /See:/ 'newGetJobResponse' smart constructor.
data GetJobResponse = GetJobResponse'
  { -- | An object that contains information about the requested job.
    job :: Core.Maybe Job,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'job', 'getJobResponse_job' - An object that contains information about the requested job.
--
-- 'httpStatus', 'getJobResponse_httpStatus' - The response's http status code.
newGetJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetJobResponse
newGetJobResponse pHttpStatus_ =
  GetJobResponse'
    { job = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains information about the requested job.
getJobResponse_job :: Lens.Lens' GetJobResponse (Core.Maybe Job)
getJobResponse_job = Lens.lens (\GetJobResponse' {job} -> job) (\s@GetJobResponse' {} a -> s {job = a} :: GetJobResponse)

-- | The response's http status code.
getJobResponse_httpStatus :: Lens.Lens' GetJobResponse Core.Int
getJobResponse_httpStatus = Lens.lens (\GetJobResponse' {httpStatus} -> httpStatus) (\s@GetJobResponse' {} a -> s {httpStatus = a} :: GetJobResponse)

instance Core.NFData GetJobResponse
