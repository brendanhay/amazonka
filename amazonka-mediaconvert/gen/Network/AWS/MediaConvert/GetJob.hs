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
-- Module      : Network.AWS.MediaConvert.GetJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve the JSON for a specific completed transcoding job.
module Network.AWS.MediaConvert.GetJob
  ( -- * Creating a Request
    GetJob (..),
    newGetJob,

    -- * Request Lenses
    getJob_id,

    -- * Destructuring the Response
    GetJobResponse (..),
    newGetJobResponse,

    -- * Response Lenses
    getJobResponse_job,
    getJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetJob' smart constructor.
data GetJob = GetJob'
  { -- | the job ID of the job.
    id :: Core.Text
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
-- 'id', 'getJob_id' - the job ID of the job.
newGetJob ::
  -- | 'id'
  Core.Text ->
  GetJob
newGetJob pId_ = GetJob' {id = pId_}

-- | the job ID of the job.
getJob_id :: Lens.Lens' GetJob Core.Text
getJob_id = Lens.lens (\GetJob' {id} -> id) (\s@GetJob' {} a -> s {id = a} :: GetJob)

instance Core.AWSRequest GetJob where
  type AWSResponse GetJob = GetJobResponse
  request = Request.get defaultService
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
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetJob where
  toPath GetJob' {..} =
    Core.mconcat ["/2017-08-29/jobs/", Core.toBS id]

instance Core.ToQuery GetJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetJobResponse' smart constructor.
data GetJobResponse = GetJobResponse'
  { -- | Each job converts an input file into an output file or files. For more
    -- information, see the User Guide at
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/what-is.html
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
-- 'job', 'getJobResponse_job' - Each job converts an input file into an output file or files. For more
-- information, see the User Guide at
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/what-is.html
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

-- | Each job converts an input file into an output file or files. For more
-- information, see the User Guide at
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/what-is.html
getJobResponse_job :: Lens.Lens' GetJobResponse (Core.Maybe Job)
getJobResponse_job = Lens.lens (\GetJobResponse' {job} -> job) (\s@GetJobResponse' {} a -> s {job = a} :: GetJobResponse)

-- | The response's http status code.
getJobResponse_httpStatus :: Lens.Lens' GetJobResponse Core.Int
getJobResponse_httpStatus = Lens.lens (\GetJobResponse' {httpStatus} -> httpStatus) (\s@GetJobResponse' {} a -> s {httpStatus = a} :: GetJobResponse)

instance Core.NFData GetJobResponse
