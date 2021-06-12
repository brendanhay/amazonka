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
-- Module      : Network.AWS.ElasticTranscoder.ReadJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ReadJob operation returns detailed information about a job.
module Network.AWS.ElasticTranscoder.ReadJob
  ( -- * Creating a Request
    ReadJob (..),
    newReadJob,

    -- * Request Lenses
    readJob_id,

    -- * Destructuring the Response
    ReadJobResponse (..),
    newReadJobResponse,

    -- * Response Lenses
    readJobResponse_httpStatus,
    readJobResponse_job,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticTranscoder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The @ReadJobRequest@ structure.
--
-- /See:/ 'newReadJob' smart constructor.
data ReadJob = ReadJob'
  { -- | The identifier of the job for which you want to get detailed
    -- information.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReadJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'readJob_id' - The identifier of the job for which you want to get detailed
-- information.
newReadJob ::
  -- | 'id'
  Core.Text ->
  ReadJob
newReadJob pId_ = ReadJob' {id = pId_}

-- | The identifier of the job for which you want to get detailed
-- information.
readJob_id :: Lens.Lens' ReadJob Core.Text
readJob_id = Lens.lens (\ReadJob' {id} -> id) (\s@ReadJob' {} a -> s {id = a} :: ReadJob)

instance Core.AWSRequest ReadJob where
  type AWSResponse ReadJob = ReadJobResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ReadJobResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "Job")
      )

instance Core.Hashable ReadJob

instance Core.NFData ReadJob

instance Core.ToHeaders ReadJob where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ReadJob where
  toPath ReadJob' {..} =
    Core.mconcat ["/2012-09-25/jobs/", Core.toBS id]

instance Core.ToQuery ReadJob where
  toQuery = Core.const Core.mempty

-- | The @ReadJobResponse@ structure.
--
-- /See:/ 'newReadJobResponse' smart constructor.
data ReadJobResponse = ReadJobResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A section of the response body that provides information about the job.
    job :: Job'
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReadJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'readJobResponse_httpStatus' - The response's http status code.
--
-- 'job', 'readJobResponse_job' - A section of the response body that provides information about the job.
newReadJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'job'
  Job' ->
  ReadJobResponse
newReadJobResponse pHttpStatus_ pJob_ =
  ReadJobResponse'
    { httpStatus = pHttpStatus_,
      job = pJob_
    }

-- | The response's http status code.
readJobResponse_httpStatus :: Lens.Lens' ReadJobResponse Core.Int
readJobResponse_httpStatus = Lens.lens (\ReadJobResponse' {httpStatus} -> httpStatus) (\s@ReadJobResponse' {} a -> s {httpStatus = a} :: ReadJobResponse)

-- | A section of the response body that provides information about the job.
readJobResponse_job :: Lens.Lens' ReadJobResponse Job'
readJobResponse_job = Lens.lens (\ReadJobResponse' {job} -> job) (\s@ReadJobResponse' {} a -> s {job = a} :: ReadJobResponse)

instance Core.NFData ReadJobResponse
