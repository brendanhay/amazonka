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
-- Module      : Network.AWS.ElasticTranscoder.CancelJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The CancelJob operation cancels an unfinished job.
--
-- You can only cancel a job that has a status of @Submitted@. To prevent a
-- pipeline from starting to process a job while you\'re getting the job
-- identifier, use UpdatePipelineStatus to temporarily pause the pipeline.
module Network.AWS.ElasticTranscoder.CancelJob
  ( -- * Creating a Request
    CancelJob (..),
    newCancelJob,

    -- * Request Lenses
    cancelJob_id,

    -- * Destructuring the Response
    CancelJobResponse (..),
    newCancelJobResponse,

    -- * Response Lenses
    cancelJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticTranscoder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The @CancelJobRequest@ structure.
--
-- /See:/ 'newCancelJob' smart constructor.
data CancelJob = CancelJob'
  { -- | The identifier of the job that you want to cancel.
    --
    -- To get a list of the jobs (including their @jobId@) that have a status
    -- of @Submitted@, use the ListJobsByStatus API action.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CancelJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'cancelJob_id' - The identifier of the job that you want to cancel.
--
-- To get a list of the jobs (including their @jobId@) that have a status
-- of @Submitted@, use the ListJobsByStatus API action.
newCancelJob ::
  -- | 'id'
  Core.Text ->
  CancelJob
newCancelJob pId_ = CancelJob' {id = pId_}

-- | The identifier of the job that you want to cancel.
--
-- To get a list of the jobs (including their @jobId@) that have a status
-- of @Submitted@, use the ListJobsByStatus API action.
cancelJob_id :: Lens.Lens' CancelJob Core.Text
cancelJob_id = Lens.lens (\CancelJob' {id} -> id) (\s@CancelJob' {} a -> s {id = a} :: CancelJob)

instance Core.AWSRequest CancelJob where
  type AWSResponse CancelJob = CancelJobResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelJobResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CancelJob

instance Core.NFData CancelJob

instance Core.ToHeaders CancelJob where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CancelJob where
  toPath CancelJob' {..} =
    Core.mconcat ["/2012-09-25/jobs/", Core.toBS id]

instance Core.ToQuery CancelJob where
  toQuery = Core.const Core.mempty

-- | The response body contains a JSON object. If the job is successfully
-- canceled, the value of @Success@ is @true@.
--
-- /See:/ 'newCancelJobResponse' smart constructor.
data CancelJobResponse = CancelJobResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CancelJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelJobResponse_httpStatus' - The response's http status code.
newCancelJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CancelJobResponse
newCancelJobResponse pHttpStatus_ =
  CancelJobResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
cancelJobResponse_httpStatus :: Lens.Lens' CancelJobResponse Core.Int
cancelJobResponse_httpStatus = Lens.lens (\CancelJobResponse' {httpStatus} -> httpStatus) (\s@CancelJobResponse' {} a -> s {httpStatus = a} :: CancelJobResponse)

instance Core.NFData CancelJobResponse
