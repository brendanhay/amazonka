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
-- Module      : Network.AWS.ElasticTranscoder.UpdatePipelineStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The UpdatePipelineStatus operation pauses or reactivates a pipeline, so
-- that the pipeline stops or restarts the processing of jobs.
--
-- Changing the pipeline status is useful if you want to cancel one or more
-- jobs. You can\'t cancel jobs after Elastic Transcoder has started
-- processing them; if you pause the pipeline to which you submitted the
-- jobs, you have more time to get the job IDs for the jobs that you want
-- to cancel, and to send a CancelJob request.
module Network.AWS.ElasticTranscoder.UpdatePipelineStatus
  ( -- * Creating a Request
    UpdatePipelineStatus (..),
    newUpdatePipelineStatus,

    -- * Request Lenses
    updatePipelineStatus_id,
    updatePipelineStatus_status,

    -- * Destructuring the Response
    UpdatePipelineStatusResponse (..),
    newUpdatePipelineStatusResponse,

    -- * Response Lenses
    updatePipelineStatusResponse_pipeline,
    updatePipelineStatusResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticTranscoder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The @UpdatePipelineStatusRequest@ structure.
--
-- /See:/ 'newUpdatePipelineStatus' smart constructor.
data UpdatePipelineStatus = UpdatePipelineStatus'
  { -- | The identifier of the pipeline to update.
    id :: Prelude.Text,
    -- | The desired status of the pipeline:
    --
    -- -   @Active@: The pipeline is processing jobs.
    --
    -- -   @Paused@: The pipeline is not currently processing jobs.
    status :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePipelineStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'updatePipelineStatus_id' - The identifier of the pipeline to update.
--
-- 'status', 'updatePipelineStatus_status' - The desired status of the pipeline:
--
-- -   @Active@: The pipeline is processing jobs.
--
-- -   @Paused@: The pipeline is not currently processing jobs.
newUpdatePipelineStatus ::
  -- | 'id'
  Prelude.Text ->
  -- | 'status'
  Prelude.Text ->
  UpdatePipelineStatus
newUpdatePipelineStatus pId_ pStatus_ =
  UpdatePipelineStatus' {id = pId_, status = pStatus_}

-- | The identifier of the pipeline to update.
updatePipelineStatus_id :: Lens.Lens' UpdatePipelineStatus Prelude.Text
updatePipelineStatus_id = Lens.lens (\UpdatePipelineStatus' {id} -> id) (\s@UpdatePipelineStatus' {} a -> s {id = a} :: UpdatePipelineStatus)

-- | The desired status of the pipeline:
--
-- -   @Active@: The pipeline is processing jobs.
--
-- -   @Paused@: The pipeline is not currently processing jobs.
updatePipelineStatus_status :: Lens.Lens' UpdatePipelineStatus Prelude.Text
updatePipelineStatus_status = Lens.lens (\UpdatePipelineStatus' {status} -> status) (\s@UpdatePipelineStatus' {} a -> s {status = a} :: UpdatePipelineStatus)

instance Core.AWSRequest UpdatePipelineStatus where
  type
    AWSResponse UpdatePipelineStatus =
      UpdatePipelineStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePipelineStatusResponse'
            Prelude.<$> (x Core..?> "Pipeline")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePipelineStatus

instance Prelude.NFData UpdatePipelineStatus

instance Core.ToHeaders UpdatePipelineStatus where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpdatePipelineStatus where
  toJSON UpdatePipelineStatus' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Status" Core..= status)]
      )

instance Core.ToPath UpdatePipelineStatus where
  toPath UpdatePipelineStatus' {..} =
    Prelude.mconcat
      ["/2012-09-25/pipelines/", Core.toBS id, "/status"]

instance Core.ToQuery UpdatePipelineStatus where
  toQuery = Prelude.const Prelude.mempty

-- | When you update status for a pipeline, Elastic Transcoder returns the
-- values that you specified in the request.
--
-- /See:/ 'newUpdatePipelineStatusResponse' smart constructor.
data UpdatePipelineStatusResponse = UpdatePipelineStatusResponse'
  { -- | A section of the response body that provides information about the
    -- pipeline.
    pipeline :: Prelude.Maybe Pipeline,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePipelineStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipeline', 'updatePipelineStatusResponse_pipeline' - A section of the response body that provides information about the
-- pipeline.
--
-- 'httpStatus', 'updatePipelineStatusResponse_httpStatus' - The response's http status code.
newUpdatePipelineStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePipelineStatusResponse
newUpdatePipelineStatusResponse pHttpStatus_ =
  UpdatePipelineStatusResponse'
    { pipeline =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A section of the response body that provides information about the
-- pipeline.
updatePipelineStatusResponse_pipeline :: Lens.Lens' UpdatePipelineStatusResponse (Prelude.Maybe Pipeline)
updatePipelineStatusResponse_pipeline = Lens.lens (\UpdatePipelineStatusResponse' {pipeline} -> pipeline) (\s@UpdatePipelineStatusResponse' {} a -> s {pipeline = a} :: UpdatePipelineStatusResponse)

-- | The response's http status code.
updatePipelineStatusResponse_httpStatus :: Lens.Lens' UpdatePipelineStatusResponse Prelude.Int
updatePipelineStatusResponse_httpStatus = Lens.lens (\UpdatePipelineStatusResponse' {httpStatus} -> httpStatus) (\s@UpdatePipelineStatusResponse' {} a -> s {httpStatus = a} :: UpdatePipelineStatusResponse)

instance Prelude.NFData UpdatePipelineStatusResponse
