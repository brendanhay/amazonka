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
-- Module      : Network.AWS.Glue.UpdateJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing job definition.
module Network.AWS.Glue.UpdateJob
  ( -- * Creating a Request
    UpdateJob (..),
    newUpdateJob,

    -- * Request Lenses
    updateJob_jobName,
    updateJob_jobUpdate,

    -- * Destructuring the Response
    UpdateJobResponse (..),
    newUpdateJobResponse,

    -- * Response Lenses
    updateJobResponse_jobName,
    updateJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateJob' smart constructor.
data UpdateJob = UpdateJob'
  { -- | The name of the job definition to update.
    jobName :: Core.Text,
    -- | Specifies the values with which to update the job definition.
    jobUpdate :: JobUpdate
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'updateJob_jobName' - The name of the job definition to update.
--
-- 'jobUpdate', 'updateJob_jobUpdate' - Specifies the values with which to update the job definition.
newUpdateJob ::
  -- | 'jobName'
  Core.Text ->
  -- | 'jobUpdate'
  JobUpdate ->
  UpdateJob
newUpdateJob pJobName_ pJobUpdate_ =
  UpdateJob'
    { jobName = pJobName_,
      jobUpdate = pJobUpdate_
    }

-- | The name of the job definition to update.
updateJob_jobName :: Lens.Lens' UpdateJob Core.Text
updateJob_jobName = Lens.lens (\UpdateJob' {jobName} -> jobName) (\s@UpdateJob' {} a -> s {jobName = a} :: UpdateJob)

-- | Specifies the values with which to update the job definition.
updateJob_jobUpdate :: Lens.Lens' UpdateJob JobUpdate
updateJob_jobUpdate = Lens.lens (\UpdateJob' {jobUpdate} -> jobUpdate) (\s@UpdateJob' {} a -> s {jobUpdate = a} :: UpdateJob)

instance Core.AWSRequest UpdateJob where
  type AWSResponse UpdateJob = UpdateJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateJobResponse'
            Core.<$> (x Core..?> "JobName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateJob

instance Core.NFData UpdateJob

instance Core.ToHeaders UpdateJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.UpdateJob" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateJob where
  toJSON UpdateJob' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("JobName" Core..= jobName),
            Core.Just ("JobUpdate" Core..= jobUpdate)
          ]
      )

instance Core.ToPath UpdateJob where
  toPath = Core.const "/"

instance Core.ToQuery UpdateJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateJobResponse' smart constructor.
data UpdateJobResponse = UpdateJobResponse'
  { -- | Returns the name of the updated job definition.
    jobName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'updateJobResponse_jobName' - Returns the name of the updated job definition.
--
-- 'httpStatus', 'updateJobResponse_httpStatus' - The response's http status code.
newUpdateJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateJobResponse
newUpdateJobResponse pHttpStatus_ =
  UpdateJobResponse'
    { jobName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns the name of the updated job definition.
updateJobResponse_jobName :: Lens.Lens' UpdateJobResponse (Core.Maybe Core.Text)
updateJobResponse_jobName = Lens.lens (\UpdateJobResponse' {jobName} -> jobName) (\s@UpdateJobResponse' {} a -> s {jobName = a} :: UpdateJobResponse)

-- | The response's http status code.
updateJobResponse_httpStatus :: Lens.Lens' UpdateJobResponse Core.Int
updateJobResponse_httpStatus = Lens.lens (\UpdateJobResponse' {httpStatus} -> httpStatus) (\s@UpdateJobResponse' {} a -> s {httpStatus = a} :: UpdateJobResponse)

instance Core.NFData UpdateJobResponse
