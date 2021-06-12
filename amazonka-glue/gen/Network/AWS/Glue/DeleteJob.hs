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
-- Module      : Network.AWS.Glue.DeleteJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified job definition. If the job definition is not found,
-- no exception is thrown.
module Network.AWS.Glue.DeleteJob
  ( -- * Creating a Request
    DeleteJob (..),
    newDeleteJob,

    -- * Request Lenses
    deleteJob_jobName,

    -- * Destructuring the Response
    DeleteJobResponse (..),
    newDeleteJobResponse,

    -- * Response Lenses
    deleteJobResponse_jobName,
    deleteJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteJob' smart constructor.
data DeleteJob = DeleteJob'
  { -- | The name of the job definition to delete.
    jobName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'deleteJob_jobName' - The name of the job definition to delete.
newDeleteJob ::
  -- | 'jobName'
  Core.Text ->
  DeleteJob
newDeleteJob pJobName_ =
  DeleteJob' {jobName = pJobName_}

-- | The name of the job definition to delete.
deleteJob_jobName :: Lens.Lens' DeleteJob Core.Text
deleteJob_jobName = Lens.lens (\DeleteJob' {jobName} -> jobName) (\s@DeleteJob' {} a -> s {jobName = a} :: DeleteJob)

instance Core.AWSRequest DeleteJob where
  type AWSResponse DeleteJob = DeleteJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteJobResponse'
            Core.<$> (x Core..?> "JobName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteJob

instance Core.NFData DeleteJob

instance Core.ToHeaders DeleteJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.DeleteJob" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteJob where
  toJSON DeleteJob' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("JobName" Core..= jobName)]
      )

instance Core.ToPath DeleteJob where
  toPath = Core.const "/"

instance Core.ToQuery DeleteJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteJobResponse' smart constructor.
data DeleteJobResponse = DeleteJobResponse'
  { -- | The name of the job definition that was deleted.
    jobName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'deleteJobResponse_jobName' - The name of the job definition that was deleted.
--
-- 'httpStatus', 'deleteJobResponse_httpStatus' - The response's http status code.
newDeleteJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteJobResponse
newDeleteJobResponse pHttpStatus_ =
  DeleteJobResponse'
    { jobName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the job definition that was deleted.
deleteJobResponse_jobName :: Lens.Lens' DeleteJobResponse (Core.Maybe Core.Text)
deleteJobResponse_jobName = Lens.lens (\DeleteJobResponse' {jobName} -> jobName) (\s@DeleteJobResponse' {} a -> s {jobName = a} :: DeleteJobResponse)

-- | The response's http status code.
deleteJobResponse_httpStatus :: Lens.Lens' DeleteJobResponse Core.Int
deleteJobResponse_httpStatus = Lens.lens (\DeleteJobResponse' {httpStatus} -> httpStatus) (\s@DeleteJobResponse' {} a -> s {httpStatus = a} :: DeleteJobResponse)

instance Core.NFData DeleteJobResponse
