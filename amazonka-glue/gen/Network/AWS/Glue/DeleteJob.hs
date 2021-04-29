{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteJob' smart constructor.
data DeleteJob = DeleteJob'
  { -- | The name of the job definition to delete.
    jobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteJob
newDeleteJob pJobName_ =
  DeleteJob' {jobName = pJobName_}

-- | The name of the job definition to delete.
deleteJob_jobName :: Lens.Lens' DeleteJob Prelude.Text
deleteJob_jobName = Lens.lens (\DeleteJob' {jobName} -> jobName) (\s@DeleteJob' {} a -> s {jobName = a} :: DeleteJob)

instance Prelude.AWSRequest DeleteJob where
  type Rs DeleteJob = DeleteJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteJobResponse'
            Prelude.<$> (x Prelude..?> "JobName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteJob

instance Prelude.NFData DeleteJob

instance Prelude.ToHeaders DeleteJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.DeleteJob" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteJob where
  toJSON DeleteJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobName" Prelude..= jobName)]
      )

instance Prelude.ToPath DeleteJob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteJobResponse' smart constructor.
data DeleteJobResponse = DeleteJobResponse'
  { -- | The name of the job definition that was deleted.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteJobResponse
newDeleteJobResponse pHttpStatus_ =
  DeleteJobResponse'
    { jobName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the job definition that was deleted.
deleteJobResponse_jobName :: Lens.Lens' DeleteJobResponse (Prelude.Maybe Prelude.Text)
deleteJobResponse_jobName = Lens.lens (\DeleteJobResponse' {jobName} -> jobName) (\s@DeleteJobResponse' {} a -> s {jobName = a} :: DeleteJobResponse)

-- | The response's http status code.
deleteJobResponse_httpStatus :: Lens.Lens' DeleteJobResponse Prelude.Int
deleteJobResponse_httpStatus = Lens.lens (\DeleteJobResponse' {httpStatus} -> httpStatus) (\s@DeleteJobResponse' {} a -> s {httpStatus = a} :: DeleteJobResponse)

instance Prelude.NFData DeleteJobResponse
