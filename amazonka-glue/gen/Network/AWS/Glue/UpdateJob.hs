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

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateJob' smart constructor.
data UpdateJob = UpdateJob'
  { -- | The name of the job definition to update.
    jobName :: Prelude.Text,
    -- | Specifies the values with which to update the job definition.
    jobUpdate :: JobUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'jobUpdate'
  JobUpdate ->
  UpdateJob
newUpdateJob pJobName_ pJobUpdate_ =
  UpdateJob'
    { jobName = pJobName_,
      jobUpdate = pJobUpdate_
    }

-- | The name of the job definition to update.
updateJob_jobName :: Lens.Lens' UpdateJob Prelude.Text
updateJob_jobName = Lens.lens (\UpdateJob' {jobName} -> jobName) (\s@UpdateJob' {} a -> s {jobName = a} :: UpdateJob)

-- | Specifies the values with which to update the job definition.
updateJob_jobUpdate :: Lens.Lens' UpdateJob JobUpdate
updateJob_jobUpdate = Lens.lens (\UpdateJob' {jobUpdate} -> jobUpdate) (\s@UpdateJob' {} a -> s {jobUpdate = a} :: UpdateJob)

instance Prelude.AWSRequest UpdateJob where
  type Rs UpdateJob = UpdateJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateJobResponse'
            Prelude.<$> (x Prelude..?> "JobName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateJob

instance Prelude.NFData UpdateJob

instance Prelude.ToHeaders UpdateJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.UpdateJob" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateJob where
  toJSON UpdateJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("JobName" Prelude..= jobName),
            Prelude.Just ("JobUpdate" Prelude..= jobUpdate)
          ]
      )

instance Prelude.ToPath UpdateJob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateJobResponse' smart constructor.
data UpdateJobResponse = UpdateJobResponse'
  { -- | Returns the name of the updated job definition.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateJobResponse
newUpdateJobResponse pHttpStatus_ =
  UpdateJobResponse'
    { jobName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns the name of the updated job definition.
updateJobResponse_jobName :: Lens.Lens' UpdateJobResponse (Prelude.Maybe Prelude.Text)
updateJobResponse_jobName = Lens.lens (\UpdateJobResponse' {jobName} -> jobName) (\s@UpdateJobResponse' {} a -> s {jobName = a} :: UpdateJobResponse)

-- | The response's http status code.
updateJobResponse_httpStatus :: Lens.Lens' UpdateJobResponse Prelude.Int
updateJobResponse_httpStatus = Lens.lens (\UpdateJobResponse' {httpStatus} -> httpStatus) (\s@UpdateJobResponse' {} a -> s {httpStatus = a} :: UpdateJobResponse)

instance Prelude.NFData UpdateJobResponse
