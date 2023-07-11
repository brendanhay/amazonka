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
-- Module      : Amazonka.Glue.UpdateJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing job definition. The previous job definition is
-- completely overwritten by this information.
module Amazonka.Glue.UpdateJob
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateJob' smart constructor.
data UpdateJob = UpdateJob'
  { -- | The name of the job definition to update.
    jobName :: Prelude.Text,
    -- | Specifies the values with which to update the job definition.
    -- Unspecified configuration is removed or reset to default values.
    jobUpdate :: JobUpdate
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
-- Unspecified configuration is removed or reset to default values.
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
-- Unspecified configuration is removed or reset to default values.
updateJob_jobUpdate :: Lens.Lens' UpdateJob JobUpdate
updateJob_jobUpdate = Lens.lens (\UpdateJob' {jobUpdate} -> jobUpdate) (\s@UpdateJob' {} a -> s {jobUpdate = a} :: UpdateJob)

instance Core.AWSRequest UpdateJob where
  type AWSResponse UpdateJob = UpdateJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateJobResponse'
            Prelude.<$> (x Data..?> "JobName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateJob where
  hashWithSalt _salt UpdateJob' {..} =
    _salt
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` jobUpdate

instance Prelude.NFData UpdateJob where
  rnf UpdateJob' {..} =
    Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobUpdate

instance Data.ToHeaders UpdateJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.UpdateJob" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateJob where
  toJSON UpdateJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("JobName" Data..= jobName),
            Prelude.Just ("JobUpdate" Data..= jobUpdate)
          ]
      )

instance Data.ToPath UpdateJob where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateJobResponse' smart constructor.
data UpdateJobResponse = UpdateJobResponse'
  { -- | Returns the name of the updated job definition.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData UpdateJobResponse where
  rnf UpdateJobResponse' {..} =
    Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf httpStatus
