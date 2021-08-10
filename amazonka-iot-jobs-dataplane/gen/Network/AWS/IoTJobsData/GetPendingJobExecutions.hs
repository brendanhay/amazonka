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
-- Module      : Network.AWS.IoTJobsData.GetPendingJobExecutions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the list of all jobs for a thing that are not in a terminal status.
module Network.AWS.IoTJobsData.GetPendingJobExecutions
  ( -- * Creating a Request
    GetPendingJobExecutions (..),
    newGetPendingJobExecutions,

    -- * Request Lenses
    getPendingJobExecutions_thingName,

    -- * Destructuring the Response
    GetPendingJobExecutionsResponse (..),
    newGetPendingJobExecutionsResponse,

    -- * Response Lenses
    getPendingJobExecutionsResponse_inProgressJobs,
    getPendingJobExecutionsResponse_queuedJobs,
    getPendingJobExecutionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTJobsData.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetPendingJobExecutions' smart constructor.
data GetPendingJobExecutions = GetPendingJobExecutions'
  { -- | The name of the thing that is executing the job.
    thingName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPendingJobExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingName', 'getPendingJobExecutions_thingName' - The name of the thing that is executing the job.
newGetPendingJobExecutions ::
  -- | 'thingName'
  Prelude.Text ->
  GetPendingJobExecutions
newGetPendingJobExecutions pThingName_ =
  GetPendingJobExecutions' {thingName = pThingName_}

-- | The name of the thing that is executing the job.
getPendingJobExecutions_thingName :: Lens.Lens' GetPendingJobExecutions Prelude.Text
getPendingJobExecutions_thingName = Lens.lens (\GetPendingJobExecutions' {thingName} -> thingName) (\s@GetPendingJobExecutions' {} a -> s {thingName = a} :: GetPendingJobExecutions)

instance Core.AWSRequest GetPendingJobExecutions where
  type
    AWSResponse GetPendingJobExecutions =
      GetPendingJobExecutionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPendingJobExecutionsResponse'
            Prelude.<$> (x Core..?> "inProgressJobs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "queuedJobs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPendingJobExecutions

instance Prelude.NFData GetPendingJobExecutions

instance Core.ToHeaders GetPendingJobExecutions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetPendingJobExecutions where
  toPath GetPendingJobExecutions' {..} =
    Prelude.mconcat
      ["/things/", Core.toBS thingName, "/jobs"]

instance Core.ToQuery GetPendingJobExecutions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPendingJobExecutionsResponse' smart constructor.
data GetPendingJobExecutionsResponse = GetPendingJobExecutionsResponse'
  { -- | A list of JobExecutionSummary objects with status IN_PROGRESS.
    inProgressJobs :: Prelude.Maybe [JobExecutionSummary],
    -- | A list of JobExecutionSummary objects with status QUEUED.
    queuedJobs :: Prelude.Maybe [JobExecutionSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPendingJobExecutionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inProgressJobs', 'getPendingJobExecutionsResponse_inProgressJobs' - A list of JobExecutionSummary objects with status IN_PROGRESS.
--
-- 'queuedJobs', 'getPendingJobExecutionsResponse_queuedJobs' - A list of JobExecutionSummary objects with status QUEUED.
--
-- 'httpStatus', 'getPendingJobExecutionsResponse_httpStatus' - The response's http status code.
newGetPendingJobExecutionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPendingJobExecutionsResponse
newGetPendingJobExecutionsResponse pHttpStatus_ =
  GetPendingJobExecutionsResponse'
    { inProgressJobs =
        Prelude.Nothing,
      queuedJobs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of JobExecutionSummary objects with status IN_PROGRESS.
getPendingJobExecutionsResponse_inProgressJobs :: Lens.Lens' GetPendingJobExecutionsResponse (Prelude.Maybe [JobExecutionSummary])
getPendingJobExecutionsResponse_inProgressJobs = Lens.lens (\GetPendingJobExecutionsResponse' {inProgressJobs} -> inProgressJobs) (\s@GetPendingJobExecutionsResponse' {} a -> s {inProgressJobs = a} :: GetPendingJobExecutionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A list of JobExecutionSummary objects with status QUEUED.
getPendingJobExecutionsResponse_queuedJobs :: Lens.Lens' GetPendingJobExecutionsResponse (Prelude.Maybe [JobExecutionSummary])
getPendingJobExecutionsResponse_queuedJobs = Lens.lens (\GetPendingJobExecutionsResponse' {queuedJobs} -> queuedJobs) (\s@GetPendingJobExecutionsResponse' {} a -> s {queuedJobs = a} :: GetPendingJobExecutionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getPendingJobExecutionsResponse_httpStatus :: Lens.Lens' GetPendingJobExecutionsResponse Prelude.Int
getPendingJobExecutionsResponse_httpStatus = Lens.lens (\GetPendingJobExecutionsResponse' {httpStatus} -> httpStatus) (\s@GetPendingJobExecutionsResponse' {} a -> s {httpStatus = a} :: GetPendingJobExecutionsResponse)

instance
  Prelude.NFData
    GetPendingJobExecutionsResponse
