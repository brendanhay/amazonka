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
-- Module      : Amazonka.EMRServerless.GetDashboardForJobRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a URL to access the job run dashboard.
module Amazonka.EMRServerless.GetDashboardForJobRun
  ( -- * Creating a Request
    GetDashboardForJobRun (..),
    newGetDashboardForJobRun,

    -- * Request Lenses
    getDashboardForJobRun_applicationId,
    getDashboardForJobRun_jobRunId,

    -- * Destructuring the Response
    GetDashboardForJobRunResponse (..),
    newGetDashboardForJobRunResponse,

    -- * Response Lenses
    getDashboardForJobRunResponse_url,
    getDashboardForJobRunResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDashboardForJobRun' smart constructor.
data GetDashboardForJobRun = GetDashboardForJobRun'
  { -- | The ID of the application.
    applicationId :: Prelude.Text,
    -- | The ID of the job run.
    jobRunId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDashboardForJobRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getDashboardForJobRun_applicationId' - The ID of the application.
--
-- 'jobRunId', 'getDashboardForJobRun_jobRunId' - The ID of the job run.
newGetDashboardForJobRun ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'jobRunId'
  Prelude.Text ->
  GetDashboardForJobRun
newGetDashboardForJobRun pApplicationId_ pJobRunId_ =
  GetDashboardForJobRun'
    { applicationId =
        pApplicationId_,
      jobRunId = pJobRunId_
    }

-- | The ID of the application.
getDashboardForJobRun_applicationId :: Lens.Lens' GetDashboardForJobRun Prelude.Text
getDashboardForJobRun_applicationId = Lens.lens (\GetDashboardForJobRun' {applicationId} -> applicationId) (\s@GetDashboardForJobRun' {} a -> s {applicationId = a} :: GetDashboardForJobRun)

-- | The ID of the job run.
getDashboardForJobRun_jobRunId :: Lens.Lens' GetDashboardForJobRun Prelude.Text
getDashboardForJobRun_jobRunId = Lens.lens (\GetDashboardForJobRun' {jobRunId} -> jobRunId) (\s@GetDashboardForJobRun' {} a -> s {jobRunId = a} :: GetDashboardForJobRun)

instance Core.AWSRequest GetDashboardForJobRun where
  type
    AWSResponse GetDashboardForJobRun =
      GetDashboardForJobRunResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDashboardForJobRunResponse'
            Prelude.<$> (x Data..?> "url")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDashboardForJobRun where
  hashWithSalt _salt GetDashboardForJobRun' {..} =
    _salt
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` jobRunId

instance Prelude.NFData GetDashboardForJobRun where
  rnf GetDashboardForJobRun' {..} =
    Prelude.rnf applicationId `Prelude.seq`
      Prelude.rnf jobRunId

instance Data.ToHeaders GetDashboardForJobRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDashboardForJobRun where
  toPath GetDashboardForJobRun' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/jobruns/",
        Data.toBS jobRunId,
        "/dashboard"
      ]

instance Data.ToQuery GetDashboardForJobRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDashboardForJobRunResponse' smart constructor.
data GetDashboardForJobRunResponse = GetDashboardForJobRunResponse'
  { -- | The URL to view job run\'s dashboard.
    url :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDashboardForJobRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'url', 'getDashboardForJobRunResponse_url' - The URL to view job run\'s dashboard.
--
-- 'httpStatus', 'getDashboardForJobRunResponse_httpStatus' - The response's http status code.
newGetDashboardForJobRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDashboardForJobRunResponse
newGetDashboardForJobRunResponse pHttpStatus_ =
  GetDashboardForJobRunResponse'
    { url =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The URL to view job run\'s dashboard.
getDashboardForJobRunResponse_url :: Lens.Lens' GetDashboardForJobRunResponse (Prelude.Maybe Prelude.Text)
getDashboardForJobRunResponse_url = Lens.lens (\GetDashboardForJobRunResponse' {url} -> url) (\s@GetDashboardForJobRunResponse' {} a -> s {url = a} :: GetDashboardForJobRunResponse)

-- | The response's http status code.
getDashboardForJobRunResponse_httpStatus :: Lens.Lens' GetDashboardForJobRunResponse Prelude.Int
getDashboardForJobRunResponse_httpStatus = Lens.lens (\GetDashboardForJobRunResponse' {httpStatus} -> httpStatus) (\s@GetDashboardForJobRunResponse' {} a -> s {httpStatus = a} :: GetDashboardForJobRunResponse)

instance Prelude.NFData GetDashboardForJobRunResponse where
  rnf GetDashboardForJobRunResponse' {..} =
    Prelude.rnf url `Prelude.seq`
      Prelude.rnf httpStatus
