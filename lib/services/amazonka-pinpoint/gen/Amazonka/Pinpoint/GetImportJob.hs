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
-- Module      : Amazonka.Pinpoint.GetImportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of a specific import
-- job for an application.
module Amazonka.Pinpoint.GetImportJob
  ( -- * Creating a Request
    GetImportJob (..),
    newGetImportJob,

    -- * Request Lenses
    getImportJob_applicationId,
    getImportJob_jobId,

    -- * Destructuring the Response
    GetImportJobResponse (..),
    newGetImportJobResponse,

    -- * Response Lenses
    getImportJobResponse_httpStatus,
    getImportJobResponse_importJobResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetImportJob' smart constructor.
data GetImportJob = GetImportJob'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    -- | The unique identifier for the job.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getImportJob_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'jobId', 'getImportJob_jobId' - The unique identifier for the job.
newGetImportJob ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'jobId'
  Prelude.Text ->
  GetImportJob
newGetImportJob pApplicationId_ pJobId_ =
  GetImportJob'
    { applicationId = pApplicationId_,
      jobId = pJobId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getImportJob_applicationId :: Lens.Lens' GetImportJob Prelude.Text
getImportJob_applicationId = Lens.lens (\GetImportJob' {applicationId} -> applicationId) (\s@GetImportJob' {} a -> s {applicationId = a} :: GetImportJob)

-- | The unique identifier for the job.
getImportJob_jobId :: Lens.Lens' GetImportJob Prelude.Text
getImportJob_jobId = Lens.lens (\GetImportJob' {jobId} -> jobId) (\s@GetImportJob' {} a -> s {jobId = a} :: GetImportJob)

instance Core.AWSRequest GetImportJob where
  type AWSResponse GetImportJob = GetImportJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetImportJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable GetImportJob where
  hashWithSalt _salt GetImportJob' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData GetImportJob where
  rnf GetImportJob' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf jobId

instance Data.ToHeaders GetImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetImportJob where
  toPath GetImportJob' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Data.toBS applicationId,
        "/jobs/import/",
        Data.toBS jobId
      ]

instance Data.ToQuery GetImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetImportJobResponse' smart constructor.
data GetImportJobResponse = GetImportJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    importJobResponse :: ImportJobResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getImportJobResponse_httpStatus' - The response's http status code.
--
-- 'importJobResponse', 'getImportJobResponse_importJobResponse' - Undocumented member.
newGetImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'importJobResponse'
  ImportJobResponse ->
  GetImportJobResponse
newGetImportJobResponse
  pHttpStatus_
  pImportJobResponse_ =
    GetImportJobResponse'
      { httpStatus = pHttpStatus_,
        importJobResponse = pImportJobResponse_
      }

-- | The response's http status code.
getImportJobResponse_httpStatus :: Lens.Lens' GetImportJobResponse Prelude.Int
getImportJobResponse_httpStatus = Lens.lens (\GetImportJobResponse' {httpStatus} -> httpStatus) (\s@GetImportJobResponse' {} a -> s {httpStatus = a} :: GetImportJobResponse)

-- | Undocumented member.
getImportJobResponse_importJobResponse :: Lens.Lens' GetImportJobResponse ImportJobResponse
getImportJobResponse_importJobResponse = Lens.lens (\GetImportJobResponse' {importJobResponse} -> importJobResponse) (\s@GetImportJobResponse' {} a -> s {importJobResponse = a} :: GetImportJobResponse)

instance Prelude.NFData GetImportJobResponse where
  rnf GetImportJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf importJobResponse
