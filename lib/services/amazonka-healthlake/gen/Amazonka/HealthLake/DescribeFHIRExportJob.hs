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
-- Module      : Amazonka.HealthLake.DescribeFHIRExportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays the properties of a FHIR export job, including the ID, ARN,
-- name, and the status of the job.
module Amazonka.HealthLake.DescribeFHIRExportJob
  ( -- * Creating a Request
    DescribeFHIRExportJob (..),
    newDescribeFHIRExportJob,

    -- * Request Lenses
    describeFHIRExportJob_datastoreId,
    describeFHIRExportJob_jobId,

    -- * Destructuring the Response
    DescribeFHIRExportJobResponse (..),
    newDescribeFHIRExportJobResponse,

    -- * Response Lenses
    describeFHIRExportJobResponse_httpStatus,
    describeFHIRExportJobResponse_exportJobProperties,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HealthLake.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFHIRExportJob' smart constructor.
data DescribeFHIRExportJob = DescribeFHIRExportJob'
  { -- | The AWS generated ID for the Data Store from which files are being
    -- exported from for an export job.
    datastoreId :: Prelude.Text,
    -- | The AWS generated ID for an export job.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFHIRExportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datastoreId', 'describeFHIRExportJob_datastoreId' - The AWS generated ID for the Data Store from which files are being
-- exported from for an export job.
--
-- 'jobId', 'describeFHIRExportJob_jobId' - The AWS generated ID for an export job.
newDescribeFHIRExportJob ::
  -- | 'datastoreId'
  Prelude.Text ->
  -- | 'jobId'
  Prelude.Text ->
  DescribeFHIRExportJob
newDescribeFHIRExportJob pDatastoreId_ pJobId_ =
  DescribeFHIRExportJob'
    { datastoreId = pDatastoreId_,
      jobId = pJobId_
    }

-- | The AWS generated ID for the Data Store from which files are being
-- exported from for an export job.
describeFHIRExportJob_datastoreId :: Lens.Lens' DescribeFHIRExportJob Prelude.Text
describeFHIRExportJob_datastoreId = Lens.lens (\DescribeFHIRExportJob' {datastoreId} -> datastoreId) (\s@DescribeFHIRExportJob' {} a -> s {datastoreId = a} :: DescribeFHIRExportJob)

-- | The AWS generated ID for an export job.
describeFHIRExportJob_jobId :: Lens.Lens' DescribeFHIRExportJob Prelude.Text
describeFHIRExportJob_jobId = Lens.lens (\DescribeFHIRExportJob' {jobId} -> jobId) (\s@DescribeFHIRExportJob' {} a -> s {jobId = a} :: DescribeFHIRExportJob)

instance Core.AWSRequest DescribeFHIRExportJob where
  type
    AWSResponse DescribeFHIRExportJob =
      DescribeFHIRExportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFHIRExportJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ExportJobProperties")
      )

instance Prelude.Hashable DescribeFHIRExportJob where
  hashWithSalt _salt DescribeFHIRExportJob' {..} =
    _salt `Prelude.hashWithSalt` datastoreId
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData DescribeFHIRExportJob where
  rnf DescribeFHIRExportJob' {..} =
    Prelude.rnf datastoreId
      `Prelude.seq` Prelude.rnf jobId

instance Data.ToHeaders DescribeFHIRExportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "HealthLake.DescribeFHIRExportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeFHIRExportJob where
  toJSON DescribeFHIRExportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DatastoreId" Data..= datastoreId),
            Prelude.Just ("JobId" Data..= jobId)
          ]
      )

instance Data.ToPath DescribeFHIRExportJob where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeFHIRExportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFHIRExportJobResponse' smart constructor.
data DescribeFHIRExportJobResponse = DescribeFHIRExportJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Displays the properties of the export job, including the ID, Arn, Name,
    -- and the status of the job.
    exportJobProperties :: ExportJobProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFHIRExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeFHIRExportJobResponse_httpStatus' - The response's http status code.
--
-- 'exportJobProperties', 'describeFHIRExportJobResponse_exportJobProperties' - Displays the properties of the export job, including the ID, Arn, Name,
-- and the status of the job.
newDescribeFHIRExportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'exportJobProperties'
  ExportJobProperties ->
  DescribeFHIRExportJobResponse
newDescribeFHIRExportJobResponse
  pHttpStatus_
  pExportJobProperties_ =
    DescribeFHIRExportJobResponse'
      { httpStatus =
          pHttpStatus_,
        exportJobProperties = pExportJobProperties_
      }

-- | The response's http status code.
describeFHIRExportJobResponse_httpStatus :: Lens.Lens' DescribeFHIRExportJobResponse Prelude.Int
describeFHIRExportJobResponse_httpStatus = Lens.lens (\DescribeFHIRExportJobResponse' {httpStatus} -> httpStatus) (\s@DescribeFHIRExportJobResponse' {} a -> s {httpStatus = a} :: DescribeFHIRExportJobResponse)

-- | Displays the properties of the export job, including the ID, Arn, Name,
-- and the status of the job.
describeFHIRExportJobResponse_exportJobProperties :: Lens.Lens' DescribeFHIRExportJobResponse ExportJobProperties
describeFHIRExportJobResponse_exportJobProperties = Lens.lens (\DescribeFHIRExportJobResponse' {exportJobProperties} -> exportJobProperties) (\s@DescribeFHIRExportJobResponse' {} a -> s {exportJobProperties = a} :: DescribeFHIRExportJobResponse)

instance Prelude.NFData DescribeFHIRExportJobResponse where
  rnf DescribeFHIRExportJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf exportJobProperties
