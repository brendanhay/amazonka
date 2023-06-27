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
-- Module      : Amazonka.HealthLake.DescribeFHIRImportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays the properties of a FHIR import job, including the ID, ARN,
-- name, and the status of the job.
module Amazonka.HealthLake.DescribeFHIRImportJob
  ( -- * Creating a Request
    DescribeFHIRImportJob (..),
    newDescribeFHIRImportJob,

    -- * Request Lenses
    describeFHIRImportJob_datastoreId,
    describeFHIRImportJob_jobId,

    -- * Destructuring the Response
    DescribeFHIRImportJobResponse (..),
    newDescribeFHIRImportJobResponse,

    -- * Response Lenses
    describeFHIRImportJobResponse_httpStatus,
    describeFHIRImportJobResponse_importJobProperties,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HealthLake.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFHIRImportJob' smart constructor.
data DescribeFHIRImportJob = DescribeFHIRImportJob'
  { -- | The AWS-generated ID of the Data Store.
    datastoreId :: Prelude.Text,
    -- | The AWS-generated job ID.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFHIRImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datastoreId', 'describeFHIRImportJob_datastoreId' - The AWS-generated ID of the Data Store.
--
-- 'jobId', 'describeFHIRImportJob_jobId' - The AWS-generated job ID.
newDescribeFHIRImportJob ::
  -- | 'datastoreId'
  Prelude.Text ->
  -- | 'jobId'
  Prelude.Text ->
  DescribeFHIRImportJob
newDescribeFHIRImportJob pDatastoreId_ pJobId_ =
  DescribeFHIRImportJob'
    { datastoreId = pDatastoreId_,
      jobId = pJobId_
    }

-- | The AWS-generated ID of the Data Store.
describeFHIRImportJob_datastoreId :: Lens.Lens' DescribeFHIRImportJob Prelude.Text
describeFHIRImportJob_datastoreId = Lens.lens (\DescribeFHIRImportJob' {datastoreId} -> datastoreId) (\s@DescribeFHIRImportJob' {} a -> s {datastoreId = a} :: DescribeFHIRImportJob)

-- | The AWS-generated job ID.
describeFHIRImportJob_jobId :: Lens.Lens' DescribeFHIRImportJob Prelude.Text
describeFHIRImportJob_jobId = Lens.lens (\DescribeFHIRImportJob' {jobId} -> jobId) (\s@DescribeFHIRImportJob' {} a -> s {jobId = a} :: DescribeFHIRImportJob)

instance Core.AWSRequest DescribeFHIRImportJob where
  type
    AWSResponse DescribeFHIRImportJob =
      DescribeFHIRImportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFHIRImportJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ImportJobProperties")
      )

instance Prelude.Hashable DescribeFHIRImportJob where
  hashWithSalt _salt DescribeFHIRImportJob' {..} =
    _salt
      `Prelude.hashWithSalt` datastoreId
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData DescribeFHIRImportJob where
  rnf DescribeFHIRImportJob' {..} =
    Prelude.rnf datastoreId
      `Prelude.seq` Prelude.rnf jobId

instance Data.ToHeaders DescribeFHIRImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "HealthLake.DescribeFHIRImportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeFHIRImportJob where
  toJSON DescribeFHIRImportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DatastoreId" Data..= datastoreId),
            Prelude.Just ("JobId" Data..= jobId)
          ]
      )

instance Data.ToPath DescribeFHIRImportJob where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeFHIRImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFHIRImportJobResponse' smart constructor.
data DescribeFHIRImportJobResponse = DescribeFHIRImportJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The properties of the Import job request, including the ID, ARN, name,
    -- and the status of the job.
    importJobProperties :: ImportJobProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFHIRImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeFHIRImportJobResponse_httpStatus' - The response's http status code.
--
-- 'importJobProperties', 'describeFHIRImportJobResponse_importJobProperties' - The properties of the Import job request, including the ID, ARN, name,
-- and the status of the job.
newDescribeFHIRImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'importJobProperties'
  ImportJobProperties ->
  DescribeFHIRImportJobResponse
newDescribeFHIRImportJobResponse
  pHttpStatus_
  pImportJobProperties_ =
    DescribeFHIRImportJobResponse'
      { httpStatus =
          pHttpStatus_,
        importJobProperties = pImportJobProperties_
      }

-- | The response's http status code.
describeFHIRImportJobResponse_httpStatus :: Lens.Lens' DescribeFHIRImportJobResponse Prelude.Int
describeFHIRImportJobResponse_httpStatus = Lens.lens (\DescribeFHIRImportJobResponse' {httpStatus} -> httpStatus) (\s@DescribeFHIRImportJobResponse' {} a -> s {httpStatus = a} :: DescribeFHIRImportJobResponse)

-- | The properties of the Import job request, including the ID, ARN, name,
-- and the status of the job.
describeFHIRImportJobResponse_importJobProperties :: Lens.Lens' DescribeFHIRImportJobResponse ImportJobProperties
describeFHIRImportJobResponse_importJobProperties = Lens.lens (\DescribeFHIRImportJobResponse' {importJobProperties} -> importJobProperties) (\s@DescribeFHIRImportJobResponse' {} a -> s {importJobProperties = a} :: DescribeFHIRImportJobResponse)

instance Prelude.NFData DescribeFHIRImportJobResponse where
  rnf DescribeFHIRImportJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf importJobProperties
