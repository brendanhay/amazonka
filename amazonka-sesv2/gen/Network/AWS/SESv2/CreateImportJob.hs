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
-- Module      : Network.AWS.SESv2.CreateImportJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an import job for a data destination.
module Network.AWS.SESv2.CreateImportJob
  ( -- * Creating a Request
    CreateImportJob (..),
    newCreateImportJob,

    -- * Request Lenses
    createImportJob_importDestination,
    createImportJob_importDataSource,

    -- * Destructuring the Response
    CreateImportJobResponse (..),
    newCreateImportJobResponse,

    -- * Response Lenses
    createImportJobResponse_jobId,
    createImportJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | Represents a request to create an import job from a data source for a
-- data destination.
--
-- /See:/ 'newCreateImportJob' smart constructor.
data CreateImportJob = CreateImportJob'
  { -- | The destination for the import job.
    importDestination :: ImportDestination,
    -- | The data source for the import job.
    importDataSource :: ImportDataSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'importDestination', 'createImportJob_importDestination' - The destination for the import job.
--
-- 'importDataSource', 'createImportJob_importDataSource' - The data source for the import job.
newCreateImportJob ::
  -- | 'importDestination'
  ImportDestination ->
  -- | 'importDataSource'
  ImportDataSource ->
  CreateImportJob
newCreateImportJob
  pImportDestination_
  pImportDataSource_ =
    CreateImportJob'
      { importDestination =
          pImportDestination_,
        importDataSource = pImportDataSource_
      }

-- | The destination for the import job.
createImportJob_importDestination :: Lens.Lens' CreateImportJob ImportDestination
createImportJob_importDestination = Lens.lens (\CreateImportJob' {importDestination} -> importDestination) (\s@CreateImportJob' {} a -> s {importDestination = a} :: CreateImportJob)

-- | The data source for the import job.
createImportJob_importDataSource :: Lens.Lens' CreateImportJob ImportDataSource
createImportJob_importDataSource = Lens.lens (\CreateImportJob' {importDataSource} -> importDataSource) (\s@CreateImportJob' {} a -> s {importDataSource = a} :: CreateImportJob)

instance Core.AWSRequest CreateImportJob where
  type
    AWSResponse CreateImportJob =
      CreateImportJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateImportJobResponse'
            Prelude.<$> (x Core..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateImportJob

instance Prelude.NFData CreateImportJob

instance Core.ToHeaders CreateImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateImportJob where
  toJSON CreateImportJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ImportDestination" Core..= importDestination),
            Prelude.Just
              ("ImportDataSource" Core..= importDataSource)
          ]
      )

instance Core.ToPath CreateImportJob where
  toPath = Prelude.const "/v2/email/import-jobs"

instance Core.ToQuery CreateImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newCreateImportJobResponse' smart constructor.
data CreateImportJobResponse = CreateImportJobResponse'
  { -- | A string that represents the import job ID.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'createImportJobResponse_jobId' - A string that represents the import job ID.
--
-- 'httpStatus', 'createImportJobResponse_httpStatus' - The response's http status code.
newCreateImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateImportJobResponse
newCreateImportJobResponse pHttpStatus_ =
  CreateImportJobResponse'
    { jobId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A string that represents the import job ID.
createImportJobResponse_jobId :: Lens.Lens' CreateImportJobResponse (Prelude.Maybe Prelude.Text)
createImportJobResponse_jobId = Lens.lens (\CreateImportJobResponse' {jobId} -> jobId) (\s@CreateImportJobResponse' {} a -> s {jobId = a} :: CreateImportJobResponse)

-- | The response's http status code.
createImportJobResponse_httpStatus :: Lens.Lens' CreateImportJobResponse Prelude.Int
createImportJobResponse_httpStatus = Lens.lens (\CreateImportJobResponse' {httpStatus} -> httpStatus) (\s@CreateImportJobResponse' {} a -> s {httpStatus = a} :: CreateImportJobResponse)

instance Prelude.NFData CreateImportJobResponse
