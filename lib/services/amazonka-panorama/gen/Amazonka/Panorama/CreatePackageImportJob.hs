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
-- Module      : Amazonka.Panorama.CreatePackageImportJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports a node package.
module Amazonka.Panorama.CreatePackageImportJob
  ( -- * Creating a Request
    CreatePackageImportJob (..),
    newCreatePackageImportJob,

    -- * Request Lenses
    createPackageImportJob_jobTags,
    createPackageImportJob_jobType,
    createPackageImportJob_inputConfig,
    createPackageImportJob_outputConfig,
    createPackageImportJob_clientToken,

    -- * Destructuring the Response
    CreatePackageImportJobResponse (..),
    newCreatePackageImportJobResponse,

    -- * Response Lenses
    createPackageImportJobResponse_httpStatus,
    createPackageImportJobResponse_jobId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Panorama.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePackageImportJob' smart constructor.
data CreatePackageImportJob = CreatePackageImportJob'
  { -- | Tags for the package import job.
    jobTags :: Prelude.Maybe [JobResourceTags],
    -- | A job type for the package import job.
    jobType :: PackageImportJobType,
    -- | An input config for the package import job.
    inputConfig :: PackageImportJobInputConfig,
    -- | An output config for the package import job.
    outputConfig :: PackageImportJobOutputConfig,
    -- | A client token for the package import job.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePackageImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobTags', 'createPackageImportJob_jobTags' - Tags for the package import job.
--
-- 'jobType', 'createPackageImportJob_jobType' - A job type for the package import job.
--
-- 'inputConfig', 'createPackageImportJob_inputConfig' - An input config for the package import job.
--
-- 'outputConfig', 'createPackageImportJob_outputConfig' - An output config for the package import job.
--
-- 'clientToken', 'createPackageImportJob_clientToken' - A client token for the package import job.
newCreatePackageImportJob ::
  -- | 'jobType'
  PackageImportJobType ->
  -- | 'inputConfig'
  PackageImportJobInputConfig ->
  -- | 'outputConfig'
  PackageImportJobOutputConfig ->
  -- | 'clientToken'
  Prelude.Text ->
  CreatePackageImportJob
newCreatePackageImportJob
  pJobType_
  pInputConfig_
  pOutputConfig_
  pClientToken_ =
    CreatePackageImportJob'
      { jobTags = Prelude.Nothing,
        jobType = pJobType_,
        inputConfig = pInputConfig_,
        outputConfig = pOutputConfig_,
        clientToken = pClientToken_
      }

-- | Tags for the package import job.
createPackageImportJob_jobTags :: Lens.Lens' CreatePackageImportJob (Prelude.Maybe [JobResourceTags])
createPackageImportJob_jobTags = Lens.lens (\CreatePackageImportJob' {jobTags} -> jobTags) (\s@CreatePackageImportJob' {} a -> s {jobTags = a} :: CreatePackageImportJob) Prelude.. Lens.mapping Lens.coerced

-- | A job type for the package import job.
createPackageImportJob_jobType :: Lens.Lens' CreatePackageImportJob PackageImportJobType
createPackageImportJob_jobType = Lens.lens (\CreatePackageImportJob' {jobType} -> jobType) (\s@CreatePackageImportJob' {} a -> s {jobType = a} :: CreatePackageImportJob)

-- | An input config for the package import job.
createPackageImportJob_inputConfig :: Lens.Lens' CreatePackageImportJob PackageImportJobInputConfig
createPackageImportJob_inputConfig = Lens.lens (\CreatePackageImportJob' {inputConfig} -> inputConfig) (\s@CreatePackageImportJob' {} a -> s {inputConfig = a} :: CreatePackageImportJob)

-- | An output config for the package import job.
createPackageImportJob_outputConfig :: Lens.Lens' CreatePackageImportJob PackageImportJobOutputConfig
createPackageImportJob_outputConfig = Lens.lens (\CreatePackageImportJob' {outputConfig} -> outputConfig) (\s@CreatePackageImportJob' {} a -> s {outputConfig = a} :: CreatePackageImportJob)

-- | A client token for the package import job.
createPackageImportJob_clientToken :: Lens.Lens' CreatePackageImportJob Prelude.Text
createPackageImportJob_clientToken = Lens.lens (\CreatePackageImportJob' {clientToken} -> clientToken) (\s@CreatePackageImportJob' {} a -> s {clientToken = a} :: CreatePackageImportJob)

instance Core.AWSRequest CreatePackageImportJob where
  type
    AWSResponse CreatePackageImportJob =
      CreatePackageImportJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePackageImportJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "JobId")
      )

instance Prelude.Hashable CreatePackageImportJob where
  hashWithSalt _salt CreatePackageImportJob' {..} =
    _salt `Prelude.hashWithSalt` jobTags
      `Prelude.hashWithSalt` jobType
      `Prelude.hashWithSalt` inputConfig
      `Prelude.hashWithSalt` outputConfig
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData CreatePackageImportJob where
  rnf CreatePackageImportJob' {..} =
    Prelude.rnf jobTags
      `Prelude.seq` Prelude.rnf jobType
      `Prelude.seq` Prelude.rnf inputConfig
      `Prelude.seq` Prelude.rnf outputConfig
      `Prelude.seq` Prelude.rnf clientToken

instance Core.ToHeaders CreatePackageImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreatePackageImportJob where
  toJSON CreatePackageImportJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("JobTags" Core..=) Prelude.<$> jobTags,
            Prelude.Just ("JobType" Core..= jobType),
            Prelude.Just ("InputConfig" Core..= inputConfig),
            Prelude.Just ("OutputConfig" Core..= outputConfig),
            Prelude.Just ("ClientToken" Core..= clientToken)
          ]
      )

instance Core.ToPath CreatePackageImportJob where
  toPath = Prelude.const "/packages/import-jobs"

instance Core.ToQuery CreatePackageImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePackageImportJobResponse' smart constructor.
data CreatePackageImportJobResponse = CreatePackageImportJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The job\'s ID.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePackageImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createPackageImportJobResponse_httpStatus' - The response's http status code.
--
-- 'jobId', 'createPackageImportJobResponse_jobId' - The job\'s ID.
newCreatePackageImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'jobId'
  Prelude.Text ->
  CreatePackageImportJobResponse
newCreatePackageImportJobResponse
  pHttpStatus_
  pJobId_ =
    CreatePackageImportJobResponse'
      { httpStatus =
          pHttpStatus_,
        jobId = pJobId_
      }

-- | The response's http status code.
createPackageImportJobResponse_httpStatus :: Lens.Lens' CreatePackageImportJobResponse Prelude.Int
createPackageImportJobResponse_httpStatus = Lens.lens (\CreatePackageImportJobResponse' {httpStatus} -> httpStatus) (\s@CreatePackageImportJobResponse' {} a -> s {httpStatus = a} :: CreatePackageImportJobResponse)

-- | The job\'s ID.
createPackageImportJobResponse_jobId :: Lens.Lens' CreatePackageImportJobResponse Prelude.Text
createPackageImportJobResponse_jobId = Lens.lens (\CreatePackageImportJobResponse' {jobId} -> jobId) (\s@CreatePackageImportJobResponse' {} a -> s {jobId = a} :: CreatePackageImportJobResponse)

instance
  Prelude.NFData
    CreatePackageImportJobResponse
  where
  rnf CreatePackageImportJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf jobId
