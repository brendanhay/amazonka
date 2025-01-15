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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    createPackageImportJob_clientToken,
    createPackageImportJob_inputConfig,
    createPackageImportJob_jobType,
    createPackageImportJob_outputConfig,

    -- * Destructuring the Response
    CreatePackageImportJobResponse (..),
    newCreatePackageImportJobResponse,

    -- * Response Lenses
    createPackageImportJobResponse_httpStatus,
    createPackageImportJobResponse_jobId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePackageImportJob' smart constructor.
data CreatePackageImportJob = CreatePackageImportJob'
  { -- | Tags for the package import job.
    jobTags :: Prelude.Maybe [JobResourceTags],
    -- | A client token for the package import job.
    clientToken :: Prelude.Text,
    -- | An input config for the package import job.
    inputConfig :: PackageImportJobInputConfig,
    -- | A job type for the package import job.
    jobType :: PackageImportJobType,
    -- | An output config for the package import job.
    outputConfig :: PackageImportJobOutputConfig
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
-- 'clientToken', 'createPackageImportJob_clientToken' - A client token for the package import job.
--
-- 'inputConfig', 'createPackageImportJob_inputConfig' - An input config for the package import job.
--
-- 'jobType', 'createPackageImportJob_jobType' - A job type for the package import job.
--
-- 'outputConfig', 'createPackageImportJob_outputConfig' - An output config for the package import job.
newCreatePackageImportJob ::
  -- | 'clientToken'
  Prelude.Text ->
  -- | 'inputConfig'
  PackageImportJobInputConfig ->
  -- | 'jobType'
  PackageImportJobType ->
  -- | 'outputConfig'
  PackageImportJobOutputConfig ->
  CreatePackageImportJob
newCreatePackageImportJob
  pClientToken_
  pInputConfig_
  pJobType_
  pOutputConfig_ =
    CreatePackageImportJob'
      { jobTags = Prelude.Nothing,
        clientToken = pClientToken_,
        inputConfig = pInputConfig_,
        jobType = pJobType_,
        outputConfig = pOutputConfig_
      }

-- | Tags for the package import job.
createPackageImportJob_jobTags :: Lens.Lens' CreatePackageImportJob (Prelude.Maybe [JobResourceTags])
createPackageImportJob_jobTags = Lens.lens (\CreatePackageImportJob' {jobTags} -> jobTags) (\s@CreatePackageImportJob' {} a -> s {jobTags = a} :: CreatePackageImportJob) Prelude.. Lens.mapping Lens.coerced

-- | A client token for the package import job.
createPackageImportJob_clientToken :: Lens.Lens' CreatePackageImportJob Prelude.Text
createPackageImportJob_clientToken = Lens.lens (\CreatePackageImportJob' {clientToken} -> clientToken) (\s@CreatePackageImportJob' {} a -> s {clientToken = a} :: CreatePackageImportJob)

-- | An input config for the package import job.
createPackageImportJob_inputConfig :: Lens.Lens' CreatePackageImportJob PackageImportJobInputConfig
createPackageImportJob_inputConfig = Lens.lens (\CreatePackageImportJob' {inputConfig} -> inputConfig) (\s@CreatePackageImportJob' {} a -> s {inputConfig = a} :: CreatePackageImportJob)

-- | A job type for the package import job.
createPackageImportJob_jobType :: Lens.Lens' CreatePackageImportJob PackageImportJobType
createPackageImportJob_jobType = Lens.lens (\CreatePackageImportJob' {jobType} -> jobType) (\s@CreatePackageImportJob' {} a -> s {jobType = a} :: CreatePackageImportJob)

-- | An output config for the package import job.
createPackageImportJob_outputConfig :: Lens.Lens' CreatePackageImportJob PackageImportJobOutputConfig
createPackageImportJob_outputConfig = Lens.lens (\CreatePackageImportJob' {outputConfig} -> outputConfig) (\s@CreatePackageImportJob' {} a -> s {outputConfig = a} :: CreatePackageImportJob)

instance Core.AWSRequest CreatePackageImportJob where
  type
    AWSResponse CreatePackageImportJob =
      CreatePackageImportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePackageImportJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "JobId")
      )

instance Prelude.Hashable CreatePackageImportJob where
  hashWithSalt _salt CreatePackageImportJob' {..} =
    _salt
      `Prelude.hashWithSalt` jobTags
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` inputConfig
      `Prelude.hashWithSalt` jobType
      `Prelude.hashWithSalt` outputConfig

instance Prelude.NFData CreatePackageImportJob where
  rnf CreatePackageImportJob' {..} =
    Prelude.rnf jobTags `Prelude.seq`
      Prelude.rnf clientToken `Prelude.seq`
        Prelude.rnf inputConfig `Prelude.seq`
          Prelude.rnf jobType `Prelude.seq`
            Prelude.rnf outputConfig

instance Data.ToHeaders CreatePackageImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePackageImportJob where
  toJSON CreatePackageImportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("JobTags" Data..=) Prelude.<$> jobTags,
            Prelude.Just ("ClientToken" Data..= clientToken),
            Prelude.Just ("InputConfig" Data..= inputConfig),
            Prelude.Just ("JobType" Data..= jobType),
            Prelude.Just ("OutputConfig" Data..= outputConfig)
          ]
      )

instance Data.ToPath CreatePackageImportJob where
  toPath = Prelude.const "/packages/import-jobs"

instance Data.ToQuery CreatePackageImportJob where
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
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf jobId
