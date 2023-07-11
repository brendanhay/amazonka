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
-- Module      : Amazonka.Amplify.CreateDeployment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a deployment for a manually deployed Amplify app. Manually
-- deployed apps are not connected to a repository.
module Amazonka.Amplify.CreateDeployment
  ( -- * Creating a Request
    CreateDeployment (..),
    newCreateDeployment,

    -- * Request Lenses
    createDeployment_fileMap,
    createDeployment_appId,
    createDeployment_branchName,

    -- * Destructuring the Response
    CreateDeploymentResponse (..),
    newCreateDeploymentResponse,

    -- * Response Lenses
    createDeploymentResponse_jobId,
    createDeploymentResponse_httpStatus,
    createDeploymentResponse_fileUploadUrls,
    createDeploymentResponse_zipUploadUrl,
  )
where

import Amazonka.Amplify.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request structure for the create a new deployment request.
--
-- /See:/ 'newCreateDeployment' smart constructor.
data CreateDeployment = CreateDeployment'
  { -- | An optional file map that contains the file name as the key and the file
    -- content md5 hash as the value. If this argument is provided, the service
    -- will generate a unique upload URL per file. Otherwise, the service will
    -- only generate a single upload URL for the zipped files.
    fileMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The unique ID for an Amplify app.
    appId :: Prelude.Text,
    -- | The name for the branch, for the job.
    branchName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileMap', 'createDeployment_fileMap' - An optional file map that contains the file name as the key and the file
-- content md5 hash as the value. If this argument is provided, the service
-- will generate a unique upload URL per file. Otherwise, the service will
-- only generate a single upload URL for the zipped files.
--
-- 'appId', 'createDeployment_appId' - The unique ID for an Amplify app.
--
-- 'branchName', 'createDeployment_branchName' - The name for the branch, for the job.
newCreateDeployment ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'branchName'
  Prelude.Text ->
  CreateDeployment
newCreateDeployment pAppId_ pBranchName_ =
  CreateDeployment'
    { fileMap = Prelude.Nothing,
      appId = pAppId_,
      branchName = pBranchName_
    }

-- | An optional file map that contains the file name as the key and the file
-- content md5 hash as the value. If this argument is provided, the service
-- will generate a unique upload URL per file. Otherwise, the service will
-- only generate a single upload URL for the zipped files.
createDeployment_fileMap :: Lens.Lens' CreateDeployment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDeployment_fileMap = Lens.lens (\CreateDeployment' {fileMap} -> fileMap) (\s@CreateDeployment' {} a -> s {fileMap = a} :: CreateDeployment) Prelude.. Lens.mapping Lens.coerced

-- | The unique ID for an Amplify app.
createDeployment_appId :: Lens.Lens' CreateDeployment Prelude.Text
createDeployment_appId = Lens.lens (\CreateDeployment' {appId} -> appId) (\s@CreateDeployment' {} a -> s {appId = a} :: CreateDeployment)

-- | The name for the branch, for the job.
createDeployment_branchName :: Lens.Lens' CreateDeployment Prelude.Text
createDeployment_branchName = Lens.lens (\CreateDeployment' {branchName} -> branchName) (\s@CreateDeployment' {} a -> s {branchName = a} :: CreateDeployment)

instance Core.AWSRequest CreateDeployment where
  type
    AWSResponse CreateDeployment =
      CreateDeploymentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDeploymentResponse'
            Prelude.<$> (x Data..?> "jobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "fileUploadUrls" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..:> "zipUploadUrl")
      )

instance Prelude.Hashable CreateDeployment where
  hashWithSalt _salt CreateDeployment' {..} =
    _salt
      `Prelude.hashWithSalt` fileMap
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` branchName

instance Prelude.NFData CreateDeployment where
  rnf CreateDeployment' {..} =
    Prelude.rnf fileMap
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf branchName

instance Data.ToHeaders CreateDeployment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDeployment where
  toJSON CreateDeployment' {..} =
    Data.object
      ( Prelude.catMaybes
          [("fileMap" Data..=) Prelude.<$> fileMap]
      )

instance Data.ToPath CreateDeployment where
  toPath CreateDeployment' {..} =
    Prelude.mconcat
      [ "/apps/",
        Data.toBS appId,
        "/branches/",
        Data.toBS branchName,
        "/deployments"
      ]

instance Data.ToQuery CreateDeployment where
  toQuery = Prelude.const Prelude.mempty

-- | The result structure for the create a new deployment request.
--
-- /See:/ 'newCreateDeploymentResponse' smart constructor.
data CreateDeploymentResponse = CreateDeploymentResponse'
  { -- | The job ID for this deployment. will supply to start deployment api.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | When the @fileMap@ argument is provided in the request, @fileUploadUrls@
    -- will contain a map of file names to upload URLs.
    fileUploadUrls :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | When the @fileMap@ argument is not provided in the request, this
    -- @zipUploadUrl@ is returned.
    zipUploadUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'createDeploymentResponse_jobId' - The job ID for this deployment. will supply to start deployment api.
--
-- 'httpStatus', 'createDeploymentResponse_httpStatus' - The response's http status code.
--
-- 'fileUploadUrls', 'createDeploymentResponse_fileUploadUrls' - When the @fileMap@ argument is provided in the request, @fileUploadUrls@
-- will contain a map of file names to upload URLs.
--
-- 'zipUploadUrl', 'createDeploymentResponse_zipUploadUrl' - When the @fileMap@ argument is not provided in the request, this
-- @zipUploadUrl@ is returned.
newCreateDeploymentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'zipUploadUrl'
  Prelude.Text ->
  CreateDeploymentResponse
newCreateDeploymentResponse
  pHttpStatus_
  pZipUploadUrl_ =
    CreateDeploymentResponse'
      { jobId = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        fileUploadUrls = Prelude.mempty,
        zipUploadUrl = pZipUploadUrl_
      }

-- | The job ID for this deployment. will supply to start deployment api.
createDeploymentResponse_jobId :: Lens.Lens' CreateDeploymentResponse (Prelude.Maybe Prelude.Text)
createDeploymentResponse_jobId = Lens.lens (\CreateDeploymentResponse' {jobId} -> jobId) (\s@CreateDeploymentResponse' {} a -> s {jobId = a} :: CreateDeploymentResponse)

-- | The response's http status code.
createDeploymentResponse_httpStatus :: Lens.Lens' CreateDeploymentResponse Prelude.Int
createDeploymentResponse_httpStatus = Lens.lens (\CreateDeploymentResponse' {httpStatus} -> httpStatus) (\s@CreateDeploymentResponse' {} a -> s {httpStatus = a} :: CreateDeploymentResponse)

-- | When the @fileMap@ argument is provided in the request, @fileUploadUrls@
-- will contain a map of file names to upload URLs.
createDeploymentResponse_fileUploadUrls :: Lens.Lens' CreateDeploymentResponse (Prelude.HashMap Prelude.Text Prelude.Text)
createDeploymentResponse_fileUploadUrls = Lens.lens (\CreateDeploymentResponse' {fileUploadUrls} -> fileUploadUrls) (\s@CreateDeploymentResponse' {} a -> s {fileUploadUrls = a} :: CreateDeploymentResponse) Prelude.. Lens.coerced

-- | When the @fileMap@ argument is not provided in the request, this
-- @zipUploadUrl@ is returned.
createDeploymentResponse_zipUploadUrl :: Lens.Lens' CreateDeploymentResponse Prelude.Text
createDeploymentResponse_zipUploadUrl = Lens.lens (\CreateDeploymentResponse' {zipUploadUrl} -> zipUploadUrl) (\s@CreateDeploymentResponse' {} a -> s {zipUploadUrl = a} :: CreateDeploymentResponse)

instance Prelude.NFData CreateDeploymentResponse where
  rnf CreateDeploymentResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf fileUploadUrls
      `Prelude.seq` Prelude.rnf zipUploadUrl
