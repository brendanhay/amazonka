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
-- Module      : Network.AWS.Glacier.InitiateJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation initiates a job of the specified type, which can be a
-- select, an archival retrieval, or a vault retrieval. For more
-- information about using this operation, see the documentation for the
-- underlying REST API
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-initiate-job-post.html Initiate a Job>.
module Network.AWS.Glacier.InitiateJob
  ( -- * Creating a Request
    InitiateJob (..),
    newInitiateJob,

    -- * Request Lenses
    initiateJob_jobParameters,
    initiateJob_accountId,
    initiateJob_vaultName,

    -- * Destructuring the Response
    InitiateJobResponse (..),
    newInitiateJobResponse,

    -- * Response Lenses
    initiateJobResponse_jobOutputPath,
    initiateJobResponse_location,
    initiateJobResponse_jobId,
    initiateJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Provides options for initiating an Amazon S3 Glacier job.
--
-- /See:/ 'newInitiateJob' smart constructor.
data InitiateJob = InitiateJob'
  { -- | Provides options for specifying job information.
    jobParameters :: Core.Maybe JobParameters,
    -- | The @AccountId@ value is the AWS account ID of the account that owns the
    -- vault. You can either specify an AWS account ID or optionally a single
    -- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
    -- ID associated with the credentials used to sign the request. If you use
    -- an account ID, do not include any hyphens (\'-\') in the ID.
    accountId :: Core.Text,
    -- | The name of the vault.
    vaultName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InitiateJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobParameters', 'initiateJob_jobParameters' - Provides options for specifying job information.
--
-- 'accountId', 'initiateJob_accountId' - The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
--
-- 'vaultName', 'initiateJob_vaultName' - The name of the vault.
newInitiateJob ::
  -- | 'accountId'
  Core.Text ->
  -- | 'vaultName'
  Core.Text ->
  InitiateJob
newInitiateJob pAccountId_ pVaultName_ =
  InitiateJob'
    { jobParameters = Core.Nothing,
      accountId = pAccountId_,
      vaultName = pVaultName_
    }

-- | Provides options for specifying job information.
initiateJob_jobParameters :: Lens.Lens' InitiateJob (Core.Maybe JobParameters)
initiateJob_jobParameters = Lens.lens (\InitiateJob' {jobParameters} -> jobParameters) (\s@InitiateJob' {} a -> s {jobParameters = a} :: InitiateJob)

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
initiateJob_accountId :: Lens.Lens' InitiateJob Core.Text
initiateJob_accountId = Lens.lens (\InitiateJob' {accountId} -> accountId) (\s@InitiateJob' {} a -> s {accountId = a} :: InitiateJob)

-- | The name of the vault.
initiateJob_vaultName :: Lens.Lens' InitiateJob Core.Text
initiateJob_vaultName = Lens.lens (\InitiateJob' {vaultName} -> vaultName) (\s@InitiateJob' {} a -> s {vaultName = a} :: InitiateJob)

instance Core.AWSRequest InitiateJob where
  type AWSResponse InitiateJob = InitiateJobResponse
  request =
    Request.glacierVersionHeader (Core._serviceVersion defaultService)
      Core.. Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          InitiateJobResponse'
            Core.<$> (h Core..#? "x-amz-job-output-path")
            Core.<*> (h Core..#? "Location")
            Core.<*> (h Core..#? "x-amz-job-id")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable InitiateJob

instance Core.NFData InitiateJob

instance Core.ToHeaders InitiateJob where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON InitiateJob where
  toJSON InitiateJob' {..} =
    Core.object
      ( Core.catMaybes
          [("jobParameters" Core..=) Core.<$> jobParameters]
      )

instance Core.ToPath InitiateJob where
  toPath InitiateJob' {..} =
    Core.mconcat
      [ "/",
        Core.toBS accountId,
        "/vaults/",
        Core.toBS vaultName,
        "/jobs"
      ]

instance Core.ToQuery InitiateJob where
  toQuery = Core.const Core.mempty

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'newInitiateJobResponse' smart constructor.
data InitiateJobResponse = InitiateJobResponse'
  { -- | The path to the location of where the select results are stored.
    jobOutputPath :: Core.Maybe Core.Text,
    -- | The relative URI path of the job.
    location :: Core.Maybe Core.Text,
    -- | The ID of the job.
    jobId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InitiateJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobOutputPath', 'initiateJobResponse_jobOutputPath' - The path to the location of where the select results are stored.
--
-- 'location', 'initiateJobResponse_location' - The relative URI path of the job.
--
-- 'jobId', 'initiateJobResponse_jobId' - The ID of the job.
--
-- 'httpStatus', 'initiateJobResponse_httpStatus' - The response's http status code.
newInitiateJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  InitiateJobResponse
newInitiateJobResponse pHttpStatus_ =
  InitiateJobResponse'
    { jobOutputPath = Core.Nothing,
      location = Core.Nothing,
      jobId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The path to the location of where the select results are stored.
initiateJobResponse_jobOutputPath :: Lens.Lens' InitiateJobResponse (Core.Maybe Core.Text)
initiateJobResponse_jobOutputPath = Lens.lens (\InitiateJobResponse' {jobOutputPath} -> jobOutputPath) (\s@InitiateJobResponse' {} a -> s {jobOutputPath = a} :: InitiateJobResponse)

-- | The relative URI path of the job.
initiateJobResponse_location :: Lens.Lens' InitiateJobResponse (Core.Maybe Core.Text)
initiateJobResponse_location = Lens.lens (\InitiateJobResponse' {location} -> location) (\s@InitiateJobResponse' {} a -> s {location = a} :: InitiateJobResponse)

-- | The ID of the job.
initiateJobResponse_jobId :: Lens.Lens' InitiateJobResponse (Core.Maybe Core.Text)
initiateJobResponse_jobId = Lens.lens (\InitiateJobResponse' {jobId} -> jobId) (\s@InitiateJobResponse' {} a -> s {jobId = a} :: InitiateJobResponse)

-- | The response's http status code.
initiateJobResponse_httpStatus :: Lens.Lens' InitiateJobResponse Core.Int
initiateJobResponse_httpStatus = Lens.lens (\InitiateJobResponse' {httpStatus} -> httpStatus) (\s@InitiateJobResponse' {} a -> s {httpStatus = a} :: InitiateJobResponse)

instance Core.NFData InitiateJobResponse
