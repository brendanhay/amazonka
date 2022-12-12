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
-- Module      : Amazonka.Glacier.InitiateJob
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Glacier.InitiateJob
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
    initiateJobResponse_jobId,
    initiateJobResponse_jobOutputPath,
    initiateJobResponse_location,
    initiateJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glacier.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Provides options for initiating an Amazon S3 Glacier job.
--
-- /See:/ 'newInitiateJob' smart constructor.
data InitiateJob = InitiateJob'
  { -- | Provides options for specifying job information.
    jobParameters :: Prelude.Maybe JobParameters,
    -- | The @AccountId@ value is the AWS account ID of the account that owns the
    -- vault. You can either specify an AWS account ID or optionally a single
    -- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
    -- ID associated with the credentials used to sign the request. If you use
    -- an account ID, do not include any hyphens (\'-\') in the ID.
    accountId :: Prelude.Text,
    -- | The name of the vault.
    vaultName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'vaultName'
  Prelude.Text ->
  InitiateJob
newInitiateJob pAccountId_ pVaultName_ =
  InitiateJob'
    { jobParameters = Prelude.Nothing,
      accountId = pAccountId_,
      vaultName = pVaultName_
    }

-- | Provides options for specifying job information.
initiateJob_jobParameters :: Lens.Lens' InitiateJob (Prelude.Maybe JobParameters)
initiateJob_jobParameters = Lens.lens (\InitiateJob' {jobParameters} -> jobParameters) (\s@InitiateJob' {} a -> s {jobParameters = a} :: InitiateJob)

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
initiateJob_accountId :: Lens.Lens' InitiateJob Prelude.Text
initiateJob_accountId = Lens.lens (\InitiateJob' {accountId} -> accountId) (\s@InitiateJob' {} a -> s {accountId = a} :: InitiateJob)

-- | The name of the vault.
initiateJob_vaultName :: Lens.Lens' InitiateJob Prelude.Text
initiateJob_vaultName = Lens.lens (\InitiateJob' {vaultName} -> vaultName) (\s@InitiateJob' {} a -> s {vaultName = a} :: InitiateJob)

instance Core.AWSRequest InitiateJob where
  type AWSResponse InitiateJob = InitiateJobResponse
  request overrides =
    Request.glacierVersionHeader (Core.version defaultService)
      Prelude.. Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          InitiateJobResponse'
            Prelude.<$> (h Data..#? "x-amz-job-id")
            Prelude.<*> (h Data..#? "x-amz-job-output-path")
            Prelude.<*> (h Data..#? "Location")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable InitiateJob where
  hashWithSalt _salt InitiateJob' {..} =
    _salt `Prelude.hashWithSalt` jobParameters
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` vaultName

instance Prelude.NFData InitiateJob where
  rnf InitiateJob' {..} =
    Prelude.rnf jobParameters
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf vaultName

instance Data.ToHeaders InitiateJob where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON InitiateJob where
  toJSON InitiateJob' {..} = Data.toJSON jobParameters

instance Data.ToPath InitiateJob where
  toPath InitiateJob' {..} =
    Prelude.mconcat
      [ "/",
        Data.toBS accountId,
        "/vaults/",
        Data.toBS vaultName,
        "/jobs"
      ]

instance Data.ToQuery InitiateJob where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'newInitiateJobResponse' smart constructor.
data InitiateJobResponse = InitiateJobResponse'
  { -- | The ID of the job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The path to the location of where the select results are stored.
    jobOutputPath :: Prelude.Maybe Prelude.Text,
    -- | The relative URI path of the job.
    location :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InitiateJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'initiateJobResponse_jobId' - The ID of the job.
--
-- 'jobOutputPath', 'initiateJobResponse_jobOutputPath' - The path to the location of where the select results are stored.
--
-- 'location', 'initiateJobResponse_location' - The relative URI path of the job.
--
-- 'httpStatus', 'initiateJobResponse_httpStatus' - The response's http status code.
newInitiateJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  InitiateJobResponse
newInitiateJobResponse pHttpStatus_ =
  InitiateJobResponse'
    { jobId = Prelude.Nothing,
      jobOutputPath = Prelude.Nothing,
      location = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the job.
initiateJobResponse_jobId :: Lens.Lens' InitiateJobResponse (Prelude.Maybe Prelude.Text)
initiateJobResponse_jobId = Lens.lens (\InitiateJobResponse' {jobId} -> jobId) (\s@InitiateJobResponse' {} a -> s {jobId = a} :: InitiateJobResponse)

-- | The path to the location of where the select results are stored.
initiateJobResponse_jobOutputPath :: Lens.Lens' InitiateJobResponse (Prelude.Maybe Prelude.Text)
initiateJobResponse_jobOutputPath = Lens.lens (\InitiateJobResponse' {jobOutputPath} -> jobOutputPath) (\s@InitiateJobResponse' {} a -> s {jobOutputPath = a} :: InitiateJobResponse)

-- | The relative URI path of the job.
initiateJobResponse_location :: Lens.Lens' InitiateJobResponse (Prelude.Maybe Prelude.Text)
initiateJobResponse_location = Lens.lens (\InitiateJobResponse' {location} -> location) (\s@InitiateJobResponse' {} a -> s {location = a} :: InitiateJobResponse)

-- | The response's http status code.
initiateJobResponse_httpStatus :: Lens.Lens' InitiateJobResponse Prelude.Int
initiateJobResponse_httpStatus = Lens.lens (\InitiateJobResponse' {httpStatus} -> httpStatus) (\s@InitiateJobResponse' {} a -> s {httpStatus = a} :: InitiateJobResponse)

instance Prelude.NFData InitiateJobResponse where
  rnf InitiateJobResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobOutputPath
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf httpStatus
