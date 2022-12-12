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
-- Module      : Amazonka.Glacier.DescribeJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns information about a job you previously initiated,
-- including the job initiation date, the user who initiated the job, the
-- job status code\/message and the Amazon SNS topic to notify after Amazon
-- S3 Glacier (Glacier) completes the job. For more information about
-- initiating a job, see InitiateJob.
--
-- This operation enables you to check the status of your job. However, it
-- is strongly recommended that you set up an Amazon SNS topic and specify
-- it in your initiate job request so that Glacier can notify the topic
-- after it completes the job.
--
-- A job ID will not expire for at least 24 hours after Glacier completes
-- the job.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For more information about using this operation, see the documentation
-- for the underlying REST API
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-describe-job-get.html Describe Job>
-- in the /Amazon Glacier Developer Guide/.
module Amazonka.Glacier.DescribeJob
  ( -- * Creating a Request
    DescribeJob (..),
    newDescribeJob,

    -- * Request Lenses
    describeJob_accountId,
    describeJob_vaultName,
    describeJob_jobId,

    -- * Destructuring the Response
    GlacierJobDescription (..),
    newGlacierJobDescription,

    -- * Response Lenses
    glacierJobDescription_action,
    glacierJobDescription_archiveId,
    glacierJobDescription_archiveSHA256TreeHash,
    glacierJobDescription_archiveSizeInBytes,
    glacierJobDescription_completed,
    glacierJobDescription_completionDate,
    glacierJobDescription_creationDate,
    glacierJobDescription_inventoryRetrievalParameters,
    glacierJobDescription_inventorySizeInBytes,
    glacierJobDescription_jobDescription,
    glacierJobDescription_jobId,
    glacierJobDescription_jobOutputPath,
    glacierJobDescription_outputLocation,
    glacierJobDescription_retrievalByteRange,
    glacierJobDescription_sHA256TreeHash,
    glacierJobDescription_sNSTopic,
    glacierJobDescription_selectParameters,
    glacierJobDescription_statusCode,
    glacierJobDescription_statusMessage,
    glacierJobDescription_tier,
    glacierJobDescription_vaultARN,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glacier.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Provides options for retrieving a job description.
--
-- /See:/ 'newDescribeJob' smart constructor.
data DescribeJob = DescribeJob'
  { -- | The @AccountId@ value is the AWS account ID of the account that owns the
    -- vault. You can either specify an AWS account ID or optionally a single
    -- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
    -- ID associated with the credentials used to sign the request. If you use
    -- an account ID, do not include any hyphens (\'-\') in the ID.
    accountId :: Prelude.Text,
    -- | The name of the vault.
    vaultName :: Prelude.Text,
    -- | The ID of the job to describe.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'describeJob_accountId' - The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
--
-- 'vaultName', 'describeJob_vaultName' - The name of the vault.
--
-- 'jobId', 'describeJob_jobId' - The ID of the job to describe.
newDescribeJob ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'vaultName'
  Prelude.Text ->
  -- | 'jobId'
  Prelude.Text ->
  DescribeJob
newDescribeJob pAccountId_ pVaultName_ pJobId_ =
  DescribeJob'
    { accountId = pAccountId_,
      vaultName = pVaultName_,
      jobId = pJobId_
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
describeJob_accountId :: Lens.Lens' DescribeJob Prelude.Text
describeJob_accountId = Lens.lens (\DescribeJob' {accountId} -> accountId) (\s@DescribeJob' {} a -> s {accountId = a} :: DescribeJob)

-- | The name of the vault.
describeJob_vaultName :: Lens.Lens' DescribeJob Prelude.Text
describeJob_vaultName = Lens.lens (\DescribeJob' {vaultName} -> vaultName) (\s@DescribeJob' {} a -> s {vaultName = a} :: DescribeJob)

-- | The ID of the job to describe.
describeJob_jobId :: Lens.Lens' DescribeJob Prelude.Text
describeJob_jobId = Lens.lens (\DescribeJob' {jobId} -> jobId) (\s@DescribeJob' {} a -> s {jobId = a} :: DescribeJob)

instance Core.AWSRequest DescribeJob where
  type AWSResponse DescribeJob = GlacierJobDescription
  request overrides =
    Request.glacierVersionHeader (Core.version defaultService)
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable DescribeJob where
  hashWithSalt _salt DescribeJob' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` vaultName
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData DescribeJob where
  rnf DescribeJob' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf vaultName
      `Prelude.seq` Prelude.rnf jobId

instance Data.ToHeaders DescribeJob where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeJob where
  toPath DescribeJob' {..} =
    Prelude.mconcat
      [ "/",
        Data.toBS accountId,
        "/vaults/",
        Data.toBS vaultName,
        "/jobs/",
        Data.toBS jobId
      ]

instance Data.ToQuery DescribeJob where
  toQuery = Prelude.const Prelude.mempty
