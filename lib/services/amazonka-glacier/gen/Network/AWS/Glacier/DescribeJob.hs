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
-- Module      : Network.AWS.Glacier.DescribeJob
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Glacier.DescribeJob
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
    glacierJobDescription_sHA256TreeHash,
    glacierJobDescription_archiveId,
    glacierJobDescription_selectParameters,
    glacierJobDescription_jobId,
    glacierJobDescription_jobOutputPath,
    glacierJobDescription_retrievalByteRange,
    glacierJobDescription_inventoryRetrievalParameters,
    glacierJobDescription_action,
    glacierJobDescription_jobDescription,
    glacierJobDescription_sNSTopic,
    glacierJobDescription_statusMessage,
    glacierJobDescription_vaultARN,
    glacierJobDescription_outputLocation,
    glacierJobDescription_tier,
    glacierJobDescription_archiveSHA256TreeHash,
    glacierJobDescription_creationDate,
    glacierJobDescription_completed,
    glacierJobDescription_completionDate,
    glacierJobDescription_inventorySizeInBytes,
    glacierJobDescription_archiveSizeInBytes,
    glacierJobDescription_statusCode,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request =
    Request.glacierVersionHeader (Core._serviceVersion defaultService)
      Prelude.. Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable DescribeJob

instance Prelude.NFData DescribeJob

instance Core.ToHeaders DescribeJob where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeJob where
  toPath DescribeJob' {..} =
    Prelude.mconcat
      [ "/",
        Core.toBS accountId,
        "/vaults/",
        Core.toBS vaultName,
        "/jobs/",
        Core.toBS jobId
      ]

instance Core.ToQuery DescribeJob where
  toQuery = Prelude.const Prelude.mempty
