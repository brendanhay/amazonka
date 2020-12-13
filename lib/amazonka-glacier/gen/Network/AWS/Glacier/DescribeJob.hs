{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.DescribeJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns information about a job you previously initiated, including the job initiation date, the user who initiated the job, the job status code/message and the Amazon SNS topic to notify after Amazon S3 Glacier (Glacier) completes the job. For more information about initiating a job, see 'InitiateJob' .
--
-- A job ID will not expire for at least 24 hours after Glacier completes the job.
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
-- For more information about using this operation, see the documentation for the underlying REST API <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-describe-job-get.html Describe Job> in the /Amazon Glacier Developer Guide/ .
module Network.AWS.Glacier.DescribeJob
  ( -- * Creating a request
    DescribeJob (..),
    mkDescribeJob,

    -- ** Request lenses
    djJobId,
    djVaultName,
    djAccountId,

    -- * Destructuring the response
    GlacierJobDescription (..),
    mkGlacierJobDescription,

    -- ** Response lenses
    gjdSHA256TreeHash,
    gjdArchiveId,
    gjdSelectParameters,
    gjdJobId,
    gjdJobOutputPath,
    gjdRetrievalByteRange,
    gjdInventoryRetrievalParameters,
    gjdAction,
    gjdJobDescription,
    gjdSNSTopic,
    gjdStatusMessage,
    gjdVaultARN,
    gjdOutputLocation,
    gjdTier,
    gjdArchiveSHA256TreeHash,
    gjdCreationDate,
    gjdCompleted,
    gjdCompletionDate,
    gjdInventorySizeInBytes,
    gjdArchiveSizeInBytes,
    gjdStatusCode,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Provides options for retrieving a job description.
--
-- /See:/ 'mkDescribeJob' smart constructor.
data DescribeJob = DescribeJob'
  { -- | The ID of the job to describe.
    jobId :: Lude.Text,
    -- | The name of the vault.
    vaultName :: Lude.Text,
    -- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
    accountId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The ID of the job to describe.
-- * 'vaultName' - The name of the vault.
-- * 'accountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
mkDescribeJob ::
  -- | 'jobId'
  Lude.Text ->
  -- | 'vaultName'
  Lude.Text ->
  -- | 'accountId'
  Lude.Text ->
  DescribeJob
mkDescribeJob pJobId_ pVaultName_ pAccountId_ =
  DescribeJob'
    { jobId = pJobId_,
      vaultName = pVaultName_,
      accountId = pAccountId_
    }

-- | The ID of the job to describe.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djJobId :: Lens.Lens' DescribeJob Lude.Text
djJobId = Lens.lens (jobId :: DescribeJob -> Lude.Text) (\s a -> s {jobId = a} :: DescribeJob)
{-# DEPRECATED djJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djVaultName :: Lens.Lens' DescribeJob Lude.Text
djVaultName = Lens.lens (vaultName :: DescribeJob -> Lude.Text) (\s a -> s {vaultName = a} :: DescribeJob)
{-# DEPRECATED djVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djAccountId :: Lens.Lens' DescribeJob Lude.Text
djAccountId = Lens.lens (accountId :: DescribeJob -> Lude.Text) (\s a -> s {accountId = a} :: DescribeJob)
{-# DEPRECATED djAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

instance Lude.AWSRequest DescribeJob where
  type Rs DescribeJob = GlacierJobDescription
  request = Req.get glacierService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders DescribeJob where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeJob where
  toPath DescribeJob' {..} =
    Lude.mconcat
      [ "/",
        Lude.toBS accountId,
        "/vaults/",
        Lude.toBS vaultName,
        "/jobs/",
        Lude.toBS jobId
      ]

instance Lude.ToQuery DescribeJob where
  toQuery = Lude.const Lude.mempty
