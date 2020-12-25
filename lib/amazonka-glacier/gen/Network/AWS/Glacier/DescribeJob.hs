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
    djAccountId,
    djVaultName,
    djJobId,

    -- * Destructuring the response
    Types.GlacierJobDescription (..),
    Types.mkGlacierJobDescription,

    -- ** Response lenses
    Types.gjdAction,
    Types.gjdArchiveId,
    Types.gjdArchiveSHA256TreeHash,
    Types.gjdArchiveSizeInBytes,
    Types.gjdCompleted,
    Types.gjdCompletionDate,
    Types.gjdCreationDate,
    Types.gjdInventoryRetrievalParameters,
    Types.gjdInventorySizeInBytes,
    Types.gjdJobDescription,
    Types.gjdJobId,
    Types.gjdJobOutputPath,
    Types.gjdOutputLocation,
    Types.gjdRetrievalByteRange,
    Types.gjdSHA256TreeHash,
    Types.gjdSNSTopic,
    Types.gjdSelectParameters,
    Types.gjdStatusCode,
    Types.gjdStatusMessage,
    Types.gjdTier,
    Types.gjdVaultARN,
  )
where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Provides options for retrieving a job description.
--
-- /See:/ 'mkDescribeJob' smart constructor.
data DescribeJob = DescribeJob'
  { -- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
    accountId :: Types.AccountId,
    -- | The name of the vault.
    vaultName :: Types.VaultName,
    -- | The ID of the job to describe.
    jobId :: Types.JobId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeJob' value with any optional fields omitted.
mkDescribeJob ::
  -- | 'accountId'
  Types.AccountId ->
  -- | 'vaultName'
  Types.VaultName ->
  -- | 'jobId'
  Types.JobId ->
  DescribeJob
mkDescribeJob accountId vaultName jobId =
  DescribeJob' {accountId, vaultName, jobId}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djAccountId :: Lens.Lens' DescribeJob Types.AccountId
djAccountId = Lens.field @"accountId"
{-# DEPRECATED djAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djVaultName :: Lens.Lens' DescribeJob Types.VaultName
djVaultName = Lens.field @"vaultName"
{-# DEPRECATED djVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

-- | The ID of the job to describe.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djJobId :: Lens.Lens' DescribeJob Types.JobId
djJobId = Lens.field @"jobId"
{-# DEPRECATED djJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Core.AWSRequest DescribeJob where
  type Rs DescribeJob = Types.GlacierJobDescription
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/" Core.<> (Core.toText accountId) Core.<> ("/vaults/")
                Core.<> (Core.toText vaultName)
                Core.<> ("/jobs/")
                Core.<> (Core.toText jobId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)
