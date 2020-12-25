{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.InitiateJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation initiates a job of the specified type, which can be a select, an archival retrieval, or a vault retrieval. For more information about using this operation, see the documentation for the underlying REST API <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-initiate-job-post.html Initiate a Job> .
module Network.AWS.Glacier.InitiateJob
  ( -- * Creating a request
    InitiateJob (..),
    mkInitiateJob,

    -- ** Request lenses
    ijAccountId,
    ijVaultName,
    ijJobParameters,

    -- * Destructuring the response
    InitiateJobResponse (..),
    mkInitiateJobResponse,

    -- ** Response lenses
    ijrrsJobId,
    ijrrsJobOutputPath,
    ijrrsLocation,
    ijrrsResponseStatus,
  )
where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Provides options for initiating an Amazon S3 Glacier job.
--
-- /See:/ 'mkInitiateJob' smart constructor.
data InitiateJob = InitiateJob'
  { -- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
    accountId :: Types.String,
    -- | The name of the vault.
    vaultName :: Types.String,
    -- | Provides options for specifying job information.
    jobParameters :: Core.Maybe Types.JobParameters
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InitiateJob' value with any optional fields omitted.
mkInitiateJob ::
  -- | 'accountId'
  Types.String ->
  -- | 'vaultName'
  Types.String ->
  InitiateJob
mkInitiateJob accountId vaultName =
  InitiateJob' {accountId, vaultName, jobParameters = Core.Nothing}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijAccountId :: Lens.Lens' InitiateJob Types.String
ijAccountId = Lens.field @"accountId"
{-# DEPRECATED ijAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijVaultName :: Lens.Lens' InitiateJob Types.String
ijVaultName = Lens.field @"vaultName"
{-# DEPRECATED ijVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

-- | Provides options for specifying job information.
--
-- /Note:/ Consider using 'jobParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijJobParameters :: Lens.Lens' InitiateJob (Core.Maybe Types.JobParameters)
ijJobParameters = Lens.field @"jobParameters"
{-# DEPRECATED ijJobParameters "Use generic-lens or generic-optics with 'jobParameters' instead." #-}

instance Core.FromJSON InitiateJob where
  toJSON InitiateJob {..} =
    Core.object
      (Core.catMaybes [("jobParameters" Core..=) Core.<$> jobParameters])

instance Core.AWSRequest InitiateJob where
  type Rs InitiateJob = InitiateJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/" Core.<> (Core.toText accountId) Core.<> ("/vaults/")
                Core.<> (Core.toText vaultName)
                Core.<> ("/jobs")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          InitiateJobResponse'
            Core.<$> (Core.parseHeaderMaybe "x-amz-job-id" h)
            Core.<*> (Core.parseHeaderMaybe "x-amz-job-output-path" h)
            Core.<*> (Core.parseHeaderMaybe "Location" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'mkInitiateJobResponse' smart constructor.
data InitiateJobResponse = InitiateJobResponse'
  { -- | The ID of the job.
    jobId :: Core.Maybe Types.String,
    -- | The path to the location of where the select results are stored.
    jobOutputPath :: Core.Maybe Types.String,
    -- | The relative URI path of the job.
    location :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InitiateJobResponse' value with any optional fields omitted.
mkInitiateJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  InitiateJobResponse
mkInitiateJobResponse responseStatus =
  InitiateJobResponse'
    { jobId = Core.Nothing,
      jobOutputPath = Core.Nothing,
      location = Core.Nothing,
      responseStatus
    }

-- | The ID of the job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrrsJobId :: Lens.Lens' InitiateJobResponse (Core.Maybe Types.String)
ijrrsJobId = Lens.field @"jobId"
{-# DEPRECATED ijrrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The path to the location of where the select results are stored.
--
-- /Note:/ Consider using 'jobOutputPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrrsJobOutputPath :: Lens.Lens' InitiateJobResponse (Core.Maybe Types.String)
ijrrsJobOutputPath = Lens.field @"jobOutputPath"
{-# DEPRECATED ijrrsJobOutputPath "Use generic-lens or generic-optics with 'jobOutputPath' instead." #-}

-- | The relative URI path of the job.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrrsLocation :: Lens.Lens' InitiateJobResponse (Core.Maybe Types.String)
ijrrsLocation = Lens.field @"location"
{-# DEPRECATED ijrrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrrsResponseStatus :: Lens.Lens' InitiateJobResponse Core.Int
ijrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ijrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
