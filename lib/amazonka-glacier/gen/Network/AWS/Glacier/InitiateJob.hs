{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      InitiateJob (..)
    , mkInitiateJob
    -- ** Request lenses
    , ijAccountId
    , ijVaultName
    , ijJobParameters

    -- * Destructuring the response
    , InitiateJobResponse (..)
    , mkInitiateJobResponse
    -- ** Response lenses
    , ijrrsJobId
    , ijrrsJobOutputPath
    , ijrrsLocation
    , ijrrsResponseStatus
    ) where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Provides options for initiating an Amazon S3 Glacier job.
--
-- /See:/ 'mkInitiateJob' smart constructor.
data InitiateJob = InitiateJob'
  { accountId :: Core.Text
    -- ^ The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
  , vaultName :: Core.Text
    -- ^ The name of the vault.
  , jobParameters :: Core.Maybe Types.JobParameters
    -- ^ Provides options for specifying job information.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InitiateJob' value with any optional fields omitted.
mkInitiateJob
    :: Core.Text -- ^ 'accountId'
    -> Core.Text -- ^ 'vaultName'
    -> InitiateJob
mkInitiateJob accountId vaultName
  = InitiateJob'{accountId, vaultName, jobParameters = Core.Nothing}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijAccountId :: Lens.Lens' InitiateJob Core.Text
ijAccountId = Lens.field @"accountId"
{-# INLINEABLE ijAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijVaultName :: Lens.Lens' InitiateJob Core.Text
ijVaultName = Lens.field @"vaultName"
{-# INLINEABLE ijVaultName #-}
{-# DEPRECATED vaultName "Use generic-lens or generic-optics with 'vaultName' instead"  #-}

-- | Provides options for specifying job information.
--
-- /Note:/ Consider using 'jobParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijJobParameters :: Lens.Lens' InitiateJob (Core.Maybe Types.JobParameters)
ijJobParameters = Lens.field @"jobParameters"
{-# INLINEABLE ijJobParameters #-}
{-# DEPRECATED jobParameters "Use generic-lens or generic-optics with 'jobParameters' instead"  #-}

instance Core.ToQuery InitiateJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders InitiateJob where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON InitiateJob where
        toJSON InitiateJob{..}
          = Core.object
              (Core.catMaybes [("jobParameters" Core..=) Core.<$> jobParameters])

instance Core.AWSRequest InitiateJob where
        type Rs InitiateJob = InitiateJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/" Core.<> Core.toText accountId Core.<> "/vaults/" Core.<>
                             Core.toText vaultName
                             Core.<> "/jobs",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 InitiateJobResponse' Core.<$>
                   (Core.parseHeaderMaybe "x-amz-job-id" h) Core.<*>
                     Core.parseHeaderMaybe "x-amz-job-output-path" h
                     Core.<*> Core.parseHeaderMaybe "Location" h
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'mkInitiateJobResponse' smart constructor.
data InitiateJobResponse = InitiateJobResponse'
  { jobId :: Core.Maybe Core.Text
    -- ^ The ID of the job.
  , jobOutputPath :: Core.Maybe Core.Text
    -- ^ The path to the location of where the select results are stored.
  , location :: Core.Maybe Core.Text
    -- ^ The relative URI path of the job.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InitiateJobResponse' value with any optional fields omitted.
mkInitiateJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> InitiateJobResponse
mkInitiateJobResponse responseStatus
  = InitiateJobResponse'{jobId = Core.Nothing,
                         jobOutputPath = Core.Nothing, location = Core.Nothing,
                         responseStatus}

-- | The ID of the job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrrsJobId :: Lens.Lens' InitiateJobResponse (Core.Maybe Core.Text)
ijrrsJobId = Lens.field @"jobId"
{-# INLINEABLE ijrrsJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The path to the location of where the select results are stored.
--
-- /Note:/ Consider using 'jobOutputPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrrsJobOutputPath :: Lens.Lens' InitiateJobResponse (Core.Maybe Core.Text)
ijrrsJobOutputPath = Lens.field @"jobOutputPath"
{-# INLINEABLE ijrrsJobOutputPath #-}
{-# DEPRECATED jobOutputPath "Use generic-lens or generic-optics with 'jobOutputPath' instead"  #-}

-- | The relative URI path of the job.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrrsLocation :: Lens.Lens' InitiateJobResponse (Core.Maybe Core.Text)
ijrrsLocation = Lens.field @"location"
{-# INLINEABLE ijrrsLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrrsResponseStatus :: Lens.Lens' InitiateJobResponse Core.Int
ijrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ijrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
