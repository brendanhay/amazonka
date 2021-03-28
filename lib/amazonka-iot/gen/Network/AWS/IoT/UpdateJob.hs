{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates supported fields of the specified job.
module Network.AWS.IoT.UpdateJob
    (
    -- * Creating a request
      UpdateJob (..)
    , mkUpdateJob
    -- ** Request lenses
    , ujJobId
    , ujAbortConfig
    , ujDescription
    , ujJobExecutionsRolloutConfig
    , ujNamespaceId
    , ujPresignedUrlConfig
    , ujTimeoutConfig

    -- * Destructuring the response
    , UpdateJobResponse (..)
    , mkUpdateJobResponse
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateJob' smart constructor.
data UpdateJob = UpdateJob'
  { jobId :: Types.JobId
    -- ^ The ID of the job to be updated.
  , abortConfig :: Core.Maybe Types.AbortConfig
    -- ^ Allows you to create criteria to abort a job.
  , description :: Core.Maybe Types.Description
    -- ^ A short text description of the job.
  , jobExecutionsRolloutConfig :: Core.Maybe Types.JobExecutionsRolloutConfig
    -- ^ Allows you to create a staged rollout of the job.
  , namespaceId :: Core.Maybe Types.NamespaceId
    -- ^ The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@ 
  , presignedUrlConfig :: Core.Maybe Types.PresignedUrlConfig
    -- ^ Configuration information for pre-signed S3 URLs.
  , timeoutConfig :: Core.Maybe Types.TimeoutConfig
    -- ^ Specifies the amount of time each device has to finish its execution of the job. The timer is started when the job execution status is set to @IN_PROGRESS@ . If the job execution status is not set to another terminal state before the time expires, it will be automatically set to @TIMED_OUT@ . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateJob' value with any optional fields omitted.
mkUpdateJob
    :: Types.JobId -- ^ 'jobId'
    -> UpdateJob
mkUpdateJob jobId
  = UpdateJob'{jobId, abortConfig = Core.Nothing,
               description = Core.Nothing,
               jobExecutionsRolloutConfig = Core.Nothing,
               namespaceId = Core.Nothing, presignedUrlConfig = Core.Nothing,
               timeoutConfig = Core.Nothing}

-- | The ID of the job to be updated.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujJobId :: Lens.Lens' UpdateJob Types.JobId
ujJobId = Lens.field @"jobId"
{-# INLINEABLE ujJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | Allows you to create criteria to abort a job.
--
-- /Note:/ Consider using 'abortConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujAbortConfig :: Lens.Lens' UpdateJob (Core.Maybe Types.AbortConfig)
ujAbortConfig = Lens.field @"abortConfig"
{-# INLINEABLE ujAbortConfig #-}
{-# DEPRECATED abortConfig "Use generic-lens or generic-optics with 'abortConfig' instead"  #-}

-- | A short text description of the job.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujDescription :: Lens.Lens' UpdateJob (Core.Maybe Types.Description)
ujDescription = Lens.field @"description"
{-# INLINEABLE ujDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Allows you to create a staged rollout of the job.
--
-- /Note:/ Consider using 'jobExecutionsRolloutConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujJobExecutionsRolloutConfig :: Lens.Lens' UpdateJob (Core.Maybe Types.JobExecutionsRolloutConfig)
ujJobExecutionsRolloutConfig = Lens.field @"jobExecutionsRolloutConfig"
{-# INLINEABLE ujJobExecutionsRolloutConfig #-}
{-# DEPRECATED jobExecutionsRolloutConfig "Use generic-lens or generic-optics with 'jobExecutionsRolloutConfig' instead"  #-}

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@ 
--
-- /Note:/ Consider using 'namespaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujNamespaceId :: Lens.Lens' UpdateJob (Core.Maybe Types.NamespaceId)
ujNamespaceId = Lens.field @"namespaceId"
{-# INLINEABLE ujNamespaceId #-}
{-# DEPRECATED namespaceId "Use generic-lens or generic-optics with 'namespaceId' instead"  #-}

-- | Configuration information for pre-signed S3 URLs.
--
-- /Note:/ Consider using 'presignedUrlConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujPresignedUrlConfig :: Lens.Lens' UpdateJob (Core.Maybe Types.PresignedUrlConfig)
ujPresignedUrlConfig = Lens.field @"presignedUrlConfig"
{-# INLINEABLE ujPresignedUrlConfig #-}
{-# DEPRECATED presignedUrlConfig "Use generic-lens or generic-optics with 'presignedUrlConfig' instead"  #-}

-- | Specifies the amount of time each device has to finish its execution of the job. The timer is started when the job execution status is set to @IN_PROGRESS@ . If the job execution status is not set to another terminal state before the time expires, it will be automatically set to @TIMED_OUT@ . 
--
-- /Note:/ Consider using 'timeoutConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujTimeoutConfig :: Lens.Lens' UpdateJob (Core.Maybe Types.TimeoutConfig)
ujTimeoutConfig = Lens.field @"timeoutConfig"
{-# INLINEABLE ujTimeoutConfig #-}
{-# DEPRECATED timeoutConfig "Use generic-lens or generic-optics with 'timeoutConfig' instead"  #-}

instance Core.ToQuery UpdateJob where
        toQuery UpdateJob{..}
          = Core.maybe Core.mempty (Core.toQueryPair "namespaceId")
              namespaceId

instance Core.ToHeaders UpdateJob where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON UpdateJob where
        toJSON UpdateJob{..}
          = Core.object
              (Core.catMaybes
                 [("abortConfig" Core..=) Core.<$> abortConfig,
                  ("description" Core..=) Core.<$> description,
                  ("jobExecutionsRolloutConfig" Core..=) Core.<$>
                    jobExecutionsRolloutConfig,
                  ("presignedUrlConfig" Core..=) Core.<$> presignedUrlConfig,
                  ("timeoutConfig" Core..=) Core.<$> timeoutConfig])

instance Core.AWSRequest UpdateJob where
        type Rs UpdateJob = UpdateJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath = "/jobs/" Core.<> Core.toText jobId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull UpdateJobResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateJobResponse' smart constructor.
data UpdateJobResponse = UpdateJobResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateJobResponse' value with any optional fields omitted.
mkUpdateJobResponse
    :: UpdateJobResponse
mkUpdateJobResponse = UpdateJobResponse'
