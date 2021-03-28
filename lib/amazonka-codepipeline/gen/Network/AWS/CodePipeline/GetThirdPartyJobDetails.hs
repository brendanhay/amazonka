{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.GetThirdPartyJobDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests the details of a job for a third party action. Used for partner actions only.
--
-- /Important:/ When this API is called, AWS CodePipeline returns temporary credentials for the S3 bucket used to store artifacts for the pipeline, if the action requires access to that S3 bucket for input or output artifacts. This API also returns any secret values defined for the action.
module Network.AWS.CodePipeline.GetThirdPartyJobDetails
    (
    -- * Creating a request
      GetThirdPartyJobDetails (..)
    , mkGetThirdPartyJobDetails
    -- ** Request lenses
    , gtpjdJobId
    , gtpjdClientToken

    -- * Destructuring the response
    , GetThirdPartyJobDetailsResponse (..)
    , mkGetThirdPartyJobDetailsResponse
    -- ** Response lenses
    , gtpjdrrsJobDetails
    , gtpjdrrsResponseStatus
    ) where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetThirdPartyJobDetails@ action.
--
-- /See:/ 'mkGetThirdPartyJobDetails' smart constructor.
data GetThirdPartyJobDetails = GetThirdPartyJobDetails'
  { jobId :: Types.ThirdPartyJobId
    -- ^ The unique system-generated ID used for identifying the job.
  , clientToken :: Types.ClientToken
    -- ^ The clientToken portion of the clientId and clientToken pair used to verify that the calling entity is allowed access to the job and its details.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetThirdPartyJobDetails' value with any optional fields omitted.
mkGetThirdPartyJobDetails
    :: Types.ThirdPartyJobId -- ^ 'jobId'
    -> Types.ClientToken -- ^ 'clientToken'
    -> GetThirdPartyJobDetails
mkGetThirdPartyJobDetails jobId clientToken
  = GetThirdPartyJobDetails'{jobId, clientToken}

-- | The unique system-generated ID used for identifying the job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtpjdJobId :: Lens.Lens' GetThirdPartyJobDetails Types.ThirdPartyJobId
gtpjdJobId = Lens.field @"jobId"
{-# INLINEABLE gtpjdJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The clientToken portion of the clientId and clientToken pair used to verify that the calling entity is allowed access to the job and its details.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtpjdClientToken :: Lens.Lens' GetThirdPartyJobDetails Types.ClientToken
gtpjdClientToken = Lens.field @"clientToken"
{-# INLINEABLE gtpjdClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

instance Core.ToQuery GetThirdPartyJobDetails where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetThirdPartyJobDetails where
        toHeaders GetThirdPartyJobDetails{..}
          = Core.pure
              ("X-Amz-Target", "CodePipeline_20150709.GetThirdPartyJobDetails")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetThirdPartyJobDetails where
        toJSON GetThirdPartyJobDetails{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("jobId" Core..= jobId),
                  Core.Just ("clientToken" Core..= clientToken)])

instance Core.AWSRequest GetThirdPartyJobDetails where
        type Rs GetThirdPartyJobDetails = GetThirdPartyJobDetailsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetThirdPartyJobDetailsResponse' Core.<$>
                   (x Core..:? "jobDetails") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @GetThirdPartyJobDetails@ action.
--
-- /See:/ 'mkGetThirdPartyJobDetailsResponse' smart constructor.
data GetThirdPartyJobDetailsResponse = GetThirdPartyJobDetailsResponse'
  { jobDetails :: Core.Maybe Types.ThirdPartyJobDetails
    -- ^ The details of the job, including any protected values defined for the job.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetThirdPartyJobDetailsResponse' value with any optional fields omitted.
mkGetThirdPartyJobDetailsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetThirdPartyJobDetailsResponse
mkGetThirdPartyJobDetailsResponse responseStatus
  = GetThirdPartyJobDetailsResponse'{jobDetails = Core.Nothing,
                                     responseStatus}

-- | The details of the job, including any protected values defined for the job.
--
-- /Note:/ Consider using 'jobDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtpjdrrsJobDetails :: Lens.Lens' GetThirdPartyJobDetailsResponse (Core.Maybe Types.ThirdPartyJobDetails)
gtpjdrrsJobDetails = Lens.field @"jobDetails"
{-# INLINEABLE gtpjdrrsJobDetails #-}
{-# DEPRECATED jobDetails "Use generic-lens or generic-optics with 'jobDetails' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtpjdrrsResponseStatus :: Lens.Lens' GetThirdPartyJobDetailsResponse Core.Int
gtpjdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtpjdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
