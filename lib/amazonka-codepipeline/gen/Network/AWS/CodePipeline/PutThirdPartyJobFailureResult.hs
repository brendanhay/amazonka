{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.PutThirdPartyJobFailureResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents the failure of a third party job as returned to the pipeline by a job worker. Used for partner actions only.
module Network.AWS.CodePipeline.PutThirdPartyJobFailureResult
    (
    -- * Creating a request
      PutThirdPartyJobFailureResult (..)
    , mkPutThirdPartyJobFailureResult
    -- ** Request lenses
    , ptpjfrJobId
    , ptpjfrClientToken
    , ptpjfrFailureDetails

    -- * Destructuring the response
    , PutThirdPartyJobFailureResultResponse (..)
    , mkPutThirdPartyJobFailureResultResponse
    ) where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @PutThirdPartyJobFailureResult@ action.
--
-- /See:/ 'mkPutThirdPartyJobFailureResult' smart constructor.
data PutThirdPartyJobFailureResult = PutThirdPartyJobFailureResult'
  { jobId :: Types.JobId
    -- ^ The ID of the job that failed. This is the same ID returned from @PollForThirdPartyJobs@ .
  , clientToken :: Types.ClientToken
    -- ^ The clientToken portion of the clientId and clientToken pair used to verify that the calling entity is allowed access to the job and its details.
  , failureDetails :: Types.FailureDetails
    -- ^ Represents information about failure details.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutThirdPartyJobFailureResult' value with any optional fields omitted.
mkPutThirdPartyJobFailureResult
    :: Types.JobId -- ^ 'jobId'
    -> Types.ClientToken -- ^ 'clientToken'
    -> Types.FailureDetails -- ^ 'failureDetails'
    -> PutThirdPartyJobFailureResult
mkPutThirdPartyJobFailureResult jobId clientToken failureDetails
  = PutThirdPartyJobFailureResult'{jobId, clientToken,
                                   failureDetails}

-- | The ID of the job that failed. This is the same ID returned from @PollForThirdPartyJobs@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptpjfrJobId :: Lens.Lens' PutThirdPartyJobFailureResult Types.JobId
ptpjfrJobId = Lens.field @"jobId"
{-# INLINEABLE ptpjfrJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The clientToken portion of the clientId and clientToken pair used to verify that the calling entity is allowed access to the job and its details.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptpjfrClientToken :: Lens.Lens' PutThirdPartyJobFailureResult Types.ClientToken
ptpjfrClientToken = Lens.field @"clientToken"
{-# INLINEABLE ptpjfrClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | Represents information about failure details.
--
-- /Note:/ Consider using 'failureDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptpjfrFailureDetails :: Lens.Lens' PutThirdPartyJobFailureResult Types.FailureDetails
ptpjfrFailureDetails = Lens.field @"failureDetails"
{-# INLINEABLE ptpjfrFailureDetails #-}
{-# DEPRECATED failureDetails "Use generic-lens or generic-optics with 'failureDetails' instead"  #-}

instance Core.ToQuery PutThirdPartyJobFailureResult where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutThirdPartyJobFailureResult where
        toHeaders PutThirdPartyJobFailureResult{..}
          = Core.pure
              ("X-Amz-Target",
               "CodePipeline_20150709.PutThirdPartyJobFailureResult")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutThirdPartyJobFailureResult where
        toJSON PutThirdPartyJobFailureResult{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("jobId" Core..= jobId),
                  Core.Just ("clientToken" Core..= clientToken),
                  Core.Just ("failureDetails" Core..= failureDetails)])

instance Core.AWSRequest PutThirdPartyJobFailureResult where
        type Rs PutThirdPartyJobFailureResult =
             PutThirdPartyJobFailureResultResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull PutThirdPartyJobFailureResultResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutThirdPartyJobFailureResultResponse' smart constructor.
data PutThirdPartyJobFailureResultResponse = PutThirdPartyJobFailureResultResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutThirdPartyJobFailureResultResponse' value with any optional fields omitted.
mkPutThirdPartyJobFailureResultResponse
    :: PutThirdPartyJobFailureResultResponse
mkPutThirdPartyJobFailureResultResponse
  = PutThirdPartyJobFailureResultResponse'
