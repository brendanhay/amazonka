{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.CancelJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The CancelJob operation cancels an unfinished job.
module Network.AWS.ElasticTranscoder.CancelJob
    (
    -- * Creating a request
      CancelJob (..)
    , mkCancelJob
    -- ** Request lenses
    , cjId

    -- * Destructuring the response
    , CancelJobResponse (..)
    , mkCancelJobResponse
    -- ** Response lenses
    , cjrfrsResponseStatus
    ) where

import qualified Network.AWS.ElasticTranscoder.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The @CancelJobRequest@ structure.
--
-- /See:/ 'mkCancelJob' smart constructor.
newtype CancelJob = CancelJob'
  { id :: Types.Id
    -- ^ The identifier of the job that you want to cancel.
--
-- To get a list of the jobs (including their @jobId@ ) that have a status of @Submitted@ , use the 'ListJobsByStatus' API action.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelJob' value with any optional fields omitted.
mkCancelJob
    :: Types.Id -- ^ 'id'
    -> CancelJob
mkCancelJob id = CancelJob'{id}

-- | The identifier of the job that you want to cancel.
--
-- To get a list of the jobs (including their @jobId@ ) that have a status of @Submitted@ , use the 'ListJobsByStatus' API action.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjId :: Lens.Lens' CancelJob Types.Id
cjId = Lens.field @"id"
{-# INLINEABLE cjId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery CancelJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CancelJob where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CancelJob where
        type Rs CancelJob = CancelJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/2012-09-25/jobs/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 CancelJobResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | The response body contains a JSON object. If the job is successfully canceled, the value of @Success@ is @true@ .
--
-- /See:/ 'mkCancelJobResponse' smart constructor.
newtype CancelJobResponse = CancelJobResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelJobResponse' value with any optional fields omitted.
mkCancelJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CancelJobResponse
mkCancelJobResponse responseStatus
  = CancelJobResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrfrsResponseStatus :: Lens.Lens' CancelJobResponse Core.Int
cjrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cjrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
