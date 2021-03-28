{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.ReadJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ReadJob operation returns detailed information about a job.
module Network.AWS.ElasticTranscoder.ReadJob
    (
    -- * Creating a request
      ReadJob (..)
    , mkReadJob
    -- ** Request lenses
    , rjId

    -- * Destructuring the response
    , ReadJobResponse (..)
    , mkReadJobResponse
    -- ** Response lenses
    , rjrrsJob
    , rjrrsResponseStatus
    ) where

import qualified Network.AWS.ElasticTranscoder.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The @ReadJobRequest@ structure.
--
-- /See:/ 'mkReadJob' smart constructor.
newtype ReadJob = ReadJob'
  { id :: Types.Id
    -- ^ The identifier of the job for which you want to get detailed information.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ReadJob' value with any optional fields omitted.
mkReadJob
    :: Types.Id -- ^ 'id'
    -> ReadJob
mkReadJob id = ReadJob'{id}

-- | The identifier of the job for which you want to get detailed information.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjId :: Lens.Lens' ReadJob Types.Id
rjId = Lens.field @"id"
{-# INLINEABLE rjId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery ReadJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ReadJob where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ReadJob where
        type Rs ReadJob = ReadJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/2012-09-25/jobs/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ReadJobResponse' Core.<$>
                   (x Core..: "Job") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The @ReadJobResponse@ structure.
--
-- /See:/ 'mkReadJobResponse' smart constructor.
data ReadJobResponse = ReadJobResponse'
  { job :: Types.Job'
    -- ^ A section of the response body that provides information about the job.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReadJobResponse' value with any optional fields omitted.
mkReadJobResponse
    :: Types.Job' -- ^ 'job'
    -> Core.Int -- ^ 'responseStatus'
    -> ReadJobResponse
mkReadJobResponse job responseStatus
  = ReadJobResponse'{job, responseStatus}

-- | A section of the response body that provides information about the job.
--
-- /Note:/ Consider using 'job' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjrrsJob :: Lens.Lens' ReadJobResponse Types.Job'
rjrrsJob = Lens.field @"job"
{-# INLINEABLE rjrrsJob #-}
{-# DEPRECATED job "Use generic-lens or generic-optics with 'job' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjrrsResponseStatus :: Lens.Lens' ReadJobResponse Core.Int
rjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
