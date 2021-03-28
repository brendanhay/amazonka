{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetJobBookmark
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information on a job bookmark entry.
module Network.AWS.Glue.GetJobBookmark
    (
    -- * Creating a request
      GetJobBookmark (..)
    , mkGetJobBookmark
    -- ** Request lenses
    , gjbJobName
    , gjbRunId

    -- * Destructuring the response
    , GetJobBookmarkResponse (..)
    , mkGetJobBookmarkResponse
    -- ** Response lenses
    , gjbrrsJobBookmarkEntry
    , gjbrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetJobBookmark' smart constructor.
data GetJobBookmark = GetJobBookmark'
  { jobName :: Types.JobName
    -- ^ The name of the job in question.
  , runId :: Core.Maybe Types.RunId
    -- ^ The unique run identifier associated with this job run.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetJobBookmark' value with any optional fields omitted.
mkGetJobBookmark
    :: Types.JobName -- ^ 'jobName'
    -> GetJobBookmark
mkGetJobBookmark jobName
  = GetJobBookmark'{jobName, runId = Core.Nothing}

-- | The name of the job in question.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjbJobName :: Lens.Lens' GetJobBookmark Types.JobName
gjbJobName = Lens.field @"jobName"
{-# INLINEABLE gjbJobName #-}
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'jobName' instead"  #-}

-- | The unique run identifier associated with this job run.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjbRunId :: Lens.Lens' GetJobBookmark (Core.Maybe Types.RunId)
gjbRunId = Lens.field @"runId"
{-# INLINEABLE gjbRunId #-}
{-# DEPRECATED runId "Use generic-lens or generic-optics with 'runId' instead"  #-}

instance Core.ToQuery GetJobBookmark where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetJobBookmark where
        toHeaders GetJobBookmark{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetJobBookmark") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetJobBookmark where
        toJSON GetJobBookmark{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("JobName" Core..= jobName),
                  ("RunId" Core..=) Core.<$> runId])

instance Core.AWSRequest GetJobBookmark where
        type Rs GetJobBookmark = GetJobBookmarkResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetJobBookmarkResponse' Core.<$>
                   (x Core..:? "JobBookmarkEntry") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetJobBookmarkResponse' smart constructor.
data GetJobBookmarkResponse = GetJobBookmarkResponse'
  { jobBookmarkEntry :: Core.Maybe Types.JobBookmarkEntry
    -- ^ A structure that defines a point that a job can resume processing.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetJobBookmarkResponse' value with any optional fields omitted.
mkGetJobBookmarkResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetJobBookmarkResponse
mkGetJobBookmarkResponse responseStatus
  = GetJobBookmarkResponse'{jobBookmarkEntry = Core.Nothing,
                            responseStatus}

-- | A structure that defines a point that a job can resume processing.
--
-- /Note:/ Consider using 'jobBookmarkEntry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjbrrsJobBookmarkEntry :: Lens.Lens' GetJobBookmarkResponse (Core.Maybe Types.JobBookmarkEntry)
gjbrrsJobBookmarkEntry = Lens.field @"jobBookmarkEntry"
{-# INLINEABLE gjbrrsJobBookmarkEntry #-}
{-# DEPRECATED jobBookmarkEntry "Use generic-lens or generic-optics with 'jobBookmarkEntry' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjbrrsResponseStatus :: Lens.Lens' GetJobBookmarkResponse Core.Int
gjbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gjbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
