{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.ResetJobBookmark
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets a bookmark entry.
module Network.AWS.Glue.ResetJobBookmark
    (
    -- * Creating a request
      ResetJobBookmark (..)
    , mkResetJobBookmark
    -- ** Request lenses
    , rjbJobName
    , rjbRunId

    -- * Destructuring the response
    , ResetJobBookmarkResponse (..)
    , mkResetJobBookmarkResponse
    -- ** Response lenses
    , rjbrrsJobBookmarkEntry
    , rjbrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkResetJobBookmark' smart constructor.
data ResetJobBookmark = ResetJobBookmark'
  { jobName :: Types.JobName
    -- ^ The name of the job in question.
  , runId :: Core.Maybe Types.RunId
    -- ^ The unique run identifier associated with this job run.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetJobBookmark' value with any optional fields omitted.
mkResetJobBookmark
    :: Types.JobName -- ^ 'jobName'
    -> ResetJobBookmark
mkResetJobBookmark jobName
  = ResetJobBookmark'{jobName, runId = Core.Nothing}

-- | The name of the job in question.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjbJobName :: Lens.Lens' ResetJobBookmark Types.JobName
rjbJobName = Lens.field @"jobName"
{-# INLINEABLE rjbJobName #-}
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'jobName' instead"  #-}

-- | The unique run identifier associated with this job run.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjbRunId :: Lens.Lens' ResetJobBookmark (Core.Maybe Types.RunId)
rjbRunId = Lens.field @"runId"
{-# INLINEABLE rjbRunId #-}
{-# DEPRECATED runId "Use generic-lens or generic-optics with 'runId' instead"  #-}

instance Core.ToQuery ResetJobBookmark where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ResetJobBookmark where
        toHeaders ResetJobBookmark{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.ResetJobBookmark") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ResetJobBookmark where
        toJSON ResetJobBookmark{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("JobName" Core..= jobName),
                  ("RunId" Core..=) Core.<$> runId])

instance Core.AWSRequest ResetJobBookmark where
        type Rs ResetJobBookmark = ResetJobBookmarkResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ResetJobBookmarkResponse' Core.<$>
                   (x Core..:? "JobBookmarkEntry") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkResetJobBookmarkResponse' smart constructor.
data ResetJobBookmarkResponse = ResetJobBookmarkResponse'
  { jobBookmarkEntry :: Core.Maybe Types.JobBookmarkEntry
    -- ^ The reset bookmark entry.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetJobBookmarkResponse' value with any optional fields omitted.
mkResetJobBookmarkResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ResetJobBookmarkResponse
mkResetJobBookmarkResponse responseStatus
  = ResetJobBookmarkResponse'{jobBookmarkEntry = Core.Nothing,
                              responseStatus}

-- | The reset bookmark entry.
--
-- /Note:/ Consider using 'jobBookmarkEntry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjbrrsJobBookmarkEntry :: Lens.Lens' ResetJobBookmarkResponse (Core.Maybe Types.JobBookmarkEntry)
rjbrrsJobBookmarkEntry = Lens.field @"jobBookmarkEntry"
{-# INLINEABLE rjbrrsJobBookmarkEntry #-}
{-# DEPRECATED jobBookmarkEntry "Use generic-lens or generic-optics with 'jobBookmarkEntry' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjbrrsResponseStatus :: Lens.Lens' ResetJobBookmarkResponse Core.Int
rjbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rjbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
