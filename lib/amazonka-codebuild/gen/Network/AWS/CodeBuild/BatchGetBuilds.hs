{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.BatchGetBuilds
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more builds.
module Network.AWS.CodeBuild.BatchGetBuilds
    (
    -- * Creating a request
      BatchGetBuilds (..)
    , mkBatchGetBuilds
    -- ** Request lenses
    , bgbIds

    -- * Destructuring the response
    , BatchGetBuildsResponse (..)
    , mkBatchGetBuildsResponse
    -- ** Response lenses
    , bgbrrsBuilds
    , bgbrrsBuildsNotFound
    , bgbrrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchGetBuilds' smart constructor.
newtype BatchGetBuilds = BatchGetBuilds'
  { ids :: Core.NonEmpty Types.NonEmptyString
    -- ^ The IDs of the builds.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetBuilds' value with any optional fields omitted.
mkBatchGetBuilds
    :: Core.NonEmpty Types.NonEmptyString -- ^ 'ids'
    -> BatchGetBuilds
mkBatchGetBuilds ids = BatchGetBuilds'{ids}

-- | The IDs of the builds.
--
-- /Note:/ Consider using 'ids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgbIds :: Lens.Lens' BatchGetBuilds (Core.NonEmpty Types.NonEmptyString)
bgbIds = Lens.field @"ids"
{-# INLINEABLE bgbIds #-}
{-# DEPRECATED ids "Use generic-lens or generic-optics with 'ids' instead"  #-}

instance Core.ToQuery BatchGetBuilds where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchGetBuilds where
        toHeaders BatchGetBuilds{..}
          = Core.pure ("X-Amz-Target", "CodeBuild_20161006.BatchGetBuilds")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchGetBuilds where
        toJSON BatchGetBuilds{..}
          = Core.object (Core.catMaybes [Core.Just ("ids" Core..= ids)])

instance Core.AWSRequest BatchGetBuilds where
        type Rs BatchGetBuilds = BatchGetBuildsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchGetBuildsResponse' Core.<$>
                   (x Core..:? "builds") Core.<*> x Core..:? "buildsNotFound" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchGetBuildsResponse' smart constructor.
data BatchGetBuildsResponse = BatchGetBuildsResponse'
  { builds :: Core.Maybe [Types.Build]
    -- ^ Information about the requested builds.
  , buildsNotFound :: Core.Maybe (Core.NonEmpty Types.NonEmptyString)
    -- ^ The IDs of builds for which information could not be found.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BatchGetBuildsResponse' value with any optional fields omitted.
mkBatchGetBuildsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchGetBuildsResponse
mkBatchGetBuildsResponse responseStatus
  = BatchGetBuildsResponse'{builds = Core.Nothing,
                            buildsNotFound = Core.Nothing, responseStatus}

-- | Information about the requested builds.
--
-- /Note:/ Consider using 'builds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgbrrsBuilds :: Lens.Lens' BatchGetBuildsResponse (Core.Maybe [Types.Build])
bgbrrsBuilds = Lens.field @"builds"
{-# INLINEABLE bgbrrsBuilds #-}
{-# DEPRECATED builds "Use generic-lens or generic-optics with 'builds' instead"  #-}

-- | The IDs of builds for which information could not be found.
--
-- /Note:/ Consider using 'buildsNotFound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgbrrsBuildsNotFound :: Lens.Lens' BatchGetBuildsResponse (Core.Maybe (Core.NonEmpty Types.NonEmptyString))
bgbrrsBuildsNotFound = Lens.field @"buildsNotFound"
{-# INLINEABLE bgbrrsBuildsNotFound #-}
{-# DEPRECATED buildsNotFound "Use generic-lens or generic-optics with 'buildsNotFound' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgbrrsResponseStatus :: Lens.Lens' BatchGetBuildsResponse Core.Int
bgbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bgbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
