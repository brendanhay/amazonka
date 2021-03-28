{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.InvalidateProjectCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the cache for a project.
module Network.AWS.CodeBuild.InvalidateProjectCache
    (
    -- * Creating a request
      InvalidateProjectCache (..)
    , mkInvalidateProjectCache
    -- ** Request lenses
    , ipcProjectName

    -- * Destructuring the response
    , InvalidateProjectCacheResponse (..)
    , mkInvalidateProjectCacheResponse
    -- ** Response lenses
    , ipcrrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkInvalidateProjectCache' smart constructor.
newtype InvalidateProjectCache = InvalidateProjectCache'
  { projectName :: Types.NonEmptyString
    -- ^ The name of the AWS CodeBuild build project that the cache is reset for.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InvalidateProjectCache' value with any optional fields omitted.
mkInvalidateProjectCache
    :: Types.NonEmptyString -- ^ 'projectName'
    -> InvalidateProjectCache
mkInvalidateProjectCache projectName
  = InvalidateProjectCache'{projectName}

-- | The name of the AWS CodeBuild build project that the cache is reset for.
--
-- /Note:/ Consider using 'projectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipcProjectName :: Lens.Lens' InvalidateProjectCache Types.NonEmptyString
ipcProjectName = Lens.field @"projectName"
{-# INLINEABLE ipcProjectName #-}
{-# DEPRECATED projectName "Use generic-lens or generic-optics with 'projectName' instead"  #-}

instance Core.ToQuery InvalidateProjectCache where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders InvalidateProjectCache where
        toHeaders InvalidateProjectCache{..}
          = Core.pure
              ("X-Amz-Target", "CodeBuild_20161006.InvalidateProjectCache")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON InvalidateProjectCache where
        toJSON InvalidateProjectCache{..}
          = Core.object
              (Core.catMaybes [Core.Just ("projectName" Core..= projectName)])

instance Core.AWSRequest InvalidateProjectCache where
        type Rs InvalidateProjectCache = InvalidateProjectCacheResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 InvalidateProjectCacheResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkInvalidateProjectCacheResponse' smart constructor.
newtype InvalidateProjectCacheResponse = InvalidateProjectCacheResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InvalidateProjectCacheResponse' value with any optional fields omitted.
mkInvalidateProjectCacheResponse
    :: Core.Int -- ^ 'responseStatus'
    -> InvalidateProjectCacheResponse
mkInvalidateProjectCacheResponse responseStatus
  = InvalidateProjectCacheResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipcrrsResponseStatus :: Lens.Lens' InvalidateProjectCacheResponse Core.Int
ipcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ipcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
