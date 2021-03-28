{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.ListCuratedEnvironmentImages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about Docker images that are managed by AWS CodeBuild.
module Network.AWS.CodeBuild.ListCuratedEnvironmentImages
    (
    -- * Creating a request
      ListCuratedEnvironmentImages (..)
    , mkListCuratedEnvironmentImages

    -- * Destructuring the response
    , ListCuratedEnvironmentImagesResponse (..)
    , mkListCuratedEnvironmentImagesResponse
    -- ** Response lenses
    , lceirrsPlatforms
    , lceirrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListCuratedEnvironmentImages' smart constructor.
data ListCuratedEnvironmentImages = ListCuratedEnvironmentImages'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCuratedEnvironmentImages' value with any optional fields omitted.
mkListCuratedEnvironmentImages
    :: ListCuratedEnvironmentImages
mkListCuratedEnvironmentImages = ListCuratedEnvironmentImages'

instance Core.ToQuery ListCuratedEnvironmentImages where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListCuratedEnvironmentImages where
        toHeaders ListCuratedEnvironmentImages{..}
          = Core.pure
              ("X-Amz-Target", "CodeBuild_20161006.ListCuratedEnvironmentImages")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListCuratedEnvironmentImages where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest ListCuratedEnvironmentImages where
        type Rs ListCuratedEnvironmentImages =
             ListCuratedEnvironmentImagesResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListCuratedEnvironmentImagesResponse' Core.<$>
                   (x Core..:? "platforms") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListCuratedEnvironmentImagesResponse' smart constructor.
data ListCuratedEnvironmentImagesResponse = ListCuratedEnvironmentImagesResponse'
  { platforms :: Core.Maybe [Types.EnvironmentPlatform]
    -- ^ Information about supported platforms for Docker images that are managed by AWS CodeBuild.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCuratedEnvironmentImagesResponse' value with any optional fields omitted.
mkListCuratedEnvironmentImagesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListCuratedEnvironmentImagesResponse
mkListCuratedEnvironmentImagesResponse responseStatus
  = ListCuratedEnvironmentImagesResponse'{platforms = Core.Nothing,
                                          responseStatus}

-- | Information about supported platforms for Docker images that are managed by AWS CodeBuild.
--
-- /Note:/ Consider using 'platforms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lceirrsPlatforms :: Lens.Lens' ListCuratedEnvironmentImagesResponse (Core.Maybe [Types.EnvironmentPlatform])
lceirrsPlatforms = Lens.field @"platforms"
{-# INLINEABLE lceirrsPlatforms #-}
{-# DEPRECATED platforms "Use generic-lens or generic-optics with 'platforms' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lceirrsResponseStatus :: Lens.Lens' ListCuratedEnvironmentImagesResponse Core.Int
lceirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lceirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
