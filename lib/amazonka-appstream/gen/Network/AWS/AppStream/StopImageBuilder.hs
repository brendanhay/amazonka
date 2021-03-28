{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.StopImageBuilder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the specified image builder.
module Network.AWS.AppStream.StopImageBuilder
    (
    -- * Creating a request
      StopImageBuilder (..)
    , mkStopImageBuilder
    -- ** Request lenses
    , sibfName

    -- * Destructuring the response
    , StopImageBuilderResponse (..)
    , mkStopImageBuilderResponse
    -- ** Response lenses
    , sibrfrsImageBuilder
    , sibrfrsResponseStatus
    ) where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopImageBuilder' smart constructor.
newtype StopImageBuilder = StopImageBuilder'
  { name :: Core.Text
    -- ^ The name of the image builder.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopImageBuilder' value with any optional fields omitted.
mkStopImageBuilder
    :: Core.Text -- ^ 'name'
    -> StopImageBuilder
mkStopImageBuilder name = StopImageBuilder'{name}

-- | The name of the image builder.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sibfName :: Lens.Lens' StopImageBuilder Core.Text
sibfName = Lens.field @"name"
{-# INLINEABLE sibfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery StopImageBuilder where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopImageBuilder where
        toHeaders StopImageBuilder{..}
          = Core.pure
              ("X-Amz-Target", "PhotonAdminProxyService.StopImageBuilder")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopImageBuilder where
        toJSON StopImageBuilder{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest StopImageBuilder where
        type Rs StopImageBuilder = StopImageBuilderResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StopImageBuilderResponse' Core.<$>
                   (x Core..:? "ImageBuilder") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopImageBuilderResponse' smart constructor.
data StopImageBuilderResponse = StopImageBuilderResponse'
  { imageBuilder :: Core.Maybe Types.ImageBuilder
    -- ^ Information about the image builder.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StopImageBuilderResponse' value with any optional fields omitted.
mkStopImageBuilderResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopImageBuilderResponse
mkStopImageBuilderResponse responseStatus
  = StopImageBuilderResponse'{imageBuilder = Core.Nothing,
                              responseStatus}

-- | Information about the image builder.
--
-- /Note:/ Consider using 'imageBuilder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sibrfrsImageBuilder :: Lens.Lens' StopImageBuilderResponse (Core.Maybe Types.ImageBuilder)
sibrfrsImageBuilder = Lens.field @"imageBuilder"
{-# INLINEABLE sibrfrsImageBuilder #-}
{-# DEPRECATED imageBuilder "Use generic-lens or generic-optics with 'imageBuilder' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sibrfrsResponseStatus :: Lens.Lens' StopImageBuilderResponse Core.Int
sibrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sibrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
