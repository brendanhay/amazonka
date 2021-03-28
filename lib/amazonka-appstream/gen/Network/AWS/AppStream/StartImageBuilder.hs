{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.StartImageBuilder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the specified image builder.
module Network.AWS.AppStream.StartImageBuilder
    (
    -- * Creating a request
      StartImageBuilder (..)
    , mkStartImageBuilder
    -- ** Request lenses
    , sibName
    , sibAppstreamAgentVersion

    -- * Destructuring the response
    , StartImageBuilderResponse (..)
    , mkStartImageBuilderResponse
    -- ** Response lenses
    , sibrrsImageBuilder
    , sibrrsResponseStatus
    ) where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartImageBuilder' smart constructor.
data StartImageBuilder = StartImageBuilder'
  { name :: Core.Text
    -- ^ The name of the image builder.
  , appstreamAgentVersion :: Core.Maybe Types.AppstreamAgentVersion
    -- ^ The version of the AppStream 2.0 agent to use for this image builder. To use the latest version of the AppStream 2.0 agent, specify [LATEST]. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartImageBuilder' value with any optional fields omitted.
mkStartImageBuilder
    :: Core.Text -- ^ 'name'
    -> StartImageBuilder
mkStartImageBuilder name
  = StartImageBuilder'{name, appstreamAgentVersion = Core.Nothing}

-- | The name of the image builder.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sibName :: Lens.Lens' StartImageBuilder Core.Text
sibName = Lens.field @"name"
{-# INLINEABLE sibName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The version of the AppStream 2.0 agent to use for this image builder. To use the latest version of the AppStream 2.0 agent, specify [LATEST]. 
--
-- /Note:/ Consider using 'appstreamAgentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sibAppstreamAgentVersion :: Lens.Lens' StartImageBuilder (Core.Maybe Types.AppstreamAgentVersion)
sibAppstreamAgentVersion = Lens.field @"appstreamAgentVersion"
{-# INLINEABLE sibAppstreamAgentVersion #-}
{-# DEPRECATED appstreamAgentVersion "Use generic-lens or generic-optics with 'appstreamAgentVersion' instead"  #-}

instance Core.ToQuery StartImageBuilder where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartImageBuilder where
        toHeaders StartImageBuilder{..}
          = Core.pure
              ("X-Amz-Target", "PhotonAdminProxyService.StartImageBuilder")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartImageBuilder where
        toJSON StartImageBuilder{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  ("AppstreamAgentVersion" Core..=) Core.<$> appstreamAgentVersion])

instance Core.AWSRequest StartImageBuilder where
        type Rs StartImageBuilder = StartImageBuilderResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartImageBuilderResponse' Core.<$>
                   (x Core..:? "ImageBuilder") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartImageBuilderResponse' smart constructor.
data StartImageBuilderResponse = StartImageBuilderResponse'
  { imageBuilder :: Core.Maybe Types.ImageBuilder
    -- ^ Information about the image builder.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StartImageBuilderResponse' value with any optional fields omitted.
mkStartImageBuilderResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartImageBuilderResponse
mkStartImageBuilderResponse responseStatus
  = StartImageBuilderResponse'{imageBuilder = Core.Nothing,
                               responseStatus}

-- | Information about the image builder.
--
-- /Note:/ Consider using 'imageBuilder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sibrrsImageBuilder :: Lens.Lens' StartImageBuilderResponse (Core.Maybe Types.ImageBuilder)
sibrrsImageBuilder = Lens.field @"imageBuilder"
{-# INLINEABLE sibrrsImageBuilder #-}
{-# DEPRECATED imageBuilder "Use generic-lens or generic-optics with 'imageBuilder' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sibrrsResponseStatus :: Lens.Lens' StartImageBuilderResponse Core.Int
sibrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sibrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
