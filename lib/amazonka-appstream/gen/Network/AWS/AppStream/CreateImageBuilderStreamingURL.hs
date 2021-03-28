{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.CreateImageBuilderStreamingURL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a URL to start an image builder streaming session.
module Network.AWS.AppStream.CreateImageBuilderStreamingURL
    (
    -- * Creating a request
      CreateImageBuilderStreamingURL (..)
    , mkCreateImageBuilderStreamingURL
    -- ** Request lenses
    , cibsurlName
    , cibsurlValidity

    -- * Destructuring the response
    , CreateImageBuilderStreamingURLResponse (..)
    , mkCreateImageBuilderStreamingURLResponse
    -- ** Response lenses
    , cibsurlrrsExpires
    , cibsurlrrsStreamingURL
    , cibsurlrrsResponseStatus
    ) where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateImageBuilderStreamingURL' smart constructor.
data CreateImageBuilderStreamingURL = CreateImageBuilderStreamingURL'
  { name :: Core.Text
    -- ^ The name of the image builder.
  , validity :: Core.Maybe Core.Integer
    -- ^ The time that the streaming URL will be valid, in seconds. Specify a value between 1 and 604800 seconds. The default is 3600 seconds.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateImageBuilderStreamingURL' value with any optional fields omitted.
mkCreateImageBuilderStreamingURL
    :: Core.Text -- ^ 'name'
    -> CreateImageBuilderStreamingURL
mkCreateImageBuilderStreamingURL name
  = CreateImageBuilderStreamingURL'{name, validity = Core.Nothing}

-- | The name of the image builder.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibsurlName :: Lens.Lens' CreateImageBuilderStreamingURL Core.Text
cibsurlName = Lens.field @"name"
{-# INLINEABLE cibsurlName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The time that the streaming URL will be valid, in seconds. Specify a value between 1 and 604800 seconds. The default is 3600 seconds.
--
-- /Note:/ Consider using 'validity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibsurlValidity :: Lens.Lens' CreateImageBuilderStreamingURL (Core.Maybe Core.Integer)
cibsurlValidity = Lens.field @"validity"
{-# INLINEABLE cibsurlValidity #-}
{-# DEPRECATED validity "Use generic-lens or generic-optics with 'validity' instead"  #-}

instance Core.ToQuery CreateImageBuilderStreamingURL where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateImageBuilderStreamingURL where
        toHeaders CreateImageBuilderStreamingURL{..}
          = Core.pure
              ("X-Amz-Target",
               "PhotonAdminProxyService.CreateImageBuilderStreamingURL")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateImageBuilderStreamingURL where
        toJSON CreateImageBuilderStreamingURL{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  ("Validity" Core..=) Core.<$> validity])

instance Core.AWSRequest CreateImageBuilderStreamingURL where
        type Rs CreateImageBuilderStreamingURL =
             CreateImageBuilderStreamingURLResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateImageBuilderStreamingURLResponse' Core.<$>
                   (x Core..:? "Expires") Core.<*> x Core..:? "StreamingURL" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateImageBuilderStreamingURLResponse' smart constructor.
data CreateImageBuilderStreamingURLResponse = CreateImageBuilderStreamingURLResponse'
  { expires :: Core.Maybe Core.NominalDiffTime
    -- ^ The elapsed time, in seconds after the Unix epoch, when this URL expires.
  , streamingURL :: Core.Maybe Core.Text
    -- ^ The URL to start the AppStream 2.0 streaming session.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateImageBuilderStreamingURLResponse' value with any optional fields omitted.
mkCreateImageBuilderStreamingURLResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateImageBuilderStreamingURLResponse
mkCreateImageBuilderStreamingURLResponse responseStatus
  = CreateImageBuilderStreamingURLResponse'{expires = Core.Nothing,
                                            streamingURL = Core.Nothing, responseStatus}

-- | The elapsed time, in seconds after the Unix epoch, when this URL expires.
--
-- /Note:/ Consider using 'expires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibsurlrrsExpires :: Lens.Lens' CreateImageBuilderStreamingURLResponse (Core.Maybe Core.NominalDiffTime)
cibsurlrrsExpires = Lens.field @"expires"
{-# INLINEABLE cibsurlrrsExpires #-}
{-# DEPRECATED expires "Use generic-lens or generic-optics with 'expires' instead"  #-}

-- | The URL to start the AppStream 2.0 streaming session.
--
-- /Note:/ Consider using 'streamingURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibsurlrrsStreamingURL :: Lens.Lens' CreateImageBuilderStreamingURLResponse (Core.Maybe Core.Text)
cibsurlrrsStreamingURL = Lens.field @"streamingURL"
{-# INLINEABLE cibsurlrrsStreamingURL #-}
{-# DEPRECATED streamingURL "Use generic-lens or generic-optics with 'streamingURL' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibsurlrrsResponseStatus :: Lens.Lens' CreateImageBuilderStreamingURLResponse Core.Int
cibsurlrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cibsurlrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
