{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.UpdateStreamingDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a streaming distribution. 
module Network.AWS.CloudFront.UpdateStreamingDistribution
    (
    -- * Creating a request
      UpdateStreamingDistribution (..)
    , mkUpdateStreamingDistribution
    -- ** Request lenses
    , usdStreamingDistributionConfig
    , usdId
    , usdIfMatch

    -- * Destructuring the response
    , UpdateStreamingDistributionResponse (..)
    , mkUpdateStreamingDistributionResponse
    -- ** Response lenses
    , usdrrsETag
    , usdrrsStreamingDistribution
    , usdrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to update a streaming distribution.
--
-- /See:/ 'mkUpdateStreamingDistribution' smart constructor.
data UpdateStreamingDistribution = UpdateStreamingDistribution'
  { streamingDistributionConfig :: Types.StreamingDistributionConfig
    -- ^ The streaming distribution's configuration information.
  , id :: Core.Text
    -- ^ The streaming distribution's id.
  , ifMatch :: Core.Maybe Core.Text
    -- ^ The value of the @ETag@ header that you received when retrieving the streaming distribution's configuration. For example: @E2QWRUHAPOMQZL@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateStreamingDistribution' value with any optional fields omitted.
mkUpdateStreamingDistribution
    :: Types.StreamingDistributionConfig -- ^ 'streamingDistributionConfig'
    -> Core.Text -- ^ 'id'
    -> UpdateStreamingDistribution
mkUpdateStreamingDistribution streamingDistributionConfig id
  = UpdateStreamingDistribution'{streamingDistributionConfig, id,
                                 ifMatch = Core.Nothing}

-- | The streaming distribution's configuration information.
--
-- /Note:/ Consider using 'streamingDistributionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdStreamingDistributionConfig :: Lens.Lens' UpdateStreamingDistribution Types.StreamingDistributionConfig
usdStreamingDistributionConfig = Lens.field @"streamingDistributionConfig"
{-# INLINEABLE usdStreamingDistributionConfig #-}
{-# DEPRECATED streamingDistributionConfig "Use generic-lens or generic-optics with 'streamingDistributionConfig' instead"  #-}

-- | The streaming distribution's id.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdId :: Lens.Lens' UpdateStreamingDistribution Core.Text
usdId = Lens.field @"id"
{-# INLINEABLE usdId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The value of the @ETag@ header that you received when retrieving the streaming distribution's configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdIfMatch :: Lens.Lens' UpdateStreamingDistribution (Core.Maybe Core.Text)
usdIfMatch = Lens.field @"ifMatch"
{-# INLINEABLE usdIfMatch #-}
{-# DEPRECATED ifMatch "Use generic-lens or generic-optics with 'ifMatch' instead"  #-}

instance Core.ToQuery UpdateStreamingDistribution where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateStreamingDistribution where
        toHeaders UpdateStreamingDistribution{..}
          = Core.toHeaders "If-Match" ifMatch

instance Core.AWSRequest UpdateStreamingDistribution where
        type Rs UpdateStreamingDistribution =
             UpdateStreamingDistributionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/2020-05-31/streaming-distribution/" Core.<> Core.toText id
                             Core.<> "/config",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 UpdateStreamingDistributionResponse' Core.<$>
                   (Core.parseHeaderMaybe "ETag" h) Core.<*> Core.parseXML x Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkUpdateStreamingDistributionResponse' smart constructor.
data UpdateStreamingDistributionResponse = UpdateStreamingDistributionResponse'
  { eTag :: Core.Maybe Core.Text
    -- ^ The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
  , streamingDistribution :: Core.Maybe Types.StreamingDistribution
    -- ^ The streaming distribution's information.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateStreamingDistributionResponse' value with any optional fields omitted.
mkUpdateStreamingDistributionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateStreamingDistributionResponse
mkUpdateStreamingDistributionResponse responseStatus
  = UpdateStreamingDistributionResponse'{eTag = Core.Nothing,
                                         streamingDistribution = Core.Nothing, responseStatus}

-- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdrrsETag :: Lens.Lens' UpdateStreamingDistributionResponse (Core.Maybe Core.Text)
usdrrsETag = Lens.field @"eTag"
{-# INLINEABLE usdrrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | The streaming distribution's information.
--
-- /Note:/ Consider using 'streamingDistribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdrrsStreamingDistribution :: Lens.Lens' UpdateStreamingDistributionResponse (Core.Maybe Types.StreamingDistribution)
usdrrsStreamingDistribution = Lens.field @"streamingDistribution"
{-# INLINEABLE usdrrsStreamingDistribution #-}
{-# DEPRECATED streamingDistribution "Use generic-lens or generic-optics with 'streamingDistribution' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdrrsResponseStatus :: Lens.Lens' UpdateStreamingDistributionResponse Core.Int
usdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE usdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
