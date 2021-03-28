{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetStreamingDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specified RTMP distribution, including the distribution configuration.
module Network.AWS.CloudFront.GetStreamingDistribution
    (
    -- * Creating a request
      GetStreamingDistribution (..)
    , mkGetStreamingDistribution
    -- ** Request lenses
    , gsdId

    -- * Destructuring the response
    , GetStreamingDistributionResponse (..)
    , mkGetStreamingDistributionResponse
    -- ** Response lenses
    , gsdrrsETag
    , gsdrrsStreamingDistribution
    , gsdrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to get a streaming distribution's information.
--
-- /See:/ 'mkGetStreamingDistribution' smart constructor.
newtype GetStreamingDistribution = GetStreamingDistribution'
  { id :: Core.Text
    -- ^ The streaming distribution's ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetStreamingDistribution' value with any optional fields omitted.
mkGetStreamingDistribution
    :: Core.Text -- ^ 'id'
    -> GetStreamingDistribution
mkGetStreamingDistribution id = GetStreamingDistribution'{id}

-- | The streaming distribution's ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdId :: Lens.Lens' GetStreamingDistribution Core.Text
gsdId = Lens.field @"id"
{-# INLINEABLE gsdId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery GetStreamingDistribution where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetStreamingDistribution where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetStreamingDistribution where
        type Rs GetStreamingDistribution = GetStreamingDistributionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2020-05-31/streaming-distribution/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetStreamingDistributionResponse' Core.<$>
                   (Core.parseHeaderMaybe "ETag" h) Core.<*> Core.parseXML x Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkGetStreamingDistributionResponse' smart constructor.
data GetStreamingDistributionResponse = GetStreamingDistributionResponse'
  { eTag :: Core.Maybe Core.Text
    -- ^ The current version of the streaming distribution's information. For example: @E2QWRUHAPOMQZL@ .
  , streamingDistribution :: Core.Maybe Types.StreamingDistribution
    -- ^ The streaming distribution's information.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetStreamingDistributionResponse' value with any optional fields omitted.
mkGetStreamingDistributionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetStreamingDistributionResponse
mkGetStreamingDistributionResponse responseStatus
  = GetStreamingDistributionResponse'{eTag = Core.Nothing,
                                      streamingDistribution = Core.Nothing, responseStatus}

-- | The current version of the streaming distribution's information. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsETag :: Lens.Lens' GetStreamingDistributionResponse (Core.Maybe Core.Text)
gsdrrsETag = Lens.field @"eTag"
{-# INLINEABLE gsdrrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | The streaming distribution's information.
--
-- /Note:/ Consider using 'streamingDistribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsStreamingDistribution :: Lens.Lens' GetStreamingDistributionResponse (Core.Maybe Types.StreamingDistribution)
gsdrrsStreamingDistribution = Lens.field @"streamingDistribution"
{-# INLINEABLE gsdrrsStreamingDistribution #-}
{-# DEPRECATED streamingDistribution "Use generic-lens or generic-optics with 'streamingDistribution' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsResponseStatus :: Lens.Lens' GetStreamingDistributionResponse Core.Int
gsdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gsdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
