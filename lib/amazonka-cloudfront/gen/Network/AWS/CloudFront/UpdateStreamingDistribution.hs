{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdateStreamingDistribution (..),
    mkUpdateStreamingDistribution,

    -- ** Request lenses
    usdStreamingDistributionConfig,
    usdId,
    usdIfMatch,

    -- * Destructuring the response
    UpdateStreamingDistributionResponse (..),
    mkUpdateStreamingDistributionResponse,

    -- ** Response lenses
    usdrrsETag,
    usdrrsStreamingDistribution,
    usdrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to update a streaming distribution.
--
-- /See:/ 'mkUpdateStreamingDistribution' smart constructor.
data UpdateStreamingDistribution = UpdateStreamingDistribution'
  { -- | The streaming distribution's configuration information.
    streamingDistributionConfig :: Types.StreamingDistributionConfig,
    -- | The streaming distribution's id.
    id :: Types.String,
    -- | The value of the @ETag@ header that you received when retrieving the streaming distribution's configuration. For example: @E2QWRUHAPOMQZL@ .
    ifMatch :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateStreamingDistribution' value with any optional fields omitted.
mkUpdateStreamingDistribution ::
  -- | 'streamingDistributionConfig'
  Types.StreamingDistributionConfig ->
  -- | 'id'
  Types.String ->
  UpdateStreamingDistribution
mkUpdateStreamingDistribution streamingDistributionConfig id =
  UpdateStreamingDistribution'
    { streamingDistributionConfig,
      id,
      ifMatch = Core.Nothing
    }

-- | The streaming distribution's configuration information.
--
-- /Note:/ Consider using 'streamingDistributionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdStreamingDistributionConfig :: Lens.Lens' UpdateStreamingDistribution Types.StreamingDistributionConfig
usdStreamingDistributionConfig = Lens.field @"streamingDistributionConfig"
{-# DEPRECATED usdStreamingDistributionConfig "Use generic-lens or generic-optics with 'streamingDistributionConfig' instead." #-}

-- | The streaming distribution's id.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdId :: Lens.Lens' UpdateStreamingDistribution Types.String
usdId = Lens.field @"id"
{-# DEPRECATED usdId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The value of the @ETag@ header that you received when retrieving the streaming distribution's configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdIfMatch :: Lens.Lens' UpdateStreamingDistribution (Core.Maybe Types.String)
usdIfMatch = Lens.field @"ifMatch"
{-# DEPRECATED usdIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

instance Core.AWSRequest UpdateStreamingDistribution where
  type
    Rs UpdateStreamingDistribution =
      UpdateStreamingDistributionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/2020-05-31/streaming-distribution/" Core.<> (Core.toText id)
                Core.<> ("/config")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.toHeaders "If-Match" ifMatch,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateStreamingDistributionResponse'
            Core.<$> (Core.parseHeaderMaybe "ETag" h)
            Core.<*> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkUpdateStreamingDistributionResponse' smart constructor.
data UpdateStreamingDistributionResponse = UpdateStreamingDistributionResponse'
  { -- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
    eTag :: Core.Maybe Types.String,
    -- | The streaming distribution's information.
    streamingDistribution :: Core.Maybe Types.StreamingDistribution,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateStreamingDistributionResponse' value with any optional fields omitted.
mkUpdateStreamingDistributionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateStreamingDistributionResponse
mkUpdateStreamingDistributionResponse responseStatus =
  UpdateStreamingDistributionResponse'
    { eTag = Core.Nothing,
      streamingDistribution = Core.Nothing,
      responseStatus
    }

-- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdrrsETag :: Lens.Lens' UpdateStreamingDistributionResponse (Core.Maybe Types.String)
usdrrsETag = Lens.field @"eTag"
{-# DEPRECATED usdrrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The streaming distribution's information.
--
-- /Note:/ Consider using 'streamingDistribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdrrsStreamingDistribution :: Lens.Lens' UpdateStreamingDistributionResponse (Core.Maybe Types.StreamingDistribution)
usdrrsStreamingDistribution = Lens.field @"streamingDistribution"
{-# DEPRECATED usdrrsStreamingDistribution "Use generic-lens or generic-optics with 'streamingDistribution' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdrrsResponseStatus :: Lens.Lens' UpdateStreamingDistributionResponse Core.Int
usdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED usdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
