{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetStreamingDistribution (..),
    mkGetStreamingDistribution,

    -- ** Request lenses
    gsdId,

    -- * Destructuring the response
    GetStreamingDistributionResponse (..),
    mkGetStreamingDistributionResponse,

    -- ** Response lenses
    gsdrrsETag,
    gsdrrsStreamingDistribution,
    gsdrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to get a streaming distribution's information.
--
-- /See:/ 'mkGetStreamingDistribution' smart constructor.
newtype GetStreamingDistribution = GetStreamingDistribution'
  { -- | The streaming distribution's ID.
    id :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetStreamingDistribution' value with any optional fields omitted.
mkGetStreamingDistribution ::
  -- | 'id'
  Types.String ->
  GetStreamingDistribution
mkGetStreamingDistribution id = GetStreamingDistribution' {id}

-- | The streaming distribution's ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdId :: Lens.Lens' GetStreamingDistribution Types.String
gsdId = Lens.field @"id"
{-# DEPRECATED gsdId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.AWSRequest GetStreamingDistribution where
  type Rs GetStreamingDistribution = GetStreamingDistributionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/2020-05-31/streaming-distribution/" Core.<> (Core.toText id)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetStreamingDistributionResponse'
            Core.<$> (Core.parseHeaderMaybe "ETag" h)
            Core.<*> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkGetStreamingDistributionResponse' smart constructor.
data GetStreamingDistributionResponse = GetStreamingDistributionResponse'
  { -- | The current version of the streaming distribution's information. For example: @E2QWRUHAPOMQZL@ .
    eTag :: Core.Maybe Types.String,
    -- | The streaming distribution's information.
    streamingDistribution :: Core.Maybe Types.StreamingDistribution,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetStreamingDistributionResponse' value with any optional fields omitted.
mkGetStreamingDistributionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetStreamingDistributionResponse
mkGetStreamingDistributionResponse responseStatus =
  GetStreamingDistributionResponse'
    { eTag = Core.Nothing,
      streamingDistribution = Core.Nothing,
      responseStatus
    }

-- | The current version of the streaming distribution's information. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsETag :: Lens.Lens' GetStreamingDistributionResponse (Core.Maybe Types.String)
gsdrrsETag = Lens.field @"eTag"
{-# DEPRECATED gsdrrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The streaming distribution's information.
--
-- /Note:/ Consider using 'streamingDistribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsStreamingDistribution :: Lens.Lens' GetStreamingDistributionResponse (Core.Maybe Types.StreamingDistribution)
gsdrrsStreamingDistribution = Lens.field @"streamingDistribution"
{-# DEPRECATED gsdrrsStreamingDistribution "Use generic-lens or generic-optics with 'streamingDistribution' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsResponseStatus :: Lens.Lens' GetStreamingDistributionResponse Core.Int
gsdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gsdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
