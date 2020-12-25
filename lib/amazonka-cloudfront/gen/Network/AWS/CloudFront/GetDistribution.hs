{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the information about a distribution.
module Network.AWS.CloudFront.GetDistribution
  ( -- * Creating a request
    GetDistribution (..),
    mkGetDistribution,

    -- ** Request lenses
    gdId,

    -- * Destructuring the response
    GetDistributionResponse (..),
    mkGetDistributionResponse,

    -- ** Response lenses
    gdrrsDistribution,
    gdrrsETag,
    gdrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to get a distribution's information.
--
-- /See:/ 'mkGetDistribution' smart constructor.
newtype GetDistribution = GetDistribution'
  { -- | The distribution's ID. If the ID is empty, an empty distribution configuration is returned.
    id :: Types.Id
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDistribution' value with any optional fields omitted.
mkGetDistribution ::
  -- | 'id'
  Types.Id ->
  GetDistribution
mkGetDistribution id = GetDistribution' {id}

-- | The distribution's ID. If the ID is empty, an empty distribution configuration is returned.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdId :: Lens.Lens' GetDistribution Types.Id
gdId = Lens.field @"id"
{-# DEPRECATED gdId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.AWSRequest GetDistribution where
  type Rs GetDistribution = GetDistributionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/2020-05-31/distribution/" Core.<> (Core.toText id)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetDistributionResponse'
            Core.<$> (Core.parseXML x)
            Core.<*> (Core.parseHeaderMaybe "ETag" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkGetDistributionResponse' smart constructor.
data GetDistributionResponse = GetDistributionResponse'
  { -- | The distribution's information.
    distribution :: Core.Maybe Types.Distribution,
    -- | The current version of the distribution's information. For example: @E2QWRUHAPOMQZL@ .
    eTag :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetDistributionResponse' value with any optional fields omitted.
mkGetDistributionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDistributionResponse
mkGetDistributionResponse responseStatus =
  GetDistributionResponse'
    { distribution = Core.Nothing,
      eTag = Core.Nothing,
      responseStatus
    }

-- | The distribution's information.
--
-- /Note:/ Consider using 'distribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsDistribution :: Lens.Lens' GetDistributionResponse (Core.Maybe Types.Distribution)
gdrrsDistribution = Lens.field @"distribution"
{-# DEPRECATED gdrrsDistribution "Use generic-lens or generic-optics with 'distribution' instead." #-}

-- | The current version of the distribution's information. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsETag :: Lens.Lens' GetDistributionResponse (Core.Maybe Types.String)
gdrrsETag = Lens.field @"eTag"
{-# DEPRECATED gdrrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsResponseStatus :: Lens.Lens' GetDistributionResponse Core.Int
gdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
