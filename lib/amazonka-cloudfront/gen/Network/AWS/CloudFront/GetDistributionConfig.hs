{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetDistributionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the configuration information about a distribution.
module Network.AWS.CloudFront.GetDistributionConfig
  ( -- * Creating a request
    GetDistributionConfig (..),
    mkGetDistributionConfig,

    -- ** Request lenses
    gdcId,

    -- * Destructuring the response
    GetDistributionConfigResponse (..),
    mkGetDistributionConfigResponse,

    -- ** Response lenses
    gdcrrsDistributionConfig,
    gdcrrsETag,
    gdcrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to get a distribution configuration.
--
-- /See:/ 'mkGetDistributionConfig' smart constructor.
newtype GetDistributionConfig = GetDistributionConfig'
  { -- | The distribution's ID. If the ID is empty, an empty distribution configuration is returned.
    id :: Types.Id
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDistributionConfig' value with any optional fields omitted.
mkGetDistributionConfig ::
  -- | 'id'
  Types.Id ->
  GetDistributionConfig
mkGetDistributionConfig id = GetDistributionConfig' {id}

-- | The distribution's ID. If the ID is empty, an empty distribution configuration is returned.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcId :: Lens.Lens' GetDistributionConfig Types.Id
gdcId = Lens.field @"id"
{-# DEPRECATED gdcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.AWSRequest GetDistributionConfig where
  type Rs GetDistributionConfig = GetDistributionConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2020-05-31/distribution/" Core.<> (Core.toText id)
                Core.<> ("/config")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetDistributionConfigResponse'
            Core.<$> (Core.parseXML x)
            Core.<*> (Core.parseHeaderMaybe "ETag" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkGetDistributionConfigResponse' smart constructor.
data GetDistributionConfigResponse = GetDistributionConfigResponse'
  { -- | The distribution's configuration information.
    distributionConfig :: Core.Maybe Types.DistributionConfig,
    -- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
    eTag :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDistributionConfigResponse' value with any optional fields omitted.
mkGetDistributionConfigResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDistributionConfigResponse
mkGetDistributionConfigResponse responseStatus =
  GetDistributionConfigResponse'
    { distributionConfig = Core.Nothing,
      eTag = Core.Nothing,
      responseStatus
    }

-- | The distribution's configuration information.
--
-- /Note:/ Consider using 'distributionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrrsDistributionConfig :: Lens.Lens' GetDistributionConfigResponse (Core.Maybe Types.DistributionConfig)
gdcrrsDistributionConfig = Lens.field @"distributionConfig"
{-# DEPRECATED gdcrrsDistributionConfig "Use generic-lens or generic-optics with 'distributionConfig' instead." #-}

-- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrrsETag :: Lens.Lens' GetDistributionConfigResponse (Core.Maybe Types.String)
gdcrrsETag = Lens.field @"eTag"
{-# DEPRECATED gdcrrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrrsResponseStatus :: Lens.Lens' GetDistributionConfigResponse Core.Int
gdcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
