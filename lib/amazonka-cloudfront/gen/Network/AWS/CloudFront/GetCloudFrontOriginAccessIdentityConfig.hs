{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentityConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the configuration information about an origin access identity.
module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentityConfig
  ( -- * Creating a request
    GetCloudFrontOriginAccessIdentityConfig (..),
    mkGetCloudFrontOriginAccessIdentityConfig,

    -- ** Request lenses
    gcfoaicId,

    -- * Destructuring the response
    GetCloudFrontOriginAccessIdentityConfigResponse (..),
    mkGetCloudFrontOriginAccessIdentityConfigResponse,

    -- ** Response lenses
    gcfoaicrrsCloudFrontOriginAccessIdentityConfig,
    gcfoaicrrsETag,
    gcfoaicrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The origin access identity's configuration information. For more information, see <https://docs.aws.amazon.com/cloudfront/latest/APIReference/API_CloudFrontOriginAccessIdentityConfig.html CloudFrontOriginAccessIdentityConfig> .
--
-- /See:/ 'mkGetCloudFrontOriginAccessIdentityConfig' smart constructor.
newtype GetCloudFrontOriginAccessIdentityConfig = GetCloudFrontOriginAccessIdentityConfig'
  { -- | The identity's ID.
    id :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetCloudFrontOriginAccessIdentityConfig' value with any optional fields omitted.
mkGetCloudFrontOriginAccessIdentityConfig ::
  -- | 'id'
  Types.String ->
  GetCloudFrontOriginAccessIdentityConfig
mkGetCloudFrontOriginAccessIdentityConfig id =
  GetCloudFrontOriginAccessIdentityConfig' {id}

-- | The identity's ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfoaicId :: Lens.Lens' GetCloudFrontOriginAccessIdentityConfig Types.String
gcfoaicId = Lens.field @"id"
{-# DEPRECATED gcfoaicId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.AWSRequest GetCloudFrontOriginAccessIdentityConfig where
  type
    Rs GetCloudFrontOriginAccessIdentityConfig =
      GetCloudFrontOriginAccessIdentityConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2020-05-31/origin-access-identity/cloudfront/"
                Core.<> (Core.toText id)
                Core.<> ("/config")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetCloudFrontOriginAccessIdentityConfigResponse'
            Core.<$> (Core.parseXML x)
            Core.<*> (Core.parseHeaderMaybe "ETag" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkGetCloudFrontOriginAccessIdentityConfigResponse' smart constructor.
data GetCloudFrontOriginAccessIdentityConfigResponse = GetCloudFrontOriginAccessIdentityConfigResponse'
  { -- | The origin access identity's configuration information.
    cloudFrontOriginAccessIdentityConfig :: Core.Maybe Types.CloudFrontOriginAccessIdentityConfig,
    -- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
    eTag :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCloudFrontOriginAccessIdentityConfigResponse' value with any optional fields omitted.
mkGetCloudFrontOriginAccessIdentityConfigResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetCloudFrontOriginAccessIdentityConfigResponse
mkGetCloudFrontOriginAccessIdentityConfigResponse responseStatus =
  GetCloudFrontOriginAccessIdentityConfigResponse'
    { cloudFrontOriginAccessIdentityConfig =
        Core.Nothing,
      eTag = Core.Nothing,
      responseStatus
    }

-- | The origin access identity's configuration information.
--
-- /Note:/ Consider using 'cloudFrontOriginAccessIdentityConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfoaicrrsCloudFrontOriginAccessIdentityConfig :: Lens.Lens' GetCloudFrontOriginAccessIdentityConfigResponse (Core.Maybe Types.CloudFrontOriginAccessIdentityConfig)
gcfoaicrrsCloudFrontOriginAccessIdentityConfig = Lens.field @"cloudFrontOriginAccessIdentityConfig"
{-# DEPRECATED gcfoaicrrsCloudFrontOriginAccessIdentityConfig "Use generic-lens or generic-optics with 'cloudFrontOriginAccessIdentityConfig' instead." #-}

-- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfoaicrrsETag :: Lens.Lens' GetCloudFrontOriginAccessIdentityConfigResponse (Core.Maybe Types.String)
gcfoaicrrsETag = Lens.field @"eTag"
{-# DEPRECATED gcfoaicrrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfoaicrrsResponseStatus :: Lens.Lens' GetCloudFrontOriginAccessIdentityConfigResponse Core.Int
gcfoaicrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcfoaicrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
