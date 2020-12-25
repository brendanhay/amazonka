{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the information about an origin access identity.
module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity
  ( -- * Creating a request
    GetCloudFrontOriginAccessIdentity (..),
    mkGetCloudFrontOriginAccessIdentity,

    -- ** Request lenses
    gcfoaiId,

    -- * Destructuring the response
    GetCloudFrontOriginAccessIdentityResponse (..),
    mkGetCloudFrontOriginAccessIdentityResponse,

    -- ** Response lenses
    gcfoairrsCloudFrontOriginAccessIdentity,
    gcfoairrsETag,
    gcfoairrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to get an origin access identity's information.
--
-- /See:/ 'mkGetCloudFrontOriginAccessIdentity' smart constructor.
newtype GetCloudFrontOriginAccessIdentity = GetCloudFrontOriginAccessIdentity'
  { -- | The identity's ID.
    id :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetCloudFrontOriginAccessIdentity' value with any optional fields omitted.
mkGetCloudFrontOriginAccessIdentity ::
  -- | 'id'
  Types.String ->
  GetCloudFrontOriginAccessIdentity
mkGetCloudFrontOriginAccessIdentity id =
  GetCloudFrontOriginAccessIdentity' {id}

-- | The identity's ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfoaiId :: Lens.Lens' GetCloudFrontOriginAccessIdentity Types.String
gcfoaiId = Lens.field @"id"
{-# DEPRECATED gcfoaiId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.AWSRequest GetCloudFrontOriginAccessIdentity where
  type
    Rs GetCloudFrontOriginAccessIdentity =
      GetCloudFrontOriginAccessIdentityResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2020-05-31/origin-access-identity/cloudfront/"
                Core.<> (Core.toText id)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetCloudFrontOriginAccessIdentityResponse'
            Core.<$> (Core.parseXML x)
            Core.<*> (Core.parseHeaderMaybe "ETag" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkGetCloudFrontOriginAccessIdentityResponse' smart constructor.
data GetCloudFrontOriginAccessIdentityResponse = GetCloudFrontOriginAccessIdentityResponse'
  { -- | The origin access identity's information.
    cloudFrontOriginAccessIdentity :: Core.Maybe Types.CloudFrontOriginAccessIdentity,
    -- | The current version of the origin access identity's information. For example: @E2QWRUHAPOMQZL@ .
    eTag :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCloudFrontOriginAccessIdentityResponse' value with any optional fields omitted.
mkGetCloudFrontOriginAccessIdentityResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetCloudFrontOriginAccessIdentityResponse
mkGetCloudFrontOriginAccessIdentityResponse responseStatus =
  GetCloudFrontOriginAccessIdentityResponse'
    { cloudFrontOriginAccessIdentity =
        Core.Nothing,
      eTag = Core.Nothing,
      responseStatus
    }

-- | The origin access identity's information.
--
-- /Note:/ Consider using 'cloudFrontOriginAccessIdentity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfoairrsCloudFrontOriginAccessIdentity :: Lens.Lens' GetCloudFrontOriginAccessIdentityResponse (Core.Maybe Types.CloudFrontOriginAccessIdentity)
gcfoairrsCloudFrontOriginAccessIdentity = Lens.field @"cloudFrontOriginAccessIdentity"
{-# DEPRECATED gcfoairrsCloudFrontOriginAccessIdentity "Use generic-lens or generic-optics with 'cloudFrontOriginAccessIdentity' instead." #-}

-- | The current version of the origin access identity's information. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfoairrsETag :: Lens.Lens' GetCloudFrontOriginAccessIdentityResponse (Core.Maybe Types.String)
gcfoairrsETag = Lens.field @"eTag"
{-# DEPRECATED gcfoairrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfoairrsResponseStatus :: Lens.Lens' GetCloudFrontOriginAccessIdentityResponse Core.Int
gcfoairrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcfoairrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
