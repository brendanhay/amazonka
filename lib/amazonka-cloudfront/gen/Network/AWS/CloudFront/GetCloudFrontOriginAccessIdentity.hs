{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetCloudFrontOriginAccessIdentity (..)
    , mkGetCloudFrontOriginAccessIdentity
    -- ** Request lenses
    , gcfoaiId

    -- * Destructuring the response
    , GetCloudFrontOriginAccessIdentityResponse (..)
    , mkGetCloudFrontOriginAccessIdentityResponse
    -- ** Response lenses
    , gcfoairrsCloudFrontOriginAccessIdentity
    , gcfoairrsETag
    , gcfoairrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to get an origin access identity's information.
--
-- /See:/ 'mkGetCloudFrontOriginAccessIdentity' smart constructor.
newtype GetCloudFrontOriginAccessIdentity = GetCloudFrontOriginAccessIdentity'
  { id :: Core.Text
    -- ^ The identity's ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetCloudFrontOriginAccessIdentity' value with any optional fields omitted.
mkGetCloudFrontOriginAccessIdentity
    :: Core.Text -- ^ 'id'
    -> GetCloudFrontOriginAccessIdentity
mkGetCloudFrontOriginAccessIdentity id
  = GetCloudFrontOriginAccessIdentity'{id}

-- | The identity's ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfoaiId :: Lens.Lens' GetCloudFrontOriginAccessIdentity Core.Text
gcfoaiId = Lens.field @"id"
{-# INLINEABLE gcfoaiId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery GetCloudFrontOriginAccessIdentity where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetCloudFrontOriginAccessIdentity where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetCloudFrontOriginAccessIdentity where
        type Rs GetCloudFrontOriginAccessIdentity =
             GetCloudFrontOriginAccessIdentityResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2020-05-31/origin-access-identity/cloudfront/" Core.<>
                             Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetCloudFrontOriginAccessIdentityResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.parseHeaderMaybe "ETag" h Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkGetCloudFrontOriginAccessIdentityResponse' smart constructor.
data GetCloudFrontOriginAccessIdentityResponse = GetCloudFrontOriginAccessIdentityResponse'
  { cloudFrontOriginAccessIdentity :: Core.Maybe Types.CloudFrontOriginAccessIdentity
    -- ^ The origin access identity's information.
  , eTag :: Core.Maybe Core.Text
    -- ^ The current version of the origin access identity's information. For example: @E2QWRUHAPOMQZL@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCloudFrontOriginAccessIdentityResponse' value with any optional fields omitted.
mkGetCloudFrontOriginAccessIdentityResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetCloudFrontOriginAccessIdentityResponse
mkGetCloudFrontOriginAccessIdentityResponse responseStatus
  = GetCloudFrontOriginAccessIdentityResponse'{cloudFrontOriginAccessIdentity
                                                 = Core.Nothing,
                                               eTag = Core.Nothing, responseStatus}

-- | The origin access identity's information.
--
-- /Note:/ Consider using 'cloudFrontOriginAccessIdentity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfoairrsCloudFrontOriginAccessIdentity :: Lens.Lens' GetCloudFrontOriginAccessIdentityResponse (Core.Maybe Types.CloudFrontOriginAccessIdentity)
gcfoairrsCloudFrontOriginAccessIdentity = Lens.field @"cloudFrontOriginAccessIdentity"
{-# INLINEABLE gcfoairrsCloudFrontOriginAccessIdentity #-}
{-# DEPRECATED cloudFrontOriginAccessIdentity "Use generic-lens or generic-optics with 'cloudFrontOriginAccessIdentity' instead"  #-}

-- | The current version of the origin access identity's information. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfoairrsETag :: Lens.Lens' GetCloudFrontOriginAccessIdentityResponse (Core.Maybe Core.Text)
gcfoairrsETag = Lens.field @"eTag"
{-# INLINEABLE gcfoairrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfoairrsResponseStatus :: Lens.Lens' GetCloudFrontOriginAccessIdentityResponse Core.Int
gcfoairrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcfoairrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
