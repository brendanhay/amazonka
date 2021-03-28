{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an origin access identity. 
module Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity
    (
    -- * Creating a request
      UpdateCloudFrontOriginAccessIdentity (..)
    , mkUpdateCloudFrontOriginAccessIdentity
    -- ** Request lenses
    , ucfoaiCloudFrontOriginAccessIdentityConfig
    , ucfoaiId
    , ucfoaiIfMatch

    -- * Destructuring the response
    , UpdateCloudFrontOriginAccessIdentityResponse (..)
    , mkUpdateCloudFrontOriginAccessIdentityResponse
    -- ** Response lenses
    , ucfoairrsCloudFrontOriginAccessIdentity
    , ucfoairrsETag
    , ucfoairrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to update an origin access identity.
--
-- /See:/ 'mkUpdateCloudFrontOriginAccessIdentity' smart constructor.
data UpdateCloudFrontOriginAccessIdentity = UpdateCloudFrontOriginAccessIdentity'
  { cloudFrontOriginAccessIdentityConfig :: Types.CloudFrontOriginAccessIdentityConfig
    -- ^ The identity's configuration information.
  , id :: Core.Text
    -- ^ The identity's id.
  , ifMatch :: Core.Maybe Core.Text
    -- ^ The value of the @ETag@ header that you received when retrieving the identity's configuration. For example: @E2QWRUHAPOMQZL@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCloudFrontOriginAccessIdentity' value with any optional fields omitted.
mkUpdateCloudFrontOriginAccessIdentity
    :: Types.CloudFrontOriginAccessIdentityConfig -- ^ 'cloudFrontOriginAccessIdentityConfig'
    -> Core.Text -- ^ 'id'
    -> UpdateCloudFrontOriginAccessIdentity
mkUpdateCloudFrontOriginAccessIdentity
  cloudFrontOriginAccessIdentityConfig id
  = UpdateCloudFrontOriginAccessIdentity'{cloudFrontOriginAccessIdentityConfig,
                                          id, ifMatch = Core.Nothing}

-- | The identity's configuration information.
--
-- /Note:/ Consider using 'cloudFrontOriginAccessIdentityConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfoaiCloudFrontOriginAccessIdentityConfig :: Lens.Lens' UpdateCloudFrontOriginAccessIdentity Types.CloudFrontOriginAccessIdentityConfig
ucfoaiCloudFrontOriginAccessIdentityConfig = Lens.field @"cloudFrontOriginAccessIdentityConfig"
{-# INLINEABLE ucfoaiCloudFrontOriginAccessIdentityConfig #-}
{-# DEPRECATED cloudFrontOriginAccessIdentityConfig "Use generic-lens or generic-optics with 'cloudFrontOriginAccessIdentityConfig' instead"  #-}

-- | The identity's id.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfoaiId :: Lens.Lens' UpdateCloudFrontOriginAccessIdentity Core.Text
ucfoaiId = Lens.field @"id"
{-# INLINEABLE ucfoaiId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The value of the @ETag@ header that you received when retrieving the identity's configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfoaiIfMatch :: Lens.Lens' UpdateCloudFrontOriginAccessIdentity (Core.Maybe Core.Text)
ucfoaiIfMatch = Lens.field @"ifMatch"
{-# INLINEABLE ucfoaiIfMatch #-}
{-# DEPRECATED ifMatch "Use generic-lens or generic-optics with 'ifMatch' instead"  #-}

instance Core.ToQuery UpdateCloudFrontOriginAccessIdentity where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateCloudFrontOriginAccessIdentity where
        toHeaders UpdateCloudFrontOriginAccessIdentity{..}
          = Core.toHeaders "If-Match" ifMatch

instance Core.AWSRequest UpdateCloudFrontOriginAccessIdentity where
        type Rs UpdateCloudFrontOriginAccessIdentity =
             UpdateCloudFrontOriginAccessIdentityResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/2020-05-31/origin-access-identity/cloudfront/" Core.<>
                             Core.toText id
                             Core.<> "/config",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 UpdateCloudFrontOriginAccessIdentityResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.parseHeaderMaybe "ETag" h Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkUpdateCloudFrontOriginAccessIdentityResponse' smart constructor.
data UpdateCloudFrontOriginAccessIdentityResponse = UpdateCloudFrontOriginAccessIdentityResponse'
  { cloudFrontOriginAccessIdentity :: Core.Maybe Types.CloudFrontOriginAccessIdentity
    -- ^ The origin access identity's information.
  , eTag :: Core.Maybe Core.Text
    -- ^ The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCloudFrontOriginAccessIdentityResponse' value with any optional fields omitted.
mkUpdateCloudFrontOriginAccessIdentityResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateCloudFrontOriginAccessIdentityResponse
mkUpdateCloudFrontOriginAccessIdentityResponse responseStatus
  = UpdateCloudFrontOriginAccessIdentityResponse'{cloudFrontOriginAccessIdentity
                                                    = Core.Nothing,
                                                  eTag = Core.Nothing, responseStatus}

-- | The origin access identity's information.
--
-- /Note:/ Consider using 'cloudFrontOriginAccessIdentity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfoairrsCloudFrontOriginAccessIdentity :: Lens.Lens' UpdateCloudFrontOriginAccessIdentityResponse (Core.Maybe Types.CloudFrontOriginAccessIdentity)
ucfoairrsCloudFrontOriginAccessIdentity = Lens.field @"cloudFrontOriginAccessIdentity"
{-# INLINEABLE ucfoairrsCloudFrontOriginAccessIdentity #-}
{-# DEPRECATED cloudFrontOriginAccessIdentity "Use generic-lens or generic-optics with 'cloudFrontOriginAccessIdentity' instead"  #-}

-- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfoairrsETag :: Lens.Lens' UpdateCloudFrontOriginAccessIdentityResponse (Core.Maybe Core.Text)
ucfoairrsETag = Lens.field @"eTag"
{-# INLINEABLE ucfoairrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfoairrsResponseStatus :: Lens.Lens' UpdateCloudFrontOriginAccessIdentityResponse Core.Int
ucfoairrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ucfoairrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
