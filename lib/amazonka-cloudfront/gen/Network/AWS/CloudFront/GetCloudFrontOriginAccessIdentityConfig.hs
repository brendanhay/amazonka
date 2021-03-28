{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetCloudFrontOriginAccessIdentityConfig (..)
    , mkGetCloudFrontOriginAccessIdentityConfig
    -- ** Request lenses
    , gcfoaicId

    -- * Destructuring the response
    , GetCloudFrontOriginAccessIdentityConfigResponse (..)
    , mkGetCloudFrontOriginAccessIdentityConfigResponse
    -- ** Response lenses
    , gcfoaicrrsCloudFrontOriginAccessIdentityConfig
    , gcfoaicrrsETag
    , gcfoaicrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The origin access identity's configuration information. For more information, see <https://docs.aws.amazon.com/cloudfront/latest/APIReference/API_CloudFrontOriginAccessIdentityConfig.html CloudFrontOriginAccessIdentityConfig> .
--
-- /See:/ 'mkGetCloudFrontOriginAccessIdentityConfig' smart constructor.
newtype GetCloudFrontOriginAccessIdentityConfig = GetCloudFrontOriginAccessIdentityConfig'
  { id :: Core.Text
    -- ^ The identity's ID. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetCloudFrontOriginAccessIdentityConfig' value with any optional fields omitted.
mkGetCloudFrontOriginAccessIdentityConfig
    :: Core.Text -- ^ 'id'
    -> GetCloudFrontOriginAccessIdentityConfig
mkGetCloudFrontOriginAccessIdentityConfig id
  = GetCloudFrontOriginAccessIdentityConfig'{id}

-- | The identity's ID. 
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfoaicId :: Lens.Lens' GetCloudFrontOriginAccessIdentityConfig Core.Text
gcfoaicId = Lens.field @"id"
{-# INLINEABLE gcfoaicId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery GetCloudFrontOriginAccessIdentityConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetCloudFrontOriginAccessIdentityConfig
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetCloudFrontOriginAccessIdentityConfig
         where
        type Rs GetCloudFrontOriginAccessIdentityConfig =
             GetCloudFrontOriginAccessIdentityConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2020-05-31/origin-access-identity/cloudfront/" Core.<>
                             Core.toText id
                             Core.<> "/config",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetCloudFrontOriginAccessIdentityConfigResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.parseHeaderMaybe "ETag" h Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkGetCloudFrontOriginAccessIdentityConfigResponse' smart constructor.
data GetCloudFrontOriginAccessIdentityConfigResponse = GetCloudFrontOriginAccessIdentityConfigResponse'
  { cloudFrontOriginAccessIdentityConfig :: Core.Maybe Types.CloudFrontOriginAccessIdentityConfig
    -- ^ The origin access identity's configuration information. 
  , eTag :: Core.Maybe Core.Text
    -- ^ The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCloudFrontOriginAccessIdentityConfigResponse' value with any optional fields omitted.
mkGetCloudFrontOriginAccessIdentityConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetCloudFrontOriginAccessIdentityConfigResponse
mkGetCloudFrontOriginAccessIdentityConfigResponse responseStatus
  = GetCloudFrontOriginAccessIdentityConfigResponse'{cloudFrontOriginAccessIdentityConfig
                                                       = Core.Nothing,
                                                     eTag = Core.Nothing, responseStatus}

-- | The origin access identity's configuration information. 
--
-- /Note:/ Consider using 'cloudFrontOriginAccessIdentityConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfoaicrrsCloudFrontOriginAccessIdentityConfig :: Lens.Lens' GetCloudFrontOriginAccessIdentityConfigResponse (Core.Maybe Types.CloudFrontOriginAccessIdentityConfig)
gcfoaicrrsCloudFrontOriginAccessIdentityConfig = Lens.field @"cloudFrontOriginAccessIdentityConfig"
{-# INLINEABLE gcfoaicrrsCloudFrontOriginAccessIdentityConfig #-}
{-# DEPRECATED cloudFrontOriginAccessIdentityConfig "Use generic-lens or generic-optics with 'cloudFrontOriginAccessIdentityConfig' instead"  #-}

-- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfoaicrrsETag :: Lens.Lens' GetCloudFrontOriginAccessIdentityConfigResponse (Core.Maybe Core.Text)
gcfoaicrrsETag = Lens.field @"eTag"
{-# INLINEABLE gcfoaicrrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfoaicrrsResponseStatus :: Lens.Lens' GetCloudFrontOriginAccessIdentityConfigResponse Core.Int
gcfoaicrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcfoaicrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
