{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetDistributionConfig (..)
    , mkGetDistributionConfig
    -- ** Request lenses
    , gdcId

    -- * Destructuring the response
    , GetDistributionConfigResponse (..)
    , mkGetDistributionConfigResponse
    -- ** Response lenses
    , gdcrrsDistributionConfig
    , gdcrrsETag
    , gdcrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to get a distribution configuration.
--
-- /See:/ 'mkGetDistributionConfig' smart constructor.
newtype GetDistributionConfig = GetDistributionConfig'
  { id :: Core.Text
    -- ^ The distribution's ID. If the ID is empty, an empty distribution configuration is returned.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDistributionConfig' value with any optional fields omitted.
mkGetDistributionConfig
    :: Core.Text -- ^ 'id'
    -> GetDistributionConfig
mkGetDistributionConfig id = GetDistributionConfig'{id}

-- | The distribution's ID. If the ID is empty, an empty distribution configuration is returned.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcId :: Lens.Lens' GetDistributionConfig Core.Text
gdcId = Lens.field @"id"
{-# INLINEABLE gdcId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery GetDistributionConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDistributionConfig where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetDistributionConfig where
        type Rs GetDistributionConfig = GetDistributionConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2020-05-31/distribution/" Core.<> Core.toText id Core.<>
                             "/config",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetDistributionConfigResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.parseHeaderMaybe "ETag" h Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkGetDistributionConfigResponse' smart constructor.
data GetDistributionConfigResponse = GetDistributionConfigResponse'
  { distributionConfig :: Core.Maybe Types.DistributionConfig
    -- ^ The distribution's configuration information.
  , eTag :: Core.Maybe Core.Text
    -- ^ The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDistributionConfigResponse' value with any optional fields omitted.
mkGetDistributionConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDistributionConfigResponse
mkGetDistributionConfigResponse responseStatus
  = GetDistributionConfigResponse'{distributionConfig = Core.Nothing,
                                   eTag = Core.Nothing, responseStatus}

-- | The distribution's configuration information.
--
-- /Note:/ Consider using 'distributionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrrsDistributionConfig :: Lens.Lens' GetDistributionConfigResponse (Core.Maybe Types.DistributionConfig)
gdcrrsDistributionConfig = Lens.field @"distributionConfig"
{-# INLINEABLE gdcrrsDistributionConfig #-}
{-# DEPRECATED distributionConfig "Use generic-lens or generic-optics with 'distributionConfig' instead"  #-}

-- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrrsETag :: Lens.Lens' GetDistributionConfigResponse (Core.Maybe Core.Text)
gdcrrsETag = Lens.field @"eTag"
{-# INLINEABLE gdcrrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrrsResponseStatus :: Lens.Lens' GetDistributionConfigResponse Core.Int
gdcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
