{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetContainerImages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the container images that are registered to your Amazon Lightsail container service.
module Network.AWS.Lightsail.GetContainerImages
    (
    -- * Creating a request
      GetContainerImages (..)
    , mkGetContainerImages
    -- ** Request lenses
    , gciServiceName

    -- * Destructuring the response
    , GetContainerImagesResponse (..)
    , mkGetContainerImagesResponse
    -- ** Response lenses
    , gcirrsContainerImages
    , gcirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetContainerImages' smart constructor.
newtype GetContainerImages = GetContainerImages'
  { serviceName :: Types.ServiceName
    -- ^ The name of the container service for which to return registered container images.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetContainerImages' value with any optional fields omitted.
mkGetContainerImages
    :: Types.ServiceName -- ^ 'serviceName'
    -> GetContainerImages
mkGetContainerImages serviceName = GetContainerImages'{serviceName}

-- | The name of the container service for which to return registered container images.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gciServiceName :: Lens.Lens' GetContainerImages Types.ServiceName
gciServiceName = Lens.field @"serviceName"
{-# INLINEABLE gciServiceName #-}
{-# DEPRECATED serviceName "Use generic-lens or generic-optics with 'serviceName' instead"  #-}

instance Core.ToQuery GetContainerImages where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetContainerImages where
        toHeaders GetContainerImages{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.GetContainerImages")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetContainerImages where
        toJSON GetContainerImages{..}
          = Core.object
              (Core.catMaybes [Core.Just ("serviceName" Core..= serviceName)])

instance Core.AWSRequest GetContainerImages where
        type Rs GetContainerImages = GetContainerImagesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetContainerImagesResponse' Core.<$>
                   (x Core..:? "containerImages") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetContainerImagesResponse' smart constructor.
data GetContainerImagesResponse = GetContainerImagesResponse'
  { containerImages :: Core.Maybe [Types.ContainerImage]
    -- ^ An array of objects that describe container images that are registered to the container service.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetContainerImagesResponse' value with any optional fields omitted.
mkGetContainerImagesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetContainerImagesResponse
mkGetContainerImagesResponse responseStatus
  = GetContainerImagesResponse'{containerImages = Core.Nothing,
                                responseStatus}

-- | An array of objects that describe container images that are registered to the container service.
--
-- /Note:/ Consider using 'containerImages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsContainerImages :: Lens.Lens' GetContainerImagesResponse (Core.Maybe [Types.ContainerImage])
gcirrsContainerImages = Lens.field @"containerImages"
{-# INLINEABLE gcirrsContainerImages #-}
{-# DEPRECATED containerImages "Use generic-lens or generic-optics with 'containerImages' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsResponseStatus :: Lens.Lens' GetContainerImagesResponse Core.Int
gcirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
