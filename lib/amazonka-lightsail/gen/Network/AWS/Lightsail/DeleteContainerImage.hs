{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteContainerImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a container image that is registered to your Amazon Lightsail container service.
module Network.AWS.Lightsail.DeleteContainerImage
    (
    -- * Creating a request
      DeleteContainerImage (..)
    , mkDeleteContainerImage
    -- ** Request lenses
    , dciServiceName
    , dciImage

    -- * Destructuring the response
    , DeleteContainerImageResponse (..)
    , mkDeleteContainerImageResponse
    -- ** Response lenses
    , dcirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteContainerImage' smart constructor.
data DeleteContainerImage = DeleteContainerImage'
  { serviceName :: Types.ContainerServiceName
    -- ^ The name of the container service for which to delete a registered container image.
  , image :: Core.Text
    -- ^ The name of the container image to delete from the container service.
--
-- Use the @GetContainerImages@ action to get the name of the container images that are registered to a container service.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteContainerImage' value with any optional fields omitted.
mkDeleteContainerImage
    :: Types.ContainerServiceName -- ^ 'serviceName'
    -> Core.Text -- ^ 'image'
    -> DeleteContainerImage
mkDeleteContainerImage serviceName image
  = DeleteContainerImage'{serviceName, image}

-- | The name of the container service for which to delete a registered container image.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciServiceName :: Lens.Lens' DeleteContainerImage Types.ContainerServiceName
dciServiceName = Lens.field @"serviceName"
{-# INLINEABLE dciServiceName #-}
{-# DEPRECATED serviceName "Use generic-lens or generic-optics with 'serviceName' instead"  #-}

-- | The name of the container image to delete from the container service.
--
-- Use the @GetContainerImages@ action to get the name of the container images that are registered to a container service.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciImage :: Lens.Lens' DeleteContainerImage Core.Text
dciImage = Lens.field @"image"
{-# INLINEABLE dciImage #-}
{-# DEPRECATED image "Use generic-lens or generic-optics with 'image' instead"  #-}

instance Core.ToQuery DeleteContainerImage where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteContainerImage where
        toHeaders DeleteContainerImage{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.DeleteContainerImage")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteContainerImage where
        toJSON DeleteContainerImage{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("serviceName" Core..= serviceName),
                  Core.Just ("image" Core..= image)])

instance Core.AWSRequest DeleteContainerImage where
        type Rs DeleteContainerImage = DeleteContainerImageResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteContainerImageResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteContainerImageResponse' smart constructor.
newtype DeleteContainerImageResponse = DeleteContainerImageResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteContainerImageResponse' value with any optional fields omitted.
mkDeleteContainerImageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteContainerImageResponse
mkDeleteContainerImageResponse responseStatus
  = DeleteContainerImageResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcirrsResponseStatus :: Lens.Lens' DeleteContainerImageResponse Core.Int
dcirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
