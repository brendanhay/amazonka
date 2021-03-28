{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.PutImageScanningConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the image scanning configuration for the specified repository.
module Network.AWS.ECR.PutImageScanningConfiguration
    (
    -- * Creating a request
      PutImageScanningConfiguration (..)
    , mkPutImageScanningConfiguration
    -- ** Request lenses
    , piscRepositoryName
    , piscImageScanningConfiguration
    , piscRegistryId

    -- * Destructuring the response
    , PutImageScanningConfigurationResponse (..)
    , mkPutImageScanningConfigurationResponse
    -- ** Response lenses
    , piscrrsImageScanningConfiguration
    , piscrrsRegistryId
    , piscrrsRepositoryName
    , piscrrsResponseStatus
    ) where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutImageScanningConfiguration' smart constructor.
data PutImageScanningConfiguration = PutImageScanningConfiguration'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository in which to update the image scanning configuration setting.
  , imageScanningConfiguration :: Types.ImageScanningConfiguration
    -- ^ The image scanning configuration for the repository. This setting determines whether images are scanned for known vulnerabilities after being pushed to the repository.
  , registryId :: Core.Maybe Types.RegistryId
    -- ^ The AWS account ID associated with the registry that contains the repository in which to update the image scanning configuration setting. If you do not specify a registry, the default registry is assumed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutImageScanningConfiguration' value with any optional fields omitted.
mkPutImageScanningConfiguration
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> Types.ImageScanningConfiguration -- ^ 'imageScanningConfiguration'
    -> PutImageScanningConfiguration
mkPutImageScanningConfiguration repositoryName
  imageScanningConfiguration
  = PutImageScanningConfiguration'{repositoryName,
                                   imageScanningConfiguration, registryId = Core.Nothing}

-- | The name of the repository in which to update the image scanning configuration setting.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piscRepositoryName :: Lens.Lens' PutImageScanningConfiguration Types.RepositoryName
piscRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE piscRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The image scanning configuration for the repository. This setting determines whether images are scanned for known vulnerabilities after being pushed to the repository.
--
-- /Note:/ Consider using 'imageScanningConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piscImageScanningConfiguration :: Lens.Lens' PutImageScanningConfiguration Types.ImageScanningConfiguration
piscImageScanningConfiguration = Lens.field @"imageScanningConfiguration"
{-# INLINEABLE piscImageScanningConfiguration #-}
{-# DEPRECATED imageScanningConfiguration "Use generic-lens or generic-optics with 'imageScanningConfiguration' instead"  #-}

-- | The AWS account ID associated with the registry that contains the repository in which to update the image scanning configuration setting. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piscRegistryId :: Lens.Lens' PutImageScanningConfiguration (Core.Maybe Types.RegistryId)
piscRegistryId = Lens.field @"registryId"
{-# INLINEABLE piscRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

instance Core.ToQuery PutImageScanningConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutImageScanningConfiguration where
        toHeaders PutImageScanningConfiguration{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerRegistry_V20150921.PutImageScanningConfiguration")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutImageScanningConfiguration where
        toJSON PutImageScanningConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  Core.Just
                    ("imageScanningConfiguration" Core..= imageScanningConfiguration),
                  ("registryId" Core..=) Core.<$> registryId])

instance Core.AWSRequest PutImageScanningConfiguration where
        type Rs PutImageScanningConfiguration =
             PutImageScanningConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutImageScanningConfigurationResponse' Core.<$>
                   (x Core..:? "imageScanningConfiguration") Core.<*>
                     x Core..:? "registryId"
                     Core.<*> x Core..:? "repositoryName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutImageScanningConfigurationResponse' smart constructor.
data PutImageScanningConfigurationResponse = PutImageScanningConfigurationResponse'
  { imageScanningConfiguration :: Core.Maybe Types.ImageScanningConfiguration
    -- ^ The image scanning configuration setting for the repository.
  , registryId :: Core.Maybe Types.RegistryId
    -- ^ The registry ID associated with the request.
  , repositoryName :: Core.Maybe Types.RepositoryName
    -- ^ The repository name associated with the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutImageScanningConfigurationResponse' value with any optional fields omitted.
mkPutImageScanningConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutImageScanningConfigurationResponse
mkPutImageScanningConfigurationResponse responseStatus
  = PutImageScanningConfigurationResponse'{imageScanningConfiguration
                                             = Core.Nothing,
                                           registryId = Core.Nothing, repositoryName = Core.Nothing,
                                           responseStatus}

-- | The image scanning configuration setting for the repository.
--
-- /Note:/ Consider using 'imageScanningConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piscrrsImageScanningConfiguration :: Lens.Lens' PutImageScanningConfigurationResponse (Core.Maybe Types.ImageScanningConfiguration)
piscrrsImageScanningConfiguration = Lens.field @"imageScanningConfiguration"
{-# INLINEABLE piscrrsImageScanningConfiguration #-}
{-# DEPRECATED imageScanningConfiguration "Use generic-lens or generic-optics with 'imageScanningConfiguration' instead"  #-}

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piscrrsRegistryId :: Lens.Lens' PutImageScanningConfigurationResponse (Core.Maybe Types.RegistryId)
piscrrsRegistryId = Lens.field @"registryId"
{-# INLINEABLE piscrrsRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piscrrsRepositoryName :: Lens.Lens' PutImageScanningConfigurationResponse (Core.Maybe Types.RepositoryName)
piscrrsRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE piscrrsRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piscrrsResponseStatus :: Lens.Lens' PutImageScanningConfigurationResponse Core.Int
piscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE piscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
