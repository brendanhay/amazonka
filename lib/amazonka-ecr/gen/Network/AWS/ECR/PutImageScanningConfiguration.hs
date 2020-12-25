{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    PutImageScanningConfiguration (..),
    mkPutImageScanningConfiguration,

    -- ** Request lenses
    piscRepositoryName,
    piscImageScanningConfiguration,
    piscRegistryId,

    -- * Destructuring the response
    PutImageScanningConfigurationResponse (..),
    mkPutImageScanningConfigurationResponse,

    -- ** Response lenses
    piscrrsImageScanningConfiguration,
    piscrrsRegistryId,
    piscrrsRepositoryName,
    piscrrsResponseStatus,
  )
where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutImageScanningConfiguration' smart constructor.
data PutImageScanningConfiguration = PutImageScanningConfiguration'
  { -- | The name of the repository in which to update the image scanning configuration setting.
    repositoryName :: Types.RepositoryName,
    -- | The image scanning configuration for the repository. This setting determines whether images are scanned for known vulnerabilities after being pushed to the repository.
    imageScanningConfiguration :: Types.ImageScanningConfiguration,
    -- | The AWS account ID associated with the registry that contains the repository in which to update the image scanning configuration setting. If you do not specify a registry, the default registry is assumed.
    registryId :: Core.Maybe Types.RegistryId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutImageScanningConfiguration' value with any optional fields omitted.
mkPutImageScanningConfiguration ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  -- | 'imageScanningConfiguration'
  Types.ImageScanningConfiguration ->
  PutImageScanningConfiguration
mkPutImageScanningConfiguration
  repositoryName
  imageScanningConfiguration =
    PutImageScanningConfiguration'
      { repositoryName,
        imageScanningConfiguration,
        registryId = Core.Nothing
      }

-- | The name of the repository in which to update the image scanning configuration setting.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piscRepositoryName :: Lens.Lens' PutImageScanningConfiguration Types.RepositoryName
piscRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED piscRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The image scanning configuration for the repository. This setting determines whether images are scanned for known vulnerabilities after being pushed to the repository.
--
-- /Note:/ Consider using 'imageScanningConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piscImageScanningConfiguration :: Lens.Lens' PutImageScanningConfiguration Types.ImageScanningConfiguration
piscImageScanningConfiguration = Lens.field @"imageScanningConfiguration"
{-# DEPRECATED piscImageScanningConfiguration "Use generic-lens or generic-optics with 'imageScanningConfiguration' instead." #-}

-- | The AWS account ID associated with the registry that contains the repository in which to update the image scanning configuration setting. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piscRegistryId :: Lens.Lens' PutImageScanningConfiguration (Core.Maybe Types.RegistryId)
piscRegistryId = Lens.field @"registryId"
{-# DEPRECATED piscRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

instance Core.FromJSON PutImageScanningConfiguration where
  toJSON PutImageScanningConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just
              ("imageScanningConfiguration" Core..= imageScanningConfiguration),
            ("registryId" Core..=) Core.<$> registryId
          ]
      )

instance Core.AWSRequest PutImageScanningConfiguration where
  type
    Rs PutImageScanningConfiguration =
      PutImageScanningConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerRegistry_V20150921.PutImageScanningConfiguration"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutImageScanningConfigurationResponse'
            Core.<$> (x Core..:? "imageScanningConfiguration")
            Core.<*> (x Core..:? "registryId")
            Core.<*> (x Core..:? "repositoryName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutImageScanningConfigurationResponse' smart constructor.
data PutImageScanningConfigurationResponse = PutImageScanningConfigurationResponse'
  { -- | The image scanning configuration setting for the repository.
    imageScanningConfiguration :: Core.Maybe Types.ImageScanningConfiguration,
    -- | The registry ID associated with the request.
    registryId :: Core.Maybe Types.RegistryId,
    -- | The repository name associated with the request.
    repositoryName :: Core.Maybe Types.RepositoryName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutImageScanningConfigurationResponse' value with any optional fields omitted.
mkPutImageScanningConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutImageScanningConfigurationResponse
mkPutImageScanningConfigurationResponse responseStatus =
  PutImageScanningConfigurationResponse'
    { imageScanningConfiguration =
        Core.Nothing,
      registryId = Core.Nothing,
      repositoryName = Core.Nothing,
      responseStatus
    }

-- | The image scanning configuration setting for the repository.
--
-- /Note:/ Consider using 'imageScanningConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piscrrsImageScanningConfiguration :: Lens.Lens' PutImageScanningConfigurationResponse (Core.Maybe Types.ImageScanningConfiguration)
piscrrsImageScanningConfiguration = Lens.field @"imageScanningConfiguration"
{-# DEPRECATED piscrrsImageScanningConfiguration "Use generic-lens or generic-optics with 'imageScanningConfiguration' instead." #-}

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piscrrsRegistryId :: Lens.Lens' PutImageScanningConfigurationResponse (Core.Maybe Types.RegistryId)
piscrrsRegistryId = Lens.field @"registryId"
{-# DEPRECATED piscrrsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piscrrsRepositoryName :: Lens.Lens' PutImageScanningConfigurationResponse (Core.Maybe Types.RepositoryName)
piscrrsRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED piscrrsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piscrrsResponseStatus :: Lens.Lens' PutImageScanningConfigurationResponse Core.Int
piscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED piscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
