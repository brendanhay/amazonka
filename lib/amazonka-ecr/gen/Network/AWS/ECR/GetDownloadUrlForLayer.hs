{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.GetDownloadUrlForLayer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the pre-signed Amazon S3 download URL corresponding to an image layer. You can only get URLs for image layers that are referenced in an image.
--
-- When an image is pulled, the GetDownloadUrlForLayer API is called once per image layer that is not already cached.
module Network.AWS.ECR.GetDownloadUrlForLayer
    (
    -- * Creating a request
      GetDownloadUrlForLayer (..)
    , mkGetDownloadUrlForLayer
    -- ** Request lenses
    , gduflRepositoryName
    , gduflLayerDigest
    , gduflRegistryId

    -- * Destructuring the response
    , GetDownloadUrlForLayerResponse (..)
    , mkGetDownloadUrlForLayerResponse
    -- ** Response lenses
    , gduflrrsDownloadUrl
    , gduflrrsLayerDigest
    , gduflrrsResponseStatus
    ) where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDownloadUrlForLayer' smart constructor.
data GetDownloadUrlForLayer = GetDownloadUrlForLayer'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository that is associated with the image layer to download.
  , layerDigest :: Types.LayerDigest
    -- ^ The digest of the image layer to download.
  , registryId :: Core.Maybe Types.RegistryId
    -- ^ The AWS account ID associated with the registry that contains the image layer to download. If you do not specify a registry, the default registry is assumed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDownloadUrlForLayer' value with any optional fields omitted.
mkGetDownloadUrlForLayer
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> Types.LayerDigest -- ^ 'layerDigest'
    -> GetDownloadUrlForLayer
mkGetDownloadUrlForLayer repositoryName layerDigest
  = GetDownloadUrlForLayer'{repositoryName, layerDigest,
                            registryId = Core.Nothing}

-- | The name of the repository that is associated with the image layer to download.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gduflRepositoryName :: Lens.Lens' GetDownloadUrlForLayer Types.RepositoryName
gduflRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE gduflRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The digest of the image layer to download.
--
-- /Note:/ Consider using 'layerDigest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gduflLayerDigest :: Lens.Lens' GetDownloadUrlForLayer Types.LayerDigest
gduflLayerDigest = Lens.field @"layerDigest"
{-# INLINEABLE gduflLayerDigest #-}
{-# DEPRECATED layerDigest "Use generic-lens or generic-optics with 'layerDigest' instead"  #-}

-- | The AWS account ID associated with the registry that contains the image layer to download. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gduflRegistryId :: Lens.Lens' GetDownloadUrlForLayer (Core.Maybe Types.RegistryId)
gduflRegistryId = Lens.field @"registryId"
{-# INLINEABLE gduflRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

instance Core.ToQuery GetDownloadUrlForLayer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDownloadUrlForLayer where
        toHeaders GetDownloadUrlForLayer{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerRegistry_V20150921.GetDownloadUrlForLayer")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetDownloadUrlForLayer where
        toJSON GetDownloadUrlForLayer{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  Core.Just ("layerDigest" Core..= layerDigest),
                  ("registryId" Core..=) Core.<$> registryId])

instance Core.AWSRequest GetDownloadUrlForLayer where
        type Rs GetDownloadUrlForLayer = GetDownloadUrlForLayerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDownloadUrlForLayerResponse' Core.<$>
                   (x Core..:? "downloadUrl") Core.<*> x Core..:? "layerDigest"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetDownloadUrlForLayerResponse' smart constructor.
data GetDownloadUrlForLayerResponse = GetDownloadUrlForLayerResponse'
  { downloadUrl :: Core.Maybe Types.Url
    -- ^ The pre-signed Amazon S3 download URL for the requested layer.
  , layerDigest :: Core.Maybe Types.LayerDigest
    -- ^ The digest of the image layer to download.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDownloadUrlForLayerResponse' value with any optional fields omitted.
mkGetDownloadUrlForLayerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDownloadUrlForLayerResponse
mkGetDownloadUrlForLayerResponse responseStatus
  = GetDownloadUrlForLayerResponse'{downloadUrl = Core.Nothing,
                                    layerDigest = Core.Nothing, responseStatus}

-- | The pre-signed Amazon S3 download URL for the requested layer.
--
-- /Note:/ Consider using 'downloadUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gduflrrsDownloadUrl :: Lens.Lens' GetDownloadUrlForLayerResponse (Core.Maybe Types.Url)
gduflrrsDownloadUrl = Lens.field @"downloadUrl"
{-# INLINEABLE gduflrrsDownloadUrl #-}
{-# DEPRECATED downloadUrl "Use generic-lens or generic-optics with 'downloadUrl' instead"  #-}

-- | The digest of the image layer to download.
--
-- /Note:/ Consider using 'layerDigest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gduflrrsLayerDigest :: Lens.Lens' GetDownloadUrlForLayerResponse (Core.Maybe Types.LayerDigest)
gduflrrsLayerDigest = Lens.field @"layerDigest"
{-# INLINEABLE gduflrrsLayerDigest #-}
{-# DEPRECATED layerDigest "Use generic-lens or generic-optics with 'layerDigest' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gduflrrsResponseStatus :: Lens.Lens' GetDownloadUrlForLayerResponse Core.Int
gduflrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gduflrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
