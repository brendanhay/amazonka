{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.PutImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the image manifest and tags associated with an image.
--
-- When an image is pushed and all new image layers have been uploaded, the PutImage API is called once to create or update the image manifest and the tags associated with the image.
module Network.AWS.ECR.PutImage
    (
    -- * Creating a request
      PutImage (..)
    , mkPutImage
    -- ** Request lenses
    , piRepositoryName
    , piImageManifest
    , piImageDigest
    , piImageManifestMediaType
    , piImageTag
    , piRegistryId

    -- * Destructuring the response
    , PutImageResponse (..)
    , mkPutImageResponse
    -- ** Response lenses
    , pirrsImage
    , pirrsResponseStatus
    ) where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutImage' smart constructor.
data PutImage = PutImage'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository in which to put the image.
  , imageManifest :: Types.ImageManifest
    -- ^ The image manifest corresponding to the image to be uploaded.
  , imageDigest :: Core.Maybe Types.ImageDigest
    -- ^ The image digest of the image manifest corresponding to the image.
  , imageManifestMediaType :: Core.Maybe Types.MediaType
    -- ^ The media type of the image manifest. If you push an image manifest that does not contain the @mediaType@ field, you must specify the @imageManifestMediaType@ in the request.
  , imageTag :: Core.Maybe Types.ImageTag
    -- ^ The tag to associate with the image. This parameter is required for images that use the Docker Image Manifest V2 Schema 2 or Open Container Initiative (OCI) formats.
  , registryId :: Core.Maybe Types.RegistryId
    -- ^ The AWS account ID associated with the registry that contains the repository in which to put the image. If you do not specify a registry, the default registry is assumed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutImage' value with any optional fields omitted.
mkPutImage
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> Types.ImageManifest -- ^ 'imageManifest'
    -> PutImage
mkPutImage repositoryName imageManifest
  = PutImage'{repositoryName, imageManifest,
              imageDigest = Core.Nothing, imageManifestMediaType = Core.Nothing,
              imageTag = Core.Nothing, registryId = Core.Nothing}

-- | The name of the repository in which to put the image.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piRepositoryName :: Lens.Lens' PutImage Types.RepositoryName
piRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE piRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The image manifest corresponding to the image to be uploaded.
--
-- /Note:/ Consider using 'imageManifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piImageManifest :: Lens.Lens' PutImage Types.ImageManifest
piImageManifest = Lens.field @"imageManifest"
{-# INLINEABLE piImageManifest #-}
{-# DEPRECATED imageManifest "Use generic-lens or generic-optics with 'imageManifest' instead"  #-}

-- | The image digest of the image manifest corresponding to the image.
--
-- /Note:/ Consider using 'imageDigest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piImageDigest :: Lens.Lens' PutImage (Core.Maybe Types.ImageDigest)
piImageDigest = Lens.field @"imageDigest"
{-# INLINEABLE piImageDigest #-}
{-# DEPRECATED imageDigest "Use generic-lens or generic-optics with 'imageDigest' instead"  #-}

-- | The media type of the image manifest. If you push an image manifest that does not contain the @mediaType@ field, you must specify the @imageManifestMediaType@ in the request.
--
-- /Note:/ Consider using 'imageManifestMediaType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piImageManifestMediaType :: Lens.Lens' PutImage (Core.Maybe Types.MediaType)
piImageManifestMediaType = Lens.field @"imageManifestMediaType"
{-# INLINEABLE piImageManifestMediaType #-}
{-# DEPRECATED imageManifestMediaType "Use generic-lens or generic-optics with 'imageManifestMediaType' instead"  #-}

-- | The tag to associate with the image. This parameter is required for images that use the Docker Image Manifest V2 Schema 2 or Open Container Initiative (OCI) formats.
--
-- /Note:/ Consider using 'imageTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piImageTag :: Lens.Lens' PutImage (Core.Maybe Types.ImageTag)
piImageTag = Lens.field @"imageTag"
{-# INLINEABLE piImageTag #-}
{-# DEPRECATED imageTag "Use generic-lens or generic-optics with 'imageTag' instead"  #-}

-- | The AWS account ID associated with the registry that contains the repository in which to put the image. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piRegistryId :: Lens.Lens' PutImage (Core.Maybe Types.RegistryId)
piRegistryId = Lens.field @"registryId"
{-# INLINEABLE piRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

instance Core.ToQuery PutImage where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutImage where
        toHeaders PutImage{..}
          = Core.pure
              ("X-Amz-Target", "AmazonEC2ContainerRegistry_V20150921.PutImage")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutImage where
        toJSON PutImage{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  Core.Just ("imageManifest" Core..= imageManifest),
                  ("imageDigest" Core..=) Core.<$> imageDigest,
                  ("imageManifestMediaType" Core..=) Core.<$> imageManifestMediaType,
                  ("imageTag" Core..=) Core.<$> imageTag,
                  ("registryId" Core..=) Core.<$> registryId])

instance Core.AWSRequest PutImage where
        type Rs PutImage = PutImageResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutImageResponse' Core.<$>
                   (x Core..:? "image") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutImageResponse' smart constructor.
data PutImageResponse = PutImageResponse'
  { image :: Core.Maybe Types.Image
    -- ^ Details of the image uploaded.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutImageResponse' value with any optional fields omitted.
mkPutImageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutImageResponse
mkPutImageResponse responseStatus
  = PutImageResponse'{image = Core.Nothing, responseStatus}

-- | Details of the image uploaded.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsImage :: Lens.Lens' PutImageResponse (Core.Maybe Types.Image)
pirrsImage = Lens.field @"image"
{-# INLINEABLE pirrsImage #-}
{-# DEPRECATED image "Use generic-lens or generic-optics with 'image' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsResponseStatus :: Lens.Lens' PutImageResponse Core.Int
pirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
