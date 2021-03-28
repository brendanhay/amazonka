{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.BatchDeleteImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a list of specified images within a repository. Images are specified with either an @imageTag@ or @imageDigest@ .
--
-- You can remove a tag from an image by specifying the image's tag in your request. When you remove the last tag from an image, the image is deleted from your repository.
-- You can completely delete an image (and all of its tags) by specifying the image's digest in your request.
module Network.AWS.ECR.BatchDeleteImage
    (
    -- * Creating a request
      BatchDeleteImage (..)
    , mkBatchDeleteImage
    -- ** Request lenses
    , bdiRepositoryName
    , bdiImageIds
    , bdiRegistryId

    -- * Destructuring the response
    , BatchDeleteImageResponse (..)
    , mkBatchDeleteImageResponse
    -- ** Response lenses
    , bdirrsFailures
    , bdirrsImageIds
    , bdirrsResponseStatus
    ) where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Deletes specified images within a specified repository. Images are specified with either the @imageTag@ or @imageDigest@ .
--
-- /See:/ 'mkBatchDeleteImage' smart constructor.
data BatchDeleteImage = BatchDeleteImage'
  { repositoryName :: Types.RepositoryName
    -- ^ The repository that contains the image to delete.
  , imageIds :: [Types.ImageIdentifier]
    -- ^ A list of image ID references that correspond to images to delete. The format of the @imageIds@ reference is @imageTag=tag@ or @imageDigest=digest@ .
  , registryId :: Core.Maybe Types.RegistryId
    -- ^ The AWS account ID associated with the registry that contains the image to delete. If you do not specify a registry, the default registry is assumed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeleteImage' value with any optional fields omitted.
mkBatchDeleteImage
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> BatchDeleteImage
mkBatchDeleteImage repositoryName
  = BatchDeleteImage'{repositoryName, imageIds = Core.mempty,
                      registryId = Core.Nothing}

-- | The repository that contains the image to delete.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdiRepositoryName :: Lens.Lens' BatchDeleteImage Types.RepositoryName
bdiRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE bdiRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | A list of image ID references that correspond to images to delete. The format of the @imageIds@ reference is @imageTag=tag@ or @imageDigest=digest@ .
--
-- /Note:/ Consider using 'imageIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdiImageIds :: Lens.Lens' BatchDeleteImage [Types.ImageIdentifier]
bdiImageIds = Lens.field @"imageIds"
{-# INLINEABLE bdiImageIds #-}
{-# DEPRECATED imageIds "Use generic-lens or generic-optics with 'imageIds' instead"  #-}

-- | The AWS account ID associated with the registry that contains the image to delete. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdiRegistryId :: Lens.Lens' BatchDeleteImage (Core.Maybe Types.RegistryId)
bdiRegistryId = Lens.field @"registryId"
{-# INLINEABLE bdiRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

instance Core.ToQuery BatchDeleteImage where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchDeleteImage where
        toHeaders BatchDeleteImage{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerRegistry_V20150921.BatchDeleteImage")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchDeleteImage where
        toJSON BatchDeleteImage{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  Core.Just ("imageIds" Core..= imageIds),
                  ("registryId" Core..=) Core.<$> registryId])

instance Core.AWSRequest BatchDeleteImage where
        type Rs BatchDeleteImage = BatchDeleteImageResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchDeleteImageResponse' Core.<$>
                   (x Core..:? "failures") Core.<*> x Core..:? "imageIds" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchDeleteImageResponse' smart constructor.
data BatchDeleteImageResponse = BatchDeleteImageResponse'
  { failures :: Core.Maybe [Types.ImageFailure]
    -- ^ Any failures associated with the call.
  , imageIds :: Core.Maybe [Types.ImageIdentifier]
    -- ^ The image IDs of the deleted images.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeleteImageResponse' value with any optional fields omitted.
mkBatchDeleteImageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchDeleteImageResponse
mkBatchDeleteImageResponse responseStatus
  = BatchDeleteImageResponse'{failures = Core.Nothing,
                              imageIds = Core.Nothing, responseStatus}

-- | Any failures associated with the call.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdirrsFailures :: Lens.Lens' BatchDeleteImageResponse (Core.Maybe [Types.ImageFailure])
bdirrsFailures = Lens.field @"failures"
{-# INLINEABLE bdirrsFailures #-}
{-# DEPRECATED failures "Use generic-lens or generic-optics with 'failures' instead"  #-}

-- | The image IDs of the deleted images.
--
-- /Note:/ Consider using 'imageIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdirrsImageIds :: Lens.Lens' BatchDeleteImageResponse (Core.Maybe [Types.ImageIdentifier])
bdirrsImageIds = Lens.field @"imageIds"
{-# INLINEABLE bdirrsImageIds #-}
{-# DEPRECATED imageIds "Use generic-lens or generic-optics with 'imageIds' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdirrsResponseStatus :: Lens.Lens' BatchDeleteImageResponse Core.Int
bdirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bdirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
