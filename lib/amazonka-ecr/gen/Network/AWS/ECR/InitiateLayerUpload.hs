{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.InitiateLayerUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Notifies Amazon ECR that you intend to upload an image layer.
--
-- When an image is pushed, the InitiateLayerUpload API is called once per image layer that has not already been uploaded. Whether or not an image layer has been uploaded is determined by the BatchCheckLayerAvailability API action.
module Network.AWS.ECR.InitiateLayerUpload
    (
    -- * Creating a request
      InitiateLayerUpload (..)
    , mkInitiateLayerUpload
    -- ** Request lenses
    , iluRepositoryName
    , iluRegistryId

    -- * Destructuring the response
    , InitiateLayerUploadResponse (..)
    , mkInitiateLayerUploadResponse
    -- ** Response lenses
    , ilurrsPartSize
    , ilurrsUploadId
    , ilurrsResponseStatus
    ) where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkInitiateLayerUpload' smart constructor.
data InitiateLayerUpload = InitiateLayerUpload'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository to which you intend to upload layers.
  , registryId :: Core.Maybe Types.RegistryId
    -- ^ The AWS account ID associated with the registry to which you intend to upload layers. If you do not specify a registry, the default registry is assumed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InitiateLayerUpload' value with any optional fields omitted.
mkInitiateLayerUpload
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> InitiateLayerUpload
mkInitiateLayerUpload repositoryName
  = InitiateLayerUpload'{repositoryName, registryId = Core.Nothing}

-- | The name of the repository to which you intend to upload layers.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iluRepositoryName :: Lens.Lens' InitiateLayerUpload Types.RepositoryName
iluRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE iluRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The AWS account ID associated with the registry to which you intend to upload layers. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iluRegistryId :: Lens.Lens' InitiateLayerUpload (Core.Maybe Types.RegistryId)
iluRegistryId = Lens.field @"registryId"
{-# INLINEABLE iluRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

instance Core.ToQuery InitiateLayerUpload where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders InitiateLayerUpload where
        toHeaders InitiateLayerUpload{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerRegistry_V20150921.InitiateLayerUpload")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON InitiateLayerUpload where
        toJSON InitiateLayerUpload{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  ("registryId" Core..=) Core.<$> registryId])

instance Core.AWSRequest InitiateLayerUpload where
        type Rs InitiateLayerUpload = InitiateLayerUploadResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 InitiateLayerUploadResponse' Core.<$>
                   (x Core..:? "partSize") Core.<*> x Core..:? "uploadId" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkInitiateLayerUploadResponse' smart constructor.
data InitiateLayerUploadResponse = InitiateLayerUploadResponse'
  { partSize :: Core.Maybe Core.Natural
    -- ^ The size, in bytes, that Amazon ECR expects future layer part uploads to be.
  , uploadId :: Core.Maybe Types.UploadId
    -- ^ The upload ID for the layer upload. This parameter is passed to further 'UploadLayerPart' and 'CompleteLayerUpload' operations.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InitiateLayerUploadResponse' value with any optional fields omitted.
mkInitiateLayerUploadResponse
    :: Core.Int -- ^ 'responseStatus'
    -> InitiateLayerUploadResponse
mkInitiateLayerUploadResponse responseStatus
  = InitiateLayerUploadResponse'{partSize = Core.Nothing,
                                 uploadId = Core.Nothing, responseStatus}

-- | The size, in bytes, that Amazon ECR expects future layer part uploads to be.
--
-- /Note:/ Consider using 'partSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilurrsPartSize :: Lens.Lens' InitiateLayerUploadResponse (Core.Maybe Core.Natural)
ilurrsPartSize = Lens.field @"partSize"
{-# INLINEABLE ilurrsPartSize #-}
{-# DEPRECATED partSize "Use generic-lens or generic-optics with 'partSize' instead"  #-}

-- | The upload ID for the layer upload. This parameter is passed to further 'UploadLayerPart' and 'CompleteLayerUpload' operations.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilurrsUploadId :: Lens.Lens' InitiateLayerUploadResponse (Core.Maybe Types.UploadId)
ilurrsUploadId = Lens.field @"uploadId"
{-# INLINEABLE ilurrsUploadId #-}
{-# DEPRECATED uploadId "Use generic-lens or generic-optics with 'uploadId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilurrsResponseStatus :: Lens.Lens' InitiateLayerUploadResponse Core.Int
ilurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ilurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
