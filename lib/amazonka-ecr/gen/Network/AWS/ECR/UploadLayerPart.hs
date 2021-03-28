{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.UploadLayerPart
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads an image layer part to Amazon ECR.
--
-- When an image is pushed, each new image layer is uploaded in parts. The maximum size of each image layer part can be 20971520 bytes (or about 20MB). The UploadLayerPart API is called once per each new image layer part.
module Network.AWS.ECR.UploadLayerPart
    (
    -- * Creating a request
      UploadLayerPart (..)
    , mkUploadLayerPart
    -- ** Request lenses
    , ulpRepositoryName
    , ulpUploadId
    , ulpPartFirstByte
    , ulpPartLastByte
    , ulpLayerPartBlob
    , ulpRegistryId

    -- * Destructuring the response
    , UploadLayerPartResponse (..)
    , mkUploadLayerPartResponse
    -- ** Response lenses
    , ulprrsLastByteReceived
    , ulprrsRegistryId
    , ulprrsRepositoryName
    , ulprrsUploadId
    , ulprrsResponseStatus
    ) where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUploadLayerPart' smart constructor.
data UploadLayerPart = UploadLayerPart'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository to which you are uploading layer parts.
  , uploadId :: Types.UploadId
    -- ^ The upload ID from a previous 'InitiateLayerUpload' operation to associate with the layer part upload.
  , partFirstByte :: Core.Natural
    -- ^ The position of the first byte of the layer part witin the overall image layer.
  , partLastByte :: Core.Natural
    -- ^ The position of the last byte of the layer part within the overall image layer.
  , layerPartBlob :: Core.Base64
    -- ^ The base64-encoded layer part payload.
  , registryId :: Core.Maybe Types.RegistryId
    -- ^ The AWS account ID associated with the registry to which you are uploading layer parts. If you do not specify a registry, the default registry is assumed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UploadLayerPart' value with any optional fields omitted.
mkUploadLayerPart
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> Types.UploadId -- ^ 'uploadId'
    -> Core.Natural -- ^ 'partFirstByte'
    -> Core.Natural -- ^ 'partLastByte'
    -> Core.Base64 -- ^ 'layerPartBlob'
    -> UploadLayerPart
mkUploadLayerPart repositoryName uploadId partFirstByte
  partLastByte layerPartBlob
  = UploadLayerPart'{repositoryName, uploadId, partFirstByte,
                     partLastByte, layerPartBlob, registryId = Core.Nothing}

-- | The name of the repository to which you are uploading layer parts.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulpRepositoryName :: Lens.Lens' UploadLayerPart Types.RepositoryName
ulpRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE ulpRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The upload ID from a previous 'InitiateLayerUpload' operation to associate with the layer part upload.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulpUploadId :: Lens.Lens' UploadLayerPart Types.UploadId
ulpUploadId = Lens.field @"uploadId"
{-# INLINEABLE ulpUploadId #-}
{-# DEPRECATED uploadId "Use generic-lens or generic-optics with 'uploadId' instead"  #-}

-- | The position of the first byte of the layer part witin the overall image layer.
--
-- /Note:/ Consider using 'partFirstByte' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulpPartFirstByte :: Lens.Lens' UploadLayerPart Core.Natural
ulpPartFirstByte = Lens.field @"partFirstByte"
{-# INLINEABLE ulpPartFirstByte #-}
{-# DEPRECATED partFirstByte "Use generic-lens or generic-optics with 'partFirstByte' instead"  #-}

-- | The position of the last byte of the layer part within the overall image layer.
--
-- /Note:/ Consider using 'partLastByte' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulpPartLastByte :: Lens.Lens' UploadLayerPart Core.Natural
ulpPartLastByte = Lens.field @"partLastByte"
{-# INLINEABLE ulpPartLastByte #-}
{-# DEPRECATED partLastByte "Use generic-lens or generic-optics with 'partLastByte' instead"  #-}

-- | The base64-encoded layer part payload.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'layerPartBlob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulpLayerPartBlob :: Lens.Lens' UploadLayerPart Core.Base64
ulpLayerPartBlob = Lens.field @"layerPartBlob"
{-# INLINEABLE ulpLayerPartBlob #-}
{-# DEPRECATED layerPartBlob "Use generic-lens or generic-optics with 'layerPartBlob' instead"  #-}

-- | The AWS account ID associated with the registry to which you are uploading layer parts. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulpRegistryId :: Lens.Lens' UploadLayerPart (Core.Maybe Types.RegistryId)
ulpRegistryId = Lens.field @"registryId"
{-# INLINEABLE ulpRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

instance Core.ToQuery UploadLayerPart where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UploadLayerPart where
        toHeaders UploadLayerPart{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerRegistry_V20150921.UploadLayerPart")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UploadLayerPart where
        toJSON UploadLayerPart{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  Core.Just ("uploadId" Core..= uploadId),
                  Core.Just ("partFirstByte" Core..= partFirstByte),
                  Core.Just ("partLastByte" Core..= partLastByte),
                  Core.Just ("layerPartBlob" Core..= layerPartBlob),
                  ("registryId" Core..=) Core.<$> registryId])

instance Core.AWSRequest UploadLayerPart where
        type Rs UploadLayerPart = UploadLayerPartResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UploadLayerPartResponse' Core.<$>
                   (x Core..:? "lastByteReceived") Core.<*> x Core..:? "registryId"
                     Core.<*> x Core..:? "repositoryName"
                     Core.<*> x Core..:? "uploadId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUploadLayerPartResponse' smart constructor.
data UploadLayerPartResponse = UploadLayerPartResponse'
  { lastByteReceived :: Core.Maybe Core.Natural
    -- ^ The integer value of the last byte received in the request.
  , registryId :: Core.Maybe Types.RegistryId
    -- ^ The registry ID associated with the request.
  , repositoryName :: Core.Maybe Types.RepositoryName
    -- ^ The repository name associated with the request.
  , uploadId :: Core.Maybe Types.UploadId
    -- ^ The upload ID associated with the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UploadLayerPartResponse' value with any optional fields omitted.
mkUploadLayerPartResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UploadLayerPartResponse
mkUploadLayerPartResponse responseStatus
  = UploadLayerPartResponse'{lastByteReceived = Core.Nothing,
                             registryId = Core.Nothing, repositoryName = Core.Nothing,
                             uploadId = Core.Nothing, responseStatus}

-- | The integer value of the last byte received in the request.
--
-- /Note:/ Consider using 'lastByteReceived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulprrsLastByteReceived :: Lens.Lens' UploadLayerPartResponse (Core.Maybe Core.Natural)
ulprrsLastByteReceived = Lens.field @"lastByteReceived"
{-# INLINEABLE ulprrsLastByteReceived #-}
{-# DEPRECATED lastByteReceived "Use generic-lens or generic-optics with 'lastByteReceived' instead"  #-}

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulprrsRegistryId :: Lens.Lens' UploadLayerPartResponse (Core.Maybe Types.RegistryId)
ulprrsRegistryId = Lens.field @"registryId"
{-# INLINEABLE ulprrsRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulprrsRepositoryName :: Lens.Lens' UploadLayerPartResponse (Core.Maybe Types.RepositoryName)
ulprrsRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE ulprrsRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The upload ID associated with the request.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulprrsUploadId :: Lens.Lens' UploadLayerPartResponse (Core.Maybe Types.UploadId)
ulprrsUploadId = Lens.field @"uploadId"
{-# INLINEABLE ulprrsUploadId #-}
{-# DEPRECATED uploadId "Use generic-lens or generic-optics with 'uploadId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulprrsResponseStatus :: Lens.Lens' UploadLayerPartResponse Core.Int
ulprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ulprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
