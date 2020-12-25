{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.CompleteLayerUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Informs Amazon ECR that the image layer upload has completed for a specified registry, repository name, and upload ID. You can optionally provide a @sha256@ digest of the image layer for data validation purposes.
--
-- When an image is pushed, the CompleteLayerUpload API is called once per each new image layer to verify that the upload has completed.
module Network.AWS.ECR.CompleteLayerUpload
  ( -- * Creating a request
    CompleteLayerUpload (..),
    mkCompleteLayerUpload,

    -- ** Request lenses
    cluRepositoryName,
    cluUploadId,
    cluLayerDigests,
    cluRegistryId,

    -- * Destructuring the response
    CompleteLayerUploadResponse (..),
    mkCompleteLayerUploadResponse,

    -- ** Response lenses
    clurrsLayerDigest,
    clurrsRegistryId,
    clurrsRepositoryName,
    clurrsUploadId,
    clurrsResponseStatus,
  )
where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCompleteLayerUpload' smart constructor.
data CompleteLayerUpload = CompleteLayerUpload'
  { -- | The name of the repository to associate with the image layer.
    repositoryName :: Types.RepositoryName,
    -- | The upload ID from a previous 'InitiateLayerUpload' operation to associate with the image layer.
    uploadId :: Types.UploadId,
    -- | The @sha256@ digest of the image layer.
    layerDigests :: Core.NonEmpty Types.LayerDigest,
    -- | The AWS account ID associated with the registry to which to upload layers. If you do not specify a registry, the default registry is assumed.
    registryId :: Core.Maybe Types.RegistryId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CompleteLayerUpload' value with any optional fields omitted.
mkCompleteLayerUpload ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  -- | 'uploadId'
  Types.UploadId ->
  -- | 'layerDigests'
  Core.NonEmpty Types.LayerDigest ->
  CompleteLayerUpload
mkCompleteLayerUpload repositoryName uploadId layerDigests =
  CompleteLayerUpload'
    { repositoryName,
      uploadId,
      layerDigests,
      registryId = Core.Nothing
    }

-- | The name of the repository to associate with the image layer.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cluRepositoryName :: Lens.Lens' CompleteLayerUpload Types.RepositoryName
cluRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED cluRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The upload ID from a previous 'InitiateLayerUpload' operation to associate with the image layer.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cluUploadId :: Lens.Lens' CompleteLayerUpload Types.UploadId
cluUploadId = Lens.field @"uploadId"
{-# DEPRECATED cluUploadId "Use generic-lens or generic-optics with 'uploadId' instead." #-}

-- | The @sha256@ digest of the image layer.
--
-- /Note:/ Consider using 'layerDigests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cluLayerDigests :: Lens.Lens' CompleteLayerUpload (Core.NonEmpty Types.LayerDigest)
cluLayerDigests = Lens.field @"layerDigests"
{-# DEPRECATED cluLayerDigests "Use generic-lens or generic-optics with 'layerDigests' instead." #-}

-- | The AWS account ID associated with the registry to which to upload layers. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cluRegistryId :: Lens.Lens' CompleteLayerUpload (Core.Maybe Types.RegistryId)
cluRegistryId = Lens.field @"registryId"
{-# DEPRECATED cluRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

instance Core.FromJSON CompleteLayerUpload where
  toJSON CompleteLayerUpload {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("uploadId" Core..= uploadId),
            Core.Just ("layerDigests" Core..= layerDigests),
            ("registryId" Core..=) Core.<$> registryId
          ]
      )

instance Core.AWSRequest CompleteLayerUpload where
  type Rs CompleteLayerUpload = CompleteLayerUploadResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerRegistry_V20150921.CompleteLayerUpload"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CompleteLayerUploadResponse'
            Core.<$> (x Core..:? "layerDigest")
            Core.<*> (x Core..:? "registryId")
            Core.<*> (x Core..:? "repositoryName")
            Core.<*> (x Core..:? "uploadId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCompleteLayerUploadResponse' smart constructor.
data CompleteLayerUploadResponse = CompleteLayerUploadResponse'
  { -- | The @sha256@ digest of the image layer.
    layerDigest :: Core.Maybe Types.LayerDigest,
    -- | The registry ID associated with the request.
    registryId :: Core.Maybe Types.RegistryId,
    -- | The repository name associated with the request.
    repositoryName :: Core.Maybe Types.RepositoryName,
    -- | The upload ID associated with the layer.
    uploadId :: Core.Maybe Types.UploadId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CompleteLayerUploadResponse' value with any optional fields omitted.
mkCompleteLayerUploadResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CompleteLayerUploadResponse
mkCompleteLayerUploadResponse responseStatus =
  CompleteLayerUploadResponse'
    { layerDigest = Core.Nothing,
      registryId = Core.Nothing,
      repositoryName = Core.Nothing,
      uploadId = Core.Nothing,
      responseStatus
    }

-- | The @sha256@ digest of the image layer.
--
-- /Note:/ Consider using 'layerDigest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clurrsLayerDigest :: Lens.Lens' CompleteLayerUploadResponse (Core.Maybe Types.LayerDigest)
clurrsLayerDigest = Lens.field @"layerDigest"
{-# DEPRECATED clurrsLayerDigest "Use generic-lens or generic-optics with 'layerDigest' instead." #-}

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clurrsRegistryId :: Lens.Lens' CompleteLayerUploadResponse (Core.Maybe Types.RegistryId)
clurrsRegistryId = Lens.field @"registryId"
{-# DEPRECATED clurrsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clurrsRepositoryName :: Lens.Lens' CompleteLayerUploadResponse (Core.Maybe Types.RepositoryName)
clurrsRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED clurrsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The upload ID associated with the layer.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clurrsUploadId :: Lens.Lens' CompleteLayerUploadResponse (Core.Maybe Types.UploadId)
clurrsUploadId = Lens.field @"uploadId"
{-# DEPRECATED clurrsUploadId "Use generic-lens or generic-optics with 'uploadId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clurrsResponseStatus :: Lens.Lens' CompleteLayerUploadResponse Core.Int
clurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED clurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
