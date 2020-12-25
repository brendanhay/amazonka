{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.BatchCheckLayerAvailability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Checks the availability of one or more image layers in a repository.
--
-- When an image is pushed to a repository, each image layer is checked to verify if it has been uploaded before. If it has been uploaded, then the image layer is skipped.
module Network.AWS.ECR.BatchCheckLayerAvailability
  ( -- * Creating a request
    BatchCheckLayerAvailability (..),
    mkBatchCheckLayerAvailability,

    -- ** Request lenses
    bclaRepositoryName,
    bclaLayerDigests,
    bclaRegistryId,

    -- * Destructuring the response
    BatchCheckLayerAvailabilityResponse (..),
    mkBatchCheckLayerAvailabilityResponse,

    -- ** Response lenses
    bclarrsFailures,
    bclarrsLayers,
    bclarrsResponseStatus,
  )
where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchCheckLayerAvailability' smart constructor.
data BatchCheckLayerAvailability = BatchCheckLayerAvailability'
  { -- | The name of the repository that is associated with the image layers to check.
    repositoryName :: Types.RepositoryName,
    -- | The digests of the image layers to check.
    layerDigests :: Core.NonEmpty Types.BatchedOperationLayerDigest,
    -- | The AWS account ID associated with the registry that contains the image layers to check. If you do not specify a registry, the default registry is assumed.
    registryId :: Core.Maybe Types.RegistryId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchCheckLayerAvailability' value with any optional fields omitted.
mkBatchCheckLayerAvailability ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  -- | 'layerDigests'
  Core.NonEmpty Types.BatchedOperationLayerDigest ->
  BatchCheckLayerAvailability
mkBatchCheckLayerAvailability repositoryName layerDigests =
  BatchCheckLayerAvailability'
    { repositoryName,
      layerDigests,
      registryId = Core.Nothing
    }

-- | The name of the repository that is associated with the image layers to check.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bclaRepositoryName :: Lens.Lens' BatchCheckLayerAvailability Types.RepositoryName
bclaRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED bclaRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The digests of the image layers to check.
--
-- /Note:/ Consider using 'layerDigests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bclaLayerDigests :: Lens.Lens' BatchCheckLayerAvailability (Core.NonEmpty Types.BatchedOperationLayerDigest)
bclaLayerDigests = Lens.field @"layerDigests"
{-# DEPRECATED bclaLayerDigests "Use generic-lens or generic-optics with 'layerDigests' instead." #-}

-- | The AWS account ID associated with the registry that contains the image layers to check. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bclaRegistryId :: Lens.Lens' BatchCheckLayerAvailability (Core.Maybe Types.RegistryId)
bclaRegistryId = Lens.field @"registryId"
{-# DEPRECATED bclaRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

instance Core.FromJSON BatchCheckLayerAvailability where
  toJSON BatchCheckLayerAvailability {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("layerDigests" Core..= layerDigests),
            ("registryId" Core..=) Core.<$> registryId
          ]
      )

instance Core.AWSRequest BatchCheckLayerAvailability where
  type
    Rs BatchCheckLayerAvailability =
      BatchCheckLayerAvailabilityResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerRegistry_V20150921.BatchCheckLayerAvailability"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchCheckLayerAvailabilityResponse'
            Core.<$> (x Core..:? "failures")
            Core.<*> (x Core..:? "layers")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchCheckLayerAvailabilityResponse' smart constructor.
data BatchCheckLayerAvailabilityResponse = BatchCheckLayerAvailabilityResponse'
  { -- | Any failures associated with the call.
    failures :: Core.Maybe [Types.LayerFailure],
    -- | A list of image layer objects corresponding to the image layer references in the request.
    layers :: Core.Maybe [Types.Layer],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchCheckLayerAvailabilityResponse' value with any optional fields omitted.
mkBatchCheckLayerAvailabilityResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchCheckLayerAvailabilityResponse
mkBatchCheckLayerAvailabilityResponse responseStatus =
  BatchCheckLayerAvailabilityResponse'
    { failures = Core.Nothing,
      layers = Core.Nothing,
      responseStatus
    }

-- | Any failures associated with the call.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bclarrsFailures :: Lens.Lens' BatchCheckLayerAvailabilityResponse (Core.Maybe [Types.LayerFailure])
bclarrsFailures = Lens.field @"failures"
{-# DEPRECATED bclarrsFailures "Use generic-lens or generic-optics with 'failures' instead." #-}

-- | A list of image layer objects corresponding to the image layer references in the request.
--
-- /Note:/ Consider using 'layers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bclarrsLayers :: Lens.Lens' BatchCheckLayerAvailabilityResponse (Core.Maybe [Types.Layer])
bclarrsLayers = Lens.field @"layers"
{-# DEPRECATED bclarrsLayers "Use generic-lens or generic-optics with 'layers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bclarrsResponseStatus :: Lens.Lens' BatchCheckLayerAvailabilityResponse Core.Int
bclarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bclarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
