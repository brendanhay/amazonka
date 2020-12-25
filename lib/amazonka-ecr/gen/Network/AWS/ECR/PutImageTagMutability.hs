{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.PutImageTagMutability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the image tag mutability settings for the specified repository. For more information, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/image-tag-mutability.html Image Tag Mutability> in the /Amazon Elastic Container Registry User Guide/ .
module Network.AWS.ECR.PutImageTagMutability
  ( -- * Creating a request
    PutImageTagMutability (..),
    mkPutImageTagMutability,

    -- ** Request lenses
    pitmRepositoryName,
    pitmImageTagMutability,
    pitmRegistryId,

    -- * Destructuring the response
    PutImageTagMutabilityResponse (..),
    mkPutImageTagMutabilityResponse,

    -- ** Response lenses
    pitmrrsImageTagMutability,
    pitmrrsRegistryId,
    pitmrrsRepositoryName,
    pitmrrsResponseStatus,
  )
where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutImageTagMutability' smart constructor.
data PutImageTagMutability = PutImageTagMutability'
  { -- | The name of the repository in which to update the image tag mutability settings.
    repositoryName :: Types.RepositoryName,
    -- | The tag mutability setting for the repository. If @MUTABLE@ is specified, image tags can be overwritten. If @IMMUTABLE@ is specified, all image tags within the repository will be immutable which will prevent them from being overwritten.
    imageTagMutability :: Types.ImageTagMutability,
    -- | The AWS account ID associated with the registry that contains the repository in which to update the image tag mutability settings. If you do not specify a registry, the default registry is assumed.
    registryId :: Core.Maybe Types.RegistryId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutImageTagMutability' value with any optional fields omitted.
mkPutImageTagMutability ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  -- | 'imageTagMutability'
  Types.ImageTagMutability ->
  PutImageTagMutability
mkPutImageTagMutability repositoryName imageTagMutability =
  PutImageTagMutability'
    { repositoryName,
      imageTagMutability,
      registryId = Core.Nothing
    }

-- | The name of the repository in which to update the image tag mutability settings.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitmRepositoryName :: Lens.Lens' PutImageTagMutability Types.RepositoryName
pitmRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED pitmRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The tag mutability setting for the repository. If @MUTABLE@ is specified, image tags can be overwritten. If @IMMUTABLE@ is specified, all image tags within the repository will be immutable which will prevent them from being overwritten.
--
-- /Note:/ Consider using 'imageTagMutability' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitmImageTagMutability :: Lens.Lens' PutImageTagMutability Types.ImageTagMutability
pitmImageTagMutability = Lens.field @"imageTagMutability"
{-# DEPRECATED pitmImageTagMutability "Use generic-lens or generic-optics with 'imageTagMutability' instead." #-}

-- | The AWS account ID associated with the registry that contains the repository in which to update the image tag mutability settings. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitmRegistryId :: Lens.Lens' PutImageTagMutability (Core.Maybe Types.RegistryId)
pitmRegistryId = Lens.field @"registryId"
{-# DEPRECATED pitmRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

instance Core.FromJSON PutImageTagMutability where
  toJSON PutImageTagMutability {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("imageTagMutability" Core..= imageTagMutability),
            ("registryId" Core..=) Core.<$> registryId
          ]
      )

instance Core.AWSRequest PutImageTagMutability where
  type Rs PutImageTagMutability = PutImageTagMutabilityResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerRegistry_V20150921.PutImageTagMutability"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutImageTagMutabilityResponse'
            Core.<$> (x Core..:? "imageTagMutability")
            Core.<*> (x Core..:? "registryId")
            Core.<*> (x Core..:? "repositoryName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutImageTagMutabilityResponse' smart constructor.
data PutImageTagMutabilityResponse = PutImageTagMutabilityResponse'
  { -- | The image tag mutability setting for the repository.
    imageTagMutability :: Core.Maybe Types.ImageTagMutability,
    -- | The registry ID associated with the request.
    registryId :: Core.Maybe Types.RegistryId,
    -- | The repository name associated with the request.
    repositoryName :: Core.Maybe Types.RepositoryName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutImageTagMutabilityResponse' value with any optional fields omitted.
mkPutImageTagMutabilityResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutImageTagMutabilityResponse
mkPutImageTagMutabilityResponse responseStatus =
  PutImageTagMutabilityResponse'
    { imageTagMutability = Core.Nothing,
      registryId = Core.Nothing,
      repositoryName = Core.Nothing,
      responseStatus
    }

-- | The image tag mutability setting for the repository.
--
-- /Note:/ Consider using 'imageTagMutability' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitmrrsImageTagMutability :: Lens.Lens' PutImageTagMutabilityResponse (Core.Maybe Types.ImageTagMutability)
pitmrrsImageTagMutability = Lens.field @"imageTagMutability"
{-# DEPRECATED pitmrrsImageTagMutability "Use generic-lens or generic-optics with 'imageTagMutability' instead." #-}

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitmrrsRegistryId :: Lens.Lens' PutImageTagMutabilityResponse (Core.Maybe Types.RegistryId)
pitmrrsRegistryId = Lens.field @"registryId"
{-# DEPRECATED pitmrrsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitmrrsRepositoryName :: Lens.Lens' PutImageTagMutabilityResponse (Core.Maybe Types.RepositoryName)
pitmrrsRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED pitmrrsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitmrrsResponseStatus :: Lens.Lens' PutImageTagMutabilityResponse Core.Int
pitmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pitmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
