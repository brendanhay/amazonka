{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.DeleteRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a repository. If the repository contains images, you must either delete all images in the repository or use the @force@ option to delete the repository.
module Network.AWS.ECR.DeleteRepository
  ( -- * Creating a request
    DeleteRepository (..),
    mkDeleteRepository,

    -- ** Request lenses
    dRepositoryName,
    dForce,
    dRegistryId,

    -- * Destructuring the response
    DeleteRepositoryResponse (..),
    mkDeleteRepositoryResponse,

    -- ** Response lenses
    drsRepository,
    drsResponseStatus,
  )
where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteRepository' smart constructor.
data DeleteRepository = DeleteRepository'
  { -- | The name of the repository to delete.
    repositoryName :: Types.RepositoryName,
    -- | If a repository contains images, forces the deletion.
    force :: Core.Maybe Core.Bool,
    -- | The AWS account ID associated with the registry that contains the repository to delete. If you do not specify a registry, the default registry is assumed.
    registryId :: Core.Maybe Types.RegistryId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRepository' value with any optional fields omitted.
mkDeleteRepository ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  DeleteRepository
mkDeleteRepository repositoryName =
  DeleteRepository'
    { repositoryName,
      force = Core.Nothing,
      registryId = Core.Nothing
    }

-- | The name of the repository to delete.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRepositoryName :: Lens.Lens' DeleteRepository Types.RepositoryName
dRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED dRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | If a repository contains images, forces the deletion.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dForce :: Lens.Lens' DeleteRepository (Core.Maybe Core.Bool)
dForce = Lens.field @"force"
{-# DEPRECATED dForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | The AWS account ID associated with the registry that contains the repository to delete. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRegistryId :: Lens.Lens' DeleteRepository (Core.Maybe Types.RegistryId)
dRegistryId = Lens.field @"registryId"
{-# DEPRECATED dRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

instance Core.FromJSON DeleteRepository where
  toJSON DeleteRepository {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            ("force" Core..=) Core.<$> force,
            ("registryId" Core..=) Core.<$> registryId
          ]
      )

instance Core.AWSRequest DeleteRepository where
  type Rs DeleteRepository = DeleteRepositoryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerRegistry_V20150921.DeleteRepository"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRepositoryResponse'
            Core.<$> (x Core..:? "repository") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteRepositoryResponse' smart constructor.
data DeleteRepositoryResponse = DeleteRepositoryResponse'
  { -- | The repository that was deleted.
    repository :: Core.Maybe Types.Repository,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteRepositoryResponse' value with any optional fields omitted.
mkDeleteRepositoryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteRepositoryResponse
mkDeleteRepositoryResponse responseStatus =
  DeleteRepositoryResponse'
    { repository = Core.Nothing,
      responseStatus
    }

-- | The repository that was deleted.
--
-- /Note:/ Consider using 'repository' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsRepository :: Lens.Lens' DeleteRepositoryResponse (Core.Maybe Types.Repository)
drsRepository = Lens.field @"repository"
{-# DEPRECATED drsRepository "Use generic-lens or generic-optics with 'repository' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteRepositoryResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
