{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteRepository (..)
    , mkDeleteRepository
    -- ** Request lenses
    , dRepositoryName
    , dForce
    , dRegistryId

    -- * Destructuring the response
    , DeleteRepositoryResponse (..)
    , mkDeleteRepositoryResponse
    -- ** Response lenses
    , drsRepository
    , drsResponseStatus
    ) where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteRepository' smart constructor.
data DeleteRepository = DeleteRepository'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository to delete.
  , force :: Core.Maybe Core.Bool
    -- ^ If a repository contains images, forces the deletion.
  , registryId :: Core.Maybe Types.RegistryId
    -- ^ The AWS account ID associated with the registry that contains the repository to delete. If you do not specify a registry, the default registry is assumed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRepository' value with any optional fields omitted.
mkDeleteRepository
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> DeleteRepository
mkDeleteRepository repositoryName
  = DeleteRepository'{repositoryName, force = Core.Nothing,
                      registryId = Core.Nothing}

-- | The name of the repository to delete.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRepositoryName :: Lens.Lens' DeleteRepository Types.RepositoryName
dRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE dRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | If a repository contains images, forces the deletion.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dForce :: Lens.Lens' DeleteRepository (Core.Maybe Core.Bool)
dForce = Lens.field @"force"
{-# INLINEABLE dForce #-}
{-# DEPRECATED force "Use generic-lens or generic-optics with 'force' instead"  #-}

-- | The AWS account ID associated with the registry that contains the repository to delete. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRegistryId :: Lens.Lens' DeleteRepository (Core.Maybe Types.RegistryId)
dRegistryId = Lens.field @"registryId"
{-# INLINEABLE dRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

instance Core.ToQuery DeleteRepository where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteRepository where
        toHeaders DeleteRepository{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerRegistry_V20150921.DeleteRepository")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteRepository where
        toJSON DeleteRepository{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  ("force" Core..=) Core.<$> force,
                  ("registryId" Core..=) Core.<$> registryId])

instance Core.AWSRequest DeleteRepository where
        type Rs DeleteRepository = DeleteRepositoryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteRepositoryResponse' Core.<$>
                   (x Core..:? "repository") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteRepositoryResponse' smart constructor.
data DeleteRepositoryResponse = DeleteRepositoryResponse'
  { repository :: Core.Maybe Types.Repository
    -- ^ The repository that was deleted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteRepositoryResponse' value with any optional fields omitted.
mkDeleteRepositoryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteRepositoryResponse
mkDeleteRepositoryResponse responseStatus
  = DeleteRepositoryResponse'{repository = Core.Nothing,
                              responseStatus}

-- | The repository that was deleted.
--
-- /Note:/ Consider using 'repository' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsRepository :: Lens.Lens' DeleteRepositoryResponse (Core.Maybe Types.Repository)
drsRepository = Lens.field @"repository"
{-# INLINEABLE drsRepository #-}
{-# DEPRECATED repository "Use generic-lens or generic-optics with 'repository' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteRepositoryResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
