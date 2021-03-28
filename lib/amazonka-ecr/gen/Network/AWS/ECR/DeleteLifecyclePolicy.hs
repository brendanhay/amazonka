{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.DeleteLifecyclePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the lifecycle policy associated with the specified repository.
module Network.AWS.ECR.DeleteLifecyclePolicy
    (
    -- * Creating a request
      DeleteLifecyclePolicy (..)
    , mkDeleteLifecyclePolicy
    -- ** Request lenses
    , dlpRepositoryName
    , dlpRegistryId

    -- * Destructuring the response
    , DeleteLifecyclePolicyResponse (..)
    , mkDeleteLifecyclePolicyResponse
    -- ** Response lenses
    , dlprrsLastEvaluatedAt
    , dlprrsLifecyclePolicyText
    , dlprrsRegistryId
    , dlprrsRepositoryName
    , dlprrsResponseStatus
    ) where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteLifecyclePolicy' smart constructor.
data DeleteLifecyclePolicy = DeleteLifecyclePolicy'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository.
  , registryId :: Core.Maybe Types.RegistryId
    -- ^ The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLifecyclePolicy' value with any optional fields omitted.
mkDeleteLifecyclePolicy
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> DeleteLifecyclePolicy
mkDeleteLifecyclePolicy repositoryName
  = DeleteLifecyclePolicy'{repositoryName, registryId = Core.Nothing}

-- | The name of the repository.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlpRepositoryName :: Lens.Lens' DeleteLifecyclePolicy Types.RepositoryName
dlpRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE dlpRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlpRegistryId :: Lens.Lens' DeleteLifecyclePolicy (Core.Maybe Types.RegistryId)
dlpRegistryId = Lens.field @"registryId"
{-# INLINEABLE dlpRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

instance Core.ToQuery DeleteLifecyclePolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteLifecyclePolicy where
        toHeaders DeleteLifecyclePolicy{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerRegistry_V20150921.DeleteLifecyclePolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteLifecyclePolicy where
        toJSON DeleteLifecyclePolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  ("registryId" Core..=) Core.<$> registryId])

instance Core.AWSRequest DeleteLifecyclePolicy where
        type Rs DeleteLifecyclePolicy = DeleteLifecyclePolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteLifecyclePolicyResponse' Core.<$>
                   (x Core..:? "lastEvaluatedAt") Core.<*>
                     x Core..:? "lifecyclePolicyText"
                     Core.<*> x Core..:? "registryId"
                     Core.<*> x Core..:? "repositoryName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteLifecyclePolicyResponse' smart constructor.
data DeleteLifecyclePolicyResponse = DeleteLifecyclePolicyResponse'
  { lastEvaluatedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time stamp of the last time that the lifecycle policy was run.
  , lifecyclePolicyText :: Core.Maybe Types.LifecyclePolicyText
    -- ^ The JSON lifecycle policy text.
  , registryId :: Core.Maybe Types.RegistryId
    -- ^ The registry ID associated with the request.
  , repositoryName :: Core.Maybe Types.RepositoryName
    -- ^ The repository name associated with the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteLifecyclePolicyResponse' value with any optional fields omitted.
mkDeleteLifecyclePolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteLifecyclePolicyResponse
mkDeleteLifecyclePolicyResponse responseStatus
  = DeleteLifecyclePolicyResponse'{lastEvaluatedAt = Core.Nothing,
                                   lifecyclePolicyText = Core.Nothing, registryId = Core.Nothing,
                                   repositoryName = Core.Nothing, responseStatus}

-- | The time stamp of the last time that the lifecycle policy was run.
--
-- /Note:/ Consider using 'lastEvaluatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlprrsLastEvaluatedAt :: Lens.Lens' DeleteLifecyclePolicyResponse (Core.Maybe Core.NominalDiffTime)
dlprrsLastEvaluatedAt = Lens.field @"lastEvaluatedAt"
{-# INLINEABLE dlprrsLastEvaluatedAt #-}
{-# DEPRECATED lastEvaluatedAt "Use generic-lens or generic-optics with 'lastEvaluatedAt' instead"  #-}

-- | The JSON lifecycle policy text.
--
-- /Note:/ Consider using 'lifecyclePolicyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlprrsLifecyclePolicyText :: Lens.Lens' DeleteLifecyclePolicyResponse (Core.Maybe Types.LifecyclePolicyText)
dlprrsLifecyclePolicyText = Lens.field @"lifecyclePolicyText"
{-# INLINEABLE dlprrsLifecyclePolicyText #-}
{-# DEPRECATED lifecyclePolicyText "Use generic-lens or generic-optics with 'lifecyclePolicyText' instead"  #-}

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlprrsRegistryId :: Lens.Lens' DeleteLifecyclePolicyResponse (Core.Maybe Types.RegistryId)
dlprrsRegistryId = Lens.field @"registryId"
{-# INLINEABLE dlprrsRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlprrsRepositoryName :: Lens.Lens' DeleteLifecyclePolicyResponse (Core.Maybe Types.RepositoryName)
dlprrsRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE dlprrsRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlprrsResponseStatus :: Lens.Lens' DeleteLifecyclePolicyResponse Core.Int
dlprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dlprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
