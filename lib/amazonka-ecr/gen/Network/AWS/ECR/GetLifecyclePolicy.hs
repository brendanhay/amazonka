{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.GetLifecyclePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the lifecycle policy for the specified repository.
module Network.AWS.ECR.GetLifecyclePolicy
    (
    -- * Creating a request
      GetLifecyclePolicy (..)
    , mkGetLifecyclePolicy
    -- ** Request lenses
    , glpRepositoryName
    , glpRegistryId

    -- * Destructuring the response
    , GetLifecyclePolicyResponse (..)
    , mkGetLifecyclePolicyResponse
    -- ** Response lenses
    , glprrsLastEvaluatedAt
    , glprrsLifecyclePolicyText
    , glprrsRegistryId
    , glprrsRepositoryName
    , glprrsResponseStatus
    ) where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetLifecyclePolicy' smart constructor.
data GetLifecyclePolicy = GetLifecyclePolicy'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository.
  , registryId :: Core.Maybe Types.RegistryId
    -- ^ The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetLifecyclePolicy' value with any optional fields omitted.
mkGetLifecyclePolicy
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> GetLifecyclePolicy
mkGetLifecyclePolicy repositoryName
  = GetLifecyclePolicy'{repositoryName, registryId = Core.Nothing}

-- | The name of the repository.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpRepositoryName :: Lens.Lens' GetLifecyclePolicy Types.RepositoryName
glpRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE glpRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpRegistryId :: Lens.Lens' GetLifecyclePolicy (Core.Maybe Types.RegistryId)
glpRegistryId = Lens.field @"registryId"
{-# INLINEABLE glpRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

instance Core.ToQuery GetLifecyclePolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetLifecyclePolicy where
        toHeaders GetLifecyclePolicy{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerRegistry_V20150921.GetLifecyclePolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetLifecyclePolicy where
        toJSON GetLifecyclePolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  ("registryId" Core..=) Core.<$> registryId])

instance Core.AWSRequest GetLifecyclePolicy where
        type Rs GetLifecyclePolicy = GetLifecyclePolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetLifecyclePolicyResponse' Core.<$>
                   (x Core..:? "lastEvaluatedAt") Core.<*>
                     x Core..:? "lifecyclePolicyText"
                     Core.<*> x Core..:? "registryId"
                     Core.<*> x Core..:? "repositoryName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetLifecyclePolicyResponse' smart constructor.
data GetLifecyclePolicyResponse = GetLifecyclePolicyResponse'
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

-- | Creates a 'GetLifecyclePolicyResponse' value with any optional fields omitted.
mkGetLifecyclePolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetLifecyclePolicyResponse
mkGetLifecyclePolicyResponse responseStatus
  = GetLifecyclePolicyResponse'{lastEvaluatedAt = Core.Nothing,
                                lifecyclePolicyText = Core.Nothing, registryId = Core.Nothing,
                                repositoryName = Core.Nothing, responseStatus}

-- | The time stamp of the last time that the lifecycle policy was run.
--
-- /Note:/ Consider using 'lastEvaluatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glprrsLastEvaluatedAt :: Lens.Lens' GetLifecyclePolicyResponse (Core.Maybe Core.NominalDiffTime)
glprrsLastEvaluatedAt = Lens.field @"lastEvaluatedAt"
{-# INLINEABLE glprrsLastEvaluatedAt #-}
{-# DEPRECATED lastEvaluatedAt "Use generic-lens or generic-optics with 'lastEvaluatedAt' instead"  #-}

-- | The JSON lifecycle policy text.
--
-- /Note:/ Consider using 'lifecyclePolicyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glprrsLifecyclePolicyText :: Lens.Lens' GetLifecyclePolicyResponse (Core.Maybe Types.LifecyclePolicyText)
glprrsLifecyclePolicyText = Lens.field @"lifecyclePolicyText"
{-# INLINEABLE glprrsLifecyclePolicyText #-}
{-# DEPRECATED lifecyclePolicyText "Use generic-lens or generic-optics with 'lifecyclePolicyText' instead"  #-}

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glprrsRegistryId :: Lens.Lens' GetLifecyclePolicyResponse (Core.Maybe Types.RegistryId)
glprrsRegistryId = Lens.field @"registryId"
{-# INLINEABLE glprrsRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glprrsRepositoryName :: Lens.Lens' GetLifecyclePolicyResponse (Core.Maybe Types.RepositoryName)
glprrsRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE glprrsRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glprrsResponseStatus :: Lens.Lens' GetLifecyclePolicyResponse Core.Int
glprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE glprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
