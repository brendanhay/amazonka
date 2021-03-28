{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.PutLifecyclePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the lifecycle policy for the specified repository. For more information, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/LifecyclePolicies.html Lifecycle Policy Template> .
module Network.AWS.ECR.PutLifecyclePolicy
    (
    -- * Creating a request
      PutLifecyclePolicy (..)
    , mkPutLifecyclePolicy
    -- ** Request lenses
    , plpRepositoryName
    , plpLifecyclePolicyText
    , plpRegistryId

    -- * Destructuring the response
    , PutLifecyclePolicyResponse (..)
    , mkPutLifecyclePolicyResponse
    -- ** Response lenses
    , plprrsLifecyclePolicyText
    , plprrsRegistryId
    , plprrsRepositoryName
    , plprrsResponseStatus
    ) where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutLifecyclePolicy' smart constructor.
data PutLifecyclePolicy = PutLifecyclePolicy'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository to receive the policy.
  , lifecyclePolicyText :: Types.LifecyclePolicyText
    -- ^ The JSON repository policy text to apply to the repository.
  , registryId :: Core.Maybe Types.RegistryId
    -- ^ The AWS account ID associated with the registry that contains the repository. If you do  not specify a registry, the default registry is assumed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutLifecyclePolicy' value with any optional fields omitted.
mkPutLifecyclePolicy
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> Types.LifecyclePolicyText -- ^ 'lifecyclePolicyText'
    -> PutLifecyclePolicy
mkPutLifecyclePolicy repositoryName lifecyclePolicyText
  = PutLifecyclePolicy'{repositoryName, lifecyclePolicyText,
                        registryId = Core.Nothing}

-- | The name of the repository to receive the policy.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plpRepositoryName :: Lens.Lens' PutLifecyclePolicy Types.RepositoryName
plpRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE plpRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The JSON repository policy text to apply to the repository.
--
-- /Note:/ Consider using 'lifecyclePolicyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plpLifecyclePolicyText :: Lens.Lens' PutLifecyclePolicy Types.LifecyclePolicyText
plpLifecyclePolicyText = Lens.field @"lifecyclePolicyText"
{-# INLINEABLE plpLifecyclePolicyText #-}
{-# DEPRECATED lifecyclePolicyText "Use generic-lens or generic-optics with 'lifecyclePolicyText' instead"  #-}

-- | The AWS account ID associated with the registry that contains the repository. If you do  not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plpRegistryId :: Lens.Lens' PutLifecyclePolicy (Core.Maybe Types.RegistryId)
plpRegistryId = Lens.field @"registryId"
{-# INLINEABLE plpRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

instance Core.ToQuery PutLifecyclePolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutLifecyclePolicy where
        toHeaders PutLifecyclePolicy{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerRegistry_V20150921.PutLifecyclePolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutLifecyclePolicy where
        toJSON PutLifecyclePolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  Core.Just ("lifecyclePolicyText" Core..= lifecyclePolicyText),
                  ("registryId" Core..=) Core.<$> registryId])

instance Core.AWSRequest PutLifecyclePolicy where
        type Rs PutLifecyclePolicy = PutLifecyclePolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutLifecyclePolicyResponse' Core.<$>
                   (x Core..:? "lifecyclePolicyText") Core.<*> x Core..:? "registryId"
                     Core.<*> x Core..:? "repositoryName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutLifecyclePolicyResponse' smart constructor.
data PutLifecyclePolicyResponse = PutLifecyclePolicyResponse'
  { lifecyclePolicyText :: Core.Maybe Types.LifecyclePolicyText
    -- ^ The JSON repository policy text.
  , registryId :: Core.Maybe Types.RegistryId
    -- ^ The registry ID associated with the request.
  , repositoryName :: Core.Maybe Types.RepositoryName
    -- ^ The repository name associated with the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutLifecyclePolicyResponse' value with any optional fields omitted.
mkPutLifecyclePolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutLifecyclePolicyResponse
mkPutLifecyclePolicyResponse responseStatus
  = PutLifecyclePolicyResponse'{lifecyclePolicyText = Core.Nothing,
                                registryId = Core.Nothing, repositoryName = Core.Nothing,
                                responseStatus}

-- | The JSON repository policy text.
--
-- /Note:/ Consider using 'lifecyclePolicyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plprrsLifecyclePolicyText :: Lens.Lens' PutLifecyclePolicyResponse (Core.Maybe Types.LifecyclePolicyText)
plprrsLifecyclePolicyText = Lens.field @"lifecyclePolicyText"
{-# INLINEABLE plprrsLifecyclePolicyText #-}
{-# DEPRECATED lifecyclePolicyText "Use generic-lens or generic-optics with 'lifecyclePolicyText' instead"  #-}

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plprrsRegistryId :: Lens.Lens' PutLifecyclePolicyResponse (Core.Maybe Types.RegistryId)
plprrsRegistryId = Lens.field @"registryId"
{-# INLINEABLE plprrsRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plprrsRepositoryName :: Lens.Lens' PutLifecyclePolicyResponse (Core.Maybe Types.RepositoryName)
plprrsRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE plprrsRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plprrsResponseStatus :: Lens.Lens' PutLifecyclePolicyResponse Core.Int
plprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE plprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
