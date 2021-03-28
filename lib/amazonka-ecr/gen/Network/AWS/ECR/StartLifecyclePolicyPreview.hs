{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.StartLifecyclePolicyPreview
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a preview of a lifecycle policy for the specified repository. This allows you to see the results before associating the lifecycle policy with the repository.
module Network.AWS.ECR.StartLifecyclePolicyPreview
    (
    -- * Creating a request
      StartLifecyclePolicyPreview (..)
    , mkStartLifecyclePolicyPreview
    -- ** Request lenses
    , slppRepositoryName
    , slppLifecyclePolicyText
    , slppRegistryId

    -- * Destructuring the response
    , StartLifecyclePolicyPreviewResponse (..)
    , mkStartLifecyclePolicyPreviewResponse
    -- ** Response lenses
    , slpprrsLifecyclePolicyText
    , slpprrsRegistryId
    , slpprrsRepositoryName
    , slpprrsStatus
    , slpprrsResponseStatus
    ) where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartLifecyclePolicyPreview' smart constructor.
data StartLifecyclePolicyPreview = StartLifecyclePolicyPreview'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository to be evaluated.
  , lifecyclePolicyText :: Core.Maybe Types.LifecyclePolicyText
    -- ^ The policy to be evaluated against. If you do not specify a policy, the current policy for the repository is used.
  , registryId :: Core.Maybe Types.RegistryId
    -- ^ The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartLifecyclePolicyPreview' value with any optional fields omitted.
mkStartLifecyclePolicyPreview
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> StartLifecyclePolicyPreview
mkStartLifecyclePolicyPreview repositoryName
  = StartLifecyclePolicyPreview'{repositoryName,
                                 lifecyclePolicyText = Core.Nothing, registryId = Core.Nothing}

-- | The name of the repository to be evaluated.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slppRepositoryName :: Lens.Lens' StartLifecyclePolicyPreview Types.RepositoryName
slppRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE slppRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The policy to be evaluated against. If you do not specify a policy, the current policy for the repository is used.
--
-- /Note:/ Consider using 'lifecyclePolicyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slppLifecyclePolicyText :: Lens.Lens' StartLifecyclePolicyPreview (Core.Maybe Types.LifecyclePolicyText)
slppLifecyclePolicyText = Lens.field @"lifecyclePolicyText"
{-# INLINEABLE slppLifecyclePolicyText #-}
{-# DEPRECATED lifecyclePolicyText "Use generic-lens or generic-optics with 'lifecyclePolicyText' instead"  #-}

-- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slppRegistryId :: Lens.Lens' StartLifecyclePolicyPreview (Core.Maybe Types.RegistryId)
slppRegistryId = Lens.field @"registryId"
{-# INLINEABLE slppRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

instance Core.ToQuery StartLifecyclePolicyPreview where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartLifecyclePolicyPreview where
        toHeaders StartLifecyclePolicyPreview{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerRegistry_V20150921.StartLifecyclePolicyPreview")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartLifecyclePolicyPreview where
        toJSON StartLifecyclePolicyPreview{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  ("lifecyclePolicyText" Core..=) Core.<$> lifecyclePolicyText,
                  ("registryId" Core..=) Core.<$> registryId])

instance Core.AWSRequest StartLifecyclePolicyPreview where
        type Rs StartLifecyclePolicyPreview =
             StartLifecyclePolicyPreviewResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartLifecyclePolicyPreviewResponse' Core.<$>
                   (x Core..:? "lifecyclePolicyText") Core.<*> x Core..:? "registryId"
                     Core.<*> x Core..:? "repositoryName"
                     Core.<*> x Core..:? "status"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartLifecyclePolicyPreviewResponse' smart constructor.
data StartLifecyclePolicyPreviewResponse = StartLifecyclePolicyPreviewResponse'
  { lifecyclePolicyText :: Core.Maybe Types.LifecyclePolicyText
    -- ^ The JSON repository policy text.
  , registryId :: Core.Maybe Types.RegistryId
    -- ^ The registry ID associated with the request.
  , repositoryName :: Core.Maybe Types.RepositoryName
    -- ^ The repository name associated with the request.
  , status :: Core.Maybe Types.LifecyclePolicyPreviewStatus
    -- ^ The status of the lifecycle policy preview request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartLifecyclePolicyPreviewResponse' value with any optional fields omitted.
mkStartLifecyclePolicyPreviewResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartLifecyclePolicyPreviewResponse
mkStartLifecyclePolicyPreviewResponse responseStatus
  = StartLifecyclePolicyPreviewResponse'{lifecyclePolicyText =
                                           Core.Nothing,
                                         registryId = Core.Nothing, repositoryName = Core.Nothing,
                                         status = Core.Nothing, responseStatus}

-- | The JSON repository policy text.
--
-- /Note:/ Consider using 'lifecyclePolicyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slpprrsLifecyclePolicyText :: Lens.Lens' StartLifecyclePolicyPreviewResponse (Core.Maybe Types.LifecyclePolicyText)
slpprrsLifecyclePolicyText = Lens.field @"lifecyclePolicyText"
{-# INLINEABLE slpprrsLifecyclePolicyText #-}
{-# DEPRECATED lifecyclePolicyText "Use generic-lens or generic-optics with 'lifecyclePolicyText' instead"  #-}

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slpprrsRegistryId :: Lens.Lens' StartLifecyclePolicyPreviewResponse (Core.Maybe Types.RegistryId)
slpprrsRegistryId = Lens.field @"registryId"
{-# INLINEABLE slpprrsRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slpprrsRepositoryName :: Lens.Lens' StartLifecyclePolicyPreviewResponse (Core.Maybe Types.RepositoryName)
slpprrsRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE slpprrsRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The status of the lifecycle policy preview request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slpprrsStatus :: Lens.Lens' StartLifecyclePolicyPreviewResponse (Core.Maybe Types.LifecyclePolicyPreviewStatus)
slpprrsStatus = Lens.field @"status"
{-# INLINEABLE slpprrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slpprrsResponseStatus :: Lens.Lens' StartLifecyclePolicyPreviewResponse Core.Int
slpprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE slpprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
