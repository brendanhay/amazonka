{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.SetRepositoryPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a repository policy to the specified repository to control access permissions. For more information, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/repository-policies.html Amazon ECR Repository Policies> in the /Amazon Elastic Container Registry User Guide/ .
module Network.AWS.ECR.SetRepositoryPolicy
    (
    -- * Creating a request
      SetRepositoryPolicy (..)
    , mkSetRepositoryPolicy
    -- ** Request lenses
    , srpRepositoryName
    , srpPolicyText
    , srpForce
    , srpRegistryId

    -- * Destructuring the response
    , SetRepositoryPolicyResponse (..)
    , mkSetRepositoryPolicyResponse
    -- ** Response lenses
    , srprrsPolicyText
    , srprrsRegistryId
    , srprrsRepositoryName
    , srprrsResponseStatus
    ) where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSetRepositoryPolicy' smart constructor.
data SetRepositoryPolicy = SetRepositoryPolicy'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository to receive the policy.
  , policyText :: Types.PolicyText
    -- ^ The JSON repository policy text to apply to the repository. For more information, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/repository-policy-examples.html Amazon ECR Repository Policies> in the /Amazon Elastic Container Registry User Guide/ .
  , force :: Core.Maybe Core.Bool
    -- ^ If the policy you are attempting to set on a repository policy would prevent you from setting another policy in the future, you must force the 'SetRepositoryPolicy' operation. This is intended to prevent accidental repository lock outs.
  , registryId :: Core.Maybe Types.RegistryId
    -- ^ The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetRepositoryPolicy' value with any optional fields omitted.
mkSetRepositoryPolicy
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> Types.PolicyText -- ^ 'policyText'
    -> SetRepositoryPolicy
mkSetRepositoryPolicy repositoryName policyText
  = SetRepositoryPolicy'{repositoryName, policyText,
                         force = Core.Nothing, registryId = Core.Nothing}

-- | The name of the repository to receive the policy.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpRepositoryName :: Lens.Lens' SetRepositoryPolicy Types.RepositoryName
srpRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE srpRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The JSON repository policy text to apply to the repository. For more information, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/repository-policy-examples.html Amazon ECR Repository Policies> in the /Amazon Elastic Container Registry User Guide/ .
--
-- /Note:/ Consider using 'policyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpPolicyText :: Lens.Lens' SetRepositoryPolicy Types.PolicyText
srpPolicyText = Lens.field @"policyText"
{-# INLINEABLE srpPolicyText #-}
{-# DEPRECATED policyText "Use generic-lens or generic-optics with 'policyText' instead"  #-}

-- | If the policy you are attempting to set on a repository policy would prevent you from setting another policy in the future, you must force the 'SetRepositoryPolicy' operation. This is intended to prevent accidental repository lock outs.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpForce :: Lens.Lens' SetRepositoryPolicy (Core.Maybe Core.Bool)
srpForce = Lens.field @"force"
{-# INLINEABLE srpForce #-}
{-# DEPRECATED force "Use generic-lens or generic-optics with 'force' instead"  #-}

-- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpRegistryId :: Lens.Lens' SetRepositoryPolicy (Core.Maybe Types.RegistryId)
srpRegistryId = Lens.field @"registryId"
{-# INLINEABLE srpRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

instance Core.ToQuery SetRepositoryPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SetRepositoryPolicy where
        toHeaders SetRepositoryPolicy{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerRegistry_V20150921.SetRepositoryPolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SetRepositoryPolicy where
        toJSON SetRepositoryPolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  Core.Just ("policyText" Core..= policyText),
                  ("force" Core..=) Core.<$> force,
                  ("registryId" Core..=) Core.<$> registryId])

instance Core.AWSRequest SetRepositoryPolicy where
        type Rs SetRepositoryPolicy = SetRepositoryPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 SetRepositoryPolicyResponse' Core.<$>
                   (x Core..:? "policyText") Core.<*> x Core..:? "registryId" Core.<*>
                     x Core..:? "repositoryName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetRepositoryPolicyResponse' smart constructor.
data SetRepositoryPolicyResponse = SetRepositoryPolicyResponse'
  { policyText :: Core.Maybe Types.RepositoryPolicyText
    -- ^ The JSON repository policy text applied to the repository.
  , registryId :: Core.Maybe Types.RegistryId
    -- ^ The registry ID associated with the request.
  , repositoryName :: Core.Maybe Types.RepositoryName
    -- ^ The repository name associated with the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetRepositoryPolicyResponse' value with any optional fields omitted.
mkSetRepositoryPolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SetRepositoryPolicyResponse
mkSetRepositoryPolicyResponse responseStatus
  = SetRepositoryPolicyResponse'{policyText = Core.Nothing,
                                 registryId = Core.Nothing, repositoryName = Core.Nothing,
                                 responseStatus}

-- | The JSON repository policy text applied to the repository.
--
-- /Note:/ Consider using 'policyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srprrsPolicyText :: Lens.Lens' SetRepositoryPolicyResponse (Core.Maybe Types.RepositoryPolicyText)
srprrsPolicyText = Lens.field @"policyText"
{-# INLINEABLE srprrsPolicyText #-}
{-# DEPRECATED policyText "Use generic-lens or generic-optics with 'policyText' instead"  #-}

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srprrsRegistryId :: Lens.Lens' SetRepositoryPolicyResponse (Core.Maybe Types.RegistryId)
srprrsRegistryId = Lens.field @"registryId"
{-# INLINEABLE srprrsRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srprrsRepositoryName :: Lens.Lens' SetRepositoryPolicyResponse (Core.Maybe Types.RepositoryName)
srprrsRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE srprrsRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srprrsResponseStatus :: Lens.Lens' SetRepositoryPolicyResponse Core.Int
srprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
