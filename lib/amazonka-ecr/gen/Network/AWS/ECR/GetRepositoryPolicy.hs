{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.GetRepositoryPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the repository policy for the specified repository.
module Network.AWS.ECR.GetRepositoryPolicy
    (
    -- * Creating a request
      GetRepositoryPolicy (..)
    , mkGetRepositoryPolicy
    -- ** Request lenses
    , grpRepositoryName
    , grpRegistryId

    -- * Destructuring the response
    , GetRepositoryPolicyResponse (..)
    , mkGetRepositoryPolicyResponse
    -- ** Response lenses
    , grprrsPolicyText
    , grprrsRegistryId
    , grprrsRepositoryName
    , grprrsResponseStatus
    ) where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetRepositoryPolicy' smart constructor.
data GetRepositoryPolicy = GetRepositoryPolicy'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository with the policy to retrieve.
  , registryId :: Core.Maybe Types.RegistryId
    -- ^ The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRepositoryPolicy' value with any optional fields omitted.
mkGetRepositoryPolicy
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> GetRepositoryPolicy
mkGetRepositoryPolicy repositoryName
  = GetRepositoryPolicy'{repositoryName, registryId = Core.Nothing}

-- | The name of the repository with the policy to retrieve.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpRepositoryName :: Lens.Lens' GetRepositoryPolicy Types.RepositoryName
grpRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE grpRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpRegistryId :: Lens.Lens' GetRepositoryPolicy (Core.Maybe Types.RegistryId)
grpRegistryId = Lens.field @"registryId"
{-# INLINEABLE grpRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

instance Core.ToQuery GetRepositoryPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetRepositoryPolicy where
        toHeaders GetRepositoryPolicy{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerRegistry_V20150921.GetRepositoryPolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetRepositoryPolicy where
        toJSON GetRepositoryPolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  ("registryId" Core..=) Core.<$> registryId])

instance Core.AWSRequest GetRepositoryPolicy where
        type Rs GetRepositoryPolicy = GetRepositoryPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetRepositoryPolicyResponse' Core.<$>
                   (x Core..:? "policyText") Core.<*> x Core..:? "registryId" Core.<*>
                     x Core..:? "repositoryName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetRepositoryPolicyResponse' smart constructor.
data GetRepositoryPolicyResponse = GetRepositoryPolicyResponse'
  { policyText :: Core.Maybe Types.PolicyText
    -- ^ The JSON repository policy text associated with the repository.
  , registryId :: Core.Maybe Types.RegistryId
    -- ^ The registry ID associated with the request.
  , repositoryName :: Core.Maybe Types.RepositoryName
    -- ^ The repository name associated with the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRepositoryPolicyResponse' value with any optional fields omitted.
mkGetRepositoryPolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetRepositoryPolicyResponse
mkGetRepositoryPolicyResponse responseStatus
  = GetRepositoryPolicyResponse'{policyText = Core.Nothing,
                                 registryId = Core.Nothing, repositoryName = Core.Nothing,
                                 responseStatus}

-- | The JSON repository policy text associated with the repository.
--
-- /Note:/ Consider using 'policyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsPolicyText :: Lens.Lens' GetRepositoryPolicyResponse (Core.Maybe Types.PolicyText)
grprrsPolicyText = Lens.field @"policyText"
{-# INLINEABLE grprrsPolicyText #-}
{-# DEPRECATED policyText "Use generic-lens or generic-optics with 'policyText' instead"  #-}

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsRegistryId :: Lens.Lens' GetRepositoryPolicyResponse (Core.Maybe Types.RegistryId)
grprrsRegistryId = Lens.field @"registryId"
{-# INLINEABLE grprrsRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsRepositoryName :: Lens.Lens' GetRepositoryPolicyResponse (Core.Maybe Types.RepositoryName)
grprrsRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE grprrsRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsResponseStatus :: Lens.Lens' GetRepositoryPolicyResponse Core.Int
grprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
