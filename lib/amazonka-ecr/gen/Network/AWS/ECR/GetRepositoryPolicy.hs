{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetRepositoryPolicy (..),
    mkGetRepositoryPolicy,

    -- ** Request lenses
    grpRepositoryName,
    grpRegistryId,

    -- * Destructuring the response
    GetRepositoryPolicyResponse (..),
    mkGetRepositoryPolicyResponse,

    -- ** Response lenses
    grprrsPolicyText,
    grprrsRegistryId,
    grprrsRepositoryName,
    grprrsResponseStatus,
  )
where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetRepositoryPolicy' smart constructor.
data GetRepositoryPolicy = GetRepositoryPolicy'
  { -- | The name of the repository with the policy to retrieve.
    repositoryName :: Types.RepositoryName,
    -- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
    registryId :: Core.Maybe Types.RegistryId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRepositoryPolicy' value with any optional fields omitted.
mkGetRepositoryPolicy ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  GetRepositoryPolicy
mkGetRepositoryPolicy repositoryName =
  GetRepositoryPolicy' {repositoryName, registryId = Core.Nothing}

-- | The name of the repository with the policy to retrieve.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpRepositoryName :: Lens.Lens' GetRepositoryPolicy Types.RepositoryName
grpRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED grpRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpRegistryId :: Lens.Lens' GetRepositoryPolicy (Core.Maybe Types.RegistryId)
grpRegistryId = Lens.field @"registryId"
{-# DEPRECATED grpRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

instance Core.FromJSON GetRepositoryPolicy where
  toJSON GetRepositoryPolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            ("registryId" Core..=) Core.<$> registryId
          ]
      )

instance Core.AWSRequest GetRepositoryPolicy where
  type Rs GetRepositoryPolicy = GetRepositoryPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerRegistry_V20150921.GetRepositoryPolicy"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRepositoryPolicyResponse'
            Core.<$> (x Core..:? "policyText")
            Core.<*> (x Core..:? "registryId")
            Core.<*> (x Core..:? "repositoryName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetRepositoryPolicyResponse' smart constructor.
data GetRepositoryPolicyResponse = GetRepositoryPolicyResponse'
  { -- | The JSON repository policy text associated with the repository.
    policyText :: Core.Maybe Types.PolicyText,
    -- | The registry ID associated with the request.
    registryId :: Core.Maybe Types.RegistryId,
    -- | The repository name associated with the request.
    repositoryName :: Core.Maybe Types.RepositoryName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRepositoryPolicyResponse' value with any optional fields omitted.
mkGetRepositoryPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetRepositoryPolicyResponse
mkGetRepositoryPolicyResponse responseStatus =
  GetRepositoryPolicyResponse'
    { policyText = Core.Nothing,
      registryId = Core.Nothing,
      repositoryName = Core.Nothing,
      responseStatus
    }

-- | The JSON repository policy text associated with the repository.
--
-- /Note:/ Consider using 'policyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsPolicyText :: Lens.Lens' GetRepositoryPolicyResponse (Core.Maybe Types.PolicyText)
grprrsPolicyText = Lens.field @"policyText"
{-# DEPRECATED grprrsPolicyText "Use generic-lens or generic-optics with 'policyText' instead." #-}

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsRegistryId :: Lens.Lens' GetRepositoryPolicyResponse (Core.Maybe Types.RegistryId)
grprrsRegistryId = Lens.field @"registryId"
{-# DEPRECATED grprrsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsRepositoryName :: Lens.Lens' GetRepositoryPolicyResponse (Core.Maybe Types.RepositoryName)
grprrsRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED grprrsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsResponseStatus :: Lens.Lens' GetRepositoryPolicyResponse Core.Int
grprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
