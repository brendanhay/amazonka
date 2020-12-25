{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetLifecyclePolicy (..),
    mkGetLifecyclePolicy,

    -- ** Request lenses
    glpRepositoryName,
    glpRegistryId,

    -- * Destructuring the response
    GetLifecyclePolicyResponse (..),
    mkGetLifecyclePolicyResponse,

    -- ** Response lenses
    glprrsLastEvaluatedAt,
    glprrsLifecyclePolicyText,
    glprrsRegistryId,
    glprrsRepositoryName,
    glprrsResponseStatus,
  )
where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetLifecyclePolicy' smart constructor.
data GetLifecyclePolicy = GetLifecyclePolicy'
  { -- | The name of the repository.
    repositoryName :: Types.RepositoryName,
    -- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
    registryId :: Core.Maybe Types.RegistryId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetLifecyclePolicy' value with any optional fields omitted.
mkGetLifecyclePolicy ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  GetLifecyclePolicy
mkGetLifecyclePolicy repositoryName =
  GetLifecyclePolicy' {repositoryName, registryId = Core.Nothing}

-- | The name of the repository.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpRepositoryName :: Lens.Lens' GetLifecyclePolicy Types.RepositoryName
glpRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED glpRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpRegistryId :: Lens.Lens' GetLifecyclePolicy (Core.Maybe Types.RegistryId)
glpRegistryId = Lens.field @"registryId"
{-# DEPRECATED glpRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

instance Core.FromJSON GetLifecyclePolicy where
  toJSON GetLifecyclePolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            ("registryId" Core..=) Core.<$> registryId
          ]
      )

instance Core.AWSRequest GetLifecyclePolicy where
  type Rs GetLifecyclePolicy = GetLifecyclePolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerRegistry_V20150921.GetLifecyclePolicy"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLifecyclePolicyResponse'
            Core.<$> (x Core..:? "lastEvaluatedAt")
            Core.<*> (x Core..:? "lifecyclePolicyText")
            Core.<*> (x Core..:? "registryId")
            Core.<*> (x Core..:? "repositoryName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetLifecyclePolicyResponse' smart constructor.
data GetLifecyclePolicyResponse = GetLifecyclePolicyResponse'
  { -- | The time stamp of the last time that the lifecycle policy was run.
    lastEvaluatedAt :: Core.Maybe Core.NominalDiffTime,
    -- | The JSON lifecycle policy text.
    lifecyclePolicyText :: Core.Maybe Types.LifecyclePolicyText,
    -- | The registry ID associated with the request.
    registryId :: Core.Maybe Types.RegistryId,
    -- | The repository name associated with the request.
    repositoryName :: Core.Maybe Types.RepositoryName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetLifecyclePolicyResponse' value with any optional fields omitted.
mkGetLifecyclePolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetLifecyclePolicyResponse
mkGetLifecyclePolicyResponse responseStatus =
  GetLifecyclePolicyResponse'
    { lastEvaluatedAt = Core.Nothing,
      lifecyclePolicyText = Core.Nothing,
      registryId = Core.Nothing,
      repositoryName = Core.Nothing,
      responseStatus
    }

-- | The time stamp of the last time that the lifecycle policy was run.
--
-- /Note:/ Consider using 'lastEvaluatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glprrsLastEvaluatedAt :: Lens.Lens' GetLifecyclePolicyResponse (Core.Maybe Core.NominalDiffTime)
glprrsLastEvaluatedAt = Lens.field @"lastEvaluatedAt"
{-# DEPRECATED glprrsLastEvaluatedAt "Use generic-lens or generic-optics with 'lastEvaluatedAt' instead." #-}

-- | The JSON lifecycle policy text.
--
-- /Note:/ Consider using 'lifecyclePolicyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glprrsLifecyclePolicyText :: Lens.Lens' GetLifecyclePolicyResponse (Core.Maybe Types.LifecyclePolicyText)
glprrsLifecyclePolicyText = Lens.field @"lifecyclePolicyText"
{-# DEPRECATED glprrsLifecyclePolicyText "Use generic-lens or generic-optics with 'lifecyclePolicyText' instead." #-}

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glprrsRegistryId :: Lens.Lens' GetLifecyclePolicyResponse (Core.Maybe Types.RegistryId)
glprrsRegistryId = Lens.field @"registryId"
{-# DEPRECATED glprrsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glprrsRepositoryName :: Lens.Lens' GetLifecyclePolicyResponse (Core.Maybe Types.RepositoryName)
glprrsRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED glprrsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glprrsResponseStatus :: Lens.Lens' GetLifecyclePolicyResponse Core.Int
glprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED glprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
