{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteLifecyclePolicy (..),
    mkDeleteLifecyclePolicy,

    -- ** Request lenses
    dlpRepositoryName,
    dlpRegistryId,

    -- * Destructuring the response
    DeleteLifecyclePolicyResponse (..),
    mkDeleteLifecyclePolicyResponse,

    -- ** Response lenses
    dlprrsLastEvaluatedAt,
    dlprrsLifecyclePolicyText,
    dlprrsRegistryId,
    dlprrsRepositoryName,
    dlprrsResponseStatus,
  )
where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteLifecyclePolicy' smart constructor.
data DeleteLifecyclePolicy = DeleteLifecyclePolicy'
  { -- | The name of the repository.
    repositoryName :: Types.RepositoryName,
    -- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
    registryId :: Core.Maybe Types.RegistryId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLifecyclePolicy' value with any optional fields omitted.
mkDeleteLifecyclePolicy ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  DeleteLifecyclePolicy
mkDeleteLifecyclePolicy repositoryName =
  DeleteLifecyclePolicy' {repositoryName, registryId = Core.Nothing}

-- | The name of the repository.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlpRepositoryName :: Lens.Lens' DeleteLifecyclePolicy Types.RepositoryName
dlpRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED dlpRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlpRegistryId :: Lens.Lens' DeleteLifecyclePolicy (Core.Maybe Types.RegistryId)
dlpRegistryId = Lens.field @"registryId"
{-# DEPRECATED dlpRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

instance Core.FromJSON DeleteLifecyclePolicy where
  toJSON DeleteLifecyclePolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            ("registryId" Core..=) Core.<$> registryId
          ]
      )

instance Core.AWSRequest DeleteLifecyclePolicy where
  type Rs DeleteLifecyclePolicy = DeleteLifecyclePolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerRegistry_V20150921.DeleteLifecyclePolicy"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteLifecyclePolicyResponse'
            Core.<$> (x Core..:? "lastEvaluatedAt")
            Core.<*> (x Core..:? "lifecyclePolicyText")
            Core.<*> (x Core..:? "registryId")
            Core.<*> (x Core..:? "repositoryName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteLifecyclePolicyResponse' smart constructor.
data DeleteLifecyclePolicyResponse = DeleteLifecyclePolicyResponse'
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

-- | Creates a 'DeleteLifecyclePolicyResponse' value with any optional fields omitted.
mkDeleteLifecyclePolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteLifecyclePolicyResponse
mkDeleteLifecyclePolicyResponse responseStatus =
  DeleteLifecyclePolicyResponse'
    { lastEvaluatedAt = Core.Nothing,
      lifecyclePolicyText = Core.Nothing,
      registryId = Core.Nothing,
      repositoryName = Core.Nothing,
      responseStatus
    }

-- | The time stamp of the last time that the lifecycle policy was run.
--
-- /Note:/ Consider using 'lastEvaluatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlprrsLastEvaluatedAt :: Lens.Lens' DeleteLifecyclePolicyResponse (Core.Maybe Core.NominalDiffTime)
dlprrsLastEvaluatedAt = Lens.field @"lastEvaluatedAt"
{-# DEPRECATED dlprrsLastEvaluatedAt "Use generic-lens or generic-optics with 'lastEvaluatedAt' instead." #-}

-- | The JSON lifecycle policy text.
--
-- /Note:/ Consider using 'lifecyclePolicyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlprrsLifecyclePolicyText :: Lens.Lens' DeleteLifecyclePolicyResponse (Core.Maybe Types.LifecyclePolicyText)
dlprrsLifecyclePolicyText = Lens.field @"lifecyclePolicyText"
{-# DEPRECATED dlprrsLifecyclePolicyText "Use generic-lens or generic-optics with 'lifecyclePolicyText' instead." #-}

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlprrsRegistryId :: Lens.Lens' DeleteLifecyclePolicyResponse (Core.Maybe Types.RegistryId)
dlprrsRegistryId = Lens.field @"registryId"
{-# DEPRECATED dlprrsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlprrsRepositoryName :: Lens.Lens' DeleteLifecyclePolicyResponse (Core.Maybe Types.RepositoryName)
dlprrsRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED dlprrsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlprrsResponseStatus :: Lens.Lens' DeleteLifecyclePolicyResponse Core.Int
dlprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
