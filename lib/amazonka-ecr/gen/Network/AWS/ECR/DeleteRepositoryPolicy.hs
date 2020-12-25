{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.DeleteRepositoryPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the repository policy associated with the specified repository.
module Network.AWS.ECR.DeleteRepositoryPolicy
  ( -- * Creating a request
    DeleteRepositoryPolicy (..),
    mkDeleteRepositoryPolicy,

    -- ** Request lenses
    drpRepositoryName,
    drpRegistryId,

    -- * Destructuring the response
    DeleteRepositoryPolicyResponse (..),
    mkDeleteRepositoryPolicyResponse,

    -- ** Response lenses
    drprrsPolicyText,
    drprrsRegistryId,
    drprrsRepositoryName,
    drprrsResponseStatus,
  )
where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteRepositoryPolicy' smart constructor.
data DeleteRepositoryPolicy = DeleteRepositoryPolicy'
  { -- | The name of the repository that is associated with the repository policy to delete.
    repositoryName :: Types.RepositoryName,
    -- | The AWS account ID associated with the registry that contains the repository policy to delete. If you do not specify a registry, the default registry is assumed.
    registryId :: Core.Maybe Types.RegistryId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRepositoryPolicy' value with any optional fields omitted.
mkDeleteRepositoryPolicy ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  DeleteRepositoryPolicy
mkDeleteRepositoryPolicy repositoryName =
  DeleteRepositoryPolicy'
    { repositoryName,
      registryId = Core.Nothing
    }

-- | The name of the repository that is associated with the repository policy to delete.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpRepositoryName :: Lens.Lens' DeleteRepositoryPolicy Types.RepositoryName
drpRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED drpRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The AWS account ID associated with the registry that contains the repository policy to delete. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpRegistryId :: Lens.Lens' DeleteRepositoryPolicy (Core.Maybe Types.RegistryId)
drpRegistryId = Lens.field @"registryId"
{-# DEPRECATED drpRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

instance Core.FromJSON DeleteRepositoryPolicy where
  toJSON DeleteRepositoryPolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            ("registryId" Core..=) Core.<$> registryId
          ]
      )

instance Core.AWSRequest DeleteRepositoryPolicy where
  type Rs DeleteRepositoryPolicy = DeleteRepositoryPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerRegistry_V20150921.DeleteRepositoryPolicy"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRepositoryPolicyResponse'
            Core.<$> (x Core..:? "policyText")
            Core.<*> (x Core..:? "registryId")
            Core.<*> (x Core..:? "repositoryName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteRepositoryPolicyResponse' smart constructor.
data DeleteRepositoryPolicyResponse = DeleteRepositoryPolicyResponse'
  { -- | The JSON repository policy that was deleted from the repository.
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

-- | Creates a 'DeleteRepositoryPolicyResponse' value with any optional fields omitted.
mkDeleteRepositoryPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteRepositoryPolicyResponse
mkDeleteRepositoryPolicyResponse responseStatus =
  DeleteRepositoryPolicyResponse'
    { policyText = Core.Nothing,
      registryId = Core.Nothing,
      repositoryName = Core.Nothing,
      responseStatus
    }

-- | The JSON repository policy that was deleted from the repository.
--
-- /Note:/ Consider using 'policyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprrsPolicyText :: Lens.Lens' DeleteRepositoryPolicyResponse (Core.Maybe Types.PolicyText)
drprrsPolicyText = Lens.field @"policyText"
{-# DEPRECATED drprrsPolicyText "Use generic-lens or generic-optics with 'policyText' instead." #-}

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprrsRegistryId :: Lens.Lens' DeleteRepositoryPolicyResponse (Core.Maybe Types.RegistryId)
drprrsRegistryId = Lens.field @"registryId"
{-# DEPRECATED drprrsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprrsRepositoryName :: Lens.Lens' DeleteRepositoryPolicyResponse (Core.Maybe Types.RepositoryName)
drprrsRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED drprrsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprrsResponseStatus :: Lens.Lens' DeleteRepositoryPolicyResponse Core.Int
drprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
