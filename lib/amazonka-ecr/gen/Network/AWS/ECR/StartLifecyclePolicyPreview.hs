{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    StartLifecyclePolicyPreview (..),
    mkStartLifecyclePolicyPreview,

    -- ** Request lenses
    slppRepositoryName,
    slppLifecyclePolicyText,
    slppRegistryId,

    -- * Destructuring the response
    StartLifecyclePolicyPreviewResponse (..),
    mkStartLifecyclePolicyPreviewResponse,

    -- ** Response lenses
    slpprrsLifecyclePolicyText,
    slpprrsRegistryId,
    slpprrsRepositoryName,
    slpprrsStatus,
    slpprrsResponseStatus,
  )
where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartLifecyclePolicyPreview' smart constructor.
data StartLifecyclePolicyPreview = StartLifecyclePolicyPreview'
  { -- | The name of the repository to be evaluated.
    repositoryName :: Types.RepositoryName,
    -- | The policy to be evaluated against. If you do not specify a policy, the current policy for the repository is used.
    lifecyclePolicyText :: Core.Maybe Types.LifecyclePolicyText,
    -- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
    registryId :: Core.Maybe Types.RegistryId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartLifecyclePolicyPreview' value with any optional fields omitted.
mkStartLifecyclePolicyPreview ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  StartLifecyclePolicyPreview
mkStartLifecyclePolicyPreview repositoryName =
  StartLifecyclePolicyPreview'
    { repositoryName,
      lifecyclePolicyText = Core.Nothing,
      registryId = Core.Nothing
    }

-- | The name of the repository to be evaluated.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slppRepositoryName :: Lens.Lens' StartLifecyclePolicyPreview Types.RepositoryName
slppRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED slppRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The policy to be evaluated against. If you do not specify a policy, the current policy for the repository is used.
--
-- /Note:/ Consider using 'lifecyclePolicyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slppLifecyclePolicyText :: Lens.Lens' StartLifecyclePolicyPreview (Core.Maybe Types.LifecyclePolicyText)
slppLifecyclePolicyText = Lens.field @"lifecyclePolicyText"
{-# DEPRECATED slppLifecyclePolicyText "Use generic-lens or generic-optics with 'lifecyclePolicyText' instead." #-}

-- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slppRegistryId :: Lens.Lens' StartLifecyclePolicyPreview (Core.Maybe Types.RegistryId)
slppRegistryId = Lens.field @"registryId"
{-# DEPRECATED slppRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

instance Core.FromJSON StartLifecyclePolicyPreview where
  toJSON StartLifecyclePolicyPreview {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            ("lifecyclePolicyText" Core..=) Core.<$> lifecyclePolicyText,
            ("registryId" Core..=) Core.<$> registryId
          ]
      )

instance Core.AWSRequest StartLifecyclePolicyPreview where
  type
    Rs StartLifecyclePolicyPreview =
      StartLifecyclePolicyPreviewResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerRegistry_V20150921.StartLifecyclePolicyPreview"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartLifecyclePolicyPreviewResponse'
            Core.<$> (x Core..:? "lifecyclePolicyText")
            Core.<*> (x Core..:? "registryId")
            Core.<*> (x Core..:? "repositoryName")
            Core.<*> (x Core..:? "status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartLifecyclePolicyPreviewResponse' smart constructor.
data StartLifecyclePolicyPreviewResponse = StartLifecyclePolicyPreviewResponse'
  { -- | The JSON repository policy text.
    lifecyclePolicyText :: Core.Maybe Types.LifecyclePolicyText,
    -- | The registry ID associated with the request.
    registryId :: Core.Maybe Types.RegistryId,
    -- | The repository name associated with the request.
    repositoryName :: Core.Maybe Types.RepositoryName,
    -- | The status of the lifecycle policy preview request.
    status :: Core.Maybe Types.LifecyclePolicyPreviewStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartLifecyclePolicyPreviewResponse' value with any optional fields omitted.
mkStartLifecyclePolicyPreviewResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartLifecyclePolicyPreviewResponse
mkStartLifecyclePolicyPreviewResponse responseStatus =
  StartLifecyclePolicyPreviewResponse'
    { lifecyclePolicyText =
        Core.Nothing,
      registryId = Core.Nothing,
      repositoryName = Core.Nothing,
      status = Core.Nothing,
      responseStatus
    }

-- | The JSON repository policy text.
--
-- /Note:/ Consider using 'lifecyclePolicyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slpprrsLifecyclePolicyText :: Lens.Lens' StartLifecyclePolicyPreviewResponse (Core.Maybe Types.LifecyclePolicyText)
slpprrsLifecyclePolicyText = Lens.field @"lifecyclePolicyText"
{-# DEPRECATED slpprrsLifecyclePolicyText "Use generic-lens or generic-optics with 'lifecyclePolicyText' instead." #-}

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slpprrsRegistryId :: Lens.Lens' StartLifecyclePolicyPreviewResponse (Core.Maybe Types.RegistryId)
slpprrsRegistryId = Lens.field @"registryId"
{-# DEPRECATED slpprrsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slpprrsRepositoryName :: Lens.Lens' StartLifecyclePolicyPreviewResponse (Core.Maybe Types.RepositoryName)
slpprrsRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED slpprrsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The status of the lifecycle policy preview request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slpprrsStatus :: Lens.Lens' StartLifecyclePolicyPreviewResponse (Core.Maybe Types.LifecyclePolicyPreviewStatus)
slpprrsStatus = Lens.field @"status"
{-# DEPRECATED slpprrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slpprrsResponseStatus :: Lens.Lens' StartLifecyclePolicyPreviewResponse Core.Int
slpprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED slpprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
