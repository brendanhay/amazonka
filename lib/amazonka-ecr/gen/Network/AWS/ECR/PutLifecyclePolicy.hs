{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    PutLifecyclePolicy (..),
    mkPutLifecyclePolicy,

    -- ** Request lenses
    plpRepositoryName,
    plpLifecyclePolicyText,
    plpRegistryId,

    -- * Destructuring the response
    PutLifecyclePolicyResponse (..),
    mkPutLifecyclePolicyResponse,

    -- ** Response lenses
    plprrsLifecyclePolicyText,
    plprrsRegistryId,
    plprrsRepositoryName,
    plprrsResponseStatus,
  )
where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutLifecyclePolicy' smart constructor.
data PutLifecyclePolicy = PutLifecyclePolicy'
  { -- | The name of the repository to receive the policy.
    repositoryName :: Types.RepositoryName,
    -- | The JSON repository policy text to apply to the repository.
    lifecyclePolicyText :: Types.LifecyclePolicyText,
    -- | The AWS account ID associated with the registry that contains the repository. If you do  not specify a registry, the default registry is assumed.
    registryId :: Core.Maybe Types.RegistryId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutLifecyclePolicy' value with any optional fields omitted.
mkPutLifecyclePolicy ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  -- | 'lifecyclePolicyText'
  Types.LifecyclePolicyText ->
  PutLifecyclePolicy
mkPutLifecyclePolicy repositoryName lifecyclePolicyText =
  PutLifecyclePolicy'
    { repositoryName,
      lifecyclePolicyText,
      registryId = Core.Nothing
    }

-- | The name of the repository to receive the policy.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plpRepositoryName :: Lens.Lens' PutLifecyclePolicy Types.RepositoryName
plpRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED plpRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The JSON repository policy text to apply to the repository.
--
-- /Note:/ Consider using 'lifecyclePolicyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plpLifecyclePolicyText :: Lens.Lens' PutLifecyclePolicy Types.LifecyclePolicyText
plpLifecyclePolicyText = Lens.field @"lifecyclePolicyText"
{-# DEPRECATED plpLifecyclePolicyText "Use generic-lens or generic-optics with 'lifecyclePolicyText' instead." #-}

-- | The AWS account ID associated with the registry that contains the repository. If you do  not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plpRegistryId :: Lens.Lens' PutLifecyclePolicy (Core.Maybe Types.RegistryId)
plpRegistryId = Lens.field @"registryId"
{-# DEPRECATED plpRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

instance Core.FromJSON PutLifecyclePolicy where
  toJSON PutLifecyclePolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("lifecyclePolicyText" Core..= lifecyclePolicyText),
            ("registryId" Core..=) Core.<$> registryId
          ]
      )

instance Core.AWSRequest PutLifecyclePolicy where
  type Rs PutLifecyclePolicy = PutLifecyclePolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerRegistry_V20150921.PutLifecyclePolicy"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutLifecyclePolicyResponse'
            Core.<$> (x Core..:? "lifecyclePolicyText")
            Core.<*> (x Core..:? "registryId")
            Core.<*> (x Core..:? "repositoryName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutLifecyclePolicyResponse' smart constructor.
data PutLifecyclePolicyResponse = PutLifecyclePolicyResponse'
  { -- | The JSON repository policy text.
    lifecyclePolicyText :: Core.Maybe Types.LifecyclePolicyText,
    -- | The registry ID associated with the request.
    registryId :: Core.Maybe Types.RegistryId,
    -- | The repository name associated with the request.
    repositoryName :: Core.Maybe Types.RepositoryName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutLifecyclePolicyResponse' value with any optional fields omitted.
mkPutLifecyclePolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutLifecyclePolicyResponse
mkPutLifecyclePolicyResponse responseStatus =
  PutLifecyclePolicyResponse'
    { lifecyclePolicyText = Core.Nothing,
      registryId = Core.Nothing,
      repositoryName = Core.Nothing,
      responseStatus
    }

-- | The JSON repository policy text.
--
-- /Note:/ Consider using 'lifecyclePolicyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plprrsLifecyclePolicyText :: Lens.Lens' PutLifecyclePolicyResponse (Core.Maybe Types.LifecyclePolicyText)
plprrsLifecyclePolicyText = Lens.field @"lifecyclePolicyText"
{-# DEPRECATED plprrsLifecyclePolicyText "Use generic-lens or generic-optics with 'lifecyclePolicyText' instead." #-}

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plprrsRegistryId :: Lens.Lens' PutLifecyclePolicyResponse (Core.Maybe Types.RegistryId)
plprrsRegistryId = Lens.field @"registryId"
{-# DEPRECATED plprrsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plprrsRepositoryName :: Lens.Lens' PutLifecyclePolicyResponse (Core.Maybe Types.RepositoryName)
plprrsRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED plprrsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plprrsResponseStatus :: Lens.Lens' PutLifecyclePolicyResponse Core.Int
plprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED plprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
