{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    SetRepositoryPolicy (..),
    mkSetRepositoryPolicy,

    -- ** Request lenses
    srpRepositoryName,
    srpPolicyText,
    srpForce,
    srpRegistryId,

    -- * Destructuring the response
    SetRepositoryPolicyResponse (..),
    mkSetRepositoryPolicyResponse,

    -- ** Response lenses
    srprrsPolicyText,
    srprrsRegistryId,
    srprrsRepositoryName,
    srprrsResponseStatus,
  )
where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSetRepositoryPolicy' smart constructor.
data SetRepositoryPolicy = SetRepositoryPolicy'
  { -- | The name of the repository to receive the policy.
    repositoryName :: Types.RepositoryName,
    -- | The JSON repository policy text to apply to the repository. For more information, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/repository-policy-examples.html Amazon ECR Repository Policies> in the /Amazon Elastic Container Registry User Guide/ .
    policyText :: Types.PolicyText,
    -- | If the policy you are attempting to set on a repository policy would prevent you from setting another policy in the future, you must force the 'SetRepositoryPolicy' operation. This is intended to prevent accidental repository lock outs.
    force :: Core.Maybe Core.Bool,
    -- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
    registryId :: Core.Maybe Types.RegistryId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetRepositoryPolicy' value with any optional fields omitted.
mkSetRepositoryPolicy ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  -- | 'policyText'
  Types.PolicyText ->
  SetRepositoryPolicy
mkSetRepositoryPolicy repositoryName policyText =
  SetRepositoryPolicy'
    { repositoryName,
      policyText,
      force = Core.Nothing,
      registryId = Core.Nothing
    }

-- | The name of the repository to receive the policy.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpRepositoryName :: Lens.Lens' SetRepositoryPolicy Types.RepositoryName
srpRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED srpRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The JSON repository policy text to apply to the repository. For more information, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/repository-policy-examples.html Amazon ECR Repository Policies> in the /Amazon Elastic Container Registry User Guide/ .
--
-- /Note:/ Consider using 'policyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpPolicyText :: Lens.Lens' SetRepositoryPolicy Types.PolicyText
srpPolicyText = Lens.field @"policyText"
{-# DEPRECATED srpPolicyText "Use generic-lens or generic-optics with 'policyText' instead." #-}

-- | If the policy you are attempting to set on a repository policy would prevent you from setting another policy in the future, you must force the 'SetRepositoryPolicy' operation. This is intended to prevent accidental repository lock outs.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpForce :: Lens.Lens' SetRepositoryPolicy (Core.Maybe Core.Bool)
srpForce = Lens.field @"force"
{-# DEPRECATED srpForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpRegistryId :: Lens.Lens' SetRepositoryPolicy (Core.Maybe Types.RegistryId)
srpRegistryId = Lens.field @"registryId"
{-# DEPRECATED srpRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

instance Core.FromJSON SetRepositoryPolicy where
  toJSON SetRepositoryPolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("policyText" Core..= policyText),
            ("force" Core..=) Core.<$> force,
            ("registryId" Core..=) Core.<$> registryId
          ]
      )

instance Core.AWSRequest SetRepositoryPolicy where
  type Rs SetRepositoryPolicy = SetRepositoryPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerRegistry_V20150921.SetRepositoryPolicy"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          SetRepositoryPolicyResponse'
            Core.<$> (x Core..:? "policyText")
            Core.<*> (x Core..:? "registryId")
            Core.<*> (x Core..:? "repositoryName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkSetRepositoryPolicyResponse' smart constructor.
data SetRepositoryPolicyResponse = SetRepositoryPolicyResponse'
  { -- | The JSON repository policy text applied to the repository.
    policyText :: Core.Maybe Types.RepositoryPolicyText,
    -- | The registry ID associated with the request.
    registryId :: Core.Maybe Types.RegistryId,
    -- | The repository name associated with the request.
    repositoryName :: Core.Maybe Types.RepositoryName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetRepositoryPolicyResponse' value with any optional fields omitted.
mkSetRepositoryPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SetRepositoryPolicyResponse
mkSetRepositoryPolicyResponse responseStatus =
  SetRepositoryPolicyResponse'
    { policyText = Core.Nothing,
      registryId = Core.Nothing,
      repositoryName = Core.Nothing,
      responseStatus
    }

-- | The JSON repository policy text applied to the repository.
--
-- /Note:/ Consider using 'policyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srprrsPolicyText :: Lens.Lens' SetRepositoryPolicyResponse (Core.Maybe Types.RepositoryPolicyText)
srprrsPolicyText = Lens.field @"policyText"
{-# DEPRECATED srprrsPolicyText "Use generic-lens or generic-optics with 'policyText' instead." #-}

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srprrsRegistryId :: Lens.Lens' SetRepositoryPolicyResponse (Core.Maybe Types.RegistryId)
srprrsRegistryId = Lens.field @"registryId"
{-# DEPRECATED srprrsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srprrsRepositoryName :: Lens.Lens' SetRepositoryPolicyResponse (Core.Maybe Types.RepositoryName)
srprrsRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED srprrsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srprrsResponseStatus :: Lens.Lens' SetRepositoryPolicyResponse Core.Int
srprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED srprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
