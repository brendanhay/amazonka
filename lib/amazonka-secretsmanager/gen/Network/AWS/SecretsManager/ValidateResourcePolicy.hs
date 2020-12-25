{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.ValidateResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates the JSON text of the resource-based policy document attached to the specified secret. The JSON request string input and response output displays formatted code with white space and line breaks for better readability. Submit your input as a single line JSON string. A resource-based policy is optional.
module Network.AWS.SecretsManager.ValidateResourcePolicy
  ( -- * Creating a request
    ValidateResourcePolicy (..),
    mkValidateResourcePolicy,

    -- ** Request lenses
    vrpResourcePolicy,
    vrpSecretId,

    -- * Destructuring the response
    ValidateResourcePolicyResponse (..),
    mkValidateResourcePolicyResponse,

    -- ** Response lenses
    vrprrsPolicyValidationPassed,
    vrprrsValidationErrors,
    vrprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SecretsManager.Types as Types

-- | /See:/ 'mkValidateResourcePolicy' smart constructor.
data ValidateResourcePolicy = ValidateResourcePolicy'
  { -- | Identifies the Resource Policy attached to the secret.
    resourcePolicy :: Types.NonEmptyResourcePolicyType,
    -- | The identifier for the secret that you want to validate a resource policy. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
    secretId :: Core.Maybe Types.SecretId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ValidateResourcePolicy' value with any optional fields omitted.
mkValidateResourcePolicy ::
  -- | 'resourcePolicy'
  Types.NonEmptyResourcePolicyType ->
  ValidateResourcePolicy
mkValidateResourcePolicy resourcePolicy =
  ValidateResourcePolicy' {resourcePolicy, secretId = Core.Nothing}

-- | Identifies the Resource Policy attached to the secret.
--
-- /Note:/ Consider using 'resourcePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vrpResourcePolicy :: Lens.Lens' ValidateResourcePolicy Types.NonEmptyResourcePolicyType
vrpResourcePolicy = Lens.field @"resourcePolicy"
{-# DEPRECATED vrpResourcePolicy "Use generic-lens or generic-optics with 'resourcePolicy' instead." #-}

-- | The identifier for the secret that you want to validate a resource policy. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vrpSecretId :: Lens.Lens' ValidateResourcePolicy (Core.Maybe Types.SecretId)
vrpSecretId = Lens.field @"secretId"
{-# DEPRECATED vrpSecretId "Use generic-lens or generic-optics with 'secretId' instead." #-}

instance Core.FromJSON ValidateResourcePolicy where
  toJSON ValidateResourcePolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourcePolicy" Core..= resourcePolicy),
            ("SecretId" Core..=) Core.<$> secretId
          ]
      )

instance Core.AWSRequest ValidateResourcePolicy where
  type Rs ValidateResourcePolicy = ValidateResourcePolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "secretsmanager.ValidateResourcePolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ValidateResourcePolicyResponse'
            Core.<$> (x Core..:? "PolicyValidationPassed")
            Core.<*> (x Core..:? "ValidationErrors")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkValidateResourcePolicyResponse' smart constructor.
data ValidateResourcePolicyResponse = ValidateResourcePolicyResponse'
  { -- | Returns a message stating that your Reource Policy passed validation.
    policyValidationPassed :: Core.Maybe Core.Bool,
    -- | Returns an error message if your policy doesn't pass validatation.
    validationErrors :: Core.Maybe [Types.ValidationErrorsEntry],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ValidateResourcePolicyResponse' value with any optional fields omitted.
mkValidateResourcePolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ValidateResourcePolicyResponse
mkValidateResourcePolicyResponse responseStatus =
  ValidateResourcePolicyResponse'
    { policyValidationPassed =
        Core.Nothing,
      validationErrors = Core.Nothing,
      responseStatus
    }

-- | Returns a message stating that your Reource Policy passed validation.
--
-- /Note:/ Consider using 'policyValidationPassed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vrprrsPolicyValidationPassed :: Lens.Lens' ValidateResourcePolicyResponse (Core.Maybe Core.Bool)
vrprrsPolicyValidationPassed = Lens.field @"policyValidationPassed"
{-# DEPRECATED vrprrsPolicyValidationPassed "Use generic-lens or generic-optics with 'policyValidationPassed' instead." #-}

-- | Returns an error message if your policy doesn't pass validatation.
--
-- /Note:/ Consider using 'validationErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vrprrsValidationErrors :: Lens.Lens' ValidateResourcePolicyResponse (Core.Maybe [Types.ValidationErrorsEntry])
vrprrsValidationErrors = Lens.field @"validationErrors"
{-# DEPRECATED vrprrsValidationErrors "Use generic-lens or generic-optics with 'validationErrors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vrprrsResponseStatus :: Lens.Lens' ValidateResourcePolicyResponse Core.Int
vrprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED vrprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
