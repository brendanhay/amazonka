{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.GetResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the JSON text of the resource-based policy document attached to the specified secret. The JSON request string input and response output displays formatted code with white space and line breaks for better readability. Submit your input as a single line JSON string.
--
-- __Minimum permissions__
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:GetResourcePolicy
--
--
-- __Related operations__
--
--     * To attach a resource policy to a secret, use 'PutResourcePolicy' .
--
--
--     * To delete the resource-based policy attached to a secret, use 'DeleteResourcePolicy' .
--
--
--     * To list all of the currently available secrets, use 'ListSecrets' .
module Network.AWS.SecretsManager.GetResourcePolicy
  ( -- * Creating a request
    GetResourcePolicy (..),
    mkGetResourcePolicy,

    -- ** Request lenses
    grpSecretId,

    -- * Destructuring the response
    GetResourcePolicyResponse (..),
    mkGetResourcePolicyResponse,

    -- ** Response lenses
    grprrsARN,
    grprrsName,
    grprrsResourcePolicy,
    grprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SecretsManager.Types as Types

-- | /See:/ 'mkGetResourcePolicy' smart constructor.
newtype GetResourcePolicy = GetResourcePolicy'
  { -- | Specifies the secret that you want to retrieve the attached resource-based policy for. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
    secretId :: Types.SecretId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetResourcePolicy' value with any optional fields omitted.
mkGetResourcePolicy ::
  -- | 'secretId'
  Types.SecretId ->
  GetResourcePolicy
mkGetResourcePolicy secretId = GetResourcePolicy' {secretId}

-- | Specifies the secret that you want to retrieve the attached resource-based policy for. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpSecretId :: Lens.Lens' GetResourcePolicy Types.SecretId
grpSecretId = Lens.field @"secretId"
{-# DEPRECATED grpSecretId "Use generic-lens or generic-optics with 'secretId' instead." #-}

instance Core.FromJSON GetResourcePolicy where
  toJSON GetResourcePolicy {..} =
    Core.object
      (Core.catMaybes [Core.Just ("SecretId" Core..= secretId)])

instance Core.AWSRequest GetResourcePolicy where
  type Rs GetResourcePolicy = GetResourcePolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "secretsmanager.GetResourcePolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourcePolicyResponse'
            Core.<$> (x Core..:? "ARN")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "ResourcePolicy")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetResourcePolicyResponse' smart constructor.
data GetResourcePolicyResponse = GetResourcePolicyResponse'
  { -- | The ARN of the secret that the resource-based policy was retrieved for.
    arn :: Core.Maybe Types.SecretARNType,
    -- | The friendly name of the secret that the resource-based policy was retrieved for.
    name :: Core.Maybe Types.NameType,
    -- | A JSON-formatted string that describes the permissions that are associated with the attached secret. These permissions are combined with any permissions that are associated with the user or role that attempts to access this secret. The combined permissions specify who can access the secret and what actions they can perform. For more information, see <http://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and Access Control for AWS Secrets Manager> in the /AWS Secrets Manager User Guide/ .
    resourcePolicy :: Core.Maybe Types.NonEmptyResourcePolicyType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetResourcePolicyResponse' value with any optional fields omitted.
mkGetResourcePolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetResourcePolicyResponse
mkGetResourcePolicyResponse responseStatus =
  GetResourcePolicyResponse'
    { arn = Core.Nothing,
      name = Core.Nothing,
      resourcePolicy = Core.Nothing,
      responseStatus
    }

-- | The ARN of the secret that the resource-based policy was retrieved for.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsARN :: Lens.Lens' GetResourcePolicyResponse (Core.Maybe Types.SecretARNType)
grprrsARN = Lens.field @"arn"
{-# DEPRECATED grprrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The friendly name of the secret that the resource-based policy was retrieved for.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsName :: Lens.Lens' GetResourcePolicyResponse (Core.Maybe Types.NameType)
grprrsName = Lens.field @"name"
{-# DEPRECATED grprrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A JSON-formatted string that describes the permissions that are associated with the attached secret. These permissions are combined with any permissions that are associated with the user or role that attempts to access this secret. The combined permissions specify who can access the secret and what actions they can perform. For more information, see <http://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and Access Control for AWS Secrets Manager> in the /AWS Secrets Manager User Guide/ .
--
-- /Note:/ Consider using 'resourcePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsResourcePolicy :: Lens.Lens' GetResourcePolicyResponse (Core.Maybe Types.NonEmptyResourcePolicyType)
grprrsResourcePolicy = Lens.field @"resourcePolicy"
{-# DEPRECATED grprrsResourcePolicy "Use generic-lens or generic-optics with 'resourcePolicy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsResponseStatus :: Lens.Lens' GetResourcePolicyResponse Core.Int
grprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
