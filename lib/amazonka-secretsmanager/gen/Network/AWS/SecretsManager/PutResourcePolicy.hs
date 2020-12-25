{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.PutResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the contents of the specified resource-based permission policy to a secret. A resource-based policy is optional. Alternatively, you can use IAM identity-based policies that specify the secret's Amazon Resource Name (ARN) in the policy statement's @Resources@ element. You can also use a combination of both identity-based and resource-based policies. The affected users and roles receive the permissions that are permitted by all of the relevant policies. For more information, see <http://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access_resource-based-policies.html Using Resource-Based Policies for AWS Secrets Manager> . For the complete description of the AWS policy syntax and grammar, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies.html IAM JSON Policy Reference> in the /IAM User Guide/ .
--
-- __Minimum permissions__
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:PutResourcePolicy
--
--
-- __Related operations__
--
--     * To retrieve the resource policy attached to a secret, use 'GetResourcePolicy' .
--
--
--     * To delete the resource-based policy that's attached to a secret, use 'DeleteResourcePolicy' .
--
--
--     * To list all of the currently available secrets, use 'ListSecrets' .
module Network.AWS.SecretsManager.PutResourcePolicy
  ( -- * Creating a request
    PutResourcePolicy (..),
    mkPutResourcePolicy,

    -- ** Request lenses
    prpSecretId,
    prpResourcePolicy,
    prpBlockPublicPolicy,

    -- * Destructuring the response
    PutResourcePolicyResponse (..),
    mkPutResourcePolicyResponse,

    -- ** Response lenses
    prprrsARN,
    prprrsName,
    prprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SecretsManager.Types as Types

-- | /See:/ 'mkPutResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'
  { -- | Specifies the secret that you want to attach the resource-based policy to. You can specify either the ARN or the friendly name of the secret.
    secretId :: Types.SecretId,
    -- | A JSON-formatted string that's constructed according to the grammar and syntax for an AWS resource-based policy. The policy in the string identifies who can access or manage this secret and its versions. For information on how to format a JSON parameter for the various command line tool environments, see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ .
    resourcePolicy :: Types.NonEmptyResourcePolicyType,
    -- | Makes an optional API call to Zelkova to validate the Resource Policy to prevent broad access to your secret.
    blockPublicPolicy :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutResourcePolicy' value with any optional fields omitted.
mkPutResourcePolicy ::
  -- | 'secretId'
  Types.SecretId ->
  -- | 'resourcePolicy'
  Types.NonEmptyResourcePolicyType ->
  PutResourcePolicy
mkPutResourcePolicy secretId resourcePolicy =
  PutResourcePolicy'
    { secretId,
      resourcePolicy,
      blockPublicPolicy = Core.Nothing
    }

-- | Specifies the secret that you want to attach the resource-based policy to. You can specify either the ARN or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpSecretId :: Lens.Lens' PutResourcePolicy Types.SecretId
prpSecretId = Lens.field @"secretId"
{-# DEPRECATED prpSecretId "Use generic-lens or generic-optics with 'secretId' instead." #-}

-- | A JSON-formatted string that's constructed according to the grammar and syntax for an AWS resource-based policy. The policy in the string identifies who can access or manage this secret and its versions. For information on how to format a JSON parameter for the various command line tool environments, see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ .
--
-- /Note:/ Consider using 'resourcePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpResourcePolicy :: Lens.Lens' PutResourcePolicy Types.NonEmptyResourcePolicyType
prpResourcePolicy = Lens.field @"resourcePolicy"
{-# DEPRECATED prpResourcePolicy "Use generic-lens or generic-optics with 'resourcePolicy' instead." #-}

-- | Makes an optional API call to Zelkova to validate the Resource Policy to prevent broad access to your secret.
--
-- /Note:/ Consider using 'blockPublicPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpBlockPublicPolicy :: Lens.Lens' PutResourcePolicy (Core.Maybe Core.Bool)
prpBlockPublicPolicy = Lens.field @"blockPublicPolicy"
{-# DEPRECATED prpBlockPublicPolicy "Use generic-lens or generic-optics with 'blockPublicPolicy' instead." #-}

instance Core.FromJSON PutResourcePolicy where
  toJSON PutResourcePolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SecretId" Core..= secretId),
            Core.Just ("ResourcePolicy" Core..= resourcePolicy),
            ("BlockPublicPolicy" Core..=) Core.<$> blockPublicPolicy
          ]
      )

instance Core.AWSRequest PutResourcePolicy where
  type Rs PutResourcePolicy = PutResourcePolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "secretsmanager.PutResourcePolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutResourcePolicyResponse'
            Core.<$> (x Core..:? "ARN")
            Core.<*> (x Core..:? "Name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'
  { -- | The ARN of the secret retrieved by the resource-based policy.
    arn :: Core.Maybe Types.SecretARNType,
    -- | The friendly name of the secret that the retrieved by the resource-based policy.
    name :: Core.Maybe Types.NameType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutResourcePolicyResponse' value with any optional fields omitted.
mkPutResourcePolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutResourcePolicyResponse
mkPutResourcePolicyResponse responseStatus =
  PutResourcePolicyResponse'
    { arn = Core.Nothing,
      name = Core.Nothing,
      responseStatus
    }

-- | The ARN of the secret retrieved by the resource-based policy.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prprrsARN :: Lens.Lens' PutResourcePolicyResponse (Core.Maybe Types.SecretARNType)
prprrsARN = Lens.field @"arn"
{-# DEPRECATED prprrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The friendly name of the secret that the retrieved by the resource-based policy.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prprrsName :: Lens.Lens' PutResourcePolicyResponse (Core.Maybe Types.NameType)
prprrsName = Lens.field @"name"
{-# DEPRECATED prprrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prprrsResponseStatus :: Lens.Lens' PutResourcePolicyResponse Core.Int
prprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED prprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
