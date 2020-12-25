{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.RotateSecret
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures and starts the asynchronous process of rotating this secret. If you include the configuration parameters, the operation sets those values for the secret and then immediately starts a rotation. If you do not include the configuration parameters, the operation starts a rotation with the values already stored in the secret. After the rotation completes, the protected service and its clients all use the new version of the secret.
--
-- This required configuration information includes the ARN of an AWS Lambda function and the time between scheduled rotations. The Lambda rotation function creates a new version of the secret and creates or updates the credentials on the protected service to match. After testing the new credentials, the function marks the new secret with the staging label @AWSCURRENT@ so that your clients all immediately begin to use the new version. For more information about rotating secrets and how to configure a Lambda function to rotate the secrets for your protected service, see <https://docs.aws.amazon.com/secretsmanager/latest/userguide/rotating-secrets.html Rotating Secrets in AWS Secrets Manager> in the /AWS Secrets Manager User Guide/ .
-- Secrets Manager schedules the next rotation when the previous one completes. Secrets Manager schedules the date by adding the rotation interval (number of days) to the actual date of the last rotation. The service chooses the hour within that 24-hour date window randomly. The minute is also chosen somewhat randomly, but weighted towards the top of the hour and influenced by a variety of factors that help distribute load.
-- The rotation function must end with the versions of the secret in one of two states:
--
--     * The @AWSPENDING@ and @AWSCURRENT@ staging labels are attached to the same version of the secret, or
--
--
--     * The @AWSPENDING@ staging label is not attached to any version of the secret.
--
--
-- If the @AWSPENDING@ staging label is present but not attached to the same version as @AWSCURRENT@ then any later invocation of @RotateSecret@ assumes that a previous rotation request is still in progress and returns an error.
-- __Minimum permissions__
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:RotateSecret
--
--
--     * lambda:InvokeFunction (on the function specified in the secret's metadata)
--
--
-- __Related operations__
--
--     * To list the secrets in your account, use 'ListSecrets' .
--
--
--     * To get the details for a version of a secret, use 'DescribeSecret' .
--
--
--     * To create a new version of a secret, use 'CreateSecret' .
--
--
--     * To attach staging labels to or remove staging labels from a version of a secret, use 'UpdateSecretVersionStage' .
module Network.AWS.SecretsManager.RotateSecret
  ( -- * Creating a request
    RotateSecret (..),
    mkRotateSecret,

    -- ** Request lenses
    rsSecretId,
    rsClientRequestToken,
    rsRotationLambdaARN,
    rsRotationRules,

    -- * Destructuring the response
    RotateSecretResponse (..),
    mkRotateSecretResponse,

    -- ** Response lenses
    rsrrsARN,
    rsrrsName,
    rsrrsVersionId,
    rsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SecretsManager.Types as Types

-- | /See:/ 'mkRotateSecret' smart constructor.
data RotateSecret = RotateSecret'
  { -- | Specifies the secret that you want to rotate. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
    secretId :: Types.SecretIdType,
    -- | (Optional) Specifies a unique identifier for the new version of the secret that helps ensure idempotency.
    --
    -- If you use the AWS CLI or one of the AWS SDK to call this operation, then you can leave this parameter empty. The CLI or SDK generates a random UUID for you and includes that in the request for this parameter. If you don't use the SDK and instead generate a raw HTTP request to the Secrets Manager service endpoint, then you must generate a @ClientRequestToken@ yourself for new versions and include that value in the request.
    -- You only need to specify your own value if you implement your own retry logic and want to ensure that a given secret is not created twice. We recommend that you generate a <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type> value to ensure uniqueness within the specified secret.
    -- Secrets Manager uses this value to prevent the accidental creation of duplicate versions if there are failures and retries during the function's processing. This value becomes the @VersionId@ of the new version.
    clientRequestToken :: Core.Maybe Types.ClientRequestTokenType,
    -- | (Optional) Specifies the ARN of the Lambda function that can rotate the secret.
    rotationLambdaARN :: Core.Maybe Types.RotationLambdaARNType,
    -- | A structure that defines the rotation configuration for this secret.
    rotationRules :: Core.Maybe Types.RotationRulesType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RotateSecret' value with any optional fields omitted.
mkRotateSecret ::
  -- | 'secretId'
  Types.SecretIdType ->
  RotateSecret
mkRotateSecret secretId =
  RotateSecret'
    { secretId,
      clientRequestToken = Core.Nothing,
      rotationLambdaARN = Core.Nothing,
      rotationRules = Core.Nothing
    }

-- | Specifies the secret that you want to rotate. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsSecretId :: Lens.Lens' RotateSecret Types.SecretIdType
rsSecretId = Lens.field @"secretId"
{-# DEPRECATED rsSecretId "Use generic-lens or generic-optics with 'secretId' instead." #-}

-- | (Optional) Specifies a unique identifier for the new version of the secret that helps ensure idempotency.
--
-- If you use the AWS CLI or one of the AWS SDK to call this operation, then you can leave this parameter empty. The CLI or SDK generates a random UUID for you and includes that in the request for this parameter. If you don't use the SDK and instead generate a raw HTTP request to the Secrets Manager service endpoint, then you must generate a @ClientRequestToken@ yourself for new versions and include that value in the request.
-- You only need to specify your own value if you implement your own retry logic and want to ensure that a given secret is not created twice. We recommend that you generate a <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type> value to ensure uniqueness within the specified secret.
-- Secrets Manager uses this value to prevent the accidental creation of duplicate versions if there are failures and retries during the function's processing. This value becomes the @VersionId@ of the new version.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsClientRequestToken :: Lens.Lens' RotateSecret (Core.Maybe Types.ClientRequestTokenType)
rsClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED rsClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | (Optional) Specifies the ARN of the Lambda function that can rotate the secret.
--
-- /Note:/ Consider using 'rotationLambdaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsRotationLambdaARN :: Lens.Lens' RotateSecret (Core.Maybe Types.RotationLambdaARNType)
rsRotationLambdaARN = Lens.field @"rotationLambdaARN"
{-# DEPRECATED rsRotationLambdaARN "Use generic-lens or generic-optics with 'rotationLambdaARN' instead." #-}

-- | A structure that defines the rotation configuration for this secret.
--
-- /Note:/ Consider using 'rotationRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsRotationRules :: Lens.Lens' RotateSecret (Core.Maybe Types.RotationRulesType)
rsRotationRules = Lens.field @"rotationRules"
{-# DEPRECATED rsRotationRules "Use generic-lens or generic-optics with 'rotationRules' instead." #-}

instance Core.FromJSON RotateSecret where
  toJSON RotateSecret {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SecretId" Core..= secretId),
            ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
            ("RotationLambdaARN" Core..=) Core.<$> rotationLambdaARN,
            ("RotationRules" Core..=) Core.<$> rotationRules
          ]
      )

instance Core.AWSRequest RotateSecret where
  type Rs RotateSecret = RotateSecretResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "secretsmanager.RotateSecret")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RotateSecretResponse'
            Core.<$> (x Core..:? "ARN")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "VersionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRotateSecretResponse' smart constructor.
data RotateSecretResponse = RotateSecretResponse'
  { -- | The ARN of the secret.
    arn :: Core.Maybe Types.ARN,
    -- | The friendly name of the secret.
    name :: Core.Maybe Types.Name,
    -- | The ID of the new version of the secret created by the rotation started by this request.
    versionId :: Core.Maybe Types.VersionId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RotateSecretResponse' value with any optional fields omitted.
mkRotateSecretResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RotateSecretResponse
mkRotateSecretResponse responseStatus =
  RotateSecretResponse'
    { arn = Core.Nothing,
      name = Core.Nothing,
      versionId = Core.Nothing,
      responseStatus
    }

-- | The ARN of the secret.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrrsARN :: Lens.Lens' RotateSecretResponse (Core.Maybe Types.ARN)
rsrrsARN = Lens.field @"arn"
{-# DEPRECATED rsrrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The friendly name of the secret.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrrsName :: Lens.Lens' RotateSecretResponse (Core.Maybe Types.Name)
rsrrsName = Lens.field @"name"
{-# DEPRECATED rsrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the new version of the secret created by the rotation started by this request.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrrsVersionId :: Lens.Lens' RotateSecretResponse (Core.Maybe Types.VersionId)
rsrrsVersionId = Lens.field @"versionId"
{-# DEPRECATED rsrrsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrrsResponseStatus :: Lens.Lens' RotateSecretResponse Core.Int
rsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
