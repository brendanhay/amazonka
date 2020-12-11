{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    rsRotationRules,
    rsClientRequestToken,
    rsRotationLambdaARN,
    rsSecretId,

    -- * Destructuring the response
    RotateSecretResponse (..),
    mkRotateSecretResponse,

    -- ** Response lenses
    rsrsVersionId,
    rsrsARN,
    rsrsName,
    rsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SecretsManager.Types

-- | /See:/ 'mkRotateSecret' smart constructor.
data RotateSecret = RotateSecret'
  { rotationRules ::
      Lude.Maybe RotationRulesType,
    clientRequestToken :: Lude.Maybe Lude.Text,
    rotationLambdaARN :: Lude.Maybe Lude.Text,
    secretId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RotateSecret' with the minimum fields required to make a request.
--
-- * 'clientRequestToken' - (Optional) Specifies a unique identifier for the new version of the secret that helps ensure idempotency.
--
-- If you use the AWS CLI or one of the AWS SDK to call this operation, then you can leave this parameter empty. The CLI or SDK generates a random UUID for you and includes that in the request for this parameter. If you don't use the SDK and instead generate a raw HTTP request to the Secrets Manager service endpoint, then you must generate a @ClientRequestToken@ yourself for new versions and include that value in the request.
-- You only need to specify your own value if you implement your own retry logic and want to ensure that a given secret is not created twice. We recommend that you generate a <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type> value to ensure uniqueness within the specified secret.
-- Secrets Manager uses this value to prevent the accidental creation of duplicate versions if there are failures and retries during the function's processing. This value becomes the @VersionId@ of the new version.
-- * 'rotationLambdaARN' - (Optional) Specifies the ARN of the Lambda function that can rotate the secret.
-- * 'rotationRules' - A structure that defines the rotation configuration for this secret.
-- * 'secretId' - Specifies the secret that you want to rotate. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
mkRotateSecret ::
  -- | 'secretId'
  Lude.Text ->
  RotateSecret
mkRotateSecret pSecretId_ =
  RotateSecret'
    { rotationRules = Lude.Nothing,
      clientRequestToken = Lude.Nothing,
      rotationLambdaARN = Lude.Nothing,
      secretId = pSecretId_
    }

-- | A structure that defines the rotation configuration for this secret.
--
-- /Note:/ Consider using 'rotationRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsRotationRules :: Lens.Lens' RotateSecret (Lude.Maybe RotationRulesType)
rsRotationRules = Lens.lens (rotationRules :: RotateSecret -> Lude.Maybe RotationRulesType) (\s a -> s {rotationRules = a} :: RotateSecret)
{-# DEPRECATED rsRotationRules "Use generic-lens or generic-optics with 'rotationRules' instead." #-}

-- | (Optional) Specifies a unique identifier for the new version of the secret that helps ensure idempotency.
--
-- If you use the AWS CLI or one of the AWS SDK to call this operation, then you can leave this parameter empty. The CLI or SDK generates a random UUID for you and includes that in the request for this parameter. If you don't use the SDK and instead generate a raw HTTP request to the Secrets Manager service endpoint, then you must generate a @ClientRequestToken@ yourself for new versions and include that value in the request.
-- You only need to specify your own value if you implement your own retry logic and want to ensure that a given secret is not created twice. We recommend that you generate a <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type> value to ensure uniqueness within the specified secret.
-- Secrets Manager uses this value to prevent the accidental creation of duplicate versions if there are failures and retries during the function's processing. This value becomes the @VersionId@ of the new version.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsClientRequestToken :: Lens.Lens' RotateSecret (Lude.Maybe Lude.Text)
rsClientRequestToken = Lens.lens (clientRequestToken :: RotateSecret -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: RotateSecret)
{-# DEPRECATED rsClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | (Optional) Specifies the ARN of the Lambda function that can rotate the secret.
--
-- /Note:/ Consider using 'rotationLambdaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsRotationLambdaARN :: Lens.Lens' RotateSecret (Lude.Maybe Lude.Text)
rsRotationLambdaARN = Lens.lens (rotationLambdaARN :: RotateSecret -> Lude.Maybe Lude.Text) (\s a -> s {rotationLambdaARN = a} :: RotateSecret)
{-# DEPRECATED rsRotationLambdaARN "Use generic-lens or generic-optics with 'rotationLambdaARN' instead." #-}

-- | Specifies the secret that you want to rotate. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsSecretId :: Lens.Lens' RotateSecret Lude.Text
rsSecretId = Lens.lens (secretId :: RotateSecret -> Lude.Text) (\s a -> s {secretId = a} :: RotateSecret)
{-# DEPRECATED rsSecretId "Use generic-lens or generic-optics with 'secretId' instead." #-}

instance Lude.AWSRequest RotateSecret where
  type Rs RotateSecret = RotateSecretResponse
  request = Req.postJSON secretsManagerService
  response =
    Res.receiveJSON
      ( \s h x ->
          RotateSecretResponse'
            Lude.<$> (x Lude..?> "VersionId")
            Lude.<*> (x Lude..?> "ARN")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RotateSecret where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("secretsmanager.RotateSecret" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RotateSecret where
  toJSON RotateSecret' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RotationRules" Lude..=) Lude.<$> rotationRules,
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            ("RotationLambdaARN" Lude..=) Lude.<$> rotationLambdaARN,
            Lude.Just ("SecretId" Lude..= secretId)
          ]
      )

instance Lude.ToPath RotateSecret where
  toPath = Lude.const "/"

instance Lude.ToQuery RotateSecret where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRotateSecretResponse' smart constructor.
data RotateSecretResponse = RotateSecretResponse'
  { versionId ::
      Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RotateSecretResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the secret.
-- * 'name' - The friendly name of the secret.
-- * 'responseStatus' - The response status code.
-- * 'versionId' - The ID of the new version of the secret created by the rotation started by this request.
mkRotateSecretResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RotateSecretResponse
mkRotateSecretResponse pResponseStatus_ =
  RotateSecretResponse'
    { versionId = Lude.Nothing,
      arn = Lude.Nothing,
      name = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the new version of the secret created by the rotation started by this request.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrsVersionId :: Lens.Lens' RotateSecretResponse (Lude.Maybe Lude.Text)
rsrsVersionId = Lens.lens (versionId :: RotateSecretResponse -> Lude.Maybe Lude.Text) (\s a -> s {versionId = a} :: RotateSecretResponse)
{-# DEPRECATED rsrsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The ARN of the secret.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrsARN :: Lens.Lens' RotateSecretResponse (Lude.Maybe Lude.Text)
rsrsARN = Lens.lens (arn :: RotateSecretResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: RotateSecretResponse)
{-# DEPRECATED rsrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The friendly name of the secret.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrsName :: Lens.Lens' RotateSecretResponse (Lude.Maybe Lude.Text)
rsrsName = Lens.lens (name :: RotateSecretResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: RotateSecretResponse)
{-# DEPRECATED rsrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrsResponseStatus :: Lens.Lens' RotateSecretResponse Lude.Int
rsrsResponseStatus = Lens.lens (responseStatus :: RotateSecretResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RotateSecretResponse)
{-# DEPRECATED rsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
