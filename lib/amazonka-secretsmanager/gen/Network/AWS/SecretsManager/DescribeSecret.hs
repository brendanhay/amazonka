{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.DescribeSecret
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of a secret. It does not include the encrypted fields. Secrets Manager only returns fields populated with a value in the response.
--
-- __Minimum permissions__
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:DescribeSecret
--
--
-- __Related operations__
--
--     * To create a secret, use 'CreateSecret' .
--
--
--     * To modify a secret, use 'UpdateSecret' .
--
--
--     * To retrieve the encrypted secret information in a version of the secret, use 'GetSecretValue' .
--
--
--     * To list all of the secrets in the AWS account, use 'ListSecrets' .
module Network.AWS.SecretsManager.DescribeSecret
  ( -- * Creating a request
    DescribeSecret (..),
    mkDescribeSecret,

    -- ** Request lenses
    dSecretId,

    -- * Destructuring the response
    DescribeSecretResponse (..),
    mkDescribeSecretResponse,

    -- ** Response lenses
    drsLastChangedDate,
    drsARN,
    drsRotationRules,
    drsDeletedDate,
    drsRotationEnabled,
    drsCreatedDate,
    drsKMSKeyId,
    drsName,
    drsVersionIdsToStages,
    drsOwningService,
    drsLastRotatedDate,
    drsLastAccessedDate,
    drsDescription,
    drsRotationLambdaARN,
    drsTags,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SecretsManager.Types

-- | /See:/ 'mkDescribeSecret' smart constructor.
newtype DescribeSecret = DescribeSecret' {secretId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSecret' with the minimum fields required to make a request.
--
-- * 'secretId' - The identifier of the secret whose details you want to retrieve. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
mkDescribeSecret ::
  -- | 'secretId'
  Lude.Text ->
  DescribeSecret
mkDescribeSecret pSecretId_ =
  DescribeSecret' {secretId = pSecretId_}

-- | The identifier of the secret whose details you want to retrieve. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSecretId :: Lens.Lens' DescribeSecret Lude.Text
dSecretId = Lens.lens (secretId :: DescribeSecret -> Lude.Text) (\s a -> s {secretId = a} :: DescribeSecret)
{-# DEPRECATED dSecretId "Use generic-lens or generic-optics with 'secretId' instead." #-}

instance Lude.AWSRequest DescribeSecret where
  type Rs DescribeSecret = DescribeSecretResponse
  request = Req.postJSON secretsManagerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeSecretResponse'
            Lude.<$> (x Lude..?> "LastChangedDate")
            Lude.<*> (x Lude..?> "ARN")
            Lude.<*> (x Lude..?> "RotationRules")
            Lude.<*> (x Lude..?> "DeletedDate")
            Lude.<*> (x Lude..?> "RotationEnabled")
            Lude.<*> (x Lude..?> "CreatedDate")
            Lude.<*> (x Lude..?> "KmsKeyId")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "VersionIdsToStages" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "OwningService")
            Lude.<*> (x Lude..?> "LastRotatedDate")
            Lude.<*> (x Lude..?> "LastAccessedDate")
            Lude.<*> (x Lude..?> "Description")
            Lude.<*> (x Lude..?> "RotationLambdaARN")
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSecret where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("secretsmanager.DescribeSecret" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeSecret where
  toJSON DescribeSecret' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("SecretId" Lude..= secretId)])

instance Lude.ToPath DescribeSecret where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSecret where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeSecretResponse' smart constructor.
data DescribeSecretResponse = DescribeSecretResponse'
  { lastChangedDate ::
      Lude.Maybe Lude.Timestamp,
    arn :: Lude.Maybe Lude.Text,
    rotationRules :: Lude.Maybe RotationRulesType,
    deletedDate :: Lude.Maybe Lude.Timestamp,
    rotationEnabled :: Lude.Maybe Lude.Bool,
    createdDate :: Lude.Maybe Lude.Timestamp,
    kmsKeyId :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    versionIdsToStages ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.NonEmpty Lude.Text)
        ),
    owningService :: Lude.Maybe Lude.Text,
    lastRotatedDate :: Lude.Maybe Lude.Timestamp,
    lastAccessedDate :: Lude.Maybe Lude.Timestamp,
    description :: Lude.Maybe Lude.Text,
    rotationLambdaARN :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
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

-- | Creates a value of 'DescribeSecretResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the secret.
-- * 'createdDate' - The date that the secret was created.
-- * 'deletedDate' - This value exists if the secret is scheduled for deletion. Some time after the specified date and time, Secrets Manager deletes the secret and all of its versions.
--
-- If a secret is scheduled for deletion, then its details, including the encrypted secret information, is not accessible. To cancel a scheduled deletion and restore access, use 'RestoreSecret' .
-- * 'description' - The user-provided description of the secret.
-- * 'kmsKeyId' - The ARN or alias of the AWS KMS customer master key (CMK) that's used to encrypt the @SecretString@ or @SecretBinary@ fields in each version of the secret. If you don't provide a key, then Secrets Manager defaults to encrypting the secret fields with the default AWS KMS CMK (the one named @awssecretsmanager@ ) for this account.
-- * 'lastAccessedDate' - The last date that this secret was accessed. This value is truncated to midnight of the date and therefore shows only the date, not the time.
-- * 'lastChangedDate' - The last date and time that this secret was modified in any way.
-- * 'lastRotatedDate' - The most recent date and time that the Secrets Manager rotation process was successfully completed. This value is null if the secret has never rotated.
-- * 'name' - The user-provided friendly name of the secret.
-- * 'owningService' - Returns the name of the service that created this secret.
-- * 'responseStatus' - The response status code.
-- * 'rotationEnabled' - Specifies whether automatic rotation is enabled for this secret.
--
-- To enable rotation, use 'RotateSecret' with @AutomaticallyRotateAfterDays@ set to a value greater than 0. To disable rotation, use 'CancelRotateSecret' .
-- * 'rotationLambdaARN' - The ARN of a Lambda function that's invoked by Secrets Manager to rotate the secret either automatically per the schedule or manually by a call to @RotateSecret@ .
-- * 'rotationRules' - A structure that contains the rotation configuration for this secret.
-- * 'tags' - The list of user-defined tags that are associated with the secret. To add tags to a secret, use 'TagResource' . To remove tags, use 'UntagResource' .
-- * 'versionIdsToStages' - A list of all of the currently assigned @VersionStage@ staging labels and the @VersionId@ that each is attached to. Staging labels are used to keep track of the different versions during the rotation process.
mkDescribeSecretResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSecretResponse
mkDescribeSecretResponse pResponseStatus_ =
  DescribeSecretResponse'
    { lastChangedDate = Lude.Nothing,
      arn = Lude.Nothing,
      rotationRules = Lude.Nothing,
      deletedDate = Lude.Nothing,
      rotationEnabled = Lude.Nothing,
      createdDate = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      name = Lude.Nothing,
      versionIdsToStages = Lude.Nothing,
      owningService = Lude.Nothing,
      lastRotatedDate = Lude.Nothing,
      lastAccessedDate = Lude.Nothing,
      description = Lude.Nothing,
      rotationLambdaARN = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The last date and time that this secret was modified in any way.
--
-- /Note:/ Consider using 'lastChangedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsLastChangedDate :: Lens.Lens' DescribeSecretResponse (Lude.Maybe Lude.Timestamp)
drsLastChangedDate = Lens.lens (lastChangedDate :: DescribeSecretResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastChangedDate = a} :: DescribeSecretResponse)
{-# DEPRECATED drsLastChangedDate "Use generic-lens or generic-optics with 'lastChangedDate' instead." #-}

-- | The ARN of the secret.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsARN :: Lens.Lens' DescribeSecretResponse (Lude.Maybe Lude.Text)
drsARN = Lens.lens (arn :: DescribeSecretResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DescribeSecretResponse)
{-# DEPRECATED drsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A structure that contains the rotation configuration for this secret.
--
-- /Note:/ Consider using 'rotationRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsRotationRules :: Lens.Lens' DescribeSecretResponse (Lude.Maybe RotationRulesType)
drsRotationRules = Lens.lens (rotationRules :: DescribeSecretResponse -> Lude.Maybe RotationRulesType) (\s a -> s {rotationRules = a} :: DescribeSecretResponse)
{-# DEPRECATED drsRotationRules "Use generic-lens or generic-optics with 'rotationRules' instead." #-}

-- | This value exists if the secret is scheduled for deletion. Some time after the specified date and time, Secrets Manager deletes the secret and all of its versions.
--
-- If a secret is scheduled for deletion, then its details, including the encrypted secret information, is not accessible. To cancel a scheduled deletion and restore access, use 'RestoreSecret' .
--
-- /Note:/ Consider using 'deletedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDeletedDate :: Lens.Lens' DescribeSecretResponse (Lude.Maybe Lude.Timestamp)
drsDeletedDate = Lens.lens (deletedDate :: DescribeSecretResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {deletedDate = a} :: DescribeSecretResponse)
{-# DEPRECATED drsDeletedDate "Use generic-lens or generic-optics with 'deletedDate' instead." #-}

-- | Specifies whether automatic rotation is enabled for this secret.
--
-- To enable rotation, use 'RotateSecret' with @AutomaticallyRotateAfterDays@ set to a value greater than 0. To disable rotation, use 'CancelRotateSecret' .
--
-- /Note:/ Consider using 'rotationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsRotationEnabled :: Lens.Lens' DescribeSecretResponse (Lude.Maybe Lude.Bool)
drsRotationEnabled = Lens.lens (rotationEnabled :: DescribeSecretResponse -> Lude.Maybe Lude.Bool) (\s a -> s {rotationEnabled = a} :: DescribeSecretResponse)
{-# DEPRECATED drsRotationEnabled "Use generic-lens or generic-optics with 'rotationEnabled' instead." #-}

-- | The date that the secret was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCreatedDate :: Lens.Lens' DescribeSecretResponse (Lude.Maybe Lude.Timestamp)
drsCreatedDate = Lens.lens (createdDate :: DescribeSecretResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: DescribeSecretResponse)
{-# DEPRECATED drsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The ARN or alias of the AWS KMS customer master key (CMK) that's used to encrypt the @SecretString@ or @SecretBinary@ fields in each version of the secret. If you don't provide a key, then Secrets Manager defaults to encrypting the secret fields with the default AWS KMS CMK (the one named @awssecretsmanager@ ) for this account.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsKMSKeyId :: Lens.Lens' DescribeSecretResponse (Lude.Maybe Lude.Text)
drsKMSKeyId = Lens.lens (kmsKeyId :: DescribeSecretResponse -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: DescribeSecretResponse)
{-# DEPRECATED drsKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The user-provided friendly name of the secret.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsName :: Lens.Lens' DescribeSecretResponse (Lude.Maybe Lude.Text)
drsName = Lens.lens (name :: DescribeSecretResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DescribeSecretResponse)
{-# DEPRECATED drsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of all of the currently assigned @VersionStage@ staging labels and the @VersionId@ that each is attached to. Staging labels are used to keep track of the different versions during the rotation process.
--
-- /Note:/ Consider using 'versionIdsToStages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsVersionIdsToStages :: Lens.Lens' DescribeSecretResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.NonEmpty Lude.Text)))
drsVersionIdsToStages = Lens.lens (versionIdsToStages :: DescribeSecretResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.NonEmpty Lude.Text))) (\s a -> s {versionIdsToStages = a} :: DescribeSecretResponse)
{-# DEPRECATED drsVersionIdsToStages "Use generic-lens or generic-optics with 'versionIdsToStages' instead." #-}

-- | Returns the name of the service that created this secret.
--
-- /Note:/ Consider using 'owningService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsOwningService :: Lens.Lens' DescribeSecretResponse (Lude.Maybe Lude.Text)
drsOwningService = Lens.lens (owningService :: DescribeSecretResponse -> Lude.Maybe Lude.Text) (\s a -> s {owningService = a} :: DescribeSecretResponse)
{-# DEPRECATED drsOwningService "Use generic-lens or generic-optics with 'owningService' instead." #-}

-- | The most recent date and time that the Secrets Manager rotation process was successfully completed. This value is null if the secret has never rotated.
--
-- /Note:/ Consider using 'lastRotatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsLastRotatedDate :: Lens.Lens' DescribeSecretResponse (Lude.Maybe Lude.Timestamp)
drsLastRotatedDate = Lens.lens (lastRotatedDate :: DescribeSecretResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastRotatedDate = a} :: DescribeSecretResponse)
{-# DEPRECATED drsLastRotatedDate "Use generic-lens or generic-optics with 'lastRotatedDate' instead." #-}

-- | The last date that this secret was accessed. This value is truncated to midnight of the date and therefore shows only the date, not the time.
--
-- /Note:/ Consider using 'lastAccessedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsLastAccessedDate :: Lens.Lens' DescribeSecretResponse (Lude.Maybe Lude.Timestamp)
drsLastAccessedDate = Lens.lens (lastAccessedDate :: DescribeSecretResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastAccessedDate = a} :: DescribeSecretResponse)
{-# DEPRECATED drsLastAccessedDate "Use generic-lens or generic-optics with 'lastAccessedDate' instead." #-}

-- | The user-provided description of the secret.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDescription :: Lens.Lens' DescribeSecretResponse (Lude.Maybe Lude.Text)
drsDescription = Lens.lens (description :: DescribeSecretResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DescribeSecretResponse)
{-# DEPRECATED drsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ARN of a Lambda function that's invoked by Secrets Manager to rotate the secret either automatically per the schedule or manually by a call to @RotateSecret@ .
--
-- /Note:/ Consider using 'rotationLambdaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsRotationLambdaARN :: Lens.Lens' DescribeSecretResponse (Lude.Maybe Lude.Text)
drsRotationLambdaARN = Lens.lens (rotationLambdaARN :: DescribeSecretResponse -> Lude.Maybe Lude.Text) (\s a -> s {rotationLambdaARN = a} :: DescribeSecretResponse)
{-# DEPRECATED drsRotationLambdaARN "Use generic-lens or generic-optics with 'rotationLambdaARN' instead." #-}

-- | The list of user-defined tags that are associated with the secret. To add tags to a secret, use 'TagResource' . To remove tags, use 'UntagResource' .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsTags :: Lens.Lens' DescribeSecretResponse (Lude.Maybe [Tag])
drsTags = Lens.lens (tags :: DescribeSecretResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: DescribeSecretResponse)
{-# DEPRECATED drsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeSecretResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeSecretResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSecretResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
