{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.Types.SecretListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecretsManager.Types.SecretListEntry
  ( SecretListEntry (..),

    -- * Smart constructor
    mkSecretListEntry,

    -- * Lenses
    sleLastChangedDate,
    sleARN,
    sleSecretVersionsToStages,
    sleRotationRules,
    sleDeletedDate,
    sleRotationEnabled,
    sleCreatedDate,
    sleKMSKeyId,
    sleName,
    sleOwningService,
    sleLastRotatedDate,
    sleLastAccessedDate,
    sleDescription,
    sleRotationLambdaARN,
    sleTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SecretsManager.Types.RotationRulesType
import Network.AWS.SecretsManager.Types.Tag

-- | A structure that contains the details about a secret. It does not include the encrypted @SecretString@ and @SecretBinary@ values. To get those values, use the 'GetSecretValue' operation.
--
-- /See:/ 'mkSecretListEntry' smart constructor.
data SecretListEntry = SecretListEntry'
  { lastChangedDate ::
      Lude.Maybe Lude.Timestamp,
    arn :: Lude.Maybe Lude.Text,
    secretVersionsToStages ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.NonEmpty Lude.Text)),
    rotationRules :: Lude.Maybe RotationRulesType,
    deletedDate :: Lude.Maybe Lude.Timestamp,
    rotationEnabled :: Lude.Maybe Lude.Bool,
    createdDate :: Lude.Maybe Lude.Timestamp,
    kmsKeyId :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    owningService :: Lude.Maybe Lude.Text,
    lastRotatedDate :: Lude.Maybe Lude.Timestamp,
    lastAccessedDate :: Lude.Maybe Lude.Timestamp,
    description :: Lude.Maybe Lude.Text,
    rotationLambdaARN :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SecretListEntry' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the secret.
--
-- For more information about ARNs in Secrets Manager, see <https://docs.aws.amazon.com/secretsmanager/latest/userguide/reference_iam-permissions.html#iam-resources Policy Resources> in the /AWS Secrets Manager User Guide/ .
-- * 'createdDate' - The date and time when a secret was created.
-- * 'deletedDate' - The date and time the deletion of the secret occurred. Not present on active secrets. The secret can be recovered until the number of days in the recovery window has passed, as specified in the @RecoveryWindowInDays@ parameter of the 'DeleteSecret' operation.
-- * 'description' - The user-provided description of the secret.
-- * 'kmsKeyId' - The ARN or alias of the AWS KMS customer master key (CMK) used to encrypt the @SecretString@ and @SecretBinary@ fields in each version of the secret. If you don't provide a key, then Secrets Manager defaults to encrypting the secret fields with the default KMS CMK, the key named @awssecretsmanager@ , for this account.
-- * 'lastAccessedDate' - The last date that this secret was accessed. This value is truncated to midnight of the date and therefore shows only the date, not the time.
-- * 'lastChangedDate' - The last date and time that this secret was modified in any way.
-- * 'lastRotatedDate' - The last date and time that the rotation process for this secret was invoked.
-- * 'name' - The friendly name of the secret. You can use forward slashes in the name to represent a path hierarchy. For example, @/prod/databases/dbserver1@ could represent the secret for a server named @dbserver1@ in the folder @databases@ in the folder @prod@ .
-- * 'owningService' - Returns the name of the service that created the secret.
-- * 'rotationEnabled' - Indicates whether automatic, scheduled rotation is enabled for this secret.
-- * 'rotationLambdaARN' - The ARN of an AWS Lambda function invoked by Secrets Manager to rotate and expire the secret either automatically per the schedule or manually by a call to 'RotateSecret' .
-- * 'rotationRules' - A structure that defines the rotation configuration for the secret.
-- * 'secretVersionsToStages' - A list of all of the currently assigned @SecretVersionStage@ staging labels and the @SecretVersionId@ attached to each one. Staging labels are used to keep track of the different versions during the rotation process.
-- * 'tags' - The list of user-defined tags associated with the secret. To add tags to a secret, use 'TagResource' . To remove tags, use 'UntagResource' .
mkSecretListEntry ::
  SecretListEntry
mkSecretListEntry =
  SecretListEntry'
    { lastChangedDate = Lude.Nothing,
      arn = Lude.Nothing,
      secretVersionsToStages = Lude.Nothing,
      rotationRules = Lude.Nothing,
      deletedDate = Lude.Nothing,
      rotationEnabled = Lude.Nothing,
      createdDate = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      name = Lude.Nothing,
      owningService = Lude.Nothing,
      lastRotatedDate = Lude.Nothing,
      lastAccessedDate = Lude.Nothing,
      description = Lude.Nothing,
      rotationLambdaARN = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The last date and time that this secret was modified in any way.
--
-- /Note:/ Consider using 'lastChangedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleLastChangedDate :: Lens.Lens' SecretListEntry (Lude.Maybe Lude.Timestamp)
sleLastChangedDate = Lens.lens (lastChangedDate :: SecretListEntry -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastChangedDate = a} :: SecretListEntry)
{-# DEPRECATED sleLastChangedDate "Use generic-lens or generic-optics with 'lastChangedDate' instead." #-}

-- | The Amazon Resource Name (ARN) of the secret.
--
-- For more information about ARNs in Secrets Manager, see <https://docs.aws.amazon.com/secretsmanager/latest/userguide/reference_iam-permissions.html#iam-resources Policy Resources> in the /AWS Secrets Manager User Guide/ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleARN :: Lens.Lens' SecretListEntry (Lude.Maybe Lude.Text)
sleARN = Lens.lens (arn :: SecretListEntry -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: SecretListEntry)
{-# DEPRECATED sleARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A list of all of the currently assigned @SecretVersionStage@ staging labels and the @SecretVersionId@ attached to each one. Staging labels are used to keep track of the different versions during the rotation process.
--
-- /Note:/ Consider using 'secretVersionsToStages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleSecretVersionsToStages :: Lens.Lens' SecretListEntry (Lude.Maybe (Lude.HashMap Lude.Text (Lude.NonEmpty Lude.Text)))
sleSecretVersionsToStages = Lens.lens (secretVersionsToStages :: SecretListEntry -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.NonEmpty Lude.Text))) (\s a -> s {secretVersionsToStages = a} :: SecretListEntry)
{-# DEPRECATED sleSecretVersionsToStages "Use generic-lens or generic-optics with 'secretVersionsToStages' instead." #-}

-- | A structure that defines the rotation configuration for the secret.
--
-- /Note:/ Consider using 'rotationRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleRotationRules :: Lens.Lens' SecretListEntry (Lude.Maybe RotationRulesType)
sleRotationRules = Lens.lens (rotationRules :: SecretListEntry -> Lude.Maybe RotationRulesType) (\s a -> s {rotationRules = a} :: SecretListEntry)
{-# DEPRECATED sleRotationRules "Use generic-lens or generic-optics with 'rotationRules' instead." #-}

-- | The date and time the deletion of the secret occurred. Not present on active secrets. The secret can be recovered until the number of days in the recovery window has passed, as specified in the @RecoveryWindowInDays@ parameter of the 'DeleteSecret' operation.
--
-- /Note:/ Consider using 'deletedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleDeletedDate :: Lens.Lens' SecretListEntry (Lude.Maybe Lude.Timestamp)
sleDeletedDate = Lens.lens (deletedDate :: SecretListEntry -> Lude.Maybe Lude.Timestamp) (\s a -> s {deletedDate = a} :: SecretListEntry)
{-# DEPRECATED sleDeletedDate "Use generic-lens or generic-optics with 'deletedDate' instead." #-}

-- | Indicates whether automatic, scheduled rotation is enabled for this secret.
--
-- /Note:/ Consider using 'rotationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleRotationEnabled :: Lens.Lens' SecretListEntry (Lude.Maybe Lude.Bool)
sleRotationEnabled = Lens.lens (rotationEnabled :: SecretListEntry -> Lude.Maybe Lude.Bool) (\s a -> s {rotationEnabled = a} :: SecretListEntry)
{-# DEPRECATED sleRotationEnabled "Use generic-lens or generic-optics with 'rotationEnabled' instead." #-}

-- | The date and time when a secret was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleCreatedDate :: Lens.Lens' SecretListEntry (Lude.Maybe Lude.Timestamp)
sleCreatedDate = Lens.lens (createdDate :: SecretListEntry -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: SecretListEntry)
{-# DEPRECATED sleCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The ARN or alias of the AWS KMS customer master key (CMK) used to encrypt the @SecretString@ and @SecretBinary@ fields in each version of the secret. If you don't provide a key, then Secrets Manager defaults to encrypting the secret fields with the default KMS CMK, the key named @awssecretsmanager@ , for this account.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleKMSKeyId :: Lens.Lens' SecretListEntry (Lude.Maybe Lude.Text)
sleKMSKeyId = Lens.lens (kmsKeyId :: SecretListEntry -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: SecretListEntry)
{-# DEPRECATED sleKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The friendly name of the secret. You can use forward slashes in the name to represent a path hierarchy. For example, @/prod/databases/dbserver1@ could represent the secret for a server named @dbserver1@ in the folder @databases@ in the folder @prod@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleName :: Lens.Lens' SecretListEntry (Lude.Maybe Lude.Text)
sleName = Lens.lens (name :: SecretListEntry -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: SecretListEntry)
{-# DEPRECATED sleName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Returns the name of the service that created the secret.
--
-- /Note:/ Consider using 'owningService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleOwningService :: Lens.Lens' SecretListEntry (Lude.Maybe Lude.Text)
sleOwningService = Lens.lens (owningService :: SecretListEntry -> Lude.Maybe Lude.Text) (\s a -> s {owningService = a} :: SecretListEntry)
{-# DEPRECATED sleOwningService "Use generic-lens or generic-optics with 'owningService' instead." #-}

-- | The last date and time that the rotation process for this secret was invoked.
--
-- /Note:/ Consider using 'lastRotatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleLastRotatedDate :: Lens.Lens' SecretListEntry (Lude.Maybe Lude.Timestamp)
sleLastRotatedDate = Lens.lens (lastRotatedDate :: SecretListEntry -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastRotatedDate = a} :: SecretListEntry)
{-# DEPRECATED sleLastRotatedDate "Use generic-lens or generic-optics with 'lastRotatedDate' instead." #-}

-- | The last date that this secret was accessed. This value is truncated to midnight of the date and therefore shows only the date, not the time.
--
-- /Note:/ Consider using 'lastAccessedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleLastAccessedDate :: Lens.Lens' SecretListEntry (Lude.Maybe Lude.Timestamp)
sleLastAccessedDate = Lens.lens (lastAccessedDate :: SecretListEntry -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastAccessedDate = a} :: SecretListEntry)
{-# DEPRECATED sleLastAccessedDate "Use generic-lens or generic-optics with 'lastAccessedDate' instead." #-}

-- | The user-provided description of the secret.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleDescription :: Lens.Lens' SecretListEntry (Lude.Maybe Lude.Text)
sleDescription = Lens.lens (description :: SecretListEntry -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: SecretListEntry)
{-# DEPRECATED sleDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ARN of an AWS Lambda function invoked by Secrets Manager to rotate and expire the secret either automatically per the schedule or manually by a call to 'RotateSecret' .
--
-- /Note:/ Consider using 'rotationLambdaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleRotationLambdaARN :: Lens.Lens' SecretListEntry (Lude.Maybe Lude.Text)
sleRotationLambdaARN = Lens.lens (rotationLambdaARN :: SecretListEntry -> Lude.Maybe Lude.Text) (\s a -> s {rotationLambdaARN = a} :: SecretListEntry)
{-# DEPRECATED sleRotationLambdaARN "Use generic-lens or generic-optics with 'rotationLambdaARN' instead." #-}

-- | The list of user-defined tags associated with the secret. To add tags to a secret, use 'TagResource' . To remove tags, use 'UntagResource' .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleTags :: Lens.Lens' SecretListEntry (Lude.Maybe [Tag])
sleTags = Lens.lens (tags :: SecretListEntry -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: SecretListEntry)
{-# DEPRECATED sleTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON SecretListEntry where
  parseJSON =
    Lude.withObject
      "SecretListEntry"
      ( \x ->
          SecretListEntry'
            Lude.<$> (x Lude..:? "LastChangedDate")
            Lude.<*> (x Lude..:? "ARN")
            Lude.<*> (x Lude..:? "SecretVersionsToStages" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "RotationRules")
            Lude.<*> (x Lude..:? "DeletedDate")
            Lude.<*> (x Lude..:? "RotationEnabled")
            Lude.<*> (x Lude..:? "CreatedDate")
            Lude.<*> (x Lude..:? "KmsKeyId")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "OwningService")
            Lude.<*> (x Lude..:? "LastRotatedDate")
            Lude.<*> (x Lude..:? "LastAccessedDate")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "RotationLambdaARN")
            Lude.<*> (x Lude..:? "Tags" Lude..!= Lude.mempty)
      )
