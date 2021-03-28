{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.Types.SecretListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SecretsManager.Types.SecretListEntry
  ( SecretListEntry (..)
  -- * Smart constructor
  , mkSecretListEntry
  -- * Lenses
  , sleARN
  , sleCreatedDate
  , sleDeletedDate
  , sleDescription
  , sleKmsKeyId
  , sleLastAccessedDate
  , sleLastChangedDate
  , sleLastRotatedDate
  , sleName
  , sleOwningService
  , sleRotationEnabled
  , sleRotationLambdaARN
  , sleRotationRules
  , sleSecretVersionsToStages
  , sleTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SecretsManager.Types.ARN as Types
import qualified Network.AWS.SecretsManager.Types.Description as Types
import qualified Network.AWS.SecretsManager.Types.KmsKeyIdType as Types
import qualified Network.AWS.SecretsManager.Types.Name as Types
import qualified Network.AWS.SecretsManager.Types.OwningService as Types
import qualified Network.AWS.SecretsManager.Types.RotationLambdaARN as Types
import qualified Network.AWS.SecretsManager.Types.RotationRulesType as Types
import qualified Network.AWS.SecretsManager.Types.SecretVersionIdType as Types
import qualified Network.AWS.SecretsManager.Types.SecretVersionStageType as Types
import qualified Network.AWS.SecretsManager.Types.Tag as Types

-- | A structure that contains the details about a secret. It does not include the encrypted @SecretString@ and @SecretBinary@ values. To get those values, use the 'GetSecretValue' operation.
--
-- /See:/ 'mkSecretListEntry' smart constructor.
data SecretListEntry = SecretListEntry'
  { arn :: Core.Maybe Types.ARN
    -- ^ The Amazon Resource Name (ARN) of the secret.
--
-- For more information about ARNs in Secrets Manager, see <https://docs.aws.amazon.com/secretsmanager/latest/userguide/reference_iam-permissions.html#iam-resources Policy Resources> in the /AWS Secrets Manager User Guide/ .
  , createdDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time when a secret was created.
  , deletedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time the deletion of the secret occurred. Not present on active secrets. The secret can be recovered until the number of days in the recovery window has passed, as specified in the @RecoveryWindowInDays@ parameter of the 'DeleteSecret' operation.
  , description :: Core.Maybe Types.Description
    -- ^ The user-provided description of the secret.
  , kmsKeyId :: Core.Maybe Types.KmsKeyIdType
    -- ^ The ARN or alias of the AWS KMS customer master key (CMK) used to encrypt the @SecretString@ and @SecretBinary@ fields in each version of the secret. If you don't provide a key, then Secrets Manager defaults to encrypting the secret fields with the default KMS CMK, the key named @awssecretsmanager@ , for this account.
  , lastAccessedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The last date that this secret was accessed. This value is truncated to midnight of the date and therefore shows only the date, not the time.
  , lastChangedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The last date and time that this secret was modified in any way.
  , lastRotatedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The last date and time that the rotation process for this secret was invoked.
  , name :: Core.Maybe Types.Name
    -- ^ The friendly name of the secret. You can use forward slashes in the name to represent a path hierarchy. For example, @/prod/databases/dbserver1@ could represent the secret for a server named @dbserver1@ in the folder @databases@ in the folder @prod@ . 
  , owningService :: Core.Maybe Types.OwningService
    -- ^ Returns the name of the service that created the secret.
  , rotationEnabled :: Core.Maybe Core.Bool
    -- ^ Indicates whether automatic, scheduled rotation is enabled for this secret.
  , rotationLambdaARN :: Core.Maybe Types.RotationLambdaARN
    -- ^ The ARN of an AWS Lambda function invoked by Secrets Manager to rotate and expire the secret either automatically per the schedule or manually by a call to 'RotateSecret' .
  , rotationRules :: Core.Maybe Types.RotationRulesType
    -- ^ A structure that defines the rotation configuration for the secret.
  , secretVersionsToStages :: Core.Maybe (Core.HashMap Types.SecretVersionIdType (Core.NonEmpty Types.SecretVersionStageType))
    -- ^ A list of all of the currently assigned @SecretVersionStage@ staging labels and the @SecretVersionId@ attached to each one. Staging labels are used to keep track of the different versions during the rotation process.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The list of user-defined tags associated with the secret. To add tags to a secret, use 'TagResource' . To remove tags, use 'UntagResource' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SecretListEntry' value with any optional fields omitted.
mkSecretListEntry
    :: SecretListEntry
mkSecretListEntry
  = SecretListEntry'{arn = Core.Nothing, createdDate = Core.Nothing,
                     deletedDate = Core.Nothing, description = Core.Nothing,
                     kmsKeyId = Core.Nothing, lastAccessedDate = Core.Nothing,
                     lastChangedDate = Core.Nothing, lastRotatedDate = Core.Nothing,
                     name = Core.Nothing, owningService = Core.Nothing,
                     rotationEnabled = Core.Nothing, rotationLambdaARN = Core.Nothing,
                     rotationRules = Core.Nothing,
                     secretVersionsToStages = Core.Nothing, tags = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the secret.
--
-- For more information about ARNs in Secrets Manager, see <https://docs.aws.amazon.com/secretsmanager/latest/userguide/reference_iam-permissions.html#iam-resources Policy Resources> in the /AWS Secrets Manager User Guide/ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleARN :: Lens.Lens' SecretListEntry (Core.Maybe Types.ARN)
sleARN = Lens.field @"arn"
{-# INLINEABLE sleARN #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The date and time when a secret was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleCreatedDate :: Lens.Lens' SecretListEntry (Core.Maybe Core.NominalDiffTime)
sleCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE sleCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | The date and time the deletion of the secret occurred. Not present on active secrets. The secret can be recovered until the number of days in the recovery window has passed, as specified in the @RecoveryWindowInDays@ parameter of the 'DeleteSecret' operation.
--
-- /Note:/ Consider using 'deletedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleDeletedDate :: Lens.Lens' SecretListEntry (Core.Maybe Core.NominalDiffTime)
sleDeletedDate = Lens.field @"deletedDate"
{-# INLINEABLE sleDeletedDate #-}
{-# DEPRECATED deletedDate "Use generic-lens or generic-optics with 'deletedDate' instead"  #-}

-- | The user-provided description of the secret.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleDescription :: Lens.Lens' SecretListEntry (Core.Maybe Types.Description)
sleDescription = Lens.field @"description"
{-# INLINEABLE sleDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The ARN or alias of the AWS KMS customer master key (CMK) used to encrypt the @SecretString@ and @SecretBinary@ fields in each version of the secret. If you don't provide a key, then Secrets Manager defaults to encrypting the secret fields with the default KMS CMK, the key named @awssecretsmanager@ , for this account.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleKmsKeyId :: Lens.Lens' SecretListEntry (Core.Maybe Types.KmsKeyIdType)
sleKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE sleKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The last date that this secret was accessed. This value is truncated to midnight of the date and therefore shows only the date, not the time.
--
-- /Note:/ Consider using 'lastAccessedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleLastAccessedDate :: Lens.Lens' SecretListEntry (Core.Maybe Core.NominalDiffTime)
sleLastAccessedDate = Lens.field @"lastAccessedDate"
{-# INLINEABLE sleLastAccessedDate #-}
{-# DEPRECATED lastAccessedDate "Use generic-lens or generic-optics with 'lastAccessedDate' instead"  #-}

-- | The last date and time that this secret was modified in any way.
--
-- /Note:/ Consider using 'lastChangedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleLastChangedDate :: Lens.Lens' SecretListEntry (Core.Maybe Core.NominalDiffTime)
sleLastChangedDate = Lens.field @"lastChangedDate"
{-# INLINEABLE sleLastChangedDate #-}
{-# DEPRECATED lastChangedDate "Use generic-lens or generic-optics with 'lastChangedDate' instead"  #-}

-- | The last date and time that the rotation process for this secret was invoked.
--
-- /Note:/ Consider using 'lastRotatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleLastRotatedDate :: Lens.Lens' SecretListEntry (Core.Maybe Core.NominalDiffTime)
sleLastRotatedDate = Lens.field @"lastRotatedDate"
{-# INLINEABLE sleLastRotatedDate #-}
{-# DEPRECATED lastRotatedDate "Use generic-lens or generic-optics with 'lastRotatedDate' instead"  #-}

-- | The friendly name of the secret. You can use forward slashes in the name to represent a path hierarchy. For example, @/prod/databases/dbserver1@ could represent the secret for a server named @dbserver1@ in the folder @databases@ in the folder @prod@ . 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleName :: Lens.Lens' SecretListEntry (Core.Maybe Types.Name)
sleName = Lens.field @"name"
{-# INLINEABLE sleName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Returns the name of the service that created the secret.
--
-- /Note:/ Consider using 'owningService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleOwningService :: Lens.Lens' SecretListEntry (Core.Maybe Types.OwningService)
sleOwningService = Lens.field @"owningService"
{-# INLINEABLE sleOwningService #-}
{-# DEPRECATED owningService "Use generic-lens or generic-optics with 'owningService' instead"  #-}

-- | Indicates whether automatic, scheduled rotation is enabled for this secret.
--
-- /Note:/ Consider using 'rotationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleRotationEnabled :: Lens.Lens' SecretListEntry (Core.Maybe Core.Bool)
sleRotationEnabled = Lens.field @"rotationEnabled"
{-# INLINEABLE sleRotationEnabled #-}
{-# DEPRECATED rotationEnabled "Use generic-lens or generic-optics with 'rotationEnabled' instead"  #-}

-- | The ARN of an AWS Lambda function invoked by Secrets Manager to rotate and expire the secret either automatically per the schedule or manually by a call to 'RotateSecret' .
--
-- /Note:/ Consider using 'rotationLambdaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleRotationLambdaARN :: Lens.Lens' SecretListEntry (Core.Maybe Types.RotationLambdaARN)
sleRotationLambdaARN = Lens.field @"rotationLambdaARN"
{-# INLINEABLE sleRotationLambdaARN #-}
{-# DEPRECATED rotationLambdaARN "Use generic-lens or generic-optics with 'rotationLambdaARN' instead"  #-}

-- | A structure that defines the rotation configuration for the secret.
--
-- /Note:/ Consider using 'rotationRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleRotationRules :: Lens.Lens' SecretListEntry (Core.Maybe Types.RotationRulesType)
sleRotationRules = Lens.field @"rotationRules"
{-# INLINEABLE sleRotationRules #-}
{-# DEPRECATED rotationRules "Use generic-lens or generic-optics with 'rotationRules' instead"  #-}

-- | A list of all of the currently assigned @SecretVersionStage@ staging labels and the @SecretVersionId@ attached to each one. Staging labels are used to keep track of the different versions during the rotation process.
--
-- /Note:/ Consider using 'secretVersionsToStages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleSecretVersionsToStages :: Lens.Lens' SecretListEntry (Core.Maybe (Core.HashMap Types.SecretVersionIdType (Core.NonEmpty Types.SecretVersionStageType)))
sleSecretVersionsToStages = Lens.field @"secretVersionsToStages"
{-# INLINEABLE sleSecretVersionsToStages #-}
{-# DEPRECATED secretVersionsToStages "Use generic-lens or generic-optics with 'secretVersionsToStages' instead"  #-}

-- | The list of user-defined tags associated with the secret. To add tags to a secret, use 'TagResource' . To remove tags, use 'UntagResource' .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sleTags :: Lens.Lens' SecretListEntry (Core.Maybe [Types.Tag])
sleTags = Lens.field @"tags"
{-# INLINEABLE sleTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON SecretListEntry where
        parseJSON
          = Core.withObject "SecretListEntry" Core.$
              \ x ->
                SecretListEntry' Core.<$>
                  (x Core..:? "ARN") Core.<*> x Core..:? "CreatedDate" Core.<*>
                    x Core..:? "DeletedDate"
                    Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "KmsKeyId"
                    Core.<*> x Core..:? "LastAccessedDate"
                    Core.<*> x Core..:? "LastChangedDate"
                    Core.<*> x Core..:? "LastRotatedDate"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "OwningService"
                    Core.<*> x Core..:? "RotationEnabled"
                    Core.<*> x Core..:? "RotationLambdaARN"
                    Core.<*> x Core..:? "RotationRules"
                    Core.<*> x Core..:? "SecretVersionsToStages"
                    Core.<*> x Core..:? "Tags"
