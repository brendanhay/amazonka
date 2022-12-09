{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SecretsManager.Types.SecretListEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecretsManager.Types.SecretListEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecretsManager.Types.RotationRulesType
import Amazonka.SecretsManager.Types.Tag

-- | A structure that contains the details about a secret. It does not
-- include the encrypted @SecretString@ and @SecretBinary@ values. To get
-- those values, use
-- <https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_GetSecretValue.html GetSecretValue>
-- .
--
-- /See:/ 'newSecretListEntry' smart constructor.
data SecretListEntry = SecretListEntry'
  { -- | The Amazon Resource Name (ARN) of the secret.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time when a secret was created.
    createdDate :: Prelude.Maybe Data.POSIX,
    -- | The date and time the deletion of the secret occurred. Not present on
    -- active secrets. The secret can be recovered until the number of days in
    -- the recovery window has passed, as specified in the
    -- @RecoveryWindowInDays@ parameter of the
    -- <https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_DeleteSecret.html DeleteSecret>
    -- operation.
    deletedDate :: Prelude.Maybe Data.POSIX,
    -- | The user-provided description of the secret.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the KMS key that Secrets Manager uses to encrypt the secret
    -- value. If the secret is encrypted with the Amazon Web Services managed
    -- key @aws\/secretsmanager@, this field is omitted.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The date that the secret was last accessed in the Region. This field is
    -- omitted if the secret has never been retrieved in the Region.
    lastAccessedDate :: Prelude.Maybe Data.POSIX,
    -- | The last date and time that this secret was modified in any way.
    lastChangedDate :: Prelude.Maybe Data.POSIX,
    -- | The most recent date and time that the Secrets Manager rotation process
    -- was successfully completed. This value is null if the secret hasn\'t
    -- ever rotated.
    lastRotatedDate :: Prelude.Maybe Data.POSIX,
    -- | The friendly name of the secret. You can use forward slashes in the name
    -- to represent a path hierarchy. For example,
    -- @\/prod\/databases\/dbserver1@ could represent the secret for a server
    -- named @dbserver1@ in the folder @databases@ in the folder @prod@.
    name :: Prelude.Maybe Prelude.Text,
    -- | Returns the name of the service that created the secret.
    owningService :: Prelude.Maybe Prelude.Text,
    -- | The Region where Secrets Manager originated the secret.
    primaryRegion :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether automatic, scheduled rotation is enabled for this
    -- secret.
    rotationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of an Amazon Web Services Lambda function invoked by Secrets
    -- Manager to rotate and expire the secret either automatically per the
    -- schedule or manually by a call to
    -- <https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_RotateSecret.html RotateSecret>
    -- .
    rotationLambdaARN :: Prelude.Maybe Prelude.Text,
    -- | A structure that defines the rotation configuration for the secret.
    rotationRules :: Prelude.Maybe RotationRulesType,
    -- | A list of all of the currently assigned @SecretVersionStage@ staging
    -- labels and the @SecretVersionId@ attached to each one. Staging labels
    -- are used to keep track of the different versions during the rotation
    -- process.
    --
    -- A version that does not have any @SecretVersionStage@ is considered
    -- deprecated and subject to deletion. Such versions are not included in
    -- this list.
    secretVersionsToStages :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)),
    -- | The list of user-defined tags associated with the secret. To add tags to
    -- a secret, use
    -- <https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_TagResource.html TagResource>
    -- . To remove tags, use
    -- <https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_UntagResource.html UntagResource>
    -- .
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecretListEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'secretListEntry_arn' - The Amazon Resource Name (ARN) of the secret.
--
-- 'createdDate', 'secretListEntry_createdDate' - The date and time when a secret was created.
--
-- 'deletedDate', 'secretListEntry_deletedDate' - The date and time the deletion of the secret occurred. Not present on
-- active secrets. The secret can be recovered until the number of days in
-- the recovery window has passed, as specified in the
-- @RecoveryWindowInDays@ parameter of the
-- <https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_DeleteSecret.html DeleteSecret>
-- operation.
--
-- 'description', 'secretListEntry_description' - The user-provided description of the secret.
--
-- 'kmsKeyId', 'secretListEntry_kmsKeyId' - The ARN of the KMS key that Secrets Manager uses to encrypt the secret
-- value. If the secret is encrypted with the Amazon Web Services managed
-- key @aws\/secretsmanager@, this field is omitted.
--
-- 'lastAccessedDate', 'secretListEntry_lastAccessedDate' - The date that the secret was last accessed in the Region. This field is
-- omitted if the secret has never been retrieved in the Region.
--
-- 'lastChangedDate', 'secretListEntry_lastChangedDate' - The last date and time that this secret was modified in any way.
--
-- 'lastRotatedDate', 'secretListEntry_lastRotatedDate' - The most recent date and time that the Secrets Manager rotation process
-- was successfully completed. This value is null if the secret hasn\'t
-- ever rotated.
--
-- 'name', 'secretListEntry_name' - The friendly name of the secret. You can use forward slashes in the name
-- to represent a path hierarchy. For example,
-- @\/prod\/databases\/dbserver1@ could represent the secret for a server
-- named @dbserver1@ in the folder @databases@ in the folder @prod@.
--
-- 'owningService', 'secretListEntry_owningService' - Returns the name of the service that created the secret.
--
-- 'primaryRegion', 'secretListEntry_primaryRegion' - The Region where Secrets Manager originated the secret.
--
-- 'rotationEnabled', 'secretListEntry_rotationEnabled' - Indicates whether automatic, scheduled rotation is enabled for this
-- secret.
--
-- 'rotationLambdaARN', 'secretListEntry_rotationLambdaARN' - The ARN of an Amazon Web Services Lambda function invoked by Secrets
-- Manager to rotate and expire the secret either automatically per the
-- schedule or manually by a call to
-- <https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_RotateSecret.html RotateSecret>
-- .
--
-- 'rotationRules', 'secretListEntry_rotationRules' - A structure that defines the rotation configuration for the secret.
--
-- 'secretVersionsToStages', 'secretListEntry_secretVersionsToStages' - A list of all of the currently assigned @SecretVersionStage@ staging
-- labels and the @SecretVersionId@ attached to each one. Staging labels
-- are used to keep track of the different versions during the rotation
-- process.
--
-- A version that does not have any @SecretVersionStage@ is considered
-- deprecated and subject to deletion. Such versions are not included in
-- this list.
--
-- 'tags', 'secretListEntry_tags' - The list of user-defined tags associated with the secret. To add tags to
-- a secret, use
-- <https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_TagResource.html TagResource>
-- . To remove tags, use
-- <https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_UntagResource.html UntagResource>
-- .
newSecretListEntry ::
  SecretListEntry
newSecretListEntry =
  SecretListEntry'
    { arn = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      deletedDate = Prelude.Nothing,
      description = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      lastAccessedDate = Prelude.Nothing,
      lastChangedDate = Prelude.Nothing,
      lastRotatedDate = Prelude.Nothing,
      name = Prelude.Nothing,
      owningService = Prelude.Nothing,
      primaryRegion = Prelude.Nothing,
      rotationEnabled = Prelude.Nothing,
      rotationLambdaARN = Prelude.Nothing,
      rotationRules = Prelude.Nothing,
      secretVersionsToStages = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the secret.
secretListEntry_arn :: Lens.Lens' SecretListEntry (Prelude.Maybe Prelude.Text)
secretListEntry_arn = Lens.lens (\SecretListEntry' {arn} -> arn) (\s@SecretListEntry' {} a -> s {arn = a} :: SecretListEntry)

-- | The date and time when a secret was created.
secretListEntry_createdDate :: Lens.Lens' SecretListEntry (Prelude.Maybe Prelude.UTCTime)
secretListEntry_createdDate = Lens.lens (\SecretListEntry' {createdDate} -> createdDate) (\s@SecretListEntry' {} a -> s {createdDate = a} :: SecretListEntry) Prelude.. Lens.mapping Data._Time

-- | The date and time the deletion of the secret occurred. Not present on
-- active secrets. The secret can be recovered until the number of days in
-- the recovery window has passed, as specified in the
-- @RecoveryWindowInDays@ parameter of the
-- <https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_DeleteSecret.html DeleteSecret>
-- operation.
secretListEntry_deletedDate :: Lens.Lens' SecretListEntry (Prelude.Maybe Prelude.UTCTime)
secretListEntry_deletedDate = Lens.lens (\SecretListEntry' {deletedDate} -> deletedDate) (\s@SecretListEntry' {} a -> s {deletedDate = a} :: SecretListEntry) Prelude.. Lens.mapping Data._Time

-- | The user-provided description of the secret.
secretListEntry_description :: Lens.Lens' SecretListEntry (Prelude.Maybe Prelude.Text)
secretListEntry_description = Lens.lens (\SecretListEntry' {description} -> description) (\s@SecretListEntry' {} a -> s {description = a} :: SecretListEntry)

-- | The ARN of the KMS key that Secrets Manager uses to encrypt the secret
-- value. If the secret is encrypted with the Amazon Web Services managed
-- key @aws\/secretsmanager@, this field is omitted.
secretListEntry_kmsKeyId :: Lens.Lens' SecretListEntry (Prelude.Maybe Prelude.Text)
secretListEntry_kmsKeyId = Lens.lens (\SecretListEntry' {kmsKeyId} -> kmsKeyId) (\s@SecretListEntry' {} a -> s {kmsKeyId = a} :: SecretListEntry)

-- | The date that the secret was last accessed in the Region. This field is
-- omitted if the secret has never been retrieved in the Region.
secretListEntry_lastAccessedDate :: Lens.Lens' SecretListEntry (Prelude.Maybe Prelude.UTCTime)
secretListEntry_lastAccessedDate = Lens.lens (\SecretListEntry' {lastAccessedDate} -> lastAccessedDate) (\s@SecretListEntry' {} a -> s {lastAccessedDate = a} :: SecretListEntry) Prelude.. Lens.mapping Data._Time

-- | The last date and time that this secret was modified in any way.
secretListEntry_lastChangedDate :: Lens.Lens' SecretListEntry (Prelude.Maybe Prelude.UTCTime)
secretListEntry_lastChangedDate = Lens.lens (\SecretListEntry' {lastChangedDate} -> lastChangedDate) (\s@SecretListEntry' {} a -> s {lastChangedDate = a} :: SecretListEntry) Prelude.. Lens.mapping Data._Time

-- | The most recent date and time that the Secrets Manager rotation process
-- was successfully completed. This value is null if the secret hasn\'t
-- ever rotated.
secretListEntry_lastRotatedDate :: Lens.Lens' SecretListEntry (Prelude.Maybe Prelude.UTCTime)
secretListEntry_lastRotatedDate = Lens.lens (\SecretListEntry' {lastRotatedDate} -> lastRotatedDate) (\s@SecretListEntry' {} a -> s {lastRotatedDate = a} :: SecretListEntry) Prelude.. Lens.mapping Data._Time

-- | The friendly name of the secret. You can use forward slashes in the name
-- to represent a path hierarchy. For example,
-- @\/prod\/databases\/dbserver1@ could represent the secret for a server
-- named @dbserver1@ in the folder @databases@ in the folder @prod@.
secretListEntry_name :: Lens.Lens' SecretListEntry (Prelude.Maybe Prelude.Text)
secretListEntry_name = Lens.lens (\SecretListEntry' {name} -> name) (\s@SecretListEntry' {} a -> s {name = a} :: SecretListEntry)

-- | Returns the name of the service that created the secret.
secretListEntry_owningService :: Lens.Lens' SecretListEntry (Prelude.Maybe Prelude.Text)
secretListEntry_owningService = Lens.lens (\SecretListEntry' {owningService} -> owningService) (\s@SecretListEntry' {} a -> s {owningService = a} :: SecretListEntry)

-- | The Region where Secrets Manager originated the secret.
secretListEntry_primaryRegion :: Lens.Lens' SecretListEntry (Prelude.Maybe Prelude.Text)
secretListEntry_primaryRegion = Lens.lens (\SecretListEntry' {primaryRegion} -> primaryRegion) (\s@SecretListEntry' {} a -> s {primaryRegion = a} :: SecretListEntry)

-- | Indicates whether automatic, scheduled rotation is enabled for this
-- secret.
secretListEntry_rotationEnabled :: Lens.Lens' SecretListEntry (Prelude.Maybe Prelude.Bool)
secretListEntry_rotationEnabled = Lens.lens (\SecretListEntry' {rotationEnabled} -> rotationEnabled) (\s@SecretListEntry' {} a -> s {rotationEnabled = a} :: SecretListEntry)

-- | The ARN of an Amazon Web Services Lambda function invoked by Secrets
-- Manager to rotate and expire the secret either automatically per the
-- schedule or manually by a call to
-- <https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_RotateSecret.html RotateSecret>
-- .
secretListEntry_rotationLambdaARN :: Lens.Lens' SecretListEntry (Prelude.Maybe Prelude.Text)
secretListEntry_rotationLambdaARN = Lens.lens (\SecretListEntry' {rotationLambdaARN} -> rotationLambdaARN) (\s@SecretListEntry' {} a -> s {rotationLambdaARN = a} :: SecretListEntry)

-- | A structure that defines the rotation configuration for the secret.
secretListEntry_rotationRules :: Lens.Lens' SecretListEntry (Prelude.Maybe RotationRulesType)
secretListEntry_rotationRules = Lens.lens (\SecretListEntry' {rotationRules} -> rotationRules) (\s@SecretListEntry' {} a -> s {rotationRules = a} :: SecretListEntry)

-- | A list of all of the currently assigned @SecretVersionStage@ staging
-- labels and the @SecretVersionId@ attached to each one. Staging labels
-- are used to keep track of the different versions during the rotation
-- process.
--
-- A version that does not have any @SecretVersionStage@ is considered
-- deprecated and subject to deletion. Such versions are not included in
-- this list.
secretListEntry_secretVersionsToStages :: Lens.Lens' SecretListEntry (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)))
secretListEntry_secretVersionsToStages = Lens.lens (\SecretListEntry' {secretVersionsToStages} -> secretVersionsToStages) (\s@SecretListEntry' {} a -> s {secretVersionsToStages = a} :: SecretListEntry) Prelude.. Lens.mapping Lens.coerced

-- | The list of user-defined tags associated with the secret. To add tags to
-- a secret, use
-- <https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_TagResource.html TagResource>
-- . To remove tags, use
-- <https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_UntagResource.html UntagResource>
-- .
secretListEntry_tags :: Lens.Lens' SecretListEntry (Prelude.Maybe [Tag])
secretListEntry_tags = Lens.lens (\SecretListEntry' {tags} -> tags) (\s@SecretListEntry' {} a -> s {tags = a} :: SecretListEntry) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON SecretListEntry where
  parseJSON =
    Data.withObject
      "SecretListEntry"
      ( \x ->
          SecretListEntry'
            Prelude.<$> (x Data..:? "ARN")
            Prelude.<*> (x Data..:? "CreatedDate")
            Prelude.<*> (x Data..:? "DeletedDate")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "LastAccessedDate")
            Prelude.<*> (x Data..:? "LastChangedDate")
            Prelude.<*> (x Data..:? "LastRotatedDate")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "OwningService")
            Prelude.<*> (x Data..:? "PrimaryRegion")
            Prelude.<*> (x Data..:? "RotationEnabled")
            Prelude.<*> (x Data..:? "RotationLambdaARN")
            Prelude.<*> (x Data..:? "RotationRules")
            Prelude.<*> ( x Data..:? "SecretVersionsToStages"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable SecretListEntry where
  hashWithSalt _salt SecretListEntry' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` deletedDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` lastAccessedDate
      `Prelude.hashWithSalt` lastChangedDate
      `Prelude.hashWithSalt` lastRotatedDate
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` owningService
      `Prelude.hashWithSalt` primaryRegion
      `Prelude.hashWithSalt` rotationEnabled
      `Prelude.hashWithSalt` rotationLambdaARN
      `Prelude.hashWithSalt` rotationRules
      `Prelude.hashWithSalt` secretVersionsToStages
      `Prelude.hashWithSalt` tags

instance Prelude.NFData SecretListEntry where
  rnf SecretListEntry' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf deletedDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf lastAccessedDate
      `Prelude.seq` Prelude.rnf lastChangedDate
      `Prelude.seq` Prelude.rnf lastRotatedDate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf owningService
      `Prelude.seq` Prelude.rnf primaryRegion
      `Prelude.seq` Prelude.rnf rotationEnabled
      `Prelude.seq` Prelude.rnf rotationLambdaARN
      `Prelude.seq` Prelude.rnf rotationRules
      `Prelude.seq` Prelude.rnf secretVersionsToStages
      `Prelude.seq` Prelude.rnf tags
