{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.DescribeSecret
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of a secret. It does not include the encrypted
-- fields. Secrets Manager only returns fields populated with a value in
-- the response.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   secretsmanager:DescribeSecret
--
-- __Related operations__
--
-- -   To create a secret, use CreateSecret.
--
-- -   To modify a secret, use UpdateSecret.
--
-- -   To retrieve the encrypted secret information in a version of the
--     secret, use GetSecretValue.
--
-- -   To list all of the secrets in the AWS account, use ListSecrets.
module Network.AWS.SecretsManager.DescribeSecret
  ( -- * Creating a Request
    DescribeSecret (..),
    newDescribeSecret,

    -- * Request Lenses
    describeSecret_secretId,

    -- * Destructuring the Response
    DescribeSecretResponse (..),
    newDescribeSecretResponse,

    -- * Response Lenses
    describeSecretResponse_createdDate,
    describeSecretResponse_owningService,
    describeSecretResponse_lastRotatedDate,
    describeSecretResponse_replicationStatus,
    describeSecretResponse_arn,
    describeSecretResponse_kmsKeyId,
    describeSecretResponse_name,
    describeSecretResponse_lastChangedDate,
    describeSecretResponse_primaryRegion,
    describeSecretResponse_rotationRules,
    describeSecretResponse_tags,
    describeSecretResponse_rotationEnabled,
    describeSecretResponse_deletedDate,
    describeSecretResponse_rotationLambdaARN,
    describeSecretResponse_description,
    describeSecretResponse_lastAccessedDate,
    describeSecretResponse_versionIdsToStages,
    describeSecretResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SecretsManager.Types

-- | /See:/ 'newDescribeSecret' smart constructor.
data DescribeSecret = DescribeSecret'
  { -- | The identifier of the secret whose details you want to retrieve. You can
    -- specify either the Amazon Resource Name (ARN) or the friendly name of
    -- the secret.
    --
    -- If you specify an ARN, we generally recommend that you specify a
    -- complete ARN. You can specify a partial ARN too—for example, if you
    -- don’t include the final hyphen and six random characters that Secrets
    -- Manager adds at the end of the ARN when you created the secret. A
    -- partial ARN match can work as long as it uniquely matches only one
    -- secret. However, if your secret has a name that ends in a hyphen
    -- followed by six characters (before Secrets Manager adds the hyphen and
    -- six characters to the ARN) and you try to use that as a partial ARN,
    -- then those characters cause Secrets Manager to assume that you’re
    -- specifying a complete ARN. This confusion can cause unexpected results.
    -- To avoid this situation, we recommend that you don’t create secret names
    -- ending with a hyphen followed by six characters.
    --
    -- If you specify an incomplete ARN without the random suffix, and instead
    -- provide the \'friendly name\', you /must/ not include the random suffix.
    -- If you do include the random suffix added by Secrets Manager, you
    -- receive either a /ResourceNotFoundException/ or an
    -- /AccessDeniedException/ error, depending on your permissions.
    secretId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSecret' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretId', 'describeSecret_secretId' - The identifier of the secret whose details you want to retrieve. You can
-- specify either the Amazon Resource Name (ARN) or the friendly name of
-- the secret.
--
-- If you specify an ARN, we generally recommend that you specify a
-- complete ARN. You can specify a partial ARN too—for example, if you
-- don’t include the final hyphen and six random characters that Secrets
-- Manager adds at the end of the ARN when you created the secret. A
-- partial ARN match can work as long as it uniquely matches only one
-- secret. However, if your secret has a name that ends in a hyphen
-- followed by six characters (before Secrets Manager adds the hyphen and
-- six characters to the ARN) and you try to use that as a partial ARN,
-- then those characters cause Secrets Manager to assume that you’re
-- specifying a complete ARN. This confusion can cause unexpected results.
-- To avoid this situation, we recommend that you don’t create secret names
-- ending with a hyphen followed by six characters.
--
-- If you specify an incomplete ARN without the random suffix, and instead
-- provide the \'friendly name\', you /must/ not include the random suffix.
-- If you do include the random suffix added by Secrets Manager, you
-- receive either a /ResourceNotFoundException/ or an
-- /AccessDeniedException/ error, depending on your permissions.
newDescribeSecret ::
  -- | 'secretId'
  Core.Text ->
  DescribeSecret
newDescribeSecret pSecretId_ =
  DescribeSecret' {secretId = pSecretId_}

-- | The identifier of the secret whose details you want to retrieve. You can
-- specify either the Amazon Resource Name (ARN) or the friendly name of
-- the secret.
--
-- If you specify an ARN, we generally recommend that you specify a
-- complete ARN. You can specify a partial ARN too—for example, if you
-- don’t include the final hyphen and six random characters that Secrets
-- Manager adds at the end of the ARN when you created the secret. A
-- partial ARN match can work as long as it uniquely matches only one
-- secret. However, if your secret has a name that ends in a hyphen
-- followed by six characters (before Secrets Manager adds the hyphen and
-- six characters to the ARN) and you try to use that as a partial ARN,
-- then those characters cause Secrets Manager to assume that you’re
-- specifying a complete ARN. This confusion can cause unexpected results.
-- To avoid this situation, we recommend that you don’t create secret names
-- ending with a hyphen followed by six characters.
--
-- If you specify an incomplete ARN without the random suffix, and instead
-- provide the \'friendly name\', you /must/ not include the random suffix.
-- If you do include the random suffix added by Secrets Manager, you
-- receive either a /ResourceNotFoundException/ or an
-- /AccessDeniedException/ error, depending on your permissions.
describeSecret_secretId :: Lens.Lens' DescribeSecret Core.Text
describeSecret_secretId = Lens.lens (\DescribeSecret' {secretId} -> secretId) (\s@DescribeSecret' {} a -> s {secretId = a} :: DescribeSecret)

instance Core.AWSRequest DescribeSecret where
  type
    AWSResponse DescribeSecret =
      DescribeSecretResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSecretResponse'
            Core.<$> (x Core..?> "CreatedDate")
            Core.<*> (x Core..?> "OwningService")
            Core.<*> (x Core..?> "LastRotatedDate")
            Core.<*> (x Core..?> "ReplicationStatus" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "ARN")
            Core.<*> (x Core..?> "KmsKeyId")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "LastChangedDate")
            Core.<*> (x Core..?> "PrimaryRegion")
            Core.<*> (x Core..?> "RotationRules")
            Core.<*> (x Core..?> "Tags" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "RotationEnabled")
            Core.<*> (x Core..?> "DeletedDate")
            Core.<*> (x Core..?> "RotationLambdaARN")
            Core.<*> (x Core..?> "Description")
            Core.<*> (x Core..?> "LastAccessedDate")
            Core.<*> ( x Core..?> "VersionIdsToStages"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeSecret

instance Core.NFData DescribeSecret

instance Core.ToHeaders DescribeSecret where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("secretsmanager.DescribeSecret" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeSecret where
  toJSON DescribeSecret' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("SecretId" Core..= secretId)]
      )

instance Core.ToPath DescribeSecret where
  toPath = Core.const "/"

instance Core.ToQuery DescribeSecret where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeSecretResponse' smart constructor.
data DescribeSecretResponse = DescribeSecretResponse'
  { -- | The date you created the secret.
    createdDate :: Core.Maybe Core.POSIX,
    -- | Returns the name of the service that created this secret.
    owningService :: Core.Maybe Core.Text,
    -- | The last date and time that the rotation process for this secret was
    -- invoked.
    --
    -- The most recent date and time that the Secrets Manager rotation process
    -- successfully completed. If the secret doesn\'t rotate, Secrets Manager
    -- returns a null value.
    lastRotatedDate :: Core.Maybe Core.POSIX,
    -- | Describes a list of replication status objects as @InProgress@, @Failed@
    -- or @InSync@.@P@
    replicationStatus :: Core.Maybe [ReplicationStatusType],
    -- | The ARN of the secret.
    arn :: Core.Maybe Core.Text,
    -- | The ARN or alias of the AWS KMS customer master key (CMK) that\'s used
    -- to encrypt the @SecretString@ or @SecretBinary@ fields in each version
    -- of the secret. If you don\'t provide a key, then Secrets Manager
    -- defaults to encrypting the secret fields with the default AWS KMS CMK
    -- (the one named @awssecretsmanager@) for this account.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | The user-provided friendly name of the secret.
    name :: Core.Maybe Core.Text,
    -- | The last date and time that this secret was modified in any way.
    lastChangedDate :: Core.Maybe Core.POSIX,
    -- | Specifies the primary region for secret replication.
    primaryRegion :: Core.Maybe Core.Text,
    -- | A structure with the rotation configuration for this secret.
    rotationRules :: Core.Maybe RotationRulesType,
    -- | The list of user-defined tags that are associated with the secret. To
    -- add tags to a secret, use TagResource. To remove tags, use
    -- UntagResource.
    tags :: Core.Maybe [Tag],
    -- | Specifies whether automatic rotation is enabled for this secret.
    --
    -- To enable rotation, use RotateSecret with @AutomaticallyRotateAfterDays@
    -- set to a value greater than 0. To disable rotation, use
    -- CancelRotateSecret.
    rotationEnabled :: Core.Maybe Core.Bool,
    -- | This value exists if the secret is scheduled for deletion. Some time
    -- after the specified date and time, Secrets Manager deletes the secret
    -- and all of its versions.
    --
    -- If a secret is scheduled for deletion, then its details, including the
    -- encrypted secret information, is not accessible. To cancel a scheduled
    -- deletion and restore access, use RestoreSecret.
    deletedDate :: Core.Maybe Core.POSIX,
    -- | The ARN of a Lambda function that\'s invoked by Secrets Manager to
    -- rotate the secret either automatically per the schedule or manually by a
    -- call to @RotateSecret@.
    rotationLambdaARN :: Core.Maybe Core.Text,
    -- | The user-provided description of the secret.
    description :: Core.Maybe Core.Text,
    -- | The last date that this secret was accessed. This value is truncated to
    -- midnight of the date and therefore shows only the date, not the time.
    lastAccessedDate :: Core.Maybe Core.POSIX,
    -- | A list of all of the currently assigned @VersionStage@ staging labels
    -- and the @VersionId@ that each is attached to. Staging labels are used to
    -- keep track of the different versions during the rotation process.
    --
    -- A version that does not have any staging labels attached is considered
    -- deprecated and subject to deletion. Such versions are not included in
    -- this list.
    versionIdsToStages :: Core.Maybe (Core.HashMap Core.Text (Core.NonEmpty Core.Text)),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSecretResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'describeSecretResponse_createdDate' - The date you created the secret.
--
-- 'owningService', 'describeSecretResponse_owningService' - Returns the name of the service that created this secret.
--
-- 'lastRotatedDate', 'describeSecretResponse_lastRotatedDate' - The last date and time that the rotation process for this secret was
-- invoked.
--
-- The most recent date and time that the Secrets Manager rotation process
-- successfully completed. If the secret doesn\'t rotate, Secrets Manager
-- returns a null value.
--
-- 'replicationStatus', 'describeSecretResponse_replicationStatus' - Describes a list of replication status objects as @InProgress@, @Failed@
-- or @InSync@.@P@
--
-- 'arn', 'describeSecretResponse_arn' - The ARN of the secret.
--
-- 'kmsKeyId', 'describeSecretResponse_kmsKeyId' - The ARN or alias of the AWS KMS customer master key (CMK) that\'s used
-- to encrypt the @SecretString@ or @SecretBinary@ fields in each version
-- of the secret. If you don\'t provide a key, then Secrets Manager
-- defaults to encrypting the secret fields with the default AWS KMS CMK
-- (the one named @awssecretsmanager@) for this account.
--
-- 'name', 'describeSecretResponse_name' - The user-provided friendly name of the secret.
--
-- 'lastChangedDate', 'describeSecretResponse_lastChangedDate' - The last date and time that this secret was modified in any way.
--
-- 'primaryRegion', 'describeSecretResponse_primaryRegion' - Specifies the primary region for secret replication.
--
-- 'rotationRules', 'describeSecretResponse_rotationRules' - A structure with the rotation configuration for this secret.
--
-- 'tags', 'describeSecretResponse_tags' - The list of user-defined tags that are associated with the secret. To
-- add tags to a secret, use TagResource. To remove tags, use
-- UntagResource.
--
-- 'rotationEnabled', 'describeSecretResponse_rotationEnabled' - Specifies whether automatic rotation is enabled for this secret.
--
-- To enable rotation, use RotateSecret with @AutomaticallyRotateAfterDays@
-- set to a value greater than 0. To disable rotation, use
-- CancelRotateSecret.
--
-- 'deletedDate', 'describeSecretResponse_deletedDate' - This value exists if the secret is scheduled for deletion. Some time
-- after the specified date and time, Secrets Manager deletes the secret
-- and all of its versions.
--
-- If a secret is scheduled for deletion, then its details, including the
-- encrypted secret information, is not accessible. To cancel a scheduled
-- deletion and restore access, use RestoreSecret.
--
-- 'rotationLambdaARN', 'describeSecretResponse_rotationLambdaARN' - The ARN of a Lambda function that\'s invoked by Secrets Manager to
-- rotate the secret either automatically per the schedule or manually by a
-- call to @RotateSecret@.
--
-- 'description', 'describeSecretResponse_description' - The user-provided description of the secret.
--
-- 'lastAccessedDate', 'describeSecretResponse_lastAccessedDate' - The last date that this secret was accessed. This value is truncated to
-- midnight of the date and therefore shows only the date, not the time.
--
-- 'versionIdsToStages', 'describeSecretResponse_versionIdsToStages' - A list of all of the currently assigned @VersionStage@ staging labels
-- and the @VersionId@ that each is attached to. Staging labels are used to
-- keep track of the different versions during the rotation process.
--
-- A version that does not have any staging labels attached is considered
-- deprecated and subject to deletion. Such versions are not included in
-- this list.
--
-- 'httpStatus', 'describeSecretResponse_httpStatus' - The response's http status code.
newDescribeSecretResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeSecretResponse
newDescribeSecretResponse pHttpStatus_ =
  DescribeSecretResponse'
    { createdDate = Core.Nothing,
      owningService = Core.Nothing,
      lastRotatedDate = Core.Nothing,
      replicationStatus = Core.Nothing,
      arn = Core.Nothing,
      kmsKeyId = Core.Nothing,
      name = Core.Nothing,
      lastChangedDate = Core.Nothing,
      primaryRegion = Core.Nothing,
      rotationRules = Core.Nothing,
      tags = Core.Nothing,
      rotationEnabled = Core.Nothing,
      deletedDate = Core.Nothing,
      rotationLambdaARN = Core.Nothing,
      description = Core.Nothing,
      lastAccessedDate = Core.Nothing,
      versionIdsToStages = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date you created the secret.
describeSecretResponse_createdDate :: Lens.Lens' DescribeSecretResponse (Core.Maybe Core.UTCTime)
describeSecretResponse_createdDate = Lens.lens (\DescribeSecretResponse' {createdDate} -> createdDate) (\s@DescribeSecretResponse' {} a -> s {createdDate = a} :: DescribeSecretResponse) Core.. Lens.mapping Core._Time

-- | Returns the name of the service that created this secret.
describeSecretResponse_owningService :: Lens.Lens' DescribeSecretResponse (Core.Maybe Core.Text)
describeSecretResponse_owningService = Lens.lens (\DescribeSecretResponse' {owningService} -> owningService) (\s@DescribeSecretResponse' {} a -> s {owningService = a} :: DescribeSecretResponse)

-- | The last date and time that the rotation process for this secret was
-- invoked.
--
-- The most recent date and time that the Secrets Manager rotation process
-- successfully completed. If the secret doesn\'t rotate, Secrets Manager
-- returns a null value.
describeSecretResponse_lastRotatedDate :: Lens.Lens' DescribeSecretResponse (Core.Maybe Core.UTCTime)
describeSecretResponse_lastRotatedDate = Lens.lens (\DescribeSecretResponse' {lastRotatedDate} -> lastRotatedDate) (\s@DescribeSecretResponse' {} a -> s {lastRotatedDate = a} :: DescribeSecretResponse) Core.. Lens.mapping Core._Time

-- | Describes a list of replication status objects as @InProgress@, @Failed@
-- or @InSync@.@P@
describeSecretResponse_replicationStatus :: Lens.Lens' DescribeSecretResponse (Core.Maybe [ReplicationStatusType])
describeSecretResponse_replicationStatus = Lens.lens (\DescribeSecretResponse' {replicationStatus} -> replicationStatus) (\s@DescribeSecretResponse' {} a -> s {replicationStatus = a} :: DescribeSecretResponse) Core.. Lens.mapping Lens._Coerce

-- | The ARN of the secret.
describeSecretResponse_arn :: Lens.Lens' DescribeSecretResponse (Core.Maybe Core.Text)
describeSecretResponse_arn = Lens.lens (\DescribeSecretResponse' {arn} -> arn) (\s@DescribeSecretResponse' {} a -> s {arn = a} :: DescribeSecretResponse)

-- | The ARN or alias of the AWS KMS customer master key (CMK) that\'s used
-- to encrypt the @SecretString@ or @SecretBinary@ fields in each version
-- of the secret. If you don\'t provide a key, then Secrets Manager
-- defaults to encrypting the secret fields with the default AWS KMS CMK
-- (the one named @awssecretsmanager@) for this account.
describeSecretResponse_kmsKeyId :: Lens.Lens' DescribeSecretResponse (Core.Maybe Core.Text)
describeSecretResponse_kmsKeyId = Lens.lens (\DescribeSecretResponse' {kmsKeyId} -> kmsKeyId) (\s@DescribeSecretResponse' {} a -> s {kmsKeyId = a} :: DescribeSecretResponse)

-- | The user-provided friendly name of the secret.
describeSecretResponse_name :: Lens.Lens' DescribeSecretResponse (Core.Maybe Core.Text)
describeSecretResponse_name = Lens.lens (\DescribeSecretResponse' {name} -> name) (\s@DescribeSecretResponse' {} a -> s {name = a} :: DescribeSecretResponse)

-- | The last date and time that this secret was modified in any way.
describeSecretResponse_lastChangedDate :: Lens.Lens' DescribeSecretResponse (Core.Maybe Core.UTCTime)
describeSecretResponse_lastChangedDate = Lens.lens (\DescribeSecretResponse' {lastChangedDate} -> lastChangedDate) (\s@DescribeSecretResponse' {} a -> s {lastChangedDate = a} :: DescribeSecretResponse) Core.. Lens.mapping Core._Time

-- | Specifies the primary region for secret replication.
describeSecretResponse_primaryRegion :: Lens.Lens' DescribeSecretResponse (Core.Maybe Core.Text)
describeSecretResponse_primaryRegion = Lens.lens (\DescribeSecretResponse' {primaryRegion} -> primaryRegion) (\s@DescribeSecretResponse' {} a -> s {primaryRegion = a} :: DescribeSecretResponse)

-- | A structure with the rotation configuration for this secret.
describeSecretResponse_rotationRules :: Lens.Lens' DescribeSecretResponse (Core.Maybe RotationRulesType)
describeSecretResponse_rotationRules = Lens.lens (\DescribeSecretResponse' {rotationRules} -> rotationRules) (\s@DescribeSecretResponse' {} a -> s {rotationRules = a} :: DescribeSecretResponse)

-- | The list of user-defined tags that are associated with the secret. To
-- add tags to a secret, use TagResource. To remove tags, use
-- UntagResource.
describeSecretResponse_tags :: Lens.Lens' DescribeSecretResponse (Core.Maybe [Tag])
describeSecretResponse_tags = Lens.lens (\DescribeSecretResponse' {tags} -> tags) (\s@DescribeSecretResponse' {} a -> s {tags = a} :: DescribeSecretResponse) Core.. Lens.mapping Lens._Coerce

-- | Specifies whether automatic rotation is enabled for this secret.
--
-- To enable rotation, use RotateSecret with @AutomaticallyRotateAfterDays@
-- set to a value greater than 0. To disable rotation, use
-- CancelRotateSecret.
describeSecretResponse_rotationEnabled :: Lens.Lens' DescribeSecretResponse (Core.Maybe Core.Bool)
describeSecretResponse_rotationEnabled = Lens.lens (\DescribeSecretResponse' {rotationEnabled} -> rotationEnabled) (\s@DescribeSecretResponse' {} a -> s {rotationEnabled = a} :: DescribeSecretResponse)

-- | This value exists if the secret is scheduled for deletion. Some time
-- after the specified date and time, Secrets Manager deletes the secret
-- and all of its versions.
--
-- If a secret is scheduled for deletion, then its details, including the
-- encrypted secret information, is not accessible. To cancel a scheduled
-- deletion and restore access, use RestoreSecret.
describeSecretResponse_deletedDate :: Lens.Lens' DescribeSecretResponse (Core.Maybe Core.UTCTime)
describeSecretResponse_deletedDate = Lens.lens (\DescribeSecretResponse' {deletedDate} -> deletedDate) (\s@DescribeSecretResponse' {} a -> s {deletedDate = a} :: DescribeSecretResponse) Core.. Lens.mapping Core._Time

-- | The ARN of a Lambda function that\'s invoked by Secrets Manager to
-- rotate the secret either automatically per the schedule or manually by a
-- call to @RotateSecret@.
describeSecretResponse_rotationLambdaARN :: Lens.Lens' DescribeSecretResponse (Core.Maybe Core.Text)
describeSecretResponse_rotationLambdaARN = Lens.lens (\DescribeSecretResponse' {rotationLambdaARN} -> rotationLambdaARN) (\s@DescribeSecretResponse' {} a -> s {rotationLambdaARN = a} :: DescribeSecretResponse)

-- | The user-provided description of the secret.
describeSecretResponse_description :: Lens.Lens' DescribeSecretResponse (Core.Maybe Core.Text)
describeSecretResponse_description = Lens.lens (\DescribeSecretResponse' {description} -> description) (\s@DescribeSecretResponse' {} a -> s {description = a} :: DescribeSecretResponse)

-- | The last date that this secret was accessed. This value is truncated to
-- midnight of the date and therefore shows only the date, not the time.
describeSecretResponse_lastAccessedDate :: Lens.Lens' DescribeSecretResponse (Core.Maybe Core.UTCTime)
describeSecretResponse_lastAccessedDate = Lens.lens (\DescribeSecretResponse' {lastAccessedDate} -> lastAccessedDate) (\s@DescribeSecretResponse' {} a -> s {lastAccessedDate = a} :: DescribeSecretResponse) Core.. Lens.mapping Core._Time

-- | A list of all of the currently assigned @VersionStage@ staging labels
-- and the @VersionId@ that each is attached to. Staging labels are used to
-- keep track of the different versions during the rotation process.
--
-- A version that does not have any staging labels attached is considered
-- deprecated and subject to deletion. Such versions are not included in
-- this list.
describeSecretResponse_versionIdsToStages :: Lens.Lens' DescribeSecretResponse (Core.Maybe (Core.HashMap Core.Text (Core.NonEmpty Core.Text)))
describeSecretResponse_versionIdsToStages = Lens.lens (\DescribeSecretResponse' {versionIdsToStages} -> versionIdsToStages) (\s@DescribeSecretResponse' {} a -> s {versionIdsToStages = a} :: DescribeSecretResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeSecretResponse_httpStatus :: Lens.Lens' DescribeSecretResponse Core.Int
describeSecretResponse_httpStatus = Lens.lens (\DescribeSecretResponse' {httpStatus} -> httpStatus) (\s@DescribeSecretResponse' {} a -> s {httpStatus = a} :: DescribeSecretResponse)

instance Core.NFData DescribeSecretResponse
