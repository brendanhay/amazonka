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
-- Module      : Amazonka.SecretsManager.DescribeSecret
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
-- -   To list all of the secrets in the Amazon Web Services account, use
--     ListSecrets.
module Amazonka.SecretsManager.DescribeSecret
  ( -- * Creating a Request
    DescribeSecret (..),
    newDescribeSecret,

    -- * Request Lenses
    describeSecret_secretId,

    -- * Destructuring the Response
    DescribeSecretResponse (..),
    newDescribeSecretResponse,

    -- * Response Lenses
    describeSecretResponse_lastChangedDate,
    describeSecretResponse_primaryRegion,
    describeSecretResponse_arn,
    describeSecretResponse_rotationRules,
    describeSecretResponse_deletedDate,
    describeSecretResponse_rotationEnabled,
    describeSecretResponse_createdDate,
    describeSecretResponse_kmsKeyId,
    describeSecretResponse_name,
    describeSecretResponse_versionIdsToStages,
    describeSecretResponse_replicationStatus,
    describeSecretResponse_owningService,
    describeSecretResponse_lastRotatedDate,
    describeSecretResponse_lastAccessedDate,
    describeSecretResponse_description,
    describeSecretResponse_rotationLambdaARN,
    describeSecretResponse_tags,
    describeSecretResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newDescribeSecret' smart constructor.
data DescribeSecret = DescribeSecret'
  { -- | The identifier of the secret whose details you want to retrieve. You can
    -- specify either the Amazon Resource Name (ARN) or the friendly name of
    -- the secret.
    --
    -- For an ARN, we recommend that you specify a complete ARN rather than a
    -- partial ARN.
    secretId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN.
newDescribeSecret ::
  -- | 'secretId'
  Prelude.Text ->
  DescribeSecret
newDescribeSecret pSecretId_ =
  DescribeSecret' {secretId = pSecretId_}

-- | The identifier of the secret whose details you want to retrieve. You can
-- specify either the Amazon Resource Name (ARN) or the friendly name of
-- the secret.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN.
describeSecret_secretId :: Lens.Lens' DescribeSecret Prelude.Text
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
            Prelude.<$> (x Core..?> "LastChangedDate")
            Prelude.<*> (x Core..?> "PrimaryRegion")
            Prelude.<*> (x Core..?> "ARN")
            Prelude.<*> (x Core..?> "RotationRules")
            Prelude.<*> (x Core..?> "DeletedDate")
            Prelude.<*> (x Core..?> "RotationEnabled")
            Prelude.<*> (x Core..?> "CreatedDate")
            Prelude.<*> (x Core..?> "KmsKeyId")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> ( x Core..?> "VersionIdsToStages"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Core..?> "ReplicationStatus"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "OwningService")
            Prelude.<*> (x Core..?> "LastRotatedDate")
            Prelude.<*> (x Core..?> "LastAccessedDate")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "RotationLambdaARN")
            Prelude.<*> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSecret

instance Prelude.NFData DescribeSecret

instance Core.ToHeaders DescribeSecret where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "secretsmanager.DescribeSecret" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeSecret where
  toJSON DescribeSecret' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("SecretId" Core..= secretId)]
      )

instance Core.ToPath DescribeSecret where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeSecret where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSecretResponse' smart constructor.
data DescribeSecretResponse = DescribeSecretResponse'
  { -- | The last date and time that this secret was modified in any way.
    lastChangedDate :: Prelude.Maybe Core.POSIX,
    -- | Specifies the primary region for secret replication.
    primaryRegion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the secret.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A structure with the rotation configuration for this secret. This field
    -- is only populated if rotation is configured.
    rotationRules :: Prelude.Maybe RotationRulesType,
    -- | This value exists if the secret is scheduled for deletion. Some time
    -- after the specified date and time, Secrets Manager deletes the secret
    -- and all of its versions.
    --
    -- If a secret is scheduled for deletion, then its details, including the
    -- encrypted secret information, is not accessible. To cancel a scheduled
    -- deletion and restore access, use RestoreSecret.
    deletedDate :: Prelude.Maybe Core.POSIX,
    -- | Specifies whether automatic rotation is enabled for this secret.
    --
    -- To enable rotation, use RotateSecret with @AutomaticallyRotateAfterDays@
    -- set to a value greater than 0. To disable rotation, use
    -- CancelRotateSecret.
    rotationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The date you created the secret.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | The ARN or alias of the Amazon Web Services KMS customer master key
    -- (CMK) that\'s used to encrypt the @SecretString@ or @SecretBinary@
    -- fields in each version of the secret. If you don\'t provide a key, then
    -- Secrets Manager defaults to encrypting the secret fields with the
    -- default Amazon Web Services KMS CMK (the one named @awssecretsmanager@)
    -- for this account.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The user-provided friendly name of the secret.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of all of the currently assigned @VersionStage@ staging labels
    -- and the @VersionId@ that each is attached to. Staging labels are used to
    -- keep track of the different versions during the rotation process.
    --
    -- A version that does not have any staging labels attached is considered
    -- deprecated and subject to deletion. Such versions are not included in
    -- this list.
    versionIdsToStages :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)),
    -- | Describes a list of replication status objects as @InProgress@, @Failed@
    -- or @InSync@.@P@
    replicationStatus :: Prelude.Maybe [ReplicationStatusType],
    -- | Returns the name of the service that created this secret.
    owningService :: Prelude.Maybe Prelude.Text,
    -- | The last date and time that the rotation process for this secret was
    -- invoked.
    --
    -- The most recent date and time that the Secrets Manager rotation process
    -- successfully completed. If the secret doesn\'t rotate, Secrets Manager
    -- returns a null value.
    lastRotatedDate :: Prelude.Maybe Core.POSIX,
    -- | The last date that this secret was accessed. This value is truncated to
    -- midnight of the date and therefore shows only the date, not the time.
    lastAccessedDate :: Prelude.Maybe Core.POSIX,
    -- | The user-provided description of the secret.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a Lambda function that\'s invoked by Secrets Manager to
    -- rotate the secret either automatically per the schedule or manually by a
    -- call to @RotateSecret@.
    rotationLambdaARN :: Prelude.Maybe Prelude.Text,
    -- | The list of user-defined tags that are associated with the secret. To
    -- add tags to a secret, use TagResource. To remove tags, use
    -- UntagResource.
    tags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSecretResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastChangedDate', 'describeSecretResponse_lastChangedDate' - The last date and time that this secret was modified in any way.
--
-- 'primaryRegion', 'describeSecretResponse_primaryRegion' - Specifies the primary region for secret replication.
--
-- 'arn', 'describeSecretResponse_arn' - The ARN of the secret.
--
-- 'rotationRules', 'describeSecretResponse_rotationRules' - A structure with the rotation configuration for this secret. This field
-- is only populated if rotation is configured.
--
-- 'deletedDate', 'describeSecretResponse_deletedDate' - This value exists if the secret is scheduled for deletion. Some time
-- after the specified date and time, Secrets Manager deletes the secret
-- and all of its versions.
--
-- If a secret is scheduled for deletion, then its details, including the
-- encrypted secret information, is not accessible. To cancel a scheduled
-- deletion and restore access, use RestoreSecret.
--
-- 'rotationEnabled', 'describeSecretResponse_rotationEnabled' - Specifies whether automatic rotation is enabled for this secret.
--
-- To enable rotation, use RotateSecret with @AutomaticallyRotateAfterDays@
-- set to a value greater than 0. To disable rotation, use
-- CancelRotateSecret.
--
-- 'createdDate', 'describeSecretResponse_createdDate' - The date you created the secret.
--
-- 'kmsKeyId', 'describeSecretResponse_kmsKeyId' - The ARN or alias of the Amazon Web Services KMS customer master key
-- (CMK) that\'s used to encrypt the @SecretString@ or @SecretBinary@
-- fields in each version of the secret. If you don\'t provide a key, then
-- Secrets Manager defaults to encrypting the secret fields with the
-- default Amazon Web Services KMS CMK (the one named @awssecretsmanager@)
-- for this account.
--
-- 'name', 'describeSecretResponse_name' - The user-provided friendly name of the secret.
--
-- 'versionIdsToStages', 'describeSecretResponse_versionIdsToStages' - A list of all of the currently assigned @VersionStage@ staging labels
-- and the @VersionId@ that each is attached to. Staging labels are used to
-- keep track of the different versions during the rotation process.
--
-- A version that does not have any staging labels attached is considered
-- deprecated and subject to deletion. Such versions are not included in
-- this list.
--
-- 'replicationStatus', 'describeSecretResponse_replicationStatus' - Describes a list of replication status objects as @InProgress@, @Failed@
-- or @InSync@.@P@
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
-- 'lastAccessedDate', 'describeSecretResponse_lastAccessedDate' - The last date that this secret was accessed. This value is truncated to
-- midnight of the date and therefore shows only the date, not the time.
--
-- 'description', 'describeSecretResponse_description' - The user-provided description of the secret.
--
-- 'rotationLambdaARN', 'describeSecretResponse_rotationLambdaARN' - The ARN of a Lambda function that\'s invoked by Secrets Manager to
-- rotate the secret either automatically per the schedule or manually by a
-- call to @RotateSecret@.
--
-- 'tags', 'describeSecretResponse_tags' - The list of user-defined tags that are associated with the secret. To
-- add tags to a secret, use TagResource. To remove tags, use
-- UntagResource.
--
-- 'httpStatus', 'describeSecretResponse_httpStatus' - The response's http status code.
newDescribeSecretResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSecretResponse
newDescribeSecretResponse pHttpStatus_ =
  DescribeSecretResponse'
    { lastChangedDate =
        Prelude.Nothing,
      primaryRegion = Prelude.Nothing,
      arn = Prelude.Nothing,
      rotationRules = Prelude.Nothing,
      deletedDate = Prelude.Nothing,
      rotationEnabled = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      name = Prelude.Nothing,
      versionIdsToStages = Prelude.Nothing,
      replicationStatus = Prelude.Nothing,
      owningService = Prelude.Nothing,
      lastRotatedDate = Prelude.Nothing,
      lastAccessedDate = Prelude.Nothing,
      description = Prelude.Nothing,
      rotationLambdaARN = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The last date and time that this secret was modified in any way.
describeSecretResponse_lastChangedDate :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe Prelude.UTCTime)
describeSecretResponse_lastChangedDate = Lens.lens (\DescribeSecretResponse' {lastChangedDate} -> lastChangedDate) (\s@DescribeSecretResponse' {} a -> s {lastChangedDate = a} :: DescribeSecretResponse) Prelude.. Lens.mapping Core._Time

-- | Specifies the primary region for secret replication.
describeSecretResponse_primaryRegion :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe Prelude.Text)
describeSecretResponse_primaryRegion = Lens.lens (\DescribeSecretResponse' {primaryRegion} -> primaryRegion) (\s@DescribeSecretResponse' {} a -> s {primaryRegion = a} :: DescribeSecretResponse)

-- | The ARN of the secret.
describeSecretResponse_arn :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe Prelude.Text)
describeSecretResponse_arn = Lens.lens (\DescribeSecretResponse' {arn} -> arn) (\s@DescribeSecretResponse' {} a -> s {arn = a} :: DescribeSecretResponse)

-- | A structure with the rotation configuration for this secret. This field
-- is only populated if rotation is configured.
describeSecretResponse_rotationRules :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe RotationRulesType)
describeSecretResponse_rotationRules = Lens.lens (\DescribeSecretResponse' {rotationRules} -> rotationRules) (\s@DescribeSecretResponse' {} a -> s {rotationRules = a} :: DescribeSecretResponse)

-- | This value exists if the secret is scheduled for deletion. Some time
-- after the specified date and time, Secrets Manager deletes the secret
-- and all of its versions.
--
-- If a secret is scheduled for deletion, then its details, including the
-- encrypted secret information, is not accessible. To cancel a scheduled
-- deletion and restore access, use RestoreSecret.
describeSecretResponse_deletedDate :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe Prelude.UTCTime)
describeSecretResponse_deletedDate = Lens.lens (\DescribeSecretResponse' {deletedDate} -> deletedDate) (\s@DescribeSecretResponse' {} a -> s {deletedDate = a} :: DescribeSecretResponse) Prelude.. Lens.mapping Core._Time

-- | Specifies whether automatic rotation is enabled for this secret.
--
-- To enable rotation, use RotateSecret with @AutomaticallyRotateAfterDays@
-- set to a value greater than 0. To disable rotation, use
-- CancelRotateSecret.
describeSecretResponse_rotationEnabled :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe Prelude.Bool)
describeSecretResponse_rotationEnabled = Lens.lens (\DescribeSecretResponse' {rotationEnabled} -> rotationEnabled) (\s@DescribeSecretResponse' {} a -> s {rotationEnabled = a} :: DescribeSecretResponse)

-- | The date you created the secret.
describeSecretResponse_createdDate :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe Prelude.UTCTime)
describeSecretResponse_createdDate = Lens.lens (\DescribeSecretResponse' {createdDate} -> createdDate) (\s@DescribeSecretResponse' {} a -> s {createdDate = a} :: DescribeSecretResponse) Prelude.. Lens.mapping Core._Time

-- | The ARN or alias of the Amazon Web Services KMS customer master key
-- (CMK) that\'s used to encrypt the @SecretString@ or @SecretBinary@
-- fields in each version of the secret. If you don\'t provide a key, then
-- Secrets Manager defaults to encrypting the secret fields with the
-- default Amazon Web Services KMS CMK (the one named @awssecretsmanager@)
-- for this account.
describeSecretResponse_kmsKeyId :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe Prelude.Text)
describeSecretResponse_kmsKeyId = Lens.lens (\DescribeSecretResponse' {kmsKeyId} -> kmsKeyId) (\s@DescribeSecretResponse' {} a -> s {kmsKeyId = a} :: DescribeSecretResponse)

-- | The user-provided friendly name of the secret.
describeSecretResponse_name :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe Prelude.Text)
describeSecretResponse_name = Lens.lens (\DescribeSecretResponse' {name} -> name) (\s@DescribeSecretResponse' {} a -> s {name = a} :: DescribeSecretResponse)

-- | A list of all of the currently assigned @VersionStage@ staging labels
-- and the @VersionId@ that each is attached to. Staging labels are used to
-- keep track of the different versions during the rotation process.
--
-- A version that does not have any staging labels attached is considered
-- deprecated and subject to deletion. Such versions are not included in
-- this list.
describeSecretResponse_versionIdsToStages :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)))
describeSecretResponse_versionIdsToStages = Lens.lens (\DescribeSecretResponse' {versionIdsToStages} -> versionIdsToStages) (\s@DescribeSecretResponse' {} a -> s {versionIdsToStages = a} :: DescribeSecretResponse) Prelude.. Lens.mapping Lens.coerced

-- | Describes a list of replication status objects as @InProgress@, @Failed@
-- or @InSync@.@P@
describeSecretResponse_replicationStatus :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe [ReplicationStatusType])
describeSecretResponse_replicationStatus = Lens.lens (\DescribeSecretResponse' {replicationStatus} -> replicationStatus) (\s@DescribeSecretResponse' {} a -> s {replicationStatus = a} :: DescribeSecretResponse) Prelude.. Lens.mapping Lens.coerced

-- | Returns the name of the service that created this secret.
describeSecretResponse_owningService :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe Prelude.Text)
describeSecretResponse_owningService = Lens.lens (\DescribeSecretResponse' {owningService} -> owningService) (\s@DescribeSecretResponse' {} a -> s {owningService = a} :: DescribeSecretResponse)

-- | The last date and time that the rotation process for this secret was
-- invoked.
--
-- The most recent date and time that the Secrets Manager rotation process
-- successfully completed. If the secret doesn\'t rotate, Secrets Manager
-- returns a null value.
describeSecretResponse_lastRotatedDate :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe Prelude.UTCTime)
describeSecretResponse_lastRotatedDate = Lens.lens (\DescribeSecretResponse' {lastRotatedDate} -> lastRotatedDate) (\s@DescribeSecretResponse' {} a -> s {lastRotatedDate = a} :: DescribeSecretResponse) Prelude.. Lens.mapping Core._Time

-- | The last date that this secret was accessed. This value is truncated to
-- midnight of the date and therefore shows only the date, not the time.
describeSecretResponse_lastAccessedDate :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe Prelude.UTCTime)
describeSecretResponse_lastAccessedDate = Lens.lens (\DescribeSecretResponse' {lastAccessedDate} -> lastAccessedDate) (\s@DescribeSecretResponse' {} a -> s {lastAccessedDate = a} :: DescribeSecretResponse) Prelude.. Lens.mapping Core._Time

-- | The user-provided description of the secret.
describeSecretResponse_description :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe Prelude.Text)
describeSecretResponse_description = Lens.lens (\DescribeSecretResponse' {description} -> description) (\s@DescribeSecretResponse' {} a -> s {description = a} :: DescribeSecretResponse)

-- | The ARN of a Lambda function that\'s invoked by Secrets Manager to
-- rotate the secret either automatically per the schedule or manually by a
-- call to @RotateSecret@.
describeSecretResponse_rotationLambdaARN :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe Prelude.Text)
describeSecretResponse_rotationLambdaARN = Lens.lens (\DescribeSecretResponse' {rotationLambdaARN} -> rotationLambdaARN) (\s@DescribeSecretResponse' {} a -> s {rotationLambdaARN = a} :: DescribeSecretResponse)

-- | The list of user-defined tags that are associated with the secret. To
-- add tags to a secret, use TagResource. To remove tags, use
-- UntagResource.
describeSecretResponse_tags :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe [Tag])
describeSecretResponse_tags = Lens.lens (\DescribeSecretResponse' {tags} -> tags) (\s@DescribeSecretResponse' {} a -> s {tags = a} :: DescribeSecretResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSecretResponse_httpStatus :: Lens.Lens' DescribeSecretResponse Prelude.Int
describeSecretResponse_httpStatus = Lens.lens (\DescribeSecretResponse' {httpStatus} -> httpStatus) (\s@DescribeSecretResponse' {} a -> s {httpStatus = a} :: DescribeSecretResponse)

instance Prelude.NFData DescribeSecretResponse
