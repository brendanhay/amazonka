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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of a secret. It does not include the encrypted
-- secret value. Secrets Manager only returns fields that have a value in
-- the response.
--
-- Secrets Manager generates a CloudTrail log entry when you call this
-- action. Do not include sensitive information in request parameters
-- because it might be logged. For more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/retrieve-ct-entries.html Logging Secrets Manager events with CloudTrail>.
--
-- __Required permissions:__ @secretsmanager:DescribeSecret@. For more
-- information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/reference_iam-permissions.html#reference_iam-permissions_actions IAM policy actions for Secrets Manager>
-- and
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and access control in Secrets Manager>.
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
    describeSecretResponse_arn,
    describeSecretResponse_createdDate,
    describeSecretResponse_deletedDate,
    describeSecretResponse_description,
    describeSecretResponse_kmsKeyId,
    describeSecretResponse_lastAccessedDate,
    describeSecretResponse_lastChangedDate,
    describeSecretResponse_lastRotatedDate,
    describeSecretResponse_name,
    describeSecretResponse_owningService,
    describeSecretResponse_primaryRegion,
    describeSecretResponse_replicationStatus,
    describeSecretResponse_rotationEnabled,
    describeSecretResponse_rotationLambdaARN,
    describeSecretResponse_rotationRules,
    describeSecretResponse_tags,
    describeSecretResponse_versionIdsToStages,
    describeSecretResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newDescribeSecret' smart constructor.
data DescribeSecret = DescribeSecret'
  { -- | The ARN or name of the secret.
    --
    -- For an ARN, we recommend that you specify a complete ARN rather than a
    -- partial ARN. See
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
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
-- 'secretId', 'describeSecret_secretId' - The ARN or name of the secret.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN. See
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
newDescribeSecret ::
  -- | 'secretId'
  Prelude.Text ->
  DescribeSecret
newDescribeSecret pSecretId_ =
  DescribeSecret' {secretId = pSecretId_}

-- | The ARN or name of the secret.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN. See
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
describeSecret_secretId :: Lens.Lens' DescribeSecret Prelude.Text
describeSecret_secretId = Lens.lens (\DescribeSecret' {secretId} -> secretId) (\s@DescribeSecret' {} a -> s {secretId = a} :: DescribeSecret)

instance Core.AWSRequest DescribeSecret where
  type
    AWSResponse DescribeSecret =
      DescribeSecretResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSecretResponse'
            Prelude.<$> (x Data..?> "ARN")
            Prelude.<*> (x Data..?> "CreatedDate")
            Prelude.<*> (x Data..?> "DeletedDate")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "KmsKeyId")
            Prelude.<*> (x Data..?> "LastAccessedDate")
            Prelude.<*> (x Data..?> "LastChangedDate")
            Prelude.<*> (x Data..?> "LastRotatedDate")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "OwningService")
            Prelude.<*> (x Data..?> "PrimaryRegion")
            Prelude.<*> ( x Data..?> "ReplicationStatus"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "RotationEnabled")
            Prelude.<*> (x Data..?> "RotationLambdaARN")
            Prelude.<*> (x Data..?> "RotationRules")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Data..?> "VersionIdsToStages"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSecret where
  hashWithSalt _salt DescribeSecret' {..} =
    _salt `Prelude.hashWithSalt` secretId

instance Prelude.NFData DescribeSecret where
  rnf DescribeSecret' {..} = Prelude.rnf secretId

instance Data.ToHeaders DescribeSecret where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "secretsmanager.DescribeSecret" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeSecret where
  toJSON DescribeSecret' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("SecretId" Data..= secretId)]
      )

instance Data.ToPath DescribeSecret where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSecret where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSecretResponse' smart constructor.
data DescribeSecretResponse = DescribeSecretResponse'
  { -- | The ARN of the secret.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date the secret was created.
    createdDate :: Prelude.Maybe Data.POSIX,
    -- | The date the secret is scheduled for deletion. If it is not scheduled
    -- for deletion, this field is omitted. When you delete a secret, Secrets
    -- Manager requires a recovery window of at least 7 days before deleting
    -- the secret. Some time after the deleted date, Secrets Manager deletes
    -- the secret, including all of its versions.
    --
    -- If a secret is scheduled for deletion, then its details, including the
    -- encrypted secret value, is not accessible. To cancel a scheduled
    -- deletion and restore access to the secret, use RestoreSecret.
    deletedDate :: Prelude.Maybe Data.POSIX,
    -- | The description of the secret.
    description :: Prelude.Maybe Prelude.Text,
    -- | The key ID or alias ARN of the KMS key that Secrets Manager uses to
    -- encrypt the secret value. If the secret is encrypted with the Amazon Web
    -- Services managed key @aws\/secretsmanager@, this field is omitted.
    -- Secrets created using the console use an KMS key ID.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The date that the secret was last accessed in the Region. This field is
    -- omitted if the secret has never been retrieved in the Region.
    lastAccessedDate :: Prelude.Maybe Data.POSIX,
    -- | The last date and time that this secret was modified in any way.
    lastChangedDate :: Prelude.Maybe Data.POSIX,
    -- | The last date and time that Secrets Manager rotated the secret. If the
    -- secret isn\'t configured for rotation, Secrets Manager returns null.
    lastRotatedDate :: Prelude.Maybe Data.POSIX,
    -- | The name of the secret.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the service that created this secret. For more information,
    -- see
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/service-linked-secrets.html Secrets managed by other Amazon Web Services services>.
    owningService :: Prelude.Maybe Prelude.Text,
    -- | The Region the secret is in. If a secret is replicated to other Regions,
    -- the replicas are listed in @ReplicationStatus@.
    primaryRegion :: Prelude.Maybe Prelude.Text,
    -- | A list of the replicas of this secret and their status:
    --
    -- -   @Failed@, which indicates that the replica was not created.
    --
    -- -   @InProgress@, which indicates that Secrets Manager is in the process
    --     of creating the replica.
    --
    -- -   @InSync@, which indicates that the replica was created.
    replicationStatus :: Prelude.Maybe [ReplicationStatusType],
    -- | Specifies whether automatic rotation is turned on for this secret.
    --
    -- To turn on rotation, use RotateSecret. To turn off rotation, use
    -- CancelRotateSecret.
    rotationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the Lambda function that Secrets Manager invokes to rotate
    -- the secret.
    rotationLambdaARN :: Prelude.Maybe Prelude.Text,
    -- | The rotation schedule and Lambda function for this secret. If the secret
    -- previously had rotation turned on, but it is now turned off, this field
    -- shows the previous rotation schedule and rotation function. If the
    -- secret never had rotation turned on, this field is omitted.
    rotationRules :: Prelude.Maybe RotationRulesType,
    -- | The list of tags attached to the secret. To add tags to a secret, use
    -- TagResource. To remove tags, use UntagResource.
    tags :: Prelude.Maybe [Tag],
    -- | A list of the versions of the secret that have staging labels attached.
    -- Versions that don\'t have staging labels are considered deprecated and
    -- Secrets Manager can delete them.
    --
    -- Secrets Manager uses staging labels to indicate the status of a secret
    -- version during rotation. The three staging labels for rotation are:
    --
    -- -   @AWSCURRENT@, which indicates the current version of the secret.
    --
    -- -   @AWSPENDING@, which indicates the version of the secret that
    --     contains new secret information that will become the next current
    --     version when rotation finishes.
    --
    --     During rotation, Secrets Manager creates an @AWSPENDING@ version ID
    --     before creating the new secret version. To check if a secret version
    --     exists, call GetSecretValue.
    --
    -- -   @AWSPREVIOUS@, which indicates the previous current version of the
    --     secret. You can use this as the /last known good/ version.
    --
    -- For more information about rotation and staging labels, see
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/rotate-secrets_how.html How rotation works>.
    versionIdsToStages :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)),
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
-- 'arn', 'describeSecretResponse_arn' - The ARN of the secret.
--
-- 'createdDate', 'describeSecretResponse_createdDate' - The date the secret was created.
--
-- 'deletedDate', 'describeSecretResponse_deletedDate' - The date the secret is scheduled for deletion. If it is not scheduled
-- for deletion, this field is omitted. When you delete a secret, Secrets
-- Manager requires a recovery window of at least 7 days before deleting
-- the secret. Some time after the deleted date, Secrets Manager deletes
-- the secret, including all of its versions.
--
-- If a secret is scheduled for deletion, then its details, including the
-- encrypted secret value, is not accessible. To cancel a scheduled
-- deletion and restore access to the secret, use RestoreSecret.
--
-- 'description', 'describeSecretResponse_description' - The description of the secret.
--
-- 'kmsKeyId', 'describeSecretResponse_kmsKeyId' - The key ID or alias ARN of the KMS key that Secrets Manager uses to
-- encrypt the secret value. If the secret is encrypted with the Amazon Web
-- Services managed key @aws\/secretsmanager@, this field is omitted.
-- Secrets created using the console use an KMS key ID.
--
-- 'lastAccessedDate', 'describeSecretResponse_lastAccessedDate' - The date that the secret was last accessed in the Region. This field is
-- omitted if the secret has never been retrieved in the Region.
--
-- 'lastChangedDate', 'describeSecretResponse_lastChangedDate' - The last date and time that this secret was modified in any way.
--
-- 'lastRotatedDate', 'describeSecretResponse_lastRotatedDate' - The last date and time that Secrets Manager rotated the secret. If the
-- secret isn\'t configured for rotation, Secrets Manager returns null.
--
-- 'name', 'describeSecretResponse_name' - The name of the secret.
--
-- 'owningService', 'describeSecretResponse_owningService' - The ID of the service that created this secret. For more information,
-- see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/service-linked-secrets.html Secrets managed by other Amazon Web Services services>.
--
-- 'primaryRegion', 'describeSecretResponse_primaryRegion' - The Region the secret is in. If a secret is replicated to other Regions,
-- the replicas are listed in @ReplicationStatus@.
--
-- 'replicationStatus', 'describeSecretResponse_replicationStatus' - A list of the replicas of this secret and their status:
--
-- -   @Failed@, which indicates that the replica was not created.
--
-- -   @InProgress@, which indicates that Secrets Manager is in the process
--     of creating the replica.
--
-- -   @InSync@, which indicates that the replica was created.
--
-- 'rotationEnabled', 'describeSecretResponse_rotationEnabled' - Specifies whether automatic rotation is turned on for this secret.
--
-- To turn on rotation, use RotateSecret. To turn off rotation, use
-- CancelRotateSecret.
--
-- 'rotationLambdaARN', 'describeSecretResponse_rotationLambdaARN' - The ARN of the Lambda function that Secrets Manager invokes to rotate
-- the secret.
--
-- 'rotationRules', 'describeSecretResponse_rotationRules' - The rotation schedule and Lambda function for this secret. If the secret
-- previously had rotation turned on, but it is now turned off, this field
-- shows the previous rotation schedule and rotation function. If the
-- secret never had rotation turned on, this field is omitted.
--
-- 'tags', 'describeSecretResponse_tags' - The list of tags attached to the secret. To add tags to a secret, use
-- TagResource. To remove tags, use UntagResource.
--
-- 'versionIdsToStages', 'describeSecretResponse_versionIdsToStages' - A list of the versions of the secret that have staging labels attached.
-- Versions that don\'t have staging labels are considered deprecated and
-- Secrets Manager can delete them.
--
-- Secrets Manager uses staging labels to indicate the status of a secret
-- version during rotation. The three staging labels for rotation are:
--
-- -   @AWSCURRENT@, which indicates the current version of the secret.
--
-- -   @AWSPENDING@, which indicates the version of the secret that
--     contains new secret information that will become the next current
--     version when rotation finishes.
--
--     During rotation, Secrets Manager creates an @AWSPENDING@ version ID
--     before creating the new secret version. To check if a secret version
--     exists, call GetSecretValue.
--
-- -   @AWSPREVIOUS@, which indicates the previous current version of the
--     secret. You can use this as the /last known good/ version.
--
-- For more information about rotation and staging labels, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/rotate-secrets_how.html How rotation works>.
--
-- 'httpStatus', 'describeSecretResponse_httpStatus' - The response's http status code.
newDescribeSecretResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSecretResponse
newDescribeSecretResponse pHttpStatus_ =
  DescribeSecretResponse'
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
      replicationStatus = Prelude.Nothing,
      rotationEnabled = Prelude.Nothing,
      rotationLambdaARN = Prelude.Nothing,
      rotationRules = Prelude.Nothing,
      tags = Prelude.Nothing,
      versionIdsToStages = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the secret.
describeSecretResponse_arn :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe Prelude.Text)
describeSecretResponse_arn = Lens.lens (\DescribeSecretResponse' {arn} -> arn) (\s@DescribeSecretResponse' {} a -> s {arn = a} :: DescribeSecretResponse)

-- | The date the secret was created.
describeSecretResponse_createdDate :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe Prelude.UTCTime)
describeSecretResponse_createdDate = Lens.lens (\DescribeSecretResponse' {createdDate} -> createdDate) (\s@DescribeSecretResponse' {} a -> s {createdDate = a} :: DescribeSecretResponse) Prelude.. Lens.mapping Data._Time

-- | The date the secret is scheduled for deletion. If it is not scheduled
-- for deletion, this field is omitted. When you delete a secret, Secrets
-- Manager requires a recovery window of at least 7 days before deleting
-- the secret. Some time after the deleted date, Secrets Manager deletes
-- the secret, including all of its versions.
--
-- If a secret is scheduled for deletion, then its details, including the
-- encrypted secret value, is not accessible. To cancel a scheduled
-- deletion and restore access to the secret, use RestoreSecret.
describeSecretResponse_deletedDate :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe Prelude.UTCTime)
describeSecretResponse_deletedDate = Lens.lens (\DescribeSecretResponse' {deletedDate} -> deletedDate) (\s@DescribeSecretResponse' {} a -> s {deletedDate = a} :: DescribeSecretResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the secret.
describeSecretResponse_description :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe Prelude.Text)
describeSecretResponse_description = Lens.lens (\DescribeSecretResponse' {description} -> description) (\s@DescribeSecretResponse' {} a -> s {description = a} :: DescribeSecretResponse)

-- | The key ID or alias ARN of the KMS key that Secrets Manager uses to
-- encrypt the secret value. If the secret is encrypted with the Amazon Web
-- Services managed key @aws\/secretsmanager@, this field is omitted.
-- Secrets created using the console use an KMS key ID.
describeSecretResponse_kmsKeyId :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe Prelude.Text)
describeSecretResponse_kmsKeyId = Lens.lens (\DescribeSecretResponse' {kmsKeyId} -> kmsKeyId) (\s@DescribeSecretResponse' {} a -> s {kmsKeyId = a} :: DescribeSecretResponse)

-- | The date that the secret was last accessed in the Region. This field is
-- omitted if the secret has never been retrieved in the Region.
describeSecretResponse_lastAccessedDate :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe Prelude.UTCTime)
describeSecretResponse_lastAccessedDate = Lens.lens (\DescribeSecretResponse' {lastAccessedDate} -> lastAccessedDate) (\s@DescribeSecretResponse' {} a -> s {lastAccessedDate = a} :: DescribeSecretResponse) Prelude.. Lens.mapping Data._Time

-- | The last date and time that this secret was modified in any way.
describeSecretResponse_lastChangedDate :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe Prelude.UTCTime)
describeSecretResponse_lastChangedDate = Lens.lens (\DescribeSecretResponse' {lastChangedDate} -> lastChangedDate) (\s@DescribeSecretResponse' {} a -> s {lastChangedDate = a} :: DescribeSecretResponse) Prelude.. Lens.mapping Data._Time

-- | The last date and time that Secrets Manager rotated the secret. If the
-- secret isn\'t configured for rotation, Secrets Manager returns null.
describeSecretResponse_lastRotatedDate :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe Prelude.UTCTime)
describeSecretResponse_lastRotatedDate = Lens.lens (\DescribeSecretResponse' {lastRotatedDate} -> lastRotatedDate) (\s@DescribeSecretResponse' {} a -> s {lastRotatedDate = a} :: DescribeSecretResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the secret.
describeSecretResponse_name :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe Prelude.Text)
describeSecretResponse_name = Lens.lens (\DescribeSecretResponse' {name} -> name) (\s@DescribeSecretResponse' {} a -> s {name = a} :: DescribeSecretResponse)

-- | The ID of the service that created this secret. For more information,
-- see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/service-linked-secrets.html Secrets managed by other Amazon Web Services services>.
describeSecretResponse_owningService :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe Prelude.Text)
describeSecretResponse_owningService = Lens.lens (\DescribeSecretResponse' {owningService} -> owningService) (\s@DescribeSecretResponse' {} a -> s {owningService = a} :: DescribeSecretResponse)

-- | The Region the secret is in. If a secret is replicated to other Regions,
-- the replicas are listed in @ReplicationStatus@.
describeSecretResponse_primaryRegion :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe Prelude.Text)
describeSecretResponse_primaryRegion = Lens.lens (\DescribeSecretResponse' {primaryRegion} -> primaryRegion) (\s@DescribeSecretResponse' {} a -> s {primaryRegion = a} :: DescribeSecretResponse)

-- | A list of the replicas of this secret and their status:
--
-- -   @Failed@, which indicates that the replica was not created.
--
-- -   @InProgress@, which indicates that Secrets Manager is in the process
--     of creating the replica.
--
-- -   @InSync@, which indicates that the replica was created.
describeSecretResponse_replicationStatus :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe [ReplicationStatusType])
describeSecretResponse_replicationStatus = Lens.lens (\DescribeSecretResponse' {replicationStatus} -> replicationStatus) (\s@DescribeSecretResponse' {} a -> s {replicationStatus = a} :: DescribeSecretResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether automatic rotation is turned on for this secret.
--
-- To turn on rotation, use RotateSecret. To turn off rotation, use
-- CancelRotateSecret.
describeSecretResponse_rotationEnabled :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe Prelude.Bool)
describeSecretResponse_rotationEnabled = Lens.lens (\DescribeSecretResponse' {rotationEnabled} -> rotationEnabled) (\s@DescribeSecretResponse' {} a -> s {rotationEnabled = a} :: DescribeSecretResponse)

-- | The ARN of the Lambda function that Secrets Manager invokes to rotate
-- the secret.
describeSecretResponse_rotationLambdaARN :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe Prelude.Text)
describeSecretResponse_rotationLambdaARN = Lens.lens (\DescribeSecretResponse' {rotationLambdaARN} -> rotationLambdaARN) (\s@DescribeSecretResponse' {} a -> s {rotationLambdaARN = a} :: DescribeSecretResponse)

-- | The rotation schedule and Lambda function for this secret. If the secret
-- previously had rotation turned on, but it is now turned off, this field
-- shows the previous rotation schedule and rotation function. If the
-- secret never had rotation turned on, this field is omitted.
describeSecretResponse_rotationRules :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe RotationRulesType)
describeSecretResponse_rotationRules = Lens.lens (\DescribeSecretResponse' {rotationRules} -> rotationRules) (\s@DescribeSecretResponse' {} a -> s {rotationRules = a} :: DescribeSecretResponse)

-- | The list of tags attached to the secret. To add tags to a secret, use
-- TagResource. To remove tags, use UntagResource.
describeSecretResponse_tags :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe [Tag])
describeSecretResponse_tags = Lens.lens (\DescribeSecretResponse' {tags} -> tags) (\s@DescribeSecretResponse' {} a -> s {tags = a} :: DescribeSecretResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of the versions of the secret that have staging labels attached.
-- Versions that don\'t have staging labels are considered deprecated and
-- Secrets Manager can delete them.
--
-- Secrets Manager uses staging labels to indicate the status of a secret
-- version during rotation. The three staging labels for rotation are:
--
-- -   @AWSCURRENT@, which indicates the current version of the secret.
--
-- -   @AWSPENDING@, which indicates the version of the secret that
--     contains new secret information that will become the next current
--     version when rotation finishes.
--
--     During rotation, Secrets Manager creates an @AWSPENDING@ version ID
--     before creating the new secret version. To check if a secret version
--     exists, call GetSecretValue.
--
-- -   @AWSPREVIOUS@, which indicates the previous current version of the
--     secret. You can use this as the /last known good/ version.
--
-- For more information about rotation and staging labels, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/rotate-secrets_how.html How rotation works>.
describeSecretResponse_versionIdsToStages :: Lens.Lens' DescribeSecretResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)))
describeSecretResponse_versionIdsToStages = Lens.lens (\DescribeSecretResponse' {versionIdsToStages} -> versionIdsToStages) (\s@DescribeSecretResponse' {} a -> s {versionIdsToStages = a} :: DescribeSecretResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSecretResponse_httpStatus :: Lens.Lens' DescribeSecretResponse Prelude.Int
describeSecretResponse_httpStatus = Lens.lens (\DescribeSecretResponse' {httpStatus} -> httpStatus) (\s@DescribeSecretResponse' {} a -> s {httpStatus = a} :: DescribeSecretResponse)

instance Prelude.NFData DescribeSecretResponse where
  rnf DescribeSecretResponse' {..} =
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
      `Prelude.seq` Prelude.rnf replicationStatus
      `Prelude.seq` Prelude.rnf rotationEnabled
      `Prelude.seq` Prelude.rnf rotationLambdaARN
      `Prelude.seq` Prelude.rnf rotationRules
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf versionIdsToStages
      `Prelude.seq` Prelude.rnf httpStatus
