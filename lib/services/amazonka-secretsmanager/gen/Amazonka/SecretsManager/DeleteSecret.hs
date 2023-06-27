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
-- Module      : Amazonka.SecretsManager.DeleteSecret
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a secret and all of its versions. You can specify a recovery
-- window during which you can restore the secret. The minimum recovery
-- window is 7 days. The default recovery window is 30 days. Secrets
-- Manager attaches a @DeletionDate@ stamp to the secret that specifies the
-- end of the recovery window. At the end of the recovery window, Secrets
-- Manager deletes the secret permanently.
--
-- You can\'t delete a primary secret that is replicated to other Regions.
-- You must first delete the replicas using RemoveRegionsFromReplication,
-- and then delete the primary secret. When you delete a replica, it is
-- deleted immediately.
--
-- You can\'t directly delete a version of a secret. Instead, you remove
-- all staging labels from the version using UpdateSecretVersionStage. This
-- marks the version as deprecated, and then Secrets Manager can
-- automatically delete the version in the background.
--
-- To determine whether an application still uses a secret, you can create
-- an Amazon CloudWatch alarm to alert you to any attempts to access a
-- secret during the recovery window. For more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/monitoring_cloudwatch_deleted-secrets.html Monitor secrets scheduled for deletion>.
--
-- Secrets Manager performs the permanent secret deletion at the end of the
-- waiting period as a background task with low priority. There is no
-- guarantee of a specific time after the recovery window for the permanent
-- delete to occur.
--
-- At any time before recovery window ends, you can use RestoreSecret to
-- remove the @DeletionDate@ and cancel the deletion of the secret.
--
-- When a secret is scheduled for deletion, you cannot retrieve the secret
-- value. You must first cancel the deletion with RestoreSecret and then
-- you can retrieve the secret.
--
-- Secrets Manager generates a CloudTrail log entry when you call this
-- action. Do not include sensitive information in request parameters
-- because it might be logged. For more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/retrieve-ct-entries.html Logging Secrets Manager events with CloudTrail>.
--
-- __Required permissions:__ @secretsmanager:DeleteSecret@. For more
-- information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/reference_iam-permissions.html#reference_iam-permissions_actions IAM policy actions for Secrets Manager>
-- and
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and access control in Secrets Manager>.
module Amazonka.SecretsManager.DeleteSecret
  ( -- * Creating a Request
    DeleteSecret (..),
    newDeleteSecret,

    -- * Request Lenses
    deleteSecret_forceDeleteWithoutRecovery,
    deleteSecret_recoveryWindowInDays,
    deleteSecret_secretId,

    -- * Destructuring the Response
    DeleteSecretResponse (..),
    newDeleteSecretResponse,

    -- * Response Lenses
    deleteSecretResponse_arn,
    deleteSecretResponse_deletionDate,
    deleteSecretResponse_name,
    deleteSecretResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newDeleteSecret' smart constructor.
data DeleteSecret = DeleteSecret'
  { -- | Specifies whether to delete the secret without any recovery window. You
    -- can\'t use both this parameter and @RecoveryWindowInDays@ in the same
    -- call. If you don\'t use either, then by default Secrets Manager uses a
    -- 30 day recovery window.
    --
    -- Secrets Manager performs the actual deletion with an asynchronous
    -- background process, so there might be a short delay before the secret is
    -- permanently deleted. If you delete a secret and then immediately create
    -- a secret with the same name, use appropriate back off and retry logic.
    --
    -- If you forcibly delete an already deleted or nonexistent secret, the
    -- operation does not return @ResourceNotFoundException@.
    --
    -- Use this parameter with caution. This parameter causes the operation to
    -- skip the normal recovery window before the permanent deletion that
    -- Secrets Manager would normally impose with the @RecoveryWindowInDays@
    -- parameter. If you delete a secret with the @ForceDeleteWithoutRecovery@
    -- parameter, then you have no opportunity to recover the secret. You lose
    -- the secret permanently.
    forceDeleteWithoutRecovery :: Prelude.Maybe Prelude.Bool,
    -- | The number of days from 7 to 30 that Secrets Manager waits before
    -- permanently deleting the secret. You can\'t use both this parameter and
    -- @ForceDeleteWithoutRecovery@ in the same call. If you don\'t use either,
    -- then by default Secrets Manager uses a 30 day recovery window.
    recoveryWindowInDays :: Prelude.Maybe Prelude.Integer,
    -- | The ARN or name of the secret to delete.
    --
    -- For an ARN, we recommend that you specify a complete ARN rather than a
    -- partial ARN. See
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
    secretId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSecret' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forceDeleteWithoutRecovery', 'deleteSecret_forceDeleteWithoutRecovery' - Specifies whether to delete the secret without any recovery window. You
-- can\'t use both this parameter and @RecoveryWindowInDays@ in the same
-- call. If you don\'t use either, then by default Secrets Manager uses a
-- 30 day recovery window.
--
-- Secrets Manager performs the actual deletion with an asynchronous
-- background process, so there might be a short delay before the secret is
-- permanently deleted. If you delete a secret and then immediately create
-- a secret with the same name, use appropriate back off and retry logic.
--
-- If you forcibly delete an already deleted or nonexistent secret, the
-- operation does not return @ResourceNotFoundException@.
--
-- Use this parameter with caution. This parameter causes the operation to
-- skip the normal recovery window before the permanent deletion that
-- Secrets Manager would normally impose with the @RecoveryWindowInDays@
-- parameter. If you delete a secret with the @ForceDeleteWithoutRecovery@
-- parameter, then you have no opportunity to recover the secret. You lose
-- the secret permanently.
--
-- 'recoveryWindowInDays', 'deleteSecret_recoveryWindowInDays' - The number of days from 7 to 30 that Secrets Manager waits before
-- permanently deleting the secret. You can\'t use both this parameter and
-- @ForceDeleteWithoutRecovery@ in the same call. If you don\'t use either,
-- then by default Secrets Manager uses a 30 day recovery window.
--
-- 'secretId', 'deleteSecret_secretId' - The ARN or name of the secret to delete.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN. See
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
newDeleteSecret ::
  -- | 'secretId'
  Prelude.Text ->
  DeleteSecret
newDeleteSecret pSecretId_ =
  DeleteSecret'
    { forceDeleteWithoutRecovery =
        Prelude.Nothing,
      recoveryWindowInDays = Prelude.Nothing,
      secretId = pSecretId_
    }

-- | Specifies whether to delete the secret without any recovery window. You
-- can\'t use both this parameter and @RecoveryWindowInDays@ in the same
-- call. If you don\'t use either, then by default Secrets Manager uses a
-- 30 day recovery window.
--
-- Secrets Manager performs the actual deletion with an asynchronous
-- background process, so there might be a short delay before the secret is
-- permanently deleted. If you delete a secret and then immediately create
-- a secret with the same name, use appropriate back off and retry logic.
--
-- If you forcibly delete an already deleted or nonexistent secret, the
-- operation does not return @ResourceNotFoundException@.
--
-- Use this parameter with caution. This parameter causes the operation to
-- skip the normal recovery window before the permanent deletion that
-- Secrets Manager would normally impose with the @RecoveryWindowInDays@
-- parameter. If you delete a secret with the @ForceDeleteWithoutRecovery@
-- parameter, then you have no opportunity to recover the secret. You lose
-- the secret permanently.
deleteSecret_forceDeleteWithoutRecovery :: Lens.Lens' DeleteSecret (Prelude.Maybe Prelude.Bool)
deleteSecret_forceDeleteWithoutRecovery = Lens.lens (\DeleteSecret' {forceDeleteWithoutRecovery} -> forceDeleteWithoutRecovery) (\s@DeleteSecret' {} a -> s {forceDeleteWithoutRecovery = a} :: DeleteSecret)

-- | The number of days from 7 to 30 that Secrets Manager waits before
-- permanently deleting the secret. You can\'t use both this parameter and
-- @ForceDeleteWithoutRecovery@ in the same call. If you don\'t use either,
-- then by default Secrets Manager uses a 30 day recovery window.
deleteSecret_recoveryWindowInDays :: Lens.Lens' DeleteSecret (Prelude.Maybe Prelude.Integer)
deleteSecret_recoveryWindowInDays = Lens.lens (\DeleteSecret' {recoveryWindowInDays} -> recoveryWindowInDays) (\s@DeleteSecret' {} a -> s {recoveryWindowInDays = a} :: DeleteSecret)

-- | The ARN or name of the secret to delete.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN. See
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
deleteSecret_secretId :: Lens.Lens' DeleteSecret Prelude.Text
deleteSecret_secretId = Lens.lens (\DeleteSecret' {secretId} -> secretId) (\s@DeleteSecret' {} a -> s {secretId = a} :: DeleteSecret)

instance Core.AWSRequest DeleteSecret where
  type AWSResponse DeleteSecret = DeleteSecretResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSecretResponse'
            Prelude.<$> (x Data..?> "ARN")
            Prelude.<*> (x Data..?> "DeletionDate")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSecret where
  hashWithSalt _salt DeleteSecret' {..} =
    _salt
      `Prelude.hashWithSalt` forceDeleteWithoutRecovery
      `Prelude.hashWithSalt` recoveryWindowInDays
      `Prelude.hashWithSalt` secretId

instance Prelude.NFData DeleteSecret where
  rnf DeleteSecret' {..} =
    Prelude.rnf forceDeleteWithoutRecovery
      `Prelude.seq` Prelude.rnf recoveryWindowInDays
      `Prelude.seq` Prelude.rnf secretId

instance Data.ToHeaders DeleteSecret where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "secretsmanager.DeleteSecret" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteSecret where
  toJSON DeleteSecret' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ForceDeleteWithoutRecovery" Data..=)
              Prelude.<$> forceDeleteWithoutRecovery,
            ("RecoveryWindowInDays" Data..=)
              Prelude.<$> recoveryWindowInDays,
            Prelude.Just ("SecretId" Data..= secretId)
          ]
      )

instance Data.ToPath DeleteSecret where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteSecret where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSecretResponse' smart constructor.
data DeleteSecretResponse = DeleteSecretResponse'
  { -- | The ARN of the secret.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time after which this secret Secrets Manager can
    -- permanently delete this secret, and it can no longer be restored. This
    -- value is the date and time of the delete request plus the number of days
    -- in @RecoveryWindowInDays@.
    deletionDate :: Prelude.Maybe Data.POSIX,
    -- | The name of the secret.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSecretResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteSecretResponse_arn' - The ARN of the secret.
--
-- 'deletionDate', 'deleteSecretResponse_deletionDate' - The date and time after which this secret Secrets Manager can
-- permanently delete this secret, and it can no longer be restored. This
-- value is the date and time of the delete request plus the number of days
-- in @RecoveryWindowInDays@.
--
-- 'name', 'deleteSecretResponse_name' - The name of the secret.
--
-- 'httpStatus', 'deleteSecretResponse_httpStatus' - The response's http status code.
newDeleteSecretResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSecretResponse
newDeleteSecretResponse pHttpStatus_ =
  DeleteSecretResponse'
    { arn = Prelude.Nothing,
      deletionDate = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the secret.
deleteSecretResponse_arn :: Lens.Lens' DeleteSecretResponse (Prelude.Maybe Prelude.Text)
deleteSecretResponse_arn = Lens.lens (\DeleteSecretResponse' {arn} -> arn) (\s@DeleteSecretResponse' {} a -> s {arn = a} :: DeleteSecretResponse)

-- | The date and time after which this secret Secrets Manager can
-- permanently delete this secret, and it can no longer be restored. This
-- value is the date and time of the delete request plus the number of days
-- in @RecoveryWindowInDays@.
deleteSecretResponse_deletionDate :: Lens.Lens' DeleteSecretResponse (Prelude.Maybe Prelude.UTCTime)
deleteSecretResponse_deletionDate = Lens.lens (\DeleteSecretResponse' {deletionDate} -> deletionDate) (\s@DeleteSecretResponse' {} a -> s {deletionDate = a} :: DeleteSecretResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the secret.
deleteSecretResponse_name :: Lens.Lens' DeleteSecretResponse (Prelude.Maybe Prelude.Text)
deleteSecretResponse_name = Lens.lens (\DeleteSecretResponse' {name} -> name) (\s@DeleteSecretResponse' {} a -> s {name = a} :: DeleteSecretResponse)

-- | The response's http status code.
deleteSecretResponse_httpStatus :: Lens.Lens' DeleteSecretResponse Prelude.Int
deleteSecretResponse_httpStatus = Lens.lens (\DeleteSecretResponse' {httpStatus} -> httpStatus) (\s@DeleteSecretResponse' {} a -> s {httpStatus = a} :: DeleteSecretResponse)

instance Prelude.NFData DeleteSecretResponse where
  rnf DeleteSecretResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf deletionDate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
