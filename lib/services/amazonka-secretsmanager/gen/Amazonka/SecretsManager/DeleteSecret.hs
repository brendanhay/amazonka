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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an entire secret and all of the versions. You can optionally
-- include a recovery window during which you can restore the secret. If
-- you don\'t specify a recovery window value, the operation defaults to 30
-- days. Secrets Manager attaches a @DeletionDate@ stamp to the secret that
-- specifies the end of the recovery window. At the end of the recovery
-- window, Secrets Manager deletes the secret permanently.
--
-- At any time before recovery window ends, you can use RestoreSecret to
-- remove the @DeletionDate@ and cancel the deletion of the secret.
--
-- You cannot access the encrypted secret information in any secret
-- scheduled for deletion. If you need to access that information, you must
-- cancel the deletion with RestoreSecret and then retrieve the
-- information.
--
-- -   There is no explicit operation to delete a version of a secret.
--     Instead, remove all staging labels from the @VersionStage@ field of
--     a version. That marks the version as deprecated and allows Secrets
--     Manager to delete it as needed. Versions without any staging labels
--     do not show up in ListSecretVersionIds unless you specify
--     @IncludeDeprecated@.
--
-- -   The permanent secret deletion at the end of the waiting period is
--     performed as a background task with low priority. There is no
--     guarantee of a specific time after the recovery window for the
--     actual delete operation to occur.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   secretsmanager:DeleteSecret
--
-- __Related operations__
--
-- -   To create a secret, use CreateSecret.
--
-- -   To cancel deletion of a version of a secret before the recovery
--     window has expired, use RestoreSecret.
module Amazonka.SecretsManager.DeleteSecret
  ( -- * Creating a Request
    DeleteSecret (..),
    newDeleteSecret,

    -- * Request Lenses
    deleteSecret_recoveryWindowInDays,
    deleteSecret_forceDeleteWithoutRecovery,
    deleteSecret_secretId,

    -- * Destructuring the Response
    DeleteSecretResponse (..),
    newDeleteSecretResponse,

    -- * Response Lenses
    deleteSecretResponse_arn,
    deleteSecretResponse_name,
    deleteSecretResponse_deletionDate,
    deleteSecretResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newDeleteSecret' smart constructor.
data DeleteSecret = DeleteSecret'
  { -- | (Optional) Specifies the number of days that Secrets Manager waits
    -- before Secrets Manager can delete the secret. You can\'t use both this
    -- parameter and the @ForceDeleteWithoutRecovery@ parameter in the same API
    -- call.
    --
    -- This value can range from 7 to 30 days with a default value of 30.
    recoveryWindowInDays :: Prelude.Maybe Prelude.Integer,
    -- | (Optional) Specifies that the secret is to be deleted without any
    -- recovery window. You can\'t use both this parameter and the
    -- @RecoveryWindowInDays@ parameter in the same API call.
    --
    -- An asynchronous background process performs the actual deletion, so
    -- there can be a short delay before the operation completes. If you write
    -- code to delete and then immediately recreate a secret with the same
    -- name, ensure that your code includes appropriate back off and retry
    -- logic.
    --
    -- Use this parameter with caution. This parameter causes the operation to
    -- skip the normal waiting period before the permanent deletion that Amazon
    -- Web Services would normally impose with the @RecoveryWindowInDays@
    -- parameter. If you delete a secret with the @ForceDeleteWithouRecovery@
    -- parameter, then you have no opportunity to recover the secret. You lose
    -- the secret permanently.
    --
    -- If you use this parameter and include a previously deleted or
    -- nonexistent secret, the operation does not return the error
    -- @ResourceNotFoundException@ in order to correctly handle retries.
    forceDeleteWithoutRecovery :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the secret to delete. You can specify either the Amazon
    -- Resource Name (ARN) or the friendly name of the secret.
    --
    -- For an ARN, we recommend that you specify a complete ARN rather than a
    -- partial ARN.
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
-- 'recoveryWindowInDays', 'deleteSecret_recoveryWindowInDays' - (Optional) Specifies the number of days that Secrets Manager waits
-- before Secrets Manager can delete the secret. You can\'t use both this
-- parameter and the @ForceDeleteWithoutRecovery@ parameter in the same API
-- call.
--
-- This value can range from 7 to 30 days with a default value of 30.
--
-- 'forceDeleteWithoutRecovery', 'deleteSecret_forceDeleteWithoutRecovery' - (Optional) Specifies that the secret is to be deleted without any
-- recovery window. You can\'t use both this parameter and the
-- @RecoveryWindowInDays@ parameter in the same API call.
--
-- An asynchronous background process performs the actual deletion, so
-- there can be a short delay before the operation completes. If you write
-- code to delete and then immediately recreate a secret with the same
-- name, ensure that your code includes appropriate back off and retry
-- logic.
--
-- Use this parameter with caution. This parameter causes the operation to
-- skip the normal waiting period before the permanent deletion that Amazon
-- Web Services would normally impose with the @RecoveryWindowInDays@
-- parameter. If you delete a secret with the @ForceDeleteWithouRecovery@
-- parameter, then you have no opportunity to recover the secret. You lose
-- the secret permanently.
--
-- If you use this parameter and include a previously deleted or
-- nonexistent secret, the operation does not return the error
-- @ResourceNotFoundException@ in order to correctly handle retries.
--
-- 'secretId', 'deleteSecret_secretId' - Specifies the secret to delete. You can specify either the Amazon
-- Resource Name (ARN) or the friendly name of the secret.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN.
newDeleteSecret ::
  -- | 'secretId'
  Prelude.Text ->
  DeleteSecret
newDeleteSecret pSecretId_ =
  DeleteSecret'
    { recoveryWindowInDays =
        Prelude.Nothing,
      forceDeleteWithoutRecovery = Prelude.Nothing,
      secretId = pSecretId_
    }

-- | (Optional) Specifies the number of days that Secrets Manager waits
-- before Secrets Manager can delete the secret. You can\'t use both this
-- parameter and the @ForceDeleteWithoutRecovery@ parameter in the same API
-- call.
--
-- This value can range from 7 to 30 days with a default value of 30.
deleteSecret_recoveryWindowInDays :: Lens.Lens' DeleteSecret (Prelude.Maybe Prelude.Integer)
deleteSecret_recoveryWindowInDays = Lens.lens (\DeleteSecret' {recoveryWindowInDays} -> recoveryWindowInDays) (\s@DeleteSecret' {} a -> s {recoveryWindowInDays = a} :: DeleteSecret)

-- | (Optional) Specifies that the secret is to be deleted without any
-- recovery window. You can\'t use both this parameter and the
-- @RecoveryWindowInDays@ parameter in the same API call.
--
-- An asynchronous background process performs the actual deletion, so
-- there can be a short delay before the operation completes. If you write
-- code to delete and then immediately recreate a secret with the same
-- name, ensure that your code includes appropriate back off and retry
-- logic.
--
-- Use this parameter with caution. This parameter causes the operation to
-- skip the normal waiting period before the permanent deletion that Amazon
-- Web Services would normally impose with the @RecoveryWindowInDays@
-- parameter. If you delete a secret with the @ForceDeleteWithouRecovery@
-- parameter, then you have no opportunity to recover the secret. You lose
-- the secret permanently.
--
-- If you use this parameter and include a previously deleted or
-- nonexistent secret, the operation does not return the error
-- @ResourceNotFoundException@ in order to correctly handle retries.
deleteSecret_forceDeleteWithoutRecovery :: Lens.Lens' DeleteSecret (Prelude.Maybe Prelude.Bool)
deleteSecret_forceDeleteWithoutRecovery = Lens.lens (\DeleteSecret' {forceDeleteWithoutRecovery} -> forceDeleteWithoutRecovery) (\s@DeleteSecret' {} a -> s {forceDeleteWithoutRecovery = a} :: DeleteSecret)

-- | Specifies the secret to delete. You can specify either the Amazon
-- Resource Name (ARN) or the friendly name of the secret.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN.
deleteSecret_secretId :: Lens.Lens' DeleteSecret Prelude.Text
deleteSecret_secretId = Lens.lens (\DeleteSecret' {secretId} -> secretId) (\s@DeleteSecret' {} a -> s {secretId = a} :: DeleteSecret)

instance Core.AWSRequest DeleteSecret where
  type AWSResponse DeleteSecret = DeleteSecretResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSecretResponse'
            Prelude.<$> (x Core..?> "ARN")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "DeletionDate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSecret where
  hashWithSalt _salt DeleteSecret' {..} =
    _salt `Prelude.hashWithSalt` recoveryWindowInDays
      `Prelude.hashWithSalt` forceDeleteWithoutRecovery
      `Prelude.hashWithSalt` secretId

instance Prelude.NFData DeleteSecret where
  rnf DeleteSecret' {..} =
    Prelude.rnf recoveryWindowInDays
      `Prelude.seq` Prelude.rnf forceDeleteWithoutRecovery
      `Prelude.seq` Prelude.rnf secretId

instance Core.ToHeaders DeleteSecret where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "secretsmanager.DeleteSecret" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteSecret where
  toJSON DeleteSecret' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RecoveryWindowInDays" Core..=)
              Prelude.<$> recoveryWindowInDays,
            ("ForceDeleteWithoutRecovery" Core..=)
              Prelude.<$> forceDeleteWithoutRecovery,
            Prelude.Just ("SecretId" Core..= secretId)
          ]
      )

instance Core.ToPath DeleteSecret where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteSecret where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSecretResponse' smart constructor.
data DeleteSecretResponse = DeleteSecretResponse'
  { -- | The ARN of the secret that is now scheduled for deletion.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The friendly name of the secret currently scheduled for deletion.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date and time after which this secret can be deleted by Secrets
    -- Manager and can no longer be restored. This value is the date and time
    -- of the delete request plus the number of days specified in
    -- @RecoveryWindowInDays@.
    deletionDate :: Prelude.Maybe Core.POSIX,
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
-- 'arn', 'deleteSecretResponse_arn' - The ARN of the secret that is now scheduled for deletion.
--
-- 'name', 'deleteSecretResponse_name' - The friendly name of the secret currently scheduled for deletion.
--
-- 'deletionDate', 'deleteSecretResponse_deletionDate' - The date and time after which this secret can be deleted by Secrets
-- Manager and can no longer be restored. This value is the date and time
-- of the delete request plus the number of days specified in
-- @RecoveryWindowInDays@.
--
-- 'httpStatus', 'deleteSecretResponse_httpStatus' - The response's http status code.
newDeleteSecretResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSecretResponse
newDeleteSecretResponse pHttpStatus_ =
  DeleteSecretResponse'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing,
      deletionDate = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the secret that is now scheduled for deletion.
deleteSecretResponse_arn :: Lens.Lens' DeleteSecretResponse (Prelude.Maybe Prelude.Text)
deleteSecretResponse_arn = Lens.lens (\DeleteSecretResponse' {arn} -> arn) (\s@DeleteSecretResponse' {} a -> s {arn = a} :: DeleteSecretResponse)

-- | The friendly name of the secret currently scheduled for deletion.
deleteSecretResponse_name :: Lens.Lens' DeleteSecretResponse (Prelude.Maybe Prelude.Text)
deleteSecretResponse_name = Lens.lens (\DeleteSecretResponse' {name} -> name) (\s@DeleteSecretResponse' {} a -> s {name = a} :: DeleteSecretResponse)

-- | The date and time after which this secret can be deleted by Secrets
-- Manager and can no longer be restored. This value is the date and time
-- of the delete request plus the number of days specified in
-- @RecoveryWindowInDays@.
deleteSecretResponse_deletionDate :: Lens.Lens' DeleteSecretResponse (Prelude.Maybe Prelude.UTCTime)
deleteSecretResponse_deletionDate = Lens.lens (\DeleteSecretResponse' {deletionDate} -> deletionDate) (\s@DeleteSecretResponse' {} a -> s {deletionDate = a} :: DeleteSecretResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
deleteSecretResponse_httpStatus :: Lens.Lens' DeleteSecretResponse Prelude.Int
deleteSecretResponse_httpStatus = Lens.lens (\DeleteSecretResponse' {httpStatus} -> httpStatus) (\s@DeleteSecretResponse' {} a -> s {httpStatus = a} :: DeleteSecretResponse)

instance Prelude.NFData DeleteSecretResponse where
  rnf DeleteSecretResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf deletionDate
      `Prelude.seq` Prelude.rnf httpStatus
