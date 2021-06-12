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
-- Module      : Network.AWS.SecretsManager.DeleteSecret
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
module Network.AWS.SecretsManager.DeleteSecret
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SecretsManager.Types

-- | /See:/ 'newDeleteSecret' smart constructor.
data DeleteSecret = DeleteSecret'
  { -- | (Optional) Specifies the number of days that Secrets Manager waits
    -- before Secrets Manager can delete the secret. You can\'t use both this
    -- parameter and the @ForceDeleteWithoutRecovery@ parameter in the same API
    -- call.
    --
    -- This value can range from 7 to 30 days with a default value of 30.
    recoveryWindowInDays :: Core.Maybe Core.Integer,
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
    -- skip the normal waiting period before the permanent deletion that AWS
    -- would normally impose with the @RecoveryWindowInDays@ parameter. If you
    -- delete a secret with the @ForceDeleteWithouRecovery@ parameter, then you
    -- have no opportunity to recover the secret. You lose the secret
    -- permanently.
    --
    -- If you use this parameter and include a previously deleted or
    -- nonexistent secret, the operation does not return the error
    -- @ResourceNotFoundException@ in order to correctly handle retries.
    forceDeleteWithoutRecovery :: Core.Maybe Core.Bool,
    -- | Specifies the secret to delete. You can specify either the Amazon
    -- Resource Name (ARN) or the friendly name of the secret.
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
-- skip the normal waiting period before the permanent deletion that AWS
-- would normally impose with the @RecoveryWindowInDays@ parameter. If you
-- delete a secret with the @ForceDeleteWithouRecovery@ parameter, then you
-- have no opportunity to recover the secret. You lose the secret
-- permanently.
--
-- If you use this parameter and include a previously deleted or
-- nonexistent secret, the operation does not return the error
-- @ResourceNotFoundException@ in order to correctly handle retries.
--
-- 'secretId', 'deleteSecret_secretId' - Specifies the secret to delete. You can specify either the Amazon
-- Resource Name (ARN) or the friendly name of the secret.
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
newDeleteSecret ::
  -- | 'secretId'
  Core.Text ->
  DeleteSecret
newDeleteSecret pSecretId_ =
  DeleteSecret'
    { recoveryWindowInDays = Core.Nothing,
      forceDeleteWithoutRecovery = Core.Nothing,
      secretId = pSecretId_
    }

-- | (Optional) Specifies the number of days that Secrets Manager waits
-- before Secrets Manager can delete the secret. You can\'t use both this
-- parameter and the @ForceDeleteWithoutRecovery@ parameter in the same API
-- call.
--
-- This value can range from 7 to 30 days with a default value of 30.
deleteSecret_recoveryWindowInDays :: Lens.Lens' DeleteSecret (Core.Maybe Core.Integer)
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
-- skip the normal waiting period before the permanent deletion that AWS
-- would normally impose with the @RecoveryWindowInDays@ parameter. If you
-- delete a secret with the @ForceDeleteWithouRecovery@ parameter, then you
-- have no opportunity to recover the secret. You lose the secret
-- permanently.
--
-- If you use this parameter and include a previously deleted or
-- nonexistent secret, the operation does not return the error
-- @ResourceNotFoundException@ in order to correctly handle retries.
deleteSecret_forceDeleteWithoutRecovery :: Lens.Lens' DeleteSecret (Core.Maybe Core.Bool)
deleteSecret_forceDeleteWithoutRecovery = Lens.lens (\DeleteSecret' {forceDeleteWithoutRecovery} -> forceDeleteWithoutRecovery) (\s@DeleteSecret' {} a -> s {forceDeleteWithoutRecovery = a} :: DeleteSecret)

-- | Specifies the secret to delete. You can specify either the Amazon
-- Resource Name (ARN) or the friendly name of the secret.
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
deleteSecret_secretId :: Lens.Lens' DeleteSecret Core.Text
deleteSecret_secretId = Lens.lens (\DeleteSecret' {secretId} -> secretId) (\s@DeleteSecret' {} a -> s {secretId = a} :: DeleteSecret)

instance Core.AWSRequest DeleteSecret where
  type AWSResponse DeleteSecret = DeleteSecretResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSecretResponse'
            Core.<$> (x Core..?> "ARN")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "DeletionDate")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteSecret

instance Core.NFData DeleteSecret

instance Core.ToHeaders DeleteSecret where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("secretsmanager.DeleteSecret" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteSecret where
  toJSON DeleteSecret' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RecoveryWindowInDays" Core..=)
              Core.<$> recoveryWindowInDays,
            ("ForceDeleteWithoutRecovery" Core..=)
              Core.<$> forceDeleteWithoutRecovery,
            Core.Just ("SecretId" Core..= secretId)
          ]
      )

instance Core.ToPath DeleteSecret where
  toPath = Core.const "/"

instance Core.ToQuery DeleteSecret where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteSecretResponse' smart constructor.
data DeleteSecretResponse = DeleteSecretResponse'
  { -- | The ARN of the secret that is now scheduled for deletion.
    arn :: Core.Maybe Core.Text,
    -- | The friendly name of the secret currently scheduled for deletion.
    name :: Core.Maybe Core.Text,
    -- | The date and time after which this secret can be deleted by Secrets
    -- Manager and can no longer be restored. This value is the date and time
    -- of the delete request plus the number of days specified in
    -- @RecoveryWindowInDays@.
    deletionDate :: Core.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteSecretResponse
newDeleteSecretResponse pHttpStatus_ =
  DeleteSecretResponse'
    { arn = Core.Nothing,
      name = Core.Nothing,
      deletionDate = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the secret that is now scheduled for deletion.
deleteSecretResponse_arn :: Lens.Lens' DeleteSecretResponse (Core.Maybe Core.Text)
deleteSecretResponse_arn = Lens.lens (\DeleteSecretResponse' {arn} -> arn) (\s@DeleteSecretResponse' {} a -> s {arn = a} :: DeleteSecretResponse)

-- | The friendly name of the secret currently scheduled for deletion.
deleteSecretResponse_name :: Lens.Lens' DeleteSecretResponse (Core.Maybe Core.Text)
deleteSecretResponse_name = Lens.lens (\DeleteSecretResponse' {name} -> name) (\s@DeleteSecretResponse' {} a -> s {name = a} :: DeleteSecretResponse)

-- | The date and time after which this secret can be deleted by Secrets
-- Manager and can no longer be restored. This value is the date and time
-- of the delete request plus the number of days specified in
-- @RecoveryWindowInDays@.
deleteSecretResponse_deletionDate :: Lens.Lens' DeleteSecretResponse (Core.Maybe Core.UTCTime)
deleteSecretResponse_deletionDate = Lens.lens (\DeleteSecretResponse' {deletionDate} -> deletionDate) (\s@DeleteSecretResponse' {} a -> s {deletionDate = a} :: DeleteSecretResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
deleteSecretResponse_httpStatus :: Lens.Lens' DeleteSecretResponse Core.Int
deleteSecretResponse_httpStatus = Lens.lens (\DeleteSecretResponse' {httpStatus} -> httpStatus) (\s@DeleteSecretResponse' {} a -> s {httpStatus = a} :: DeleteSecretResponse)

instance Core.NFData DeleteSecretResponse
