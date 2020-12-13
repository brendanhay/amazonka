{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.DeleteSecret
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an entire secret and all of its versions. You can optionally include a recovery window during which you can restore the secret. If you don't specify a recovery window value, the operation defaults to 30 days. Secrets Manager attaches a @DeletionDate@ stamp to the secret that specifies the end of the recovery window. At the end of the recovery window, Secrets Manager deletes the secret permanently.
--
-- At any time before recovery window ends, you can use 'RestoreSecret' to remove the @DeletionDate@ and cancel the deletion of the secret.
-- You cannot access the encrypted secret information in any secret that is scheduled for deletion. If you need to access that information, you must cancel the deletion with 'RestoreSecret' and then retrieve the information.
-- __Minimum permissions__
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:DeleteSecret
--
--
-- __Related operations__
--
--     * To create a secret, use 'CreateSecret' .
--
--
--     * To cancel deletion of a version of a secret before the recovery window has expired, use 'RestoreSecret' .
module Network.AWS.SecretsManager.DeleteSecret
  ( -- * Creating a request
    DeleteSecret (..),
    mkDeleteSecret,

    -- ** Request lenses
    dSecretId,
    dRecoveryWindowInDays,
    dForceDeleteWithoutRecovery,

    -- * Destructuring the response
    DeleteSecretResponse (..),
    mkDeleteSecretResponse,

    -- ** Response lenses
    dsrsARN,
    dsrsName,
    dsrsDeletionDate,
    dsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SecretsManager.Types

-- | /See:/ 'mkDeleteSecret' smart constructor.
data DeleteSecret = DeleteSecret'
  { -- | Specifies the secret that you want to delete. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
    secretId :: Lude.Text,
    -- | (Optional) Specifies the number of days that Secrets Manager waits before it can delete the secret. You can't use both this parameter and the @ForceDeleteWithoutRecovery@ parameter in the same API call.
    --
    -- This value can range from 7 to 30 days. The default value is 30.
    recoveryWindowInDays :: Lude.Maybe Lude.Integer,
    -- | (Optional) Specifies that the secret is to be deleted without any recovery window. You can't use both this parameter and the @RecoveryWindowInDays@ parameter in the same API call.
    --
    -- An asynchronous background process performs the actual deletion, so there can be a short delay before the operation completes. If you write code to delete and then immediately recreate a secret with the same name, ensure that your code includes appropriate back off and retry logic.
    -- /Important:/ Use this parameter with caution. This parameter causes the operation to skip the normal waiting period before the permanent deletion that AWS would normally impose with the @RecoveryWindowInDays@ parameter. If you delete a secret with the @ForceDeleteWithouRecovery@ parameter, then you have no opportunity to recover the secret. It is permanently lost.
    forceDeleteWithoutRecovery :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSecret' with the minimum fields required to make a request.
--
-- * 'secretId' - Specifies the secret that you want to delete. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
-- * 'recoveryWindowInDays' - (Optional) Specifies the number of days that Secrets Manager waits before it can delete the secret. You can't use both this parameter and the @ForceDeleteWithoutRecovery@ parameter in the same API call.
--
-- This value can range from 7 to 30 days. The default value is 30.
-- * 'forceDeleteWithoutRecovery' - (Optional) Specifies that the secret is to be deleted without any recovery window. You can't use both this parameter and the @RecoveryWindowInDays@ parameter in the same API call.
--
-- An asynchronous background process performs the actual deletion, so there can be a short delay before the operation completes. If you write code to delete and then immediately recreate a secret with the same name, ensure that your code includes appropriate back off and retry logic.
-- /Important:/ Use this parameter with caution. This parameter causes the operation to skip the normal waiting period before the permanent deletion that AWS would normally impose with the @RecoveryWindowInDays@ parameter. If you delete a secret with the @ForceDeleteWithouRecovery@ parameter, then you have no opportunity to recover the secret. It is permanently lost.
mkDeleteSecret ::
  -- | 'secretId'
  Lude.Text ->
  DeleteSecret
mkDeleteSecret pSecretId_ =
  DeleteSecret'
    { secretId = pSecretId_,
      recoveryWindowInDays = Lude.Nothing,
      forceDeleteWithoutRecovery = Lude.Nothing
    }

-- | Specifies the secret that you want to delete. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSecretId :: Lens.Lens' DeleteSecret Lude.Text
dSecretId = Lens.lens (secretId :: DeleteSecret -> Lude.Text) (\s a -> s {secretId = a} :: DeleteSecret)
{-# DEPRECATED dSecretId "Use generic-lens or generic-optics with 'secretId' instead." #-}

-- | (Optional) Specifies the number of days that Secrets Manager waits before it can delete the secret. You can't use both this parameter and the @ForceDeleteWithoutRecovery@ parameter in the same API call.
--
-- This value can range from 7 to 30 days. The default value is 30.
--
-- /Note:/ Consider using 'recoveryWindowInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRecoveryWindowInDays :: Lens.Lens' DeleteSecret (Lude.Maybe Lude.Integer)
dRecoveryWindowInDays = Lens.lens (recoveryWindowInDays :: DeleteSecret -> Lude.Maybe Lude.Integer) (\s a -> s {recoveryWindowInDays = a} :: DeleteSecret)
{-# DEPRECATED dRecoveryWindowInDays "Use generic-lens or generic-optics with 'recoveryWindowInDays' instead." #-}

-- | (Optional) Specifies that the secret is to be deleted without any recovery window. You can't use both this parameter and the @RecoveryWindowInDays@ parameter in the same API call.
--
-- An asynchronous background process performs the actual deletion, so there can be a short delay before the operation completes. If you write code to delete and then immediately recreate a secret with the same name, ensure that your code includes appropriate back off and retry logic.
-- /Important:/ Use this parameter with caution. This parameter causes the operation to skip the normal waiting period before the permanent deletion that AWS would normally impose with the @RecoveryWindowInDays@ parameter. If you delete a secret with the @ForceDeleteWithouRecovery@ parameter, then you have no opportunity to recover the secret. It is permanently lost.
--
-- /Note:/ Consider using 'forceDeleteWithoutRecovery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dForceDeleteWithoutRecovery :: Lens.Lens' DeleteSecret (Lude.Maybe Lude.Bool)
dForceDeleteWithoutRecovery = Lens.lens (forceDeleteWithoutRecovery :: DeleteSecret -> Lude.Maybe Lude.Bool) (\s a -> s {forceDeleteWithoutRecovery = a} :: DeleteSecret)
{-# DEPRECATED dForceDeleteWithoutRecovery "Use generic-lens or generic-optics with 'forceDeleteWithoutRecovery' instead." #-}

instance Lude.AWSRequest DeleteSecret where
  type Rs DeleteSecret = DeleteSecretResponse
  request = Req.postJSON secretsManagerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteSecretResponse'
            Lude.<$> (x Lude..?> "ARN")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "DeletionDate")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteSecret where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("secretsmanager.DeleteSecret" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteSecret where
  toJSON DeleteSecret' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SecretId" Lude..= secretId),
            ("RecoveryWindowInDays" Lude..=) Lude.<$> recoveryWindowInDays,
            ("ForceDeleteWithoutRecovery" Lude..=)
              Lude.<$> forceDeleteWithoutRecovery
          ]
      )

instance Lude.ToPath DeleteSecret where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteSecret where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteSecretResponse' smart constructor.
data DeleteSecretResponse = DeleteSecretResponse'
  { -- | The ARN of the secret that is now scheduled for deletion.
    arn :: Lude.Maybe Lude.Text,
    -- | The friendly name of the secret that is now scheduled for deletion.
    name :: Lude.Maybe Lude.Text,
    -- | The date and time after which this secret can be deleted by Secrets Manager and can no longer be restored. This value is the date and time of the delete request plus the number of days specified in @RecoveryWindowInDays@ .
    deletionDate :: Lude.Maybe Lude.Timestamp,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSecretResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the secret that is now scheduled for deletion.
-- * 'name' - The friendly name of the secret that is now scheduled for deletion.
-- * 'deletionDate' - The date and time after which this secret can be deleted by Secrets Manager and can no longer be restored. This value is the date and time of the delete request plus the number of days specified in @RecoveryWindowInDays@ .
-- * 'responseStatus' - The response status code.
mkDeleteSecretResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteSecretResponse
mkDeleteSecretResponse pResponseStatus_ =
  DeleteSecretResponse'
    { arn = Lude.Nothing,
      name = Lude.Nothing,
      deletionDate = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the secret that is now scheduled for deletion.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsARN :: Lens.Lens' DeleteSecretResponse (Lude.Maybe Lude.Text)
dsrsARN = Lens.lens (arn :: DeleteSecretResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DeleteSecretResponse)
{-# DEPRECATED dsrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The friendly name of the secret that is now scheduled for deletion.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsName :: Lens.Lens' DeleteSecretResponse (Lude.Maybe Lude.Text)
dsrsName = Lens.lens (name :: DeleteSecretResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DeleteSecretResponse)
{-# DEPRECATED dsrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The date and time after which this secret can be deleted by Secrets Manager and can no longer be restored. This value is the date and time of the delete request plus the number of days specified in @RecoveryWindowInDays@ .
--
-- /Note:/ Consider using 'deletionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsDeletionDate :: Lens.Lens' DeleteSecretResponse (Lude.Maybe Lude.Timestamp)
dsrsDeletionDate = Lens.lens (deletionDate :: DeleteSecretResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {deletionDate = a} :: DeleteSecretResponse)
{-# DEPRECATED dsrsDeletionDate "Use generic-lens or generic-optics with 'deletionDate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DeleteSecretResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DeleteSecretResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteSecretResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
