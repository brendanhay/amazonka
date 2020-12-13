{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.RestoreSecret
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the scheduled deletion of a secret by removing the @DeletedDate@ time stamp. This makes the secret accessible to query once again.
--
-- __Minimum permissions__
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:RestoreSecret
--
--
-- __Related operations__
--
--     * To delete a secret, use 'DeleteSecret' .
module Network.AWS.SecretsManager.RestoreSecret
  ( -- * Creating a request
    RestoreSecret (..),
    mkRestoreSecret,

    -- ** Request lenses
    rsSecretId,

    -- * Destructuring the response
    RestoreSecretResponse (..),
    mkRestoreSecretResponse,

    -- ** Response lenses
    rrsARN,
    rrsName,
    rrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SecretsManager.Types

-- | /See:/ 'mkRestoreSecret' smart constructor.
newtype RestoreSecret = RestoreSecret'
  { -- | Specifies the secret that you want to restore from a previously scheduled deletion. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
    secretId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreSecret' with the minimum fields required to make a request.
--
-- * 'secretId' - Specifies the secret that you want to restore from a previously scheduled deletion. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
mkRestoreSecret ::
  -- | 'secretId'
  Lude.Text ->
  RestoreSecret
mkRestoreSecret pSecretId_ = RestoreSecret' {secretId = pSecretId_}

-- | Specifies the secret that you want to restore from a previously scheduled deletion. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsSecretId :: Lens.Lens' RestoreSecret Lude.Text
rsSecretId = Lens.lens (secretId :: RestoreSecret -> Lude.Text) (\s a -> s {secretId = a} :: RestoreSecret)
{-# DEPRECATED rsSecretId "Use generic-lens or generic-optics with 'secretId' instead." #-}

instance Lude.AWSRequest RestoreSecret where
  type Rs RestoreSecret = RestoreSecretResponse
  request = Req.postJSON secretsManagerService
  response =
    Res.receiveJSON
      ( \s h x ->
          RestoreSecretResponse'
            Lude.<$> (x Lude..?> "ARN")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RestoreSecret where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("secretsmanager.RestoreSecret" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RestoreSecret where
  toJSON RestoreSecret' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("SecretId" Lude..= secretId)])

instance Lude.ToPath RestoreSecret where
  toPath = Lude.const "/"

instance Lude.ToQuery RestoreSecret where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRestoreSecretResponse' smart constructor.
data RestoreSecretResponse = RestoreSecretResponse'
  { -- | The ARN of the secret that was restored.
    arn :: Lude.Maybe Lude.Text,
    -- | The friendly name of the secret that was restored.
    name :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreSecretResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the secret that was restored.
-- * 'name' - The friendly name of the secret that was restored.
-- * 'responseStatus' - The response status code.
mkRestoreSecretResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RestoreSecretResponse
mkRestoreSecretResponse pResponseStatus_ =
  RestoreSecretResponse'
    { arn = Lude.Nothing,
      name = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the secret that was restored.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsARN :: Lens.Lens' RestoreSecretResponse (Lude.Maybe Lude.Text)
rrsARN = Lens.lens (arn :: RestoreSecretResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: RestoreSecretResponse)
{-# DEPRECATED rrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The friendly name of the secret that was restored.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsName :: Lens.Lens' RestoreSecretResponse (Lude.Maybe Lude.Text)
rrsName = Lens.lens (name :: RestoreSecretResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: RestoreSecretResponse)
{-# DEPRECATED rrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsResponseStatus :: Lens.Lens' RestoreSecretResponse Lude.Int
rrsResponseStatus = Lens.lens (responseStatus :: RestoreSecretResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RestoreSecretResponse)
{-# DEPRECATED rrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
