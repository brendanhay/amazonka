{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.DeleteResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the resource-based permission policy attached to the secret.
--
-- __Minimum permissions__
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:DeleteResourcePolicy
--
--
-- __Related operations__
--
--     * To attach a resource policy to a secret, use 'PutResourcePolicy' .
--
--
--     * To retrieve the current resource-based policy that's attached to a secret, use 'GetResourcePolicy' .
--
--
--     * To list all of the currently available secrets, use 'ListSecrets' .
module Network.AWS.SecretsManager.DeleteResourcePolicy
  ( -- * Creating a request
    DeleteResourcePolicy (..),
    mkDeleteResourcePolicy,

    -- ** Request lenses
    drpSecretId,

    -- * Destructuring the response
    DeleteResourcePolicyResponse (..),
    mkDeleteResourcePolicyResponse,

    -- ** Response lenses
    drprsARN,
    drprsName,
    drprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SecretsManager.Types

-- | /See:/ 'mkDeleteResourcePolicy' smart constructor.
newtype DeleteResourcePolicy = DeleteResourcePolicy'
  { secretId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteResourcePolicy' with the minimum fields required to make a request.
--
-- * 'secretId' - Specifies the secret that you want to delete the attached resource-based policy for. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
mkDeleteResourcePolicy ::
  -- | 'secretId'
  Lude.Text ->
  DeleteResourcePolicy
mkDeleteResourcePolicy pSecretId_ =
  DeleteResourcePolicy' {secretId = pSecretId_}

-- | Specifies the secret that you want to delete the attached resource-based policy for. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpSecretId :: Lens.Lens' DeleteResourcePolicy Lude.Text
drpSecretId = Lens.lens (secretId :: DeleteResourcePolicy -> Lude.Text) (\s a -> s {secretId = a} :: DeleteResourcePolicy)
{-# DEPRECATED drpSecretId "Use generic-lens or generic-optics with 'secretId' instead." #-}

instance Lude.AWSRequest DeleteResourcePolicy where
  type Rs DeleteResourcePolicy = DeleteResourcePolicyResponse
  request = Req.postJSON secretsManagerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteResourcePolicyResponse'
            Lude.<$> (x Lude..?> "ARN")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteResourcePolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("secretsmanager.DeleteResourcePolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteResourcePolicy where
  toJSON DeleteResourcePolicy' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("SecretId" Lude..= secretId)])

instance Lude.ToPath DeleteResourcePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteResourcePolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteResourcePolicyResponse' smart constructor.
data DeleteResourcePolicyResponse = DeleteResourcePolicyResponse'
  { arn ::
      Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteResourcePolicyResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the secret that the resource-based policy was deleted for.
-- * 'name' - The friendly name of the secret that the resource-based policy was deleted for.
-- * 'responseStatus' - The response status code.
mkDeleteResourcePolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteResourcePolicyResponse
mkDeleteResourcePolicyResponse pResponseStatus_ =
  DeleteResourcePolicyResponse'
    { arn = Lude.Nothing,
      name = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the secret that the resource-based policy was deleted for.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprsARN :: Lens.Lens' DeleteResourcePolicyResponse (Lude.Maybe Lude.Text)
drprsARN = Lens.lens (arn :: DeleteResourcePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DeleteResourcePolicyResponse)
{-# DEPRECATED drprsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The friendly name of the secret that the resource-based policy was deleted for.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprsName :: Lens.Lens' DeleteResourcePolicyResponse (Lude.Maybe Lude.Text)
drprsName = Lens.lens (name :: DeleteResourcePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DeleteResourcePolicyResponse)
{-# DEPRECATED drprsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprsResponseStatus :: Lens.Lens' DeleteResourcePolicyResponse Lude.Int
drprsResponseStatus = Lens.lens (responseStatus :: DeleteResourcePolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteResourcePolicyResponse)
{-# DEPRECATED drprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
