{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.GetResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the JSON text of the resource-based policy document attached to the specified secret. The JSON request string input and response output displays formatted code with white space and line breaks for better readability. Submit your input as a single line JSON string.
--
-- __Minimum permissions__
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:GetResourcePolicy
--
--
-- __Related operations__
--
--     * To attach a resource policy to a secret, use 'PutResourcePolicy' .
--
--
--     * To delete the resource-based policy attached to a secret, use 'DeleteResourcePolicy' .
--
--
--     * To list all of the currently available secrets, use 'ListSecrets' .
module Network.AWS.SecretsManager.GetResourcePolicy
  ( -- * Creating a request
    GetResourcePolicy (..),
    mkGetResourcePolicy,

    -- ** Request lenses
    grpSecretId,

    -- * Destructuring the response
    GetResourcePolicyResponse (..),
    mkGetResourcePolicyResponse,

    -- ** Response lenses
    grprsResourcePolicy,
    grprsARN,
    grprsName,
    grprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SecretsManager.Types

-- | /See:/ 'mkGetResourcePolicy' smart constructor.
newtype GetResourcePolicy = GetResourcePolicy'
  { -- | Specifies the secret that you want to retrieve the attached resource-based policy for. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
    secretId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetResourcePolicy' with the minimum fields required to make a request.
--
-- * 'secretId' - Specifies the secret that you want to retrieve the attached resource-based policy for. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
mkGetResourcePolicy ::
  -- | 'secretId'
  Lude.Text ->
  GetResourcePolicy
mkGetResourcePolicy pSecretId_ =
  GetResourcePolicy' {secretId = pSecretId_}

-- | Specifies the secret that you want to retrieve the attached resource-based policy for. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpSecretId :: Lens.Lens' GetResourcePolicy Lude.Text
grpSecretId = Lens.lens (secretId :: GetResourcePolicy -> Lude.Text) (\s a -> s {secretId = a} :: GetResourcePolicy)
{-# DEPRECATED grpSecretId "Use generic-lens or generic-optics with 'secretId' instead." #-}

instance Lude.AWSRequest GetResourcePolicy where
  type Rs GetResourcePolicy = GetResourcePolicyResponse
  request = Req.postJSON secretsManagerService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetResourcePolicyResponse'
            Lude.<$> (x Lude..?> "ResourcePolicy")
            Lude.<*> (x Lude..?> "ARN")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetResourcePolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("secretsmanager.GetResourcePolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetResourcePolicy where
  toJSON GetResourcePolicy' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("SecretId" Lude..= secretId)])

instance Lude.ToPath GetResourcePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery GetResourcePolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetResourcePolicyResponse' smart constructor.
data GetResourcePolicyResponse = GetResourcePolicyResponse'
  { -- | A JSON-formatted string that describes the permissions that are associated with the attached secret. These permissions are combined with any permissions that are associated with the user or role that attempts to access this secret. The combined permissions specify who can access the secret and what actions they can perform. For more information, see <http://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and Access Control for AWS Secrets Manager> in the /AWS Secrets Manager User Guide/ .
    resourcePolicy :: Lude.Maybe Lude.Text,
    -- | The ARN of the secret that the resource-based policy was retrieved for.
    arn :: Lude.Maybe Lude.Text,
    -- | The friendly name of the secret that the resource-based policy was retrieved for.
    name :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetResourcePolicyResponse' with the minimum fields required to make a request.
--
-- * 'resourcePolicy' - A JSON-formatted string that describes the permissions that are associated with the attached secret. These permissions are combined with any permissions that are associated with the user or role that attempts to access this secret. The combined permissions specify who can access the secret and what actions they can perform. For more information, see <http://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and Access Control for AWS Secrets Manager> in the /AWS Secrets Manager User Guide/ .
-- * 'arn' - The ARN of the secret that the resource-based policy was retrieved for.
-- * 'name' - The friendly name of the secret that the resource-based policy was retrieved for.
-- * 'responseStatus' - The response status code.
mkGetResourcePolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetResourcePolicyResponse
mkGetResourcePolicyResponse pResponseStatus_ =
  GetResourcePolicyResponse'
    { resourcePolicy = Lude.Nothing,
      arn = Lude.Nothing,
      name = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A JSON-formatted string that describes the permissions that are associated with the attached secret. These permissions are combined with any permissions that are associated with the user or role that attempts to access this secret. The combined permissions specify who can access the secret and what actions they can perform. For more information, see <http://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and Access Control for AWS Secrets Manager> in the /AWS Secrets Manager User Guide/ .
--
-- /Note:/ Consider using 'resourcePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprsResourcePolicy :: Lens.Lens' GetResourcePolicyResponse (Lude.Maybe Lude.Text)
grprsResourcePolicy = Lens.lens (resourcePolicy :: GetResourcePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {resourcePolicy = a} :: GetResourcePolicyResponse)
{-# DEPRECATED grprsResourcePolicy "Use generic-lens or generic-optics with 'resourcePolicy' instead." #-}

-- | The ARN of the secret that the resource-based policy was retrieved for.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprsARN :: Lens.Lens' GetResourcePolicyResponse (Lude.Maybe Lude.Text)
grprsARN = Lens.lens (arn :: GetResourcePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GetResourcePolicyResponse)
{-# DEPRECATED grprsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The friendly name of the secret that the resource-based policy was retrieved for.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprsName :: Lens.Lens' GetResourcePolicyResponse (Lude.Maybe Lude.Text)
grprsName = Lens.lens (name :: GetResourcePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetResourcePolicyResponse)
{-# DEPRECATED grprsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprsResponseStatus :: Lens.Lens' GetResourcePolicyResponse Lude.Int
grprsResponseStatus = Lens.lens (responseStatus :: GetResourcePolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetResourcePolicyResponse)
{-# DEPRECATED grprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
