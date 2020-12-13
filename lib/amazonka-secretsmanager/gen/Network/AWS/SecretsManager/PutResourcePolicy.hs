{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.PutResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the contents of the specified resource-based permission policy to a secret. A resource-based policy is optional. Alternatively, you can use IAM identity-based policies that specify the secret's Amazon Resource Name (ARN) in the policy statement's @Resources@ element. You can also use a combination of both identity-based and resource-based policies. The affected users and roles receive the permissions that are permitted by all of the relevant policies. For more information, see <http://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access_resource-based-policies.html Using Resource-Based Policies for AWS Secrets Manager> . For the complete description of the AWS policy syntax and grammar, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies.html IAM JSON Policy Reference> in the /IAM User Guide/ .
--
-- __Minimum permissions__
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:PutResourcePolicy
--
--
-- __Related operations__
--
--     * To retrieve the resource policy attached to a secret, use 'GetResourcePolicy' .
--
--
--     * To delete the resource-based policy that's attached to a secret, use 'DeleteResourcePolicy' .
--
--
--     * To list all of the currently available secrets, use 'ListSecrets' .
module Network.AWS.SecretsManager.PutResourcePolicy
  ( -- * Creating a request
    PutResourcePolicy (..),
    mkPutResourcePolicy,

    -- ** Request lenses
    prpResourcePolicy,
    prpBlockPublicPolicy,
    prpSecretId,

    -- * Destructuring the response
    PutResourcePolicyResponse (..),
    mkPutResourcePolicyResponse,

    -- ** Response lenses
    prprsARN,
    prprsName,
    prprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SecretsManager.Types

-- | /See:/ 'mkPutResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'
  { -- | A JSON-formatted string that's constructed according to the grammar and syntax for an AWS resource-based policy. The policy in the string identifies who can access or manage this secret and its versions. For information on how to format a JSON parameter for the various command line tool environments, see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ .
    resourcePolicy :: Lude.Text,
    -- | Makes an optional API call to Zelkova to validate the Resource Policy to prevent broad access to your secret.
    blockPublicPolicy :: Lude.Maybe Lude.Bool,
    -- | Specifies the secret that you want to attach the resource-based policy to. You can specify either the ARN or the friendly name of the secret.
    secretId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutResourcePolicy' with the minimum fields required to make a request.
--
-- * 'resourcePolicy' - A JSON-formatted string that's constructed according to the grammar and syntax for an AWS resource-based policy. The policy in the string identifies who can access or manage this secret and its versions. For information on how to format a JSON parameter for the various command line tool environments, see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ .
-- * 'blockPublicPolicy' - Makes an optional API call to Zelkova to validate the Resource Policy to prevent broad access to your secret.
-- * 'secretId' - Specifies the secret that you want to attach the resource-based policy to. You can specify either the ARN or the friendly name of the secret.
mkPutResourcePolicy ::
  -- | 'resourcePolicy'
  Lude.Text ->
  -- | 'secretId'
  Lude.Text ->
  PutResourcePolicy
mkPutResourcePolicy pResourcePolicy_ pSecretId_ =
  PutResourcePolicy'
    { resourcePolicy = pResourcePolicy_,
      blockPublicPolicy = Lude.Nothing,
      secretId = pSecretId_
    }

-- | A JSON-formatted string that's constructed according to the grammar and syntax for an AWS resource-based policy. The policy in the string identifies who can access or manage this secret and its versions. For information on how to format a JSON parameter for the various command line tool environments, see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ .
--
-- /Note:/ Consider using 'resourcePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpResourcePolicy :: Lens.Lens' PutResourcePolicy Lude.Text
prpResourcePolicy = Lens.lens (resourcePolicy :: PutResourcePolicy -> Lude.Text) (\s a -> s {resourcePolicy = a} :: PutResourcePolicy)
{-# DEPRECATED prpResourcePolicy "Use generic-lens or generic-optics with 'resourcePolicy' instead." #-}

-- | Makes an optional API call to Zelkova to validate the Resource Policy to prevent broad access to your secret.
--
-- /Note:/ Consider using 'blockPublicPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpBlockPublicPolicy :: Lens.Lens' PutResourcePolicy (Lude.Maybe Lude.Bool)
prpBlockPublicPolicy = Lens.lens (blockPublicPolicy :: PutResourcePolicy -> Lude.Maybe Lude.Bool) (\s a -> s {blockPublicPolicy = a} :: PutResourcePolicy)
{-# DEPRECATED prpBlockPublicPolicy "Use generic-lens or generic-optics with 'blockPublicPolicy' instead." #-}

-- | Specifies the secret that you want to attach the resource-based policy to. You can specify either the ARN or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpSecretId :: Lens.Lens' PutResourcePolicy Lude.Text
prpSecretId = Lens.lens (secretId :: PutResourcePolicy -> Lude.Text) (\s a -> s {secretId = a} :: PutResourcePolicy)
{-# DEPRECATED prpSecretId "Use generic-lens or generic-optics with 'secretId' instead." #-}

instance Lude.AWSRequest PutResourcePolicy where
  type Rs PutResourcePolicy = PutResourcePolicyResponse
  request = Req.postJSON secretsManagerService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutResourcePolicyResponse'
            Lude.<$> (x Lude..?> "ARN")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutResourcePolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("secretsmanager.PutResourcePolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutResourcePolicy where
  toJSON PutResourcePolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourcePolicy" Lude..= resourcePolicy),
            ("BlockPublicPolicy" Lude..=) Lude.<$> blockPublicPolicy,
            Lude.Just ("SecretId" Lude..= secretId)
          ]
      )

instance Lude.ToPath PutResourcePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery PutResourcePolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'
  { -- | The ARN of the secret retrieved by the resource-based policy.
    arn :: Lude.Maybe Lude.Text,
    -- | The friendly name of the secret that the retrieved by the resource-based policy.
    name :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutResourcePolicyResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the secret retrieved by the resource-based policy.
-- * 'name' - The friendly name of the secret that the retrieved by the resource-based policy.
-- * 'responseStatus' - The response status code.
mkPutResourcePolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutResourcePolicyResponse
mkPutResourcePolicyResponse pResponseStatus_ =
  PutResourcePolicyResponse'
    { arn = Lude.Nothing,
      name = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the secret retrieved by the resource-based policy.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prprsARN :: Lens.Lens' PutResourcePolicyResponse (Lude.Maybe Lude.Text)
prprsARN = Lens.lens (arn :: PutResourcePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: PutResourcePolicyResponse)
{-# DEPRECATED prprsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The friendly name of the secret that the retrieved by the resource-based policy.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prprsName :: Lens.Lens' PutResourcePolicyResponse (Lude.Maybe Lude.Text)
prprsName = Lens.lens (name :: PutResourcePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: PutResourcePolicyResponse)
{-# DEPRECATED prprsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prprsResponseStatus :: Lens.Lens' PutResourcePolicyResponse Lude.Int
prprsResponseStatus = Lens.lens (responseStatus :: PutResourcePolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutResourcePolicyResponse)
{-# DEPRECATED prprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
