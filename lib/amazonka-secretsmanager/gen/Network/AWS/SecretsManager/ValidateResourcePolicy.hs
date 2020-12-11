{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.ValidateResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates the JSON text of the resource-based policy document attached to the specified secret. The JSON request string input and response output displays formatted code with white space and line breaks for better readability. Submit your input as a single line JSON string. A resource-based policy is optional.
module Network.AWS.SecretsManager.ValidateResourcePolicy
  ( -- * Creating a request
    ValidateResourcePolicy (..),
    mkValidateResourcePolicy,

    -- ** Request lenses
    vrpSecretId,
    vrpResourcePolicy,

    -- * Destructuring the response
    ValidateResourcePolicyResponse (..),
    mkValidateResourcePolicyResponse,

    -- ** Response lenses
    vrprsValidationErrors,
    vrprsPolicyValidationPassed,
    vrprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SecretsManager.Types

-- | /See:/ 'mkValidateResourcePolicy' smart constructor.
data ValidateResourcePolicy = ValidateResourcePolicy'
  { secretId ::
      Lude.Maybe Lude.Text,
    resourcePolicy :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ValidateResourcePolicy' with the minimum fields required to make a request.
--
-- * 'resourcePolicy' - Identifies the Resource Policy attached to the secret.
-- * 'secretId' - The identifier for the secret that you want to validate a resource policy. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
mkValidateResourcePolicy ::
  -- | 'resourcePolicy'
  Lude.Text ->
  ValidateResourcePolicy
mkValidateResourcePolicy pResourcePolicy_ =
  ValidateResourcePolicy'
    { secretId = Lude.Nothing,
      resourcePolicy = pResourcePolicy_
    }

-- | The identifier for the secret that you want to validate a resource policy. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vrpSecretId :: Lens.Lens' ValidateResourcePolicy (Lude.Maybe Lude.Text)
vrpSecretId = Lens.lens (secretId :: ValidateResourcePolicy -> Lude.Maybe Lude.Text) (\s a -> s {secretId = a} :: ValidateResourcePolicy)
{-# DEPRECATED vrpSecretId "Use generic-lens or generic-optics with 'secretId' instead." #-}

-- | Identifies the Resource Policy attached to the secret.
--
-- /Note:/ Consider using 'resourcePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vrpResourcePolicy :: Lens.Lens' ValidateResourcePolicy Lude.Text
vrpResourcePolicy = Lens.lens (resourcePolicy :: ValidateResourcePolicy -> Lude.Text) (\s a -> s {resourcePolicy = a} :: ValidateResourcePolicy)
{-# DEPRECATED vrpResourcePolicy "Use generic-lens or generic-optics with 'resourcePolicy' instead." #-}

instance Lude.AWSRequest ValidateResourcePolicy where
  type Rs ValidateResourcePolicy = ValidateResourcePolicyResponse
  request = Req.postJSON secretsManagerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ValidateResourcePolicyResponse'
            Lude.<$> (x Lude..?> "ValidationErrors" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "PolicyValidationPassed")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ValidateResourcePolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("secretsmanager.ValidateResourcePolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ValidateResourcePolicy where
  toJSON ValidateResourcePolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SecretId" Lude..=) Lude.<$> secretId,
            Lude.Just ("ResourcePolicy" Lude..= resourcePolicy)
          ]
      )

instance Lude.ToPath ValidateResourcePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery ValidateResourcePolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkValidateResourcePolicyResponse' smart constructor.
data ValidateResourcePolicyResponse = ValidateResourcePolicyResponse'
  { validationErrors ::
      Lude.Maybe
        [ValidationErrorsEntry],
    policyValidationPassed ::
      Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'ValidateResourcePolicyResponse' with the minimum fields required to make a request.
--
-- * 'policyValidationPassed' - Returns a message stating that your Reource Policy passed validation.
-- * 'responseStatus' - The response status code.
-- * 'validationErrors' - Returns an error message if your policy doesn't pass validatation.
mkValidateResourcePolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ValidateResourcePolicyResponse
mkValidateResourcePolicyResponse pResponseStatus_ =
  ValidateResourcePolicyResponse'
    { validationErrors = Lude.Nothing,
      policyValidationPassed = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns an error message if your policy doesn't pass validatation.
--
-- /Note:/ Consider using 'validationErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vrprsValidationErrors :: Lens.Lens' ValidateResourcePolicyResponse (Lude.Maybe [ValidationErrorsEntry])
vrprsValidationErrors = Lens.lens (validationErrors :: ValidateResourcePolicyResponse -> Lude.Maybe [ValidationErrorsEntry]) (\s a -> s {validationErrors = a} :: ValidateResourcePolicyResponse)
{-# DEPRECATED vrprsValidationErrors "Use generic-lens or generic-optics with 'validationErrors' instead." #-}

-- | Returns a message stating that your Reource Policy passed validation.
--
-- /Note:/ Consider using 'policyValidationPassed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vrprsPolicyValidationPassed :: Lens.Lens' ValidateResourcePolicyResponse (Lude.Maybe Lude.Bool)
vrprsPolicyValidationPassed = Lens.lens (policyValidationPassed :: ValidateResourcePolicyResponse -> Lude.Maybe Lude.Bool) (\s a -> s {policyValidationPassed = a} :: ValidateResourcePolicyResponse)
{-# DEPRECATED vrprsPolicyValidationPassed "Use generic-lens or generic-optics with 'policyValidationPassed' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vrprsResponseStatus :: Lens.Lens' ValidateResourcePolicyResponse Lude.Int
vrprsResponseStatus = Lens.lens (responseStatus :: ValidateResourcePolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ValidateResourcePolicyResponse)
{-# DEPRECATED vrprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
