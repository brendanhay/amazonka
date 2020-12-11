{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.GetVaultAccessPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation retrieves the @access-policy@ subresource set on the vault; for more information on setting this subresource, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-SetVaultAccessPolicy.html Set Vault Access Policy (PUT access-policy)> . If there is no access policy set on the vault, the operation returns a @404 Not found@ error. For more information about vault access policies, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-access-policy.html Amazon Glacier Access Control with Vault Access Policies> .
module Network.AWS.Glacier.GetVaultAccessPolicy
  ( -- * Creating a request
    GetVaultAccessPolicy (..),
    mkGetVaultAccessPolicy,

    -- ** Request lenses
    gvapAccountId,
    gvapVaultName,

    -- * Destructuring the response
    GetVaultAccessPolicyResponse (..),
    mkGetVaultAccessPolicyResponse,

    -- ** Response lenses
    gvaprsPolicy,
    gvaprsResponseStatus,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input for GetVaultAccessPolicy.
--
-- /See:/ 'mkGetVaultAccessPolicy' smart constructor.
data GetVaultAccessPolicy = GetVaultAccessPolicy'
  { accountId ::
      Lude.Text,
    vaultName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetVaultAccessPolicy' with the minimum fields required to make a request.
--
-- * 'accountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
-- * 'vaultName' - The name of the vault.
mkGetVaultAccessPolicy ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'vaultName'
  Lude.Text ->
  GetVaultAccessPolicy
mkGetVaultAccessPolicy pAccountId_ pVaultName_ =
  GetVaultAccessPolicy'
    { accountId = pAccountId_,
      vaultName = pVaultName_
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvapAccountId :: Lens.Lens' GetVaultAccessPolicy Lude.Text
gvapAccountId = Lens.lens (accountId :: GetVaultAccessPolicy -> Lude.Text) (\s a -> s {accountId = a} :: GetVaultAccessPolicy)
{-# DEPRECATED gvapAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvapVaultName :: Lens.Lens' GetVaultAccessPolicy Lude.Text
gvapVaultName = Lens.lens (vaultName :: GetVaultAccessPolicy -> Lude.Text) (\s a -> s {vaultName = a} :: GetVaultAccessPolicy)
{-# DEPRECATED gvapVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

instance Lude.AWSRequest GetVaultAccessPolicy where
  type Rs GetVaultAccessPolicy = GetVaultAccessPolicyResponse
  request = Req.get glacierService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetVaultAccessPolicyResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetVaultAccessPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetVaultAccessPolicy where
  toPath GetVaultAccessPolicy' {..} =
    Lude.mconcat
      [ "/",
        Lude.toBS accountId,
        "/vaults/",
        Lude.toBS vaultName,
        "/access-policy"
      ]

instance Lude.ToQuery GetVaultAccessPolicy where
  toQuery = Lude.const Lude.mempty

-- | Output for GetVaultAccessPolicy.
--
-- /See:/ 'mkGetVaultAccessPolicyResponse' smart constructor.
data GetVaultAccessPolicyResponse = GetVaultAccessPolicyResponse'
  { policy ::
      Lude.Maybe VaultAccessPolicy,
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

-- | Creates a value of 'GetVaultAccessPolicyResponse' with the minimum fields required to make a request.
--
-- * 'policy' - Contains the returned vault access policy as a JSON string.
-- * 'responseStatus' - The response status code.
mkGetVaultAccessPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetVaultAccessPolicyResponse
mkGetVaultAccessPolicyResponse pResponseStatus_ =
  GetVaultAccessPolicyResponse'
    { policy = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains the returned vault access policy as a JSON string.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvaprsPolicy :: Lens.Lens' GetVaultAccessPolicyResponse (Lude.Maybe VaultAccessPolicy)
gvaprsPolicy = Lens.lens (policy :: GetVaultAccessPolicyResponse -> Lude.Maybe VaultAccessPolicy) (\s a -> s {policy = a} :: GetVaultAccessPolicyResponse)
{-# DEPRECATED gvaprsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvaprsResponseStatus :: Lens.Lens' GetVaultAccessPolicyResponse Lude.Int
gvaprsResponseStatus = Lens.lens (responseStatus :: GetVaultAccessPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetVaultAccessPolicyResponse)
{-# DEPRECATED gvaprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
