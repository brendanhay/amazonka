{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.SetVaultAccessPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation configures an access policy for a vault and will overwrite an existing policy. To configure a vault access policy, send a PUT request to the @access-policy@ subresource of the vault. An access policy is specific to a vault and is also called a vault subresource. You can set one access policy per vault and the policy can be up to 20 KB in size. For more information about vault access policies, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-access-policy.html Amazon Glacier Access Control with Vault Access Policies> .
module Network.AWS.Glacier.SetVaultAccessPolicy
  ( -- * Creating a request
    SetVaultAccessPolicy (..),
    mkSetVaultAccessPolicy,

    -- ** Request lenses
    svapVaultName,
    svapAccountId,
    svapPolicy,

    -- * Destructuring the response
    SetVaultAccessPolicyResponse (..),
    mkSetVaultAccessPolicyResponse,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | SetVaultAccessPolicy input.
--
-- /See:/ 'mkSetVaultAccessPolicy' smart constructor.
data SetVaultAccessPolicy = SetVaultAccessPolicy'
  { -- | The name of the vault.
    vaultName :: Lude.Text,
    -- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
    accountId :: Lude.Text,
    -- | The vault access policy as a JSON string.
    policy :: Lude.Maybe VaultAccessPolicy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetVaultAccessPolicy' with the minimum fields required to make a request.
--
-- * 'vaultName' - The name of the vault.
-- * 'accountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
-- * 'policy' - The vault access policy as a JSON string.
mkSetVaultAccessPolicy ::
  -- | 'vaultName'
  Lude.Text ->
  -- | 'accountId'
  Lude.Text ->
  SetVaultAccessPolicy
mkSetVaultAccessPolicy pVaultName_ pAccountId_ =
  SetVaultAccessPolicy'
    { vaultName = pVaultName_,
      accountId = pAccountId_,
      policy = Lude.Nothing
    }

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svapVaultName :: Lens.Lens' SetVaultAccessPolicy Lude.Text
svapVaultName = Lens.lens (vaultName :: SetVaultAccessPolicy -> Lude.Text) (\s a -> s {vaultName = a} :: SetVaultAccessPolicy)
{-# DEPRECATED svapVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svapAccountId :: Lens.Lens' SetVaultAccessPolicy Lude.Text
svapAccountId = Lens.lens (accountId :: SetVaultAccessPolicy -> Lude.Text) (\s a -> s {accountId = a} :: SetVaultAccessPolicy)
{-# DEPRECATED svapAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The vault access policy as a JSON string.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svapPolicy :: Lens.Lens' SetVaultAccessPolicy (Lude.Maybe VaultAccessPolicy)
svapPolicy = Lens.lens (policy :: SetVaultAccessPolicy -> Lude.Maybe VaultAccessPolicy) (\s a -> s {policy = a} :: SetVaultAccessPolicy)
{-# DEPRECATED svapPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

instance Lude.AWSRequest SetVaultAccessPolicy where
  type Rs SetVaultAccessPolicy = SetVaultAccessPolicyResponse
  request = Req.putJSON glacierService
  response = Res.receiveNull SetVaultAccessPolicyResponse'

instance Lude.ToHeaders SetVaultAccessPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON SetVaultAccessPolicy where
  toJSON SetVaultAccessPolicy' {..} =
    Lude.object (Lude.catMaybes [("policy" Lude..=) Lude.<$> policy])

instance Lude.ToPath SetVaultAccessPolicy where
  toPath SetVaultAccessPolicy' {..} =
    Lude.mconcat
      [ "/",
        Lude.toBS accountId,
        "/vaults/",
        Lude.toBS vaultName,
        "/access-policy"
      ]

instance Lude.ToQuery SetVaultAccessPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSetVaultAccessPolicyResponse' smart constructor.
data SetVaultAccessPolicyResponse = SetVaultAccessPolicyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetVaultAccessPolicyResponse' with the minimum fields required to make a request.
mkSetVaultAccessPolicyResponse ::
  SetVaultAccessPolicyResponse
mkSetVaultAccessPolicyResponse = SetVaultAccessPolicyResponse'
