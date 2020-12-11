{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.DeleteVaultAccessPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes the access policy associated with the specified vault. The operation is eventually consistent; that is, it might take some time for Amazon S3 Glacier to completely remove the access policy, and you might still see the effect of the policy for a short time after you send the delete request.
--
-- This operation is idempotent. You can invoke delete multiple times, even if there is no policy associated with the vault. For more information about vault access policies, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-access-policy.html Amazon Glacier Access Control with Vault Access Policies> .
module Network.AWS.Glacier.DeleteVaultAccessPolicy
  ( -- * Creating a request
    DeleteVaultAccessPolicy (..),
    mkDeleteVaultAccessPolicy,

    -- ** Request lenses
    dvapAccountId,
    dvapVaultName,

    -- * Destructuring the response
    DeleteVaultAccessPolicyResponse (..),
    mkDeleteVaultAccessPolicyResponse,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | DeleteVaultAccessPolicy input.
--
-- /See:/ 'mkDeleteVaultAccessPolicy' smart constructor.
data DeleteVaultAccessPolicy = DeleteVaultAccessPolicy'
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

-- | Creates a value of 'DeleteVaultAccessPolicy' with the minimum fields required to make a request.
--
-- * 'accountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
-- * 'vaultName' - The name of the vault.
mkDeleteVaultAccessPolicy ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'vaultName'
  Lude.Text ->
  DeleteVaultAccessPolicy
mkDeleteVaultAccessPolicy pAccountId_ pVaultName_ =
  DeleteVaultAccessPolicy'
    { accountId = pAccountId_,
      vaultName = pVaultName_
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvapAccountId :: Lens.Lens' DeleteVaultAccessPolicy Lude.Text
dvapAccountId = Lens.lens (accountId :: DeleteVaultAccessPolicy -> Lude.Text) (\s a -> s {accountId = a} :: DeleteVaultAccessPolicy)
{-# DEPRECATED dvapAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvapVaultName :: Lens.Lens' DeleteVaultAccessPolicy Lude.Text
dvapVaultName = Lens.lens (vaultName :: DeleteVaultAccessPolicy -> Lude.Text) (\s a -> s {vaultName = a} :: DeleteVaultAccessPolicy)
{-# DEPRECATED dvapVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

instance Lude.AWSRequest DeleteVaultAccessPolicy where
  type Rs DeleteVaultAccessPolicy = DeleteVaultAccessPolicyResponse
  request = Req.delete glacierService
  response = Res.receiveNull DeleteVaultAccessPolicyResponse'

instance Lude.ToHeaders DeleteVaultAccessPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteVaultAccessPolicy where
  toPath DeleteVaultAccessPolicy' {..} =
    Lude.mconcat
      [ "/",
        Lude.toBS accountId,
        "/vaults/",
        Lude.toBS vaultName,
        "/access-policy"
      ]

instance Lude.ToQuery DeleteVaultAccessPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteVaultAccessPolicyResponse' smart constructor.
data DeleteVaultAccessPolicyResponse = DeleteVaultAccessPolicyResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVaultAccessPolicyResponse' with the minimum fields required to make a request.
mkDeleteVaultAccessPolicyResponse ::
  DeleteVaultAccessPolicyResponse
mkDeleteVaultAccessPolicyResponse =
  DeleteVaultAccessPolicyResponse'
