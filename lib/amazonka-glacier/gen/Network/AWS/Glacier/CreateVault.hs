{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.CreateVault
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation creates a new vault with the specified name. The name of the vault must be unique within a region for an AWS account. You can create up to 1,000 vaults per account. If you need to create more vaults, contact Amazon S3 Glacier.
--
-- You must use the following guidelines when naming a vault.
--
--     * Names can be between 1 and 255 characters long.
--
--
--     * Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (hyphen), and '.' (period).
--
--
-- This operation is idempotent.
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
-- For conceptual information and underlying REST API, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/creating-vaults.html Creating a Vault in Amazon Glacier> and <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-put.html Create Vault > in the /Amazon Glacier Developer Guide/ .
module Network.AWS.Glacier.CreateVault
  ( -- * Creating a request
    CreateVault (..),
    mkCreateVault,

    -- ** Request lenses
    cvAccountId,
    cvVaultName,

    -- * Destructuring the response
    CreateVaultResponse (..),
    mkCreateVaultResponse,

    -- ** Response lenses
    cvrsLocation,
    cvrsResponseStatus,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Provides options to create a vault.
--
-- /See:/ 'mkCreateVault' smart constructor.
data CreateVault = CreateVault'
  { accountId :: Lude.Text,
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

-- | Creates a value of 'CreateVault' with the minimum fields required to make a request.
--
-- * 'accountId' - The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
-- * 'vaultName' - The name of the vault.
mkCreateVault ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'vaultName'
  Lude.Text ->
  CreateVault
mkCreateVault pAccountId_ pVaultName_ =
  CreateVault' {accountId = pAccountId_, vaultName = pVaultName_}

-- | The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvAccountId :: Lens.Lens' CreateVault Lude.Text
cvAccountId = Lens.lens (accountId :: CreateVault -> Lude.Text) (\s a -> s {accountId = a} :: CreateVault)
{-# DEPRECATED cvAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvVaultName :: Lens.Lens' CreateVault Lude.Text
cvVaultName = Lens.lens (vaultName :: CreateVault -> Lude.Text) (\s a -> s {vaultName = a} :: CreateVault)
{-# DEPRECATED cvVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

instance Lude.AWSRequest CreateVault where
  type Rs CreateVault = CreateVaultResponse
  request = Req.putJSON glacierService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateVaultResponse'
            Lude.<$> (h Lude..#? "Location") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateVault where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateVault where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath CreateVault where
  toPath CreateVault' {..} =
    Lude.mconcat
      ["/", Lude.toBS accountId, "/vaults/", Lude.toBS vaultName]

instance Lude.ToQuery CreateVault where
  toQuery = Lude.const Lude.mempty

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'mkCreateVaultResponse' smart constructor.
data CreateVaultResponse = CreateVaultResponse'
  { location ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateVaultResponse' with the minimum fields required to make a request.
--
-- * 'location' - The URI of the vault that was created.
-- * 'responseStatus' - The response status code.
mkCreateVaultResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateVaultResponse
mkCreateVaultResponse pResponseStatus_ =
  CreateVaultResponse'
    { location = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The URI of the vault that was created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrsLocation :: Lens.Lens' CreateVaultResponse (Lude.Maybe Lude.Text)
cvrsLocation = Lens.lens (location :: CreateVaultResponse -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: CreateVaultResponse)
{-# DEPRECATED cvrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrsResponseStatus :: Lens.Lens' CreateVaultResponse Lude.Int
cvrsResponseStatus = Lens.lens (responseStatus :: CreateVaultResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateVaultResponse)
{-# DEPRECATED cvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
