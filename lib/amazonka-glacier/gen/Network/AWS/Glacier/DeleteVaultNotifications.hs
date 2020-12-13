{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.DeleteVaultNotifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes the notification configuration set for a vault. The operation is eventually consistent; that is, it might take some time for Amazon S3 Glacier to completely disable the notifications and you might still receive some notifications for a short time after you send the delete request.
--
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
-- For conceptual information and underlying REST API, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/configuring-notifications.html Configuring Vault Notifications in Amazon S3 Glacier> and <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-notifications-delete.html Delete Vault Notification Configuration > in the Amazon S3 Glacier Developer Guide.
module Network.AWS.Glacier.DeleteVaultNotifications
  ( -- * Creating a request
    DeleteVaultNotifications (..),
    mkDeleteVaultNotifications,

    -- ** Request lenses
    dvnVaultName,
    dvnAccountId,

    -- * Destructuring the response
    DeleteVaultNotificationsResponse (..),
    mkDeleteVaultNotificationsResponse,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Provides options for deleting a vault notification configuration from an Amazon Glacier vault.
--
-- /See:/ 'mkDeleteVaultNotifications' smart constructor.
data DeleteVaultNotifications = DeleteVaultNotifications'
  { -- | The name of the vault.
    vaultName :: Lude.Text,
    -- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
    accountId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVaultNotifications' with the minimum fields required to make a request.
--
-- * 'vaultName' - The name of the vault.
-- * 'accountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
mkDeleteVaultNotifications ::
  -- | 'vaultName'
  Lude.Text ->
  -- | 'accountId'
  Lude.Text ->
  DeleteVaultNotifications
mkDeleteVaultNotifications pVaultName_ pAccountId_ =
  DeleteVaultNotifications'
    { vaultName = pVaultName_,
      accountId = pAccountId_
    }

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvnVaultName :: Lens.Lens' DeleteVaultNotifications Lude.Text
dvnVaultName = Lens.lens (vaultName :: DeleteVaultNotifications -> Lude.Text) (\s a -> s {vaultName = a} :: DeleteVaultNotifications)
{-# DEPRECATED dvnVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvnAccountId :: Lens.Lens' DeleteVaultNotifications Lude.Text
dvnAccountId = Lens.lens (accountId :: DeleteVaultNotifications -> Lude.Text) (\s a -> s {accountId = a} :: DeleteVaultNotifications)
{-# DEPRECATED dvnAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

instance Lude.AWSRequest DeleteVaultNotifications where
  type Rs DeleteVaultNotifications = DeleteVaultNotificationsResponse
  request = Req.delete glacierService
  response = Res.receiveNull DeleteVaultNotificationsResponse'

instance Lude.ToHeaders DeleteVaultNotifications where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteVaultNotifications where
  toPath DeleteVaultNotifications' {..} =
    Lude.mconcat
      [ "/",
        Lude.toBS accountId,
        "/vaults/",
        Lude.toBS vaultName,
        "/notification-configuration"
      ]

instance Lude.ToQuery DeleteVaultNotifications where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteVaultNotificationsResponse' smart constructor.
data DeleteVaultNotificationsResponse = DeleteVaultNotificationsResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVaultNotificationsResponse' with the minimum fields required to make a request.
mkDeleteVaultNotificationsResponse ::
  DeleteVaultNotificationsResponse
mkDeleteVaultNotificationsResponse =
  DeleteVaultNotificationsResponse'
