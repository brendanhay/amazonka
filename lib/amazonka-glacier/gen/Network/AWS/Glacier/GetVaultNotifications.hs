{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.GetVaultNotifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation retrieves the @notification-configuration@ subresource of the specified vault.
--
-- For information about setting a notification configuration on a vault, see 'SetVaultNotifications' . If a notification configuration for a vault is not set, the operation returns a @404 Not Found@ error. For more information about vault notifications, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/configuring-notifications.html Configuring Vault Notifications in Amazon S3 Glacier> .
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
-- For conceptual information and underlying REST API, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/configuring-notifications.html Configuring Vault Notifications in Amazon S3 Glacier> and <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-notifications-get.html Get Vault Notification Configuration > in the /Amazon Glacier Developer Guide/ .
module Network.AWS.Glacier.GetVaultNotifications
  ( -- * Creating a request
    GetVaultNotifications (..),
    mkGetVaultNotifications,

    -- ** Request lenses
    gvnAccountId,
    gvnVaultName,

    -- * Destructuring the response
    GetVaultNotificationsResponse (..),
    mkGetVaultNotificationsResponse,

    -- ** Response lenses
    gvnrsVaultNotificationConfig,
    gvnrsResponseStatus,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Provides options for retrieving the notification configuration set on an Amazon Glacier vault.
--
-- /See:/ 'mkGetVaultNotifications' smart constructor.
data GetVaultNotifications = GetVaultNotifications'
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

-- | Creates a value of 'GetVaultNotifications' with the minimum fields required to make a request.
--
-- * 'accountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
-- * 'vaultName' - The name of the vault.
mkGetVaultNotifications ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'vaultName'
  Lude.Text ->
  GetVaultNotifications
mkGetVaultNotifications pAccountId_ pVaultName_ =
  GetVaultNotifications'
    { accountId = pAccountId_,
      vaultName = pVaultName_
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvnAccountId :: Lens.Lens' GetVaultNotifications Lude.Text
gvnAccountId = Lens.lens (accountId :: GetVaultNotifications -> Lude.Text) (\s a -> s {accountId = a} :: GetVaultNotifications)
{-# DEPRECATED gvnAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvnVaultName :: Lens.Lens' GetVaultNotifications Lude.Text
gvnVaultName = Lens.lens (vaultName :: GetVaultNotifications -> Lude.Text) (\s a -> s {vaultName = a} :: GetVaultNotifications)
{-# DEPRECATED gvnVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

instance Lude.AWSRequest GetVaultNotifications where
  type Rs GetVaultNotifications = GetVaultNotificationsResponse
  request = Req.get glacierService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetVaultNotificationsResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetVaultNotifications where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetVaultNotifications where
  toPath GetVaultNotifications' {..} =
    Lude.mconcat
      [ "/",
        Lude.toBS accountId,
        "/vaults/",
        Lude.toBS vaultName,
        "/notification-configuration"
      ]

instance Lude.ToQuery GetVaultNotifications where
  toQuery = Lude.const Lude.mempty

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'mkGetVaultNotificationsResponse' smart constructor.
data GetVaultNotificationsResponse = GetVaultNotificationsResponse'
  { vaultNotificationConfig ::
      Lude.Maybe
        VaultNotificationConfig,
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

-- | Creates a value of 'GetVaultNotificationsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'vaultNotificationConfig' - Returns the notification configuration set on the vault.
mkGetVaultNotificationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetVaultNotificationsResponse
mkGetVaultNotificationsResponse pResponseStatus_ =
  GetVaultNotificationsResponse'
    { vaultNotificationConfig =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns the notification configuration set on the vault.
--
-- /Note:/ Consider using 'vaultNotificationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvnrsVaultNotificationConfig :: Lens.Lens' GetVaultNotificationsResponse (Lude.Maybe VaultNotificationConfig)
gvnrsVaultNotificationConfig = Lens.lens (vaultNotificationConfig :: GetVaultNotificationsResponse -> Lude.Maybe VaultNotificationConfig) (\s a -> s {vaultNotificationConfig = a} :: GetVaultNotificationsResponse)
{-# DEPRECATED gvnrsVaultNotificationConfig "Use generic-lens or generic-optics with 'vaultNotificationConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvnrsResponseStatus :: Lens.Lens' GetVaultNotificationsResponse Lude.Int
gvnrsResponseStatus = Lens.lens (responseStatus :: GetVaultNotificationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetVaultNotificationsResponse)
{-# DEPRECATED gvnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
