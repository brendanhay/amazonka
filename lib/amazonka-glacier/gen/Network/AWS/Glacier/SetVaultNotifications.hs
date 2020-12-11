{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.SetVaultNotifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation configures notifications that will be sent when specific events happen to a vault. By default, you don't get any notifications.
--
-- To configure vault notifications, send a PUT request to the @notification-configuration@ subresource of the vault. The request should include a JSON document that provides an Amazon SNS topic and specific events for which you want Amazon S3 Glacier to send notifications to the topic.
-- Amazon SNS topics must grant permission to the vault to be allowed to publish notifications to the topic. You can configure a vault to publish a notification for the following vault events:
--
--     * __ArchiveRetrievalCompleted__ This event occurs when a job that was initiated for an archive retrieval is completed ('InitiateJob' ). The status of the completed job can be "Succeeded" or "Failed". The notification sent to the SNS topic is the same output as returned from 'DescribeJob' .
--
--
--     * __InventoryRetrievalCompleted__ This event occurs when a job that was initiated for an inventory retrieval is completed ('InitiateJob' ). The status of the completed job can be "Succeeded" or "Failed". The notification sent to the SNS topic is the same output as returned from 'DescribeJob' .
--
--
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
-- For conceptual information and underlying REST API, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/configuring-notifications.html Configuring Vault Notifications in Amazon S3 Glacier> and <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-notifications-put.html Set Vault Notification Configuration > in the /Amazon Glacier Developer Guide/ .
module Network.AWS.Glacier.SetVaultNotifications
  ( -- * Creating a request
    SetVaultNotifications (..),
    mkSetVaultNotifications,

    -- ** Request lenses
    svnVaultNotificationConfig,
    svnAccountId,
    svnVaultName,

    -- * Destructuring the response
    SetVaultNotificationsResponse (..),
    mkSetVaultNotificationsResponse,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Provides options to configure notifications that will be sent when specific events happen to a vault.
--
-- /See:/ 'mkSetVaultNotifications' smart constructor.
data SetVaultNotifications = SetVaultNotifications'
  { vaultNotificationConfig ::
      Lude.Maybe VaultNotificationConfig,
    accountId :: Lude.Text,
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

-- | Creates a value of 'SetVaultNotifications' with the minimum fields required to make a request.
--
-- * 'accountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
-- * 'vaultName' - The name of the vault.
-- * 'vaultNotificationConfig' - Provides options for specifying notification configuration.
mkSetVaultNotifications ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'vaultName'
  Lude.Text ->
  SetVaultNotifications
mkSetVaultNotifications pAccountId_ pVaultName_ =
  SetVaultNotifications'
    { vaultNotificationConfig = Lude.Nothing,
      accountId = pAccountId_,
      vaultName = pVaultName_
    }

-- | Provides options for specifying notification configuration.
--
-- /Note:/ Consider using 'vaultNotificationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svnVaultNotificationConfig :: Lens.Lens' SetVaultNotifications (Lude.Maybe VaultNotificationConfig)
svnVaultNotificationConfig = Lens.lens (vaultNotificationConfig :: SetVaultNotifications -> Lude.Maybe VaultNotificationConfig) (\s a -> s {vaultNotificationConfig = a} :: SetVaultNotifications)
{-# DEPRECATED svnVaultNotificationConfig "Use generic-lens or generic-optics with 'vaultNotificationConfig' instead." #-}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svnAccountId :: Lens.Lens' SetVaultNotifications Lude.Text
svnAccountId = Lens.lens (accountId :: SetVaultNotifications -> Lude.Text) (\s a -> s {accountId = a} :: SetVaultNotifications)
{-# DEPRECATED svnAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svnVaultName :: Lens.Lens' SetVaultNotifications Lude.Text
svnVaultName = Lens.lens (vaultName :: SetVaultNotifications -> Lude.Text) (\s a -> s {vaultName = a} :: SetVaultNotifications)
{-# DEPRECATED svnVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

instance Lude.AWSRequest SetVaultNotifications where
  type Rs SetVaultNotifications = SetVaultNotificationsResponse
  request = Req.putJSON glacierService
  response = Res.receiveNull SetVaultNotificationsResponse'

instance Lude.ToHeaders SetVaultNotifications where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON SetVaultNotifications where
  toJSON SetVaultNotifications' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("vaultNotificationConfig" Lude..=)
              Lude.<$> vaultNotificationConfig
          ]
      )

instance Lude.ToPath SetVaultNotifications where
  toPath SetVaultNotifications' {..} =
    Lude.mconcat
      [ "/",
        Lude.toBS accountId,
        "/vaults/",
        Lude.toBS vaultName,
        "/notification-configuration"
      ]

instance Lude.ToQuery SetVaultNotifications where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSetVaultNotificationsResponse' smart constructor.
data SetVaultNotificationsResponse = SetVaultNotificationsResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetVaultNotificationsResponse' with the minimum fields required to make a request.
mkSetVaultNotificationsResponse ::
  SetVaultNotificationsResponse
mkSetVaultNotificationsResponse = SetVaultNotificationsResponse'
