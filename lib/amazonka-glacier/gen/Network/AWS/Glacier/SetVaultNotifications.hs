{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      SetVaultNotifications (..)
    , mkSetVaultNotifications
    -- ** Request lenses
    , svnAccountId
    , svnVaultName
    , svnVaultNotificationConfig

    -- * Destructuring the response
    , SetVaultNotificationsResponse (..)
    , mkSetVaultNotificationsResponse
    ) where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Provides options to configure notifications that will be sent when specific events happen to a vault.
--
-- /See:/ 'mkSetVaultNotifications' smart constructor.
data SetVaultNotifications = SetVaultNotifications'
  { accountId :: Core.Text
    -- ^ The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
  , vaultName :: Core.Text
    -- ^ The name of the vault.
  , vaultNotificationConfig :: Core.Maybe Types.VaultNotificationConfig
    -- ^ Provides options for specifying notification configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetVaultNotifications' value with any optional fields omitted.
mkSetVaultNotifications
    :: Core.Text -- ^ 'accountId'
    -> Core.Text -- ^ 'vaultName'
    -> SetVaultNotifications
mkSetVaultNotifications accountId vaultName
  = SetVaultNotifications'{accountId, vaultName,
                           vaultNotificationConfig = Core.Nothing}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svnAccountId :: Lens.Lens' SetVaultNotifications Core.Text
svnAccountId = Lens.field @"accountId"
{-# INLINEABLE svnAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svnVaultName :: Lens.Lens' SetVaultNotifications Core.Text
svnVaultName = Lens.field @"vaultName"
{-# INLINEABLE svnVaultName #-}
{-# DEPRECATED vaultName "Use generic-lens or generic-optics with 'vaultName' instead"  #-}

-- | Provides options for specifying notification configuration.
--
-- /Note:/ Consider using 'vaultNotificationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svnVaultNotificationConfig :: Lens.Lens' SetVaultNotifications (Core.Maybe Types.VaultNotificationConfig)
svnVaultNotificationConfig = Lens.field @"vaultNotificationConfig"
{-# INLINEABLE svnVaultNotificationConfig #-}
{-# DEPRECATED vaultNotificationConfig "Use generic-lens or generic-optics with 'vaultNotificationConfig' instead"  #-}

instance Core.ToQuery SetVaultNotifications where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SetVaultNotifications where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON SetVaultNotifications where
        toJSON SetVaultNotifications{..}
          = Core.object
              (Core.catMaybes
                 [("vaultNotificationConfig" Core..=) Core.<$>
                    vaultNotificationConfig])

instance Core.AWSRequest SetVaultNotifications where
        type Rs SetVaultNotifications = SetVaultNotificationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/" Core.<> Core.toText accountId Core.<> "/vaults/" Core.<>
                             Core.toText vaultName
                             Core.<> "/notification-configuration",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull SetVaultNotificationsResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetVaultNotificationsResponse' smart constructor.
data SetVaultNotificationsResponse = SetVaultNotificationsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetVaultNotificationsResponse' value with any optional fields omitted.
mkSetVaultNotificationsResponse
    :: SetVaultNotificationsResponse
mkSetVaultNotificationsResponse = SetVaultNotificationsResponse'
