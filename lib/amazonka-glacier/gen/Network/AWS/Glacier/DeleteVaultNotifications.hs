{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteVaultNotifications (..)
    , mkDeleteVaultNotifications
    -- ** Request lenses
    , dvnAccountId
    , dvnVaultName

    -- * Destructuring the response
    , DeleteVaultNotificationsResponse (..)
    , mkDeleteVaultNotificationsResponse
    ) where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Provides options for deleting a vault notification configuration from an Amazon Glacier vault.
--
-- /See:/ 'mkDeleteVaultNotifications' smart constructor.
data DeleteVaultNotifications = DeleteVaultNotifications'
  { accountId :: Core.Text
    -- ^ The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID. 
  , vaultName :: Core.Text
    -- ^ The name of the vault.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVaultNotifications' value with any optional fields omitted.
mkDeleteVaultNotifications
    :: Core.Text -- ^ 'accountId'
    -> Core.Text -- ^ 'vaultName'
    -> DeleteVaultNotifications
mkDeleteVaultNotifications accountId vaultName
  = DeleteVaultNotifications'{accountId, vaultName}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID. 
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvnAccountId :: Lens.Lens' DeleteVaultNotifications Core.Text
dvnAccountId = Lens.field @"accountId"
{-# INLINEABLE dvnAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvnVaultName :: Lens.Lens' DeleteVaultNotifications Core.Text
dvnVaultName = Lens.field @"vaultName"
{-# INLINEABLE dvnVaultName #-}
{-# DEPRECATED vaultName "Use generic-lens or generic-optics with 'vaultName' instead"  #-}

instance Core.ToQuery DeleteVaultNotifications where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteVaultNotifications where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteVaultNotifications where
        type Rs DeleteVaultNotifications = DeleteVaultNotificationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/" Core.<> Core.toText accountId Core.<> "/vaults/" Core.<>
                             Core.toText vaultName
                             Core.<> "/notification-configuration",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteVaultNotificationsResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteVaultNotificationsResponse' smart constructor.
data DeleteVaultNotificationsResponse = DeleteVaultNotificationsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVaultNotificationsResponse' value with any optional fields omitted.
mkDeleteVaultNotificationsResponse
    :: DeleteVaultNotificationsResponse
mkDeleteVaultNotificationsResponse
  = DeleteVaultNotificationsResponse'
