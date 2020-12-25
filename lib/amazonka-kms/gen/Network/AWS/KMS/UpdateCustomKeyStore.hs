{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.UpdateCustomKeyStore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the properties of a custom key store. Use the @CustomKeyStoreId@ parameter to identify the custom key store you want to edit. Use the remaining parameters to change the properties of the custom key store.
--
-- You can only update a custom key store that is disconnected. To disconnect the custom key store, use 'DisconnectCustomKeyStore' . To reconnect the custom key store after the update completes, use 'ConnectCustomKeyStore' . To find the connection state of a custom key store, use the 'DescribeCustomKeyStores' operation.
-- Use the parameters of @UpdateCustomKeyStore@ to edit your keystore settings.
--
--     * Use the __NewCustomKeyStoreName__ parameter to change the friendly name of the custom key store to the value that you specify.
--
--
--
--     * Use the __KeyStorePassword__ parameter tell AWS KMS the current password of the <https://docs.aws.amazon.com/kms/latest/developerguide/key-store-concepts.html#concept-kmsuser @kmsuser@ crypto user (CU)> in the associated AWS CloudHSM cluster. You can use this parameter to <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-password fix connection failures> that occur when AWS KMS cannot log into the associated cluster because the @kmsuser@ password has changed. This value does not change the password in the AWS CloudHSM cluster.
--
--
--
--     * Use the __CloudHsmClusterId__ parameter to associate the custom key store with a different, but related, AWS CloudHSM cluster. You can use this parameter to repair a custom key store if its AWS CloudHSM cluster becomes corrupted or is deleted, or when you need to create or restore a cluster from a backup.
--
--
-- If the operation succeeds, it returns a JSON object with no properties.
-- This operation is part of the <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html Custom Key Store feature> feature in AWS KMS, which combines the convenience and extensive integration of AWS KMS with the isolation and control of a single-tenant key store.
module Network.AWS.KMS.UpdateCustomKeyStore
  ( -- * Creating a request
    UpdateCustomKeyStore (..),
    mkUpdateCustomKeyStore,

    -- ** Request lenses
    ucksCustomKeyStoreId,
    ucksCloudHsmClusterId,
    ucksKeyStorePassword,
    ucksNewCustomKeyStoreName,

    -- * Destructuring the response
    UpdateCustomKeyStoreResponse (..),
    mkUpdateCustomKeyStoreResponse,

    -- ** Response lenses
    ucksrrsResponseStatus,
  )
where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateCustomKeyStore' smart constructor.
data UpdateCustomKeyStore = UpdateCustomKeyStore'
  { -- | Identifies the custom key store that you want to update. Enter the ID of the custom key store. To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
    customKeyStoreId :: Types.CustomKeyStoreIdType,
    -- | Associates the custom key store with a related AWS CloudHSM cluster.
    --
    -- Enter the cluster ID of the cluster that you used to create the custom key store or a cluster that shares a backup history and has the same cluster certificate as the original cluster. You cannot use this parameter to associate a custom key store with an unrelated cluster. In addition, the replacement cluster must <https://docs.aws.amazon.com/kms/latest/developerguide/create-keystore.html#before-keystore fulfill the requirements> for a cluster associated with a custom key store. To view the cluster certificate of a cluster, use the <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters> operation.
    cloudHsmClusterId :: Core.Maybe Types.CloudHsmClusterIdType,
    -- | Enter the current password of the @kmsuser@ crypto user (CU) in the AWS CloudHSM cluster that is associated with the custom key store.
    --
    -- This parameter tells AWS KMS the current password of the @kmsuser@ crypto user (CU). It does not set or change the password of any users in the AWS CloudHSM cluster.
    keyStorePassword :: Core.Maybe Types.KeyStorePasswordType,
    -- | Changes the friendly name of the custom key store to the value that you specify. The custom key store name must be unique in the AWS account.
    newCustomKeyStoreName :: Core.Maybe Types.CustomKeyStoreNameType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCustomKeyStore' value with any optional fields omitted.
mkUpdateCustomKeyStore ::
  -- | 'customKeyStoreId'
  Types.CustomKeyStoreIdType ->
  UpdateCustomKeyStore
mkUpdateCustomKeyStore customKeyStoreId =
  UpdateCustomKeyStore'
    { customKeyStoreId,
      cloudHsmClusterId = Core.Nothing,
      keyStorePassword = Core.Nothing,
      newCustomKeyStoreName = Core.Nothing
    }

-- | Identifies the custom key store that you want to update. Enter the ID of the custom key store. To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
--
-- /Note:/ Consider using 'customKeyStoreId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucksCustomKeyStoreId :: Lens.Lens' UpdateCustomKeyStore Types.CustomKeyStoreIdType
ucksCustomKeyStoreId = Lens.field @"customKeyStoreId"
{-# DEPRECATED ucksCustomKeyStoreId "Use generic-lens or generic-optics with 'customKeyStoreId' instead." #-}

-- | Associates the custom key store with a related AWS CloudHSM cluster.
--
-- Enter the cluster ID of the cluster that you used to create the custom key store or a cluster that shares a backup history and has the same cluster certificate as the original cluster. You cannot use this parameter to associate a custom key store with an unrelated cluster. In addition, the replacement cluster must <https://docs.aws.amazon.com/kms/latest/developerguide/create-keystore.html#before-keystore fulfill the requirements> for a cluster associated with a custom key store. To view the cluster certificate of a cluster, use the <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters> operation.
--
-- /Note:/ Consider using 'cloudHsmClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucksCloudHsmClusterId :: Lens.Lens' UpdateCustomKeyStore (Core.Maybe Types.CloudHsmClusterIdType)
ucksCloudHsmClusterId = Lens.field @"cloudHsmClusterId"
{-# DEPRECATED ucksCloudHsmClusterId "Use generic-lens or generic-optics with 'cloudHsmClusterId' instead." #-}

-- | Enter the current password of the @kmsuser@ crypto user (CU) in the AWS CloudHSM cluster that is associated with the custom key store.
--
-- This parameter tells AWS KMS the current password of the @kmsuser@ crypto user (CU). It does not set or change the password of any users in the AWS CloudHSM cluster.
--
-- /Note:/ Consider using 'keyStorePassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucksKeyStorePassword :: Lens.Lens' UpdateCustomKeyStore (Core.Maybe Types.KeyStorePasswordType)
ucksKeyStorePassword = Lens.field @"keyStorePassword"
{-# DEPRECATED ucksKeyStorePassword "Use generic-lens or generic-optics with 'keyStorePassword' instead." #-}

-- | Changes the friendly name of the custom key store to the value that you specify. The custom key store name must be unique in the AWS account.
--
-- /Note:/ Consider using 'newCustomKeyStoreName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucksNewCustomKeyStoreName :: Lens.Lens' UpdateCustomKeyStore (Core.Maybe Types.CustomKeyStoreNameType)
ucksNewCustomKeyStoreName = Lens.field @"newCustomKeyStoreName"
{-# DEPRECATED ucksNewCustomKeyStoreName "Use generic-lens or generic-optics with 'newCustomKeyStoreName' instead." #-}

instance Core.FromJSON UpdateCustomKeyStore where
  toJSON UpdateCustomKeyStore {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("CustomKeyStoreId" Core..= customKeyStoreId),
            ("CloudHsmClusterId" Core..=) Core.<$> cloudHsmClusterId,
            ("KeyStorePassword" Core..=) Core.<$> keyStorePassword,
            ("NewCustomKeyStoreName" Core..=) Core.<$> newCustomKeyStoreName
          ]
      )

instance Core.AWSRequest UpdateCustomKeyStore where
  type Rs UpdateCustomKeyStore = UpdateCustomKeyStoreResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "TrentService.UpdateCustomKeyStore")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateCustomKeyStoreResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateCustomKeyStoreResponse' smart constructor.
newtype UpdateCustomKeyStoreResponse = UpdateCustomKeyStoreResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCustomKeyStoreResponse' value with any optional fields omitted.
mkUpdateCustomKeyStoreResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateCustomKeyStoreResponse
mkUpdateCustomKeyStoreResponse responseStatus =
  UpdateCustomKeyStoreResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucksrrsResponseStatus :: Lens.Lens' UpdateCustomKeyStoreResponse Core.Int
ucksrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ucksrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
