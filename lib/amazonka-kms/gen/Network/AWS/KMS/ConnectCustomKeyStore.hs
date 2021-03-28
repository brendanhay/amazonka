{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.ConnectCustomKeyStore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Connects or reconnects a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> to its associated AWS CloudHSM cluster.
--
-- The custom key store must be connected before you can create customer master keys (CMKs) in the key store or use the CMKs it contains. You can disconnect and reconnect a custom key store at any time.
-- To connect a custom key store, its associated AWS CloudHSM cluster must have at least one active HSM. To get the number of active HSMs in a cluster, use the <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters> operation. To add HSMs to the cluster, use the <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_CreateHsm.html CreateHsm> operation. Also, the <https://docs.aws.amazon.com/kms/latest/developerguide/key-store-concepts.html#concept-kmsuser @kmsuser@ crypto user> (CU) must not be logged into the cluster. This prevents AWS KMS from using this account to log in.
-- The connection process can take an extended amount of time to complete; up to 20 minutes. This operation starts the connection process, but it does not wait for it to complete. When it succeeds, this operation quickly returns an HTTP 200 response and a JSON object with no properties. However, this response does not indicate that the custom key store is connected. To get the connection state of the custom key store, use the 'DescribeCustomKeyStores' operation.
-- During the connection process, AWS KMS finds the AWS CloudHSM cluster that is associated with the custom key store, creates the connection infrastructure, connects to the cluster, logs into the AWS CloudHSM client as the @kmsuser@ CU, and rotates its password.
-- The @ConnectCustomKeyStore@ operation might fail for various reasons. To find the reason, use the 'DescribeCustomKeyStores' operation and see the @ConnectionErrorCode@ in the response. For help interpreting the @ConnectionErrorCode@ , see 'CustomKeyStoresListEntry' .
-- To fix the failure, use the 'DisconnectCustomKeyStore' operation to disconnect the custom key store, correct the error, use the 'UpdateCustomKeyStore' operation if necessary, and then use @ConnectCustomKeyStore@ again.
-- If you are having trouble connecting or disconnecting a custom key store, see <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html Troubleshooting a Custom Key Store> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.ConnectCustomKeyStore
    (
    -- * Creating a request
      ConnectCustomKeyStore (..)
    , mkConnectCustomKeyStore
    -- ** Request lenses
    , ccksCustomKeyStoreId

    -- * Destructuring the response
    , ConnectCustomKeyStoreResponse (..)
    , mkConnectCustomKeyStoreResponse
    -- ** Response lenses
    , crsResponseStatus
    ) where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkConnectCustomKeyStore' smart constructor.
newtype ConnectCustomKeyStore = ConnectCustomKeyStore'
  { customKeyStoreId :: Types.CustomKeyStoreId
    -- ^ Enter the key store ID of the custom key store that you want to connect. To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ConnectCustomKeyStore' value with any optional fields omitted.
mkConnectCustomKeyStore
    :: Types.CustomKeyStoreId -- ^ 'customKeyStoreId'
    -> ConnectCustomKeyStore
mkConnectCustomKeyStore customKeyStoreId
  = ConnectCustomKeyStore'{customKeyStoreId}

-- | Enter the key store ID of the custom key store that you want to connect. To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
--
-- /Note:/ Consider using 'customKeyStoreId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccksCustomKeyStoreId :: Lens.Lens' ConnectCustomKeyStore Types.CustomKeyStoreId
ccksCustomKeyStoreId = Lens.field @"customKeyStoreId"
{-# INLINEABLE ccksCustomKeyStoreId #-}
{-# DEPRECATED customKeyStoreId "Use generic-lens or generic-optics with 'customKeyStoreId' instead"  #-}

instance Core.ToQuery ConnectCustomKeyStore where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ConnectCustomKeyStore where
        toHeaders ConnectCustomKeyStore{..}
          = Core.pure ("X-Amz-Target", "TrentService.ConnectCustomKeyStore")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ConnectCustomKeyStore where
        toJSON ConnectCustomKeyStore{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CustomKeyStoreId" Core..= customKeyStoreId)])

instance Core.AWSRequest ConnectCustomKeyStore where
        type Rs ConnectCustomKeyStore = ConnectCustomKeyStoreResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 ConnectCustomKeyStoreResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkConnectCustomKeyStoreResponse' smart constructor.
newtype ConnectCustomKeyStoreResponse = ConnectCustomKeyStoreResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ConnectCustomKeyStoreResponse' value with any optional fields omitted.
mkConnectCustomKeyStoreResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ConnectCustomKeyStoreResponse
mkConnectCustomKeyStoreResponse responseStatus
  = ConnectCustomKeyStoreResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' ConnectCustomKeyStoreResponse Core.Int
crsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
