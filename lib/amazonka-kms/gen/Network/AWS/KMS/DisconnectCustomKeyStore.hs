{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.DisconnectCustomKeyStore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disconnects the <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> from its associated AWS CloudHSM cluster. While a custom key store is disconnected, you can manage the custom key store and its customer master keys (CMKs), but you cannot create or use CMKs in the custom key store. You can reconnect the custom key store at any time.
--
--
-- To find the connection state of a custom key store, use the 'DescribeCustomKeyStores' operation. To reconnect a custom key store, use the 'ConnectCustomKeyStore' operation.
-- If the operation succeeds, it returns a JSON object with no properties.
-- This operation is part of the <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html Custom Key Store feature> feature in AWS KMS, which combines the convenience and extensive integration of AWS KMS with the isolation and control of a single-tenant key store.
module Network.AWS.KMS.DisconnectCustomKeyStore
  ( -- * Creating a request
    DisconnectCustomKeyStore (..),
    mkDisconnectCustomKeyStore,

    -- ** Request lenses
    dCustomKeyStoreId,

    -- * Destructuring the response
    DisconnectCustomKeyStoreResponse (..),
    mkDisconnectCustomKeyStoreResponse,

    -- ** Response lenses
    dcksrrsResponseStatus,
  )
where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisconnectCustomKeyStore' smart constructor.
newtype DisconnectCustomKeyStore = DisconnectCustomKeyStore'
  { -- | Enter the ID of the custom key store you want to disconnect. To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
    customKeyStoreId :: Types.CustomKeyStoreId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisconnectCustomKeyStore' value with any optional fields omitted.
mkDisconnectCustomKeyStore ::
  -- | 'customKeyStoreId'
  Types.CustomKeyStoreId ->
  DisconnectCustomKeyStore
mkDisconnectCustomKeyStore customKeyStoreId =
  DisconnectCustomKeyStore' {customKeyStoreId}

-- | Enter the ID of the custom key store you want to disconnect. To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
--
-- /Note:/ Consider using 'customKeyStoreId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCustomKeyStoreId :: Lens.Lens' DisconnectCustomKeyStore Types.CustomKeyStoreId
dCustomKeyStoreId = Lens.field @"customKeyStoreId"
{-# DEPRECATED dCustomKeyStoreId "Use generic-lens or generic-optics with 'customKeyStoreId' instead." #-}

instance Core.FromJSON DisconnectCustomKeyStore where
  toJSON DisconnectCustomKeyStore {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("CustomKeyStoreId" Core..= customKeyStoreId)]
      )

instance Core.AWSRequest DisconnectCustomKeyStore where
  type Rs DisconnectCustomKeyStore = DisconnectCustomKeyStoreResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "TrentService.DisconnectCustomKeyStore")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisconnectCustomKeyStoreResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisconnectCustomKeyStoreResponse' smart constructor.
newtype DisconnectCustomKeyStoreResponse = DisconnectCustomKeyStoreResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisconnectCustomKeyStoreResponse' value with any optional fields omitted.
mkDisconnectCustomKeyStoreResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisconnectCustomKeyStoreResponse
mkDisconnectCustomKeyStoreResponse responseStatus =
  DisconnectCustomKeyStoreResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcksrrsResponseStatus :: Lens.Lens' DisconnectCustomKeyStoreResponse Core.Int
dcksrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcksrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
