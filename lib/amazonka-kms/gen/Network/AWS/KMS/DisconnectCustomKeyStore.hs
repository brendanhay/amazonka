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
    dcksCustomKeyStoreId,

    -- * Destructuring the response
    DisconnectCustomKeyStoreResponse (..),
    mkDisconnectCustomKeyStoreResponse,

    -- ** Response lenses
    dcksrsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisconnectCustomKeyStore' smart constructor.
newtype DisconnectCustomKeyStore = DisconnectCustomKeyStore'
  { -- | Enter the ID of the custom key store you want to disconnect. To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
    customKeyStoreId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisconnectCustomKeyStore' with the minimum fields required to make a request.
--
-- * 'customKeyStoreId' - Enter the ID of the custom key store you want to disconnect. To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
mkDisconnectCustomKeyStore ::
  -- | 'customKeyStoreId'
  Lude.Text ->
  DisconnectCustomKeyStore
mkDisconnectCustomKeyStore pCustomKeyStoreId_ =
  DisconnectCustomKeyStore' {customKeyStoreId = pCustomKeyStoreId_}

-- | Enter the ID of the custom key store you want to disconnect. To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
--
-- /Note:/ Consider using 'customKeyStoreId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcksCustomKeyStoreId :: Lens.Lens' DisconnectCustomKeyStore Lude.Text
dcksCustomKeyStoreId = Lens.lens (customKeyStoreId :: DisconnectCustomKeyStore -> Lude.Text) (\s a -> s {customKeyStoreId = a} :: DisconnectCustomKeyStore)
{-# DEPRECATED dcksCustomKeyStoreId "Use generic-lens or generic-optics with 'customKeyStoreId' instead." #-}

instance Lude.AWSRequest DisconnectCustomKeyStore where
  type Rs DisconnectCustomKeyStore = DisconnectCustomKeyStoreResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisconnectCustomKeyStoreResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisconnectCustomKeyStore where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.DisconnectCustomKeyStore" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisconnectCustomKeyStore where
  toJSON DisconnectCustomKeyStore' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("CustomKeyStoreId" Lude..= customKeyStoreId)]
      )

instance Lude.ToPath DisconnectCustomKeyStore where
  toPath = Lude.const "/"

instance Lude.ToQuery DisconnectCustomKeyStore where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisconnectCustomKeyStoreResponse' smart constructor.
newtype DisconnectCustomKeyStoreResponse = DisconnectCustomKeyStoreResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisconnectCustomKeyStoreResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisconnectCustomKeyStoreResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisconnectCustomKeyStoreResponse
mkDisconnectCustomKeyStoreResponse pResponseStatus_ =
  DisconnectCustomKeyStoreResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcksrsResponseStatus :: Lens.Lens' DisconnectCustomKeyStoreResponse Lude.Int
dcksrsResponseStatus = Lens.lens (responseStatus :: DisconnectCustomKeyStoreResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisconnectCustomKeyStoreResponse)
{-# DEPRECATED dcksrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
