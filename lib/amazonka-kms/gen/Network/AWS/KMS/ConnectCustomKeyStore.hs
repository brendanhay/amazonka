{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ConnectCustomKeyStore (..),
    mkConnectCustomKeyStore,

    -- ** Request lenses
    ccksCustomKeyStoreId,

    -- * Destructuring the response
    ConnectCustomKeyStoreResponse (..),
    mkConnectCustomKeyStoreResponse,

    -- ** Response lenses
    crsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkConnectCustomKeyStore' smart constructor.
newtype ConnectCustomKeyStore = ConnectCustomKeyStore'
  { customKeyStoreId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConnectCustomKeyStore' with the minimum fields required to make a request.
--
-- * 'customKeyStoreId' - Enter the key store ID of the custom key store that you want to connect. To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
mkConnectCustomKeyStore ::
  -- | 'customKeyStoreId'
  Lude.Text ->
  ConnectCustomKeyStore
mkConnectCustomKeyStore pCustomKeyStoreId_ =
  ConnectCustomKeyStore' {customKeyStoreId = pCustomKeyStoreId_}

-- | Enter the key store ID of the custom key store that you want to connect. To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
--
-- /Note:/ Consider using 'customKeyStoreId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccksCustomKeyStoreId :: Lens.Lens' ConnectCustomKeyStore Lude.Text
ccksCustomKeyStoreId = Lens.lens (customKeyStoreId :: ConnectCustomKeyStore -> Lude.Text) (\s a -> s {customKeyStoreId = a} :: ConnectCustomKeyStore)
{-# DEPRECATED ccksCustomKeyStoreId "Use generic-lens or generic-optics with 'customKeyStoreId' instead." #-}

instance Lude.AWSRequest ConnectCustomKeyStore where
  type Rs ConnectCustomKeyStore = ConnectCustomKeyStoreResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ConnectCustomKeyStoreResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ConnectCustomKeyStore where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.ConnectCustomKeyStore" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ConnectCustomKeyStore where
  toJSON ConnectCustomKeyStore' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("CustomKeyStoreId" Lude..= customKeyStoreId)]
      )

instance Lude.ToPath ConnectCustomKeyStore where
  toPath = Lude.const "/"

instance Lude.ToQuery ConnectCustomKeyStore where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkConnectCustomKeyStoreResponse' smart constructor.
newtype ConnectCustomKeyStoreResponse = ConnectCustomKeyStoreResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConnectCustomKeyStoreResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkConnectCustomKeyStoreResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ConnectCustomKeyStoreResponse
mkConnectCustomKeyStoreResponse pResponseStatus_ =
  ConnectCustomKeyStoreResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' ConnectCustomKeyStoreResponse Lude.Int
crsResponseStatus = Lens.lens (responseStatus :: ConnectCustomKeyStoreResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ConnectCustomKeyStoreResponse)
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
