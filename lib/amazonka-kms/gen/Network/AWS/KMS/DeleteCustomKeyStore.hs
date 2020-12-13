{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.DeleteCustomKeyStore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> . This operation does not delete the AWS CloudHSM cluster that is associated with the custom key store, or affect any users or keys in the cluster.
--
-- The custom key store that you delete cannot contain any AWS KMS <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys customer master keys (CMKs)> . Before deleting the key store, verify that you will never need to use any of the CMKs in the key store for any <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations> . Then, use 'ScheduleKeyDeletion' to delete the AWS KMS customer master keys (CMKs) from the key store. When the scheduled waiting period expires, the @ScheduleKeyDeletion@ operation deletes the CMKs. Then it makes a best effort to delete the key material from the associated cluster. However, you might need to manually <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-orphaned-key delete the orphaned key material> from the cluster and its backups.
-- After all CMKs are deleted from AWS KMS, use 'DisconnectCustomKeyStore' to disconnect the key store from AWS KMS. Then, you can delete the custom key store.
-- Instead of deleting the custom key store, consider using 'DisconnectCustomKeyStore' to disconnect it from AWS KMS. While the key store is disconnected, you cannot create or use the CMKs in the key store. But, you do not need to delete CMKs and you can reconnect a disconnected custom key store at any time.
-- If the operation succeeds, it returns a JSON object with no properties.
-- This operation is part of the <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html Custom Key Store feature> feature in AWS KMS, which combines the convenience and extensive integration of AWS KMS with the isolation and control of a single-tenant key store.
module Network.AWS.KMS.DeleteCustomKeyStore
  ( -- * Creating a request
    DeleteCustomKeyStore (..),
    mkDeleteCustomKeyStore,

    -- ** Request lenses
    dcksfCustomKeyStoreId,

    -- * Destructuring the response
    DeleteCustomKeyStoreResponse (..),
    mkDeleteCustomKeyStoreResponse,

    -- ** Response lenses
    dcksfrsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteCustomKeyStore' smart constructor.
newtype DeleteCustomKeyStore = DeleteCustomKeyStore'
  { -- | Enter the ID of the custom key store you want to delete. To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
    customKeyStoreId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCustomKeyStore' with the minimum fields required to make a request.
--
-- * 'customKeyStoreId' - Enter the ID of the custom key store you want to delete. To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
mkDeleteCustomKeyStore ::
  -- | 'customKeyStoreId'
  Lude.Text ->
  DeleteCustomKeyStore
mkDeleteCustomKeyStore pCustomKeyStoreId_ =
  DeleteCustomKeyStore' {customKeyStoreId = pCustomKeyStoreId_}

-- | Enter the ID of the custom key store you want to delete. To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
--
-- /Note:/ Consider using 'customKeyStoreId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcksfCustomKeyStoreId :: Lens.Lens' DeleteCustomKeyStore Lude.Text
dcksfCustomKeyStoreId = Lens.lens (customKeyStoreId :: DeleteCustomKeyStore -> Lude.Text) (\s a -> s {customKeyStoreId = a} :: DeleteCustomKeyStore)
{-# DEPRECATED dcksfCustomKeyStoreId "Use generic-lens or generic-optics with 'customKeyStoreId' instead." #-}

instance Lude.AWSRequest DeleteCustomKeyStore where
  type Rs DeleteCustomKeyStore = DeleteCustomKeyStoreResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteCustomKeyStoreResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteCustomKeyStore where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.DeleteCustomKeyStore" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteCustomKeyStore where
  toJSON DeleteCustomKeyStore' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("CustomKeyStoreId" Lude..= customKeyStoreId)]
      )

instance Lude.ToPath DeleteCustomKeyStore where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteCustomKeyStore where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteCustomKeyStoreResponse' smart constructor.
newtype DeleteCustomKeyStoreResponse = DeleteCustomKeyStoreResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCustomKeyStoreResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteCustomKeyStoreResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteCustomKeyStoreResponse
mkDeleteCustomKeyStoreResponse pResponseStatus_ =
  DeleteCustomKeyStoreResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcksfrsResponseStatus :: Lens.Lens' DeleteCustomKeyStoreResponse Lude.Int
dcksfrsResponseStatus = Lens.lens (responseStatus :: DeleteCustomKeyStoreResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteCustomKeyStoreResponse)
{-# DEPRECATED dcksfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
