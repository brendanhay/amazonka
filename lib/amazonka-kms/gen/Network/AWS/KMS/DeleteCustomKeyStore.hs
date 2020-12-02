{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
--
-- The custom key store that you delete cannot contain any AWS KMS <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys customer master keys (CMKs)> . Before deleting the key store, verify that you will never need to use any of the CMKs in the key store for any <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations> . Then, use 'ScheduleKeyDeletion' to delete the AWS KMS customer master keys (CMKs) from the key store. When the scheduled waiting period expires, the @ScheduleKeyDeletion@ operation deletes the CMKs. Then it makes a best effort to delete the key material from the associated cluster. However, you might need to manually <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-orphaned-key delete the orphaned key material> from the cluster and its backups.
--
-- After all CMKs are deleted from AWS KMS, use 'DisconnectCustomKeyStore' to disconnect the key store from AWS KMS. Then, you can delete the custom key store.
--
-- Instead of deleting the custom key store, consider using 'DisconnectCustomKeyStore' to disconnect it from AWS KMS. While the key store is disconnected, you cannot create or use the CMKs in the key store. But, you do not need to delete CMKs and you can reconnect a disconnected custom key store at any time.
--
-- If the operation succeeds, it returns a JSON object with no properties.
--
-- This operation is part of the <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html Custom Key Store feature> feature in AWS KMS, which combines the convenience and extensive integration of AWS KMS with the isolation and control of a single-tenant key store.
module Network.AWS.KMS.DeleteCustomKeyStore
  ( -- * Creating a Request
    deleteCustomKeyStore,
    DeleteCustomKeyStore,

    -- * Request Lenses
    dcksCustomKeyStoreId,

    -- * Destructuring the Response
    deleteCustomKeyStoreResponse,
    DeleteCustomKeyStoreResponse,

    -- * Response Lenses
    delrsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteCustomKeyStore' smart constructor.
newtype DeleteCustomKeyStore = DeleteCustomKeyStore'
  { _dcksCustomKeyStoreId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteCustomKeyStore' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcksCustomKeyStoreId' - Enter the ID of the custom key store you want to delete. To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
deleteCustomKeyStore ::
  -- | 'dcksCustomKeyStoreId'
  Text ->
  DeleteCustomKeyStore
deleteCustomKeyStore pCustomKeyStoreId_ =
  DeleteCustomKeyStore' {_dcksCustomKeyStoreId = pCustomKeyStoreId_}

-- | Enter the ID of the custom key store you want to delete. To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
dcksCustomKeyStoreId :: Lens' DeleteCustomKeyStore Text
dcksCustomKeyStoreId = lens _dcksCustomKeyStoreId (\s a -> s {_dcksCustomKeyStoreId = a})

instance AWSRequest DeleteCustomKeyStore where
  type Rs DeleteCustomKeyStore = DeleteCustomKeyStoreResponse
  request = postJSON kms
  response =
    receiveEmpty
      (\s h x -> DeleteCustomKeyStoreResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteCustomKeyStore

instance NFData DeleteCustomKeyStore

instance ToHeaders DeleteCustomKeyStore where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("TrentService.DeleteCustomKeyStore" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteCustomKeyStore where
  toJSON DeleteCustomKeyStore' {..} =
    object
      (catMaybes [Just ("CustomKeyStoreId" .= _dcksCustomKeyStoreId)])

instance ToPath DeleteCustomKeyStore where
  toPath = const "/"

instance ToQuery DeleteCustomKeyStore where
  toQuery = const mempty

-- | /See:/ 'deleteCustomKeyStoreResponse' smart constructor.
newtype DeleteCustomKeyStoreResponse = DeleteCustomKeyStoreResponse'
  { _delrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteCustomKeyStoreResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteCustomKeyStoreResponse ::
  -- | 'delrsResponseStatus'
  Int ->
  DeleteCustomKeyStoreResponse
deleteCustomKeyStoreResponse pResponseStatus_ =
  DeleteCustomKeyStoreResponse'
    { _delrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteCustomKeyStoreResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\s a -> s {_delrsResponseStatus = a})

instance NFData DeleteCustomKeyStoreResponse
