{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.ConnectCustomKeyStore
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Connects or reconnects a <http://docs.aws.amazon.com/kms/latest/developerguide/key-store-overview.html custom key store> to its associated AWS CloudHSM cluster.
--
--
-- The custom key store must be connected before you can create customer master keys (CMKs) in the key store or use the CMKs it contains. You can disconnect and reconnect a custom key store at any time.
--
-- To connect a custom key store, its associated AWS CloudHSM cluster must have at least one active HSM. To get the number of active HSMs in a cluster, use the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters DescribeClusters> operation. To add HSMs to the cluster, use the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_CreateHsm CreateHsm> operation.
--
-- The connection process can take an extended amount of time to complete; up to 20 minutes. This operation starts the connection process, but it does not wait for it to complete. When it succeeds, this operation quickly returns an HTTP 200 response and a JSON object with no properties. However, this response does not indicate that the custom key store is connected. To get the connection state of the custom key store, use the 'DescribeCustomKeyStores' operation.
--
-- During the connection process, AWS KMS finds the AWS CloudHSM cluster that is associated with the custom key store, creates the connection infrastructure, connects to the cluster, logs into the AWS CloudHSM client as the <http://docs.aws.amazon.com/kms/latest/developerguide/key-store-concepts.html#concept-kmsuser @kmsuser@ crypto user> (CU), and rotates its password.
--
-- The @ConnectCustomKeyStore@ operation might fail for various reasons. To find the reason, use the 'DescribeCustomKeyStores' operation and see the @ConnectionErrorCode@ in the response. For help interpreting the @ConnectionErrorCode@ , see 'CustomKeyStoresListEntry' .
--
-- To fix the failure, use the 'DisconnectCustomKeyStore' operation to disconnect the custom key store, correct the error, use the 'UpdateCustomKeyStore' operation if necessary, and then use @ConnectCustomKeyStore@ again.
--
-- If you are having trouble connecting or disconnecting a custom key store, see <http://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html Troubleshooting a Custom Key Store> in the /AWS Key Management Service Developer Guide/ .
--
module Network.AWS.KMS.ConnectCustomKeyStore
    (
    -- * Creating a Request
      connectCustomKeyStore
    , ConnectCustomKeyStore
    -- * Request Lenses
    , ccksCustomKeyStoreId

    -- * Destructuring the Response
    , connectCustomKeyStoreResponse
    , ConnectCustomKeyStoreResponse
    -- * Response Lenses
    , crsResponseStatus
    ) where

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'connectCustomKeyStore' smart constructor.
newtype ConnectCustomKeyStore = ConnectCustomKeyStore'
  { _ccksCustomKeyStoreId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConnectCustomKeyStore' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccksCustomKeyStoreId' - Enter the key store ID of the custom key store that you want to connect. To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
connectCustomKeyStore
    :: Text -- ^ 'ccksCustomKeyStoreId'
    -> ConnectCustomKeyStore
connectCustomKeyStore pCustomKeyStoreId_ =
  ConnectCustomKeyStore' {_ccksCustomKeyStoreId = pCustomKeyStoreId_}


-- | Enter the key store ID of the custom key store that you want to connect. To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
ccksCustomKeyStoreId :: Lens' ConnectCustomKeyStore Text
ccksCustomKeyStoreId = lens _ccksCustomKeyStoreId (\ s a -> s{_ccksCustomKeyStoreId = a})

instance AWSRequest ConnectCustomKeyStore where
        type Rs ConnectCustomKeyStore =
             ConnectCustomKeyStoreResponse
        request = postJSON kms
        response
          = receiveEmpty
              (\ s h x ->
                 ConnectCustomKeyStoreResponse' <$>
                   (pure (fromEnum s)))

instance Hashable ConnectCustomKeyStore where

instance NFData ConnectCustomKeyStore where

instance ToHeaders ConnectCustomKeyStore where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.ConnectCustomKeyStore" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ConnectCustomKeyStore where
        toJSON ConnectCustomKeyStore'{..}
          = object
              (catMaybes
                 [Just ("CustomKeyStoreId" .= _ccksCustomKeyStoreId)])

instance ToPath ConnectCustomKeyStore where
        toPath = const "/"

instance ToQuery ConnectCustomKeyStore where
        toQuery = const mempty

-- | /See:/ 'connectCustomKeyStoreResponse' smart constructor.
newtype ConnectCustomKeyStoreResponse = ConnectCustomKeyStoreResponse'
  { _crsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConnectCustomKeyStoreResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsResponseStatus' - -- | The response status code.
connectCustomKeyStoreResponse
    :: Int -- ^ 'crsResponseStatus'
    -> ConnectCustomKeyStoreResponse
connectCustomKeyStoreResponse pResponseStatus_ =
  ConnectCustomKeyStoreResponse' {_crsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
crsResponseStatus :: Lens' ConnectCustomKeyStoreResponse Int
crsResponseStatus = lens _crsResponseStatus (\ s a -> s{_crsResponseStatus = a})

instance NFData ConnectCustomKeyStoreResponse where
