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
-- Module      : Network.AWS.KMS.UpdateCustomKeyStore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the properties of a custom key store. Use the @CustomKeyStoreId@ parameter to identify the custom key store you want to edit. Use the remaining parameters to change the properties of the custom key store.
--
--
-- You can only update a custom key store that is disconnected. To disconnect the custom key store, use 'DisconnectCustomKeyStore' . To reconnect the custom key store after the update completes, use 'ConnectCustomKeyStore' . To find the connection state of a custom key store, use the 'DescribeCustomKeyStores' operation.
--
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
--
-- If the operation succeeds, it returns a JSON object with no properties.
--
-- This operation is part of the <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html Custom Key Store feature> feature in AWS KMS, which combines the convenience and extensive integration of AWS KMS with the isolation and control of a single-tenant key store.
module Network.AWS.KMS.UpdateCustomKeyStore
  ( -- * Creating a Request
    updateCustomKeyStore,
    UpdateCustomKeyStore,

    -- * Request Lenses
    ucksKeyStorePassword,
    ucksCloudHSMClusterId,
    ucksNewCustomKeyStoreName,
    ucksCustomKeyStoreId,

    -- * Destructuring the Response
    updateCustomKeyStoreResponse,
    UpdateCustomKeyStoreResponse,

    -- * Response Lenses
    ucksrsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateCustomKeyStore' smart constructor.
data UpdateCustomKeyStore = UpdateCustomKeyStore'
  { _ucksKeyStorePassword ::
      !(Maybe (Sensitive Text)),
    _ucksCloudHSMClusterId :: !(Maybe Text),
    _ucksNewCustomKeyStoreName :: !(Maybe Text),
    _ucksCustomKeyStoreId :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateCustomKeyStore' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucksKeyStorePassword' - Enter the current password of the @kmsuser@ crypto user (CU) in the AWS CloudHSM cluster that is associated with the custom key store. This parameter tells AWS KMS the current password of the @kmsuser@ crypto user (CU). It does not set or change the password of any users in the AWS CloudHSM cluster.
--
-- * 'ucksCloudHSMClusterId' - Associates the custom key store with a related AWS CloudHSM cluster.  Enter the cluster ID of the cluster that you used to create the custom key store or a cluster that shares a backup history and has the same cluster certificate as the original cluster. You cannot use this parameter to associate a custom key store with an unrelated cluster. In addition, the replacement cluster must <https://docs.aws.amazon.com/kms/latest/developerguide/create-keystore.html#before-keystore fulfill the requirements> for a cluster associated with a custom key store. To view the cluster certificate of a cluster, use the <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters> operation.
--
-- * 'ucksNewCustomKeyStoreName' - Changes the friendly name of the custom key store to the value that you specify. The custom key store name must be unique in the AWS account.
--
-- * 'ucksCustomKeyStoreId' - Identifies the custom key store that you want to update. Enter the ID of the custom key store. To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
updateCustomKeyStore ::
  -- | 'ucksCustomKeyStoreId'
  Text ->
  UpdateCustomKeyStore
updateCustomKeyStore pCustomKeyStoreId_ =
  UpdateCustomKeyStore'
    { _ucksKeyStorePassword = Nothing,
      _ucksCloudHSMClusterId = Nothing,
      _ucksNewCustomKeyStoreName = Nothing,
      _ucksCustomKeyStoreId = pCustomKeyStoreId_
    }

-- | Enter the current password of the @kmsuser@ crypto user (CU) in the AWS CloudHSM cluster that is associated with the custom key store. This parameter tells AWS KMS the current password of the @kmsuser@ crypto user (CU). It does not set or change the password of any users in the AWS CloudHSM cluster.
ucksKeyStorePassword :: Lens' UpdateCustomKeyStore (Maybe Text)
ucksKeyStorePassword = lens _ucksKeyStorePassword (\s a -> s {_ucksKeyStorePassword = a}) . mapping _Sensitive

-- | Associates the custom key store with a related AWS CloudHSM cluster.  Enter the cluster ID of the cluster that you used to create the custom key store or a cluster that shares a backup history and has the same cluster certificate as the original cluster. You cannot use this parameter to associate a custom key store with an unrelated cluster. In addition, the replacement cluster must <https://docs.aws.amazon.com/kms/latest/developerguide/create-keystore.html#before-keystore fulfill the requirements> for a cluster associated with a custom key store. To view the cluster certificate of a cluster, use the <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters> operation.
ucksCloudHSMClusterId :: Lens' UpdateCustomKeyStore (Maybe Text)
ucksCloudHSMClusterId = lens _ucksCloudHSMClusterId (\s a -> s {_ucksCloudHSMClusterId = a})

-- | Changes the friendly name of the custom key store to the value that you specify. The custom key store name must be unique in the AWS account.
ucksNewCustomKeyStoreName :: Lens' UpdateCustomKeyStore (Maybe Text)
ucksNewCustomKeyStoreName = lens _ucksNewCustomKeyStoreName (\s a -> s {_ucksNewCustomKeyStoreName = a})

-- | Identifies the custom key store that you want to update. Enter the ID of the custom key store. To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
ucksCustomKeyStoreId :: Lens' UpdateCustomKeyStore Text
ucksCustomKeyStoreId = lens _ucksCustomKeyStoreId (\s a -> s {_ucksCustomKeyStoreId = a})

instance AWSRequest UpdateCustomKeyStore where
  type Rs UpdateCustomKeyStore = UpdateCustomKeyStoreResponse
  request = postJSON kms
  response =
    receiveEmpty
      (\s h x -> UpdateCustomKeyStoreResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateCustomKeyStore

instance NFData UpdateCustomKeyStore

instance ToHeaders UpdateCustomKeyStore where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("TrentService.UpdateCustomKeyStore" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateCustomKeyStore where
  toJSON UpdateCustomKeyStore' {..} =
    object
      ( catMaybes
          [ ("KeyStorePassword" .=) <$> _ucksKeyStorePassword,
            ("CloudHsmClusterId" .=) <$> _ucksCloudHSMClusterId,
            ("NewCustomKeyStoreName" .=) <$> _ucksNewCustomKeyStoreName,
            Just ("CustomKeyStoreId" .= _ucksCustomKeyStoreId)
          ]
      )

instance ToPath UpdateCustomKeyStore where
  toPath = const "/"

instance ToQuery UpdateCustomKeyStore where
  toQuery = const mempty

-- | /See:/ 'updateCustomKeyStoreResponse' smart constructor.
newtype UpdateCustomKeyStoreResponse = UpdateCustomKeyStoreResponse'
  { _ucksrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateCustomKeyStoreResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucksrsResponseStatus' - -- | The response status code.
updateCustomKeyStoreResponse ::
  -- | 'ucksrsResponseStatus'
  Int ->
  UpdateCustomKeyStoreResponse
updateCustomKeyStoreResponse pResponseStatus_ =
  UpdateCustomKeyStoreResponse'
    { _ucksrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
ucksrsResponseStatus :: Lens' UpdateCustomKeyStoreResponse Int
ucksrsResponseStatus = lens _ucksrsResponseStatus (\s a -> s {_ucksrsResponseStatus = a})

instance NFData UpdateCustomKeyStoreResponse
