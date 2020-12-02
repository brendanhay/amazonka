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
-- Module      : Network.AWS.KMS.CreateCustomKeyStore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> that is associated with an <https://docs.aws.amazon.com/cloudhsm/latest/userguide/clusters.html AWS CloudHSM cluster> that you own and manage.
--
--
-- This operation is part of the <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html Custom Key Store feature> feature in AWS KMS, which combines the convenience and extensive integration of AWS KMS with the isolation and control of a single-tenant key store.
--
-- Before you create the custom key store, you must assemble the required elements, including an AWS CloudHSM cluster that fulfills the requirements for a custom key store. For details about the required elements, see <https://docs.aws.amazon.com/kms/latest/developerguide/create-keystore.html#before-keystore Assemble the Prerequisites> in the /AWS Key Management Service Developer Guide/ .
--
-- When the operation completes successfully, it returns the ID of the new custom key store. Before you can use your new custom key store, you need to use the 'ConnectCustomKeyStore' operation to connect the new key store to its AWS CloudHSM cluster. Even if you are not going to use your custom key store immediately, you might want to connect it to verify that all settings are correct and then disconnect it until you are ready to use it.
--
-- For help with failures, see <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html Troubleshooting a Custom Key Store> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.CreateCustomKeyStore
  ( -- * Creating a Request
    createCustomKeyStore,
    CreateCustomKeyStore,

    -- * Request Lenses
    ccksCustomKeyStoreName,
    ccksCloudHSMClusterId,
    ccksTrustAnchorCertificate,
    ccksKeyStorePassword,

    -- * Destructuring the Response
    createCustomKeyStoreResponse,
    CreateCustomKeyStoreResponse,

    -- * Response Lenses
    ccksrsCustomKeyStoreId,
    ccksrsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createCustomKeyStore' smart constructor.
data CreateCustomKeyStore = CreateCustomKeyStore'
  { _ccksCustomKeyStoreName ::
      !Text,
    _ccksCloudHSMClusterId :: !Text,
    _ccksTrustAnchorCertificate :: !Text,
    _ccksKeyStorePassword :: !(Sensitive Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCustomKeyStore' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccksCustomKeyStoreName' - Specifies a friendly name for the custom key store. The name must be unique in your AWS account.
--
-- * 'ccksCloudHSMClusterId' - Identifies the AWS CloudHSM cluster for the custom key store. Enter the cluster ID of any active AWS CloudHSM cluster that is not already associated with a custom key store. To find the cluster ID, use the <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters> operation.
--
-- * 'ccksTrustAnchorCertificate' - Enter the content of the trust anchor certificate for the cluster. This is the content of the @customerCA.crt@ file that you created when you <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html initialized the cluster> .
--
-- * 'ccksKeyStorePassword' - Enter the password of the <https://docs.aws.amazon.com/kms/latest/developerguide/key-store-concepts.html#concept-kmsuser @kmsuser@ crypto user (CU) account> in the specified AWS CloudHSM cluster. AWS KMS logs into the cluster as this user to manage key material on your behalf. The password must be a string of 7 to 32 characters. Its value is case sensitive. This parameter tells AWS KMS the @kmsuser@ account password; it does not change the password in the AWS CloudHSM cluster.
createCustomKeyStore ::
  -- | 'ccksCustomKeyStoreName'
  Text ->
  -- | 'ccksCloudHSMClusterId'
  Text ->
  -- | 'ccksTrustAnchorCertificate'
  Text ->
  -- | 'ccksKeyStorePassword'
  Text ->
  CreateCustomKeyStore
createCustomKeyStore
  pCustomKeyStoreName_
  pCloudHSMClusterId_
  pTrustAnchorCertificate_
  pKeyStorePassword_ =
    CreateCustomKeyStore'
      { _ccksCustomKeyStoreName =
          pCustomKeyStoreName_,
        _ccksCloudHSMClusterId = pCloudHSMClusterId_,
        _ccksTrustAnchorCertificate = pTrustAnchorCertificate_,
        _ccksKeyStorePassword = _Sensitive # pKeyStorePassword_
      }

-- | Specifies a friendly name for the custom key store. The name must be unique in your AWS account.
ccksCustomKeyStoreName :: Lens' CreateCustomKeyStore Text
ccksCustomKeyStoreName = lens _ccksCustomKeyStoreName (\s a -> s {_ccksCustomKeyStoreName = a})

-- | Identifies the AWS CloudHSM cluster for the custom key store. Enter the cluster ID of any active AWS CloudHSM cluster that is not already associated with a custom key store. To find the cluster ID, use the <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters> operation.
ccksCloudHSMClusterId :: Lens' CreateCustomKeyStore Text
ccksCloudHSMClusterId = lens _ccksCloudHSMClusterId (\s a -> s {_ccksCloudHSMClusterId = a})

-- | Enter the content of the trust anchor certificate for the cluster. This is the content of the @customerCA.crt@ file that you created when you <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html initialized the cluster> .
ccksTrustAnchorCertificate :: Lens' CreateCustomKeyStore Text
ccksTrustAnchorCertificate = lens _ccksTrustAnchorCertificate (\s a -> s {_ccksTrustAnchorCertificate = a})

-- | Enter the password of the <https://docs.aws.amazon.com/kms/latest/developerguide/key-store-concepts.html#concept-kmsuser @kmsuser@ crypto user (CU) account> in the specified AWS CloudHSM cluster. AWS KMS logs into the cluster as this user to manage key material on your behalf. The password must be a string of 7 to 32 characters. Its value is case sensitive. This parameter tells AWS KMS the @kmsuser@ account password; it does not change the password in the AWS CloudHSM cluster.
ccksKeyStorePassword :: Lens' CreateCustomKeyStore Text
ccksKeyStorePassword = lens _ccksKeyStorePassword (\s a -> s {_ccksKeyStorePassword = a}) . _Sensitive

instance AWSRequest CreateCustomKeyStore where
  type Rs CreateCustomKeyStore = CreateCustomKeyStoreResponse
  request = postJSON kms
  response =
    receiveJSON
      ( \s h x ->
          CreateCustomKeyStoreResponse'
            <$> (x .?> "CustomKeyStoreId") <*> (pure (fromEnum s))
      )

instance Hashable CreateCustomKeyStore

instance NFData CreateCustomKeyStore

instance ToHeaders CreateCustomKeyStore where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("TrentService.CreateCustomKeyStore" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateCustomKeyStore where
  toJSON CreateCustomKeyStore' {..} =
    object
      ( catMaybes
          [ Just ("CustomKeyStoreName" .= _ccksCustomKeyStoreName),
            Just ("CloudHsmClusterId" .= _ccksCloudHSMClusterId),
            Just ("TrustAnchorCertificate" .= _ccksTrustAnchorCertificate),
            Just ("KeyStorePassword" .= _ccksKeyStorePassword)
          ]
      )

instance ToPath CreateCustomKeyStore where
  toPath = const "/"

instance ToQuery CreateCustomKeyStore where
  toQuery = const mempty

-- | /See:/ 'createCustomKeyStoreResponse' smart constructor.
data CreateCustomKeyStoreResponse = CreateCustomKeyStoreResponse'
  { _ccksrsCustomKeyStoreId ::
      !(Maybe Text),
    _ccksrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCustomKeyStoreResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccksrsCustomKeyStoreId' - A unique identifier for the new custom key store.
--
-- * 'ccksrsResponseStatus' - -- | The response status code.
createCustomKeyStoreResponse ::
  -- | 'ccksrsResponseStatus'
  Int ->
  CreateCustomKeyStoreResponse
createCustomKeyStoreResponse pResponseStatus_ =
  CreateCustomKeyStoreResponse'
    { _ccksrsCustomKeyStoreId = Nothing,
      _ccksrsResponseStatus = pResponseStatus_
    }

-- | A unique identifier for the new custom key store.
ccksrsCustomKeyStoreId :: Lens' CreateCustomKeyStoreResponse (Maybe Text)
ccksrsCustomKeyStoreId = lens _ccksrsCustomKeyStoreId (\s a -> s {_ccksrsCustomKeyStoreId = a})

-- | -- | The response status code.
ccksrsResponseStatus :: Lens' CreateCustomKeyStoreResponse Int
ccksrsResponseStatus = lens _ccksrsResponseStatus (\s a -> s {_ccksrsResponseStatus = a})

instance NFData CreateCustomKeyStoreResponse
