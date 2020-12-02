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
-- Module      : Network.AWS.KMS.DescribeCustomKeyStores
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key stores> in the account and region.
--
--
-- This operation is part of the <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html Custom Key Store feature> feature in AWS KMS, which combines the convenience and extensive integration of AWS KMS with the isolation and control of a single-tenant key store.
--
-- By default, this operation returns information about all custom key stores in the account and region. To get only information about a particular custom key store, use either the @CustomKeyStoreName@ or @CustomKeyStoreId@ parameter (but not both).
--
-- To determine whether the custom key store is connected to its AWS CloudHSM cluster, use the @ConnectionState@ element in the response. If an attempt to connect the custom key store failed, the @ConnectionState@ value is @FAILED@ and the @ConnectionErrorCode@ element in the response indicates the cause of the failure. For help interpreting the @ConnectionErrorCode@ , see 'CustomKeyStoresListEntry' .
--
-- Custom key stores have a @DISCONNECTED@ connection state if the key store has never been connected or you use the 'DisconnectCustomKeyStore' operation to disconnect it. If your custom key store state is @CONNECTED@ but you are having trouble using it, make sure that its associated AWS CloudHSM cluster is active and contains the minimum number of HSMs required for the operation, if any.
--
-- For help repairing your custom key store, see the <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html Troubleshooting Custom Key Stores> topic in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.DescribeCustomKeyStores
  ( -- * Creating a Request
    describeCustomKeyStores,
    DescribeCustomKeyStores,

    -- * Request Lenses
    dckssCustomKeyStoreName,
    dckssMarker,
    dckssLimit,
    dckssCustomKeyStoreId,

    -- * Destructuring the Response
    describeCustomKeyStoresResponse,
    DescribeCustomKeyStoresResponse,

    -- * Response Lenses
    dckssrsTruncated,
    dckssrsNextMarker,
    dckssrsCustomKeyStores,
    dckssrsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeCustomKeyStores' smart constructor.
data DescribeCustomKeyStores = DescribeCustomKeyStores'
  { _dckssCustomKeyStoreName ::
      !(Maybe Text),
    _dckssMarker :: !(Maybe Text),
    _dckssLimit :: !(Maybe Nat),
    _dckssCustomKeyStoreId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeCustomKeyStores' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dckssCustomKeyStoreName' - Gets only information about the specified custom key store. Enter the friendly name of the custom key store. By default, this operation gets information about all custom key stores in the account and region. To limit the output to a particular custom key store, you can use either the @CustomKeyStoreId@ or @CustomKeyStoreName@ parameter, but not both.
--
-- * 'dckssMarker' - Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
--
-- * 'dckssLimit' - Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
--
-- * 'dckssCustomKeyStoreId' - Gets only information about the specified custom key store. Enter the key store ID. By default, this operation gets information about all custom key stores in the account and region. To limit the output to a particular custom key store, you can use either the @CustomKeyStoreId@ or @CustomKeyStoreName@ parameter, but not both.
describeCustomKeyStores ::
  DescribeCustomKeyStores
describeCustomKeyStores =
  DescribeCustomKeyStores'
    { _dckssCustomKeyStoreName = Nothing,
      _dckssMarker = Nothing,
      _dckssLimit = Nothing,
      _dckssCustomKeyStoreId = Nothing
    }

-- | Gets only information about the specified custom key store. Enter the friendly name of the custom key store. By default, this operation gets information about all custom key stores in the account and region. To limit the output to a particular custom key store, you can use either the @CustomKeyStoreId@ or @CustomKeyStoreName@ parameter, but not both.
dckssCustomKeyStoreName :: Lens' DescribeCustomKeyStores (Maybe Text)
dckssCustomKeyStoreName = lens _dckssCustomKeyStoreName (\s a -> s {_dckssCustomKeyStoreName = a})

-- | Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
dckssMarker :: Lens' DescribeCustomKeyStores (Maybe Text)
dckssMarker = lens _dckssMarker (\s a -> s {_dckssMarker = a})

-- | Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
dckssLimit :: Lens' DescribeCustomKeyStores (Maybe Natural)
dckssLimit = lens _dckssLimit (\s a -> s {_dckssLimit = a}) . mapping _Nat

-- | Gets only information about the specified custom key store. Enter the key store ID. By default, this operation gets information about all custom key stores in the account and region. To limit the output to a particular custom key store, you can use either the @CustomKeyStoreId@ or @CustomKeyStoreName@ parameter, but not both.
dckssCustomKeyStoreId :: Lens' DescribeCustomKeyStores (Maybe Text)
dckssCustomKeyStoreId = lens _dckssCustomKeyStoreId (\s a -> s {_dckssCustomKeyStoreId = a})

instance AWSRequest DescribeCustomKeyStores where
  type Rs DescribeCustomKeyStores = DescribeCustomKeyStoresResponse
  request = postJSON kms
  response =
    receiveJSON
      ( \s h x ->
          DescribeCustomKeyStoresResponse'
            <$> (x .?> "Truncated")
            <*> (x .?> "NextMarker")
            <*> (x .?> "CustomKeyStores" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeCustomKeyStores

instance NFData DescribeCustomKeyStores

instance ToHeaders DescribeCustomKeyStores where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("TrentService.DescribeCustomKeyStores" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeCustomKeyStores where
  toJSON DescribeCustomKeyStores' {..} =
    object
      ( catMaybes
          [ ("CustomKeyStoreName" .=) <$> _dckssCustomKeyStoreName,
            ("Marker" .=) <$> _dckssMarker,
            ("Limit" .=) <$> _dckssLimit,
            ("CustomKeyStoreId" .=) <$> _dckssCustomKeyStoreId
          ]
      )

instance ToPath DescribeCustomKeyStores where
  toPath = const "/"

instance ToQuery DescribeCustomKeyStores where
  toQuery = const mempty

-- | /See:/ 'describeCustomKeyStoresResponse' smart constructor.
data DescribeCustomKeyStoresResponse = DescribeCustomKeyStoresResponse'
  { _dckssrsTruncated ::
      !(Maybe Bool),
    _dckssrsNextMarker ::
      !(Maybe Text),
    _dckssrsCustomKeyStores ::
      !( Maybe
           [CustomKeyStoresListEntry]
       ),
    _dckssrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeCustomKeyStoresResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dckssrsTruncated' - A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
--
-- * 'dckssrsNextMarker' - When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
--
-- * 'dckssrsCustomKeyStores' - Contains metadata about each custom key store.
--
-- * 'dckssrsResponseStatus' - -- | The response status code.
describeCustomKeyStoresResponse ::
  -- | 'dckssrsResponseStatus'
  Int ->
  DescribeCustomKeyStoresResponse
describeCustomKeyStoresResponse pResponseStatus_ =
  DescribeCustomKeyStoresResponse'
    { _dckssrsTruncated = Nothing,
      _dckssrsNextMarker = Nothing,
      _dckssrsCustomKeyStores = Nothing,
      _dckssrsResponseStatus = pResponseStatus_
    }

-- | A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
dckssrsTruncated :: Lens' DescribeCustomKeyStoresResponse (Maybe Bool)
dckssrsTruncated = lens _dckssrsTruncated (\s a -> s {_dckssrsTruncated = a})

-- | When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
dckssrsNextMarker :: Lens' DescribeCustomKeyStoresResponse (Maybe Text)
dckssrsNextMarker = lens _dckssrsNextMarker (\s a -> s {_dckssrsNextMarker = a})

-- | Contains metadata about each custom key store.
dckssrsCustomKeyStores :: Lens' DescribeCustomKeyStoresResponse [CustomKeyStoresListEntry]
dckssrsCustomKeyStores = lens _dckssrsCustomKeyStores (\s a -> s {_dckssrsCustomKeyStores = a}) . _Default . _Coerce

-- | -- | The response status code.
dckssrsResponseStatus :: Lens' DescribeCustomKeyStoresResponse Int
dckssrsResponseStatus = lens _dckssrsResponseStatus (\s a -> s {_dckssrsResponseStatus = a})

instance NFData DescribeCustomKeyStoresResponse
