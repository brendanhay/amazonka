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
-- Module      : Network.AWS.ServiceCatalog.ListStackInstancesForProvisionedProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about stack instances that are associated with the specified @CFN_STACKSET@ type provisioned product. You can filter for stack instances that are associated with a specific AWS account name or region.
module Network.AWS.ServiceCatalog.ListStackInstancesForProvisionedProduct
  ( -- * Creating a Request
    listStackInstancesForProvisionedProduct,
    ListStackInstancesForProvisionedProduct,

    -- * Request Lenses
    lsifppAcceptLanguage,
    lsifppPageToken,
    lsifppPageSize,
    lsifppProvisionedProductId,

    -- * Destructuring the Response
    listStackInstancesForProvisionedProductResponse,
    ListStackInstancesForProvisionedProductResponse,

    -- * Response Lenses
    lsifpprsNextPageToken,
    lsifpprsStackInstances,
    lsifpprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'listStackInstancesForProvisionedProduct' smart constructor.
data ListStackInstancesForProvisionedProduct = ListStackInstancesForProvisionedProduct'
  { _lsifppAcceptLanguage ::
      !( Maybe
           Text
       ),
    _lsifppPageToken ::
      !( Maybe
           Text
       ),
    _lsifppPageSize ::
      !( Maybe
           Nat
       ),
    _lsifppProvisionedProductId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListStackInstancesForProvisionedProduct' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsifppAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'lsifppPageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
--
-- * 'lsifppPageSize' - The maximum number of items to return with this call.
--
-- * 'lsifppProvisionedProductId' - The identifier of the provisioned product.
listStackInstancesForProvisionedProduct ::
  -- | 'lsifppProvisionedProductId'
  Text ->
  ListStackInstancesForProvisionedProduct
listStackInstancesForProvisionedProduct pProvisionedProductId_ =
  ListStackInstancesForProvisionedProduct'
    { _lsifppAcceptLanguage =
        Nothing,
      _lsifppPageToken = Nothing,
      _lsifppPageSize = Nothing,
      _lsifppProvisionedProductId = pProvisionedProductId_
    }

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
lsifppAcceptLanguage :: Lens' ListStackInstancesForProvisionedProduct (Maybe Text)
lsifppAcceptLanguage = lens _lsifppAcceptLanguage (\s a -> s {_lsifppAcceptLanguage = a})

-- | The page token for the next set of results. To retrieve the first set of results, use null.
lsifppPageToken :: Lens' ListStackInstancesForProvisionedProduct (Maybe Text)
lsifppPageToken = lens _lsifppPageToken (\s a -> s {_lsifppPageToken = a})

-- | The maximum number of items to return with this call.
lsifppPageSize :: Lens' ListStackInstancesForProvisionedProduct (Maybe Natural)
lsifppPageSize = lens _lsifppPageSize (\s a -> s {_lsifppPageSize = a}) . mapping _Nat

-- | The identifier of the provisioned product.
lsifppProvisionedProductId :: Lens' ListStackInstancesForProvisionedProduct Text
lsifppProvisionedProductId = lens _lsifppProvisionedProductId (\s a -> s {_lsifppProvisionedProductId = a})

instance AWSRequest ListStackInstancesForProvisionedProduct where
  type
    Rs ListStackInstancesForProvisionedProduct =
      ListStackInstancesForProvisionedProductResponse
  request = postJSON serviceCatalog
  response =
    receiveJSON
      ( \s h x ->
          ListStackInstancesForProvisionedProductResponse'
            <$> (x .?> "NextPageToken")
            <*> (x .?> "StackInstances" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListStackInstancesForProvisionedProduct

instance NFData ListStackInstancesForProvisionedProduct

instance ToHeaders ListStackInstancesForProvisionedProduct where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWS242ServiceCatalogService.ListStackInstancesForProvisionedProduct" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListStackInstancesForProvisionedProduct where
  toJSON ListStackInstancesForProvisionedProduct' {..} =
    object
      ( catMaybes
          [ ("AcceptLanguage" .=) <$> _lsifppAcceptLanguage,
            ("PageToken" .=) <$> _lsifppPageToken,
            ("PageSize" .=) <$> _lsifppPageSize,
            Just ("ProvisionedProductId" .= _lsifppProvisionedProductId)
          ]
      )

instance ToPath ListStackInstancesForProvisionedProduct where
  toPath = const "/"

instance ToQuery ListStackInstancesForProvisionedProduct where
  toQuery = const mempty

-- | /See:/ 'listStackInstancesForProvisionedProductResponse' smart constructor.
data ListStackInstancesForProvisionedProductResponse = ListStackInstancesForProvisionedProductResponse'
  { _lsifpprsNextPageToken ::
      !( Maybe
           Text
       ),
    _lsifpprsStackInstances ::
      !( Maybe
           [StackInstance]
       ),
    _lsifpprsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ListStackInstancesForProvisionedProductResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsifpprsNextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- * 'lsifpprsStackInstances' - List of stack instances.
--
-- * 'lsifpprsResponseStatus' - -- | The response status code.
listStackInstancesForProvisionedProductResponse ::
  -- | 'lsifpprsResponseStatus'
  Int ->
  ListStackInstancesForProvisionedProductResponse
listStackInstancesForProvisionedProductResponse pResponseStatus_ =
  ListStackInstancesForProvisionedProductResponse'
    { _lsifpprsNextPageToken =
        Nothing,
      _lsifpprsStackInstances = Nothing,
      _lsifpprsResponseStatus = pResponseStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
lsifpprsNextPageToken :: Lens' ListStackInstancesForProvisionedProductResponse (Maybe Text)
lsifpprsNextPageToken = lens _lsifpprsNextPageToken (\s a -> s {_lsifpprsNextPageToken = a})

-- | List of stack instances.
lsifpprsStackInstances :: Lens' ListStackInstancesForProvisionedProductResponse [StackInstance]
lsifpprsStackInstances = lens _lsifpprsStackInstances (\s a -> s {_lsifpprsStackInstances = a}) . _Default . _Coerce

-- | -- | The response status code.
lsifpprsResponseStatus :: Lens' ListStackInstancesForProvisionedProductResponse Int
lsifpprsResponseStatus = lens _lsifpprsResponseStatus (\s a -> s {_lsifpprsResponseStatus = a})

instance NFData ListStackInstancesForProvisionedProductResponse
