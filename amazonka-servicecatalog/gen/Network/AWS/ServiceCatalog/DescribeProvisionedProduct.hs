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
-- Module      : Network.AWS.ServiceCatalog.DescribeProvisionedProduct
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve detailed information about the provisioned product.
--
--
module Network.AWS.ServiceCatalog.DescribeProvisionedProduct
    (
    -- * Creating a Request
      describeProvisionedProduct
    , DescribeProvisionedProduct
    -- * Request Lenses
    , dpppAcceptLanguage
    , dpppId

    -- * Destructuring the Response
    , describeProvisionedProductResponse
    , DescribeProvisionedProductResponse
    -- * Response Lenses
    , drsProvisionedProductDetail
    , drsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.ServiceCatalog.Types
import           Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'describeProvisionedProduct' smart constructor.
data DescribeProvisionedProduct = DescribeProvisionedProduct'
    { _dpppAcceptLanguage :: !(Maybe Text)
    , _dpppId             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeProvisionedProduct' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpppAcceptLanguage' - The language code to use for this operation. Supported language codes are as follows: "en" (English) "jp" (Japanese) "zh" (Chinese) If no code is specified, "en" is used as the default.
--
-- * 'dpppId' - The provisioned product identifier.
describeProvisionedProduct
    :: Text -- ^ 'dpppId'
    -> DescribeProvisionedProduct
describeProvisionedProduct pId_ =
    DescribeProvisionedProduct'
    { _dpppAcceptLanguage = Nothing
    , _dpppId = pId_
    }

-- | The language code to use for this operation. Supported language codes are as follows: "en" (English) "jp" (Japanese) "zh" (Chinese) If no code is specified, "en" is used as the default.
dpppAcceptLanguage :: Lens' DescribeProvisionedProduct (Maybe Text)
dpppAcceptLanguage = lens _dpppAcceptLanguage (\ s a -> s{_dpppAcceptLanguage = a});

-- | The provisioned product identifier.
dpppId :: Lens' DescribeProvisionedProduct Text
dpppId = lens _dpppId (\ s a -> s{_dpppId = a});

instance AWSRequest DescribeProvisionedProduct where
        type Rs DescribeProvisionedProduct =
             DescribeProvisionedProductResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 DescribeProvisionedProductResponse' <$>
                   (x .?> "ProvisionedProductDetail") <*>
                     (pure (fromEnum s)))

instance Hashable DescribeProvisionedProduct

instance NFData DescribeProvisionedProduct

instance ToHeaders DescribeProvisionedProduct where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DescribeProvisionedProduct"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeProvisionedProduct where
        toJSON DescribeProvisionedProduct'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _dpppAcceptLanguage,
                  Just ("Id" .= _dpppId)])

instance ToPath DescribeProvisionedProduct where
        toPath = const "/"

instance ToQuery DescribeProvisionedProduct where
        toQuery = const mempty

-- | /See:/ 'describeProvisionedProductResponse' smart constructor.
data DescribeProvisionedProductResponse = DescribeProvisionedProductResponse'
    { _drsProvisionedProductDetail :: !(Maybe ProvisionedProductDetail)
    , _drsResponseStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeProvisionedProductResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsProvisionedProductDetail' - Detailed provisioned product information.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeProvisionedProductResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeProvisionedProductResponse
describeProvisionedProductResponse pResponseStatus_ =
    DescribeProvisionedProductResponse'
    { _drsProvisionedProductDetail = Nothing
    , _drsResponseStatus = pResponseStatus_
    }

-- | Detailed provisioned product information.
drsProvisionedProductDetail :: Lens' DescribeProvisionedProductResponse (Maybe ProvisionedProductDetail)
drsProvisionedProductDetail = lens _drsProvisionedProductDetail (\ s a -> s{_drsProvisionedProductDetail = a});

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeProvisionedProductResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a});

instance NFData DescribeProvisionedProductResponse
